module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as HE
import Html.Attributes as HA
import Task
import Process
import Effect exposing (Effect(..))
import Repo exposing (Repo)
import Remote exposing (Remote)


type alias HttpError = ()
type alias Id = String
type alias Comment = { id : Id }
type alias Item = { id : Id, comments : List Id }
type alias Index = List Id


type alias Model =
    { index : Remote HttpError Index
    , item : Repo Id HttpError Item
    , comment : Repo Id HttpError Comment
    }


type Msg
    = RecvIndex (Result HttpError Index)
    | RecvItem Id (Result HttpError Item)
    | RecvComment Id (Result HttpError Comment)


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    { index = Remote.empty
    , item = Repo.empty
    , comment = Repo.empty
    }
    |> runEffects


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecvIndex result ->
            setIndex (Remote.fromResult result) model
                |> runEffects

        RecvItem id result ->
            setItem id (Remote.fromResult result) model
                |> runEffects

        RecvComment id result ->
            setComment id (Remote.fromResult result) model
                |> runEffects


runEffects : Model -> ( Model, Cmd Msg )
runEffects model =
    effect
        |> Effect.with model
        |> Effect.run (doNothing model) (doNothing model)


effect : Model -> Effect HttpError () Model (Cmd Msg)
effect =
    Effect.do loadIndex <| \ids ->
    Effect.do (ids |> Effect.sequence loadItem) <| \items ->
    Effect.do (items |> Effect.sequence (.comments >> Effect.all loadComment)) <| \_ ->
        Effect.return ()


doNothing : Model -> x -> ( Model, Cmd Msg )
doNothing model _ =
    ( model, Cmd.none )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Elm-effekt"
    , body = 
        [ Html.text <| Debug.toString model.index
        , Html.hr [] []
        , Html.text <| Debug.toString model.item
        , Html.hr [] []
        , Html.text <| Debug.toString model.comment
        ]
    }


-- Index

getIndex : Model -> Remote HttpError Index
getIndex = 
    .index

setIndex : Remote HttpError Index -> Model -> Model
setIndex x model = 
    { model | index = x }

fetchIndex = 
    msgToCmdWithDelay 1000 (RecvIndex <| Ok [ "1", "2", "3" ])

loadIndex =
    Remote.load fetchIndex getIndex setIndex


-- Item

getItem : Id -> Model -> Remote HttpError Item
getItem id model = 
    Repo.get id model.item 

setItem : Id -> Remote HttpError Item -> Model -> Model
setItem id x model = 
    { model | item = Repo.set id x model.item }

fetchItem id = 
    msgToCmdWithDelay 1000 (RecvItem id <| Ok { id = id, comments = [ "1", "2", "3" ] })

loadItem =
    Repo.load fetchItem getItem setItem 


-- Comment

getComment : Id -> Model -> Remote HttpError Comment
getComment id model = 
    Repo.get id model.comment 

setComment : Id -> Remote HttpError Comment -> Model -> Model
setComment id x model = 
    { model | comment = Repo.set id x model.comment }

fetchComment id = 
    msgToCmdWithDelay 1000 (RecvComment id <| Ok { id = id })

loadComment =
    Repo.load fetchComment getComment setComment 


-- Helpers

msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity
        (Task.succeed msg)


msgToCmdWithDelay : Float -> msg -> Cmd msg
msgToCmdWithDelay delay msg =
    Task.perform identity
        (Process.sleep delay
            |> Task.andThen (\_ -> Task.succeed msg)
        )
