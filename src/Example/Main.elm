module Example.Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as HE
import Html.Attributes as HA
import Task
import Process
import Effect exposing (Effect(..))
import Repo exposing (Repo)
import Remote exposing (Remote)
import Focus exposing (Focus)
import Control exposing (Control)
import Control.Async


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
    { index = Remote.notAsked
    , item = Repo.empty
    , comment = Repo.empty
    }
    |> runEffects


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecvIndex result ->
            Focus.update 
                indexLens 
                (Remote.append <| Remote.fromResult result)
                model
                |> runEffects

        RecvItem id result ->
            Focus.update 
                itemLens 
                (Repo.update id (Remote.append <| Remote.fromResult result))
                model
                |> runEffects

        RecvComment id result ->
            Focus.update 
                commentLens 
                (Repo.update id (Remote.append <| Remote.fromResult result))
                model
                |> runEffects


runEffects : Model -> ( Model, Cmd Msg )
runEffects model =
    Control.run model effect


effect =
    let
        commentsLoad item =
            List.map commentLoad item.comments
                |> Control.Async.combine
    in
    Control.do indexLoad <| \ids ->
    Control.do (List.map itemLoad ids |> Control.sequence) <| \items ->
    Control.do (List.map commentsLoad items |> Control.sequence) <| \_ ->
        Control.succeed ()


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

indexLens = 
    Focus.create .index (\fn m -> { m | index = fn m.index })

indexFetch = 
    msgToCmdWithDelay 1000 (RecvIndex <| Ok [ "1", "2", "3" ])

indexLoad =
    Remote.load indexLens indexFetch


-- Item

itemLens = 
    Focus.create .item (\fn m -> { m | item = fn m.item })

itemFetch id = 
    msgToCmdWithDelay 1000 (RecvItem id <| Ok { id = id, comments = [ "1", "2", "3" ] })

itemLoad =
    Repo.load itemLens itemFetch


-- Comment

commentLens = 
    Focus.create .comment (\fn m -> { m | comment = fn m.comment })

commentFetch id = 
    msgToCmdWithDelay 1000 (RecvComment id <| Ok { id = id })

commentLoad =
    Repo.load commentLens commentFetch


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
