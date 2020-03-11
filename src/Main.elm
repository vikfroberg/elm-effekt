module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as HE
import Html.Attributes as HA
import Task
import Process


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
    { index = NotAsked
    , item = []
    , comment = []
    }
    |> load


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


type alias HttpError = ()
type alias Id = String


type Remote e a 
    = NotAsked
    | Loading
    | Error e
    | Found a


fromResult : Result e a -> Remote e a
fromResult result =
    case result of
        Ok a ->
            Found a

        Err e ->
            Error e


type alias Repo id e a =
    List ( id, Remote e a )

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecvIndex result ->
            setIndex (fromResult result) model
                |> load

        RecvItem id result ->
            setItem id (fromResult result) model
                |> load

        RecvComment id result ->
            setComment id (fromResult result) model
                |> load


type State e a updater
    = Perform updater
    | Fail e
    | Succeed a

succeed = Succeed >> always

call : Cmd msg -> (model -> Remote e a) -> (Remote e a -> model -> model) -> model -> State e a ( model, Cmd msg )
call cmd get set model =
    case get model of
        NotAsked ->
            ( set Loading model
            , cmd
            )
            |> Perform

        Loading ->
            ( model
            , Cmd.none
            )
            |> Perform

        Found a ->
            Succeed a

        Error e ->
            Fail e


callWith : (x -> Cmd msg) -> (x -> model -> Remote e a) -> (x -> Remote e a -> model -> model) -> x -> model -> State e a ( model, Cmd msg )
callWith toCmd toGet toSet x model =
    call (toCmd x) (toGet x) (toSet x) model


getIndex = .index
setIndex x model = { model | index = x }

getItem : Id -> Model -> Remote HttpError Item
getItem id model = 
    model.item 
        |> List.filter (Tuple.first >> (==) id) 
        |> List.head 
        |> Maybe.map Tuple.second
        |> Maybe.withDefault NotAsked

setItem : Id -> Remote HttpError Item -> Model -> Model
setItem id x model = 
    let newItem = List.filter (Tuple.first >> (/=) id) model.item in
    { model | item = ( id, x ) :: newItem }

getComment : Id -> Model -> Remote HttpError Comment
getComment id model = 
    model.comment 
        |> List.filter (Tuple.first >> (==) id) 
        |> List.head 
        |> Maybe.map Tuple.second
        |> Maybe.withDefault NotAsked

setComment : Id -> Remote HttpError Comment -> Model -> Model
setComment id x model = 
    let newComment = List.filter (Tuple.first >> (/=) id) model.comment in
    { model | comment = ( id, x ) :: newComment }


with : a -> (a -> b) -> b
with a fn =
    fn a


fetchIndex = msgToCmdWithDelay 1000 (RecvIndex <| Ok [ "1", "2", "3" ])
fetchItem id = msgToCmdWithDelay 1000 (RecvItem id <| Ok { id = id, comments = [ "1", "2", "3" ] })
fetchComment id = msgToCmdWithDelay 1000 (RecvComment id <| Ok { id = id })


do : (model -> State e a p) -> (a -> model -> State e b p) -> model -> State e b p
do toState cb model =
    case toState model of
        Succeed a -> 
            cb a model

        Perform p ->
            Perform p

        Fail e ->
            Fail e


mapTo : b -> (model -> State e a p) -> model -> State e b p
mapTo x toState model =
    case toState model of
        Succeed _ ->
            Succeed x

        Perform p ->
            Perform p

        Fail e ->
            Fail e


sequence : (x -> model -> State e a p) -> List x -> model -> State e (List a) p
sequence toState list model =
    let
        sequence_ acc list_ =
            case acc of
                Succeed xs ->
                    case list_ of
                        [] ->
                            acc

                        x :: rest ->
                            case toState x model of
                                Succeed a ->
                                    sequence_ (Succeed (a :: xs)) rest

                                Fail e ->
                                    -- TODO: Should it really fail here?
                                    -- It should probably continue without adding the item
                                    Fail e

                                Perform p ->
                                    Perform p
                _ ->
                    acc
    in
    sequence_ (Succeed []) list


all : (x -> model -> State e a ( model, Cmd msg )) -> List x -> model -> State e (List a) ( model, Cmd msg )
all toState list initModel =
    List.foldl (\x acc ->
        case acc of
            Perform ( m0, c0 ) ->
                case toState x m0 of
                    Perform ( m1, c1 ) ->
                        Perform ( m1, Cmd.batch [ c0, c1 ] )

                    _ ->
                        acc

            Succeed ys ->
                case toState x initModel of
                    Perform ( m1, c1 ) ->
                        Perform ( m1, c1 )

                    Succeed y ->
                        Succeed (y :: ys)

                    Fail e ->
                        Fail e
            _ ->
                acc
    )
    (Succeed [])
    list


load : Model -> ( Model, Cmd Msg )
load model =
    case load_ model of
        Perform x ->
            x

        Succeed a ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


load_ : Model -> State HttpError () ( Model, Cmd Msg )
load_ model =
    with model <|
        do (call fetchIndex getIndex setIndex) <| \ids ->
        do (ids |> sequence (callWith fetchItem getItem setItem)) <| \items ->
        do (items |> sequence (.comments >> all (callWith fetchComment getComment setComment))) <| \_ ->
            succeed ()


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
