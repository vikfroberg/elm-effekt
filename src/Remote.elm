module Remote exposing (..)

import Effect exposing (Effect)


type Remote e a 
    = NotAsked
    | Loading
    | Error e
    | Found a


pure : a -> Remote e a
pure = 
    Found


return : a -> Remote e a
return = 
    pure


empty : Remote e a
empty =
    NotAsked


loading : Remote e a
loading =
    Loading


fromResult : Result e a -> Remote e a
fromResult result =
    case result of
        Ok a ->
            Found a

        Err e ->
            Error e


load : cmd -> (model -> Remote e a) -> (Remote e a -> model -> model) -> model -> Effect e a model cmd
load cmd get set model =
    case get model of
        NotAsked ->
            ( set Loading model
            , [ cmd ]
            )
            |> Effect.Perform

        Loading ->
            ( model
            , []
            )
            |> Effect.Perform

        Found a ->
            Effect.Succeed a

        Error e ->
            Effect.Fail e
