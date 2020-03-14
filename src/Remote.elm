module Remote exposing 
    ( Remote
    , pure
    , return
    , empty
    , loading
    , withDefault
    , fromResult
    , map
    , andThen
    , load
    )

import Effect exposing (Effect)
import Focus exposing (Focus)
import StateEffect exposing (StateEffect)


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


-- TODO: If it has empty, should it have an append and what does that mean?
-- Other name for empty probably...
empty : Remote e a
empty =
    NotAsked


loading : Remote e a
loading =
    Loading


withDefault : a -> Remote e a -> a
withDefault default remote =
    case remote of
        Found a ->
            a

        _ ->
            default


map : (a -> b) -> Remote e a -> Remote e b 
map fn remote =
    andThen (fn >> Found) remote


andThen : (a -> Remote e b) -> Remote e a -> Remote e b 
andThen fn remote =
    case remote of
        Found a ->
            fn a

        Loading ->
            Loading

        Error e ->
            Error e

        NotAsked ->
            NotAsked


fromResult : Result e a -> Remote e a
fromResult result =
    case result of
        Ok a ->
            Found a

        Err e ->
            Error e


load : Focus s (Remote e a) -> List cmd -> StateEffect s e a cmd
load lens cmds = 
    StateEffect.advance (\s ->
        case Focus.get lens s of
            NotAsked ->
                ( Effect.load cmds
                , Focus.set lens Loading s
                )

            Loading ->
                ( Effect.wait
                , s
                )

            Error e ->
                ( Effect.fail e
                , s
                )

            Found a ->
                ( Effect.pure a
                , s
                )
    )
