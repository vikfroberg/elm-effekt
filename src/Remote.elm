module Remote exposing 
    ( Remote

    , notAsked
    , loading
    , error
    , found
    , fromResult
    , withDefault

    , map

    , andThen
    , do

    , empty
    , append
    , concat

    , map2
    , map3
    , andMap
    , ap
    , combine

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


-- Monoid


empty : Remote e a
empty = 
    NotAsked


append : Remote e a -> Remote e a -> Remote e a
append ra rb =
    case rb of
        NotAsked ->
            case ra of
                Found a ->
                    Found a

                Error e ->
                    Error e

                _ ->
                    NotAsked

        Loading ->
            case ra of
                Found a ->
                    Found a

                Error e ->
                    Error e

                _ ->
                    Loading

        Error e ->
            Error e

        Found a ->
            Found a


concat : List (Remote e a) -> Remote e a
concat rs =
      List.foldr append empty rs


-- Apply


map2 :
    (a -> b -> c)
    -> Remote e a
    -> Remote e b
    -> Remote e c
map2 f a b = 
    map f a
        |> andMap b


map3 :
    (a -> b -> c -> d)
    -> Remote e a
    -> Remote e b
    -> Remote e c
    -> Remote e d
map3 f a b c = 
    map f a
        |> andMap b
        |> andMap c


map4 :
    (a -> b -> c -> d -> e)
    -> Remote err a
    -> Remote err b
    -> Remote err c
    -> Remote err d
    -> Remote err e
map4 f a b c d = 
    map f a
        |> andMap b
        |> andMap c
        |> andMap d


map5 :
    (a -> b -> c -> d -> e -> f)
    -> Remote err a
    -> Remote err b
    -> Remote err c
    -> Remote err d
    -> Remote err e
    -> Remote err f
map5 f a b c d e = 
    map f a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e


ap : Remote e (a -> b) -> Remote e a -> Remote e b
ap rf ra =
    andThen (\f -> map f ra) rf


andMap : Remote e a -> Remote e (a -> b) -> Remote e b
andMap ra rf = 
    ap rf ra


combine : List (Remote e a) -> Remote e (List a)
combine rs =
    List.foldr (map2 (::)) (found []) rs


-- Constructors


found : a -> Remote e a
found = 
    Found


notAsked : Remote e a
notAsked =
    NotAsked


loading : Remote e a
loading =
    Loading


error : e -> Remote e a
error e =
    Error e


fromResult : Result e a -> Remote e a
fromResult result =
    case result of
        Ok a ->
            Found a

        Err e ->
            Error e


withDefault : a -> Remote e a -> a
withDefault default remote =
    case remote of
        Found a ->
            a

        _ ->
            default


-- Functor


map : (a -> b) -> Remote e a -> Remote e b 
map fn remote =
    andThen (fn >> Found) remote


-- Monad 


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


do : Remote e a -> (a -> Remote e b) -> Remote e b
do ea fn = 
    andThen fn ea


load : Focus s (Remote e a) -> List cmd -> StateEffect s e a cmd
load lens cmds = 
    StateEffect.advance (\s ->
        case Focus.get lens s of
            NotAsked ->
                ( Effect.perform cmds
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
                ( Effect.succeed a
                , s
                )
    )
