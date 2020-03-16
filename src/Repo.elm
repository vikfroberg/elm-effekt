module Repo exposing 
    ( Repo
    , empty
    , get
    , set
    , update
    , load
    , toLens
    )

import Remote exposing (Remote)
import AssocList as Dict exposing (Dict)
import Focus exposing (Focus)
import Control exposing (Control)


type Repo id e a
    = Repo (Dict id (Remote e a))


empty : Repo id e a
empty =
    Repo Dict.empty


get : id -> Repo id e a -> Remote e a
get id (Repo cache) = 
    Dict.get id cache
        |> Maybe.withDefault Remote.notAsked


set : id -> Remote e a -> Repo id e a -> Repo id e a
set id a (Repo cache) = 
    Dict.insert id a cache
        |> Repo


update : id -> (Remote e a -> Remote e a) -> Repo id e a -> Repo id e a
update id fn (Repo cache) = 
    set id (fn <| get id (Repo cache)) (Repo cache)


load : Focus model (Repo id e a) -> (id -> Cmd msg) -> id -> Control model e a ( model, Cmd msg )
load lens toCmds id =
    Remote.load (Focus.compose lens <| toLens id) (toCmds id)


toLens : id -> Focus (Repo id e a) (Remote e a)
toLens id = 
    Focus.create (get id) (update id)
