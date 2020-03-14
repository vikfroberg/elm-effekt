module Repo exposing (..)

import Remote exposing (Remote)
import AssocList as Dict exposing (Dict)
import Focus exposing (Focus)
import StateEffect exposing (StateEffect)


type Repo id e a
    = Repo (Dict id (Remote e a))


empty : Repo id e a
empty =
    Repo Dict.empty


get : id -> Repo id e a -> Remote e a
get id (Repo cache) = 
    Dict.get id cache
        |> Maybe.withDefault Remote.empty


set : id -> Remote e a -> Repo id e a -> Repo id e a
set id a (Repo cache) = 
    Dict.insert id a cache
        |> Repo


toLens : id -> Focus (Repo id e a) (Remote e a)
toLens id = 
    Focus.create (get id) (update id)


update : id -> (Remote e a -> Remote e a) -> Repo id e a -> Repo id e a
update id fn (Repo cache) = 
    set id (fn <| get id (Repo cache)) (Repo cache)


load : Focus s (Repo id e a) -> (id -> List cmd) -> id -> StateEffect s e a cmd
load lens toCmds id =
    Remote.load (Focus.compose lens <| toLens id) (toCmds id)
