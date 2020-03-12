module Repo exposing (..)

import Remote exposing (Remote)
import Cache exposing (Cache)
import Effect exposing (Effect)


type Repo id e a
    = Repo (Cache id (Remote e a))


empty : Repo id e a
empty =
    Repo Cache.empty


get : id -> Repo id e a -> Remote e a
get id (Repo cache) = 
    Cache.get id cache
        |> Maybe.withDefault Remote.empty

set : id -> Remote e a -> Repo id e a -> Repo id e a
set id a (Repo cache) = 
    Cache.set id a cache
        |> Repo


load : (id -> cmd) -> (id -> model -> Remote e a) -> (id -> Remote e a -> model -> model) -> id -> model -> Effect e a model cmd
load toCmd toGet toSet id model =
    Remote.load (toCmd id) (toGet id) (toSet id) model
