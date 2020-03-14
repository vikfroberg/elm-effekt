module Cache exposing (..)


type Cache id a
    = Cache (List ( id, a ))


empty : Cache id a
empty =
    Cache []


get : id -> Cache id a -> Maybe a
get id (Cache list) = 
    list
        |> List.filter (Tuple.first >> (==) id) 
        |> List.head 
        |> Maybe.map Tuple.second


set : id -> a -> Cache id a -> Cache id a
set id a (Cache list) = 
    ( id, a ) :: List.filter (Tuple.first >> (/=) id) list
        |> Cache


update : id -> (Maybe a -> Maybe a) -> Cache id a -> Cache id a
update id fn (Cache list) = 
    get id (Cache list)
        |> fn
        |> Maybe.map (Tuple.pair id)
        |> List.singleton
        |> List.filterMap identity
        |> List.append (List.filter (Tuple.first >> (/=) id) list)
        |> Cache
