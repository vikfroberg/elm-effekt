module Proof exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


functor { return, map } =
    describe "Functor"
        [ test "Identity" <| \_ ->
            let
                a = return 1
            in
            Expect.equal a (map identity a)
        , test "Composition" <| \_ ->
            let
                a = return 1
                f = (+) 1
                g = (+) 2
            in
            Expect.equal
                (map (f << g) a)
                ((map f << map g) a)
                
        ]


monoid { return, empty, append } =
    describe "Monoid"
        [ test "Right identity" <| \_ ->
            let
                a = return 1
            in
            Expect.equal 
                (a |> append empty)
                a
        , test "Left identity" <| \_ ->
            let
                a = return 1
            in
            Expect.equal 
                (empty |> append a)
                a
        ]


semigroup { return, append } =
    describe "Semigroup"
        [ test "Associativity" <| \_ ->
            let
                a = return 1
                b = return 2
                c = return 3
            in
            Expect.equal
                (a |> append b |> append c)
                (a |> append (b |> append c))
        ]


-- apply : 
--     { return : a -> fa
--     -- , ap : ff -> fa -> fb
-- --     , map3 : (a -> b -> c -> d) -> fa -> fb -> fc -> fd 
--     }
--     -> Test
-- apply { return } =
--     describe "Apply"
--         [ test "Associative composition" <| \_ ->
--             let
--                 fa = return 1
--                 ff = return ((+) 1)
--                 fg = return ((+) 2)
--             in
--             Expect.equal
--                 1
--                 1
--                 -- (map3 (<<) ff fg fa)
--                 -- (a <| ap g <| ap f)
--         ]











