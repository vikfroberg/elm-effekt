module EffectTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Effect exposing (..)
import Proof


suite : Test
suite =
    describe "Effect"
        [ Proof.functor { return = succeed, map = map }
        , Proof.semigroup { return = succeed, append = append }
        , Proof.monoid { return = succeed, empty = empty, append = append }
        ]
