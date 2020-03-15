module RemoteTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Remote exposing (..)
import Proof


suite : Test
suite =
    describe "Effect"
        [ Proof.functor { return = found, map = map }
        , Proof.semigroup { return = found, append = append }
        , Proof.monoid { return = found, empty = empty, append = append }
        -- , Proof.apply { return = found, ap = ap, map3 = map3 }
        ]
