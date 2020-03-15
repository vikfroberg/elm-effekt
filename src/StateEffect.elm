module StateEffect exposing (..)

import Effect exposing (Effect)
import State exposing (State)


type StateEffect s e a cmd 
    = StateEffect (State s (Effect e a cmd))


flip ( a, b ) = 
    ( b, a )


run : s -> StateEffect s e a cmd -> ( s, Effect e a cmd )
run s (StateEffect state) = 
    State.run s state
        |> flip


advance : (s -> ( s, Effect e a cmd )) -> StateEffect s e a cmd
advance f = 
    State.advance (f >> flip)
        |> StateEffect


return : a -> StateEffect s e a cmd
return a = 
    State.map (always <| Effect.succeed a) State.get
        |> StateEffect



do : StateEffect s e a cmd -> (a -> StateEffect s e b cmd) -> StateEffect s e b cmd
do a f = 
    andThen f a


unwrap (StateEffect state) =
    state


andThen : (a -> StateEffect s e b cmd) -> StateEffect s e a cmd -> StateEffect s e b cmd
andThen f (StateEffect state) =
    State.andThen (\effect ->
        let
            newEffect =
                Effect.map f effect
        in
        case (effect |> Effect.map Just |> Effect.withDefault Nothing) of
            Just a ->
                f a

            Nothing
            |> Effect.map (f >> unwrap)
            |> Effect.withDefault (state |> State.map (Effect.append effect))
    )
    state
    |> StateEffect


map : (a -> b) -> StateEffect s e a cmd -> StateEffect s e b cmd
map f =
    andThen (f >> return)


map2 :
    (a -> b -> c) 
    -> StateEffect s e a cmd 
    -> StateEffect s e b cmd 
    -> StateEffect s e c cmd
map2 f a b =
    map f a
        |> andMap b


-- waitAll : List (StateEffect s e a cmd) -> StateEffect s e (List a) cmd
-- waitAll states =
--       List.foldr (wait2 (::)) (return []) states


-- wait2 : (a -> b -> c) -> StateEffect s e a cmd -> StateEffect s e b cmd -> StateEffect s e c cmd
-- wait2 f sa sb =
--     case ( sa, sb ) of
--         ( Succeed a, Succeed b ) ->
--             Succeed (f a b)

--         _ ->
--             append 
--                 (map2 f sa empty)
--                 (map2 f sa sb)


ap : StateEffect s e (a -> b) cmd -> StateEffect s e a cmd -> StateEffect s e b cmd
ap sf sa =
    andThen (\f -> map f sa) sf


andMap : StateEffect s e a cmd -> StateEffect s e (a -> b) cmd -> StateEffect s e b cmd
andMap sa sf =
    ap sf sa


combine : List (StateEffect s e a cmd) -> StateEffect s e (List a) cmd
combine states =
    List.foldl (map2 (::)) (return []) states



sequence : List (StateEffect s e a cmd) -> StateEffect s e (List a) cmd
sequence = combine
