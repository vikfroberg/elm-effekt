module StateEffect exposing (..)

import Effect exposing (Effect(..))
import State exposing (State)


type StateEffect s e a cmd 
    = StateEffect (State s (Effect e a cmd))


run s (StateEffect state) = 
    State.run s state


advance fn = 
    State.advance fn
        |> StateEffect


return a = 
    State.map (always <| Succeed a) State.get
        |> StateEffect


do a b = 
    andThen b a


andThen : (a -> StateEffect s e b cmd) -> StateEffect s e a cmd -> StateEffect s e b cmd
andThen fn (StateEffect state) =
    State.andThen (\effect ->
        case effect of
            Succeed a ->
                let
                    (StateEffect newState) = fn a 
                in
                newState

            Fail e ->
                state
                    |> State.map (Effect.append (Fail e))

            Load cmds ->
                state
                    |> State.map (Effect.append (Load cmds))
    )
    state
        |> StateEffect


map fn =
    andThen (fn >> return)


map2 fn (StateEffect s1) (StateEffect s2) =
    State.map2 (Effect.map2 fn) s1 s2
        |> StateEffect


seq2 fn s1 s2 =
    do s2 <| \eff1 ->
    do s1 <| \eff2 ->
        return (fn eff2 eff1)


sequence states =
    List.foldl (seq2 (::)) (return []) states


combine states =
    List.foldl (map2 (::)) (return []) states


-- race = Debug.todo "implement" 
-- or = Debug.todo "implement"
