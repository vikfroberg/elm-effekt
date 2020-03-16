module Control exposing (..)

import Effect exposing (Effect)


type alias Control s e a return = 
   (s -> Effect e a return)


perform : return -> Control s e a return
perform return = 
    \sa -> Effect.perform return


succeed : a -> Control s e a return
succeed a = 
    \s -> Effect.succeed a


fail : e -> Control s e a return
fail e = 
    \s -> Effect.fail e


run : model -> Control model e a ( model, Cmd msg ) -> ( model, Cmd msg )
run s t =
    t s |> Effect.run ( s, Cmd.none )


create : (s -> Control s e a return) -> Control s e a return
create f s =
    f s s


do : Control s e a return -> (a -> Control s e b return) -> Control s e b return
do a b = 
    andThen b a


andThen : (a -> Control s e b return) -> Control s e a return -> Control s e b return
andThen f h =
    \sa ->
        let
            effect =
                h sa
        in
        Effect.andThen (\a -> f a sa) effect


map : (a -> b) -> Control s e a return -> Control s e b return
map f a =
    andThen (f >> succeed) a


combine : List (Control s e a return) -> Control s e (List a) return
combine xs =
      List.foldr (map2 (::)) (succeed []) xs


sequence : List (Control s e a return) -> Control s e (List a) return
sequence = 
    combine


andMap : 
   Control s e a return 
   -> Control s e (a -> b) return 
   -> Control s e b return
andMap fa ff =
    andThen (\f -> map f fa) ff


map2 : 
    (a -> b -> c) 
    -> Control s e a return 
    -> Control s e b return 
    -> Control s e c return
map2 f fa fb =
    map f fa
        |> andMap fb


map3 : 
    (a -> b -> c -> d)
    -> Control s e a return 
    -> Control s e b return 
    -> Control s e c return
    -> Control s e d return
map3 f fa fb fc =
    map f fa
        |> andMap fb
        |> andMap fc


map4 : 
    (a -> b -> c -> d -> e) 
    -> Control s e a return 
    -> Control s e b return 
    -> Control s e c return
    -> Control s e d return
    -> Control s e e return
map4 f fa fb fc fd =
    map f fa
        |> andMap fb
        |> andMap fc
        |> andMap fd


map5 : 
    (a -> b -> c -> d -> e -> f) 
    -> Control s e a return 
    -> Control s e b return 
    -> Control s e c return
    -> Control s e d return
    -> Control s e e return
    -> Control s e f return
map5 f fa fb fc fd fe =
    map f fa
        |> andMap fb
        |> andMap fc
        |> andMap fd
        |> andMap fe
