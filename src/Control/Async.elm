module Control.Async exposing (..)

import Effect exposing (Effect)
import Control exposing (Control)


andMap : 
   Control model e a ( model, Cmd msg ) 
   -> Control model e (a -> b) ( model, Cmd msg ) 
   -> Control model e b ( model, Cmd msg )
andMap fa ff =
    \s ->
        case fa s of
            Effect.Perform ( sa, cmda ) ->
                case ff sa of
                    Effect.Perform ( sb, cmdb ) ->
                        Effect.Perform ( sb, Cmd.batch [ cmda, cmdb ] )

                    _ ->
                        Effect.Perform ( sa, cmda )

            Effect.Succeed a ->
                case ff s of
                    Effect.Succeed f ->
                        Effect.Succeed <| f a

                    Effect.Fail e ->
                        Effect.Fail e

                    Effect.Perform return ->
                        Effect.Perform return

            Effect.Fail e ->
                Effect.Fail e


combine : List (Control model e a ( model, Cmd msg )) -> Control model e (List a) ( model, Cmd msg )
combine xs =
    List.foldr (map2 (::)) (Control.succeed []) xs


map2 : 
    (a -> b -> c) 
    -> Control model e a ( model, Cmd msg ) 
    -> Control model e b ( model, Cmd msg ) 
    -> Control model e c ( model, Cmd msg )
map2 f fa fb =
    Control.map f fa
        |> andMap fb


map3 : 
    (a -> b -> c -> d)
    -> Control model e a ( model, Cmd msg ) 
    -> Control model e b ( model, Cmd msg ) 
    -> Control model e c ( model, Cmd msg )
    -> Control model e d ( model, Cmd msg )
map3 f fa fb fc =
    Control.map f fa
        |> andMap fb
        |> andMap fc


map4 : 
    (a -> b -> c -> d -> e) 
    -> Control model e a ( model, Cmd msg ) 
    -> Control model e b ( model, Cmd msg ) 
    -> Control model e c ( model, Cmd msg )
    -> Control model e d ( model, Cmd msg )
    -> Control model e e ( model, Cmd msg )
map4 f fa fb fc fd =
    Control.map f fa
        |> andMap fb
        |> andMap fc
        |> andMap fd


map5 : 
    (a -> b -> c -> d -> e -> f) 
    -> Control model e a ( model, Cmd msg ) 
    -> Control model e b ( model, Cmd msg ) 
    -> Control model e c ( model, Cmd msg )
    -> Control model e d ( model, Cmd msg )
    -> Control model e e ( model, Cmd msg )
    -> Control model e f ( model, Cmd msg )
map5 f fa fb fc fd fe =
    Control.map f fa
        |> andMap fb
        |> andMap fc
        |> andMap fd
        |> andMap fe
