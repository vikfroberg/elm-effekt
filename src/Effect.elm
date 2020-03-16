module Effect exposing (..)


type Effect e a return
    = Succeed a
    | Fail e
    | Perform return


run : return -> Effect e a return -> return
run default fa =
    case fa of
        Perform return ->
            return

        _ ->
            default


-- Constructors


succeed : a -> Effect e a return
succeed a = 
    Succeed a


fail : e -> Effect e a return
fail e = 
    Fail e


perform : return -> Effect e a return
perform return = 
    Perform return


-- Helpers


withDefault : a -> Effect e a return -> a
withDefault default fa =
    case fa of
        Succeed a ->
            a

        _ ->
            default


-- Functor


map : (a -> b) -> Effect e a return -> Effect e b return
map f =
    andThen (f >> Succeed)


mapError : (e -> r) -> Effect e a return -> Effect r a return
mapError f fa =
    onError (f >> Fail) fa


-- Applicative


andMap : Effect e a return -> Effect e (a -> b) return -> Effect e b return
andMap fa ff =
    andThen (\f -> map f fa) ff


map2 : 
    (a -> b -> c) 
    -> Effect e a return 
    -> Effect e b return 
    -> Effect e c return
map2 f a b =
    map f a
        |> andMap b
        

map3 : 
    (a -> b -> c -> d) 
    -> Effect e a return 
    -> Effect e b return 
    -> Effect e c return
    -> Effect e d return
map3 f a b c =
    map f a
        |> andMap b
        |> andMap c


map4 : 
    (a -> b -> c -> d -> e) 
    -> Effect e a return 
    -> Effect e b return 
    -> Effect e c return
    -> Effect e d return
    -> Effect e e return
map4 f a b c d =
    map f a
        |> andMap b
        |> andMap c
        |> andMap d


map5 : 
    (a -> b -> c -> d -> e -> f) 
    -> Effect e a return 
    -> Effect e b return 
    -> Effect e c return
    -> Effect e d return
    -> Effect e e return
    -> Effect e f return
map5 f a b c d e =
    map f a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e


combine : List (Effect e a return) -> Effect e (List a) return
combine xs =
      List.foldr (map2 (::)) (succeed []) xs


-- Monad


onError : (e -> Effect r a return) -> Effect e a return -> Effect r a return
onError f fa =
    case fa of
        Succeed a ->
            Succeed a

        Fail e ->
            f e

        Perform return ->
            Perform return


andThen : (a -> Effect e b return) -> Effect e a return ->  Effect e b return
andThen f fa =
    case fa of
        Succeed a ->
            f a

        Fail e ->
            Fail e

        Perform return ->
            Perform return
