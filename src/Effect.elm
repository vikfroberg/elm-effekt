module Effect exposing (..)


type Effect e a cmd
    = Succeed a
    | Fail e
    | Perform (List cmd)
    | Empty


-- Constructors


wait : Effect e a cmd
wait = 
    Perform []


succeed : a -> Effect e a cmd
succeed a = 
    Succeed a


fail : e -> Effect e a cmd
fail e = 
    Fail e


perform : List cmd -> Effect e a cmd
perform cmds = 
    Perform cmds


withDefault : a -> Effect e a cmd -> a
withDefault default effect =
    case effect of
        Succeed a ->
            a

        _ ->
            default


toResult : Effect e a cmd -> 


-- onError


-- Monoid


empty : Effect e a cmd
empty = Empty


concat : List (Effect e a cmd) -> Effect e a cmd
concat es =
    List.foldr append empty es


append : Effect e a cmd -> Effect e a cmd -> Effect e a cmd
append eff acc =
    case acc of
        Empty ->
            eff

        Fail e ->
            Fail e

        Succeed a ->
            case eff of
                Perform cmds ->
                    Perform cmds

                _ ->
                    Succeed a

        Perform cmds ->
            case eff of
                Perform cmds2 ->
                    Perform (cmds ++ cmds2)

                Succeed _ ->
                    Perform cmds

                Fail _ ->
                    Perform cmds

                Empty ->
                    Perform cmds


fold : { concat : List cmd -> cmds, empty : cmds } -> Effect e a cmd -> cmds
fold monoid effect = 
    case effect of
        Perform cmds ->
            monoid.concat cmds

        _ ->
            monoid.empty

-- Functor


map : (a -> b) -> Effect e a cmd -> Effect e b cmd
map f =
    andThen (f >> Succeed)


mapError : (e -> r) -> Effect e a cmd -> Effect r a cmd
mapError f a =
    onError (f >> fail) a


-- Applicative


ap : Effect e (a -> b) cmd -> Effect e a cmd -> Effect e b cmd
ap ef ea =
    andThen (\f -> map f ea) ef


andMap : Effect e a cmd -> Effect e (a -> b) cmd -> Effect e b cmd
andMap sa sf =
    ap sf sa


map2 : 
    (a -> b -> c) 
    -> Effect e a cmd 
    -> Effect e b cmd 
    -> Effect e c cmd
map2 f a b =
    map f a
        |> andMap b
        

map3 : 
    (a -> b -> c -> d) 
    -> Effect e a cmd 
    -> Effect e b cmd 
    -> Effect e c cmd
    -> Effect e d cmd
map3 f a b c =
    map f a
        |> andMap b
        |> andMap c


map4 : 
    (a -> b -> c -> d -> e) 
    -> Effect e a cmd 
    -> Effect e b cmd 
    -> Effect e c cmd
    -> Effect e d cmd
    -> Effect e e cmd
map4 f a b c d =
    map f a
        |> andMap b
        |> andMap c
        |> andMap d


map5 : 
    (a -> b -> c -> d -> e -> f) 
    -> Effect e a cmd 
    -> Effect e b cmd 
    -> Effect e c cmd
    -> Effect e d cmd
    -> Effect e e cmd
    -> Effect e f cmd
map5 f a b c d e =
    map f a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e


waitAll : List (Effect e a cmd) -> Effect e (List a) cmd
waitAll es =
      List.foldr (wait2 (::)) (succeed []) es


wait2 : (a -> b -> c) -> Effect e a cmd -> Effect e b cmd -> Effect e c cmd
wait2 f ea eb =
    case ( ea, eb ) of
        ( Succeed a, Succeed b ) ->
            Succeed (f a b)

        _ ->
            append 
                (map2 f ea empty)
                (map2 f ea eb)


combine : List (Effect e a cmd) -> Effect e (List a) cmd
combine eas =
      List.foldr (map2 (::)) (succeed []) eas


sequence : List (Effect e a cmd) -> Effect e (List a) cmd
sequence = combine


-- Monad


onError : (e -> Effect r a cmd) -> Effect e a cmd -> Effect r a cmd
onError f ea =
    case ea of
        Empty ->
            Empty

        Succeed a ->
            Succeed a

        Fail e ->
            f e

        Perform cmds ->
            Perform cmds


andThen : (a -> Effect e b cmd) -> Effect e a cmd ->  Effect e b cmd
andThen f ea =
    case ea of
        Empty ->
            Empty

        Succeed a ->
            f a

        Fail e ->
            Fail e

        Perform cmds ->
            Perform cmds


do : Effect e a cmd -> (a -> Effect e b cmd) -> Effect e b cmd
do ea fn = 
    andThen fn ea
