module Effect exposing (..)


type Effect e a model cmd
    = Perform ( model, List cmd )
    | Fail e
    | Succeed a


pure = 
    Succeed >> always


return = 
    pure


run : (a -> ( model, Cmd msg )) -> (e -> ( model, Cmd msg )) -> Effect e a model (Cmd msg) -> ( model, Cmd msg )
run handleSuccess handleFailure effect =
    case effect of
        Perform ( model, cmdList ) ->
            ( model, Cmd.batch cmdList )

        Succeed a ->
            handleSuccess a

        Fail e ->
            handleFailure e


with : a -> (a -> b) -> b
with a fn =
    fn a


do : (model -> Effect e a model cmd) -> (a -> model -> Effect e b model cmd) -> model -> Effect e b model cmd
do toState cb model =
    case toState model of
        Succeed a -> 
            cb a model

        Perform p ->
            Perform p

        Fail e ->
            Fail e


mapTo : b -> (model -> Effect e a model cmd) -> model -> Effect e b model cmd
mapTo x toState model =
    case toState model of
        Succeed _ ->
            Succeed x

        Perform p ->
            Perform p

        Fail e ->
            Fail e


sequence : (x -> model -> Effect e a model cmd) -> List x -> model -> Effect e (List a) model cmd
sequence toState list model =
    let
        sequence_ acc list_ =
            case acc of
                Succeed xs ->
                    case list_ of
                        [] ->
                            acc

                        x :: rest ->
                            case toState x model of
                                Succeed a ->
                                    sequence_ (Succeed (a :: xs)) rest

                                Fail e ->
                                    Fail e
                                    -- TODO: Should it really fail here?
                                    -- It should probably continue without adding the item
                                    -- sequence_ (Succeed xs) rest

                                Perform p ->
                                    Perform p
                _ ->
                    acc
    in
    sequence_ (Succeed []) list


race toState list model =
    Debug.todo "Not implmented"


all : (x -> model -> Effect e a model cmd) -> List x -> model -> Effect e (List a) model cmd
all toState list initModel =
    List.foldl (\x acc ->
        case acc of
            Perform ( m0, c0 ) ->
                case toState x m0 of
                    Perform ( m1, c1 ) ->
                        Perform ( m1, c0 ++ c1 )

                    _ ->
                        acc

            Succeed ys ->
                case toState x initModel of
                    Perform ( m1, c1 ) ->
                        Perform ( m1, c1 )

                    Succeed y ->
                        Succeed (y :: ys)

                    Fail e ->
                        Fail e
            _ ->
                acc
    )
    (Succeed [])
    list
