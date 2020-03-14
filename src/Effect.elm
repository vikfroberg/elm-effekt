module Effect exposing (..)

import Prelude exposing (..)


type Effect e a cmd
    = Succeed a
    | Fail e
    | Load (List cmd)


wait = Load []
pure a = Succeed a
fail e = Fail e
load cmds = Load cmds


append eff acc =
    case acc of
        Load cmds ->
            case eff of
                Load cmds2 ->
                    Load (cmds ++ cmds2)

                Succeed _ ->
                    Load cmds

                Fail _ ->
                    Load cmds

        _ ->
            eff


map fn =
    andThen (fn >> Succeed)


map2 fn eff1 eff2 =
    case ( eff1, eff2 ) of
        ( Load cmds1, Load cmds2 ) ->
            Load (cmds1 ++ cmds2)

        ( _, Load cmds ) ->
            Load cmds

        ( Load cmds, _ ) ->
            Load cmds

        _ ->
            eff2


combine effects =
      List.foldl (map2 (::)) (pure []) effects


do a b = 
    andThen b a


andThen fn effect =
    case effect of
        Succeed a ->
            fn a

        Fail e ->
            Fail e

        Load cmds ->
            Load cmds


withDefault default effect =
    case effect of
        Succeed a ->
            a

        _ ->
            default


toCmds effect = 
    case effect of
        Load cmds ->
            Cmd.batch cmds

        _ ->
            Cmd.batch []
