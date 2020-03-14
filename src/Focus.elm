module Focus exposing (Focus, get, set, update, compose, create)


type Focus big small =
  Focus
    { get : big -> small
    , update : (small -> small) -> big -> big
    }


create : (big -> small) -> ((small -> small) -> big -> big) -> Focus big small
create getter updater =
  Focus { get = getter, update = updater }


id = 
    Focus { get = identity, update = \fn big -> fn big }


get : Focus big small -> big -> small
get (Focus focus) big =
  focus.get big


set : Focus big small -> small -> big -> big
set (Focus focus) small big =
  focus.update (always small) big


update : Focus big small -> (small -> small) -> big -> big
update (Focus focus) f big =
  focus.update f big



compose : Focus big medium -> Focus medium small -> Focus big small
compose (Focus largerFocus) (Focus smallerFocus) =
  let
    getter big =
      smallerFocus.get (largerFocus.get big)

    updater f big =
      largerFocus.update (smallerFocus.update f) big
  in
    Focus { get = getter, update = updater }
