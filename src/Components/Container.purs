module Components.Container where

import Prelude
import Carpenter (spec, Render, Update)
import React (createClass, ReactClass)
import React.DOM (text, button, h1', div')
import React.DOM.Props (onClick)

type State = Int

data Action = Increment | Decrement

containerComponent :: forall props. ReactClass props
containerComponent = createClass $ spec 0 update render

update :: forall props eff. Update State props Action eff
update yield _ action _ _ =
  case action of
    Increment -> yield (_ + 1)
    Decrement -> yield (_ - 1)

render :: forall props. Render State props Action
render dispatch _ state _ =
  div'
    [ h1' [ text (show state) ]
    , button [ onClick \_ -> dispatch Increment ] [ text "+" ]
    , button [ onClick \_ -> dispatch Decrement ] [ text "-" ]
    ]
