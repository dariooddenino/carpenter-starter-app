module Components.Router
  ( routerComponent
  , Action(..)
  , State
  ) where

import Prelude
import React as React
import React.DOM as R
import Carpenter (Render, Update)
import Carpenter.Cedar (cedarSpec', CedarClass)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM) as DOM
import DOM.Event.Event (preventDefault) as DOM
import DOM.Event.EventTarget (addEventListener, eventListener) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Event.EventTypes (hashchange, load) as DOM
import DOM.HTML.Location (hash) as DOM
import DOM.HTML.Types (windowToEventTarget) as DOM
import DOM.HTML.Window (location) as DOM

data Action
  = Init
  | Change String

type State = String

routerComponent :: CedarClass State Action
routerComponent = React.createClass $ cedarSpec' Init update render

update :: ∀ props eff. Update State props Action (dom :: DOM.DOM | eff)
update yield dispatch action _ _ = case action of
  Init -> do
    window <- liftEff $ DOM.window
    liftEff $ DOM.addEventListener (DOM.hashchange) (DOM.eventListener onHashChange) false (DOM.windowToEventTarget window)
    liftEff $ DOM.addEventListener (DOM.load) (DOM.eventListener onHashChange) false (DOM.windowToEventTarget window)
    hash <- liftEff $ DOM.location window >>= DOM.hash
    yield $ const hash
  Change hash ->
    yield $ const hash
  where
    onHashChange e = void $ do
      DOM.preventDefault e
      hash <- DOM.window >>= DOM.location >>= DOM.hash
      dispatch $ Change hash

render :: ∀ props. Render State props Action
render _ _ _ _ = R.div' []
