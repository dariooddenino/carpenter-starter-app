module Main where

import Prelude
import Components.Container (containerComponent)
import Control.Monad.Eff (Eff)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import DOM.Node.Types (Element) as DOM
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)
import React (createFactory, ReactElement)
import ReactDOM (render)

main :: Eff (dom :: DOM.DOM) Unit
main = void $ do
  element <- getAppElement
  render container element
  where
    container :: ReactElement
    container = createFactory containerComponent {}

    getAppElement :: ∀ eff. Eff (dom :: DOM.DOM | eff) DOM.Element
    getAppElement = do
      document <- DOM.window >>= DOM.document
      element <- DOM.querySelector "#app" (DOM.htmlDocumentToParentNode document)
      pure $ unsafePartial fromJust (toMaybe element)
