module Main where

import Prelude
import Carpenter.Cedar (ignore')
import Components.TodoList (todoListComponent, init)
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
import React (ReactElement)
import ReactDOM (render)

main :: Eff (dom :: DOM.DOM) Unit
main = void $ getContainer >>= render todoList
  where
    todoList :: ReactElement
    todoList = ignore' todoListComponent init

    getContainer :: Eff (dom :: DOM.DOM) DOM.Element
    getContainer = do
      document <- DOM.window >>= DOM.document
      element <- DOM.querySelector "#app" (DOM.htmlDocumentToParentNode document)
      pure $ unsafePartial fromJust (toMaybe element)
