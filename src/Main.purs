module Main where

import Prelude
import Carpenter.Cedar (ignore')
import Components.Todo (Todo)
import Components.TodoList (init, todoListComponent)
import Control.Monad.Eff (Eff)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import DOM.Node.Types (Element) as DOM
import DOM.WebStorage (getItem, getLocalStorage, STORAGE)
import Data.Maybe (fromMaybe, fromJust)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)
import React (ReactElement)
import ReactDOM (render)
import Storage (todoListKey)

main :: Eff (dom :: DOM.DOM, storage :: STORAGE) Unit
main = void $ do
  localStorage <- getLocalStorage
  container <- getContainer
  tasks <- getItem localStorage todoListKey
  render (todoList (fromMaybe [] tasks)) container
  where
    todoList :: Array Todo -> ReactElement
    todoList tasks = ignore' todoListComponent (init tasks)

    getContainer :: ∀ eff. Eff (dom :: DOM.DOM | eff) DOM.Element
    getContainer = do
      document <- DOM.window >>= DOM.document
      element <- DOM.querySelector "#app" (DOM.htmlDocumentToParentNode document)
      pure $ unsafePartial fromJust (toMaybe element)
