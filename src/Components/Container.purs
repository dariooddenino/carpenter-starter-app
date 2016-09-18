module Components.Container
  ( containerComponent
  , Action
  ) where

import Prelude
import Components.TodoList as TodoList
import Carpenter (Render, spec', Update)
import Carpenter.Cedar (watchAndCapture')
import Components.TodoList (todoListComponent, TodoList)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.WebStorage (setItem, getItem, getLocalStorage, STORAGE)
import Data.StorageKey (uidKey, tasksKey)
import Data.Maybe (fromMaybe)
import React (createClass, ReactClass)

data Action
  = Load
  | TodoListAction TodoList.Action TodoList.TodoList

containerComponent :: ∀ props. ReactClass props
containerComponent = createClass $ spec' (TodoList.init [] 0) Load update render

update :: ∀ props eff. Update TodoList props Action (dom :: DOM, storage :: STORAGE | eff)
update yield _ action _ state = do
  localStorage <- liftEff $ getLocalStorage
  case action of
    Load -> do
      tasks <- liftEff $ getItem localStorage tasksKey
      uid <- liftEff $ getItem localStorage uidKey
      yield $ _ { tasks = fromMaybe [] tasks, uid = fromMaybe 0 uid }

    TodoListAction TodoList.Save todoList -> do
      liftEff $ setItem localStorage tasksKey todoList.tasks
      liftEff $ setItem localStorage uidKey todoList.uid
      pure state

    TodoListAction _ _ -> pure state

render :: ∀ props. Render TodoList props Action
render dispatch _ state _ = watchAndCapture' todoListComponent (\a s -> dispatch $ TodoListAction a s) state
