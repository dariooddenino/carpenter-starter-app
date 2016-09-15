module Components.Container where

import Prelude
import Components.TodoList as TodoList
import Carpenter (Render, spec', Update)
import Carpenter.Cedar (capture')
import Components.Todo (Todo)
import Components.TodoList (todoListComponent)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.WebStorage (setItem, getItem, getLocalStorage, STORAGE)
import Data.Maybe (fromMaybe)
import React (createClass, ReactClass)
import Storage (uidKey, tasksKey)

data Action
  = Load
  | TodoListAction TodoList.Action

type State =
  { tasks :: Array Todo
  , uid :: Int
  }

containerComponent :: ∀ props. ReactClass props
containerComponent = createClass $ spec' { tasks: [], uid: 0 } Load update render

update :: ∀ props eff. Update State props Action (dom :: DOM, storage :: STORAGE | eff)
update yield _ action _ state = case action of
  Load -> do
    state <- liftEff (do
      localStorage <- getLocalStorage
      tasks <- getItem localStorage tasksKey
      uid <- getItem localStorage uidKey
      pure { tasks: fromMaybe [] tasks, uid: fromMaybe 0 uid }
    )
    yield $ const state

  TodoListAction tlaction -> case tlaction of
    TodoList.Save todoList -> do
      liftEff (do
        localStorage <- getLocalStorage
        setItem localStorage tasksKey todoList.tasks
        setItem localStorage uidKey todoList.uid
      )
      pure state
    _ ->
      pure state

render :: ∀ props. Render State props Action
render dispatch _ state _ = capture' todoListComponent (dispatch <<< TodoListAction) (TodoList.init state.tasks state.uid)
