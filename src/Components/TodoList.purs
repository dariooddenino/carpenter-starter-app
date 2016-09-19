module Components.TodoList
  ( todoListComponent
  , init
  , update
  , TodoList
  , Action(..)
  ) where

import Prelude
import React as React
import React.DOM as R
import React.DOM.Props as P
import Carpenter (Render, Update)
import Carpenter.Cedar (cedarSpec, CedarClass, watch')
import Carpenter.Router (Route, match, (:->), router)
import Components.Task (taskComponent, Task(Task))
import Components.Task (init) as Task
import Control.Monad.Eff.Class (liftEff)
import Data.Array (null, filter, length, mapMaybe, (:))
import Data.Filter (Filter(..), predicate)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import React (ReactElement)
import Unsafe.Coerce (unsafeCoerce)

data Action
  = Insert String
  | Update Int (Maybe Task)
  | CheckAll Boolean
  | ClearCompleted
  | ChangeFilter Filter
  | EditField String
  | Save

type TodoList =
  { field :: String
  , tasks :: Array Task
  , uid :: Int
  , filter :: Filter
  }

todoListComponent :: CedarClass TodoList Action
todoListComponent = React.createClass $ cedarSpec update render

init :: Array Task -> Int -> TodoList
init tasks uid = { field: "", tasks: tasks, uid: uid, filter: All }

routes :: Route -> Filter
routes = match
  [ "#/all" :-> All
  , "#/active" :-> Active
  , "#/completed" :-> Completed
  ] All

update :: ∀ props eff. Update TodoList props Action eff
update yield dispatch action _ _ =
  case action of
    Insert description -> do
      state <- yield $ \s -> s
        { field = ""
        , tasks = (Task.init description s.uid) : s.tasks
        , uid = s.uid + 1
        }
      save state

    Update id taskM -> do
      state <- yield $ \s -> s { tasks = mapMaybe (\(Task t) -> if t.id == id then taskM else Just (Task t)) s.tasks }
      save state

    CheckAll check -> do
      state <- yield $ \s -> s { tasks = map (\(Task t) -> Task t { completed = check }) s.tasks }
      save state

    ClearCompleted -> do
      state <- yield $ \s -> s { tasks = filter (\(Task t) -> not t.completed) s.tasks }
      save state

    ChangeFilter filter -> do
      yield $ _ { filter = filter }

    EditField field -> do
      yield $ _ { field = field }

    Save ->
      yield id

  where
    save state = liftEff $ dispatch Save *> pure state

render :: ∀ props. Render TodoList props Action
render dispatch props state children =
  R.section [ P.className "todoapp" ]
    [ router (dispatch <<< ChangeFilter) routes
    , renderHeader dispatch props state children
    , renderList dispatch props state children
    , renderFooter dispatch props state children
    ]

renderHeader :: ∀ props. Render TodoList props Action
renderHeader dispatch _ state _ =
  R.header [ P.className "header" ]
    [ R.h1 [] [ R.text "todos" ]
    , R.input
      [ P.className "new-todo"
      , P.placeholder "What needs to be done?"
      , P.value state.field
      , P.autoFocus true
      , P.onInput \e -> dispatch $ EditField (unsafeCoerce e).target.value
      , P.onKeyDown keyDown
      ] []
    ]
  where
    keyDown e
      | e.keyCode == 13 = dispatch $ Insert state.field
      | otherwise       = pure unit

renderList :: ∀ props. Render TodoList props Action
renderList dispatch _ state _ =
  R.section
    [ P.className "main"
    , P.style { display: if null state.tasks then "none" else "inherit" }
    ]
    -- toggle all checkbox
    [ R.input
      [ P.className "toggle-all"
      , P._type "checkbox"
      , P.checked $ if allCompleted then "checked" else ""
      , P.onChange \_ -> dispatch $ CheckAll (not allCompleted)
      ] []
    , R.label [ P.htmlFor "toggle-all" ] [ R.text "Mark all as complete" ]
    -- filtered tasks
    , R.ul [ P.className "todo-list" ] (map todo filteredTasks)
    ]
  where
    todo t = watch' taskComponent (dispatch <<< Update (getTodoId t)) (Just t)
    getTodoId (Task t) = t.id

    allCompleted = all (predicate Completed) state.tasks
    filteredTasks = filter (predicate state.filter) state.tasks

renderFooter :: ∀ props. Render TodoList props Action
renderFooter dispatch _ state _ =
  R.footer
    [ P.className "footer"
    , P.style { display: if null state.tasks then "none" else "inherit" }
    ]
    -- completed tasks
    [ R.span [ P.className "todo-count" ]
      [ R.strong [] [ R.text $ show activeTasks ]
      , R.text (if activeTasks == 1 then " item left" else " item left")
      ]
    -- tasks filter
    , R.ul [ P.className "filters" ]
      [ filterButton All "#/"
      , filterButton Active "#/active"
      , filterButton Completed "#/completed"
      ]
    -- clear completed button
    , R.button
      [ P.className "clear-completed"
      , P.style { display: if completedTasks == 0 then "none" else "inherit" }
      , P.onClick \_ -> dispatch ClearCompleted
      ]
      [ R.text "Clear completed" ]
    ]
  where
    completedTasks = length $ filter (predicate Completed) state.tasks
    activeTasks = length state.tasks - completedTasks
    isSelected f = if state.filter == f then [ P.className "selected" ] else []

    filterButton :: Filter -> String -> ReactElement
    filterButton filter url =
      R.li
        [ P.onClick \_ -> dispatch $ ChangeFilter filter ]
        [ R.a
          [ P.className if filter == state.filter then "selected" else ""
          , P.href url
          ]
          [ R.text (show filter) ]
        ]
