module Components.TodoList
  ( todoListComponent
  , init
  , TodoList
  , Action
  ) where

import Prelude
import Components.Todo as Todo
import React as React
import React.DOM as R
import React.DOM.Props as P
import Carpenter (Render, Update)
import Carpenter.Cedar (cedarSpec, CedarClass, watch')
import Components.Router (routerComponent)
import Data.Array (null, filter, length, mapMaybe, (:))
import Data.Filter (Filter(..), predicate)
import Data.Foldable (all)
import Data.Maybe (Maybe(Just))
import React (ReactElement)
import Unsafe.Coerce (unsafeCoerce)

data Action
  = Insert String
  | Update Int (Maybe Todo.Todo)
  | CheckAll Boolean
  | ClearCompleted
  | ChangeFilter Filter
  | EditField String
  | UrlChanged String

type TodoList =
  { field :: String
  , tasks :: Array Todo.Todo
  , uid :: Int
  , filter :: Filter
  }

todoListComponent :: CedarClass TodoList Action
todoListComponent = React.createClass $ cedarSpec update render

init :: TodoList
init = { field: "", tasks: [], uid: 0, filter: All }

update :: ∀ props eff. Update TodoList props Action eff
update yield _ action _ _ = case action of
  Insert description ->
    yield $ \s -> s
      { field = ""
      , tasks = (Todo.init description s.uid) : s.tasks
      , uid = s.uid + 1
      }

  Update id taskM -> do
    yield $ \s -> s { tasks = mapMaybe (\t -> if t.id == id then taskM else Just t) s.tasks }

  CheckAll check ->
    yield $ \s -> s { tasks = map (_ { completed = check }) s.tasks }

  ClearCompleted ->
    yield $ \s -> s { tasks = filter (not _.completed) s.tasks }

  ChangeFilter filter ->
    yield $ _ { filter = filter }

  EditField field ->
    yield $ _ { field = field }

  UrlChanged hash -> case hash of
    "#/active" -> yield $ _ { filter = Active }
    "#/completed" -> yield $ _ { filter = Completed }
    _ -> yield $ _ { filter = All }

render :: ∀ props. Render TodoList props Action
render dispatch props state children =
  R.section [ P.className "todoapp" ]
    [ renderHeader dispatch props state children
    , renderList dispatch props state children
    , renderFooter dispatch props state children
    ]

renderHeader :: ∀ props. Render TodoList props Action
renderHeader dispatch _ state _ =
  R.header [ P.className "header" ]
    [ R.h1 [] [ R.text "todos" ]
    , watch' routerComponent (dispatch <<< UrlChanged) ""
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
    , R.ul [ P.className "todo-list" ]
        (map (\t -> watch' Todo.todoComponent (dispatch <<< Update t.id) (Just t)) filteredTasks)
    ]
  where
    allCompleted = all _.completed state.tasks
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
