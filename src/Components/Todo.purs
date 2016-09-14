module Components.Todo
  ( todoComponent
  , init
  , Todo
  , Action
  ) where

import React as React
import React.DOM as R
import React.DOM.Props as P
import Carpenter (Render, Update)
import Carpenter.Cedar (CedarClass, cedarSpec)
import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.MonadPlus (guard)
import DOM (DOM)
import Data.Maybe (isNothing, isJust, fromMaybe, Maybe(..))
import Data.String (null, trim)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (div)

foreign import focusTodo :: ∀ a e. Int -> Eff (dom :: DOM | e) a

data Action
  = Focus
  | Cancel
  | Edit String
  | Commit
  | Check Boolean
  | Delete

type Todo =
  { id :: Int
  , description :: String
  , completed :: Boolean
  , edits :: Maybe String
  }

todoComponent :: CedarClass (Maybe Todo) Action
todoComponent = React.createClass $ cedarSpec update render

init :: String -> Int -> Todo
init description id = { id: id, description: description, completed: false, edits: Nothing }

update :: ∀ props eff. Update (Maybe Todo) props Action (console :: CONSOLE, dom :: DOM | eff)
update yield action _ state = case action of
  Focus -> do
    yield $ map (\t -> t { edits = Just t.description })
    case state of
      Just todo -> do
        liftEff $ log todo.description
        liftEff $ focusTodo todo.id
      Nothing -> yield id

  Cancel ->
    yield $ map (_ { edits = Nothing })

  Commit -> yield $ const do
    todo <- state
    if isNothing todo.edits
      then pure todo
      else do
        description <- todo.edits
        guard $ not $ null (trim description)
        pure $ todo { description = description, edits = Nothing }

  Edit edit ->
    yield $ map (_ { edits = Just edit })

  Check check ->
    yield $ map (_ { completed = check })

  Delete ->
    yield $ const Nothing

render :: ∀ props. Render (Maybe Todo) props Action
render dispatch props state children = case state of
  Just todo -> renderTodo dispatch props todo children
  Nothing -> R.div' []

renderTodo :: ∀ props. Render Todo props Action
renderTodo dispatch _ todo _ =
  R.li [ P.className classes ]
    -- todo view
    [ R.div [ P.className "view" ]
      -- toggle checkbox
      [ R.input
        [ P.className "toggle"
        , P._type "checkbox"
        , P.checked $ if todo.completed then "checked" else ""
        , P.onChange \_ -> dispatch $ Check (not todo.completed)
        ] []
      -- description
      , R.label
        [ P.onDoubleClick \_ -> dispatch Focus ]
        [ R.text description ]
      -- destroy button
      , R.button [ P.className "destroy", P.onClick \_ -> dispatch Delete ] []
      ]
    -- input field
    , R.input
      [ P.className "edit"
      , P.value description
      , P.name "title"
      , P._id ("todo-" <> show todo.id)
      , P.onChange \e -> dispatch $ Edit (unsafeCoerce e).target.value
      , P.onBlur \_ -> dispatch Commit
      , P.onKeyDown keyDown
      ] []
    ]
  where
    classes = (if todo.completed then "completed " else "") <> (if isJust todo.edits then "editing" else "")
    description = fromMaybe todo.description todo.edits
    keyDown e
      | e.keyCode == 13 = dispatch $ Commit
      | e.keyCode == 27 = dispatch $ Cancel
      | otherwise       = pure unit

