module Components.Task
  ( taskComponent
  , init
  , update
  , Task(..)
  , Action(..)
  ) where

import React as React
import React.DOM as R
import React.DOM.Props as P
import Carpenter (Render, Update)
import Carpenter.Cedar (CedarClass, cedarSpec')
import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.MonadPlus (guard)
import DOM (DOM)
import Data.Generic (gShow, class Generic)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (null, trim)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (div)

foreign import focusTask :: ∀ e. Int -> Eff (dom :: DOM | e) Unit

data Action
  = Focus
  | Cancel
  | Edit String
  | Commit
  | Check Boolean
  | Delete

newtype Task = Task
  { id :: Int
  , description :: String
  , completed :: Boolean
  , edits :: Maybe String
  }

derive instance eqTask :: Eq Task
derive instance genericTask :: Generic Task
instance showTask :: Show Task
  where show = gShow

taskComponent :: CedarClass (Maybe Task) Action
taskComponent = React.createClass $ cedarSpec' Cancel update render

init :: String -> Int -> Task
init description id = Task { id: id, description: description, completed: false, edits: Nothing }

update :: ∀ props eff. Update (Maybe Task) props Action (dom :: DOM | eff)
update _ _ _ _ Nothing = pure Nothing
update yield _ action _ (Just (Task task)) =
  case action of
    Focus -> do
      state <- yield $ map (updateTask \t -> t { edits = Just t.description })
      liftEff $ focusTask task.id
      pure state

    Cancel ->
      yield $ map (updateTask _ { edits = Nothing })

    Commit -> yield $ \state -> do
      Task task <- state
      case task.edits of
        Nothing ->
          pure $ Task task
        Just description -> do
          guard $ not (null (trim description))
          pure $ Task task { description = description, edits = Nothing }

    Edit edit ->
      yield $ map (updateTask _ { edits = Just edit })

    Check check ->
      yield $ map (updateTask _ { completed = check })

    Delete ->
      yield $ const Nothing

  where
    updateTask f (Task t) = Task (f t)

render :: ∀ props. Render (Maybe Task) props Action
render dispatch props state children =
  case state of
    Just task ->
      renderTask dispatch props task children
    Nothing ->
      R.div' children

renderTask :: ∀ props. Render Task props Action
renderTask dispatch _ (Task task) _ =
  R.li [ P.className classes ]
    -- task view
    [ R.div [ P.className "view" ]
      -- toggle checkbox
      [ R.input
        [ P.className "toggle"
        , P._type "checkbox"
        , P.checked $ if task.completed then "checked" else ""
        , P.onChange \_ -> dispatch $ Check (not task.completed)
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
      , P._id ("todo-" <> show task.id)
      , P.onChange \e -> dispatch $ Edit (unsafeCoerce e).target.value
      , P.onBlur \_ -> dispatch Commit
      , P.onKeyDown keyDown
      ] []
    ]
  where
    classes = (if task.completed then "completed " else "") <> (if isJust task.edits then "editing" else "")
    description = fromMaybe task.description task.edits
    keyDown e
      | e.keyCode == 13 = dispatch $ Commit
      | e.keyCode == 27 = dispatch $ Cancel
      | otherwise       = pure unit

