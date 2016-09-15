module Data.Filter where

import Prelude
import Components.Todo (Todo(Todo))

data Filter = All | Active | Completed

derive instance eqFilter :: Eq Filter

instance showFilter :: Show Filter where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

predicate :: Filter -> Todo -> Boolean
predicate All _ = true
predicate Active (Todo t) = not t.completed
predicate Completed (Todo t) = t.completed
