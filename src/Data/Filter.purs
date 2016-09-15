module Data.Filter where

import Prelude
import Components.Task (Task(Task))

data Filter = All | Active | Completed

derive instance eqFilter :: Eq Filter

instance showFilter :: Show Filter where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

predicate :: Filter -> Task -> Boolean
predicate All _ = true
predicate Active (Task t) = not t.completed
predicate Completed (Task t) = t.completed
