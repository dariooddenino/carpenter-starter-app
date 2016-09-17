module Data.StorageKey where

import Components.Task (Task)
import Data.Generic (class Generic)

data StorageKey a
  = TasksKey
  | UidKey

derive instance genericKey :: Generic (StorageKey a)

tasksKey :: StorageKey (Array Task)
tasksKey = TasksKey

uidKey :: StorageKey Int
uidKey = UidKey
