module Components.TodoList.Storage where

import Components.Task (Task)
import Data.Generic (class Generic)

data Key a
  = TasksKey
  | UidKey

derive instance genericKey :: Generic (Key a)

tasksKey :: Key (Array Task)
tasksKey = TasksKey

uidKey :: Key Int
uidKey = UidKey
