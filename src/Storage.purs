module Storage where

import Components.Todo (Todo)
import Data.Generic (class Generic)

data Key a
  = TasksKey
  | UidKey

derive instance genericTodoListKey :: Generic (Key a)

tasksKey :: Key (Array Todo)
tasksKey = TasksKey

uidKey :: Key Int
uidKey = UidKey
