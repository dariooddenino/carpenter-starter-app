module Storage where

import Components.Todo (Todo)
import Data.Generic (class Generic)

data TodoListKey a = TodoListKey

derive instance genericTodoListKey :: Generic (TodoListKey a)

todoListKey :: TodoListKey (Array Todo)
todoListKey = TodoListKey
