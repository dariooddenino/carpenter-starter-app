module Test.Components.TodoList where

import Prelude
import Components.Task as Task
import Components.TodoList as Todo
import Test.Unit.Assert as Assert
import Carpenter.Cedar (mockUpdate)
import Components.Task (Task(..))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Test.Unit (test, suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

testTodoList :: ∀ eff. Eff (testOutput :: TESTOUTPUT, dom :: DOM, console :: CONSOLE, avar :: AVAR | eff) Unit
testTodoList = runTest do

  suite "TodoList" do

    test "should insert tasks" do
      let l = todoList
          expected = todoList { tasks = (Task.init "Dolor" 2) : todoList.tasks, uid = 3 }

      l' <- update (Todo.Insert "Dolor") l

      Assert.equal expected.tasks l'.tasks
      Assert.equal expected.uid   l'.uid

    test "should insert tasks from text field" do
      let l = todoList
          expected = todoList { tasks = (Task.init "Dolor" 2) : todoList.tasks, uid = 3 }

      l' <- update (Todo.EditField "Dolor") l >>= \l'' -> update (Todo.Insert l''.field) l''

      Assert.equal expected.tasks l'.tasks
      Assert.equal expected.uid   l'.uid

    test "should update tasks" do
      let l = todoList
          expected = Todo.init [Task.init "Ipsum" 1, Task.init "Ipsum" 0] 2

      l' <- update (Todo.Update 0 (Just $ Task.init "Ipsum" 0)) l

      Assert.equal expected.tasks l'.tasks
      Assert.equal expected.uid   l'.uid

    test "should remove tasks" do
      let l = todoList
          expected = Todo.init [Task.init "Ipsum" 1] 2

      l' <- update (Todo.Update 0 Nothing) l

      Assert.equal expected.tasks l'.tasks
      Assert.equal expected.uid   l'.uid

    test "should clear completed tasks" do
      let l = todoList
          expected = Todo.init [Task.init "Ipsum" 1] 2

      l' <- update (Todo.Update 0 (Just $ Task { id: 0, description: "Lorem", completed: true, edits: Nothing })) l
        >>= update (Todo.ClearCompleted)

      Assert.equal expected.tasks l'.tasks
      Assert.equal expected.uid   l'.uid

    test "should check all tasks" do
      let l = todoList
          expected = Todo.init [Task { id: 1, description: "Ipsum", completed: true, edits: Nothing }, Task { id: 0, description: "Lorem", completed: true, edits: Nothing }] 2

      l' <- update (Todo.CheckAll true) l

      Assert.equal expected.tasks l'.tasks
      Assert.equal expected.uid   l'.uid

    test "should be empty after checking all and removing completed tasks" do
      let l = todoList
          expected = Todo.init [] 2

      l' <- update (Todo.CheckAll true) l
        >>= update (Todo.ClearCompleted)

      Assert.equal expected.tasks l'.tasks
      Assert.equal expected.uid   l'.uid

  where
    update = mockUpdate Todo.update

    todoList = Todo.init [Task.init "Ipsum" 1, Task.init "Lorem" 0] 2
