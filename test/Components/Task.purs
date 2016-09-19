module Test.Components.Task where

import Prelude
import Components.Task as Task
import Test.Unit.Assert as Assert
import Carpenter.Cedar (mockUpdate)
import Components.Task (Task(Task))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Test.Unit (test, suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

testTask :: ∀ eff. Eff (testOutput :: TESTOUTPUT, dom :: DOM, console :: CONSOLE, avar :: AVAR | eff) Unit
testTask = unsafePartial $ runTest do

  suite "Task" do

    test "should set edits to description on focus" do
      let t = Just (Task.init "Lorem" 1)
          expected = updateTask (_ { edits = Just "Lorem" }) t
      t' <- update Task.Focus t
      Assert.equal expected t'

    test "should change description after commiting edits" do
      let t = Just (Task.init "Lorem" 1)
          expected = Just (Task.init "Ipsum" 1)
      t' <- update (Task.Edit "Ipsum") t >>= update Task.Commit
      Assert.equal expected t'

    test "should change description after commiting edits" do
      let t = Just (Task.init "Lorem" 1)
          expected = t
      t' <- update (Task.Edit "Ipsum") t >>= update Task.Cancel
      Assert.equal expected t'

    test "should yield Nothing for any action after Delete" do
      let t = Just (Task.init "Lorem" 1)
          expected = Nothing

      deleted <- update (Task.Delete) t

      deleted'1 <- update (Task.Edit "Ipsum") deleted
      Assert.equal expected deleted'1

      deleted'2 <- update (Task.Edit "Ipsum") deleted >>= update Task.Commit
      Assert.equal expected deleted'2

      deleted'3 <- update (Task.Check true) deleted
      Assert.equal expected deleted'3

      deleted'4 <- update Task.Delete deleted
      Assert.equal expected deleted'4

  where
    update = mockUpdate Task.update
    updateTask f = map \(Task t) -> Task (f t)
