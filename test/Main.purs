module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Test.Components.Task (testTask)
import Test.Unit.Console (TESTOUTPUT)

main :: ∀ eff. Eff (testOutput :: TESTOUTPUT, dom :: DOM, console :: CONSOLE, avar :: AVAR | eff) Unit
main = do
  testTask
