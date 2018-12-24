module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Test.Spec.Data as Data
import Test.Spec.Capability as Capability

main ∷ Effect Unit
main = run [consoleReporter] do
  Data.spec
  Capability.spec
