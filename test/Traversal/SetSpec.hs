module Traversal.SetSpec where

import Test.Hspec
import Checks
import qualified Traversal.Set

spec :: Spec
spec = parallel $ genericSpec Traversal.Set.solver 2
