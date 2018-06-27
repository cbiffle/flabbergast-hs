module Traversal.FilteredSetSpec where

import Test.Hspec
import Checks
import qualified Traversal.FilteredSet

spec :: Spec
spec = parallel $ genericSpec Traversal.FilteredSet.solver 2
