module Traversal.FilteredPrefixSetSpec where

import Test.Hspec
import Checks
import qualified Traversal.FilteredPrefixSet

spec :: Spec
spec = parallel $ genericSpec Traversal.FilteredPrefixSet.solver 2
