module Traversal.FilteredPrefixHAMTSpec where

import Test.Hspec
import Checks
import qualified Traversal.FilteredPrefixHAMT

spec :: Spec
spec = parallel $ genericSpec Traversal.FilteredPrefixHAMT.solver 2
