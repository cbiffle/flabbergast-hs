module Traversal.FilteredHAMTSpec where

import Test.Hspec
import Checks
import qualified Traversal.FilteredHAMT

spec :: Spec
spec = parallel $ genericSpec Traversal.FilteredHAMT.solver 2
