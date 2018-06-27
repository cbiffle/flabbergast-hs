module Traversal.IncrementalFPHAMTSpec where

import Test.Hspec
import Checks
import qualified Traversal.IncrementalFPHAMT

spec :: Spec
spec = parallel $ genericSpec Traversal.IncrementalFPHAMT.solver 2
