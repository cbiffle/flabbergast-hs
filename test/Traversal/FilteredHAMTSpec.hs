module Traversal.FilteredHAMTSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.FilteredHAMT

spec :: Spec
spec = parallel $ genericSpec Traversal.FilteredHAMT.solver 2
