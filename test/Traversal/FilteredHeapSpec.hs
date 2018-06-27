module Traversal.FilteredHeapSpec where

import Test.Hspec
import Checks
import qualified Traversal.FilteredHeap

spec :: Spec
spec = parallel $ genericSpec Traversal.FilteredHeap.solver 2
