module Traversal.NotHeapSpec where

import Test.Hspec
import Checks
import qualified Traversal.NotHeap

spec :: Spec
spec = parallel $ genericSpec Traversal.NotHeap.solver 2
