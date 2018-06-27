module Traversal.HeapSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.Heap

spec :: Spec
spec = parallel $ genericSpec Traversal.Heap.solver 2
