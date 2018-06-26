{-# LANGUAGE TypeApplications #-}
module Traversal.NotHeapSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.NotHeap

spec :: Spec
spec = parallel $ genericSpec @Traversal.NotHeap.T 2