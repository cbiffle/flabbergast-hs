{-# LANGUAGE TypeApplications #-}
module Traversal.List2Spec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.List2

spec :: Spec
spec = parallel $ genericSpec @Traversal.List2.T 2
