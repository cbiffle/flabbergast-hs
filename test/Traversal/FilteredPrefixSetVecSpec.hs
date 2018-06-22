{-# LANGUAGE TypeApplications #-}
module Traversal.FilteredPrefixSetVecSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.FilteredPrefixSetVec

spec :: Spec
spec = parallel $ genericSpec @Traversal.FilteredPrefixSetVec.T 2