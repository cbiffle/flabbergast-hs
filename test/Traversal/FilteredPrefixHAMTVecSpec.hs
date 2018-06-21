{-# LANGUAGE TypeApplications #-}
module Traversal.FilteredPrefixHAMTVecSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.FilteredPrefixHAMTVec

spec :: Spec
spec = parallel $ genericSpec @Traversal.FilteredPrefixHAMTVec.T 2
