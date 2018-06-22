{-# LANGUAGE TypeApplications #-}
module Traversal.FilteredPrefixBSHAMTSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.FilteredPrefixBSHAMT

spec :: Spec
spec = parallel $ genericSpec @Traversal.FilteredPrefixBSHAMT.T 2
