{-# LANGUAGE TypeApplications #-}
module Traversal.FilteredPrefixHAMTSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.FilteredPrefixHAMT

spec :: Spec
spec = genericSpec @Traversal.FilteredPrefixHAMT.T 2
