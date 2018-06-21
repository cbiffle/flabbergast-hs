{-# LANGUAGE TypeApplications #-}
module Traversal.FilteredPrefixSetSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.FilteredPrefixSet

spec :: Spec
spec = parallel $ genericSpec @Traversal.FilteredPrefixSet.T 2
