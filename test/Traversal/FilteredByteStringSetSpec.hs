{-# LANGUAGE TypeApplications #-}
module Traversal.FilteredByteStringSetSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.FilteredByteStringSet

spec :: Spec
spec = parallel $ genericSpec @Traversal.FilteredByteStringSet.T 2
