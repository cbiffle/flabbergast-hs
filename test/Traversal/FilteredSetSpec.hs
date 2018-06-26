{-# LANGUAGE TypeApplications #-}
module Traversal.FilteredSetSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.FilteredSet

spec :: Spec
spec = parallel $ genericSpec @Traversal.FilteredSet.T 2
