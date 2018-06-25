{-# LANGUAGE TypeApplications #-}
module DP.FilteredOnePassTreeSpec where

import Test.Hspec
import Base
import Checks
import qualified DP.FilteredOnePassTree

spec :: Spec
spec = parallel $ genericSpec @DP.FilteredOnePassTree.T 2
