{-# LANGUAGE TypeApplications #-}
module DP.FilteredOnePassSpec where

import Test.Hspec
import Base
import Checks
import qualified DP.FilteredOnePass

spec :: Spec
spec = parallel $ genericSpec @DP.FilteredOnePass.T 2
