{-# LANGUAGE TypeApplications #-}
module DP.Filtered1PVBSSpec where

import Test.Hspec
import Base
import Checks
import qualified DP.Filtered1PVBS

spec :: Spec
spec = parallel $ genericSpec @DP.Filtered1PVBS.T 2
