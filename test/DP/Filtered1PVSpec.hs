{-# LANGUAGE TypeApplications #-}
module DP.Filtered1PVSpec where

import Test.Hspec
import Base
import Checks
import qualified DP.Filtered1PV

spec :: Spec
spec = parallel $ genericSpec @DP.Filtered1PV.T 2
