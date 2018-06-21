{-# LANGUAGE TypeApplications #-}
module DP.TwoPassSpec where

import Test.Hspec
import Base
import Checks
import qualified DP.TwoPass

spec :: Spec
spec = genericSpec @DP.TwoPass.T 2
