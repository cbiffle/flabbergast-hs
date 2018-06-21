{-# LANGUAGE TypeApplications #-}
module DP.OnePassSinglePathVecSpec where

import Test.Hspec
import Base
import Checks
import qualified DP.OnePassSinglePathVec

spec :: Spec
spec = genericSpec @DP.OnePassSinglePathVec.T 2
