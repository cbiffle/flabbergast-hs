{-# LANGUAGE TypeApplications #-}
module DP.OnePassSinglePathVecSpec where

import Test.Hspec
import Base
import Checks
import qualified DP.OnePassSinglePathVec

spec :: Spec
spec = parallel $ genericSpec @DP.OnePassSinglePathVec.T 2
