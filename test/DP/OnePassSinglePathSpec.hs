{-# LANGUAGE TypeApplications #-}
module DP.OnePassSinglePathSpec where

import Test.Hspec
import Base
import Checks
import qualified DP.OnePassSinglePath

spec :: Spec
spec = parallel $ genericSpec @DP.OnePassSinglePath.T 2
