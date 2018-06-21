{-# LANGUAGE TypeApplications #-}
module DP.OnePassSpec where

import Test.Hspec
import Base
import Checks
import qualified DP.OnePass

spec :: Spec
spec = parallel $ genericSpec @DP.OnePass.T 2
