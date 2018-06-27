module DP.OnePassSpec where

import Test.Hspec
import Checks
import qualified DP.OnePass

spec :: Spec
spec = parallel $ genericSpec DP.OnePass.solver 2
