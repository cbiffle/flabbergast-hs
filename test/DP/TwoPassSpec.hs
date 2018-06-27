module DP.TwoPassSpec where

import Test.Hspec
import Checks
import qualified DP.TwoPass

spec :: Spec
spec = parallel $ genericSpec DP.TwoPass.solver 2
