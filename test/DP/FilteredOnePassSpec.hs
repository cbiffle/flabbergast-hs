module DP.FilteredOnePassSpec where

import Test.Hspec
import Checks
import qualified DP.FilteredOnePass

spec :: Spec
spec = parallel $ genericSpec DP.FilteredOnePass.solver 2
