module DP.FilteredOnePassTreeSpec where

import Test.Hspec
import Checks
import qualified DP.FilteredOnePassTree

spec :: Spec
spec = parallel $ genericSpec DP.FilteredOnePassTree.solver 2
