module Traversal.ListSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.List

spec :: Spec
spec = parallel $ genericSpec Traversal.List.solver 2
