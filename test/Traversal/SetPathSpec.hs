{-# LANGUAGE TypeApplications #-}
module Traversal.SetPathSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.SetPath

spec :: Spec
spec = parallel $ genericSpec @Traversal.SetPath.T 2
