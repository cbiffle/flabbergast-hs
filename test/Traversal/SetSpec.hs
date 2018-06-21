{-# LANGUAGE TypeApplications #-}
module Traversal.SetSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.Set

spec :: Spec
spec = genericSpec @Traversal.Set.T 2
