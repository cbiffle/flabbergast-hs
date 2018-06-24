{-# LANGUAGE TypeApplications #-}
module Traversal.IncrementalFPHAMTSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.IncrementalFPHAMT

spec :: Spec
spec = parallel $ genericSpec @Traversal.IncrementalFPHAMT.T 2
