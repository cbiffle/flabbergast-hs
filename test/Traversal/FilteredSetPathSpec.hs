{-# LANGUAGE TypeApplications #-}
module Traversal.FilteredSetPathSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.FilteredSetPath

spec :: Spec
spec = parallel $ genericSpec @Traversal.FilteredSetPath.T 2
