{-# LANGUAGE TypeApplications #-}
module Traversal.FilteredTrieSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.FilteredTrie

spec :: Spec
spec = genericSpec @Traversal.FilteredTrie.T 2
