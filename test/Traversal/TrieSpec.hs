{-# LANGUAGE TypeApplications #-}
module Traversal.TrieSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.Trie

spec :: Spec
spec = genericSpec @Traversal.Trie.T 2
