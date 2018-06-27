module Traversal.TrieSpec where

import Test.Hspec
import Base
import Checks
import qualified Traversal.Trie

spec :: Spec
spec = parallel $ genericSpec Traversal.Trie.solver 2
