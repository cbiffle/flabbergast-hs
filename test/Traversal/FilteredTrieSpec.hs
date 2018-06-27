module Traversal.FilteredTrieSpec where

import Test.Hspec
import Checks
import qualified Traversal.FilteredTrie

spec :: Spec
spec = parallel $ genericSpec Traversal.FilteredTrie.solver 2
