{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Weigh

import Control.DeepSeq (force)
import Bench
import Base

import qualified DP.TwoPass
import qualified DP.OnePass
import qualified DP.FilteredOnePass
import qualified DP.FilteredOnePassTree
import qualified Traversal.List
import qualified Traversal.Trie
import qualified Traversal.Set
import qualified Traversal.FilteredSet
import qualified Traversal.FilteredHAMT
import qualified Traversal.FilteredTrie
import qualified Traversal.FilteredPrefixHAMT
import qualified Traversal.FilteredPrefixSet
import qualified Traversal.IncrementalFPHAMT
import qualified Traversal.Heap
import qualified Traversal.FilteredHeap
import qualified Traversal.NotHeap

sfunc :: String -> Solver -> RawDictionary -> Weigh ()
sfunc name solver d = func name (uncurry solver) (force (d, board4x4))

sfunc2 :: String -> Solver -> RawDictionary -> Weigh ()
sfunc2 name solver d = func name (uncurry solver)
                                 (force (d, mkGridOf ["IN", "TE"]))

main = do
  !d <- force <$> loadDictFile "bench/dict.txt"
  mainWith $ do
    setColumns [Case, Allocated, GCs, Live]

    io "dictionary" (fmap force . loadDictFile) "bench/dict.txt"

    sfunc2 "Traversal.List" Traversal.List.solver d

    sfunc "DP.TwoPass" DP.TwoPass.solver d
    sfunc "DP.OnePass" DP.OnePass.solver d
    sfunc "DP.FilteredOnePass" DP.FilteredOnePass.solver d
    sfunc "DP.FilteredOnePassTree" DP.FilteredOnePassTree.solver d
    sfunc "Traversal.Trie" Traversal.Trie.solver d
    sfunc "Traversal.Set" Traversal.Set.solver d
    sfunc "Traversal.FilteredSet" Traversal.FilteredSet.solver d
    sfunc "Traversal.FilteredHAMT" Traversal.FilteredHAMT.solver d
    sfunc "Traversal.FilteredTrie" Traversal.FilteredTrie.solver d
    sfunc "Traversal.FilteredPrefixHAMT" Traversal.FilteredPrefixHAMT.solver d
    sfunc "Traversal.FilteredPrefixSet" Traversal.FilteredPrefixSet.solver d
    sfunc "Traversal.IncrementalFPHAMT" Traversal.IncrementalFPHAMT.solver d
    sfunc "Traversal.Heap" Traversal.Heap.solver d
    sfunc "Traversal.FilteredHeap" Traversal.FilteredHeap.solver d
    sfunc "Traversal.NotHeap" Traversal.NotHeap.solver d
