{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Criterion.Types (reportFile)
import Base
import Bench

import qualified DP.FilteredOnePass
import qualified DP.FilteredOnePassTree
import qualified DP.OnePass
import qualified DP.TwoPass
import qualified Traversal.FilteredHAMT
import qualified Traversal.FilteredPrefixHAMT
import qualified Traversal.IncrementalFPHAMT
import qualified Traversal.FilteredPrefixSet
import qualified Traversal.FilteredSet
import qualified Traversal.FilteredTrie
import qualified Traversal.List
import qualified Traversal.Set
import qualified Traversal.Trie
import qualified Traversal.Heap
import qualified Traversal.NotHeap
import qualified Traversal.FilteredHeap

data SizeClass = Size2 | Size3 | Size4 | Size6 deriving (Eq, Ord, Show)

boards = [ (Size2, mkGridOf ["AW", "OP"])
         , (Size3, mkGridOf ["OCN", "ENN", "VNA"])
         , (Size4, board4x4)
         , (Size6, mkGridOf [ "OAFMPE"
                            , "IDALAA"
                            , "AFTTIW"
                            , "EOPASL"
                            , "SDCWNO"
                            , "NWLSHF"
                            ])
         ]


benches =
  [ (Size4, solveBench Traversal.List.solver "Traversal.List")

  , (Size3, solveBench Traversal.Set.solver "Traversal.Set")
  , (Size3, solveBench Traversal.FilteredSet.solver "Traversal.FilteredSet")
  , (Size3, solveBench Traversal.FilteredHAMT.solver "Traversal.FilteredHAMT")

  , (Size6, solveBench Traversal.FilteredPrefixSet.solver "Traversal.FilteredPrefixSet")
  , (Size6, solveBench Traversal.Trie.solver "Traversal.Trie")
  , (Size6, solveBench Traversal.FilteredTrie.solver "Traversal.FilteredTrie")
  , (Size6, solveBench Traversal.FilteredPrefixHAMT.solver "Traversal.FilteredPrefixHAMT")
  , (Size6, solveBench Traversal.IncrementalFPHAMT.solver "Traversal.IncrementalFPHAMT")
  , (Size6, solveBench Traversal.Heap.solver "Traversal.Heap")
  , (Size6, solveBench Traversal.FilteredHeap.solver "Traversal.FilteredHeap")
  , (Size6, solveBench Traversal.NotHeap.solver "Traversal.NotHeap")

  , (Size6, solveBench DP.TwoPass.solver "DP.TwoPass")
  , (Size6, solveBench DP.OnePass.solver "DP.OnePass")
  , (Size6, solveBench DP.FilteredOnePass.solver "DP.FilteredOnePass")
  , (Size6, solveBench DP.FilteredOnePassTree.solver "DP.FilteredOnePassTree")
  ]

main = do
  !rawDict <- loadDictFile "bench/dict.txt"
  let config = defaultConfig { reportFile = Just "bench-all.html" }
  defaultMainWith config $ flip map boards $ \(speed, board) ->
    bgroup (show speed) $
      flip map (map snd $ filter ((>= speed) . fst) benches) $ \bnch ->
        bnch rawDict board
