{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Criterion.Types (reportFile)
import Control.DeepSeq (force, NFData)
import Paths_flabbergast
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
  [ (Size4, solveBench @Traversal.List.T "Traversal.List")

  , (Size3, solveBench @Traversal.Set.T "Traversal.Set")
  , (Size3, solveBench @Traversal.FilteredSet.T "Traversal.FilteredSet")
  , (Size3, solveBench @Traversal.FilteredHAMT.T "Traversal.FilteredHAMT")

  , (Size4, solveBench @Traversal.FilteredPrefixSet.T "Traversal.FilteredPrefixSet")
  , (Size4, solveBench @Traversal.Trie.T "Traversal.Trie")
  , (Size4, solveBench @Traversal.FilteredTrie.T "Traversal.FilteredTrie")
  , (Size4, solveBench @Traversal.FilteredPrefixHAMT.T "Traversal.FilteredPrefixHAMT")
  , (Size4, solveBench @Traversal.IncrementalFPHAMT.T "Traversal.IncrementalFPHAMT")
  , (Size6, solveBench @Traversal.Heap.T "Traversal.Heap")
  , (Size6, solveBench @Traversal.Heap.T "Traversal.NotHeap")

  , (Size4, solveBench @DP.TwoPass.T "DP.TwoPass")
  , (Size4, solveBench @DP.OnePass.T "DP.OnePass")
  , (Size4, solveBench @DP.FilteredOnePass.T "DP.FilteredOnePass")
  , (Size4, solveBench @DP.FilteredOnePassTree.T "DP.FilteredOnePassTree")
  ]

main = do
  !rawDict <- loadDictFile "bench/dict.txt"
  let config = defaultConfig { reportFile = Just "bench-all.html" }
  defaultMainWith config $ flip map boards $ \(speed, board) ->
    bgroup (show speed) $
      flip map (map snd $ filter ((>= speed) . fst) benches) $ \bnch ->
        bnch rawDict board
