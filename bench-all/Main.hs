{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Criterion.Types (reportFile)
import Control.DeepSeq (force, NFData)
import Paths_boggle
import Base
import Bench

import qualified DP.Filtered1PV
import qualified DP.OnePass
import qualified DP.OnePassSinglePath
import qualified DP.TwoPass
import qualified Traversal.FilteredPrefixHAMT
import qualified Traversal.FilteredPrefixSet
import qualified Traversal.FilteredSet
import qualified Traversal.FilteredTrie
import qualified Traversal.List
import qualified Traversal.Set
import qualified Traversal.Trie

data SizeClass = Size2 | Size3 | Size4 deriving (Eq, Show)

boards = [ (Size2, mkGridOf ["AW", "OP"])
         , (Size3, mkGridOf ["OCN", "ENN", "VNA"])
         , (Size4, board4x4)
         ]


benches =
  [ (Size2, comboBench @Traversal.List.T "Traversal.List")

  , (Size3, comboBench @Traversal.Set.T "Traversal.Set")
  , (Size3, comboBench @Traversal.FilteredSet.T "Traversal.FilteredSet")

  , (Size4, comboBench @Traversal.FilteredPrefixSet.T "Traversal.FilteredPrefixSet")
  , (Size4, comboBench @Traversal.Trie.T "Traversal.Trie")
  , (Size4, comboBench @Traversal.FilteredTrie.T "Traversal.FilteredTrie")
  , (Size4, comboBench @Traversal.FilteredPrefixHAMT.T "Traversal.FilteredPrefixHAMT")

  , (Size4, comboBench @DP.TwoPass.T "DP.TwoPass")
  , (Size4, comboBench @DP.OnePass.T "DP.OnePass")
  , (Size4, comboBench @DP.OnePassSinglePath.T "DP.OnePassSinglePath")
  , (Size4, comboBench @DP.Filtered1PV.T "DP.Filtered1PV")
  ]

main = do
  !rawDict <- loadDictFile "bench/dict.txt"
  let config = defaultConfig { reportFile = Just "boggle-all.html" }
  defaultMainWith config $ flip map boards $ \(speed, board) ->
    bgroup (show speed) $
      flip map (map snd $ filter ((== speed) . fst) benches) $ \bnch ->
        bnch rawDict board
