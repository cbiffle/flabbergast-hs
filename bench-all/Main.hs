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

boards = [ (Slow, mkGridOf ["AW", "OP"])
         , (Fast, board4x4)
         ]

data Speed = Slow | Fast deriving (Eq, Show)

benches =
  [ (Slow, comboBench @Traversal.List.T "Traversal.List")
--  , (Fast, comboBench @Traversal.Set.T "Traversal.Set")

  , (Fast, comboBench @Traversal.FilteredSet.T "Traversal.FilteredSet")
  , (Fast, comboBench @Traversal.FilteredPrefixSet.T "Traversal.FilteredPrefixSet")
  , (Fast, comboBench @Traversal.Trie.T "Traversal.Trie")
  , (Fast, comboBench @Traversal.FilteredTrie.T "Traversal.FilteredTrie")
  , (Fast, comboBench @Traversal.FilteredPrefixHAMT.T "Traversal.FilteredPrefixHAMT")

  , (Fast, comboBench @DP.TwoPass.T "DP.TwoPass")
  , (Fast, comboBench @DP.OnePass.T "DP.OnePass")
  , (Fast, comboBench @DP.OnePassSinglePath.T "DP.OnePassSinglePath")
  , (Fast, comboBench @DP.Filtered1PV.T "DP.Filtered1PV")
  ]

main = do
  !rawDict <- loadDictFile "bench/dict.txt"
  let config = defaultConfig { reportFile = Just "boggle-all.html" }
  defaultMainWith config $ flip map boards $ \(speed, board) ->
    bgroup (show speed) $
      flip map (map snd $ filter ((== speed) . fst) benches) $ \bnch ->
        bnch rawDict board
