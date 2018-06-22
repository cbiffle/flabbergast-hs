{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import Criterion.Types (reportFile)
import Control.DeepSeq (force, NFData)
import Paths_boggle
import Base
import Bench

import qualified DP.Filtered1PV
import qualified DP.Filtered1PVBS
import qualified DP.OnePass
import qualified DP.OnePassSinglePath
import qualified DP.OnePassSinglePathVec
import qualified DP.TwoPass
import qualified Traversal.FilteredByteStringSet
import qualified Traversal.FilteredPrefixBSHAMT
import qualified Traversal.FilteredPrefixHAMT
import qualified Traversal.FilteredPrefixHAMTVec
import qualified Traversal.FilteredPrefixSet
import qualified Traversal.FilteredPrefixSetVec
import qualified Traversal.FilteredSetPath
import qualified Traversal.FilteredTrie
import qualified Traversal.List2
import qualified Traversal.List
import qualified Traversal.Set
import qualified Traversal.SetPath
import qualified Traversal.Trie

boards = [ (Slow, ["AW", "OP"])
         , (Fast, board4x4)
         ]

data Speed = Slow | Fast deriving (Eq, Show)

benches =
  [ (Slow, comboBench @Traversal.List.T "Traversal.List")
  , (Slow, comboBench @Traversal.List2.T "Traversal.List2")
  , (Slow, comboBench @Traversal.Set.T "Traversal.Set")
  , (Slow, comboBench @Traversal.SetPath.T "Traversal.SetPath")

  , (Slow, comboBench @Traversal.FilteredSetPath.T "Traversal.FilteredSetPath")
  , (Slow, comboBench @Traversal.FilteredByteStringSet.T "Traversal.FilteredByteStringSet")
  , (Fast, comboBench @Traversal.FilteredPrefixSet.T "Traversal.FilteredPrefixSet")
  , (Fast, comboBench @Traversal.FilteredPrefixSetVec.T "Traversal.FilteredPrefixSetVec")
  , (Fast, comboBench @Traversal.Trie.T "Traversal.Trie")
  , (Fast, comboBench @Traversal.FilteredTrie.T "Traversal.FilteredTrie")
  , (Fast, comboBench @Traversal.FilteredPrefixHAMT.T "Traversal.FilteredPrefixHAMT")
  , (Fast, comboBench @Traversal.FilteredPrefixHAMTVec.T "Traversal.FilteredPrefixHAMTVec")
  , (Fast, comboBench @Traversal.FilteredPrefixBSHAMT.T "Traversal.FilteredPrefixBSHAMT")

  , (Fast, comboBench @DP.TwoPass.T "DP.TwoPass")
  , (Fast, comboBench @DP.OnePass.T "DP.OnePass")
  , (Fast, comboBench @DP.OnePassSinglePath.T "DP.OnePassSinglePath")
  , (Fast, comboBench @DP.OnePassSinglePathVec.T "DP.OnePassSinglePathVec")
  , (Fast, comboBench @DP.Filtered1PV.T "DP.Filtered1PV")
  , (Fast, comboBench @DP.Filtered1PVBS.T "DP.Filtered1PVBS")
  ]

main = do
  !rawDict <- loadDictFile "bench/dict.txt"
  let config = defaultConfig { reportFile = Just "boggle-all.html" }
  defaultMainWith config $ flip map boards $ \(speed, board) ->
    bgroup (show speed) $
      flip map (map snd $ filter ((== speed) . fst) benches) $ \bnch ->
        bnch rawDict board
