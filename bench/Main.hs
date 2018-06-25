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

import qualified DP.FilteredOnePassTree as DP
import qualified Traversal.FilteredTrie as Trie
import qualified Traversal.IncrementalFPHAMT as Set

boards = [ ("4x4", board4x4)
         , ("5x5", mkGridOf [ "EEQPO"
                            , "NIEOE"
                            , "SOESR"
                            , "PPHCE"
                            , "DEITE"
                            ])
         , ("6x6", mkGridOf [ "IGYREN"
                            , "OTRHEO"
                            , "SLETLD"
                            , "GEHDRO"
                            , "PEARRE"
                            , "NETIIM"
                            ])
         ]

benches =
  [ comboBench @Set.T "set"
  , comboBench @Trie.T "trie"
  , comboBench @DP.T "dp"
  ]

main = do
  !rawDict <- loadDictFile "bench/dict.txt"
  let config = defaultConfig { reportFile = Just "boggle-bench.html" }
  defaultMainWith config $ flip map boards $ \(bname, board) ->
    bgroup bname $ flip map benches $ \f -> f rawDict board
