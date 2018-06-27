{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Criterion.Types (reportFile)
import Base
import Bench

import qualified DP.FilteredOnePassTree as DP
import qualified Traversal.FilteredTrie as Trie
import qualified Traversal.IncrementalFPHAMT as Set
import qualified Traversal.Heap as Heap

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
  [ solveBench Set.solver "set"
  , solveBench Trie.solver "trie"
  , solveBench DP.solver "dp"
  , solveBench Heap.solver "heap"
  ]

main = do
  !rawDict <- loadDictFile "bench/dict.txt"
  let config = defaultConfig { reportFile = Just "bench-best.html" }
  defaultMainWith config $ flip map boards $ \(bname, board) ->
    bgroup bname $ flip map benches $ \f -> f rawDict board
