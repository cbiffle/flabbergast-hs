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

import qualified DP.OnePassSinglePathVec as DP
import qualified Traversal.FilteredTrie as Trie
import qualified Traversal.FilteredPrefixSet as Set

loadFile name = do
  p <- getDataFileName name
  force . lines <$> readFile p

boards = [{- ("2x2", ["MS","UO"])
         ,-} ("3x3", ["TEI","EAA","ZJY"])
         , ("4x4", ["INVS","TENE","APUU","GNDO"])
         ]

sbench :: forall s. (Solver s, NFData (CookedDict s), NFData (CookedBoard s))
          => String -> RawDictionary -> RawBoard -> Benchmark
sbench name d b =
  let preD :: CookedDict s = cookDict @s d
      preB :: CookedBoard s = cookBoard @s b
  in bgroup name
    [ bench "solve-only" $ nf (uncurry (solve @s)) (preD, preB)
    , bench "cook-and-solve" $ nf (uncurry (cookAndSolve @s)) (d, b)
    ]

benches =
  [ sbench @Set.T "set"
  , sbench @Trie.T "trie"
  , sbench @DP.T "dp"
  ]

main = do
  !rawDict <- loadFile "bench/dict.txt"
  let config = defaultConfig { reportFile = Just "boggle-bench.html" }
  defaultMainWith config $ flip map boards $ \(bname, board) ->
    bgroup bname $ flip map benches $ \f -> f rawDict board
