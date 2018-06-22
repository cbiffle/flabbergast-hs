{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Bench where

import Criterion.Main
import Criterion.Types (reportFile)
import Control.DeepSeq (force, NFData)
import Paths_boggle
import Base

import qualified DP.Filtered1PV as DP
import qualified Traversal.FilteredTrie as Trie
import qualified Traversal.FilteredPrefixSet as Set

loadDictFile :: FilePath -> IO RawDictionary
loadDictFile name = do
  p <- getDataFileName name
  force . lines <$> readFile p

board4x4 :: RawBoard
board4x4 = ["INVS","TENE","APUU","GNDO"]


-- | Benchmark that applies the solver repeatedly to a given dictionary and
-- board, cooking each only once.
solveBench :: forall s.
              (Solver s, NFData (CookedDict s), NFData (CookedBoard s))
           => RawDictionary -> RawBoard -> Benchmark
solveBench d b =
  let preD :: CookedDict s = cookDict @s d
      preB :: CookedBoard s = cookBoard @s b
  in bench "solve-only" $ nf (uncurry (solve @s)) (preD, preB)

-- | Benchmark that applies the solver repeatedly to a given dictionary and
-- board, cooking the dictionary in advance and the board each time.
--
-- This simulates the effect of algorithm-specific dictionary preprocessing.
preBench :: forall s. (Solver s, NFData (CookedDict s), NFData (CookedBoard s))
         => RawDictionary -> RawBoard -> Benchmark
preBench d b = let preD :: CookedDict s = cookDict @s d
      in bench "precook-dict" $ nf (solve @s preD . cookBoard @s) b

-- | Benchmark that applies the solver repeatedly to a given dictionary and
-- board, cooking each every time.
cookBench :: forall s. (Solver s, NFData (CookedDict s), NFData (CookedBoard s))
          => RawDictionary -> RawBoard -> Benchmark
cookBench d b = bench "cook-all" $ nf (uncurry (cookAndSolve @s)) (d, b)

comboBench :: forall s.
              (Solver s, NFData (CookedDict s), NFData (CookedBoard s))
           => String -> RawDictionary -> RawBoard -> Benchmark
comboBench name d b = bgroup name
    [ solveBench @s d b
    , preBench @s d b
    , cookBench @s d b
    ]
