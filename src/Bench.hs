{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bench where

import Criterion.Main
import Criterion.Types (reportFile)
import Control.Arrow ((&&&))
import Control.DeepSeq (force, NFData)
import qualified Data.ByteString.Char8 as BS
import Paths_boggle
import Base

loadDictFile :: FilePath -> IO RawDictionary
loadDictFile name = do
  p <- getDataFileName name
  map (id &&& BS.sort) . BS.lines <$> BS.readFile p

board4x4 :: RawBoard
board4x4 = mkGridOf ["INVS","TENE","APUU","GNDO"]


-- | Benchmark that applies the solver repeatedly to a given dictionary and
-- board, cooking each only once.
solveBench :: forall s.
              (Solver s, NFData (CookedBoard s))
           => RawDictionary -> RawBoard -> Benchmark
solveBench d b =
  let preB :: CookedBoard s = cookBoard @s b
  in bench "solve-only" $ nf (uncurry (solve @s)) (d, preB)

-- | Benchmark that applies the solver repeatedly to a given dictionary and
-- board, cooking each every time.
cookBench :: forall s. (Solver s, NFData (CookedBoard s))
          => RawDictionary -> RawBoard -> Benchmark
cookBench d b = bench "cook-all" $ nf (uncurry (cookAndSolve @s)) (d, b)

comboBench :: forall s.
              (Solver s, NFData (CookedBoard s))
           => String -> RawDictionary -> RawBoard -> Benchmark
comboBench name d b = bgroup name
    [ solveBench @s d b
    , cookBench @s d b
    ]
