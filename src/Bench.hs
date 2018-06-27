{-# LANGUAGE OverloadedStrings #-}

module Bench where

import Criterion.Main
import Criterion.Types (reportFile)
import Control.Arrow ((&&&))
import Control.DeepSeq (force, NFData)
import qualified Data.ByteString.Char8 as BS
import Paths_flabbergast
import Base

loadDictFile :: FilePath -> IO RawDictionary
loadDictFile name = do
  p <- getDataFileName name
  map (id &&& BS.sort) . BS.lines <$> BS.readFile p

board4x4 :: RawBoard
board4x4 = mkGridOf ["INVS","TENE","APUU","GNDO"]

-- | Benchmark that applies the solver repeatedly to a given dictionary and
-- board, cooking each only once.
solveBench :: Solver -> String -> RawDictionary -> RawBoard -> Benchmark
solveBench solver name d b = bench name $ nf (uncurry solver) (d, b)
