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
import qualified B1
import qualified B2
import qualified B3
import qualified B4
import qualified B5
import qualified B6
import qualified B7
import qualified B8
import qualified B9
import qualified B10
import qualified B11
import qualified B12

loadFile name = do
  p <- getDataFileName name
  force . lines <$> readFile p

boards = [{- ("2x2", ["MS","UO"])
         , ("3x3", ["TEI","EAA","ZJY"])
         , ("4x3", ["RNUI","TAAO","ETUA"])
         ,-} ("4x4", ["INVS","TENE","APUU","GNDO"])
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

main = do
  !rawDict <- loadFile "bench/dict.txt"
  let config = defaultConfig { reportFile = Just "boggle-bench.html" }
  defaultMainWith config $ flip map boards $ \(bname, board) ->
    bgroup bname
      [ bgroup "naive" $
          filter (const (boardWidth board < 3))
            [ sbench @B1.Naive "B1" rawDict board
            , sbench @B2.Naive "B2" rawDict board
            ]
      , bgroup "set" $
          filter (const (boardWidth board < 3))
            [ sbench @B3.SetBased "B3" rawDict board
            , sbench @B4.SetBased "B4" rawDict board
            ]
          ++
          filter (const (boardHeight board < 4))
            [ sbench @B5.SetBased "B5" rawDict board
            , sbench @B11.SetBased "B11" rawDict board
            ]
          ++
          [ sbench @B10.SetBased "B10" rawDict board
          ]
      , bgroup "dp"
          [{- sbench @B6.DP "B6" rawDict board
          , sbench @B7.DP "B7" rawDict board
          , sbench @B8.DP "B8" rawDict board
          ,-} sbench @B9.DP "B9" rawDict board
          ]
      , bgroup "trie"
          [ sbench @B12.TrieBased "B12" rawDict board
          ]
      ]
