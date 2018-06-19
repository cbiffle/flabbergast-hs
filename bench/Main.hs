{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import Control.DeepSeq (force, NFData)
import Paths_boggle
import Base
import qualified B1
import qualified B2
import qualified B3
import qualified B4
import qualified B5
import qualified B6

loadFile name = do
  p <- getDataFileName name
  force . lines <$> readFile p

boards = [ ("2x2", ["MS","UO"])
         , ("3x3", ["TEI","EAA","ZJY"])
         , ("4x3", ["RNUI","TAAO","ETUA"])
         , ("4x4", ["INVS","TENE","APUU","GNDO"])
         ]

sbench :: forall s. (Solver s, NFData (CookedDict s), NFData (CookedBoard s))
          => String -> RawDictionary -> RawBoard -> Benchmark
sbench name d b =
  let preD :: CookedDict s = cookDict @s d
      preB :: CookedBoard s = cookBoard @s b
  in bgroup name
    [{- bench "cookDict" $ nf (cookDict @s) d
 --   , bench "cookBoard" $ nf (cookBoard @s) b
    ,-} bench "solve" $ nf (uncurry (solve @s)) (preD, preB)
    ]

main = do
  !rawDict <- loadFile "bench/dict.txt"
  defaultMain $ flip map boards $ \(bname, board) ->
    bgroup bname
--      [ sbench @B1.Naive "B1" rawDict board
--      , sbench @B2.Naive "B2" rawDict board
--    [ sbench @B3.SetBased "B3" rawDict board
--      [ sbench @B4.SetBased "B4" rawDict board
--    [ sbench @B5.SetBased "B5" rawDict board
      [ sbench @B6.DP "B6" rawDict board
      ]
