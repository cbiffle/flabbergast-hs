{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming.
--
-- The idea is to do a fuzzy search using a cheap algorithm that might have
-- false positives (due to square reuse), and then do an expensive search only
-- if that succeeds. Because the fuzzy search is very cheap, we do it per word
-- in the dictionary instead of per place on the board. Once we know a word is
-- possible, we do the expensive search for all instances of it.
module B6 (DP) where

import Control.Arrow ((&&&))
import Data.List (sort, isSubsequenceOf)
import Control.DeepSeq (NFData(..))
import Base

cheap' :: RawBoard -> [[Bool]] -> String -> [[Bool]]
cheap' _ last [] = last
cheap' b last (c : cs) =
  let next = mapWithPos (\p bc -> b `tile` p == c
                               && or [last `tile` p'
                                        | p' <- nextSteps b p])
                        b
  in cheap' b next cs

cheap :: RawBoard -> [[Bool]] -> String -> Bool
cheap b last w = or (map or (cheap' b last w))

expensive :: RawBoard -> String -> [(String, Path)]
expensive b target = [r | p <- positions b
                        , b `tile` p == head target
                        , r <- search (tail target) [p]]
                        -- TODO
  where
    search [] p = [(target, reverse p)]
    search (c : cs) path =
      [r | n <- nextSteps b (head path)
         , n `notElem` path
         , b `tile` n == c
         , r <- search cs (n : path)
         ]

data DP

instance Solver DP where
  type CookedDict DP = RawDictionary
  cookDict = id

  type CookedBoard DP = RawBoard
  cookBoard = id

  solve d b = [r | word <- d
                 , cheap b trues word
                 , r <- expensive b word
                 ]
    where trues = map (map (const True)) b
