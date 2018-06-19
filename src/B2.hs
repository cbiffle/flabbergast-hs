{-# LANGUAGE TypeFamilies #-}
module B2 where

import Data.List (nub)
import Base

search :: RawDictionary -> RawBoard -> Path -> String -> [(String, Path)]
search d b path word =
  (if word `elem` d then ((reverse word, reverse path) :) else id)
  [r | n <- nextSteps b (head path)
     , n `notElem` path
     , let c = b !! snd n !! fst n
     , let (p', w') = (n : path, c : word)
     , r <- search d b p' w'
     ]

data Naive

instance Solver Naive where
  type CookedDict Naive = RawDictionary
  type CookedBoard Naive = RawBoard
  cookDict = fmap reverse
  cookBoard = id
  solve d b = [r | pos <- positions b
              , r <- search d b [pos] [b !! snd pos !! fst pos]
              ]
