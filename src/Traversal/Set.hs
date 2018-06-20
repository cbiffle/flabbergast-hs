{-# LANGUAGE TypeFamilies #-}
module Traversal.Set (T) where

import qualified Data.Set as S
import Base

type Dictionary = S.Set String

search :: Dictionary -> RawBoard -> Path -> String -> [(String, Path)]
search d b path word =
  (if word `S.member` d then ((reverse word, reverse path) :) else id)
  [r | n <- nextSteps b (head path)
     , n `notElem` path
     , let (p', w') = (n : path, (b !! snd n !! fst n) : word)
     , r <- search d b p' w'
     ]

data T

instance Solver T where
  type CookedDict T = Dictionary
  cookDict = S.fromList . fmap reverse

  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = [r | pos <- positions b
                 , r <- search d b [pos] [b !! snd pos !! fst pos]
                 ]
