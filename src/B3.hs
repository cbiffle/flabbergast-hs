{-# LANGUAGE TypeFamilies #-}
module B3 where

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

data SetBased

instance Solver SetBased where
  type CookedDict SetBased = Dictionary
  cookDict = S.fromList . fmap reverse

  type CookedBoard SetBased = RawBoard
  cookBoard = id

  solve d b = [r | pos <- positions b
                 , r <- search d b [pos] [b !! snd pos !! fst pos]
                 ]
