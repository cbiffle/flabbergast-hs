{-# LANGUAGE TypeFamilies #-}
module B1 where

import Base

search :: RawDictionary -> RawBoard -> Path -> String -> [(String, Path)]
search d b path word =
  let paths = [(n : path, w) | n <- nextSteps b (head path)
                             , n `notElem` path
                             , let c = b !! snd n !! fst n
                             , let w = word ++ [c]
                             ]
  in (if word `elem` d then ((word, reverse path) :) else id)
      [r | (p', w') <- paths
         , r <- search d b p' w'
         ]

data Naive

instance Solver Naive where
  type CookedDict Naive = RawDictionary
  type CookedBoard Naive = RawBoard
  cookDict = id
  cookBoard = id
  solve d b = [r | pos <- positions b
              , r <- search d b [pos] [b !! snd pos !! fst pos]
              ]
