{-# LANGUAGE TypeFamilies #-}
module Traversal.SetPath
  ( T
  , SPath, emptyPath, extendPath, notInPath, pathList, pathHead
  )  where

import qualified Data.Set as S
import Base

type Dictionary = S.Set String

data SPath = SPath !Path !(S.Set Pos)

emptyPath = SPath [] S.empty

extendPath p (SPath list set) = SPath (p : list) (p `S.insert` set)

notInPath p (SPath _ set) = p `S.notMember` set

pathList (SPath list _) = reverse list

pathHead (SPath (p : _) _) = p

search :: Dictionary -> RawBoard -> SPath -> String -> [(String, Path)]
search d b path word =
  (if word `S.member` d then ((reverse word, pathList path) :) else id)
  [r | n <- nextSteps b (pathHead path)
     , n `notInPath` path
     , let (p', w') = (n `extendPath` path, (b !! snd n !! fst n) : word)
     , r <- search d b p' w'
     ]

data T

instance Solver T where
  type CookedDict T = Dictionary
  cookDict = S.fromList . fmap reverse

  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = [r | pos <- positions b
                 , r <- search d b (pos `extendPath` emptyPath)
                                   [b !! snd pos !! fst pos]
                 ]
