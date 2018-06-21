{-# LANGUAGE TypeFamilies #-}

-- | Uses a set-based dictionary, but filtered to only words that can be
-- assembled from some permutation of the board. This is conservative, but
-- still reduces the dictionary by 40x or better when using the 1992 Boggle
-- dice set. (It often reduces the dictionary to less than 1/1000 its original
-- size.)
--
-- The down side of this approach is that we can't construct the dictionary set
-- until we've seen the board, so set construction time counts toward solve
-- time. But we *can* preprocess the dictionary in board-independent ways.
module Traversal.FilteredSetPath (T) where


import qualified Data.Set as S
import Control.Arrow ((&&&))
import Data.List (sort, isSubsequenceOf)
import Control.DeepSeq (NFData(..))
import Base
import Uniq

import Traversal.SetPath
  (SPath, emptyPath, extendPath, notInPath, pathList, pathHead)

search :: S.Set String -> RawBoard -> SPath -> String -> [(String, Path)]
search d b path word =
  (if word `S.member` d then ((reverse word, pathList path) :) else id)
  [r | n <- nextSteps b (pathHead path)
     , n `notInPath` path
     , let (p', w') = (n `extendPath` path, (b !! snd n !! fst n) : word)
     , r <- search d b p' w'
     ]

data CBoard = CBoard !RawBoard -- ^ The board.
                     ![Char]  -- ^ The sorted characters of the board.

instance NFData CBoard where
  rnf (CBoard b cs) = rnf (b, cs)

data T

instance Solver T where
  type CookedDict T = [(String, String)] -- reversed, sorted
  cookDict = fmap (reverse &&& sort)

  type CookedBoard T = CBoard
  cookBoard b = CBoard b $ sort $ concat b

  solve d (CBoard b cs) =
    let d' = S.fromList $ [rw | (rw, sw) <- d, sw `isSubsequenceOf` cs]
    in uniqBy fst $
       [r | pos <- positions b
          , r <- search d' b (pos `extendPath` emptyPath)
                             [b !! snd pos !! fst pos]
                 ]
