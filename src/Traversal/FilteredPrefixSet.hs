{-# LANGUAGE TypeFamilies #-}

-- | Set-based filtered dictionary with prefix set.
module Traversal.FilteredPrefixSet (T) where


import qualified Data.Set as S
import Control.Arrow ((&&&))
import Data.List (sort, isSubsequenceOf, tails)
import Control.DeepSeq (NFData(..))
import Base

import Traversal.SetPath
  (SPath, emptyPath, extendPath, notInPath, pathList, pathHead)

search :: S.Set String -> S.Set String -> RawBoard -> SPath -> String
       -> [(String, Path)]
search d pre b path word =
  (if word `S.member` d then ((reverse word, pathList path) :) else id)
  [r | n <- nextSteps b (pathHead path)
     , n `notInPath` path
     , let (p', w') = (n `extendPath` path, (b !! snd n !! fst n) : word)
     , w' `S.member` pre
     , r <- search d pre b p' w'
     ]

data CBoard = CBoard !RawBoard -- ^ The board.
                     ![Char]  -- ^ The sorted characters of the board.

instance NFData CBoard where
  rnf (CBoard b cs) = rnf (b, cs)

data T

instance Solver T where
  type CookedDict T = [(String, String, [String])]
                          -- reversed, sorted, reversed prefixes
  cookDict = fmap (\w -> let r = reverse w in (r, sort w, tails r))

  type CookedBoard T = CBoard
  cookBoard b = CBoard b $ sort $ concat b

  solve d (CBoard b cs) =
    let df = [e | e@(_, sw, _) <- d, sw `isSubsequenceOf` cs]
        d' = S.fromList $ [rw | (rw, _, _) <- df]
        pre = S.fromList $ [t | (_, _, ts) <- df, t <- ts]
    in [r | pos <- positions b
          , r <- search d' pre b (pos `extendPath` emptyPath)
                                 [b !! snd pos !! fst pos]
                 ]