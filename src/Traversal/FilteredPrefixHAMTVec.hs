{-# LANGUAGE TypeFamilies #-}

-- | HAMT-based filtered dictionary with prefix set.
--
-- For our usage pattern, HAMTs appear to offer better performance than ordered
-- sets.
module Traversal.FilteredPrefixHAMTVec (T) where


import qualified Data.HashSet as H
import Control.Arrow ((&&&))
import Data.List (sort, isSubsequenceOf, tails)
import Control.DeepSeq (NFData(..))
import Base
import Uniq

import GridOf
import qualified Data.Vector as V

search :: H.HashSet String -> H.HashSet String -> GridOf Char -> [Int] -> String
       -> [(String, Path)]
search d pre b@(GO gv gp gn) path word =
  (if word `H.member` d then ((reverse word, reverse $ map (gp V.!) path) :)
                        else id)
  [r | n <- gn V.! (head path)
     , n `notElem` path
     , let (p', w') = (n : path, (gv V.! n) : word)
     , w' `H.member` pre
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

  type CookedBoard T = (GridOf Char, [Char])
  cookBoard = mkGridOf &&& (sort . concat)

  solve d (b, cs) =
    let df = [e | e@(_, sw, _) <- d, sw `isSubsequenceOf` cs]
        d' = H.fromList $ [rw | (rw, _, _) <- df]
        pre = H.fromList $ [t | (_, _, ts) <- df, t <- ts]
    in uniqBy fst $
       [r | (pos, c) <- V.toList $ V.indexed $ goVec b
          , r <- search d' pre b [pos] [c]
          ]
