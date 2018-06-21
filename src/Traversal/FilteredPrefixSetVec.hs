{-# LANGUAGE TypeFamilies #-}

-- | Set-based filtered dictionary with prefix set, using GridOf board
-- representation.
module Traversal.FilteredPrefixSetVec (T) where


import qualified Data.Set as S
import Control.Arrow ((&&&))
import Data.List (sort, isSubsequenceOf, tails)
import Control.DeepSeq (NFData(..))
import Base
import Uniq

import GridOf
import qualified Data.Vector as V

search :: S.Set String -> S.Set String -> GridOf Char -> [Int] -> String
       -> [(String, Path)]
search d pre b@(GO gv gp gn) ipath word =
  (if word `S.member` d then ((reverse word, map (gp V.!) $ reverse ipath) :)
                        else id)
  [r | ni <- gn V.! head ipath
     , ni `notElem` ipath
     , let (ip', w') = (ni : ipath, (gv V.! ni) : word)
     , w' `S.member` pre
     , r <- search d pre b ip' w'
     ]

data T

instance Solver T where
  type CookedDict T = [(String, String, [String])]
                          -- reversed, sorted, reversed prefixes
  cookDict = fmap (\w -> let r = reverse w in (r, sort w, tails r))

  type CookedBoard T = (GridOf Char, [Char])
  cookBoard = mkGridOf &&& (sort . concat)

  solve d (b, cs) =
    let df = [e | e@(_, sw, _) <- d, sw `isSubsequenceOf` cs]
        d' = S.fromList $ [rw | (rw, _, _) <- df]
        pre = S.fromList $ [t | (_, _, ts) <- df, t <- ts]
    in uniqBy fst $
       [r | i <- [0 .. V.length (goVec b) - 1]
          , r <- search d' pre b [i]
                                 [goVec b V.! i]
          ]
