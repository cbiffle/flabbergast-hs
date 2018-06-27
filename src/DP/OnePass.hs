-- | Dynamic programming, redux.
--
-- Instead of a fuzzy DP solution followed by an exact DFS, this is an exact
-- DP solution. In each step, it propagates all valid paths through a given
-- tile for a given word. By maintaining the path information it can detect,
-- and reject, self-intersecting paths.
module DP.OnePass (solver) where

import Control.Arrow ((&&&))
import Data.List (foldl')
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import Base

-- | Propagates potential paths, given a grid of known paths and the next
-- character in the word.
step :: RawBoard -> GridOf [IPath] -> Char -> GridOf [IPath]
step b paths c = b `gfor` \i bc ->
  [i : path | bc == c
            , ni <- neighborIndices b i
            , path <- paths `at` ni
            , i `notElem` path]

-- | Searches for instances of a single candidate word 'w' in 'b'. Returns one
-- such instance if found.
search1 :: RawBoard -> BS.ByteString -> Maybe (BS.ByteString, Path)
search1 b w =
  fmap (\p -> (w, ipath b $ reverse p)) $
  listToMaybe $ concat $ ungrid $
  BS.foldl' (step b) seed w
  where seed = b `gfor` \_ _ -> [[]]

solver :: Solver
solver d b = [r | (word, _) <- d
                , r <- maybeToList $ search1 b word
                ]
