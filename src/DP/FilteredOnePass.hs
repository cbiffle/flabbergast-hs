-- | Dynamic programming, redux^2. Filtering the dictionary, since this
-- algorithm scales linearly in the size of the *used* dictionary.
module DP.FilteredOnePass (solver) where

import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.ByteString.Char8 as BS
import Base
import ByteStringUtil

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
solver d b =
  let cs = BS.sort $ BS.pack $ ungrid b
  in [r | (word, sorted) <- d
        , sorted `isSubsequenceOf` cs
        , r <- maybeToList $ search1 b word
        ]
