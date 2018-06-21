module Uniq where

import Data.Hashable (Hashable)
import Data.HashSet as H

-- | Lazily removes duplicates from a list, where duplicates are determined by
-- comparing the projections by `index`.
--
-- For a list of n unique elements and d duplicates, this is amortized O(n+d)
-- in time, and O(n) in space. Algorithms that are already guaranteed to
-- produce a unique list should skip this.
uniqBy :: (Eq b, Hashable b) => (a -> b) -> [a] -> [a]
uniqBy index = go H.empty
  where
    go _ [] = []
    go seen (x : xs) =
      let xi = index x
      in if index x `H.member` seen
            then go seen xs
            else x : go (xi `H.insert` seen) xs

