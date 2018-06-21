module Uniq where

import Data.Set as S

-- | Lazily removes duplicates from a list, where duplicates are determined by
-- comparing the projections by `index`.
--
-- For a list of n unique elements and d duplicates, this is O((n+d) log n) in
-- time, and O(n) in space. Algorithms that are already guaranteed to produce a
-- unique list should skip this.
uniqBy :: (Eq b, Ord b) => (a -> b) -> [a] -> [a]
uniqBy index = go S.empty
  where
    go _ [] = []
    go seen (x : xs) =
      let xi = index x
      in if index x `S.member` seen
            then go seen xs
            else x : go (xi `S.insert` seen) xs

