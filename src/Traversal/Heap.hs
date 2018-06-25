{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Heap-based solver.
--
-- This is a very different algorithm that does dictionary-guided traversal
-- while making a single pass over the dictionary. It's based on M.J. Hecht's
-- work here:
--
-- http://www.mh-z.com/untangle/alg_heap.html
module Traversal.Heap where

import qualified Data.Heap as H
import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.Writer.Lazy (WriterT, tell, execWriterT)
import Control.Monad.State.Strict (State, modify, gets, evalState)
import Data.Maybe (listToMaybe)
import Data.Monoid (Endo(..))
import qualified Data.ByteString.Char8 as BS
import Base

-- | A path through the board leading to a particular character. Strictly, only
-- the character of the head is necessary, but we link entire CharPath nodes to
-- avoid allocating.
data CharPath = Snoc { cpChar :: !Char
                     , cpIndex :: !Int
                     , cpRest :: !(Maybe CharPath)
                     }
                -- TODO unpack?
  deriving (Ord, Show)

-- | Because we know the mapping from index to character is fixed for a given
-- board, we can ignore the character field for the purposes of equality.
instance Eq CharPath where
  Snoc _ i1 r1 == Snoc _ i2 r2 = i1 == i2 || r1 == r2

toIPath = iter []
  where iter !acc (Snoc _ i mr) = let acc' = i : acc
                                  in maybe acc' (iter acc') mr

i `usedIn` Snoc _ i' mr = i == i' || maybe False (i `usedIn`) mr

-- Observations:
--
-- The construction of heaps mirrors the recursion pattern, so heaps can be
-- passed as simple arguments.
--
-- The dictionary state is threaded through all calls, and could be modeled as
-- State.
--
-- This makes the emission of results harder, but they could be a Writer.

type Search = WriterT (Endo [(BS.ByteString, IPath)]) (State RawDictionary)

search :: RawBoard -> H.Heap CharPath -> BS.ByteString -> CharPath -> Search ()
search b heap prefix cpX = do
  -- Any words lexicographically less than the prefix are no longer
  -- possible.  Discard them.
  modify $ dropWhile ((< prefix) . fst)
  -- Take the prefix if it is a word.
  mhead <- gets $ fmap fst . listToMaybe
  case mhead of
    Nothing -> pure ()  -- TODO: should really early exit here
    Just w -> do
      when (w == prefix) $ do
        tell (Endo ((w, toIPath cpX):))
        modify tail
      when (prefix `BS.isPrefixOf` w) $ drain heap
  where
    drain h = case H.uncons h of
      Nothing -> pure ()
      Just (cp0, h') -> do
        let (equivs, h'') = H.span (\e -> cpChar cp0 == cpChar e) h'
            neighbors cp = map (\i -> Snoc (b `at` i) i (Just cp)) $
                           filter (not . (`usedIn` cp)) $
                           neighborIndices b (cpIndex cp)
            h2 = H.concatMap (H.fromList . neighbors)
                             (cp0 `H.insert` equivs)
        search b h2 (prefix `BS.snoc` cpChar cp0) cp0
        drain h''

data T

instance Solver T where
  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = map (second (ipath b)) $ (`appEndo` []) $
              flip evalState d $ execWriterT $
              search b start BS.empty undefined  -- TODO hack
    where start = H.fromList $ map (\(i, c) -> Snoc c i Nothing) $
                  zip [0..] (ungrid b)
