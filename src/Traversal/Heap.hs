{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Heap-based solver.
--
-- This is a very different algorithm that does dictionary-guided traversal
-- while making a single pass over the dictionary. It's based on M.J. Hecht's
-- work here:
--
-- http://www.mh-z.com/untangle/alg_heap.html
--
-- Compared to the other traversals, it has the useful property that it
-- discovers paths across the board in strict alphabetical order. This lets it
-- scan the dictionary in a single pass.
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
                     }  -- FWIW, UNPACK does nothing here; -O2 suffices
                     deriving (Ord, Show, Eq)

pathIndices (Snoc _ i mr) = i : maybe [] pathIndices mr

toIPath = reverse . pathIndices

i `usedIn` cp = i `elem` pathIndices cp

type Search = WriterT (Endo [(BS.ByteString, IPath)]) (State RawDictionary)

-- | Checks for 'word' in the dictionary, skipping lexicographically earlier
-- words and emitting it if found. Then, checks if there are any words that
-- can be constructed by adding letters to the end of 'word'. If so, transfers
-- into 'drain'.
check :: RawBoard -> H.Heap CharPath -> BS.ByteString -> CharPath -> Search ()
check b heap word cpX = do
  -- Any words lexicographically less than the word are no longer
  -- possible.  Discard them.
  modify $ dropWhile ((< word) . fst)
  -- Inspect the first word in the dictionary, if the dictionary is not empty.
  mhead <- gets $ fmap fst . listToMaybe
  case mhead of
    Nothing -> pure ()  -- Dictionary exhausted.
    Just w -> do
      when (w == word) $ do -- Exact word found, emit it.
        tell (Endo ((w, toIPath cpX):))
        modify tail
      -- If adding letters to the word might find matches, try it.
      when (word `BS.isPrefixOf` w) $ drain b heap word

-- | Consumes board positions from 'h' in alphabetical order, generating a new
-- heap at each step by moving outward from each position. Calls 'check' at each
-- new position.
drain :: RawBoard -> H.Heap CharPath -> BS.ByteString -> Search ()
drain b h prefix = case H.uncons h of
  Nothing -> pure ()
  Just (cp0, h') -> do
    let (equivs, h'') = H.span (\e -> cpChar cp0 == cpChar e) h'
        neighbors cp = map (\i -> Snoc (b `at` i) i (Just cp)) $
                       filter (not . (`usedIn` cp)) $
                       neighborIndices b (cpIndex cp)
        h2 = H.concatMap (H.fromList . neighbors)
                         (cp0 `H.insert` equivs)
    check b h2 (prefix `BS.snoc` cpChar cp0) cp0
    drain b h'' prefix

data T

instance Solver T where
  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = map (second (ipath b)) $ (`appEndo` []) $
              flip evalState d $ execWriterT $
              check b start BS.empty undefined  -- TODO hack
    where start = H.fromList $ map (\(i, c) -> Snoc c i Nothing) $
                  zip [0..] (ungrid b)
