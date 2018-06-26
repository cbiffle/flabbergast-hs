module Traversal.Set (T) where

import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import Base
import Uniq

import Traversal.Generic

data T

instance Solver T where
  solve d =
    let set = S.fromList $ map fst d
    in uniqBy fst . paths (exhaustive (`S.member` set))
