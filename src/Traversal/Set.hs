module Traversal.Set (solver) where

import qualified Data.Set as S
import Base
import Uniq

import Traversal.Generic

solver :: Solver
solver d =
  let set = S.fromList $ map fst d
  in uniqBy fst . paths (exhaustive (`S.member` set))
