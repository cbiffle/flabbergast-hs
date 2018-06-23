{-# LANGUAGE TypeApplications #-}

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Base
import qualified Data.ByteString.Char8 as BS
--import DP.Filtered1PV as DP
import qualified Traversal.List as ALG

main = do
  dict <- map (id &&& BS.sort) . BS.lines <$> BS.readFile "dict.txt"
  board <- mkGridOf . BS.lines <$> BS.readFile "board.txt"

  forM_ (cookAndSolve @ALG.T dict board) $ \(w, _) -> BS.putStrLn w
