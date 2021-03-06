import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Base
import qualified Data.ByteString.Char8 as BS
import qualified Traversal.Heap as ALG

main = do
  dict <- map (id &&& BS.sort) . BS.lines <$> BS.readFile "dict.txt"
  board <- mkGridOf . BS.lines <$> BS.readFile "board.txt"

  forM_ (ALG.solver dict board) $ \(w, _) -> BS.putStrLn w
