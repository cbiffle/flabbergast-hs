module ByteStringUtil where

import qualified Data.ByteString as B

isSubsequenceOf :: B.ByteString -> B.ByteString -> Bool
isSubsequenceOf as bs = search 0 0
  where
    search ai bi
      | ai == B.length as = True
      | bi == B.length bs = False
      | otherwise =
        case (as `B.index` ai) `compare` (bs `B.index` bi) of
          EQ -> search (ai + 1) (bi + 1)
          GT -> search ai (bi + 1)
          LT -> False
