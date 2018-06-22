module ByteStringUtil where

import qualified Data.ByteString as B

isSubsequenceOf :: B.ByteString -> B.ByteString -> Bool
isSubsequenceOf as bs
  | B.null as = True
  | B.null bs = False
  | otherwise =
      let Just (a, as') = B.uncons as
          Just (b, bs') = B.uncons bs
      in case a `compare` b of
          EQ -> as' `isSubsequenceOf` bs'
          GT -> as `isSubsequenceOf` bs'
          LT -> False
