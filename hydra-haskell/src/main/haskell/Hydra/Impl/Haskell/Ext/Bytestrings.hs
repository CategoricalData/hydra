module Hydra.Impl.Haskell.Ext.Bytestrings where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy.Encoding as DTE
import qualified Data.Text.Lazy as TL


bytesToString :: BS.ByteString -> String
bytesToString = TL.unpack . DTE.decodeUtf8

stringToBytes :: String -> BS.ByteString
stringToBytes = DTE.encodeUtf8 . TL.pack
