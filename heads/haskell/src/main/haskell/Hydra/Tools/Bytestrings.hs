-- | Utilities for converting between lazy ByteStrings and Strings via UTF-8 encoding

module Hydra.Tools.Bytestrings where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy.Encoding as DTE
import qualified Data.Text.Lazy as TL


bytesToString :: BS.ByteString -> String
bytesToString = TL.unpack . DTE.decodeUtf8

stringToBytes :: String -> BS.ByteString
stringToBytes = DTE.encodeUtf8 . TL.pack
