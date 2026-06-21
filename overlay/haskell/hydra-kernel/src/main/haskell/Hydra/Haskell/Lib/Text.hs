-- | Haskell implementations of hydra.lib.text primitives

module Hydra.Haskell.Lib.Text where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE


-- | Decode a sequence of bytes as UTF-8 text, returning Left with a host-provided
-- message if the bytes are not valid UTF-8.
decodeUtf8 :: BS.ByteString -> Either String String
decodeUtf8 bytes = case TE.decodeUtf8' bytes of
  Left err -> Left (show (err :: TEE.UnicodeException))
  Right t -> Right (T.unpack t)

-- | Encode text as a sequence of UTF-8 bytes. Total: every string is valid Unicode.
encodeUtf8 :: String -> BS.ByteString
encodeUtf8 = TE.encodeUtf8 . T.pack
