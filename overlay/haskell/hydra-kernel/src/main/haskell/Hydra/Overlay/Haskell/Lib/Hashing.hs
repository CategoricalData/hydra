-- | Haskell implementations of hydra.lib.hashing primitives

module Hydra.Overlay.Haskell.Lib.Hashing where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA


-- | Compute the SHA-256 digest of a sequence of bytes, returning the 32-byte digest.
sha256 :: BS.ByteString -> BS.ByteString
sha256 = BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict

-- | Compute the SHA-256 digest of a sequence of bytes as a 64-character lowercase hex string.
sha256Hex :: BS.ByteString -> String
sha256Hex = SHA.showDigest . SHA.sha256 . BL.fromStrict
