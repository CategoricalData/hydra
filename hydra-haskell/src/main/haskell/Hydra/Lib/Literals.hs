-- | Haskell implementations of hydra.lib.literals primitives

module Hydra.Lib.Literals where

import Data.Int
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Note: These binary conversion functions have two versions:
-- 1. String -> String versions for backward compatibility with current generated code
-- 2. ByteString versions will be used after regenerating Core.hs


-- | Convert a bigfloat (Double) to a bigint (Integer).
bigfloatToBigint :: Double -> Integer
bigfloatToBigint = round

-- | Convert a bigfloat (Double) to a float32 (Float).
bigfloatToFloat32 :: Double -> Float
bigfloatToFloat32 = realToFrac

-- | Convert a bigfloat (Double) to a float64 (Double).
bigfloatToFloat64 :: Double -> Double
bigfloatToFloat64 = id

-- | Convert a bigint (Integer) to a bigfloat (Double).
bigintToBigfloat :: Integer -> Double
bigintToBigfloat = fromIntegral

-- | Convert a bigint (Integer) to an int8.
bigintToInt8 :: Integer -> Int8
bigintToInt8 = fromIntegral

-- | Convert a bigint (Integer) to an int16.
bigintToInt16 :: Integer -> Int16
bigintToInt16 = fromIntegral

-- | Convert a bigint (Integer) to an int32.
bigintToInt32 :: Integer -> Int
bigintToInt32 = fromIntegral

-- | Convert a bigint (Integer) to an int64.
bigintToInt64 :: Integer -> Int64
bigintToInt64 = fromIntegral

-- | Convert a bigint (Integer) to a uint8.
bigintToUint8 :: Integer -> Int16
bigintToUint8 = fromIntegral

-- | Convert a bigint (Integer) to a uint16.
bigintToUint16 :: Integer -> Int
bigintToUint16 = fromIntegral

-- | Convert a bigint (Integer) to a uint32.
bigintToUint32 :: Integer -> Int64
bigintToUint32 = fromIntegral

-- | Convert a bigint (Integer) to a uint64.
bigintToUint64 :: Integer -> Integer
bigintToUint64 = id

-- | Convert binary to a list of byte values (0-255).
binaryToBytes :: B.ByteString -> [Int]
binaryToBytes = fmap fromIntegral . B.unpack

-- | Convert binary to string by base64 encoding.
binaryToString :: B.ByteString -> String
binaryToString = T.unpack . TE.decodeUtf8 . B64.encode

-- | Alias for binaryToString (for compatibility during transition).
binaryToStringBS :: B.ByteString -> String
binaryToStringBS = binaryToString

-- | Convert a float32 (Float) to a bigfloat (Double).
float32ToBigfloat :: Float -> Double
float32ToBigfloat = realToFrac

-- | Convert a float64 (Double) to a bigfloat (Double).
float64ToBigfloat :: Double -> Double
float64ToBigfloat = id

-- | Convert an int8 to a bigint (Integer).
int8ToBigint :: Int8 -> Integer
int8ToBigint = fromIntegral

-- | Convert an int16 to a bigint (Integer).
int16ToBigint :: Int16 -> Integer
int16ToBigint = fromIntegral

-- | Convert an int32 to a bigint (Integer).
int32ToBigint :: Int -> Integer
int32ToBigint = fromIntegral

-- | Convert an int64 to a bigint (Integer).
int64ToBigint :: Int64 -> Integer
int64ToBigint = fromIntegral

-- | Parse a string to a bigfloat (Double).
readBigfloat :: String -> Maybe Double
readBigfloat s = readMaybe s :: Maybe Double

-- | Parse a string to a bigint (Integer).
readBigint :: String -> Maybe Integer
readBigint s = readMaybe s :: Maybe Integer

-- | Parse a string to a boolean.
readBoolean :: String -> Maybe Bool
readBoolean s = if s == "true" then Just True
  else if s == "false" then Just False
  else Nothing

-- | Parse a string to a float32 (Float).
readFloat32 :: String -> Maybe Float
readFloat32 s = readMaybe s :: Maybe Float

-- | Parse a string to a float64 (Double).
readFloat64 :: String -> Maybe Double
readFloat64 s = readMaybe s :: Maybe Double

-- | Parse a string to an int8 (-128 to 127).
readInt8 :: String -> Maybe Int8
readInt8 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= -128 && n <= 127 then Just (fromIntegral n) else Nothing

-- | Parse a string to an int16 (-32768 to 32767).
readInt16 :: String -> Maybe Int16
readInt16 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= -32768 && n <= 32767 then Just (fromIntegral n) else Nothing

-- | Parse a string to an int32.
readInt32 :: String -> Maybe Int
readInt32 s = readMaybe s :: Maybe Int

-- | Parse a string to an int64.
readInt64 :: String -> Maybe Int64
readInt64 s = readMaybe s :: Maybe Int64

-- | Parse a string literal.
readString :: String -> Maybe String
readString s = readMaybe s :: Maybe String

-- Note: Hydra uses wider signed types to represent unsigned values without overflow
-- Uint8 -> Int16, Uint16 -> Int, Uint32 -> Int64, Uint64 -> Integer
-- The read functions parse as unsigned and validate the range

-- | Parse a string to a uint8 (0 to 255).
readUint8 :: String -> Maybe Int16
readUint8 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 255 then Just (fromIntegral n) else Nothing

-- | Parse a string to a uint16 (0 to 65535).
readUint16 :: String -> Maybe Int
readUint16 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 65535 then Just (fromIntegral n) else Nothing

-- | Parse a string to a uint32 (0 to 4294967295).
readUint32 :: String -> Maybe Int64
readUint32 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 4294967295 then Just (fromIntegral n) else Nothing

-- | Parse a string to a uint64 (0 to 18446744073709551615).
readUint64 :: String -> Maybe Integer
readUint64 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 18446744073709551615 then Just n else Nothing

-- | Convert a bigfloat (Double) to string.
showBigfloat :: Double -> String
showBigfloat = show

-- | Convert a bigint (Integer) to string.
showBigint :: Integer -> String
showBigint = show

-- | Convert a boolean to string.
showBoolean :: Bool -> String
showBoolean b = case b of
  True -> "true"
  False -> "false"

-- | Convert a float32 (Float) to string.
showFloat32 :: Float -> String
showFloat32 = show

-- | Convert a float64 (Double) to string.
showFloat64 :: Double -> String
showFloat64 = show

-- | Convert an int8 to string.
showInt8 :: Int8 -> String
showInt8 = show

-- | Convert an int16 to string.
showInt16 :: Int16 -> String
showInt16 = show

-- | Convert an int32 to string.
showInt32 :: Int -> String
showInt32 = show

-- | Convert an int64 to string.
showInt64 :: Int64 -> String
showInt64 = show

-- | Convert a uint8 to string.
showUint8 :: Int16 -> String
showUint8 = show

-- | Convert a uint16 to string.
showUint16 :: Int -> String
showUint16 = show

-- | Convert a uint32 to string.
showUint32 :: Int64 -> String
showUint32 = show

-- | Convert a uint64 to string.
showUint64 :: Integer -> String
showUint64 = show

-- | Convert a string to a quoted string representation.
showString :: String -> String
showString = show

-- | Convert string to binary by base64 decoding.
-- Returns an empty ByteString if decoding fails.
stringToBinary :: String -> B.ByteString
stringToBinary s = case B64.decode (TE.encodeUtf8 $ T.pack s) of
  Left _ -> B.empty
  Right bs -> bs

-- | Convert a uint8 to a bigint (Integer).
uint8ToBigint :: Int16 -> Integer
uint8ToBigint = fromIntegral

-- | Convert a uint16 to a bigint (Integer).
uint16ToBigint :: Int -> Integer
uint16ToBigint = fromIntegral

-- | Convert a uint32 to a bigint (Integer).
uint32ToBigint :: Int64 -> Integer
uint32ToBigint = fromIntegral

-- | Convert a uint64 to a bigint (Integer).
uint64ToBigint :: Integer -> Integer
uint64ToBigint = id
