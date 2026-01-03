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


bigfloatToBigint :: Double -> Integer
bigfloatToBigint = round

bigfloatToFloat32 :: Double -> Float
bigfloatToFloat32 = realToFrac

bigfloatToFloat64 :: Double -> Double
bigfloatToFloat64 = id

bigintToBigfloat :: Integer -> Double
bigintToBigfloat = fromIntegral

bigintToInt8 :: Integer -> Int8
bigintToInt8 = fromIntegral

bigintToInt16 :: Integer -> Int16
bigintToInt16 = fromIntegral

bigintToInt32 :: Integer -> Int
bigintToInt32 = fromIntegral

bigintToInt64 :: Integer -> Int64
bigintToInt64 = fromIntegral

bigintToUint8 :: Integer -> Int16
bigintToUint8 = fromIntegral

bigintToUint16 :: Integer -> Int
bigintToUint16 = fromIntegral

bigintToUint32 :: Integer -> Int64
bigintToUint32 = fromIntegral

bigintToUint64 :: Integer -> Integer
bigintToUint64 = id

-- | Encode a ByteString to a base64-encoded String
binaryToString :: B.ByteString -> String
binaryToString = T.unpack . TE.decodeUtf8 . B64.encode

-- | Alias for binaryToString (for compatibility during transition)
binaryToStringBS :: B.ByteString -> String
binaryToStringBS = binaryToString

float32ToBigfloat :: Float -> Double
float32ToBigfloat = realToFrac

float64ToBigfloat :: Double -> Double
float64ToBigfloat = id

int8ToBigint :: Int8 -> Integer
int8ToBigint = fromIntegral

int16ToBigint :: Int16 -> Integer
int16ToBigint = fromIntegral

int32ToBigint :: Int -> Integer
int32ToBigint = fromIntegral

int64ToBigint :: Int64 -> Integer
int64ToBigint = fromIntegral

readBigfloat :: String -> Maybe Double
readBigfloat s = readMaybe s :: Maybe Double

readBigint :: String -> Maybe Integer
readBigint s = readMaybe s :: Maybe Integer

readBoolean :: String -> Maybe Bool
readBoolean s = if s == "true" then Just True
  else if s == "false" then Just False
  else Nothing

readFloat32 :: String -> Maybe Float
readFloat32 s = readMaybe s :: Maybe Float

readFloat64 :: String -> Maybe Double
readFloat64 s = readMaybe s :: Maybe Double

readInt8 :: String -> Maybe Int8
readInt8 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= -128 && n <= 127 then Just (fromIntegral n) else Nothing

readInt16 :: String -> Maybe Int16
readInt16 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= -32768 && n <= 32767 then Just (fromIntegral n) else Nothing

readInt32 :: String -> Maybe Int
readInt32 s = readMaybe s :: Maybe Int

readInt64 :: String -> Maybe Int64
readInt64 s = readMaybe s :: Maybe Int64

readString :: String -> Maybe String
readString s = readMaybe s :: Maybe String

-- Note: Hydra uses wider signed types to represent unsigned values without overflow
-- Uint8 -> Int16, Uint16 -> Int, Uint32 -> Int64, Uint64 -> Integer
-- The read functions parse as unsigned and validate the range

readUint8 :: String -> Maybe Int16
readUint8 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 255 then Just (fromIntegral n) else Nothing

readUint16 :: String -> Maybe Int
readUint16 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 65535 then Just (fromIntegral n) else Nothing

readUint32 :: String -> Maybe Int64
readUint32 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 4294967295 then Just (fromIntegral n) else Nothing

readUint64 :: String -> Maybe Integer
readUint64 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 18446744073709551615 then Just n else Nothing

showBigfloat :: Double -> String
showBigfloat = show

showBigint :: Integer -> String
showBigint = show

showBoolean :: Bool -> String
showBoolean b = case b of
  True -> "true"
  False -> "false"

showFloat32 :: Float -> String
showFloat32 = show

showFloat64 :: Double -> String
showFloat64 = show

showInt8 :: Int8 -> String
showInt8 = show

showInt16 :: Int16 -> String
showInt16 = show

showInt32 :: Int -> String
showInt32 = show

showInt64 :: Int64 -> String
showInt64 = show

showUint8 :: Int16 -> String
showUint8 = show

showUint16 :: Int -> String
showUint16 = show

showUint32 :: Int64 -> String
showUint32 = show

showUint64 :: Integer -> String
showUint64 = show

showString :: String -> String
showString = show

-- | Decode a base64-encoded String to a ByteString.
-- Returns an empty ByteString if decoding fails.
stringToBinary :: String -> B.ByteString
stringToBinary s = case B64.decode (TE.encodeUtf8 $ T.pack s) of
  Left _ -> B.empty
  Right bs -> bs

uint8ToBigint :: Int16 -> Integer
uint8ToBigint = fromIntegral

uint16ToBigint :: Int -> Integer
uint16ToBigint = fromIntegral

uint32ToBigint :: Int64 -> Integer
uint32ToBigint = fromIntegral

uint64ToBigint :: Integer -> Integer
uint64ToBigint = id
