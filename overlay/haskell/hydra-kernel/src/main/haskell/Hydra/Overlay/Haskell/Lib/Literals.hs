-- | Haskell implementations of hydra.lib.literals primitives

module Hydra.Overlay.Haskell.Lib.Literals where

import Data.Int
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Convert a bigint (Integer) to a decimal (Scientific).
bigintToDecimal :: Integer -> Scientific
bigintToDecimal n = Sci.scientific n 0

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

-- | Convert a decimal (Scientific) to a bigint (Integer) using banker's rounding
-- (round half to even), matching Haskell's 'round' and the BigDecimal/BigInt behavior
-- in the other Hydra hosts.
decimalToBigint :: Scientific -> Integer
decimalToBigint = round

-- | Convert a decimal (Scientific) to a float32 (Float). May lose precision.
decimalToFloat32 :: Scientific -> Float
decimalToFloat32 = toRealFloat

-- | Convert a decimal (Scientific) to a float64 (Double). May lose precision.
decimalToFloat64 :: Scientific -> Double
decimalToFloat64 = toRealFloat

-- | Convert binary to string by base64 encoding.
binaryToBase64 :: B.ByteString -> String
binaryToBase64 = T.unpack . TE.decodeUtf8 . B64.encode

-- | Convert a float32 (Float) to a decimal (Scientific).
float32ToDecimal :: Float -> Scientific
float32ToDecimal = fromFloatDigits

-- | Convert a float32 (Float) to a float64 (Double).
-- Preserves IEEE special values (NaN, +/-Infinity); plain realToFrac
-- routes through Rational and collapses Inf to maxFinite (~3.4028e38).
float32ToFloat64 :: Float -> Double
float32ToFloat64 x
  | isNaN x      = 0/0
  | isInfinite x = if x < 0 then -1/0 else 1/0
  | otherwise    = realToFrac x

-- | Convert a float64 (Double) to a decimal (Scientific).
float64ToDecimal :: Double -> Scientific
float64ToDecimal = fromFloatDigits

-- | Convert a float64 (Double) to a float32 (Float). May lose precision.
-- IEEE special values are preserved; for finite values, realToFrac
-- (which converts via Rational) is used.
float64ToFloat32 :: Double -> Float
float64ToFloat32 x
  | isNaN x      = 0/0
  | isInfinite x = if x < 0 then -1/0 else 1/0
  | otherwise    = realToFrac x

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

-- | Parse a string to a bigint (Integer).
parseBigint :: String -> Maybe Integer
parseBigint s = readMaybe s :: Maybe Integer

-- | Parse a string to a boolean.
parseBoolean :: String -> Maybe Bool
parseBoolean s = if s == "true" then Just True
  else if s == "false" then Just False
  else Nothing

-- | Parse a string to a decimal (Scientific).
parseDecimal :: String -> Maybe Scientific
parseDecimal s = readMaybe s :: Maybe Scientific

-- | Parse a string to a float32 (Float).
parseFloat32 :: String -> Maybe Float
parseFloat32 s = readMaybe s :: Maybe Float

-- | Parse a string to a float64 (Double).
parseFloat64 :: String -> Maybe Double
parseFloat64 s = readMaybe s :: Maybe Double

-- | Parse a string to an int16 (-32768 to 32767).
parseInt16 :: String -> Maybe Int16
parseInt16 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= -32768 && n <= 32767 then Just (fromIntegral n) else Nothing

-- | Parse a string to an int32.
parseInt32 :: String -> Maybe Int
parseInt32 s = readMaybe s :: Maybe Int

-- | Parse a string to an int64.
parseInt64 :: String -> Maybe Int64
parseInt64 s = readMaybe s :: Maybe Int64

-- | Parse a string to an int8 (-128 to 127).
parseInt8 :: String -> Maybe Int8
parseInt8 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= -128 && n <= 127 then Just (fromIntegral n) else Nothing

-- | Parse a string literal.
parseString :: String -> Maybe String
parseString s = readMaybe s :: Maybe String

-- Note: Hydra uses wider signed types to represent unsigned values without overflow
-- Uint8 -> Int16, Uint16 -> Int, Uint32 -> Int64, Uint64 -> Integer
-- The parse functions parse as unsigned and validate the range

-- | Parse a string to a uint16 (0 to 65535).
parseUint16 :: String -> Maybe Int
parseUint16 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 65535 then Just (fromIntegral n) else Nothing

-- | Parse a string to a uint32 (0 to 4294967295).
parseUint32 :: String -> Maybe Int64
parseUint32 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 4294967295 then Just (fromIntegral n) else Nothing

-- | Parse a string to a uint64 (0 to 18446744073709551615).
parseUint64 :: String -> Maybe Integer
parseUint64 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 18446744073709551615 then Just n else Nothing

-- | Parse a string to a uint8 (0 to 255).
parseUint8 :: String -> Maybe Int16
parseUint8 s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 0 && n <= 255 then Just (fromIntegral n) else Nothing

-- | Convert a bigint (Integer) to string.
printBigint :: Integer -> String
printBigint = show

-- | Convert a boolean to string.
printBoolean :: Bool -> String
printBoolean b = case b of
  True -> "true"
  False -> "false"

-- | Convert a decimal (Scientific) to string. Uses Scientific's default format
--   (regular notation for small-magnitude values; scientific notation for very
--   large or very small).
printDecimal :: Scientific -> String
printDecimal = show

-- | Convert a float32 (Float) to string.
printFloat32 :: Float -> String
printFloat32 = show

-- | Convert a float64 (Double) to string.
printFloat64 :: Double -> String
printFloat64 = show

-- | Convert an int16 to string.
printInt16 :: Int16 -> String
printInt16 = show

-- | Convert an int32 to string.
printInt32 :: Int -> String
printInt32 = show

-- | Convert an int64 to string.
printInt64 :: Int64 -> String
printInt64 = show

-- | Convert an int8 to string.
printInt8 :: Int8 -> String
printInt8 = show

-- | Convert a string to a quoted string representation.
printString :: String -> String
printString = show

-- | Convert a uint16 to string.
printUint16 :: Int -> String
printUint16 = show

-- | Convert a uint32 to string.
printUint32 :: Int64 -> String
printUint32 = show

-- | Convert a uint64 to string.
printUint64 :: Integer -> String
printUint64 = show

-- | Convert a uint8 to string.
printUint8 :: Int16 -> String
printUint8 = show

-- | Convert string to binary by base64 decoding.
-- Returns an empty ByteString if decoding fails.
base64ToBinary :: String -> B.ByteString
base64ToBinary s = case B64.decode (TE.encodeUtf8 $ T.pack s) of
  Left _ -> B.empty
  Right bs -> bs

-- | Parse a string as a Scientific decimal. Errors on malformed input.
stringToDecimal :: String -> Scientific
stringToDecimal = read

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
