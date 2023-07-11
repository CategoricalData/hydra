-- | Haskell implementations of hydra/lib/literals primitives

module Hydra.Lib.Literals where

import Data.Int


-- TODO: expose as a primitive
bigfloatToBigint :: Double -> Integer
bigfloatToBigint = round

-- TODO: expose as a primitive
bigintToBigfloat :: Integer -> Double
bigintToBigfloat = fromIntegral

equalBinary :: String -> String -> Bool
equalBinary = (==)

equalBoolean :: Bool -> Bool -> Bool
equalBoolean = (==)

equalBigfloat :: Double -> Double -> Bool
equalBigfloat = (==)

equalFloat32 :: Float -> Float -> Bool
equalFloat32 = (==)

equalFloat64 :: Double -> Double -> Bool
equalFloat64 = (==)

equalBigint :: Integer -> Integer -> Bool
equalBigint = (==)

equalInt8 :: Int8 -> Int8 -> Bool
equalInt8 = (==)

equalInt16 :: Int16 -> Int16 -> Bool
equalInt16 = (==)

equalInt32 :: Int -> Int -> Bool
equalInt32 = (==)

equalInt64 :: Int64 -> Int64 -> Bool
equalInt64 = (==)

equalUint8 :: Int16 -> Int16 -> Bool
equalUint8 = (==)

equalUint16 :: Int -> Int -> Bool
equalUint16 = (==)

equalUint32 :: Int64 -> Int64 -> Bool
equalUint32 = (==)

equalUint64 :: Integer -> Integer -> Bool
equalUint64 = (==)

equalString :: String -> String -> Bool
equalString = (==)

showInt32 :: Int -> String
showInt32 = show

showString :: String -> String
showString = show
