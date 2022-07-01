module Hydra.Lib.Literals where


-- TODO: expose as a primitive
bigfloatToBigint :: Double -> Integer
bigfloatToBigint = round

-- TODO: expose as a primitive
bigintToBigfloat :: Integer -> Double
bigintToBigfloat = fromIntegral

showInt32 :: Int -> String
showInt32 = show

showString :: String -> String
showString = show
