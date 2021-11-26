module Hydra.Impl.Haskell.Lib.Math (
  mathPrimitives,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl

import qualified Data.Char as C
import qualified Data.List as L


mathPrimitives :: (Default a, Show a) => [PrimitiveFunction a]
mathPrimitives = [
  prim "neg" int32Type int32Type
    $ withInt32 $ int32Value . negate,
  prim "add" int32Type (functionType int32Type int32Type)
    $ withTwoInt32s $ \x y -> int32Value $ x + y,
  prim "sub" int32Type (functionType int32Type int32Type)
    $ withTwoInt32s $ \x y -> int32Value $ x - y,
  prim "mul" int32Type (functionType int32Type int32Type)
    $ withTwoInt32s $ \x y -> int32Value $ x * y,
  prim "div" int32Type (functionType int32Type int32Type)
    $ withTwoInt32s $ \x y -> int32Value $ x `div` y,
  prim "mod" int32Type (functionType int32Type int32Type)
    $ withTwoInt32s $ \x y -> int32Value $ x `mod` y,
  prim "rem" int32Type (functionType int32Type int32Type)
    $ withTwoInt32s $ \x y -> int32Value $ x `rem` y]

int32MathFunc :: String -> Name
int32MathFunc local = "hydra/lib/math/int32." ++ local

prim :: String -> Type -> Type -> ([Term a] -> Result (Term a)) -> PrimitiveFunction a
prim nm dom cod impl = PrimitiveFunction {
  primitiveFunctionName = int32MathFunc nm,
  primitiveFunctionType = FunctionType dom cod,
  primitiveFunctionImplementation  = impl}

withInt32 :: Show a => (Int -> Term a) -> [Term a] -> Result (Term a)
withInt32 func args = do
  expectNArgs 1 args
  func <$> expectInt32Term (L.head args)

withTwoInt32s :: Show a => (Int -> Int -> Term a) -> [Term a] -> Result (Term a)
withTwoInt32s func args = do
  expectNArgs 2 args
  func <$> expectInt32Term (L.head args) <*> expectInt32Term (args !! 1)
