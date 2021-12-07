module Hydra.Lib.Math where

import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Prims


hsNeg :: Int -> Int
hsNeg = negate

hsAdd :: Int -> Int -> Int
hsAdd x y = x + y

hsSub :: Int -> Int -> Int
hsSub x y = x - y

hsMul :: Int -> Int -> Int
hsMul x y = x * y

hsDiv :: Int -> Int -> Int
hsDiv = div

hsMod :: Int -> Int -> Int
hsMod = mod

hsRem :: Int -> Int -> Int
hsRem = rem

mathPrimitives :: (Default a, Show a) => [PrimitiveFunction a]
mathPrimitives = [
    int32Prim "neg" [int32Type, int32Type]
      $ withInt32 int32Value hsNeg,
    int32Prim "add" [int32Type, int32Type, int32Type]
      $ withTwoInt32s int32Value hsAdd,
    int32Prim "sub" [int32Type, int32Type, int32Type]
      $ withTwoInt32s int32Value hsSub,
    int32Prim "mul" [int32Type, int32Type, int32Type]
      $ withTwoInt32s int32Value hsMul,
    int32Prim "div" [int32Type, int32Type, int32Type]
      $ withTwoInt32s int32Value hsDiv,
    int32Prim "mod" [int32Type, int32Type, int32Type]
      $ withTwoInt32s int32Value hsMod,
    int32Prim "rem" [int32Type, int32Type, int32Type]
      $ withTwoInt32s int32Value hsRem]
  where
    int32Prim = prim "hydra/lib/math/int32" 
