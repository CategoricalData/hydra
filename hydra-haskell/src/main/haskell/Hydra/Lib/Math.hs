module Hydra.Lib.Math where

import Hydra.Core
import Hydra.Evaluation
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

_hydra_lib_math_int32 :: Name
_hydra_lib_math_int32 = "hydra/lib/math/int32"

hydraLibMathInt32Primitives :: (Default a, Show a) => [PrimitiveFunction a]
hydraLibMathInt32Primitives = [
    prim1 _hydra_lib_math_int32 "neg" int32Input int32Output hsNeg,
    prim2 _hydra_lib_math_int32 "add" int32Input int32Input int32Output hsAdd,
    prim2 _hydra_lib_math_int32 "sub" int32Input int32Input int32Output hsSub,
    prim2 _hydra_lib_math_int32 "mul" int32Input int32Input int32Output hsMul,
    prim2 _hydra_lib_math_int32 "div" int32Input int32Input int32Output hsDiv,
    prim2 _hydra_lib_math_int32 "mod" int32Input int32Input int32Output hsMod,
    prim2 _hydra_lib_math_int32 "rem" int32Input int32Input int32Output hsRem]
