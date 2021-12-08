module Hydra.Lib.Math where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Prims


hsNeg :: Int -> Result Int
hsNeg = pure . negate

hsAdd :: Int -> Int -> Result Int
hsAdd x y = pure $ x + y

hsSub :: Int -> Int -> Result Int
hsSub x y = pure $ x - y

hsMul :: Int -> Int -> Result Int
hsMul x y = pure $ x * y

hsDiv :: Int -> Int -> Result Int
hsDiv x y = pure $ div x y

hsMod :: Int -> Int -> Result Int
hsMod x y = pure $ mod x y

hsRem :: Int -> Int -> Result Int
hsRem x y = pure $ rem x y

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
