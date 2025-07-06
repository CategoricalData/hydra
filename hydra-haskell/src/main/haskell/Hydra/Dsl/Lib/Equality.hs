module Hydra.Dsl.Lib.Equality where

import Hydra.Core
import Hydra.Graph
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms

import Data.Int


compareInt32 :: TTerm Int -> TTerm Int -> TTerm Comparison
compareInt32 = primitive2 _equality_compareInt32

equal :: TTerm a -> TTerm a -> TTerm Bool
equal = primitive2 _equality_equal

identity :: TTerm x -> TTerm x
identity = primitive1 _equality_identity

gtFloat32 :: TTerm Float -> TTerm Float -> TTerm Bool
gtFloat32 = primitive2 _equality_gtFloat32

gtFloat64 :: TTerm Double -> TTerm Double -> TTerm Bool
gtFloat64 = primitive2 _equality_gtFloat64

gtInt32 :: TTerm Int -> TTerm Int -> TTerm Bool
gtInt32 = primitive2 _equality_gtInt32

gteFloat32 :: TTerm Float -> TTerm Float -> TTerm Bool
gteFloat32 = primitive2 _equality_gteFloat32

gteFloat64 :: TTerm Double -> TTerm Double -> TTerm Bool
gteFloat64 = primitive2 _equality_gteFloat64

gteInt32 :: TTerm Int -> TTerm Int -> TTerm Bool
gteInt32 = primitive2 _equality_gteInt32

ltFloat32 :: TTerm Float -> TTerm Float -> TTerm Bool
ltFloat32 = primitive2 _equality_ltFloat32

ltFloat64 :: TTerm Double -> TTerm Double -> TTerm Bool
ltFloat64 = primitive2 _equality_ltFloat64

ltInt32 :: TTerm Int -> TTerm Int -> TTerm Bool
ltInt32 = primitive2 _equality_ltInt32

lteFloat32 :: TTerm Float -> TTerm Float -> TTerm Bool
lteFloat32 = primitive2 _equality_lteFloat32

lteFloat64 :: TTerm Double -> TTerm Double -> TTerm Bool
lteFloat64 = primitive2 _equality_lteFloat64

lteInt32 :: TTerm Int -> TTerm Int -> TTerm Bool
lteInt32 = primitive2 _equality_lteInt32
