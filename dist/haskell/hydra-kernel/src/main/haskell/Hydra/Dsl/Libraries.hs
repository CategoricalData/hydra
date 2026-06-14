
-- | Implementations of the Hydra standard libraries in Haskell
module Hydra.Dsl.Libraries (
  module Hydra.Dsl.Libraries,
) where

import Hydra.Kernel
import Hydra.Dsl.Prims as Prims
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Hydra.Haskell.Lib.Chars as Chars
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Math as Math
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Regex as Regex
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Lib.Chars as DefChars
import qualified Hydra.Lib.Eithers as DefEithers
import qualified Hydra.Lib.Equality as DefEquality
import qualified Hydra.Lib.Lists as DefLists
import qualified Hydra.Lib.Literals as DefLiterals
import qualified Hydra.Lib.Logic as DefLogic
import qualified Hydra.Lib.Maps as DefMaps
import qualified Hydra.Lib.Math as DefMath
import qualified Hydra.Lib.Optionals as DefOptionals
import qualified Hydra.Lib.Pairs as DefPairs
import qualified Hydra.Lib.Regex as DefRegex
import qualified Hydra.Lib.Sets as DefSets
import qualified Hydra.Lib.Strings as DefStrings

import qualified Data.List as L


-- Compatibility aliases for legacy _* names (from the old Staging.Lib.Names module).
-- These map old names to the new generated Hydra.Lib.Names constants.

-- ModuleName aliases

-- Chars

-- Eithers

-- Equality

-- Lists

-- Literals

-- Logic

-- Maps

-- Math

-- Optionals

-- Pairs

-- Sets

-- Regex

-- Strings

-- Type classes


-- Type variables (TypeVar) for primitive type schemes
-- Unconstrained
_a, _b, _c, _d, _k, _k1, _k2, _s, _v, _v1, _v2, _w, _x, _y, _z :: TypeVar
_a = v "a"
_b = v "b"
_c = v "c"
_d = v "d"
_k = v "k"
_k1 = v "k1"
_k2 = v "k2"
_s = v "s"
_v = v "v"
_v1 = v "v1"
_v2 = v "v2"
_w = v "w"
_x = v "x"
_y = v "y"
_z = v "z"

-- Ord-constrained
_kOrd, _k1Ord, _k2Ord, _xOrd, _yOrd :: TypeVar
_kOrd = vOrd "k"
_k1Ord = vOrd "k1"
_k2Ord = vOrd "k2"
_xOrd = vOrd "x"
_yOrd = vOrd "y"

-- Eq-constrained
_xEq :: TypeVar
_xEq = vEq "x"

-- Term coders for type variables (used in primitive implementations)
a_, b_, c_, d_, k_, k1_, k2_, s_, v_, v1_, v2_, w_, x_, y_, z_ :: TermCoder Term
a_ = variable "a"
b_ = variable "b"
c_ = variable "c"
d_ = variable "d"
k_ = variable "k"
k1_ = variable "k1"
k2_ = variable "k2"
s_ = variable "s"
v_ = variable "v"
v1_ = variable "v1"
v2_ = variable "v2"
w_ = variable "w"
x_ = variable "x"
y_ = variable "y"
z_ = variable "z"

-- | A TermCoder for function types which uses beta reduction to bridge term-level
--   functions to native functions. This allows higher-order primitives like map,
--   filter, foldl, etc. to use native implementations rather than eval-level ones.
--   Used by the DYNAMIC higher-order primitives (those that must inspect a reduced
--   per-element application result), pending their migration to term-level
--   defaultImplementations under issue #446.
fun :: TermCoder x -> TermCoder y -> TermCoder (x -> y)
fun = Prims.functionWithReduce (\cx g t -> reduceTerm cx g True t)

-- | A graph-free TermCoder for function types (issue #446): the bridged native function builds an
--   unreduced @apply funTerm argTerm@ term and lets the outer reducer fold it, rather than calling
--   the reducer here. Used by the STATIC higher-order primitives — those whose result shape is fixed
--   by the (already-reduced) data argument's spine, so they never inspect a reduced per-element
--   application. This is the form that requires no InferenceContext/Graph, enabling the carrier drop.
funT :: TermCoder x -> TermCoder y -> TermCoder (x -> y)
funT = Prims.functionDeferred

hydraLibChars :: Library
hydraLibChars = standardLibrary (ModuleName "hydra.lib.chars") [
  prim1 (Prims.primName DefChars.isAlphaNum) Chars.isAlphaNum [] int32 boolean,
  prim1 (Prims.primName DefChars.isLower)    Chars.isLower    [] int32 boolean,
  prim1 (Prims.primName DefChars.isSpace)    Chars.isSpace    [] int32 boolean,
  prim1 (Prims.primName DefChars.isUpper)    Chars.isUpper    [] int32 boolean,
  prim1 (Prims.primName DefChars.toLower)    Chars.toLower    [] int32 int32,
  prim1 (Prims.primName DefChars.toUpper)    Chars.toUpper    [] int32 int32]

hydraLibEithers :: Library
hydraLibEithers = standardLibrary (ModuleName "hydra.lib.eithers") [
    prim3       (Prims.primName DefEithers.bimap)            Eithers.bimap            [_x, _y, _z, _w] (funT x_ z_) (funT y_ w_) (Prims.either_ x_ y_) (Prims.either_ z_ w_),
    prim2       (Prims.primName DefEithers.bind)             Eithers.bind             [_x, _y, _z]     (Prims.either_ x_ y_) (fun y_ (Prims.either_ x_ z_)) (Prims.either_ x_ z_),
    prim3       (Prims.primName DefEithers.either)           Eithers.either           [_x, _y, _z]     (funT x_ z_) (funT y_ z_) (Prims.either_ x_ y_) z_,
    prim3       (Prims.primName DefEithers.foldl)            Eithers.foldl            [_x, _y, _z]     (fun x_ (fun y_ (Prims.either_ z_ x_))) x_ (list y_) (Prims.either_ z_ x_),
    Prims.lazyArgs [0] $ prim2 (Prims.primName DefEithers.fromLeft)  Eithers.fromLeft  [_x, _y]         x_ (Prims.either_ x_ y_) x_,
    Prims.lazyArgs [0] $ prim2 (Prims.primName DefEithers.fromRight) Eithers.fromRight [_x, _y]         y_ (Prims.either_ x_ y_) y_,
    prim1       (Prims.primName DefEithers.isLeft)           Eithers.isLeft           [_x, _y]         (Prims.either_ x_ y_) boolean,
    prim1       (Prims.primName DefEithers.isRight)          Eithers.isRight          [_x, _y]         (Prims.either_ x_ y_) boolean,
    prim1       (Prims.primName DefEithers.lefts)            Eithers.lefts            [_x, _y]         (list $ Prims.either_ x_ y_) (list x_),
    prim2       (Prims.primName DefEithers.map)              Eithers.map              [_x, _y, _z]     (funT x_ y_) (Prims.either_ z_ x_) (Prims.either_ z_ y_),
    prim2       (Prims.primName DefEithers.mapList)          Eithers.mapList          [_x, _y, _z]     (fun x_ (Prims.either_ z_ y_)) (list x_) (Prims.either_ z_ (list y_)),
    prim2       (Prims.primName DefEithers.mapOptional)         Eithers.mapOptional         [_x, _y, _z]     (fun x_ (Prims.either_ z_ y_)) (optional x_) (Prims.either_ z_ (optional y_)),
    prim2       (Prims.primName DefEithers.mapSet)           Eithers.mapSet           [_x, _y, _z]     (fun x_ (Prims.either_ z_ y_)) (set x_) (Prims.either_ z_ (set y_)),
    prim1       (Prims.primName DefEithers.partitionEithers) Eithers.partitionEithers [_x, _y]         (list $ Prims.either_ x_ y_) (pair (list x_) (list y_)),
    prim1       (Prims.primName DefEithers.rights)           Eithers.rights           [_x, _y]         (list $ Prims.either_ x_ y_) (list y_)]

hydraLibEquality :: Library
hydraLibEquality = standardLibrary (ModuleName "hydra.lib.equality") [
    prim2 (Prims.primName DefEquality.compare)  Equality.compare  [_xOrd] x_ x_ comparison,
    prim2 (Prims.primName DefEquality.equal)    Equality.equal    [_xEq]  x_ x_ boolean,
    prim2 (Prims.primName DefEquality.gt)       Equality.gt       [_xOrd] x_ x_ boolean,
    prim2 (Prims.primName DefEquality.gte)      Equality.gte      [_xOrd] x_ x_ boolean,
    prim1 (Prims.primName DefEquality.identity) Equality.identity [_x]    x_ x_,
    prim2 (Prims.primName DefEquality.lt)       Equality.lt       [_xOrd] x_ x_ boolean,
    prim2 (Prims.primName DefEquality.lte)      Equality.lte      [_xOrd] x_ x_ boolean,
    prim2 (Prims.primName DefEquality.max)      Equality.max      [_xOrd] x_ x_ x_,
    prim2 (Prims.primName DefEquality.min)      Equality.min      [_xOrd] x_ x_ x_]

hydraLibLists :: Library
hydraLibLists = standardLibrary (ModuleName "hydra.lib.lists") [
    prim2     (Prims.primName DefLists.apply)        Lists.apply         [_x, _y]     (list $ funT x_ y_) (list x_) (list y_),
    prim2     (Prims.primName DefLists.bind)        Lists.bind          [_x, _y]     (list x_) (fun x_ (list y_)) (list y_),
    prim1     (Prims.primName DefLists.concat)      Lists.concat        [_x]         (list (list x_)) (list x_),
    prim2     (Prims.primName DefLists.concat2)     Lists.concat2       [_x]         (list x_) (list x_) (list x_),
    prim2     (Prims.primName DefLists.cons)        Lists.cons          [_x]         x_ (list x_) (list x_),
    prim2     (Prims.primName DefLists.drop)        Lists.drop          [_x]         int32 (list x_) (list x_),
    prim2     (Prims.primName DefLists.dropWhile)   Lists.dropWhile     [_x]         (fun x_ boolean) (list x_) (list x_),
    prim2     (Prims.primName DefLists.elem)        Lists.elem          [_xEq]       x_ (list x_) boolean,
    prim2     (Prims.primName DefLists.filter)      Lists.filter        [_x]         (fun x_ boolean) (list x_) (list x_),
    prim2     (Prims.primName DefLists.find)        Lists.find          [_x]         (fun x_ boolean) (list x_) (optional x_),
    prim3     (Prims.primName DefLists.foldl)       Lists.foldl         [_y, _x]     (funT y_ (funT x_ y_)) y_ (list x_) y_,
    prim3     (Prims.primName DefLists.foldr)       Lists.foldr         [_x, _y]     (funT x_ (funT y_ y_)) y_ (list x_) y_,
    prim1     (Prims.primName DefLists.group)       Lists.group         [_xEq]       (list x_) (list (list x_)),
    prim2     (Prims.primName DefLists.intercalate) Lists.intercalate   [_x]         (list x_) (list (list x_)) (list x_),
    prim2     (Prims.primName DefLists.intersperse) Lists.intersperse   [_x]         x_ (list x_) (list x_),
    prim1     (Prims.primName DefLists.length)      Lists.length        [_x]         (list x_) int32,
    prim2     (Prims.primName DefLists.map)         Lists.map           [_x, _y]     (funT x_ y_) (list x_) (list y_),
    prim2     (Prims.primName DefLists.maybeAt)     Lists.maybeAt       [_x]         int32 (list x_) (optional x_),
    prim1     (Prims.primName DefLists.maybeHead)   Lists.maybeHead     [_x]         (list x_) (optional x_),
    prim1     (Prims.primName DefLists.maybeInit)   Lists.maybeInit     [_x]         (list x_) (optional (list x_)),
    prim1     (Prims.primName DefLists.maybeLast)   Lists.maybeLast     [_x]         (list x_) (optional x_),
    prim1     (Prims.primName DefLists.maybeTail)   Lists.maybeTail     [_x]         (list x_) (optional (list x_)),
    prim1     (Prims.primName DefLists.nub)         Lists.nub           [_xEq]       (list x_) (list x_),
    prim1     (Prims.primName DefLists.null)        Lists.null          [_x]         (list x_) boolean,
    prim2     (Prims.primName DefLists.partition)   Lists.partition     [_x]         (fun x_ boolean) (list x_) (pair (list x_) (list x_)),
    prim1     (Prims.primName DefLists.pure)        Lists.pure          [_x]         x_ (list x_),
    prim2     (Prims.primName DefLists.replicate)   Lists.replicate     [_x]         int32 x_ (list x_),
    prim1     (Prims.primName DefLists.reverse)     Lists.reverse       [_x]         (list x_) (list x_),
    prim1     (Prims.primName DefLists.singleton)   Lists.singleton     [_x]         x_ (list x_),
    prim1     (Prims.primName DefLists.sort)        Lists.sort          [_xOrd]      (list x_) (list x_),
    prim2     (Prims.primName DefLists.sortOn)      Lists.sortOn        [_x, _yOrd]  (fun x_ y_) (list x_) (list x_),
    prim2     (Prims.primName DefLists.span)        Lists.span          [_x]         (fun x_ boolean) (list x_) (pair (list x_) (list x_)),
    prim2     (Prims.primName DefLists.take)        Lists.take          [_x]         int32 (list x_) (list x_),
    prim1     (Prims.primName DefLists.transpose)   Lists.transpose     [_x]         (list (list x_)) (list (list x_)),
    prim1     (Prims.primName DefLists.uncons)      Lists.uncons        [_x]         (list x_) (optional (pair x_ (list x_))),
    prim2     (Prims.primName DefLists.zip)         Lists.zip           [_x, _y]     (list x_) (list y_) (list (pair x_ y_)),
    prim3     (Prims.primName DefLists.zipWith)     Lists.zipWith       [_x, _y, _z] (funT x_ $ funT y_ z_) (list x_) (list y_) (list z_)]

hydraLibLiterals :: Library
hydraLibLiterals = standardLibrary (ModuleName "hydra.lib.literals") [
  prim1 (Prims.primName DefLiterals.bigintToDecimal)   Literals.bigintToDecimal   [] bigint decimal,
  prim1 (Prims.primName DefLiterals.bigintToInt8)      Literals.bigintToInt8      [] bigint int8,
  prim1 (Prims.primName DefLiterals.bigintToInt16)     Literals.bigintToInt16     [] bigint int16,
  prim1 (Prims.primName DefLiterals.bigintToInt32)     Literals.bigintToInt32     [] bigint int32,
  prim1 (Prims.primName DefLiterals.bigintToInt64)     Literals.bigintToInt64     [] bigint int64,
  prim1 (Prims.primName DefLiterals.bigintToUint8)     Literals.bigintToUint8     [] bigint uint8,
  prim1 (Prims.primName DefLiterals.bigintToUint16)    Literals.bigintToUint16    [] bigint uint16,
  prim1 (Prims.primName DefLiterals.bigintToUint32)    Literals.bigintToUint32    [] bigint uint32,
  prim1 (Prims.primName DefLiterals.bigintToUint64)    Literals.bigintToUint64    [] bigint uint64,
  prim1 (Prims.primName DefLiterals.binaryToBytes)     Literals.binaryToBytes     [] binary (list int32),
  prim1 (Prims.primName DefLiterals.binaryToString)    Literals.binaryToString    [] binary string,
  prim1 (Prims.primName DefLiterals.decimalToBigint)   Literals.decimalToBigint   [] decimal bigint,
  prim1 (Prims.primName DefLiterals.decimalToFloat32)  Literals.decimalToFloat32  [] decimal float32,
  prim1 (Prims.primName DefLiterals.decimalToFloat64)  Literals.decimalToFloat64  [] decimal float64,
  prim1 (Prims.primName DefLiterals.float32ToDecimal)  Literals.float32ToDecimal  [] float32 decimal,
  prim1 (Prims.primName DefLiterals.float32ToFloat64)  Literals.float32ToFloat64  [] float32 float64,
  prim1 (Prims.primName DefLiterals.float64ToDecimal)  Literals.float64ToDecimal  [] float64 decimal,
  prim1 (Prims.primName DefLiterals.float64ToFloat32)  Literals.float64ToFloat32  [] float64 float32,
  prim1 (Prims.primName DefLiterals.int8ToBigint)      Literals.int8ToBigint      [] int8 bigint,
  prim1 (Prims.primName DefLiterals.int16ToBigint)     Literals.int16ToBigint     [] int16 bigint,
  prim1 (Prims.primName DefLiterals.int32ToBigint)     Literals.int32ToBigint     [] int32 bigint,
  prim1 (Prims.primName DefLiterals.int64ToBigint)     Literals.int64ToBigint     [] int64 bigint,
  prim1 (Prims.primName DefLiterals.readBigint)        Literals.readBigint        [] string (optional bigint),
  prim1 (Prims.primName DefLiterals.readBoolean)       Literals.readBoolean       [] string (optional boolean),
  prim1 (Prims.primName DefLiterals.readDecimal)       Literals.readDecimal       [] string (optional decimal),
  prim1 (Prims.primName DefLiterals.readFloat32)       Literals.readFloat32       [] string (optional float32),
  prim1 (Prims.primName DefLiterals.readFloat64)       Literals.readFloat64       [] string (optional float64),
  prim1 (Prims.primName DefLiterals.readInt8)          Literals.readInt8          [] string (optional int8),
  prim1 (Prims.primName DefLiterals.readInt16)         Literals.readInt16         [] string (optional int16),
  prim1 (Prims.primName DefLiterals.readInt32)         Literals.readInt32         [] string (optional int32),
  prim1 (Prims.primName DefLiterals.readInt64)         Literals.readInt64         [] string (optional int64),
  prim1 (Prims.primName DefLiterals.readString)        Literals.readString        [] string (optional string),
  prim1 (Prims.primName DefLiterals.readUint8)         Literals.readUint8         [] string (optional uint8),
  prim1 (Prims.primName DefLiterals.readUint16)        Literals.readUint16        [] string (optional uint16),
  prim1 (Prims.primName DefLiterals.readUint32)        Literals.readUint32        [] string (optional uint32),
  prim1 (Prims.primName DefLiterals.readUint64)        Literals.readUint64        [] string (optional uint64),
  prim1 (Prims.primName DefLiterals.showBigint)        Literals.showBigint        [] bigint string,
  prim1 (Prims.primName DefLiterals.showBoolean)       Literals.showBoolean       [] boolean string,
  prim1 (Prims.primName DefLiterals.showDecimal)       Literals.showDecimal       [] decimal string,
  prim1 (Prims.primName DefLiterals.showFloat32)       Literals.showFloat32       [] float32 string,
  prim1 (Prims.primName DefLiterals.showFloat64)       Literals.showFloat64       [] float64 string,
  prim1 (Prims.primName DefLiterals.showInt8)          Literals.showInt8          [] int8 string,
  prim1 (Prims.primName DefLiterals.showInt16)         Literals.showInt16         [] int16 string,
  prim1 (Prims.primName DefLiterals.showInt32)         Literals.showInt32         [] int32 string,
  prim1 (Prims.primName DefLiterals.showInt64)         Literals.showInt64         [] int64 string,
  prim1 (Prims.primName DefLiterals.showString)        Literals.showString        [] string string,
  prim1 (Prims.primName DefLiterals.showUint8)         Literals.showUint8         [] uint8 string,
  prim1 (Prims.primName DefLiterals.showUint16)        Literals.showUint16        [] uint16 string,
  prim1 (Prims.primName DefLiterals.showUint32)        Literals.showUint32        [] uint32 string,
  prim1 (Prims.primName DefLiterals.showUint64)        Literals.showUint64        [] uint64 string,
  prim1 (Prims.primName DefLiterals.stringToBinary)    Literals.stringToBinary    [] string binary,
  prim1 (Prims.primName DefLiterals.uint8ToBigint)     Literals.uint8ToBigint     [] uint8 bigint,
  prim1 (Prims.primName DefLiterals.uint16ToBigint)    Literals.uint16ToBigint    [] uint16 bigint,
  prim1 (Prims.primName DefLiterals.uint32ToBigint)    Literals.uint32ToBigint    [] uint32 bigint,
  prim1 (Prims.primName DefLiterals.uint64ToBigint)    Literals.uint64ToBigint    [] uint64 bigint]

hydraLibLogic :: Library
hydraLibLogic = standardLibrary (ModuleName "hydra.lib.logic") [
    prim2 (Prims.primName DefLogic.and)    Logic.and    []   boolean boolean boolean,
    Prims.lazyArgs [1, 2] $ prim3 (Prims.primName DefLogic.ifElse) Logic.ifElse [_x] boolean x_ x_ x_,
    prim1 (Prims.primName DefLogic.not)    Logic.not    []   boolean boolean,
    prim2 (Prims.primName DefLogic.or)     Logic.or     []   boolean boolean boolean]

hydraLibMaps :: Library
hydraLibMaps = standardLibrary (ModuleName "hydra.lib.maps") [
    prim3     (Prims.primName DefMaps.alter)           Maps.alter             [_v, _kOrd]                  (fun (optional v_) (optional v_)) k_ mapKv mapKv,
    prim3     (Prims.primName DefMaps.bimap)           Maps.bimap             [_k1Ord, _k2Ord, _v1, _v2]   (fun k1_ k2_) (fun v1_ v2_) (Prims.map k1_ v1_) (Prims.map k2_ v2_),
    prim2     (Prims.primName DefMaps.delete)          Maps.delete            [_kOrd, _v]                  k_ mapKv mapKv,
    prim1     (Prims.primName DefMaps.elems)           Maps.elems             [_kOrd, _v]                  mapKv (list v_),
    prim0     (Prims.primName DefMaps.empty)           Maps.empty             [_kOrd, _v]                  mapKv,
    prim2     (Prims.primName DefMaps.filter)          Maps.filter            [_v, _kOrd]                  (fun v_ boolean) mapKv mapKv,
    prim2     (Prims.primName DefMaps.filterWithKey)   Maps.filterWithKey     [_kOrd, _v]                  (fun k_ (fun v_ boolean)) mapKv mapKv,
    Prims.lazyArgs [0] $ prim3 (Prims.primName DefMaps.findWithDefault) Maps.findWithDefault   [_v, _kOrd]                  v_ k_ mapKv v_,
    prim1     (Prims.primName DefMaps.fromList)        Maps.fromList          [_kOrd, _v]                  (list $ pair k_ v_) mapKv,
    prim3     (Prims.primName DefMaps.insert)          Maps.insert            [_kOrd, _v]                  k_ v_ mapKv mapKv,
    prim1     (Prims.primName DefMaps.keys)            Maps.keys              [_kOrd, _v]                  mapKv (list k_),
    prim2     (Prims.primName DefMaps.lookup)          Maps.lookup            [_kOrd, _v]                  k_ mapKv (optional v_),
    prim2     (Prims.primName DefMaps.map)             Maps.map               [_v1, _v2, _kOrd]            (funT v1_ v2_) (Prims.map k_ v1_) (Prims.map k_ v2_),
    prim2     (Prims.primName DefMaps.mapKeys)         Maps.mapKeys           [_k1Ord, _k2Ord, _v]         (fun k1_ k2_) (Prims.map k1_ v_) (Prims.map k2_ v_),
    prim2     (Prims.primName DefMaps.member)          Maps.member            [_kOrd, _v]                  k_ mapKv boolean,
    prim1     (Prims.primName DefMaps.null)            Maps.null              [_kOrd, _v]                  mapKv boolean,
    prim2     (Prims.primName DefMaps.singleton)       Maps.singleton         [_kOrd, _v]                  k_ v_ mapKv,
    prim1     (Prims.primName DefMaps.size)            Maps.size              [_kOrd, _v]                  mapKv int32,
    prim1     (Prims.primName DefMaps.toList)          Maps.toList            [_kOrd, _v]                  mapKv (list $ pair k_ v_),
    prim2     (Prims.primName DefMaps.union)           Maps.union             [_kOrd, _v]                  mapKv mapKv mapKv]
  where
    mapKv = Prims.map k_ v_

hydraLibMathFloat64 :: Library
hydraLibMathFloat64 = standardLibrary (ModuleName "hydra.lib.math") [
  prim1 (Prims.primName DefMath.acos)     Math.acos     [] float64 float64,
  prim1 (Prims.primName DefMath.acosh)    Math.acosh    [] float64 float64,
  prim2 (Prims.primName DefMath.addFloat64) Math.addFloat64 [] float64 float64 float64,
  prim1 (Prims.primName DefMath.asin)     Math.asin     [] float64 float64,
  prim1 (Prims.primName DefMath.asinh)    Math.asinh    [] float64 float64,
  prim1 (Prims.primName DefMath.atan)     Math.atan     [] float64 float64,
  prim2 (Prims.primName DefMath.atan2)    Math.atan2    [] float64 float64 float64,
  prim1 (Prims.primName DefMath.atanh)    Math.atanh    [] float64 float64,
  prim1 (Prims.primName DefMath.ceiling)  Math.ceiling  [] float64 float64,
  prim1 (Prims.primName DefMath.cos)      Math.cos      [] float64 float64,
  prim1 (Prims.primName DefMath.cosh)     Math.cosh     [] float64 float64,
  prim0 (Prims.primName DefMath.e)        Math.e        [] float64,
  prim1 (Prims.primName DefMath.exp)      Math.exp      [] float64 float64,
  prim1 (Prims.primName DefMath.floor)    Math.floor    [] float64 float64,
  prim1 (Prims.primName DefMath.log)      Math.log      [] float64 float64,
  prim2 (Prims.primName DefMath.logBase)  Math.logBase  [] float64 float64 float64,
  prim2 (Prims.primName DefMath.mulFloat64) Math.mulFloat64 [] float64 float64 float64,
  prim1 (Prims.primName DefMath.negateFloat64) Math.negateFloat64 [] float64 float64,
  prim0 (Prims.primName DefMath.pi)       Math.pi       [] float64,
  prim2 (Prims.primName DefMath.pow)      Math.pow      [] float64 float64 float64,
  prim1 (Prims.primName DefMath.round)         Math.round         [] float64 float64,
  prim2 (Prims.primName DefMath.roundFloat32)  Math.roundFloat32  [] int32 float32 float32,
  prim2 (Prims.primName DefMath.roundFloat64)  Math.roundFloat64  [] int32 float64 float64,
  prim1 (Prims.primName DefMath.sin)            Math.sin           [] float64 float64,
  prim1 (Prims.primName DefMath.sinh)     Math.sinh     [] float64 float64,
  prim1 (Prims.primName DefMath.sqrt)     Math.sqrt     [] float64 float64,
  prim2 (Prims.primName DefMath.subFloat64) Math.subFloat64 [] float64 float64 float64,
  prim1 (Prims.primName DefMath.tan)      Math.tan      [] float64 float64,
  prim1 (Prims.primName DefMath.tanh)     Math.tanh     [] float64 float64,
  prim1 (Prims.primName DefMath.truncate) Math.truncate [] float64 float64]

hydraLibMathInt32 :: Library
hydraLibMathInt32 = standardLibrary (ModuleName "hydra.lib.math") [
  prim1 (Prims.primName DefMath.abs)    Math.abs    [] int32 int32,
  prim2 (Prims.primName DefMath.add)    Math.add    [] int32 int32 int32,
  prim1 (Prims.primName DefMath.even)   Math.even   [] int32 boolean,
  prim2 (Prims.primName DefMath.max)    Math.max    [] int32 int32 int32,
  prim2 (Prims.primName DefMath.maybeDiv) Math.maybeDiv [] int32 int32 (optional int32),
  prim2 (Prims.primName DefMath.min)    Math.min    [] int32 int32 int32,
  prim2 (Prims.primName DefMath.maybeMod) Math.maybeMod [] int32 int32 (optional int32),
  prim2 (Prims.primName DefMath.mul)    Math.mul    [] int32 int32 int32,
  prim1 (Prims.primName DefMath.negate) Math.negate [] int32 int32,
  prim1 (Prims.primName DefMath.odd)    Math.odd    [] int32 boolean,
  prim1 (Prims.primName DefMath.maybePred) Math.maybePred [] int32 (optional int32),
  prim2 (Prims.primName DefMath.range)  Math.range  [] int32 int32 (list int32),
  prim2 (Prims.primName DefMath.maybeRem) Math.maybeRem [] int32 int32 (optional int32),
  prim1 (Prims.primName DefMath.signum) Math.signum [] int32 int32,
  prim2 (Prims.primName DefMath.sub)    Math.sub    [] int32 int32 int32,
  prim1 (Prims.primName DefMath.maybeSucc) Math.maybeSucc [] int32 (optional int32)]

hydraLibOptionals :: Library
hydraLibOptionals = standardLibrary (ModuleName "hydra.lib.optionals") [
    prim2     (Prims.primName DefOptionals.apply)     Optionals.apply        [_x, _y]     (optional $ funT x_ y_) (optional x_) (optional y_),
    prim2     (Prims.primName DefOptionals.bind)      Optionals.bind         [_x, _y]     (optional x_) (fun x_ (optional y_)) (optional y_),
    Prims.lazyArgs [1] $ prim3 (Prims.primName DefOptionals.cases)     Optionals.cases        [_x, _y]     (optional x_) y_ (funT x_ y_) y_,
    prim1     (Prims.primName DefOptionals.cat)       Optionals.cat          [_x]         (list $ optional x_) (list x_),
    prim3     (Prims.primName DefOptionals.compose)   Optionals.compose      [_x, _y, _z] (fun x_ $ optional y_) (fun y_ $ optional z_) x_ (optional z_),
    Prims.lazyArgs [0] $ prim2 (Prims.primName DefOptionals.fromOptional) Optionals.fromOptional    [_x]         x_ (optional x_) x_,
    prim1     (Prims.primName DefOptionals.isGiven)    Optionals.isGiven       [_x]         (optional x_) boolean,
    prim1     (Prims.primName DefOptionals.isNone) Optionals.isNone    [_x]         (optional x_) boolean,
    prim2     (Prims.primName DefOptionals.map)       Optionals.map          [_x, _y]     (funT x_ y_) (optional x_) (optional y_),
    prim2     (Prims.primName DefOptionals.mapOptional)  Optionals.mapOptional     [_x, _y]     (fun x_ $ optional y_) (list x_) (list y_),
    prim1     (Prims.primName DefOptionals.pure)      Optionals.pure         [_x]         x_ (optional x_),
    prim1     (Prims.primName DefOptionals.toList)    Optionals.toList       [_x]         (optional x_) (list x_)]

hydraLibPairs :: Library
hydraLibPairs = standardLibrary (ModuleName "hydra.lib.pairs") [
    prim3     (Prims.primName DefPairs.bimap)  Pairs.bimap      [_a, _b, _c, _d] (funT a_ c_) (funT b_ d_) (pair a_ b_) (pair c_ d_),
    prim1     (Prims.primName DefPairs.first)  Pairs.first      [_a, _b]         (pair a_ b_) a_,
    prim1     (Prims.primName DefPairs.second) Pairs.second     [_a, _b]         (pair a_ b_) b_]

hydraLibRegex :: Library
hydraLibRegex = standardLibrary (ModuleName "hydra.lib.regex") [
  prim2 (Prims.primName DefRegex.find)       Regex.find       [] string string (optional string),
  prim2 (Prims.primName DefRegex.findAll)    Regex.findAll    [] string string (list string),
  prim2 (Prims.primName DefRegex.matches)    Regex.matches    [] string string boolean,
  prim3 (Prims.primName DefRegex.replace)    Regex.replace    [] string string string string,
  prim3 (Prims.primName DefRegex.replaceAll) Regex.replaceAll [] string string string string,
  prim2 (Prims.primName DefRegex.split)      Regex.split      [] string string (list string)]

hydraLibSets :: Library
hydraLibSets = standardLibrary (ModuleName "hydra.lib.sets") [
    prim2     (Prims.primName DefSets.delete)       Sets.delete       [_xOrd]        x_ (set x_) (set x_),
    prim2     (Prims.primName DefSets.difference)   Sets.difference   [_xOrd]        (set x_) (set x_) (set x_),
    prim0     (Prims.primName DefSets.empty)        Sets.empty        [_xOrd]        (set x_),
    prim1     (Prims.primName DefSets.fromList)     Sets.fromList     [_xOrd]        (list x_) (set x_),
    prim2     (Prims.primName DefSets.insert)       Sets.insert       [_xOrd]        x_ (set x_) (set x_),
    prim2     (Prims.primName DefSets.intersection) Sets.intersection [_xOrd]        (set x_) (set x_) (set x_),
    prim2     (Prims.primName DefSets.map)          Sets.map          [_xOrd, _yOrd] (fun x_ y_) (set x_) (set y_),
    prim2     (Prims.primName DefSets.member)       Sets.member       [_xOrd]        x_ (set x_) boolean,
    prim1     (Prims.primName DefSets.null)         Sets.null         [_xOrd]        (set x_) boolean,
    prim1     (Prims.primName DefSets.singleton)    Sets.singleton    [_xOrd]        x_ (set x_),
    prim1     (Prims.primName DefSets.size)         Sets.size         [_xOrd]        (set x_) int32,
    prim1     (Prims.primName DefSets.toList)       Sets.toList       [_xOrd]        (set x_) (list x_),
    prim2     (Prims.primName DefSets.union)        Sets.union        [_xOrd]        (set x_) (set x_) (set x_),
    prim1     (Prims.primName DefSets.unions)       Sets.unions       [_xOrd]        (list $ set x_) (set x_)]

hydraLibStrings :: Library
hydraLibStrings = standardLibrary (ModuleName "hydra.lib.strings") [
  prim1 (Prims.primName DefStrings.cat)         Strings.cat         [] (list string) string,
  prim2 (Prims.primName DefStrings.cat2)        Strings.cat2        [] string string string,
  prim1 (Prims.primName DefStrings.fromList)    Strings.fromList    [] (list int32) string,
  prim2 (Prims.primName DefStrings.intercalate) Strings.intercalate [] string (list string) string,
  prim1 (Prims.primName DefStrings.length)      Strings.length      [] string int32,
  prim1 (Prims.primName DefStrings.lines)       Strings.lines       [] string (list string),
  prim2 (Prims.primName DefStrings.maybeCharAt) Strings.maybeCharAt [] int32 string (optional int32),
  prim1 (Prims.primName DefStrings.null)        Strings.null        [] string boolean,
  prim2 (Prims.primName DefStrings.splitOn)     Strings.splitOn     [] string string (list string),
  prim1 (Prims.primName DefStrings.toList)      Strings.toList      [] string (list int32),
  prim1 (Prims.primName DefStrings.toLower)     Strings.toLower     [] string string,
  prim1 (Prims.primName DefStrings.toUpper)     Strings.toUpper     [] string string,
  prim1 (Prims.primName DefStrings.unlines)     Strings.unlines     [] (list string) string]

standardLibraries :: [Library]
standardLibraries = [
  hydraLibChars,
  hydraLibEithers,
  hydraLibEquality,
  hydraLibLists,
  hydraLibLiterals,
  hydraLibLogic,
  hydraLibMaps,
  hydraLibMathFloat64,
  hydraLibMathInt32,
  hydraLibOptionals,
  hydraLibPairs,
  hydraLibRegex,
  hydraLibSets,
  hydraLibStrings]

standardLibrary :: ModuleName -> [Primitive] -> Library
standardLibrary ns prims = Library {
  libraryName = ns,
  libraryPrefix = L.drop (L.length ("hydra.lib." :: String)) $ unModuleName ns,
  libraryPrimitives = prims}
