
-- | Implementations of the Hydra standard libraries in Haskell
module Hydra.Overlay.Haskell.Libraries (
  module Hydra.Overlay.Haskell.Libraries,
) where

import Hydra.Kernel
import Hydra.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Terms as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Types as Types

import qualified Hydra.Overlay.Haskell.Lib.Chars as Chars
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Overlay.Haskell.Lib.Equality as Equality
import qualified Hydra.Overlay.Haskell.Lib.Functions as Functions
import qualified Hydra.Overlay.Haskell.Lib.Hashing as Hashing
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Literals as Literals
import qualified Hydra.Overlay.Haskell.Lib.Logic as Logic
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Math as Math
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Ordering as Ordering
import qualified Hydra.Overlay.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Overlay.Haskell.Lib.Regex as Regex
import qualified Hydra.Overlay.Haskell.Lib.Sets as Sets
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Overlay.Haskell.Lib.Text as Text
import qualified Hydra.Lib.Chars as DefChars
import qualified Hydra.Lib.Effects as DefEffects
import qualified Hydra.Lib.Eithers as DefEithers
import qualified Hydra.Lib.Equality as DefEquality
import qualified Hydra.Lib.Files as DefFiles
import qualified Hydra.Lib.Functions as DefFunctions
import qualified Hydra.Lib.Hashing as DefHashing
import qualified Hydra.Lib.Lists as DefLists
import qualified Hydra.Lib.Literals as DefLiterals
import qualified Hydra.Lib.Logic as DefLogic
import qualified Hydra.Lib.Maps as DefMaps
import qualified Hydra.Lib.Math as DefMath
import qualified Hydra.Lib.Optionals as DefOptionals
import qualified Hydra.Lib.Ordering as DefOrdering
import qualified Hydra.Lib.Pairs as DefPairs
import qualified Hydra.Lib.Regex as DefRegex
import qualified Hydra.Lib.Sets as DefSets
import qualified Hydra.Lib.Strings as DefStrings
import qualified Hydra.Lib.System as DefSystem
import qualified Hydra.Lib.Text as DefText

import qualified Data.List as L


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
hydraLibChars = standardLibrary [
  prim1 DefChars.isAlpha    Chars.isAlpha    int32 boolean,
  prim1 DefChars.isAlphaNum Chars.isAlphaNum int32 boolean,
  prim1 DefChars.isDigit    Chars.isDigit    int32 boolean,
  prim1 DefChars.isLower    Chars.isLower    int32 boolean,
  prim1 DefChars.isSpace    Chars.isSpace    int32 boolean,
  prim1 DefChars.isUpper    Chars.isUpper    int32 boolean,
  prim1 DefChars.toLower    Chars.toLower    int32 int32,
  prim1 DefChars.toUpper    Chars.toUpper    int32 int32]

hydraLibEffects :: Library
hydraLibEffects = standardLibrary [
    unsupportedEffectPrimitive DefEffects.apply,
    unsupportedEffectPrimitive DefEffects.bind,
    unsupportedEffectPrimitive DefEffects.compose,
    unsupportedEffectPrimitive DefEffects.foldList,
    unsupportedEffectPrimitive DefEffects.map,
    unsupportedEffectPrimitive DefEffects.mapList,
    unsupportedEffectPrimitive DefEffects.mapOptional,
    unsupportedEffectPrimitive DefEffects.mapSet,
    unsupportedEffectPrimitive DefEffects.pure]

hydraLibFiles :: Library
hydraLibFiles = standardLibrary [
    unsupportedEffectPrimitive DefFiles.appendFile,
    unsupportedEffectPrimitive DefFiles.copy,
    unsupportedEffectPrimitive DefFiles.createDirectory,
    unsupportedEffectPrimitive DefFiles.exists,
    unsupportedEffectPrimitive DefFiles.listDirectory,
    unsupportedEffectPrimitive DefFiles.readFile,
    unsupportedEffectPrimitive DefFiles.removeDirectory,
    unsupportedEffectPrimitive DefFiles.removeFile,
    unsupportedEffectPrimitive DefFiles.rename,
    unsupportedEffectPrimitive DefFiles.status,
    unsupportedEffectPrimitive DefFiles.writeFile]

hydraLibEithers :: Library
hydraLibEithers = standardLibrary [
    prim2       DefEithers.apply            Eithers.apply            (Prims.either_ x_ (funT y_ z_)) (Prims.either_ x_ y_) (Prims.either_ x_ z_),
    prim3       DefEithers.bimap            Eithers.bimap            (funT x_ z_) (funT y_ w_) (Prims.either_ x_ y_) (Prims.either_ z_ w_),
    prim2       DefEithers.bind             Eithers.bind             (Prims.either_ x_ y_) (fun y_ (Prims.either_ x_ z_)) (Prims.either_ x_ z_),
    prim3       DefEithers.compose          Eithers.compose          (fun x_ (Prims.either_ w_ y_)) (fun y_ (Prims.either_ w_ z_)) x_ (Prims.either_ w_ z_),
    prim3       DefEithers.either           Eithers.either           (funT x_ z_) (funT y_ z_) (Prims.either_ x_ y_) z_,
    prim3       DefEithers.foldList         Eithers.foldList         (fun x_ (fun y_ (Prims.either_ z_ x_))) x_ (list y_) (Prims.either_ z_ x_),
    prim3       DefEithers.foldList            Eithers.foldList            (fun x_ (fun y_ (Prims.either_ z_ x_))) x_ (list y_) (Prims.either_ z_ x_),
    prim1       DefEithers.isLeft           Eithers.isLeft           (Prims.either_ x_ y_) boolean,
    prim1       DefEithers.isRight          Eithers.isRight          (Prims.either_ x_ y_) boolean,
    prim1       DefEithers.lefts            Eithers.lefts            (list $ Prims.either_ x_ y_) (list x_),
    prim2       DefEithers.map              Eithers.map              (funT x_ y_) (Prims.either_ z_ x_) (Prims.either_ z_ y_),
    prim2       DefEithers.mapList          Eithers.mapList          (fun x_ (Prims.either_ z_ y_)) (list x_) (Prims.either_ z_ (list y_)),
    prim2       DefEithers.mapOptional         Eithers.mapOptional         (fun x_ (Prims.either_ z_ y_)) (optional x_) (Prims.either_ z_ (optional y_)),
    prim2       DefEithers.mapSet           Eithers.mapSet           (fun x_ (Prims.either_ z_ y_)) (set x_) (Prims.either_ z_ (set y_)),
    prim1       DefEithers.partition        Eithers.partition        (list $ Prims.either_ x_ y_) (pair (list x_) (list y_)),
    prim1       DefEithers.partition Eithers.partition (list $ Prims.either_ x_ y_) (pair (list x_) (list y_)),
    prim1       DefEithers.pure             Eithers.pure             y_ (Prims.either_ x_ y_),
    prim1       DefEithers.rights           Eithers.rights           (list $ Prims.either_ x_ y_) (list y_)]

hydraLibEquality :: Library
hydraLibEquality = standardLibrary [
    prim2 DefEquality.equal    Equality.equal    x_ x_ boolean,
    prim2 DefEquality.notEqual Equality.notEqual x_ x_ boolean]

hydraLibOrdering :: Library
hydraLibOrdering = standardLibrary [
    prim2 DefOrdering.compare  Ordering.compare  x_ x_ comparison,
    prim2 DefOrdering.gt       Ordering.gt       x_ x_ boolean,
    prim2 DefOrdering.gte      Ordering.gte      x_ x_ boolean,
    prim2 DefOrdering.lt       Ordering.lt       x_ x_ boolean,
    prim2 DefOrdering.lte      Ordering.lte      x_ x_ boolean,
    prim2 DefOrdering.max      Ordering.max      x_ x_ x_,
    prim2 DefOrdering.min      Ordering.min      x_ x_ x_]

hydraLibFunctions :: Library
hydraLibFunctions = standardLibrary [
    prim3 DefFunctions.compose  Functions.compose  (funT y_ z_) (funT x_ y_) x_ z_,
    prim2 DefFunctions.const    Functions.const    x_ y_ x_,
    prim3 DefFunctions.flip     Functions.flip     (funT x_ (funT y_ z_)) y_ x_ z_,
    prim1 DefFunctions.identity Functions.identity x_ x_]

hydraLibLists :: Library
hydraLibLists = standardLibrary [
    prim2     DefLists.apply        Lists.apply         (list $ funT x_ y_) (list x_) (list y_),
    prim2     DefLists.at          Lists.at            int32 (list x_) (optional x_),
    prim2     DefLists.bind        Lists.bind          (list x_) (fun x_ (list y_)) (list y_),
    prim3     DefLists.compose     Lists.compose       (fun x_ (list y_)) (fun y_ (list z_)) x_ (list z_),
    prim1     DefLists.concat      Lists.concat        (list (list x_)) (list x_),
    prim2     DefLists.concat2     Lists.concat2       (list x_) (list x_) (list x_),
    prim2     DefLists.cons        Lists.cons          x_ (list x_) (list x_),
    prim1     DefLists.distinct    Lists.distinct      (list x_) (list x_),
    prim2     DefLists.drop        Lists.drop          int32 (list x_) (list x_),
    prim2     DefLists.dropWhile   Lists.dropWhile     (fun x_ boolean) (list x_) (list x_),
    prim2     DefLists.member        Lists.member          x_ (list x_) boolean,
    prim2     DefLists.filter      Lists.filter        (fun x_ boolean) (list x_) (list x_),
    prim2     DefLists.find        Lists.find          (fun x_ boolean) (list x_) (optional x_),
    prim3     DefLists.foldList    Lists.foldList      (fun x_ (fun y_ (list x_))) x_ (list y_) (list x_),
    prim3     DefLists.foldl       Lists.foldl         (funT y_ (funT x_ y_)) y_ (list x_) y_,
    prim3     DefLists.foldr       Lists.foldr         (funT x_ (funT y_ y_)) y_ (list x_) y_,
    prim1     DefLists.group       Lists.group         (list x_) (list (list x_)),
    prim1     DefLists.head        Lists.head          (list x_) (optional x_),
    prim1     DefLists.init        Lists.init          (list x_) (optional (list x_)),
    prim2     DefLists.join Lists.join   (list x_) (list (list x_)) (list x_),
    prim2     DefLists.join        Lists.join          (list x_) (list (list x_)) (list x_),
    prim2     DefLists.intersperse Lists.intersperse   x_ (list x_) (list x_),
    prim1     DefLists.last        Lists.last          (list x_) (optional x_),
    prim1     DefLists.length      Lists.length        (list x_) int32,
    prim2     DefLists.map         Lists.map           (funT x_ y_) (list x_) (list y_),
    prim2     DefLists.mapList     Lists.mapList       (fun x_ (list y_)) (list x_) (list (list y_)),
    prim2     DefLists.mapOptional Lists.mapOptional   (fun x_ (list y_)) (optional x_) (list (optional y_)),
    prim2     DefLists.mapSet      Lists.mapSet        (fun x_ (list y_)) (set x_) (list (set y_)),
    prim2     DefLists.member      Lists.member        x_ (list x_) boolean,
    prim2     DefLists.at     Lists.at       int32 (list x_) (optional x_),
    prim1     DefLists.head   Lists.head     (list x_) (optional x_),
    prim1     DefLists.init   Lists.init     (list x_) (optional (list x_)),
    prim1     DefLists.last   Lists.last     (list x_) (optional x_),
    prim1     DefLists.tail   Lists.tail     (list x_) (optional (list x_)),
    prim1     DefLists.distinct         Lists.distinct           (list x_) (list x_),
    prim1     DefLists.null        Lists.null          (list x_) boolean,
    prim2     DefLists.partition   Lists.partition     (fun x_ boolean) (list x_) (pair (list x_) (list x_)),
    prim1     DefLists.pure        Lists.pure          x_ (list x_),
    prim2     DefLists.replicate   Lists.replicate     int32 x_ (list x_),
    prim1     DefLists.reverse     Lists.reverse       (list x_) (list x_),
    prim1     DefLists.singleton   Lists.singleton     x_ (list x_),
    prim1     DefLists.sort        Lists.sort          (list x_) (list x_),
    prim2     DefLists.sortBy      Lists.sortBy        (fun x_ y_) (list x_) (list x_),
    prim2     DefLists.sortBy      Lists.sortBy        (fun x_ y_) (list x_) (list x_),
    prim2     DefLists.span        Lists.span          (fun x_ boolean) (list x_) (pair (list x_) (list x_)),
    prim1     DefLists.tail        Lists.tail          (list x_) (optional (list x_)),
    prim2     DefLists.take        Lists.take          int32 (list x_) (list x_),
    prim2     DefLists.takeWhile   Lists.takeWhile     (fun x_ boolean) (list x_) (list x_),
    prim1     DefLists.transpose   Lists.transpose     (list (list x_)) (list (list x_)),
    prim1     DefLists.uncons      Lists.uncons        (list x_) (optional (pair x_ (list x_))),
    prim2     DefLists.zip         Lists.zip           (list x_) (list y_) (list (pair x_ y_)),
    prim3     DefLists.zipWith     Lists.zipWith       (funT x_ $ funT y_ z_) (list x_) (list y_) (list z_)]

hydraLibLiterals :: Library
hydraLibLiterals = standardLibrary [
  prim1 DefLiterals.bigintToDecimal   Literals.bigintToDecimal   bigint decimal,
  prim1 DefLiterals.bigintToInt8      Literals.bigintToInt8      bigint int8,
  prim1 DefLiterals.bigintToInt16     Literals.bigintToInt16     bigint int16,
  prim1 DefLiterals.bigintToInt32     Literals.bigintToInt32     bigint int32,
  prim1 DefLiterals.bigintToInt64     Literals.bigintToInt64     bigint int64,
  prim1 DefLiterals.bigintToUint8     Literals.bigintToUint8     bigint uint8,
  prim1 DefLiterals.bigintToUint16    Literals.bigintToUint16    bigint uint16,
  prim1 DefLiterals.bigintToUint32    Literals.bigintToUint32    bigint uint32,
  prim1 DefLiterals.bigintToUint64    Literals.bigintToUint64    bigint uint64,
  prim1 DefLiterals.binaryToBytes     Literals.binaryToBytes     binary (list int32),
  prim1 DefLiterals.binaryToString    Literals.binaryToString    binary string,
  prim1 DefLiterals.decimalToBigint   Literals.decimalToBigint   decimal bigint,
  prim1 DefLiterals.decimalToFloat32  Literals.decimalToFloat32  decimal float32,
  prim1 DefLiterals.decimalToFloat64  Literals.decimalToFloat64  decimal float64,
  prim1 DefLiterals.float32ToDecimal  Literals.float32ToDecimal  float32 decimal,
  prim1 DefLiterals.float32ToFloat64  Literals.float32ToFloat64  float32 float64,
  prim1 DefLiterals.float64ToDecimal  Literals.float64ToDecimal  float64 decimal,
  prim1 DefLiterals.float64ToFloat32  Literals.float64ToFloat32  float64 float32,
  prim1 DefLiterals.int8ToBigint      Literals.int8ToBigint      int8 bigint,
  prim1 DefLiterals.int16ToBigint     Literals.int16ToBigint     int16 bigint,
  prim1 DefLiterals.int32ToBigint     Literals.int32ToBigint     int32 bigint,
  prim1 DefLiterals.int64ToBigint     Literals.int64ToBigint     int64 bigint,
  prim1 DefLiterals.readBigint        Literals.readBigint        string (optional bigint),
  prim1 DefLiterals.parseBoolean       Literals.parseBoolean       string (optional boolean),
  prim1 DefLiterals.readDecimal       Literals.readDecimal       string (optional decimal),
  prim1 DefLiterals.readFloat32       Literals.readFloat32       string (optional float32),
  prim1 DefLiterals.readFloat64       Literals.readFloat64       string (optional float64),
  prim1 DefLiterals.readInt8          Literals.readInt8          string (optional int8),
  prim1 DefLiterals.readInt16         Literals.readInt16         string (optional int16),
  prim1 DefLiterals.readInt32         Literals.readInt32         string (optional int32),
  prim1 DefLiterals.readInt64         Literals.readInt64         string (optional int64),
  prim1 DefLiterals.parseString        Literals.parseString        string (optional string),
  prim1 DefLiterals.readUint8         Literals.readUint8         string (optional uint8),
  prim1 DefLiterals.readUint16        Literals.readUint16        string (optional uint16),
  prim1 DefLiterals.readUint32        Literals.readUint32        string (optional uint32),
  prim1 DefLiterals.readUint64        Literals.readUint64        string (optional uint64),
  prim1 DefLiterals.showBigint        Literals.showBigint        bigint string,
  prim1 DefLiterals.printBoolean       Literals.printBoolean       boolean string,
  prim1 DefLiterals.showDecimal       Literals.showDecimal       decimal string,
  prim1 DefLiterals.showFloat32       Literals.showFloat32       float32 string,
  prim1 DefLiterals.showFloat64       Literals.showFloat64       float64 string,
  prim1 DefLiterals.showInt8          Literals.showInt8          int8 string,
  prim1 DefLiterals.showInt16         Literals.showInt16         int16 string,
  prim1 DefLiterals.showInt32         Literals.showInt32         int32 string,
  prim1 DefLiterals.showInt64         Literals.showInt64         int64 string,
  prim1 DefLiterals.printString        Literals.printString        string string,
  prim1 DefLiterals.showUint8         Literals.showUint8         uint8 string,
  prim1 DefLiterals.showUint16        Literals.showUint16        uint16 string,
  prim1 DefLiterals.showUint32        Literals.showUint32        uint32 string,
  prim1 DefLiterals.showUint64        Literals.showUint64        uint64 string,
  prim1 DefLiterals.stringToBinary    Literals.stringToBinary    string binary,
  prim1 DefLiterals.uint8ToBigint     Literals.uint8ToBigint     uint8 bigint,
  prim1 DefLiterals.uint16ToBigint    Literals.uint16ToBigint    uint16 bigint,
  prim1 DefLiterals.uint32ToBigint    Literals.uint32ToBigint    uint32 bigint,
  prim1 DefLiterals.uint64ToBigint    Literals.uint64ToBigint    uint64 bigint]

hydraLibLogic :: Library
hydraLibLogic = standardLibrary [
    prim2 DefLogic.and    Logic.and    boolean boolean boolean,
    prim3 DefLogic.ifElse Logic.ifElse boolean x_ x_ x_,
    prim1 DefLogic.not    Logic.not    boolean boolean,
    prim2 DefLogic.or     Logic.or     boolean boolean boolean]

hydraLibMaps :: Library
hydraLibMaps = standardLibrary [
    prim3     DefMaps.alter           Maps.alter             (fun (optional v_) (optional v_)) k_ mapKv mapKv,
    prim3     DefMaps.bimap           Maps.bimap             (fun k1_ k2_) (fun v1_ v2_) (Prims.map k1_ v1_) (Prims.map k2_ v2_),
    prim2     DefMaps.delete          Maps.delete            k_ mapKv mapKv,
    prim2     DefMaps.difference      Maps.difference        mapKv mapKv mapKv,
    prim1     DefMaps.elems           Maps.elems             mapKv (list v_),
    prim0     DefMaps.empty           Maps.empty             mapKv,
    prim2     DefMaps.filter          Maps.filter            (fun v_ boolean) mapKv mapKv,
    prim2     DefMaps.filterWithKey   Maps.filterWithKey     (fun k_ (fun v_ boolean)) mapKv mapKv,
    prim3 DefMaps.findWithDefault Maps.findWithDefault   v_ k_ mapKv v_,
    prim1     DefMaps.fromList        Maps.fromList          (list $ pair k_ v_) mapKv,
    prim3     DefMaps.insert          Maps.insert            k_ v_ mapKv mapKv,
    prim2     DefMaps.intersection    Maps.intersection      mapKv mapKv mapKv,
    prim1     DefMaps.keys            Maps.keys              mapKv (list k_),
    prim2     DefMaps.lookup          Maps.lookup            k_ mapKv (optional v_),
    prim2     DefMaps.map             Maps.map               (funT v1_ v2_) (Prims.map k_ v1_) (Prims.map k_ v2_),
    prim2     DefMaps.mapKeys         Maps.mapKeys           (fun k1_ k2_) (Prims.map k1_ v_) (Prims.map k2_ v_),
    prim2     DefMaps.member          Maps.member            k_ mapKv boolean,
    prim1     DefMaps.null            Maps.null              mapKv boolean,
    prim2     DefMaps.singleton       Maps.singleton         k_ v_ mapKv,
    prim1     DefMaps.size            Maps.size              mapKv int32,
    prim1     DefMaps.toList          Maps.toList            mapKv (list $ pair k_ v_),
    prim2     DefMaps.union           Maps.union             mapKv mapKv mapKv,
    prim1     DefMaps.unions          Maps.unions            (list mapKv) mapKv]
  where
    mapKv = Prims.map k_ v_

hydraLibMathFloat64 :: Library
hydraLibMathFloat64 = standardLibrary [
  prim1 DefMath.acos     Math.acos     float64 float64,
  prim1 DefMath.acosh    Math.acosh    float64 float64,
  prim2 DefMath.addFloat64 Math.addFloat64 float64 float64 float64,
  prim1 DefMath.asin     Math.asin     float64 float64,
  prim1 DefMath.asinh    Math.asinh    float64 float64,
  prim1 DefMath.atan     Math.atan     float64 float64,
  prim2 DefMath.atan2    Math.atan2    float64 float64 float64,
  prim1 DefMath.atanh    Math.atanh    float64 float64,
  prim1 DefMath.ceiling  Math.ceiling  float64 float64,
  prim1 DefMath.cos      Math.cos      float64 float64,
  prim1 DefMath.cosh     Math.cosh     float64 float64,
  prim0 DefMath.e        Math.e        float64,
  prim1 DefMath.exp      Math.exp      float64 float64,
  prim1 DefMath.floor    Math.floor    float64 float64,
  prim1 DefMath.log      Math.log      float64 float64,
  prim2 DefMath.logBase  Math.logBase  float64 float64 float64,
  prim2 DefMath.mulFloat64 Math.mulFloat64 float64 float64 float64,
  prim1 DefMath.negateFloat64 Math.negateFloat64 float64 float64,
  prim0 DefMath.pi       Math.pi       float64,
  prim2 DefMath.pow      Math.pow      float64 float64 float64,
  prim1 DefMath.round         Math.round         float64 float64,
  prim2 DefMath.roundFloat32  Math.roundFloat32  int32 float32 float32,
  prim2 DefMath.roundFloat64  Math.roundFloat64  int32 float64 float64,
  prim1 DefMath.sin            Math.sin           float64 float64,
  prim1 DefMath.sinh     Math.sinh     float64 float64,
  prim1 DefMath.sqrt     Math.sqrt     float64 float64,
  prim2 DefMath.subFloat64 Math.subFloat64 float64 float64 float64,
  prim1 DefMath.tan      Math.tan      float64 float64,
  prim1 DefMath.tanh     Math.tanh     float64 float64,
  prim1 DefMath.truncate Math.truncate float64 float64]

hydraLibMathInt32 :: Library
hydraLibMathInt32 = standardLibrary [
  prim1 DefMath.abs    Math.abs    int32 int32,
  prim2 DefMath.add    Math.addTerm    x_ x_ x_,
  prim1 DefMath.even   Math.even   int32 boolean,
  prim2 DefMath.div Math.div int32 int32 (optional int32),
  prim2 DefMath.mod Math.mod int32 int32 (optional int32),
  prim2 DefMath.mul    Math.mulTerm    x_ x_ x_,
  prim1 DefMath.negate Math.negateTerm x_ x_,
  prim1 DefMath.odd    Math.odd    int32 boolean,
  prim2 DefMath.range  Math.range  int32 int32 (list int32),
  prim2 DefMath.rem Math.rem int32 int32 (optional int32),
  prim1 DefMath.signum Math.signum int32 int32,
  prim2 DefMath.sub    Math.subTerm    x_ x_ x_]

hydraLibOptionals :: Library
hydraLibOptionals = standardLibrary [
    prim2     DefOptionals.apply     Optionals.apply        (optional $ funT x_ y_) (optional x_) (optional y_),
    prim2     DefOptionals.bind      Optionals.bind         (optional x_) (fun x_ (optional y_)) (optional y_),
    prim3 DefOptionals.cases     Optionals.cases        (optional x_) y_ (funT x_ y_) y_,
    prim1     DefOptionals.givens       Optionals.givens          (list $ optional x_) (list x_),
    prim3     DefOptionals.compose   Optionals.compose      (fun x_ $ optional y_) (fun y_ $ optional z_) x_ (optional z_),
    prim3     DefOptionals.foldList  Optionals.foldList     (fun x_ (fun y_ (optional x_))) x_ (list y_) (optional x_),
    prim2 DefOptionals.withDefault Optionals.withDefault    x_ (optional x_) x_,
    prim1     DefOptionals.givens    Optionals.givens       (list $ optional x_) (list x_),
    prim1     DefOptionals.isGiven    Optionals.isGiven       (optional x_) boolean,
    prim1     DefOptionals.isNone Optionals.isNone    (optional x_) boolean,
    prim2     DefOptionals.map       Optionals.map          (funT x_ y_) (optional x_) (optional y_),
    prim2     DefOptionals.mapList   Optionals.mapList      (fun x_ (optional y_)) (list x_) (optional (list y_)),
    prim2     DefOptionals.mapOptional  Optionals.mapOptional     (fun x_ $ optional y_) (list x_) (list y_),
    prim2     DefOptionals.mapSet    Optionals.mapSet       (fun x_ (optional y_)) (set x_) (optional (set y_)),
    prim1     DefOptionals.pure      Optionals.pure         x_ (optional x_),
    prim1     DefOptionals.toList    Optionals.toList       (optional x_) (list x_),
    prim2 DefOptionals.withDefault Optionals.withDefault    x_ (optional x_) x_]

hydraLibPairs :: Library
hydraLibPairs = standardLibrary [
    prim3     DefPairs.bimap  Pairs.bimap      (funT a_ c_) (funT b_ d_) (pair a_ b_) (pair c_ d_),
    prim1     DefPairs.first  Pairs.first      (pair a_ b_) a_,
    prim1     DefPairs.second Pairs.second     (pair a_ b_) b_]

hydraLibRegex :: Library
hydraLibRegex = standardLibrary [
  prim2 DefRegex.find       Regex.find       string string (optional string),
  prim2 DefRegex.findAll    Regex.findAll    string string (list string),
  prim2 DefRegex.matches    Regex.matches    string string boolean,
  prim3 DefRegex.replace    Regex.replace    string string string string,
  prim3 DefRegex.replaceAll Regex.replaceAll string string string string,
  prim2 DefRegex.split      Regex.split      string string (list string)]

hydraLibSets :: Library
hydraLibSets = standardLibrary [
    prim2     DefSets.delete       Sets.delete       x_ (set x_) (set x_),
    prim2     DefSets.difference   Sets.difference   (set x_) (set x_) (set x_),
    prim0     DefSets.empty        Sets.empty        (set x_),
    prim2     DefSets.filter       Sets.filter       (fun x_ boolean) (set x_) (set x_),
    prim1     DefSets.fromList     Sets.fromList     (list x_) (set x_),
    prim2     DefSets.insert       Sets.insert       x_ (set x_) (set x_),
    prim2     DefSets.intersection Sets.intersection (set x_) (set x_) (set x_),
    prim2     DefSets.map          Sets.map          (fun x_ y_) (set x_) (set y_),
    prim2     DefSets.member       Sets.member       x_ (set x_) boolean,
    prim1     DefSets.null         Sets.null         (set x_) boolean,
    prim1     DefSets.singleton    Sets.singleton    x_ (set x_),
    prim1     DefSets.size         Sets.size         (set x_) int32,
    prim1     DefSets.toList       Sets.toList       (set x_) (list x_),
    prim2     DefSets.union        Sets.union        (set x_) (set x_) (set x_),
    prim1     DefSets.unions       Sets.unions       (list $ set x_) (set x_)]

hydraLibStrings :: Library
hydraLibStrings = standardLibrary [
  prim1 DefStrings.concat         Strings.concat         (list string) string,
  prim2 DefStrings.concat2        Strings.concat2        string string string,
  prim2 DefStrings.charAt      Strings.charAt      int32 string (optional int32),
  prim1 DefStrings.concat      Strings.concat      (list string) string,
  prim2 DefStrings.concat2     Strings.concat2     string string string,
  prim1 DefStrings.fromList    Strings.fromList    (list int32) string,
  prim2 DefStrings.join Strings.join string (list string) string,
  prim2 DefStrings.join        Strings.join        string (list string) string,
  prim1 DefStrings.length      Strings.length      string int32,
  prim1 DefStrings.lines       Strings.lines       string (list string),
  prim2 DefStrings.charAt Strings.charAt int32 string (optional int32),
  prim1 DefStrings.null        Strings.null        string boolean,
  prim2 DefStrings.splitOn     Strings.splitOn     string string (list string),
  prim1 DefStrings.toList      Strings.toList      string (list int32),
  prim1 DefStrings.toLower     Strings.toLower     string string,
  prim1 DefStrings.toUpper     Strings.toUpper     string string,
  prim1 DefStrings.unlines     Strings.unlines     (list string) string]

hydraLibSystem :: Library
hydraLibSystem = standardLibrary [
    unsupportedEffectPrimitive DefSystem.execute,
    unsupportedEffectPrimitive DefSystem.exit,
    unsupportedEffectPrimitive DefSystem.getEnvironment,
    unsupportedEffectPrimitive DefSystem.getEnvironmentVariable,
    unsupportedEffectPrimitive DefSystem.getTime,
    unsupportedEffectPrimitive DefSystem.getWorkingDirectory,
    unsupportedEffectPrimitive DefSystem.readStdin,
    unsupportedEffectPrimitive DefSystem.writeStderr,
    unsupportedEffectPrimitive DefSystem.writeStdout]

hydraLibHashing :: Library
hydraLibHashing = standardLibrary [
  prim1 DefHashing.sha256 Hashing.sha256 binary binary,
  prim1 DefHashing.sha256Hex Hashing.sha256Hex binary string]

hydraLibText :: Library
hydraLibText = standardLibrary [
  prim1 DefText.decodeUtf8 Text.decodeUtf8 binary (Prims.either_ string string),
  prim1 DefText.encodeUtf8 Text.encodeUtf8 string binary]

standardLibraries :: [Library]
standardLibraries = [
  hydraLibChars,
  hydraLibEffects,
  hydraLibEithers,
  hydraLibEquality,
  hydraLibFiles,
  hydraLibFunctions,
  hydraLibHashing,
  hydraLibLists,
  hydraLibLiterals,
  hydraLibLogic,
  hydraLibMaps,
  hydraLibMathFloat64,
  hydraLibMathInt32,
  hydraLibOptionals,
  hydraLibOrdering,
  hydraLibPairs,
  hydraLibRegex,
  hydraLibSets,
  hydraLibStrings,
  hydraLibSystem,
  hydraLibText]

-- | Assemble a library from its primitives. The library's module name (e.g. "hydra.lib.chars")
-- is *derived* from the primitives' shared namespace rather than passed as a literal string (#473):
-- every primitive in a library shares the namespace, so it is read off the first one's
-- PrimitiveDefinition name. Fails loudly on an empty list.
standardLibrary :: [Primitive] -> Library
standardLibrary [] = error "standardLibrary: empty primitive list (cannot derive module name)"
standardLibrary prims = Library {
  libraryName = ns,
  libraryPrefix = L.drop (L.length ("hydra.lib." :: String)) $ unModuleName ns,
  libraryPrimitives = prims}
  where
    -- namespace = the primitive name with its final ".<local>" segment dropped
    firstName = unName $ primitiveDefinitionName $ primitiveDefinition $ head prims
    ns = ModuleName $ reverse $ L.drop 1 $ L.dropWhile (/= '.') $ reverse firstName

-- | Register effect primitives for name resolution and inference, while keeping
-- Hydra's pure reducer from interpreting host effects as ordinary terms.
unsupportedEffectPrimitive :: PrimitiveDefinition -> Primitive
unsupportedEffectPrimitive def = Primitive def implementation
  where
    implementation _ _ = Left $ ErrorOther $ OtherError $
      "effect primitive cannot be reduced by Hydra's pure Haskell reducer: "
      ++ unName (primitiveDefinitionName def)
