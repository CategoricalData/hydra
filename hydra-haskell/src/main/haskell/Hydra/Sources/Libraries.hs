
-- | Implementations of the Hydra standard libraries in Haskell
module Hydra.Sources.Libraries (
  module Hydra.Sources.Libraries,
  module Hydra.Staging.Lib.Names,
) where

import Hydra.Kernel
import Hydra.Staging.Lib.Names
import Hydra.Dsl.Prims as Prims
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Hydra.Lib.Chars as Chars
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings

import qualified Hydra.Eval.Lib.Eithers as EvalEithers
import qualified Hydra.Eval.Lib.Flows as EvalFlows
import qualified Hydra.Eval.Lib.Lists as EvalLists
import qualified Hydra.Eval.Lib.Maps as EvalMaps
import qualified Hydra.Eval.Lib.Maybes as EvalMaybes
import qualified Hydra.Eval.Lib.Pairs as EvalPairs
import qualified Hydra.Eval.Lib.Sets as EvalSets

import qualified Data.List as L


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

standardLibraries :: [Library]
standardLibraries = [
  hydraLibChars,
  hydraLibEithers,
  hydraLibEquality,
  hydraLibFlows,
  hydraLibLists,
  hydraLibLiterals,
  hydraLibLogic,
  hydraLibMaps,
  hydraLibMathFloat64,
  hydraLibMathInt32,
  hydraLibMaybes,
  hydraLibPairs,
  hydraLibSets,
  hydraLibStrings]

standardLibrary :: Namespace -> [Primitive] -> Library
standardLibrary ns prims = Library {
  libraryNamespace = ns,
  libraryPrefix = L.drop (L.length ("hydra.lib." :: String)) $ unNamespace ns,
  libraryPrimitives = prims}

hydraLibChars :: Library
hydraLibChars = standardLibrary _hydra_lib_chars [
  prim1 _chars_isAlphaNum Chars.isAlphaNum [] int32 boolean,
  prim1 _chars_isLower    Chars.isLower    [] int32 boolean,
  prim1 _chars_isSpace    Chars.isSpace    [] int32 boolean,
  prim1 _chars_isUpper    Chars.isUpper    [] int32 boolean,
  prim1 _chars_toLower    Chars.toLower    [] int32 int32,
  prim1 _chars_toUpper    Chars.toUpper    [] int32 int32]

hydraLibEithers :: Library
hydraLibEithers = standardLibrary _hydra_lib_eithers [
    prim3Eval   _eithers_bimap            EvalEithers.bimap        [_x, _y, _z, _w] (function x_ z_) (function y_ w_) (Prims.either_ x_ y_) (Prims.either_ z_ w_),
    prim3Eval   _eithers_either           EvalEithers.either       [_x, _y, _z]     (function x_ z_) (function y_ z_) (Prims.either_ x_ y_) z_,
    prim2       _eithers_fromLeft         Eithers.fromLeft         [_x, _y]         x_ (Prims.either_ x_ y_) x_,
    prim2       _eithers_fromRight        Eithers.fromRight        [_x, _y]         y_ (Prims.either_ x_ y_) y_,
    prim1       _eithers_isLeft           Eithers.isLeft           [_x, _y]         (Prims.either_ x_ y_) boolean,
    prim1       _eithers_isRight          Eithers.isRight          [_x, _y]         (Prims.either_ x_ y_) boolean,
    prim1       _eithers_lefts            Eithers.lefts            [_x, _y]         (list $ Prims.either_ x_ y_) (list x_),
    prim2Eval   _eithers_map              EvalEithers.map          [_x, _y, _z]     (function x_ y_) (Prims.either_ z_ x_) (Prims.either_ z_ y_),
    prim2Eval   _eithers_mapList          EvalEithers.mapList      [_x, _y, _z]     (function x_ (Prims.either_ z_ y_)) (list x_) (Prims.either_ z_ (list y_)),
    prim2Eval   _eithers_mapMaybe         EvalEithers.mapMaybe     [_x, _y, _z]     (function x_ (Prims.either_ z_ y_)) (optional x_) (Prims.either_ z_ (optional y_)),
    -- TODO: add mapMap, mapPair, mapSet when their implementations are ready
    prim1       _eithers_partitionEithers Eithers.partitionEithers [_x, _y]         (list $ Prims.either_ x_ y_) (pair (list x_) (list y_)),
    prim1       _eithers_rights           Eithers.rights           [_x, _y]         (list $ Prims.either_ x_ y_) (list y_)]

hydraLibEquality :: Library
hydraLibEquality = standardLibrary _hydra_lib_equality [
    prim2 _equality_compare  Equality.compare  [_xOrd] x_ x_ comparison,
    prim2 _equality_equal    Equality.equal    [_xEq]  x_ x_ boolean,
    prim1 _equality_identity Equality.identity [_x]    x_ x_,
    prim2 _equality_gt       Equality.gt       [_xOrd] x_ x_ boolean,
    prim2 _equality_gte      Equality.gte      [_xOrd] x_ x_ boolean,
    prim2 _equality_lt       Equality.lt       [_xOrd] x_ x_ boolean,
    prim2 _equality_lte      Equality.lte      [_xOrd] x_ x_ boolean,
    prim2 _equality_max      Equality.max      [_xOrd] x_ x_ x_,
    prim2 _equality_min      Equality.min      [_xOrd] x_ x_ x_]

hydraLibFlows :: Library
hydraLibFlows = standardLibrary _hydra_lib_flows [
    prim2Eval _flows_apply    EvalFlows.apply    [_s, _x, _y]                 (flow s_ (function x_ y_)) (flow s_ x_) (flow s_ y_),
    prim2Eval _flows_bind     EvalFlows.bind     [_s, _x, _y]                 (flow s_ x_) (function x_ (flow s_ y_)) (flow s_ y_),
    prim1     _flows_fail     Flows.fail         [_s, _x]                     string (flow s_ x_),
    prim3Eval _flows_foldl    EvalFlows.foldl    [_y, _x, _s]                 (function y_ (function x_ (flow s_ y_))) y_ (list x_) (flow s_ y_),
    prim2Eval _flows_map      EvalFlows.map      [_x, _y, _s]                 (function x_ y_) (flow s_ x_) (flow s_ y_),
    prim2Eval _flows_mapElems EvalFlows.mapElems [_v1, _s, _v2, _kOrd]        (function v1_ (flow s_ v2_)) (Prims.map k_ v1_) (flow s_ (Prims.map k_ v2_)),
    prim2Eval _flows_mapKeys  EvalFlows.mapKeys  [_k1Ord, _s, _k2Ord, _v]     (function k1_ (flow s_ k2_)) (Prims.map k1_ v_) (flow s_ (Prims.map k2_ v_)),
    prim2Eval _flows_mapList  EvalFlows.mapList  [_x, _s, _y]                 (function x_ (flow s_ y_)) (list x_) (flow s_ (list y_)),
    prim2Eval _flows_mapMaybe EvalFlows.mapMaybe [_x, _s, _y]                 (function x_ $ flow s_ y_) (optional x_) (flow s_ $ optional y_),
    prim2Eval _flows_mapSet   EvalFlows.mapSet   [_xOrd, _s, _yOrd]           (function x_ (flow s_ y_)) (set x_) (flow s_ (set y_)),
    prim1     _flows_pure     Flows.pure         [_x, _s]                     x_ (flow s_ x_),
    prim1     _flows_sequence Flows.sequence     [_s, _x]                     (list (flow s_ x_)) (flow s_ (list x_))]

hydraLibLists :: Library
hydraLibLists = standardLibrary _hydra_lib_lists [
    prim2Eval _lists_apply       EvalLists.apply     [_x, _y]     (list $ function x_ y_) (list x_) (list y_),
    prim2     _lists_at          Lists.at            [_x]         int32 (list x_) x_,
    prim2Eval _lists_bind        EvalLists.bind      [_x, _y]     (list x_) (function x_ (list y_)) (list y_),
    prim1     _lists_concat      Lists.concat        [_x]         (list (list x_)) (list x_),
    prim2     _lists_concat2     Lists.concat2       [_x]         (list x_) (list x_) (list x_),
    prim2     _lists_cons        Lists.cons          [_x]         x_ (list x_) (list x_),
    prim2     _lists_drop        Lists.drop          [_x]         int32 (list x_) (list x_),
    prim2Eval _lists_dropWhile   EvalLists.dropWhile [_x]         (function x_ boolean) (list x_) (list x_),
    prim2     _lists_elem        Lists.elem          [_xEq]       x_ (list x_) boolean,
    prim2Eval _lists_filter      EvalLists.filter    [_x]         (function x_ boolean) (list x_) (list x_),
    prim3Eval _lists_foldl       EvalLists.foldl     [_y, _x]     (function y_ (function x_ y_)) y_ (list x_) y_,
    prim1     _lists_group       Lists.group         [_xEq]       (list x_) (list (list x_)),
    prim1     _lists_head        Lists.head          [_x]         (list x_) x_,
    prim1     _lists_init        Lists.init          [_x]         (list x_) (list x_),
    prim2     _lists_intercalate Lists.intercalate   [_x]         (list x_) (list (list x_)) (list x_),
    prim2     _lists_intersperse Lists.intersperse   [_x]         x_ (list x_) (list x_),
    prim1     _lists_last        Lists.last          [_x]         (list x_) x_,
    prim1     _lists_length      Lists.length        [_x]         (list x_) int32,
    prim2Eval _lists_map         EvalLists.map       [_x, _y]     (function x_ y_) (list x_) (list y_),
    prim1     _lists_nub         Lists.nub           [_xEq]       (list x_) (list x_),
    prim1     _lists_null        Lists.null          [_x]         (list x_) boolean,
    prim1     _lists_pure        Lists.pure          [_x]         x_ (list x_),
    prim2     _lists_replicate   Lists.replicate     [_x]         int32 x_ (list x_),
    prim1     _lists_reverse     Lists.reverse       [_x]         (list x_) (list x_),
    prim1     _lists_safeHead    Lists.safeHead      [_x]         (list x_) (optional x_),
    prim1     _lists_singleton   Lists.singleton     [_x]         x_ (list x_),
    prim2Eval _lists_sortOn      EvalLists.sortOn    [_x, _yOrd]  (function x_ y_) (list x_) (list x_),
    prim2Eval _lists_span        EvalLists.span      [_x]         (function x_ boolean) (list x_) (pair (list x_) (list x_)),
    prim1     _lists_sort        Lists.sort          [_xOrd]      (list x_) (list x_),
    prim1     _lists_tail        Lists.tail          [_x]         (list x_) (list x_),
    prim2     _lists_take        Lists.take          [_x]         int32 (list x_) (list x_),
    prim1     _lists_transpose   Lists.transpose     [_x]         (list (list x_)) (list (list x_)),
    prim2     _lists_zip         Lists.zip           [_x, _y]     (list x_) (list y_) (list (pair x_ y_)),
    prim3Eval _lists_zipWith     EvalLists.zipWith   [_x, _y, _z] (function x_ $ function y_ z_) (list x_) (list y_) (list z_)]

hydraLibLiterals :: Library
hydraLibLiterals = standardLibrary _hydra_lib_literals [
  prim1 _literals_bigfloatToBigint  Literals.bigfloatToBigint  [] bigfloat bigint,
  prim1 _literals_bigfloatToFloat32 Literals.bigfloatToFloat32 [] bigfloat float32,
  prim1 _literals_bigfloatToFloat64 Literals.bigfloatToFloat64 [] bigfloat float64,
  prim1 _literals_bigintToBigfloat  Literals.bigintToBigfloat  [] bigint bigfloat,
  prim1 _literals_bigintToInt8      Literals.bigintToInt8      [] bigint int8,
  prim1 _literals_bigintToInt16     Literals.bigintToInt16     [] bigint int16,
  prim1 _literals_bigintToInt32     Literals.bigintToInt32     [] bigint int32,
  prim1 _literals_bigintToInt64     Literals.bigintToInt64     [] bigint int64,
  prim1 _literals_bigintToUint8     Literals.bigintToUint8     [] bigint uint8,
  prim1 _literals_bigintToUint16    Literals.bigintToUint16    [] bigint uint16,
  prim1 _literals_bigintToUint32    Literals.bigintToUint32    [] bigint uint32,
  prim1 _literals_bigintToUint64    Literals.bigintToUint64    [] bigint uint64,
  prim1 _literals_binaryToString    Literals.binaryToString    [] binary string,
  prim1 _literals_float32ToBigfloat Literals.float32ToBigfloat [] float32 bigfloat,
  prim1 _literals_float64ToBigfloat Literals.float64ToBigfloat [] float64 bigfloat,
  prim1 _literals_int8ToBigint      Literals.int8ToBigint      [] int8 bigint,
  prim1 _literals_int16ToBigint     Literals.int16ToBigint     [] int16 bigint,
  prim1 _literals_int32ToBigint     Literals.int32ToBigint     [] int32 bigint,
  prim1 _literals_int64ToBigint     Literals.int64ToBigint     [] int64 bigint,
  prim1 _literals_readBigfloat      Literals.readBigfloat      [] string (optional bigfloat),
  prim1 _literals_readBoolean       Literals.readBoolean       [] string (optional boolean),
  prim1 _literals_readFloat32       Literals.readFloat32       [] string (optional float32),
  prim1 _literals_readFloat64       Literals.readFloat64       [] string (optional float64),
  prim1 _literals_readInt32         Literals.readInt32         [] string (optional int32),
  prim1 _literals_readInt64         Literals.readInt64         [] string (optional int64),
  prim1 _literals_readString        Literals.readString        [] string (optional string),
  prim1 _literals_showBigfloat      Literals.showBigfloat      [] bigfloat string,
  prim1 _literals_showBigint        Literals.showBigint        [] bigint string,
  prim1 _literals_showBoolean       Literals.showBoolean       [] boolean string,
  prim1 _literals_showFloat32       Literals.showFloat32       [] float32 string,
  prim1 _literals_showFloat64       Literals.showFloat64       [] float64 string,
  prim1 _literals_showInt8          Literals.showInt8          [] int8 string,
  prim1 _literals_showInt16         Literals.showInt16         [] int16 string,
  prim1 _literals_showInt32         Literals.showInt32         [] int32 string,
  prim1 _literals_showInt64         Literals.showInt64         [] int64 string,
  prim1 _literals_showUint8         Literals.showUint8         [] uint8 string,
  prim1 _literals_showUint16        Literals.showUint16        [] uint16 string,
  prim1 _literals_showUint32        Literals.showUint32        [] uint32 string,
  prim1 _literals_showUint64        Literals.showUint64        [] uint64 string,
  prim1 _literals_showString        Literals.showString        [] string string,
  prim1 _literals_stringToBinary    Literals.stringToBinary    [] string binary,
  prim1 _literals_uint8ToBigint     Literals.uint8ToBigint     [] uint8 bigint,
  prim1 _literals_uint16ToBigint    Literals.uint16ToBigint    [] uint16 bigint,
  prim1 _literals_uint32ToBigint    Literals.uint32ToBigint    [] uint32 bigint,
  prim1 _literals_uint64ToBigint    Literals.uint64ToBigint    [] uint64 bigint]

hydraLibLogic :: Library
hydraLibLogic = standardLibrary _hydra_lib_logic [
    prim2 _logic_and    Logic.and    []   boolean boolean boolean,
    prim3 _logic_ifElse Logic.ifElse [_x] boolean x_ x_ x_,
    prim1 _logic_not    Logic.not    []   boolean boolean,
    prim2 _logic_or     Logic.or     []   boolean boolean boolean]

hydraLibMaps :: Library
hydraLibMaps = standardLibrary _hydra_lib_maps [
    prim3Eval _maps_alter           EvalMaps.alter         [_v, _kOrd]                  (function (optional v_) (optional v_)) k_ mapKv mapKv,
    prim3Eval _maps_bimap           EvalMaps.bimap         [_k1Ord, _k2Ord, _v1, _v2]   (function k1_ k2_) (function v1_ v2_) (Prims.map k1_ v1_) (Prims.map k2_ v2_),
    prim1     _maps_elems           Maps.elems             [_kOrd, _v]                  mapKv (list v_),
    prim2     _maps_delete          Maps.delete            [_kOrd, _v]                  k_ mapKv mapKv,
    prim0     _maps_empty           Maps.empty             [_kOrd, _v]                  mapKv,
    prim2Eval _maps_filter          EvalMaps.filter        [_v, _kOrd]                  (function v_ boolean) mapKv mapKv,
    prim2Eval _maps_filterWithKey   EvalMaps.filterWithKey [_kOrd, _v]                  (function k_ (function v_ boolean)) mapKv mapKv,
    prim3     _maps_findWithDefault Maps.findWithDefault   [_v, _kOrd]                  v_ k_ mapKv v_,
    prim1     _maps_fromList        Maps.fromList          [_kOrd, _v]                  (list $ pair k_ v_) mapKv,
    prim3     _maps_insert          Maps.insert            [_kOrd, _v]                  k_ v_ mapKv mapKv,
    prim1     _maps_keys            Maps.keys              [_kOrd, _v]                  mapKv (list k_),
    prim2     _maps_lookup          Maps.lookup            [_kOrd, _v]                  k_ mapKv (optional v_),
    prim2Eval _maps_map             EvalMaps.map           [_v1, _v2, _kOrd]            (function v1_ v2_) (Prims.map k_ v1_) (Prims.map k_ v2_),
    prim2Eval _maps_mapKeys         EvalMaps.mapKeys       [_k1Ord, _k2Ord, _v]         (function k1_ k2_) (Prims.map k1_ v_) (Prims.map k2_ v_),
    prim2     _maps_member          Maps.member            [_kOrd, _v]                  k_ mapKv boolean,
    prim1     _maps_null            Maps.null              [_kOrd, _v]                  mapKv boolean,
    prim1     _maps_size            Maps.size              [_kOrd, _v]                  mapKv int32,
    prim2     _maps_singleton       Maps.singleton         [_kOrd, _v]                  k_ v_ mapKv,
    prim1     _maps_toList          Maps.toList            [_kOrd, _v]                  mapKv (list $ pair k_ v_),
    prim2     _maps_union           Maps.union             [_kOrd, _v]                  mapKv mapKv mapKv]
  where
    mapKv = Prims.map k_ v_

hydraLibMathFloat64 :: Library
hydraLibMathFloat64 = standardLibrary _hydra_lib_math [
  prim1 _math_acos     Math.acos     [] float64 float64,
  prim1 _math_acosh    Math.acosh    [] float64 float64,
  prim1 _math_asin     Math.asin     [] float64 float64,
  prim1 _math_asinh    Math.asinh    [] float64 float64,
  prim1 _math_atan     Math.atan     [] float64 float64,
  prim2 _math_atan2    Math.atan2    [] float64 float64 float64,
  prim1 _math_atanh    Math.atanh    [] float64 float64,
  prim1 _math_ceiling  Math.ceiling  [] float64 bigint,
  prim1 _math_cos      Math.cos      [] float64 float64,
  prim1 _math_cosh     Math.cosh     [] float64 float64,
  prim0 _math_e        Math.e        [] float64,
  prim1 _math_exp      Math.exp      [] float64 float64,
  prim1 _math_floor    Math.floor    [] float64 bigint,
  prim1 _math_log      Math.log      [] float64 float64,
  prim2 _math_logBase  Math.logBase  [] float64 float64 float64,
  prim0 _math_pi       Math.pi       [] float64,
  prim2 _math_pow      Math.pow      [] float64 float64 float64,
  prim1 _math_round    Math.round    [] float64 bigint,
  prim1 _math_sin      Math.sin      [] float64 float64,
  prim1 _math_sinh     Math.sinh     [] float64 float64,
  prim1 _math_sqrt     Math.sqrt     [] float64 float64,
  prim1 _math_tan      Math.tan      [] float64 float64,
  prim1 _math_tanh     Math.tanh     [] float64 float64,
  prim1 _math_truncate Math.truncate [] float64 bigint]

hydraLibMathInt32 :: Library
hydraLibMathInt32 = standardLibrary _hydra_lib_math [
  prim1 _math_abs    Math.abs    [] int32 int32,
  prim2 _math_add    Math.add    [] int32 int32 int32,
  prim2 _math_div    Math.div    [] int32 int32 int32,
  prim1 _math_even   Math.even   [] int32 boolean,
  prim2 _math_mod    Math.mod    [] int32 int32 int32,
  prim2 _math_mul    Math.mul    [] int32 int32 int32,
  prim1 _math_negate Math.negate [] int32 int32,
  prim1 _math_odd    Math.odd    [] int32 boolean,
  prim1 _math_pred   Math.pred   [] int32 int32,
  prim2 _math_range  Math.range  [] int32 int32 (list int32),
  prim2 _math_rem    Math.rem    [] int32 int32 int32,
  prim1 _math_signum Math.signum [] int32 int32,
  prim2 _math_sub    Math.sub    [] int32 int32 int32,
  prim1 _math_succ   Math.succ   [] int32 int32]

hydraLibMaybes :: Library
hydraLibMaybes = standardLibrary _hydra_lib_maybes [
    prim2Eval _maybes_apply     EvalMaybes.apply    [_x, _y]     (optional $ function x_ y_) (optional x_) (optional y_),
    prim2Eval _maybes_bind      EvalMaybes.bind     [_x, _y]     (optional x_) (function x_ (optional y_)) (optional y_),
    prim3Eval _maybes_cases     EvalMaybes.cases    [_x, _y]     (optional x_) y_ (function x_ y_) y_,
    prim1     _maybes_cat       Maybes.cat          [_x]         (list $ optional x_) (list x_),
    prim3Eval _maybes_compose   EvalMaybes.compose  [_x, _y, _z] (function x_ $ optional y_) (function y_ $ optional z_) x_ (optional z_),
    prim1     _maybes_fromJust  Maybes.fromJust     [_x]         (optional x_) x_,
    prim2     _maybes_fromMaybe Maybes.fromMaybe    [_x]         x_ (optional x_) x_,
    prim1     _maybes_isJust    Maybes.isJust       [_x]         (optional x_) boolean,
    prim1     _maybes_isNothing Maybes.isNothing    [_x]         (optional x_) boolean,
    prim2Eval _maybes_map       EvalMaybes.map      [_x, _y]     (function x_ y_) (optional x_) (optional y_),
    prim2Eval _maybes_mapMaybe  EvalMaybes.mapMaybe [_x, _y]     (function x_ $ optional y_) (list x_) (list y_),
    prim3Eval _maybes_maybe     EvalMaybes.maybe    [_y, _x]     y_ (function x_ y_) (optional x_) y_,
    prim1     _maybes_pure      Maybes.pure         [_x]         x_ (optional x_)]

hydraLibPairs :: Library
hydraLibPairs = standardLibrary _hydra_lib_pairs [
    prim3Eval _pairs_bimap  EvalPairs.bimap  [_a, _b, _c, _d] (function a_ c_) (function b_ d_) (pair a_ b_) (pair c_ d_),
    prim1     _pairs_first  Pairs.first      [_a, _b]         (pair a_ b_) a_,
    prim1     _pairs_second Pairs.second     [_a, _b]         (pair a_ b_) b_]

hydraLibSets :: Library
hydraLibSets = standardLibrary _hydra_lib_sets [
    prim2     _sets_delete       Sets.delete       [_xOrd]        x_ (set x_) (set x_),
    prim2     _sets_difference   Sets.difference   [_xOrd]        (set x_) (set x_) (set x_),
    prim0     _sets_empty        Sets.empty        [_xOrd]        (set x_),
    prim1     _sets_fromList     Sets.fromList     [_xOrd]        (list x_) (set x_),
    prim2     _sets_insert       Sets.insert       [_xOrd]        x_ (set x_) (set x_),
    prim2     _sets_intersection Sets.intersection [_xOrd]        (set x_) (set x_) (set x_),
    prim2Eval _sets_map          EvalSets.map      [_xOrd, _yOrd] (function x_ y_) (set x_) (set y_),
    prim2     _sets_member       Sets.member       [_xOrd]        x_ (set x_) boolean,
    prim1     _sets_null         Sets.null         [_xOrd]        (set x_) boolean,
    prim1     _sets_singleton    Sets.singleton    [_xOrd]        x_ (set x_),
    prim1     _sets_size         Sets.size         [_xOrd]        (set x_) int32,
    prim1     _sets_toList       Sets.toList       [_xOrd]        (set x_) (list x_),
    prim2     _sets_union        Sets.union        [_xOrd]        (set x_) (set x_) (set x_),
    prim1     _sets_unions       Sets.unions       [_xOrd]        (list $ set x_) (set x_)]

hydraLibStrings :: Library
hydraLibStrings = standardLibrary _hydra_lib_strings [
  prim1 _strings_cat         Strings.cat         [] (list string) string,
  prim2 _strings_cat2        Strings.cat2        [] string string string,
  prim2 _strings_charAt      Strings.charAt      [] int32 string int32,
  prim1 _strings_fromList    Strings.fromList    [] (list int32) string,
  prim2 _strings_intercalate Strings.intercalate [] string (list string) string,
  prim1 _strings_length      Strings.length      [] string int32,
  prim1 _strings_lines       Strings.lines       [] string (list string),
  prim1 _strings_null        Strings.null        [] string boolean,
  prim2 _strings_splitOn     Strings.splitOn     [] string string (list string),
  prim1 _strings_toList      Strings.toList      [] string (list int32),
  prim1 _strings_toLower     Strings.toLower     [] string string,
  prim1 _strings_toUpper     Strings.toUpper     [] string string,
  prim1 _strings_unlines     Strings.unlines     [] (list string) string]
