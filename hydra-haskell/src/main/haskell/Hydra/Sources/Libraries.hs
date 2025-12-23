
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
    prim3Eval   _eithers_bimap            EvalEithers.bimap        ["x", "y", "z", "w"] (function x z) (function y w) (Prims.either_ x y) (Prims.either_ z w),
    prim3Eval   _eithers_either           EvalEithers.either       ["x", "y", "z"]      (function x z) (function y z) (Prims.either_ x y) z,
    prim2       _eithers_fromLeft         Eithers.fromLeft         ["x", "y"]           x (Prims.either_ x y) x,
    prim2       _eithers_fromRight        Eithers.fromRight        ["x", "y"]           y (Prims.either_ x y) y,
    prim1       _eithers_isLeft           Eithers.isLeft           ["x", "y"]           (Prims.either_ x y) boolean,
    prim1       _eithers_isRight          Eithers.isRight          ["x", "y"]           (Prims.either_ x y) boolean,
    prim1       _eithers_lefts            Eithers.lefts            ["x", "y"]           (list $ Prims.either_ x y) (list x),
    prim2Eval   _eithers_map              EvalEithers.map          ["x", "y", "z"]      (function x y) (Prims.either_ z x) (Prims.either_ z y),
    prim2Eval   _eithers_mapList          EvalEithers.mapList      ["x", "y", "z"]      (function x (Prims.either_ z y)) (list x) (Prims.either_ z (list y)),
    prim2Eval   _eithers_mapMaybe         EvalEithers.mapMaybe     ["x", "y", "z"]      (function x (Prims.either_ z y)) (optional x) (Prims.either_ z (optional y)),
    -- TODO: add mapMap, mapPair, mapSet when their implementations are ready
    prim1       _eithers_partitionEithers Eithers.partitionEithers ["x", "y"]           (list $ Prims.either_ x y) (pair (list x) (list y)),
    prim1       _eithers_rights           Eithers.rights           ["x", "y"]           (list $ Prims.either_ x y) (list y)]
  where
    x = variable "x"
    y = variable "y"
    z = variable "z"
    w = variable "w"

hydraLibEquality :: Library
hydraLibEquality = standardLibrary _hydra_lib_equality [
    prim2 _equality_compare  Equality.compare  ["x"] x x comparison,
    prim2 _equality_equal    Equality.equal    ["x"] x x boolean,
    prim1 _equality_identity Equality.identity ["x"] x x,
    prim2 _equality_gt       Equality.gt       ["x"] x x boolean,
    prim2 _equality_gte      Equality.gte      ["x"] x x boolean,
    prim2 _equality_lt       Equality.lt       ["x"] x x boolean,
    prim2 _equality_lte      Equality.lte      ["x"] x x boolean,
    prim2 _equality_max      Equality.max      ["x"] x x x,
    prim2 _equality_min      Equality.min      ["x"] x x x]
  where
    x = variable "x"

hydraLibFlows :: Library
hydraLibFlows = standardLibrary _hydra_lib_flows [
    prim2Eval _flows_apply    EvalFlows.apply    ["s", "x", "y"]        (flow s (function x y)) (flow s x) (flow s y),
    prim2Eval _flows_bind     EvalFlows.bind     ["s", "x", "y"]        (flow s x) (function x (flow s y)) (flow s y),
    prim1     _flows_fail     Flows.fail         ["s", "x"]             string (flow s x),
    prim3Eval _flows_foldl    EvalFlows.foldl    ["y", "x", "s"]        (function y (function x (flow s y))) y (list x) (flow s y),
    prim2Eval _flows_map      EvalFlows.map      ["x", "y", "s"]        (function x y) (flow s x) (flow s y),
    prim2Eval _flows_mapElems EvalFlows.mapElems ["v1", "s", "v2", "k"] (function v1 (flow s v2)) (Prims.map k v1) (flow s (Prims.map k v2)),
    prim2Eval _flows_mapKeys  EvalFlows.mapKeys  ["k1", "s", "k2", "v"] (function k1 (flow s k2)) (Prims.map k1 v) (flow s (Prims.map k2 v)),
    prim2Eval _flows_mapList  EvalFlows.mapList  ["x", "s", "y"]        (function x (flow s y)) (list x) (flow s (list y)),
    prim2Eval _flows_mapMaybe EvalFlows.mapMaybe ["x", "s", "y"]        (function x $ flow s y) (optional x) (flow s $ optional y),
    prim2Eval _flows_mapSet   EvalFlows.mapSet   ["x", "s", "y"]        (function x (flow s y)) (set x) (flow s (set y)),
    prim1     _flows_pure     Flows.pure         ["x", "s"]             x (flow s x),
    prim1     _flows_sequence Flows.sequence     ["s", "x"]             (list (flow s x)) (flow s (list x))]
  where
    s = variable "s"
    k = variable "k"
    k1 = variable "k1"
    k2 = variable "k2"
    x = variable "x"
    v = variable "v"
    v1 = variable "v1"
    v2 = variable "v2"
    y = variable "y"

hydraLibLists :: Library
hydraLibLists = standardLibrary _hydra_lib_lists [
    prim2Eval _lists_apply       EvalLists.apply     ["x", "y"] (list $ function x y) (list x) (list y),
    prim2     _lists_at          Lists.at            ["x"] int32 (list x) x,
    prim2Eval _lists_bind        EvalLists.bind      ["x", "y"] (list x) (function x (list y)) (list y),
    prim1     _lists_concat      Lists.concat        ["x"] (list (list x)) (list x),
    prim2     _lists_concat2     Lists.concat2       ["x"] (list x) (list x) (list x),
    prim2     _lists_cons        Lists.cons          ["x"] x (list x) (list x),
    prim2     _lists_drop        Lists.drop          ["x"] int32 (list x) (list x),
    prim2Eval _lists_dropWhile   EvalLists.dropWhile ["x"] (function x boolean) (list x) (list x),
    prim2     _lists_elem        Lists.elem          ["x"] x (list x) boolean,
    prim2Eval _lists_filter      EvalLists.filter    ["x"] (function x boolean) (list x) (list x),
    prim3Eval _lists_foldl       EvalLists.foldl     ["y", "x"] (function y (function x y)) y (list x) y,
    prim1     _lists_group       Lists.group         ["x"] (list x) (list (list x)),
    prim1     _lists_head        Lists.head          ["x"] (list x) x,
    prim1     _lists_init        Lists.init          ["x"] (list x) (list x),
    prim2     _lists_intercalate Lists.intercalate   ["x"] (list x) (list (list x)) (list x),
    prim2     _lists_intersperse Lists.intersperse   ["x"] x (list x) (list x),
    prim1     _lists_last        Lists.last          ["x"] (list x) x,
    prim1     _lists_length      Lists.length        ["x"] (list x) int32,
    prim2Eval _lists_map         EvalLists.map       ["x", "y"] (function x y) (list x) (list y),
    prim1     _lists_nub         Lists.nub           ["x"] (list x) (list x),
    prim1     _lists_null        Lists.null          ["x"] (list x) boolean,
    prim1     _lists_pure        Lists.pure          ["x"] x (list x),
    prim2     _lists_replicate   Lists.replicate     ["x"] int32 x (list x),
    prim1     _lists_reverse     Lists.reverse       ["x"] (list x) (list x),
    prim1     _lists_safeHead    Lists.safeHead      ["x"] (list x) (optional x),
    prim1     _lists_singleton   Lists.singleton     ["x"] x (list x),
    prim2Eval _lists_sortOn      EvalLists.sortOn    ["x", "y"] (function x y) (list x) (list x),
    prim2Eval _lists_span        EvalLists.span      ["x"] (function x boolean) (list x) (pair (list x) (list x)),
    prim1     _lists_sort        Lists.sort          ["x"] (list x) (list x),
    prim1     _lists_tail        Lists.tail          ["x"] (list x) (list x),
    prim2     _lists_take        Lists.take          ["x"] int32 (list x) (list x),
    prim1     _lists_transpose   Lists.transpose     ["x"] (list (list x)) (list (list x)),
    prim2     _lists_zip         Lists.zip           ["x", "y"] (list x) (list y) (list (pair x y)),
    prim3Eval _lists_zipWith     EvalLists.zipWith   ["x", "y", "z"] (function x $ function y z) (list x) (list y) (list z)]
  where
    x = variable "x"
    y = variable "y"
    z = variable "z"

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
    prim2 _logic_and    Logic.and    []    boolean boolean boolean,
    prim3 _logic_ifElse Logic.ifElse ["x"] boolean x x x,
    prim1 _logic_not    Logic.not    []    boolean boolean,
    prim2 _logic_or     Logic.or     []    boolean boolean boolean]
  where
    x = variable "x"

hydraLibMaps :: Library
hydraLibMaps = standardLibrary _hydra_lib_maps [
    prim3Eval _maps_alter           EvalMaps.alter         ["v", "k"]               (function (optional v) (optional v)) k mapKv mapKv,
    prim3Eval _maps_bimap           EvalMaps.bimap         ["k1", "k2", "v1", "v2"] (function k1 k2) (function v1 v2) (Prims.map k1 v1) (Prims.map k2 v2),
    prim1     _maps_elems           Maps.elems             ["k", "v"]               mapKv (list v),
    prim2     _maps_delete          Maps.delete            ["k", "v"]               k mapKv mapKv,
    prim0     _maps_empty           Maps.empty             ["k", "v"]               mapKv,
    prim2Eval _maps_filter          EvalMaps.filter        ["v", "k"]               (function v boolean) mapKv mapKv,
    prim2Eval _maps_filterWithKey   EvalMaps.filterWithKey ["k", "v"]               (function k (function v boolean)) mapKv mapKv,
    prim3     _maps_findWithDefault Maps.findWithDefault   ["v", "k"]               v k mapKv v,
    prim1     _maps_fromList        Maps.fromList          ["k", "v"]               (list $ pair k v) mapKv,
    prim3     _maps_insert          Maps.insert            ["k", "v"]               k v mapKv mapKv,
    prim1     _maps_keys            Maps.keys              ["k", "v"]               mapKv (list k),
    prim2     _maps_lookup          Maps.lookup            ["k", "v"]               k mapKv (optional v),
    prim2Eval _maps_map             EvalMaps.map           ["v1", "v2", "k"]        (function v1 v2) (Prims.map k v1) (Prims.map k v2),
    prim2Eval _maps_mapKeys         EvalMaps.mapKeys       ["k1", "k2", "v"]        (function k1 k2) (Prims.map k1 v) (Prims.map k2 v),
    prim2     _maps_member          Maps.member            ["k", "v"]               k mapKv boolean,
    prim1     _maps_null            Maps.null              ["k", "v"]               mapKv boolean,
    prim1     _maps_size            Maps.size              ["k", "v"]               mapKv int32,
    prim2     _maps_singleton       Maps.singleton         ["k", "v"]               k v mapKv,
    prim1     _maps_size            Maps.size              ["k", "v"]               mapKv int32,
    prim1     _maps_toList          Maps.toList            ["k", "v"]               mapKv (list $ pair k v),
    prim2     _maps_union           Maps.union             ["k", "v"]               mapKv mapKv mapKv]
  where
    k = variable "k"
    k1 = variable "k1"
    k2 = variable "k2"
    v = variable "v"
    v1 = variable "v1"
    v2 = variable "v2"
    mapKv = Prims.map k v

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
    prim2Eval _maybes_apply     EvalMaybes.apply    ["x", "y"]      (optional $ function x y) (optional x) (optional y),
    prim2Eval _maybes_bind      EvalMaybes.bind     ["x", "y"]      (optional x) (function x (optional y)) (optional y),
    prim3Eval _maybes_cases     EvalMaybes.cases    ["x", "y"]      (optional x) y (function x y) y,
    prim1     _maybes_cat       Maybes.cat          ["x"]           (list $ optional x) (list x),
    prim3Eval _maybes_compose   EvalMaybes.compose  ["x", "y", "z"] (function x $ optional y) (function y $ optional z) x (optional z),
    prim1     _maybes_fromJust  Maybes.fromJust     ["x"]           (optional x) x,
    prim2     _maybes_fromMaybe Maybes.fromMaybe    ["x"]           x (optional x) x,
    prim1     _maybes_isJust    Maybes.isJust       ["x"]           (optional x) boolean,
    prim1     _maybes_isNothing Maybes.isNothing    ["x"]           (optional x) boolean,
    prim2Eval _maybes_map       EvalMaybes.map      ["x", "y"]      (function x y) (optional x) (optional y),
    prim2Eval _maybes_mapMaybe  EvalMaybes.mapMaybe ["x", "y"]      (function x $ optional y) (list x) (list y),
    prim3Eval _maybes_maybe     EvalMaybes.maybe    ["y", "x"]      y (function x y) (optional x) y,
    prim1     _maybes_pure      Maybes.pure         ["x"]           x (optional x)]
  where
    x = variable "x"
    y = variable "y"
    z = variable "z"

hydraLibPairs :: Library
hydraLibPairs = standardLibrary _hydra_lib_pairs [
    prim3Eval _pairs_bimap  EvalPairs.bimap  ["a", "b", "c", "d"] (function a c) (function b d) (pair a b) (pair c d),
    prim1     _pairs_first  Pairs.first      ["a", "b"]           (pair a b) a,
    prim1     _pairs_second Pairs.second     ["a", "b"]           (pair a b) b]
  where
    a = variable "a"
    b = variable "b"
    c = variable "c"
    d = variable "d"

hydraLibSets :: Library
hydraLibSets = standardLibrary _hydra_lib_sets [
    prim2     _sets_delete       Sets.delete       ["x"]      x (set x) (set x),
    prim2     _sets_difference   Sets.difference   ["x"]      (set x) (set x) (set x),
    prim0     _sets_empty        Sets.empty        ["x"]      (set x),
    prim1     _sets_fromList     Sets.fromList     ["x"]      (list x) (set x),
    prim2     _sets_insert       Sets.insert       ["x"]      x (set x) (set x),
    prim2     _sets_intersection Sets.intersection ["x"]      (set x) (set x) (set x),
    prim2Eval _sets_map          EvalSets.map      ["x", "y"] (function x y) (set x) (set y),
    prim2     _sets_member       Sets.member       ["x"]      x (set x) boolean,
    prim1     _sets_null         Sets.null         ["x"]      (set x) boolean,
    prim1     _sets_singleton    Sets.singleton    ["x"]      x (set x),
    prim1     _sets_size         Sets.size         ["x"]      (set x) int32,
    prim1     _sets_toList       Sets.toList       ["x"]      (set x) (list x),
    prim2     _sets_union        Sets.union        ["x"]      (set x) (set x) (set x),
    prim1     _sets_unions       Sets.unions       ["x"]      (list $ set x) (set x)]
  where
    x = variable "x"
    y = variable "y"

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
