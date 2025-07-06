{-# LANGUAGE OverloadedStrings #-}

-- | Implementations of the Hydra standard libraries in Haskell
module Hydra.Sources.Libraries where

import Hydra.Kernel
import qualified Hydra.Extract.Core as ExtractCore
import Hydra.Dsl.Prims as Prims
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Hydra.Lib.Chars as Chars
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings

import qualified Data.List as L


-- * Hydra standard library

standardLibrary :: Namespace -> [Primitive] -> Library
standardLibrary ns prims = Library {
  libraryNamespace = ns,
  libraryPrefix = L.drop (L.length ("hydra.lib." :: String)) $ unNamespace ns,
  libraryPrimitives = prims}

standardLibraries :: [Library]
standardLibraries = [
  hydraLibChars,
  hydraLibEquality,
  hydraLibFlows,
  hydraLibLists,
  hydraLibLiterals,
  hydraLibLogic,
  hydraLibMaps,
  hydraLibMathInt32,
  hydraLibOptionals,
  hydraLibSets,
  hydraLibStrings]

-- * hydra.lib.chars primitives

_hydra_lib_chars :: Namespace
_hydra_lib_chars = Namespace "hydra.lib.chars"

_chars_isAlphaNum = qname _hydra_lib_chars "isAlphaNum" :: Name
_chars_isLower    = qname _hydra_lib_chars "isLower" :: Name
_chars_isSpace    = qname _hydra_lib_chars "isSpace" :: Name
_chars_isUpper    = qname _hydra_lib_chars "isUpper" :: Name
_chars_toLower    = qname _hydra_lib_chars "toLower" :: Name
_chars_toUpper    = qname _hydra_lib_chars "toUpper" :: Name

hydraLibChars :: Library
hydraLibChars = standardLibrary _hydra_lib_strings [
  prim1 _chars_isAlphaNum Chars.isAlphaNum [] int32 boolean,
  prim1 _chars_isLower Chars.isLower [] int32 boolean,
  prim1 _chars_isSpace Chars.isSpace [] int32 boolean,
  prim1 _chars_isUpper Chars.isUpper [] int32 boolean,
  prim1 _chars_toLower Chars.toLower [] int32 int32,
  prim1 _chars_toUpper Chars.toUpper [] int32 int32]

-- * hydra.lib.equality primitives

_hydra_lib_equality :: Namespace
_hydra_lib_equality = Namespace "hydra.lib.equality"

_equality_compare  = qname _hydra_lib_equality "compare" :: Name
_equality_equal         = qname _hydra_lib_equality "equal" :: Name
_equality_gtFloat32     = qname _hydra_lib_equality "gtFloat32" :: Name
_equality_gtFloat64     = qname _hydra_lib_equality "gtFloat64" :: Name
_equality_gtInt32       = qname _hydra_lib_equality "gtInt32" :: Name
_equality_gteFloat32    = qname _hydra_lib_equality "gteFloat32" :: Name
_equality_gteFloat64    = qname _hydra_lib_equality "gteFloat64" :: Name
_equality_gteInt32      = qname _hydra_lib_equality "gteInt32" :: Name
_equality_identity      = qname _hydra_lib_equality "identity" :: Name
_equality_ltFloat32     = qname _hydra_lib_equality "ltFloat32" :: Name
_equality_ltFloat64     = qname _hydra_lib_equality "ltFloat64" :: Name
_equality_ltInt32       = qname _hydra_lib_equality "ltInt32" :: Name
_equality_lteFloat32    = qname _hydra_lib_equality "lteFloat32" :: Name
_equality_lteFloat64    = qname _hydra_lib_equality "lteFloat64" :: Name
_equality_lteInt32      = qname _hydra_lib_equality "lteInt32" :: Name

hydraLibEquality :: Library
hydraLibEquality = standardLibrary _hydra_lib_equality [
    prim2 _equality_compare  Equality.compare  []    int32 int32 comparison,
    prim2 _equality_equal         Equality.equal         []    x x boolean,
    prim1 _equality_identity      Equality.identity      ["x"] x x,
    prim2 _equality_gtFloat32     Equality.gtFloat32     []    float32 float32 boolean,
    prim2 _equality_gtFloat64     Equality.gtFloat64     []    float64 float64 boolean,
    prim2 _equality_gtInt32       Equality.gtInt32       []    int32 int32 boolean,
    prim2 _equality_gteFloat32    Equality.gteFloat32    []    float32 float32 boolean,
    prim2 _equality_gteFloat64    Equality.gteFloat64    []    float64 float64 boolean,
    prim2 _equality_gteInt32      Equality.gteInt32      []    int32 int32 boolean,
    prim2 _equality_ltFloat32     Equality.ltFloat32     []    float32 float32 boolean,
    prim2 _equality_ltFloat64     Equality.ltFloat64     []    float64 float64 boolean,
    prim2 _equality_ltInt32       Equality.ltInt32       []    int32 int32 boolean,
    prim2 _equality_lteFloat32    Equality.lteFloat32    []    float32 float32 boolean,
    prim2 _equality_lteFloat64    Equality.lteFloat64    []    float64 float64 boolean,
    prim2 _equality_lteInt32      Equality.lteInt32      []    int32 int32 boolean]
  where
    x = variable "x"

-- * hydra.lib.flows primitives

_hydra_lib_flows :: Namespace
_hydra_lib_flows = Namespace "hydra.lib.flows"

_flows_apply            = qname _hydra_lib_flows "apply" :: Name
_flows_bind             = qname _hydra_lib_flows "bind" :: Name
_flows_fail             = qname _hydra_lib_flows "fail" :: Name
_flows_map              = qname _hydra_lib_flows "map" :: Name
_flows_mapList          = qname _hydra_lib_flows "mapList" :: Name
_flows_pure             = qname _hydra_lib_flows "pure" :: Name
_flows_sequence         = qname _hydra_lib_flows "sequence" :: Name
_flows_traverseOptional = qname _hydra_lib_flows "traverseOptional" :: Name

hydraLibFlows :: Library
hydraLibFlows = standardLibrary _hydra_lib_flows [
    prim2 _flows_apply    Flows.apply    ["s", "x", "y"] (flow s (function x y)) (flow s x) (flow s y),
    prim2 _flows_bind     Flows.bind     ["s", "x", "y"] (flow s x) (function x (flow s y)) (flow s y),
    prim1 _flows_fail     Flows.fail     ["s", "x"]      string (flow s x),
    prim2 _flows_map      Flows.map      ["s", "x", "y"] (function x y) (flow s x) (flow s y),
    prim2 _flows_mapList  Flows.mapList  ["s", "x", "y"] (function x (flow s y)) (list x) (flow s (list y)),
    prim1 _flows_pure     Flows.pure     ["s", "x"]      x (flow s x),
    prim1 _flows_sequence Flows.sequence ["s", "x"]      (list (flow s x)) (flow s (list x)),
    prim2 _flows_traverseOptional Flows.traverseOptional ["s", "x", "y"] (function x $ flow s y) (optional x) (flow s $ optional y)]
  where
    s = variable "s"
    x = variable "x"
    y = variable "y"

-- * hydra.lib.lists primitives

_hydra_lib_lists :: Namespace
_hydra_lib_lists = Namespace "hydra.lib.lists"

_lists_apply       = qname _hydra_lib_lists "apply" :: Name
_lists_at          = qname _hydra_lib_lists "at" :: Name
_lists_bind        = qname _hydra_lib_lists "bind" :: Name
_lists_concat      = qname _hydra_lib_lists "concat" :: Name
_lists_concat2     = qname _hydra_lib_lists "concat2" :: Name
_lists_cons        = qname _hydra_lib_lists "cons" :: Name
_lists_drop        = qname _hydra_lib_lists "drop" :: Name
_lists_dropWhile   = qname _hydra_lib_lists "dropWhile" :: Name
_lists_elem        = qname _hydra_lib_lists "elem" :: Name
_lists_filter      = qname _hydra_lib_lists "filter" :: Name
_lists_foldl       = qname _hydra_lib_lists "foldl" :: Name
_lists_head        = qname _hydra_lib_lists "head" :: Name
_lists_init        = qname _hydra_lib_lists "init" :: Name
_lists_intercalate = qname _hydra_lib_lists "intercalate" :: Name
_lists_intersperse = qname _hydra_lib_lists "intersperse" :: Name
_lists_last        = qname _hydra_lib_lists "last" :: Name
_lists_length      = qname _hydra_lib_lists "length" :: Name
_lists_map         = qname _hydra_lib_lists "map" :: Name
_lists_nub         = qname _hydra_lib_lists "nub" :: Name
_lists_null        = qname _hydra_lib_lists "null" :: Name
_lists_pure        = qname _hydra_lib_lists "pure" :: Name
_lists_replicate   = qname _hydra_lib_lists "replicate" :: Name
_lists_reverse     = qname _hydra_lib_lists "reverse" :: Name
_lists_safeHead    = qname _hydra_lib_lists "safeHead" :: Name
_lists_singleton   = qname _hydra_lib_lists "singleton" :: Name
_lists_sort        = qname _hydra_lib_lists "sort" :: Name
_lists_sortOn      = qname _hydra_lib_lists "sortOn" :: Name
_lists_span        = qname _hydra_lib_lists "span" :: Name
_lists_tail        = qname _hydra_lib_lists "tail" :: Name
_lists_take        = qname _hydra_lib_lists "take" :: Name
_lists_transpose   = qname _hydra_lib_lists "transpose" :: Name
_lists_zip         = qname _hydra_lib_lists "zip" :: Name
_lists_zipWith     = qname _hydra_lib_lists "zipWith" :: Name

hydraLibLists :: Library
hydraLibLists = standardLibrary _hydra_lib_lists [
    prim2Interp _lists_apply       (Just applyInterp) ["x", "y"] (list $ function x y) (list x) (list y),
    prim2       _lists_at          Lists.at           ["x"] int32 (list x) x,
    prim2Interp _lists_bind        (Just bindInterp)  ["x", "y"] (list x) (function x (list y)) (list y),
    prim1       _lists_concat      Lists.concat       ["x"] (list (list x)) (list x),
    prim2       _lists_concat2     Lists.concat2      ["x"] (list x) (list x) (list x),
    prim2       _lists_cons        Lists.cons         ["x"] x (list x) (list x),
    prim2       _lists_drop        Lists.drop         ["x"] int32 (list x) (list x),
    prim2Interp _lists_dropWhile   Nothing            ["x"] (function x boolean) (list x) (list x),
    prim2       _lists_elem        Lists.elem         ["x"] x (list x) boolean,
    prim2       _lists_filter      Lists.filter       ["x"] (function x boolean) (list x) (list x),
    prim3       _lists_foldl       Lists.foldl        ["x", "y"] (function y (function x y)) y (list x) y,
    prim1       _lists_head        Lists.head         ["x"] (list x) x,
    prim1       _lists_init        Lists.init         ["x"] (list x) (list x),
    prim2       _lists_intercalate Lists.intercalate  ["x"] (list x) (list (list x)) (list x),
    prim2       _lists_intersperse Lists.intersperse  ["x"] x (list x) (list x),
    prim1       _lists_last        Lists.last         ["x"] (list x) x,
    prim1       _lists_length      Lists.length       ["x"] (list x) int32,
    prim2Interp _lists_map         (Just mapInterp)   ["x", "y"] (function x y) (list x) (list y),
    prim1       _lists_nub         Lists.nub          ["x"] (list x) (list x),
    prim1       _lists_null        Lists.null         ["x"] (list x) boolean,
    prim1       _lists_pure        Lists.pure         ["x"] x (list x),
    prim2       _lists_replicate   Lists.replicate    ["x"] int32 x (list x),
    prim1       _lists_reverse     Lists.reverse      ["x"] (list x) (list x),
    prim1       _lists_safeHead    Lists.safeHead     ["x"] (list x) (optional x),
    prim1       _lists_singleton   Lists.singleton    ["x"] x (list x),
    prim2Interp _lists_sortOn      Nothing            ["x", "y"] (function x y) (list x) (list x),
    prim2Interp _lists_span        Nothing            ["x", "y"] (function x boolean) (list x) (pair (list x) (list x)),
    prim1       _lists_sort        Lists.sort         ["x"] (list x) (list x),
    prim1       _lists_tail        Lists.tail         ["x"] (list x) (list x),
    prim2       _lists_take        Lists.take         ["x"] int32 (list x) (list x),
    prim1       _lists_transpose   Lists.transpose    ["x"] (list (list x)) (list (list x)),
    prim2       _lists_zip         Lists.zip          ["x", "y"] (list x) (list y) (list (pair x y)),
    prim3       _lists_zipWith     Lists.zipWith      ["x", "y", "z"] (function x $ function y z) (list x) (list y) (list z)]
  where
    x = variable "x"
    y = variable "y"
    z = variable "z"

-- | Interpreted implementation of hydra.lib.lists.apply
applyInterp :: Term -> Term -> Flow Graph Term
applyInterp funs' args' = do
    funs <- ExtractCore.list Prelude.pure funs'
    args <- ExtractCore.list Prelude.pure args'
    return $ Terms.list $ L.concat (helper args <$> funs)
  where
    helper args f = Terms.apply f <$> args

-- | Interpreted implementation of hydra.lib.lists.bind
bindInterp :: Term -> Term -> Flow Graph Term
bindInterp args' fun = do
    args <- ExtractCore.list Prelude.pure args'
    return $ Terms.apply (Terms.primitive _lists_concat) (Terms.list $ Terms.apply fun <$> args)

-- | Interpreted implementation of hydra.lib.lists.map
mapInterp :: Term -> Term -> Flow Graph Term
mapInterp fun args' = do
    args <- ExtractCore.list Prelude.pure args'
    return $ Terms.list (Terms.apply fun <$> args)

-- * hydra.lib.literals primitives

_hydra_lib_literals :: Namespace
_hydra_lib_literals = Namespace "hydra.lib.literals"

_literals_bigfloatToBigint  = qname _hydra_lib_literals "bigfloatToBigint" :: Name
_literals_bigfloatToFloat32 = qname _hydra_lib_literals "bigfloatToFloat32" :: Name
_literals_bigfloatToFloat64 = qname _hydra_lib_literals "bigfloatToFloat64" :: Name
_literals_bigintToBigfloat  = qname _hydra_lib_literals "bigintToBigfloat" :: Name
_literals_bigintToInt8      = qname _hydra_lib_literals "bigintToInt8" :: Name
_literals_bigintToInt16     = qname _hydra_lib_literals "bigintToInt16" :: Name
_literals_bigintToInt32     = qname _hydra_lib_literals "bigintToInt32" :: Name
_literals_bigintToInt64     = qname _hydra_lib_literals "bigintToInt64" :: Name
_literals_bigintToUint8     = qname _hydra_lib_literals "bigintToUint8" :: Name
_literals_bigintToUint16    = qname _hydra_lib_literals "bigintToUint16" :: Name
_literals_bigintToUint32    = qname _hydra_lib_literals "bigintToUint32" :: Name
_literals_bigintToUint64    = qname _hydra_lib_literals "bigintToUint64" :: Name
_literals_binaryToString    = qname _hydra_lib_literals "binaryToString" :: Name
_literals_float32ToBigfloat = qname _hydra_lib_literals "float32ToBigfloat" :: Name
_literals_float64ToBigfloat = qname _hydra_lib_literals "float64ToBigfloat" :: Name
_literals_int8ToBigint      = qname _hydra_lib_literals "int8ToBigint" :: Name
_literals_int16ToBigint     = qname _hydra_lib_literals "int16ToBigint" :: Name
_literals_int32ToBigint     = qname _hydra_lib_literals "int32ToBigint" :: Name
_literals_int64ToBigint     = qname _hydra_lib_literals "int64ToBigint" :: Name
_literals_readBigfloat      = qname _hydra_lib_literals "readBigfloat" :: Name
_literals_readBoolean       = qname _hydra_lib_literals "readBoolean" :: Name
_literals_readFloat32       = qname _hydra_lib_literals "readFloat32" :: Name
_literals_readFloat64       = qname _hydra_lib_literals "readFloat64" :: Name
_literals_readInt32         = qname _hydra_lib_literals "readInt32" :: Name
_literals_readInt64         = qname _hydra_lib_literals "readInt64" :: Name
_literals_readString        = qname _hydra_lib_literals "readString" :: Name
_literals_showBigfloat      = qname _hydra_lib_literals "showBigfloat" :: Name
_literals_showBigint        = qname _hydra_lib_literals "showBigint" :: Name
_literals_showBoolean       = qname _hydra_lib_literals "show" :: Name
_literals_showFloat32       = qname _hydra_lib_literals "showFloat32" :: Name
_literals_showFloat64       = qname _hydra_lib_literals "showFloat64" :: Name
_literals_showInt8          = qname _hydra_lib_literals "showInt8" :: Name
_literals_showInt16         = qname _hydra_lib_literals "showInt16" :: Name
_literals_showInt32         = qname _hydra_lib_literals "showInt32" :: Name
_literals_showInt64         = qname _hydra_lib_literals "showInt64" :: Name
_literals_showUint8         = qname _hydra_lib_literals "showUint8" :: Name
_literals_showUint16        = qname _hydra_lib_literals "showUint16" :: Name
_literals_showUint32        = qname _hydra_lib_literals "showUint32" :: Name
_literals_showUint64        = qname _hydra_lib_literals "showUint64" :: Name
_literals_showString        = qname _hydra_lib_literals "showString" :: Name
_literals_stringToBinary    = qname _hydra_lib_literals "stringToBinary" :: Name
_literals_uint8ToBigint     = qname _hydra_lib_literals "uint8ToBigint" :: Name
_literals_uint16ToBigint    = qname _hydra_lib_literals "uint16ToBigint" :: Name
_literals_uint32ToBigint    = qname _hydra_lib_literals "uint32ToBigint" :: Name
_literals_uint64ToBigint    = qname _hydra_lib_literals "uint64ToBigint" :: Name

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

-- * hydra.lib.logic primitives

_hydra_lib_logic :: Namespace
_hydra_lib_logic = Namespace "hydra.lib.logic"

_logic_and = qname _hydra_lib_logic "and" :: Name
_logic_ifElse = qname _hydra_lib_logic "ifElse" :: Name
_logic_not    = qname _hydra_lib_logic "not" :: Name
_logic_or     = qname _hydra_lib_logic "or" :: Name

hydraLibLogic :: Library
hydraLibLogic = standardLibrary _hydra_lib_logic [
    prim2 _logic_and    Logic.and    []    boolean boolean boolean,
    prim3 _logic_ifElse Logic.ifElse ["x"] boolean x x x,
    prim1 _logic_not    Logic.not    []    boolean boolean,
    prim2 _logic_or     Logic.or     []    boolean boolean boolean]
  where
    x = variable "x"

-- * hydra.lib.maps primitives

_hydra_lib_maps :: Namespace
_hydra_lib_maps = Namespace "hydra.lib.maps"

_maps_alter           = qname _hydra_lib_maps "alter" :: Name
_maps_bimap           = qname _hydra_lib_maps "bimap" :: Name
_maps_elems           = qname _hydra_lib_maps "elems" :: Name
_maps_empty           = qname _hydra_lib_maps "empty" :: Name
_maps_filter          = qname _hydra_lib_maps "filter" :: Name
_maps_filterWithKey   = qname _hydra_lib_maps "filterWithKey" :: Name
_maps_findWithDefault = qname _hydra_lib_maps "findWithDefault" :: Name
_maps_fromList        = qname _hydra_lib_maps "fromList" :: Name
_maps_insert          = qname _hydra_lib_maps "insert" :: Name
_maps_keys            = qname _hydra_lib_maps "keys" :: Name
_maps_lookup          = qname _hydra_lib_maps "lookup" :: Name
_maps_map             = qname _hydra_lib_maps "map" :: Name
_maps_mapKeys         = qname _hydra_lib_maps "mapKeys" :: Name
_maps_member          = qname _hydra_lib_maps "member" :: Name
_maps_null            = qname _hydra_lib_maps "null" :: Name
_maps_remove          = qname _hydra_lib_maps "remove" :: Name
_maps_singleton       = qname _hydra_lib_maps "singleton" :: Name
_maps_size            = qname _hydra_lib_maps "size" :: Name
_maps_toList          = qname _hydra_lib_maps "toList" :: Name
_maps_union           = qname _hydra_lib_maps "union" :: Name
_maps_values          = qname _hydra_lib_maps "values" :: Name

hydraLibMaps :: Library
hydraLibMaps = standardLibrary _hydra_lib_maps [
    prim3Interp _maps_alter     Nothing        ["k", "v"]               (function (optional v) (optional v)) k mapKv mapKv,
    prim3 _maps_bimap           Maps.bimap     ["k1", "k2", "v1", "v2"] (function k1 k2) (function v1 v2) (Prims.map k1 v1) (Prims.map k2 v2),
    prim1 _maps_elems           Maps.elems     ["k", "v"]               mapKv (list v),
    prim0 _maps_empty           Maps.empty     ["k", "v"]               mapKv,
    prim2 _maps_filter          Maps.filter    ["k", "v"]               (function v boolean) mapKv mapKv,
    prim2 _maps_filterWithKey   Maps.filterWithKey ["k", "v"]           (function k (function v boolean)) mapKv mapKv,
    prim3 _maps_findWithDefault Maps.findWithDefault ["k", "v"]         v k mapKv v,
    prim1 _maps_fromList        Maps.fromList  ["k", "v"]               (list $ pair k v) mapKv,
    prim3 _maps_insert          Maps.insert    ["k", "v"]               k v mapKv mapKv,
    prim1 _maps_keys            Maps.keys      ["k", "v"]               mapKv (list k),
    prim2 _maps_lookup          Maps.lookup    ["k", "v"]               k mapKv (optional v),
    prim2 _maps_map             Maps.map       ["k", "v1", "v2"]        (function v1 v2) (Prims.map k v1) (Prims.map k v2),
    prim2 _maps_mapKeys         Maps.mapKeys   ["k1", "k2", "v"]        (function k1 k2) (Prims.map k1 v) (Prims.map k2 v),
    prim2 _maps_member          Maps.member    ["k", "v"]               k mapKv boolean,
    prim1 _maps_null            Maps.null      ["k", "v"]               mapKv boolean,
    prim1 _maps_size            Maps.size      ["k", "v"]               mapKv int32,
    prim2 _maps_remove          Maps.remove    ["k", "v"]               k mapKv mapKv,
    prim2 _maps_singleton       Maps.singleton ["k", "v"]               k v mapKv,
    prim1 _maps_size            Maps.size      ["k", "v"]               mapKv int32,
    prim1 _maps_toList          Maps.toList    ["k", "v"]               mapKv (list $ pair k v),
    prim2 _maps_union           Maps.union     ["k", "v"]               mapKv mapKv mapKv]
  where
    k = variable "k"
    k1 = variable "k1"
    k2 = variable "k2"
    v = variable "v"
    v1 = variable "v1"
    v2 = variable "v2"
    mapKv = Prims.map k v

-- * hydra.lib.math primitives

_hydra_lib_math :: Namespace
_hydra_lib_math = Namespace "hydra.lib.math"

_math_add        = qname _hydra_lib_math "add" :: Name
_math_div        = qname _hydra_lib_math "div" :: Name
_math_min        = qname _hydra_lib_math "min" :: Name
_math_mod        = qname _hydra_lib_math "mod" :: Name
_math_mul        = qname _hydra_lib_math "mul" :: Name
_math_neg        = qname _hydra_lib_math "neg" :: Name
_math_rangeInt32 = qname _hydra_lib_math "rangeInt32" :: Name
_math_rem        = qname _hydra_lib_math "rem" :: Name
_math_sub        = qname _hydra_lib_math "sub" :: Name

hydraLibMathInt32 :: Library
hydraLibMathInt32 = standardLibrary _hydra_lib_math [
  prim2 _math_add        Math.add        [] int32 int32 int32,
  prim2 _math_div        Math.div        [] int32 int32 int32,
  prim2 _math_min        Math.min        [] int32 int32 int32,
  prim2 _math_mod        Math.mod        [] int32 int32 int32,
  prim2 _math_mul        Math.mul        [] int32 int32 int32,
  prim1 _math_neg        Math.neg        [] int32 int32,
  prim2 _math_rangeInt32 Math.rangeInt32 [] int32 int32 (list int32),
  prim2 _math_rem        Math.rem        [] int32 int32 int32,
  prim2 _math_sub        Math.sub        [] int32 int32 int32]

-- * hydra.lib.optionals primitives

_hydra_lib_optionals :: Namespace
_hydra_lib_optionals = Namespace "hydra.lib.optionals"

_optionals_apply :: Name
_optionals_apply     = qname _hydra_lib_optionals "apply" :: Name
_optionals_bind      = qname _hydra_lib_optionals "bind" :: Name
_optionals_cases     = qname _hydra_lib_optionals "cases" :: Name
_optionals_cat       = qname _hydra_lib_optionals "cat" :: Name
_optionals_compose   = qname _hydra_lib_optionals "compose" :: Name
_optionals_fromJust  = qname _hydra_lib_optionals "fromJust" :: Name
_optionals_fromMaybe = qname _hydra_lib_optionals "fromMaybe" :: Name
_optionals_isJust    = qname _hydra_lib_optionals "isJust" :: Name
_optionals_isNothing = qname _hydra_lib_optionals "isNothing" :: Name
_optionals_map       = qname _hydra_lib_optionals "map" :: Name
_optionals_mapMaybe  = qname _hydra_lib_optionals "mapMaybe" :: Name
_optionals_maybe     = qname _hydra_lib_optionals "maybe" :: Name
_optionals_pure      = qname _hydra_lib_optionals "pure" :: Name

hydraLibOptionals :: Library
hydraLibOptionals = standardLibrary _hydra_lib_optionals [
    prim2       _optionals_apply     Optionals.apply           ["x", "y"]      (optional $ function x y) (optional x) (optional y),
    prim2       _optionals_bind      Optionals.bind            ["x", "y"]      (optional x) (function x (optional y)) (optional y),
    prim3Interp _optionals_cases     (Just casesInterp)        ["x", "y"]      (optional x) y (function x y) y,
    prim1       _optionals_cat       Optionals.cat             ["x"]           (list $ optional x) (list x),
    prim2       _optionals_compose   Optionals.compose         ["x", "y", "z"] (function x $ optional y) (function y $ optional z) (function x $ optional z),
    prim1       _optionals_fromJust  Optionals.fromJust        ["x"]           (optional x) x,
    prim2       _optionals_fromMaybe Optionals.fromMaybe       ["x"]           x (optional x) x,
    prim1       _optionals_isJust    Optionals.isJust          ["x"]           (optional x) boolean,
    prim1       _optionals_isNothing Optionals.isNothing       ["x"]           (optional x) boolean,
    prim2Interp _optionals_map       (Just optionalsMapInterp) ["x", "y"]      (function x y) (optional x) (optional y),
    prim2Interp _optionals_mapMaybe  Nothing                   ["x", "y"]      (function x $ optional y) (list x) (list y),
    prim3Interp _optionals_maybe     (Just maybeInterp)        ["x", "y"]      y (function x y) (optional x) y,
    prim1       _optionals_pure      Optionals.pure            ["x"]           x (optional x)]
  where
    x = variable "x"
    y = variable "y"
    z = variable "z"

-- | Interpreted implementation of hydra.lib.optionals.cases
casesInterp :: Term -> Term -> Term -> Flow Graph Term
casesInterp opt def fun = maybeInterp def fun opt

-- | Interpreted implementation of hydra.lib.optionals.maybe
maybeInterp :: Term -> Term -> Term -> Flow Graph Term
maybeInterp def fun opt = do
    mval <- ExtractCore.optional Prelude.pure opt
    return $ case mval of
      Nothing -> def
      Just val -> Terms.apply fun val

optionalsMapInterp :: Term -> Term -> Flow Graph Term
optionalsMapInterp fun opt = do
    mval <- ExtractCore.optional Prelude.pure opt
    return $ case mval of
      Nothing -> Terms.nothing
      Just val -> Terms.just $ Terms.apply fun val

-- * hydra.lib.sets primitives

_hydra_lib_sets :: Namespace
_hydra_lib_sets = Namespace "hydra.lib.sets"

_sets_delete       = qname _hydra_lib_sets "delete" :: Name
_sets_difference   = qname _hydra_lib_sets "difference" :: Name
_sets_empty        = qname _hydra_lib_sets "empty" :: Name
_sets_fromList     = qname _hydra_lib_sets "fromList" :: Name
_sets_insert       = qname _hydra_lib_sets "insert" :: Name
_sets_intersection = qname _hydra_lib_sets "intersection" :: Name
_sets_map          = qname _hydra_lib_sets "map" :: Name
_sets_member       = qname _hydra_lib_sets "member" :: Name
_sets_null         = qname _hydra_lib_sets "null" :: Name
_sets_singleton    = qname _hydra_lib_sets "singleton" :: Name
_sets_size         = qname _hydra_lib_sets "size" :: Name
_sets_toList       = qname _hydra_lib_sets "toList" :: Name
_sets_union        = qname _hydra_lib_sets "union" :: Name
_sets_unions       = qname _hydra_lib_sets "unions" :: Name

hydraLibSets :: Library
hydraLibSets = standardLibrary _hydra_lib_sets [
    prim2 _sets_delete       Sets.delete       ["x"]      x (set x) (set x),
    prim2 _sets_difference   Sets.difference   ["x"]      (set x) (set x) (set x),
    prim0 _sets_empty        Sets.empty        ["x"]      (set x),
    prim1 _sets_fromList     Sets.fromList     ["x"]      (list x) (set x),
    prim2 _sets_insert       Sets.insert       ["x"]      x (set x) (set x),
    prim2 _sets_intersection Sets.intersection ["x"]      (set x) (set x) (set x),
    prim2 _sets_map          Sets.map          ["x", "y"] (function x y) (set x) (set y),
    prim2 _sets_member       Sets.member       ["x"]      x (set x) boolean,
    prim1 _sets_null         Sets.null         ["x"]      (set x) boolean,
    prim1 _sets_singleton    Sets.singleton    ["x"]      x (set x),
    prim1 _sets_size         Sets.size         ["x"]      (set x) int32,
    prim1 _sets_toList       Sets.toList       ["x"]      (set x) (list x),
    prim2 _sets_union        Sets.union        ["x"]      (set x) (set x) (set x),
    prim1 _sets_unions       Sets.unions       ["x"]      (list $ set x) (set x)]
  where
    x = variable "x"
    y = variable "y"

-- * hydra.lib.strings primitives

_hydra_lib_strings :: Namespace
_hydra_lib_strings = Namespace "hydra.lib.strings"

_strings_cat         = qname _hydra_lib_strings "cat" :: Name
_strings_cat2        = qname _hydra_lib_strings "cat2" :: Name
_strings_charAt      = qname _hydra_lib_strings "charAt" :: Name
_strings_fromList    = qname _hydra_lib_strings "fromList" :: Name
_strings_intercalate = qname _hydra_lib_strings "intercalate" :: Name
_strings_null        = qname _hydra_lib_strings "null" :: Name
_strings_length      = qname _hydra_lib_strings "length" :: Name
_strings_lines       = qname _hydra_lib_strings "lines" :: Name
_strings_splitOn     = qname _hydra_lib_strings "splitOn" :: Name
_strings_toList      = qname _hydra_lib_strings "toList" :: Name
_strings_toLower     = qname _hydra_lib_strings "toLower" :: Name
_strings_toUpper     = qname _hydra_lib_strings "toUpper" :: Name
_strings_unlines     = qname _hydra_lib_strings "unlines" :: Name

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
