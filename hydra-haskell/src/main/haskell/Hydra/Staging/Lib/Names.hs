-- | Namespaces and primitive names for the Hydra standard library
module Hydra.Staging.Lib.Names where

import Hydra.Core
import Hydra.Module
import Hydra.Names


_hydra_lib_chars :: Namespace
_hydra_lib_chars = Namespace "hydra.lib.chars"

_chars_isAlphaNum = qname _hydra_lib_chars "isAlphaNum" :: Name
_chars_isLower    = qname _hydra_lib_chars "isLower" :: Name
_chars_isSpace    = qname _hydra_lib_chars "isSpace" :: Name
_chars_isUpper    = qname _hydra_lib_chars "isUpper" :: Name
_chars_toLower    = qname _hydra_lib_chars "toLower" :: Name
_chars_toUpper    = qname _hydra_lib_chars "toUpper" :: Name

_hydra_lib_eithers :: Namespace
_hydra_lib_eithers = Namespace "hydra.lib.eithers"

_eithers_either           = qname _hydra_lib_eithers "either" :: Name
_eithers_fromLeft         = qname _hydra_lib_eithers "fromLeft" :: Name
_eithers_fromRight        = qname _hydra_lib_eithers "fromRight" :: Name
_eithers_isLeft           = qname _hydra_lib_eithers "isLeft" :: Name
_eithers_isRight          = qname _hydra_lib_eithers "isRight" :: Name
_eithers_lefts            = qname _hydra_lib_eithers "lefts" :: Name
_eithers_partitionEithers = qname _hydra_lib_eithers "partitionEithers" :: Name
_eithers_rights           = qname _hydra_lib_eithers "rights" :: Name

_hydra_lib_equality :: Namespace
_hydra_lib_equality = Namespace "hydra.lib.equality"

_equality_compare  = qname _hydra_lib_equality "compare" :: Name
_equality_equal    = qname _hydra_lib_equality "equal" :: Name
_equality_gt       = qname _hydra_lib_equality "gt" :: Name
_equality_gte      = qname _hydra_lib_equality "gte" :: Name
_equality_identity = qname _hydra_lib_equality "identity" :: Name
_equality_lt       = qname _hydra_lib_equality "lt" :: Name
_equality_lte      = qname _hydra_lib_equality "lte" :: Name
_equality_max      = qname _hydra_lib_equality "max" :: Name
_equality_min      = qname _hydra_lib_equality "min" :: Name

_hydra_lib_flows :: Namespace
_hydra_lib_flows = Namespace "hydra.lib.flows"

_flows_apply       = qname _hydra_lib_flows "apply" :: Name
_flows_bind        = qname _hydra_lib_flows "bind" :: Name
_flows_fail        = qname _hydra_lib_flows "fail" :: Name
_flows_foldl       = qname _hydra_lib_flows "foldl" :: Name
_flows_map         = qname _hydra_lib_flows "map" :: Name
_flows_mapElems    = qname _hydra_lib_flows "mapElems" :: Name
_flows_mapKeys     = qname _hydra_lib_flows "mapKeys" :: Name
_flows_mapList     = qname _hydra_lib_flows "mapList" :: Name
_flows_mapMaybe    = qname _hydra_lib_flows "mapMaybe" :: Name
_flows_mapSet      = qname _hydra_lib_flows "mapSet" :: Name
_flows_pure        = qname _hydra_lib_flows "pure" :: Name
_flows_sequence    = qname _hydra_lib_flows "sequence" :: Name

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
_lists_group       = qname _hydra_lib_lists "group" :: Name
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

_hydra_lib_logic :: Namespace
_hydra_lib_logic = Namespace "hydra.lib.logic"

_logic_and = qname _hydra_lib_logic "and" :: Name
_logic_ifElse = qname _hydra_lib_logic "ifElse" :: Name
_logic_not    = qname _hydra_lib_logic "not" :: Name
_logic_or     = qname _hydra_lib_logic "or" :: Name

_hydra_lib_maps :: Namespace
_hydra_lib_maps = Namespace "hydra.lib.maps"

_maps_alter           = qname _hydra_lib_maps "alter" :: Name
_maps_bimap           = qname _hydra_lib_maps "bimap" :: Name
_maps_delete          = qname _hydra_lib_maps "delete" :: Name
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
_maps_singleton       = qname _hydra_lib_maps "singleton" :: Name
_maps_size            = qname _hydra_lib_maps "size" :: Name
_maps_toList          = qname _hydra_lib_maps "toList" :: Name
_maps_union           = qname _hydra_lib_maps "union" :: Name

_hydra_lib_math :: Namespace
_hydra_lib_math = Namespace "hydra.lib.math"

_math_abs     = qname _hydra_lib_math "abs" :: Name
_math_acos    = qname _hydra_lib_math "acos" :: Name
_math_acosh   = qname _hydra_lib_math "acosh" :: Name
_math_add     = qname _hydra_lib_math "add" :: Name
_math_asin    = qname _hydra_lib_math "asin" :: Name
_math_asinh   = qname _hydra_lib_math "asinh" :: Name
_math_atan    = qname _hydra_lib_math "atan" :: Name
_math_atan2   = qname _hydra_lib_math "atan2" :: Name
_math_atanh   = qname _hydra_lib_math "atanh" :: Name
_math_ceiling = qname _hydra_lib_math "ceiling" :: Name
_math_cos     = qname _hydra_lib_math "cos" :: Name
_math_cosh    = qname _hydra_lib_math "cosh" :: Name
_math_div     = qname _hydra_lib_math "div" :: Name
_math_e       = qname _hydra_lib_math "e" :: Name
_math_even    = qname _hydra_lib_math "even" :: Name
_math_exp     = qname _hydra_lib_math "exp" :: Name
_math_floor   = qname _hydra_lib_math "floor" :: Name
_math_log     = qname _hydra_lib_math "log" :: Name
_math_logBase = qname _hydra_lib_math "logBase" :: Name
_math_mod     = qname _hydra_lib_math "mod" :: Name
_math_mul     = qname _hydra_lib_math "mul" :: Name
_math_negate  = qname _hydra_lib_math "negate" :: Name
_math_odd     = qname _hydra_lib_math "odd" :: Name
_math_pi      = qname _hydra_lib_math "pi" :: Name
_math_pow     = qname _hydra_lib_math "pow" :: Name
_math_pred    = qname _hydra_lib_math "pred" :: Name
_math_range   = qname _hydra_lib_math "range" :: Name
_math_rem     = qname _hydra_lib_math "rem" :: Name
_math_round   = qname _hydra_lib_math "round" :: Name
_math_signum  = qname _hydra_lib_math "signum" :: Name
_math_sin     = qname _hydra_lib_math "sin" :: Name
_math_sinh    = qname _hydra_lib_math "sinh" :: Name
_math_sqrt    = qname _hydra_lib_math "sqrt" :: Name
_math_sub     = qname _hydra_lib_math "sub" :: Name
_math_succ    = qname _hydra_lib_math "succ" :: Name
_math_tan     = qname _hydra_lib_math "tan" :: Name
_math_tanh    = qname _hydra_lib_math "tanh" :: Name
_math_truncate = qname _hydra_lib_math "truncate" :: Name

_hydra_lib_maybes :: Namespace
_hydra_lib_maybes = Namespace "hydra.lib.maybes"

_maybes_apply :: Name
_maybes_apply     = qname _hydra_lib_maybes "apply" :: Name
_maybes_bind      = qname _hydra_lib_maybes "bind" :: Name
_maybes_cases     = qname _hydra_lib_maybes "cases" :: Name
_maybes_cat       = qname _hydra_lib_maybes "cat" :: Name
_maybes_compose   = qname _hydra_lib_maybes "compose" :: Name
_maybes_fromJust  = qname _hydra_lib_maybes "fromJust" :: Name
_maybes_fromMaybe = qname _hydra_lib_maybes "fromMaybe" :: Name
_maybes_isJust    = qname _hydra_lib_maybes "isJust" :: Name
_maybes_isNothing = qname _hydra_lib_maybes "isNothing" :: Name
_maybes_map       = qname _hydra_lib_maybes "map" :: Name
_maybes_mapMaybe  = qname _hydra_lib_maybes "mapMaybe" :: Name
_maybes_maybe     = qname _hydra_lib_maybes "maybe" :: Name
_maybes_pure      = qname _hydra_lib_maybes "pure" :: Name

_hydra_lib_pairs :: Namespace
_hydra_lib_pairs = Namespace "hydra.lib.pairs"

_pairs_first  = qname _hydra_lib_pairs "first"  :: Name
_pairs_second = qname _hydra_lib_pairs "second" :: Name

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
