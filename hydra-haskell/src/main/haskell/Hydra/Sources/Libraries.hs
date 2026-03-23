
-- | Implementations of the Hydra standard libraries in Haskell
module Hydra.Sources.Libraries (
  module Hydra.Sources.Libraries,
) where

import Hydra.Kernel
import qualified Hydra.Lib.Names as LibNames
import Hydra.Dsl.Prims as Prims
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Hydra.Lib.Chars as Chars
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Regex as Regex
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings

import qualified Data.List as L


-- Compatibility aliases for legacy _* names (from the old Staging.Lib.Names module).
-- These map old names to the new generated Hydra.Lib.Names constants.

-- Namespace aliases
_hydra_lib_chars    = LibNames.chars
_hydra_lib_eithers  = LibNames.eithers
_hydra_lib_equality = LibNames.equality
_hydra_lib_lists    = LibNames.lists
_hydra_lib_literals = LibNames.literals
_hydra_lib_logic    = LibNames.logic
_hydra_lib_maps     = LibNames.maps
_hydra_lib_math     = LibNames.math
_hydra_lib_maybes   = LibNames.maybes
_hydra_lib_pairs    = LibNames.pairs
_hydra_lib_regex    = LibNames.regex
_hydra_lib_sets     = LibNames.sets
_hydra_lib_strings  = LibNames.strings
_hydra_typeclass    = LibNames.typeclass

-- Chars
_chars_isAlphaNum = LibNames.charsIsAlphaNum
_chars_isLower    = LibNames.charsIsLower
_chars_isSpace    = LibNames.charsIsSpace
_chars_isUpper    = LibNames.charsIsUpper
_chars_toLower    = LibNames.charsToLower
_chars_toUpper    = LibNames.charsToUpper

-- Eithers
_eithers_bind             = LibNames.eithersBind
_eithers_bimap            = LibNames.eithersBimap
_eithers_either           = LibNames.eithersEither
_eithers_foldl            = LibNames.eithersFoldl
_eithers_fromLeft         = LibNames.eithersFromLeft
_eithers_fromRight        = LibNames.eithersFromRight
_eithers_isLeft           = LibNames.eithersIsLeft
_eithers_isRight          = LibNames.eithersIsRight
_eithers_lefts            = LibNames.eithersLefts
_eithers_map              = LibNames.eithersMap
_eithers_mapList          = LibNames.eithersMapList
_eithers_mapMaybe         = LibNames.eithersMapMaybe
_eithers_mapSet           = LibNames.eithersMapSet
_eithers_partitionEithers = LibNames.eithersPartitionEithers
_eithers_rights           = LibNames.eithersRights

-- Equality
_equality_compare  = LibNames.equalityCompare
_equality_equal    = LibNames.equalityEqual
_equality_gt       = LibNames.equalityGt
_equality_gte      = LibNames.equalityGte
_equality_identity = LibNames.equalityIdentity
_equality_lt       = LibNames.equalityLt
_equality_lte      = LibNames.equalityLte
_equality_max      = LibNames.equalityMax
_equality_min      = LibNames.equalityMin

-- Lists
_lists_apply       = LibNames.listsApply
_lists_at          = LibNames.listsAt
_lists_bind        = LibNames.listsBind
_lists_concat      = LibNames.listsConcat
_lists_concat2     = LibNames.listsConcat2
_lists_cons        = LibNames.listsCons
_lists_drop        = LibNames.listsDrop
_lists_dropWhile   = LibNames.listsDropWhile
_lists_elem        = LibNames.listsElem
_lists_filter      = LibNames.listsFilter
_lists_find        = LibNames.listsFind
_lists_foldl       = LibNames.listsFoldl
_lists_foldr       = LibNames.listsFoldr
_lists_group       = LibNames.listsGroup
_lists_head        = LibNames.listsHead
_lists_init        = LibNames.listsInit
_lists_intercalate = LibNames.listsIntercalate
_lists_intersperse = LibNames.listsIntersperse
_lists_last        = LibNames.listsLast
_lists_length      = LibNames.listsLength
_lists_map         = LibNames.listsMap
_lists_nub         = LibNames.listsNub
_lists_null        = LibNames.listsNull
_lists_partition   = LibNames.listsPartition
_lists_pure        = LibNames.listsPure
_lists_replicate   = LibNames.listsReplicate
_lists_reverse     = LibNames.listsReverse
_lists_safeHead    = LibNames.listsSafeHead
_lists_singleton   = LibNames.listsSingleton
_lists_sort        = LibNames.listsSort
_lists_sortOn      = LibNames.listsSortOn
_lists_span        = LibNames.listsSpan
_lists_tail        = LibNames.listsTail
_lists_take        = LibNames.listsTake
_lists_transpose   = LibNames.listsTranspose
_lists_zip         = LibNames.listsZip
_lists_zipWith     = LibNames.listsZipWith

-- Literals
_literals_bigfloatToBigint  = LibNames.literalsBigfloatToBigint
_literals_bigfloatToFloat32 = LibNames.literalsBigfloatToFloat32
_literals_bigfloatToFloat64 = LibNames.literalsBigfloatToFloat64
_literals_bigintToBigfloat  = LibNames.literalsBigintToBigfloat
_literals_bigintToInt8      = LibNames.literalsBigintToInt8
_literals_bigintToInt16     = LibNames.literalsBigintToInt16
_literals_bigintToInt32     = LibNames.literalsBigintToInt32
_literals_bigintToInt64     = LibNames.literalsBigintToInt64
_literals_bigintToUint8     = LibNames.literalsBigintToUint8
_literals_bigintToUint16    = LibNames.literalsBigintToUint16
_literals_bigintToUint32    = LibNames.literalsBigintToUint32
_literals_bigintToUint64    = LibNames.literalsBigintToUint64
_literals_binaryToBytes     = LibNames.literalsBinaryToBytes
_literals_binaryToString    = LibNames.literalsBinaryToString
_literals_float32ToBigfloat = LibNames.literalsFloat32ToBigfloat
_literals_float64ToBigfloat = LibNames.literalsFloat64ToBigfloat
_literals_int8ToBigint      = LibNames.literalsInt8ToBigint
_literals_int16ToBigint     = LibNames.literalsInt16ToBigint
_literals_int32ToBigint     = LibNames.literalsInt32ToBigint
_literals_int64ToBigint     = LibNames.literalsInt64ToBigint
_literals_readBigfloat      = LibNames.literalsReadBigfloat
_literals_readBigint        = LibNames.literalsReadBigint
_literals_readBoolean       = LibNames.literalsReadBoolean
_literals_readFloat32       = LibNames.literalsReadFloat32
_literals_readFloat64       = LibNames.literalsReadFloat64
_literals_readInt8          = LibNames.literalsReadInt8
_literals_readInt16         = LibNames.literalsReadInt16
_literals_readInt32         = LibNames.literalsReadInt32
_literals_readInt64         = LibNames.literalsReadInt64
_literals_readString        = LibNames.literalsReadString
_literals_readUint8         = LibNames.literalsReadUint8
_literals_readUint16        = LibNames.literalsReadUint16
_literals_readUint32        = LibNames.literalsReadUint32
_literals_readUint64        = LibNames.literalsReadUint64
_literals_showBigfloat      = LibNames.literalsShowBigfloat
_literals_showBigint        = LibNames.literalsShowBigint
_literals_showBoolean       = LibNames.literalsShowBoolean
_literals_showFloat32       = LibNames.literalsShowFloat32
_literals_showFloat64       = LibNames.literalsShowFloat64
_literals_showInt8          = LibNames.literalsShowInt8
_literals_showInt16         = LibNames.literalsShowInt16
_literals_showInt32         = LibNames.literalsShowInt32
_literals_showInt64         = LibNames.literalsShowInt64
_literals_showUint8         = LibNames.literalsShowUint8
_literals_showUint16        = LibNames.literalsShowUint16
_literals_showUint32        = LibNames.literalsShowUint32
_literals_showUint64        = LibNames.literalsShowUint64
_literals_showString        = LibNames.literalsShowString
_literals_stringToBinary    = LibNames.literalsStringToBinary
_literals_uint8ToBigint     = LibNames.literalsUint8ToBigint
_literals_uint16ToBigint    = LibNames.literalsUint16ToBigint
_literals_uint32ToBigint    = LibNames.literalsUint32ToBigint
_literals_uint64ToBigint    = LibNames.literalsUint64ToBigint

-- Logic
_logic_and    = LibNames.logicAnd
_logic_ifElse = LibNames.logicIfElse
_logic_not    = LibNames.logicNot
_logic_or     = LibNames.logicOr

-- Maps
_maps_alter           = LibNames.mapsAlter
_maps_bimap           = LibNames.mapsBimap
_maps_delete          = LibNames.mapsDelete
_maps_elems           = LibNames.mapsElems
_maps_empty           = LibNames.mapsEmpty
_maps_filter          = LibNames.mapsFilter
_maps_filterWithKey   = LibNames.mapsFilterWithKey
_maps_findWithDefault = LibNames.mapsFindWithDefault
_maps_fromList        = LibNames.mapsFromList
_maps_insert          = LibNames.mapsInsert
_maps_keys            = LibNames.mapsKeys
_maps_lookup          = LibNames.mapsLookup
_maps_map             = LibNames.mapsMap
_maps_mapKeys         = LibNames.mapsMapKeys
_maps_member          = LibNames.mapsMember
_maps_null            = LibNames.mapsNull
_maps_singleton       = LibNames.mapsSingleton
_maps_size            = LibNames.mapsSize
_maps_toList          = LibNames.mapsToList
_maps_union           = LibNames.mapsUnion

-- Math
_math_abs      = LibNames.mathAbs
_math_acos     = LibNames.mathAcos
_math_acosh    = LibNames.mathAcosh
_math_add      = LibNames.mathAdd
_math_asin     = LibNames.mathAsin
_math_asinh    = LibNames.mathAsinh
_math_atan     = LibNames.mathAtan
_math_atan2    = LibNames.mathAtan2
_math_atanh    = LibNames.mathAtanh
_math_ceiling  = LibNames.mathCeiling
_math_cos      = LibNames.mathCos
_math_cosh     = LibNames.mathCosh
_math_div      = LibNames.mathDiv
_math_e        = LibNames.mathE
_math_even     = LibNames.mathEven
_math_exp      = LibNames.mathExp
_math_floor    = LibNames.mathFloor
_math_log      = LibNames.mathLog
_math_logBase  = LibNames.mathLogBase
_math_max      = LibNames.mathMax
_math_min      = LibNames.mathMin
_math_mod      = LibNames.mathMod
_math_mul      = LibNames.mathMul
_math_negate   = LibNames.mathNegate
_math_odd      = LibNames.mathOdd
_math_pi       = LibNames.mathPi
_math_pow      = LibNames.mathPow
_math_pred     = LibNames.mathPred
_math_range    = LibNames.mathRange
_math_rem      = LibNames.mathRem
_math_round         = LibNames.mathRound
_math_roundBigfloat = LibNames.mathRoundBigfloat
_math_roundFloat32  = LibNames.mathRoundFloat32
_math_roundFloat64  = LibNames.mathRoundFloat64
_math_signum        = LibNames.mathSignum
_math_sin      = LibNames.mathSin
_math_sinh     = LibNames.mathSinh
_math_sqrt     = LibNames.mathSqrt
_math_sub      = LibNames.mathSub
_math_succ     = LibNames.mathSucc
_math_tan      = LibNames.mathTan
_math_tanh     = LibNames.mathTanh
_math_truncate = LibNames.mathTruncate

-- Maybes
_maybes_apply     = LibNames.maybesApply
_maybes_bind      = LibNames.maybesBind
_maybes_cases     = LibNames.maybesCases
_maybes_cat       = LibNames.maybesCat
_maybes_compose   = LibNames.maybesCompose
_maybes_fromJust  = LibNames.maybesFromJust
_maybes_fromMaybe = LibNames.maybesFromMaybe
_maybes_isJust    = LibNames.maybesIsJust
_maybes_isNothing = LibNames.maybesIsNothing
_maybes_map       = LibNames.maybesMap
_maybes_mapMaybe  = LibNames.maybesMapMaybe
_maybes_maybe     = LibNames.maybesMaybe
_maybes_pure      = LibNames.maybesPure
_maybes_toList    = LibNames.maybesToList

-- Pairs
_pairs_bimap  = LibNames.pairsBimap
_pairs_first  = LibNames.pairsFirst
_pairs_second = LibNames.pairsSecond

-- Sets
_sets_delete       = LibNames.setsDelete
_sets_difference   = LibNames.setsDifference
_sets_empty        = LibNames.setsEmpty
_sets_fromList     = LibNames.setsFromList
_sets_insert       = LibNames.setsInsert
_sets_intersection = LibNames.setsIntersection
_sets_map          = LibNames.setsMap
_sets_member       = LibNames.setsMember
_sets_null         = LibNames.setsNull
_sets_singleton    = LibNames.setsSingleton
_sets_size         = LibNames.setsSize
_sets_toList       = LibNames.setsToList
_sets_union        = LibNames.setsUnion
_sets_unions       = LibNames.setsUnions

-- Regex
_regex_find       = LibNames.regexFind
_regex_findAll    = LibNames.regexFindAll
_regex_matches    = LibNames.regexMatches
_regex_replace    = LibNames.regexReplace
_regex_replaceAll = LibNames.regexReplaceAll
_regex_split      = LibNames.regexSplit

-- Strings
_strings_cat         = LibNames.stringsCat
_strings_cat2        = LibNames.stringsCat2
_strings_charAt      = LibNames.stringsCharAt
_strings_fromList    = LibNames.stringsFromList
_strings_intercalate = LibNames.stringsIntercalate
_strings_null        = LibNames.stringsNull
_strings_length      = LibNames.stringsLength
_strings_lines       = LibNames.stringsLines
_strings_splitOn     = LibNames.stringsSplitOn
_strings_toList      = LibNames.stringsToList
_strings_toLower     = LibNames.stringsToLower
_strings_toUpper     = LibNames.stringsToUpper
_strings_unlines     = LibNames.stringsUnlines

-- Type classes
_typeclass_Eq  = LibNames.typeclassEq
_typeclass_Ord = LibNames.typeclassOrd


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
  hydraLibLists,
  hydraLibLiterals,
  hydraLibLogic,
  hydraLibMaps,
  hydraLibMathFloat64,
  hydraLibMathInt32,
  hydraLibMaybes,
  hydraLibPairs,
  hydraLibRegex,
  hydraLibSets,
  hydraLibStrings]

standardLibrary :: Namespace -> [Primitive] -> Library
standardLibrary ns prims = Library {
  libraryNamespace = ns,
  libraryPrefix = L.drop (L.length ("hydra.lib." :: String)) $ unNamespace ns,
  libraryPrimitives = prims}

-- | A TermCoder for function types which uses beta reduction to bridge term-level
--   functions to native functions. This allows higher-order primitives like map,
--   filter, foldl, etc. to use native implementations rather than eval-level ones.
fun :: TermCoder x -> TermCoder y -> TermCoder (x -> y)
fun = Prims.functionWithReduce (\cx g t -> reduceTerm cx g True t)

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
    prim2       _eithers_bind             Eithers.bind             [_x, _y, _z]     (Prims.either_ x_ y_) (fun y_ (Prims.either_ x_ z_)) (Prims.either_ x_ z_),
    prim3       _eithers_bimap            Eithers.bimap            [_x, _y, _z, _w] (fun x_ z_) (fun y_ w_) (Prims.either_ x_ y_) (Prims.either_ z_ w_),
    prim3       _eithers_either           Eithers.either           [_x, _y, _z]     (fun x_ z_) (fun y_ z_) (Prims.either_ x_ y_) z_,
    prim3       _eithers_foldl            Eithers.foldl            [_x, _y, _z]     (fun x_ (fun y_ (Prims.either_ z_ x_))) x_ (list y_) (Prims.either_ z_ x_),
    prim2       _eithers_fromLeft         Eithers.fromLeft         [_x, _y]         x_ (Prims.either_ x_ y_) x_,
    prim2       _eithers_fromRight        Eithers.fromRight        [_x, _y]         y_ (Prims.either_ x_ y_) y_,
    prim1       _eithers_isLeft           Eithers.isLeft           [_x, _y]         (Prims.either_ x_ y_) boolean,
    prim1       _eithers_isRight          Eithers.isRight          [_x, _y]         (Prims.either_ x_ y_) boolean,
    prim1       _eithers_lefts            Eithers.lefts            [_x, _y]         (list $ Prims.either_ x_ y_) (list x_),
    prim2       _eithers_map              Eithers.map              [_x, _y, _z]     (fun x_ y_) (Prims.either_ z_ x_) (Prims.either_ z_ y_),
    prim2       _eithers_mapList          Eithers.mapList          [_x, _y, _z]     (fun x_ (Prims.either_ z_ y_)) (list x_) (Prims.either_ z_ (list y_)),
    prim2       _eithers_mapMaybe         Eithers.mapMaybe         [_x, _y, _z]     (fun x_ (Prims.either_ z_ y_)) (optional x_) (Prims.either_ z_ (optional y_)),
    prim2       _eithers_mapSet           Eithers.mapSet           [_x, _y, _z]     (fun x_ (Prims.either_ z_ y_)) (set x_) (Prims.either_ z_ (set y_)),
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

hydraLibLists :: Library
hydraLibLists = standardLibrary _hydra_lib_lists [
    prim2     _lists_apply        Lists.apply         [_x, _y]     (list $ fun x_ y_) (list x_) (list y_),
    prim2     _lists_at          Lists.at            [_x]         int32 (list x_) x_,
    prim2     _lists_bind        Lists.bind          [_x, _y]     (list x_) (fun x_ (list y_)) (list y_),
    prim1     _lists_concat      Lists.concat        [_x]         (list (list x_)) (list x_),
    prim2     _lists_concat2     Lists.concat2       [_x]         (list x_) (list x_) (list x_),
    prim2     _lists_cons        Lists.cons          [_x]         x_ (list x_) (list x_),
    prim2     _lists_drop        Lists.drop          [_x]         int32 (list x_) (list x_),
    prim2     _lists_dropWhile   Lists.dropWhile     [_x]         (fun x_ boolean) (list x_) (list x_),
    prim2     _lists_elem        Lists.elem          [_xEq]       x_ (list x_) boolean,
    prim2     _lists_filter      Lists.filter        [_x]         (fun x_ boolean) (list x_) (list x_),
    prim2     _lists_find        Lists.find          [_x]         (fun x_ boolean) (list x_) (optional x_),
    prim3     _lists_foldl       Lists.foldl         [_y, _x]     (fun y_ (fun x_ y_)) y_ (list x_) y_,
    prim3     _lists_foldr       Lists.foldr         [_x, _y]     (fun x_ (fun y_ y_)) y_ (list x_) y_,
    prim1     _lists_group       Lists.group         [_xEq]       (list x_) (list (list x_)),
    prim1     _lists_head        Lists.head          [_x]         (list x_) x_,
    prim1     _lists_init        Lists.init          [_x]         (list x_) (list x_),
    prim2     _lists_intercalate Lists.intercalate   [_x]         (list x_) (list (list x_)) (list x_),
    prim2     _lists_intersperse Lists.intersperse   [_x]         x_ (list x_) (list x_),
    prim1     _lists_last        Lists.last          [_x]         (list x_) x_,
    prim1     _lists_length      Lists.length        [_x]         (list x_) int32,
    prim2     _lists_map         Lists.map           [_x, _y]     (fun x_ y_) (list x_) (list y_),
    prim1     _lists_nub         Lists.nub           [_xEq]       (list x_) (list x_),
    prim1     _lists_null        Lists.null          [_x]         (list x_) boolean,
    prim2     _lists_partition   Lists.partition     [_x]         (fun x_ boolean) (list x_) (pair (list x_) (list x_)),
    prim1     _lists_pure        Lists.pure          [_x]         x_ (list x_),
    prim2     _lists_replicate   Lists.replicate     [_x]         int32 x_ (list x_),
    prim1     _lists_reverse     Lists.reverse       [_x]         (list x_) (list x_),
    prim1     _lists_safeHead    Lists.safeHead      [_x]         (list x_) (optional x_),
    prim1     _lists_singleton   Lists.singleton     [_x]         x_ (list x_),
    prim2     _lists_sortOn      Lists.sortOn        [_x, _yOrd]  (fun x_ y_) (list x_) (list x_),
    prim2     _lists_span        Lists.span          [_x]         (fun x_ boolean) (list x_) (pair (list x_) (list x_)),
    prim1     _lists_sort        Lists.sort          [_xOrd]      (list x_) (list x_),
    prim1     _lists_tail        Lists.tail          [_x]         (list x_) (list x_),
    prim2     _lists_take        Lists.take          [_x]         int32 (list x_) (list x_),
    prim1     _lists_transpose   Lists.transpose     [_x]         (list (list x_)) (list (list x_)),
    prim2     _lists_zip         Lists.zip           [_x, _y]     (list x_) (list y_) (list (pair x_ y_)),
    prim3     _lists_zipWith     Lists.zipWith       [_x, _y, _z] (fun x_ $ fun y_ z_) (list x_) (list y_) (list z_)]

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
  prim1 _literals_binaryToBytes     Literals.binaryToBytes     [] binary (list int32),
  prim1 _literals_binaryToString    Literals.binaryToString    [] binary string,
  prim1 _literals_float32ToBigfloat Literals.float32ToBigfloat [] float32 bigfloat,
  prim1 _literals_float64ToBigfloat Literals.float64ToBigfloat [] float64 bigfloat,
  prim1 _literals_int8ToBigint      Literals.int8ToBigint      [] int8 bigint,
  prim1 _literals_int16ToBigint     Literals.int16ToBigint     [] int16 bigint,
  prim1 _literals_int32ToBigint     Literals.int32ToBigint     [] int32 bigint,
  prim1 _literals_int64ToBigint     Literals.int64ToBigint     [] int64 bigint,
  prim1 _literals_readBigfloat      Literals.readBigfloat      [] string (optional bigfloat),
  prim1 _literals_readBigint        Literals.readBigint        [] string (optional bigint),
  prim1 _literals_readBoolean       Literals.readBoolean       [] string (optional boolean),
  prim1 _literals_readFloat32       Literals.readFloat32       [] string (optional float32),
  prim1 _literals_readFloat64       Literals.readFloat64       [] string (optional float64),
  prim1 _literals_readInt8          Literals.readInt8          [] string (optional int8),
  prim1 _literals_readInt16         Literals.readInt16         [] string (optional int16),
  prim1 _literals_readInt32         Literals.readInt32         [] string (optional int32),
  prim1 _literals_readInt64         Literals.readInt64         [] string (optional int64),
  prim1 _literals_readString        Literals.readString        [] string (optional string),
  prim1 _literals_readUint8         Literals.readUint8         [] string (optional uint8),
  prim1 _literals_readUint16        Literals.readUint16        [] string (optional uint16),
  prim1 _literals_readUint32        Literals.readUint32        [] string (optional uint32),
  prim1 _literals_readUint64        Literals.readUint64        [] string (optional uint64),
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
    prim3     _maps_alter           Maps.alter             [_v, _kOrd]                  (fun (optional v_) (optional v_)) k_ mapKv mapKv,
    prim3     _maps_bimap           Maps.bimap             [_k1Ord, _k2Ord, _v1, _v2]   (fun k1_ k2_) (fun v1_ v2_) (Prims.map k1_ v1_) (Prims.map k2_ v2_),
    prim1     _maps_elems           Maps.elems             [_kOrd, _v]                  mapKv (list v_),
    prim2     _maps_delete          Maps.delete            [_kOrd, _v]                  k_ mapKv mapKv,
    prim0     _maps_empty           Maps.empty             [_kOrd, _v]                  mapKv,
    prim2     _maps_filter          Maps.filter            [_v, _kOrd]                  (fun v_ boolean) mapKv mapKv,
    prim2     _maps_filterWithKey   Maps.filterWithKey     [_kOrd, _v]                  (fun k_ (fun v_ boolean)) mapKv mapKv,
    prim3     _maps_findWithDefault Maps.findWithDefault   [_v, _kOrd]                  v_ k_ mapKv v_,
    prim1     _maps_fromList        Maps.fromList          [_kOrd, _v]                  (list $ pair k_ v_) mapKv,
    prim3     _maps_insert          Maps.insert            [_kOrd, _v]                  k_ v_ mapKv mapKv,
    prim1     _maps_keys            Maps.keys              [_kOrd, _v]                  mapKv (list k_),
    prim2     _maps_lookup          Maps.lookup            [_kOrd, _v]                  k_ mapKv (optional v_),
    prim2     _maps_map             Maps.map               [_v1, _v2, _kOrd]            (fun v1_ v2_) (Prims.map k_ v1_) (Prims.map k_ v2_),
    prim2     _maps_mapKeys         Maps.mapKeys           [_k1Ord, _k2Ord, _v]         (fun k1_ k2_) (Prims.map k1_ v_) (Prims.map k2_ v_),
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
  prim1 _math_round         Math.round         [] float64 bigint,
  prim2 _math_roundBigfloat Math.roundBigfloat [] int32 bigfloat bigfloat,
  prim2 _math_roundFloat32  Math.roundFloat32  [] int32 float32 float32,
  prim2 _math_roundFloat64  Math.roundFloat64  [] int32 float64 float64,
  prim1 _math_sin            Math.sin           [] float64 float64,
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
  prim2 _math_max    Math.max    [] int32 int32 int32,
  prim2 _math_min    Math.min    [] int32 int32 int32,
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
    prim2     _maybes_apply     Maybes.apply        [_x, _y]     (optional $ fun x_ y_) (optional x_) (optional y_),
    prim2     _maybes_bind      Maybes.bind         [_x, _y]     (optional x_) (fun x_ (optional y_)) (optional y_),
    prim3     _maybes_cases     Maybes.cases        [_x, _y]     (optional x_) y_ (fun x_ y_) y_,
    prim1     _maybes_cat       Maybes.cat          [_x]         (list $ optional x_) (list x_),
    prim3     _maybes_compose   Maybes.compose      [_x, _y, _z] (fun x_ $ optional y_) (fun y_ $ optional z_) x_ (optional z_),
    prim1     _maybes_fromJust  Maybes.fromJust     [_x]         (optional x_) x_,
    prim2     _maybes_fromMaybe Maybes.fromMaybe    [_x]         x_ (optional x_) x_,
    prim1     _maybes_isJust    Maybes.isJust       [_x]         (optional x_) boolean,
    prim1     _maybes_isNothing Maybes.isNothing    [_x]         (optional x_) boolean,
    prim2     _maybes_map       Maybes.map          [_x, _y]     (fun x_ y_) (optional x_) (optional y_),
    prim2     _maybes_mapMaybe  Maybes.mapMaybe     [_x, _y]     (fun x_ $ optional y_) (list x_) (list y_),
    prim3     _maybes_maybe     Maybes.maybe        [_y, _x]     y_ (fun x_ y_) (optional x_) y_,
    prim1     _maybes_pure      Maybes.pure         [_x]         x_ (optional x_),
    prim1     _maybes_toList    Maybes.toList       [_x]         (optional x_) (list x_)]

hydraLibPairs :: Library
hydraLibPairs = standardLibrary _hydra_lib_pairs [
    prim3     _pairs_bimap  Pairs.bimap      [_a, _b, _c, _d] (fun a_ c_) (fun b_ d_) (pair a_ b_) (pair c_ d_),
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
    prim2     _sets_map          Sets.map          [_xOrd, _yOrd] (fun x_ y_) (set x_) (set y_),
    prim2     _sets_member       Sets.member       [_xOrd]        x_ (set x_) boolean,
    prim1     _sets_null         Sets.null         [_xOrd]        (set x_) boolean,
    prim1     _sets_singleton    Sets.singleton    [_xOrd]        x_ (set x_),
    prim1     _sets_size         Sets.size         [_xOrd]        (set x_) int32,
    prim1     _sets_toList       Sets.toList       [_xOrd]        (set x_) (list x_),
    prim2     _sets_union        Sets.union        [_xOrd]        (set x_) (set x_) (set x_),
    prim1     _sets_unions       Sets.unions       [_xOrd]        (list $ set x_) (set x_)]

hydraLibRegex :: Library
hydraLibRegex = standardLibrary _hydra_lib_regex [
  prim2 _regex_find       Regex.find       [] string string (optional string),
  prim2 _regex_findAll    Regex.findAll    [] string string (list string),
  prim2 _regex_matches    Regex.matches    [] string string boolean,
  prim3 _regex_replace    Regex.replace    [] string string string string,
  prim3 _regex_replaceAll Regex.replaceAll [] string string string string,
  prim2 _regex_split      Regex.split      [] string string (list string)]

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
