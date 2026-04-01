-- | Namespaces and primitive names for the Hydra standard library

module Hydra.Sources.Kernel.Lib.Names where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (qname)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: Namespace
ns = Namespace "hydra.lib.names"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces $
    Just "Namespaces and primitive names for the Hydra standard library"
  where
    elements = [
      -- Namespace constants
      toDefinition chars,
      toDefinition eithers,
      toDefinition equality,
      toDefinition lists,
      toDefinition literals,
      toDefinition logic,
      toDefinition maps,
      toDefinition math,
      toDefinition maybes,
      toDefinition pairs,
      toDefinition regex,
      toDefinition sets,
      toDefinition strings,
      toDefinition typeclass,

      -- chars primitives
      toDefinition charsIsAlphaNum,
      toDefinition charsIsLower,
      toDefinition charsIsSpace,
      toDefinition charsIsUpper,
      toDefinition charsToLower,
      toDefinition charsToUpper,

      -- eithers primitives
      toDefinition eithersBind,
      toDefinition eithersBimap,
      toDefinition eithersEither,
      toDefinition eithersFoldl,
      toDefinition eithersFromLeft,
      toDefinition eithersFromRight,
      toDefinition eithersIsLeft,
      toDefinition eithersIsRight,
      toDefinition eithersLefts,
      toDefinition eithersMap,
      toDefinition eithersMapList,
      toDefinition eithersMapMaybe,
      toDefinition eithersMapSet,
      toDefinition eithersPartitionEithers,
      toDefinition eithersRights,

      -- equality primitives
      toDefinition equalityCompare,
      toDefinition equalityEqual,
      toDefinition equalityGt,
      toDefinition equalityGte,
      toDefinition equalityIdentity,
      toDefinition equalityLt,
      toDefinition equalityLte,
      toDefinition equalityMax,
      toDefinition equalityMin,

      -- lists primitives
      toDefinition listsApply,
      toDefinition listsAt,
      toDefinition listsBind,
      toDefinition listsConcat,
      toDefinition listsConcat2,
      toDefinition listsCons,
      toDefinition listsDrop,
      toDefinition listsDropWhile,
      toDefinition listsElem,
      toDefinition listsFilter,
      toDefinition listsFind,
      toDefinition listsFoldl,
      toDefinition listsFoldr,
      toDefinition listsGroup,
      toDefinition listsHead,
      toDefinition listsInit,
      toDefinition listsIntercalate,
      toDefinition listsIntersperse,
      toDefinition listsLast,
      toDefinition listsLength,
      toDefinition listsMap,
      toDefinition listsNub,
      toDefinition listsNull,
      toDefinition listsPartition,
      toDefinition listsPure,
      toDefinition listsReplicate,
      toDefinition listsReverse,
      toDefinition listsSafeHead,
      toDefinition listsSingleton,
      toDefinition listsSort,
      toDefinition listsSortOn,
      toDefinition listsSpan,
      toDefinition listsTail,
      toDefinition listsTake,
      toDefinition listsTranspose,
      toDefinition listsZip,
      toDefinition listsZipWith,

      -- literals primitives
      toDefinition literalsBigfloatToBigint,
      toDefinition literalsBigfloatToFloat32,
      toDefinition literalsBigfloatToFloat64,
      toDefinition literalsBigintToBigfloat,
      toDefinition literalsBigintToInt8,
      toDefinition literalsBigintToInt16,
      toDefinition literalsBigintToInt32,
      toDefinition literalsBigintToInt64,
      toDefinition literalsBigintToUint8,
      toDefinition literalsBigintToUint16,
      toDefinition literalsBigintToUint32,
      toDefinition literalsBigintToUint64,
      toDefinition literalsBinaryToBytes,
      toDefinition literalsBinaryToString,
      toDefinition literalsFloat32ToBigfloat,
      toDefinition literalsFloat64ToBigfloat,
      toDefinition literalsInt8ToBigint,
      toDefinition literalsInt16ToBigint,
      toDefinition literalsInt32ToBigint,
      toDefinition literalsInt64ToBigint,
      toDefinition literalsReadBigfloat,
      toDefinition literalsReadBigint,
      toDefinition literalsReadBoolean,
      toDefinition literalsReadFloat32,
      toDefinition literalsReadFloat64,
      toDefinition literalsReadInt8,
      toDefinition literalsReadInt16,
      toDefinition literalsReadInt32,
      toDefinition literalsReadInt64,
      toDefinition literalsReadString,
      toDefinition literalsReadUint8,
      toDefinition literalsReadUint16,
      toDefinition literalsReadUint32,
      toDefinition literalsReadUint64,
      toDefinition literalsShowBigfloat,
      toDefinition literalsShowBigint,
      toDefinition literalsShowBoolean,
      toDefinition literalsShowFloat32,
      toDefinition literalsShowFloat64,
      toDefinition literalsShowInt8,
      toDefinition literalsShowInt16,
      toDefinition literalsShowInt32,
      toDefinition literalsShowInt64,
      toDefinition literalsShowUint8,
      toDefinition literalsShowUint16,
      toDefinition literalsShowUint32,
      toDefinition literalsShowUint64,
      toDefinition literalsShowString,
      toDefinition literalsStringToBinary,
      toDefinition literalsUint8ToBigint,
      toDefinition literalsUint16ToBigint,
      toDefinition literalsUint32ToBigint,
      toDefinition literalsUint64ToBigint,

      -- logic primitives
      toDefinition logicAnd,
      toDefinition logicIfElse,
      toDefinition logicNot,
      toDefinition logicOr,

      -- maps primitives
      toDefinition mapsAlter,
      toDefinition mapsBimap,
      toDefinition mapsDelete,
      toDefinition mapsElems,
      toDefinition mapsEmpty,
      toDefinition mapsFilter,
      toDefinition mapsFilterWithKey,
      toDefinition mapsFindWithDefault,
      toDefinition mapsFromList,
      toDefinition mapsInsert,
      toDefinition mapsKeys,
      toDefinition mapsLookup,
      toDefinition mapsMap,
      toDefinition mapsMapKeys,
      toDefinition mapsMember,
      toDefinition mapsNull,
      toDefinition mapsSingleton,
      toDefinition mapsSize,
      toDefinition mapsToList,
      toDefinition mapsUnion,

      -- math primitives
      toDefinition mathAbs,
      toDefinition mathAcos,
      toDefinition mathAcosh,
      toDefinition mathAdd,
      toDefinition mathAsin,
      toDefinition mathAsinh,
      toDefinition mathAtan,
      toDefinition mathAtan2,
      toDefinition mathAtanh,
      toDefinition mathCeiling,
      toDefinition mathCos,
      toDefinition mathCosh,
      toDefinition mathDiv,
      toDefinition mathE,
      toDefinition mathEven,
      toDefinition mathExp,
      toDefinition mathFloor,
      toDefinition mathLog,
      toDefinition mathLogBase,
      toDefinition mathMax,
      toDefinition mathMin,
      toDefinition mathMod,
      toDefinition mathMul,
      toDefinition mathNegate,
      toDefinition mathOdd,
      toDefinition mathPi,
      toDefinition mathPow,
      toDefinition mathPred,
      toDefinition mathRange,
      toDefinition mathRem,
      toDefinition mathRound,
      toDefinition mathRoundBigfloat,
      toDefinition mathRoundFloat32,
      toDefinition mathRoundFloat64,
      toDefinition mathSignum,
      toDefinition mathSin,
      toDefinition mathSinh,
      toDefinition mathSqrt,
      toDefinition mathSub,
      toDefinition mathSucc,
      toDefinition mathTan,
      toDefinition mathTanh,
      toDefinition mathTruncate,

      -- maybes primitives
      toDefinition maybesApply,
      toDefinition maybesBind,
      toDefinition maybesCases,
      toDefinition maybesCat,
      toDefinition maybesCompose,
      toDefinition maybesFromJust,
      toDefinition maybesFromMaybe,
      toDefinition maybesIsJust,
      toDefinition maybesIsNothing,
      toDefinition maybesMap,
      toDefinition maybesMapMaybe,
      toDefinition maybesMaybe,
      toDefinition maybesPure,
      toDefinition maybesToList,

      -- pairs primitives
      toDefinition pairsBimap,
      toDefinition pairsFirst,
      toDefinition pairsSecond,

      -- sets primitives
      toDefinition setsDelete,
      toDefinition setsDifference,
      toDefinition setsEmpty,
      toDefinition setsFromList,
      toDefinition setsInsert,
      toDefinition setsIntersection,
      toDefinition setsMap,
      toDefinition setsMember,
      toDefinition setsNull,
      toDefinition setsSingleton,
      toDefinition setsSize,
      toDefinition setsToList,
      toDefinition setsUnion,
      toDefinition setsUnions,

      -- regex primitives
      toDefinition regexFind,
      toDefinition regexFindAll,
      toDefinition regexMatches,
      toDefinition regexReplace,
      toDefinition regexReplaceAll,
      toDefinition regexSplit,

      -- strings primitives
      toDefinition stringsCat,
      toDefinition stringsCat2,
      toDefinition stringsCharAt,
      toDefinition stringsFromList,
      toDefinition stringsIntercalate,
      toDefinition stringsNull,
      toDefinition stringsLength,
      toDefinition stringsLines,
      toDefinition stringsSplitOn,
      toDefinition stringsToList,
      toDefinition stringsToLower,
      toDefinition stringsToUpper,
      toDefinition stringsUnlines,

      -- typeclass names
      toDefinition typeclassEq,
      toDefinition typeclassOrd]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- | Helper: define a namespace constant
defineNs :: String -> String -> TTermDefinition Namespace
defineNs name nsStr = define name $
  wrap _Namespace $ string nsStr

-- | Helper: define a primitive name constant as a simple Name data constructor
defineName :: String -> String -> String -> TTermDefinition Name
defineName name nsStr localName = define name $
  wrap _Name $ string (nsStr <> "." <> localName)

-- Namespace constants

chars :: TTermDefinition Namespace
chars = defineNs "chars" "hydra.lib.chars"

eithers :: TTermDefinition Namespace
eithers = defineNs "eithers" "hydra.lib.eithers"

equality :: TTermDefinition Namespace
equality = defineNs "equality" "hydra.lib.equality"

lists :: TTermDefinition Namespace
lists = defineNs "lists" "hydra.lib.lists"

literals :: TTermDefinition Namespace
literals = defineNs "literals" "hydra.lib.literals"

logic :: TTermDefinition Namespace
logic = defineNs "logic" "hydra.lib.logic"

maps :: TTermDefinition Namespace
maps = defineNs "maps" "hydra.lib.maps"

math :: TTermDefinition Namespace
math = defineNs "math" "hydra.lib.math"

maybes :: TTermDefinition Namespace
maybes = defineNs "maybes" "hydra.lib.maybes"

pairs :: TTermDefinition Namespace
pairs = defineNs "pairs" "hydra.lib.pairs"

regex :: TTermDefinition Namespace
regex = defineNs "regex" "hydra.lib.regex"

sets :: TTermDefinition Namespace
sets = defineNs "sets" "hydra.lib.sets"

strings :: TTermDefinition Namespace
strings = defineNs "strings" "hydra.lib.strings"

typeclass :: TTermDefinition Namespace
typeclass = defineNs "typeclass" "hydra.typeclass"

-- Chars primitives

charsIsAlphaNum = defineName "charsIsAlphaNum" "hydra.lib.chars" "isAlphaNum"
charsIsLower    = defineName "charsIsLower" "hydra.lib.chars" "isLower"
charsIsSpace    = defineName "charsIsSpace" "hydra.lib.chars" "isSpace"
charsIsUpper    = defineName "charsIsUpper" "hydra.lib.chars" "isUpper"
charsToLower    = defineName "charsToLower" "hydra.lib.chars" "toLower"
charsToUpper    = defineName "charsToUpper" "hydra.lib.chars" "toUpper"

-- Eithers primitives

eithersBind             = defineName "eithersBind" "hydra.lib.eithers" "bind"
eithersBimap            = defineName "eithersBimap" "hydra.lib.eithers" "bimap"
eithersEither           = defineName "eithersEither" "hydra.lib.eithers" "either"
eithersFoldl            = defineName "eithersFoldl" "hydra.lib.eithers" "foldl"
eithersFromLeft         = defineName "eithersFromLeft" "hydra.lib.eithers" "fromLeft"
eithersFromRight        = defineName "eithersFromRight" "hydra.lib.eithers" "fromRight"
eithersIsLeft           = defineName "eithersIsLeft" "hydra.lib.eithers" "isLeft"
eithersIsRight          = defineName "eithersIsRight" "hydra.lib.eithers" "isRight"
eithersLefts            = defineName "eithersLefts" "hydra.lib.eithers" "lefts"
eithersMap              = defineName "eithersMap" "hydra.lib.eithers" "map"
eithersMapList          = defineName "eithersMapList" "hydra.lib.eithers" "mapList"
eithersMapMaybe         = defineName "eithersMapMaybe" "hydra.lib.eithers" "mapMaybe"
eithersMapSet           = defineName "eithersMapSet" "hydra.lib.eithers" "mapSet"
eithersPartitionEithers = defineName "eithersPartitionEithers" "hydra.lib.eithers" "partitionEithers"
eithersRights           = defineName "eithersRights" "hydra.lib.eithers" "rights"

-- Equality primitives

equalityCompare  = defineName "equalityCompare" "hydra.lib.equality" "compare"
equalityEqual    = defineName "equalityEqual" "hydra.lib.equality" "equal"
equalityGt       = defineName "equalityGt" "hydra.lib.equality" "gt"
equalityGte      = defineName "equalityGte" "hydra.lib.equality" "gte"
equalityIdentity = defineName "equalityIdentity" "hydra.lib.equality" "identity"
equalityLt       = defineName "equalityLt" "hydra.lib.equality" "lt"
equalityLte      = defineName "equalityLte" "hydra.lib.equality" "lte"
equalityMax      = defineName "equalityMax" "hydra.lib.equality" "max"
equalityMin      = defineName "equalityMin" "hydra.lib.equality" "min"

-- Lists primitives

listsApply       = defineName "listsApply" "hydra.lib.lists" "apply"
listsAt          = defineName "listsAt" "hydra.lib.lists" "at"
listsBind        = defineName "listsBind" "hydra.lib.lists" "bind"
listsConcat      = defineName "listsConcat" "hydra.lib.lists" "concat"
listsConcat2     = defineName "listsConcat2" "hydra.lib.lists" "concat2"
listsCons        = defineName "listsCons" "hydra.lib.lists" "cons"
listsDrop        = defineName "listsDrop" "hydra.lib.lists" "drop"
listsDropWhile   = defineName "listsDropWhile" "hydra.lib.lists" "dropWhile"
listsElem        = defineName "listsElem" "hydra.lib.lists" "elem"
listsFilter      = defineName "listsFilter" "hydra.lib.lists" "filter"
listsFind        = defineName "listsFind" "hydra.lib.lists" "find"
listsFoldl       = defineName "listsFoldl" "hydra.lib.lists" "foldl"
listsFoldr       = defineName "listsFoldr" "hydra.lib.lists" "foldr"
listsGroup       = defineName "listsGroup" "hydra.lib.lists" "group"
listsHead        = defineName "listsHead" "hydra.lib.lists" "head"
listsInit        = defineName "listsInit" "hydra.lib.lists" "init"
listsIntercalate = defineName "listsIntercalate" "hydra.lib.lists" "intercalate"
listsIntersperse = defineName "listsIntersperse" "hydra.lib.lists" "intersperse"
listsLast        = defineName "listsLast" "hydra.lib.lists" "last"
listsLength      = defineName "listsLength" "hydra.lib.lists" "length"
listsMap         = defineName "listsMap" "hydra.lib.lists" "map"
listsNub         = defineName "listsNub" "hydra.lib.lists" "nub"
listsNull        = defineName "listsNull" "hydra.lib.lists" "null"
listsPartition   = defineName "listsPartition" "hydra.lib.lists" "partition"
listsPure        = defineName "listsPure" "hydra.lib.lists" "pure"
listsReplicate   = defineName "listsReplicate" "hydra.lib.lists" "replicate"
listsReverse     = defineName "listsReverse" "hydra.lib.lists" "reverse"
listsSafeHead    = defineName "listsSafeHead" "hydra.lib.lists" "safeHead"
listsSingleton   = defineName "listsSingleton" "hydra.lib.lists" "singleton"
listsSort        = defineName "listsSort" "hydra.lib.lists" "sort"
listsSortOn      = defineName "listsSortOn" "hydra.lib.lists" "sortOn"
listsSpan        = defineName "listsSpan" "hydra.lib.lists" "span"
listsTail        = defineName "listsTail" "hydra.lib.lists" "tail"
listsTake        = defineName "listsTake" "hydra.lib.lists" "take"
listsTranspose   = defineName "listsTranspose" "hydra.lib.lists" "transpose"
listsZip         = defineName "listsZip" "hydra.lib.lists" "zip"
listsZipWith     = defineName "listsZipWith" "hydra.lib.lists" "zipWith"

-- Literals primitives

literalsBigfloatToBigint  = defineName "literalsBigfloatToBigint" "hydra.lib.literals" "bigfloatToBigint"
literalsBigfloatToFloat32 = defineName "literalsBigfloatToFloat32" "hydra.lib.literals" "bigfloatToFloat32"
literalsBigfloatToFloat64 = defineName "literalsBigfloatToFloat64" "hydra.lib.literals" "bigfloatToFloat64"
literalsBigintToBigfloat  = defineName "literalsBigintToBigfloat" "hydra.lib.literals" "bigintToBigfloat"
literalsBigintToInt8      = defineName "literalsBigintToInt8" "hydra.lib.literals" "bigintToInt8"
literalsBigintToInt16     = defineName "literalsBigintToInt16" "hydra.lib.literals" "bigintToInt16"
literalsBigintToInt32     = defineName "literalsBigintToInt32" "hydra.lib.literals" "bigintToInt32"
literalsBigintToInt64     = defineName "literalsBigintToInt64" "hydra.lib.literals" "bigintToInt64"
literalsBigintToUint8     = defineName "literalsBigintToUint8" "hydra.lib.literals" "bigintToUint8"
literalsBigintToUint16    = defineName "literalsBigintToUint16" "hydra.lib.literals" "bigintToUint16"
literalsBigintToUint32    = defineName "literalsBigintToUint32" "hydra.lib.literals" "bigintToUint32"
literalsBigintToUint64    = defineName "literalsBigintToUint64" "hydra.lib.literals" "bigintToUint64"
literalsBinaryToBytes     = defineName "literalsBinaryToBytes" "hydra.lib.literals" "binaryToBytes"
literalsBinaryToString    = defineName "literalsBinaryToString" "hydra.lib.literals" "binaryToString"
literalsFloat32ToBigfloat = defineName "literalsFloat32ToBigfloat" "hydra.lib.literals" "float32ToBigfloat"
literalsFloat64ToBigfloat = defineName "literalsFloat64ToBigfloat" "hydra.lib.literals" "float64ToBigfloat"
literalsInt8ToBigint      = defineName "literalsInt8ToBigint" "hydra.lib.literals" "int8ToBigint"
literalsInt16ToBigint     = defineName "literalsInt16ToBigint" "hydra.lib.literals" "int16ToBigint"
literalsInt32ToBigint     = defineName "literalsInt32ToBigint" "hydra.lib.literals" "int32ToBigint"
literalsInt64ToBigint     = defineName "literalsInt64ToBigint" "hydra.lib.literals" "int64ToBigint"
literalsReadBigfloat      = defineName "literalsReadBigfloat" "hydra.lib.literals" "readBigfloat"
literalsReadBigint        = defineName "literalsReadBigint" "hydra.lib.literals" "readBigint"
literalsReadBoolean       = defineName "literalsReadBoolean" "hydra.lib.literals" "readBoolean"
literalsReadFloat32       = defineName "literalsReadFloat32" "hydra.lib.literals" "readFloat32"
literalsReadFloat64       = defineName "literalsReadFloat64" "hydra.lib.literals" "readFloat64"
literalsReadInt8          = defineName "literalsReadInt8" "hydra.lib.literals" "readInt8"
literalsReadInt16         = defineName "literalsReadInt16" "hydra.lib.literals" "readInt16"
literalsReadInt32         = defineName "literalsReadInt32" "hydra.lib.literals" "readInt32"
literalsReadInt64         = defineName "literalsReadInt64" "hydra.lib.literals" "readInt64"
literalsReadString        = defineName "literalsReadString" "hydra.lib.literals" "readString"
literalsReadUint8         = defineName "literalsReadUint8" "hydra.lib.literals" "readUint8"
literalsReadUint16        = defineName "literalsReadUint16" "hydra.lib.literals" "readUint16"
literalsReadUint32        = defineName "literalsReadUint32" "hydra.lib.literals" "readUint32"
literalsReadUint64        = defineName "literalsReadUint64" "hydra.lib.literals" "readUint64"
literalsShowBigfloat      = defineName "literalsShowBigfloat" "hydra.lib.literals" "showBigfloat"
literalsShowBigint        = defineName "literalsShowBigint" "hydra.lib.literals" "showBigint"
literalsShowBoolean       = defineName "literalsShowBoolean" "hydra.lib.literals" "showBoolean"
literalsShowFloat32       = defineName "literalsShowFloat32" "hydra.lib.literals" "showFloat32"
literalsShowFloat64       = defineName "literalsShowFloat64" "hydra.lib.literals" "showFloat64"
literalsShowInt8          = defineName "literalsShowInt8" "hydra.lib.literals" "showInt8"
literalsShowInt16         = defineName "literalsShowInt16" "hydra.lib.literals" "showInt16"
literalsShowInt32         = defineName "literalsShowInt32" "hydra.lib.literals" "showInt32"
literalsShowInt64         = defineName "literalsShowInt64" "hydra.lib.literals" "showInt64"
literalsShowUint8         = defineName "literalsShowUint8" "hydra.lib.literals" "showUint8"
literalsShowUint16        = defineName "literalsShowUint16" "hydra.lib.literals" "showUint16"
literalsShowUint32        = defineName "literalsShowUint32" "hydra.lib.literals" "showUint32"
literalsShowUint64        = defineName "literalsShowUint64" "hydra.lib.literals" "showUint64"
literalsShowString        = defineName "literalsShowString" "hydra.lib.literals" "showString"
literalsStringToBinary    = defineName "literalsStringToBinary" "hydra.lib.literals" "stringToBinary"
literalsUint8ToBigint     = defineName "literalsUint8ToBigint" "hydra.lib.literals" "uint8ToBigint"
literalsUint16ToBigint    = defineName "literalsUint16ToBigint" "hydra.lib.literals" "uint16ToBigint"
literalsUint32ToBigint    = defineName "literalsUint32ToBigint" "hydra.lib.literals" "uint32ToBigint"
literalsUint64ToBigint    = defineName "literalsUint64ToBigint" "hydra.lib.literals" "uint64ToBigint"

-- Logic primitives

logicAnd    = defineName "logicAnd" "hydra.lib.logic" "and"
logicIfElse = defineName "logicIfElse" "hydra.lib.logic" "ifElse"
logicNot    = defineName "logicNot" "hydra.lib.logic" "not"
logicOr     = defineName "logicOr" "hydra.lib.logic" "or"

-- Maps primitives

mapsAlter           = defineName "mapsAlter" "hydra.lib.maps" "alter"
mapsBimap           = defineName "mapsBimap" "hydra.lib.maps" "bimap"
mapsDelete          = defineName "mapsDelete" "hydra.lib.maps" "delete"
mapsElems           = defineName "mapsElems" "hydra.lib.maps" "elems"
mapsEmpty           = defineName "mapsEmpty" "hydra.lib.maps" "empty"
mapsFilter          = defineName "mapsFilter" "hydra.lib.maps" "filter"
mapsFilterWithKey   = defineName "mapsFilterWithKey" "hydra.lib.maps" "filterWithKey"
mapsFindWithDefault = defineName "mapsFindWithDefault" "hydra.lib.maps" "findWithDefault"
mapsFromList        = defineName "mapsFromList" "hydra.lib.maps" "fromList"
mapsInsert          = defineName "mapsInsert" "hydra.lib.maps" "insert"
mapsKeys            = defineName "mapsKeys" "hydra.lib.maps" "keys"
mapsLookup          = defineName "mapsLookup" "hydra.lib.maps" "lookup"
mapsMap             = defineName "mapsMap" "hydra.lib.maps" "map"
mapsMapKeys         = defineName "mapsMapKeys" "hydra.lib.maps" "mapKeys"
mapsMember          = defineName "mapsMember" "hydra.lib.maps" "member"
mapsNull            = defineName "mapsNull" "hydra.lib.maps" "null"
mapsSingleton       = defineName "mapsSingleton" "hydra.lib.maps" "singleton"
mapsSize            = defineName "mapsSize" "hydra.lib.maps" "size"
mapsToList          = defineName "mapsToList" "hydra.lib.maps" "toList"
mapsUnion           = defineName "mapsUnion" "hydra.lib.maps" "union"

-- Math primitives

mathAbs      = defineName "mathAbs" "hydra.lib.math" "abs"
mathAcos     = defineName "mathAcos" "hydra.lib.math" "acos"
mathAcosh    = defineName "mathAcosh" "hydra.lib.math" "acosh"
mathAdd      = defineName "mathAdd" "hydra.lib.math" "add"
mathAsin     = defineName "mathAsin" "hydra.lib.math" "asin"
mathAsinh    = defineName "mathAsinh" "hydra.lib.math" "asinh"
mathAtan     = defineName "mathAtan" "hydra.lib.math" "atan"
mathAtan2    = defineName "mathAtan2" "hydra.lib.math" "atan2"
mathAtanh    = defineName "mathAtanh" "hydra.lib.math" "atanh"
mathCeiling  = defineName "mathCeiling" "hydra.lib.math" "ceiling"
mathCos      = defineName "mathCos" "hydra.lib.math" "cos"
mathCosh     = defineName "mathCosh" "hydra.lib.math" "cosh"
mathDiv      = defineName "mathDiv" "hydra.lib.math" "div"
mathE        = defineName "mathE" "hydra.lib.math" "e"
mathEven     = defineName "mathEven" "hydra.lib.math" "even"
mathExp      = defineName "mathExp" "hydra.lib.math" "exp"
mathFloor    = defineName "mathFloor" "hydra.lib.math" "floor"
mathLog      = defineName "mathLog" "hydra.lib.math" "log"
mathLogBase  = defineName "mathLogBase" "hydra.lib.math" "logBase"
mathMax      = defineName "mathMax" "hydra.lib.math" "max"
mathMin      = defineName "mathMin" "hydra.lib.math" "min"
mathMod      = defineName "mathMod" "hydra.lib.math" "mod"
mathMul      = defineName "mathMul" "hydra.lib.math" "mul"
mathNegate   = defineName "mathNegate" "hydra.lib.math" "negate"
mathOdd      = defineName "mathOdd" "hydra.lib.math" "odd"
mathPi       = defineName "mathPi" "hydra.lib.math" "pi"
mathPow      = defineName "mathPow" "hydra.lib.math" "pow"
mathPred     = defineName "mathPred" "hydra.lib.math" "pred"
mathRange    = defineName "mathRange" "hydra.lib.math" "range"
mathRem          = defineName "mathRem" "hydra.lib.math" "rem"
mathRound        = defineName "mathRound" "hydra.lib.math" "round"
mathRoundBigfloat = defineName "mathRoundBigfloat" "hydra.lib.math" "roundBigfloat"
mathRoundFloat32 = defineName "mathRoundFloat32" "hydra.lib.math" "roundFloat32"
mathRoundFloat64 = defineName "mathRoundFloat64" "hydra.lib.math" "roundFloat64"
mathSignum       = defineName "mathSignum" "hydra.lib.math" "signum"
mathSin      = defineName "mathSin" "hydra.lib.math" "sin"
mathSinh     = defineName "mathSinh" "hydra.lib.math" "sinh"
mathSqrt     = defineName "mathSqrt" "hydra.lib.math" "sqrt"
mathSub      = defineName "mathSub" "hydra.lib.math" "sub"
mathSucc     = defineName "mathSucc" "hydra.lib.math" "succ"
mathTan      = defineName "mathTan" "hydra.lib.math" "tan"
mathTanh     = defineName "mathTanh" "hydra.lib.math" "tanh"
mathTruncate = defineName "mathTruncate" "hydra.lib.math" "truncate"

-- Maybes primitives

maybesApply     = defineName "maybesApply" "hydra.lib.maybes" "apply"
maybesBind      = defineName "maybesBind" "hydra.lib.maybes" "bind"
maybesCases     = defineName "maybesCases" "hydra.lib.maybes" "cases"
maybesCat       = defineName "maybesCat" "hydra.lib.maybes" "cat"
maybesCompose   = defineName "maybesCompose" "hydra.lib.maybes" "compose"
maybesFromJust  = defineName "maybesFromJust" "hydra.lib.maybes" "fromJust"
maybesFromMaybe = defineName "maybesFromMaybe" "hydra.lib.maybes" "fromMaybe"
maybesIsJust    = defineName "maybesIsJust" "hydra.lib.maybes" "isJust"
maybesIsNothing = defineName "maybesIsNothing" "hydra.lib.maybes" "isNothing"
maybesMap       = defineName "maybesMap" "hydra.lib.maybes" "map"
maybesMapMaybe  = defineName "maybesMapMaybe" "hydra.lib.maybes" "mapMaybe"
maybesMaybe     = defineName "maybesMaybe" "hydra.lib.maybes" "maybe"
maybesPure      = defineName "maybesPure" "hydra.lib.maybes" "pure"
maybesToList    = defineName "maybesToList" "hydra.lib.maybes" "toList"

-- Pairs primitives

pairsBimap  = defineName "pairsBimap" "hydra.lib.pairs" "bimap"
pairsFirst  = defineName "pairsFirst" "hydra.lib.pairs" "first"
pairsSecond = defineName "pairsSecond" "hydra.lib.pairs" "second"

-- Sets primitives

setsDelete       = defineName "setsDelete" "hydra.lib.sets" "delete"
setsDifference   = defineName "setsDifference" "hydra.lib.sets" "difference"
setsEmpty        = defineName "setsEmpty" "hydra.lib.sets" "empty"
setsFromList     = defineName "setsFromList" "hydra.lib.sets" "fromList"
setsInsert       = defineName "setsInsert" "hydra.lib.sets" "insert"
setsIntersection = defineName "setsIntersection" "hydra.lib.sets" "intersection"
setsMap          = defineName "setsMap" "hydra.lib.sets" "map"
setsMember       = defineName "setsMember" "hydra.lib.sets" "member"
setsNull         = defineName "setsNull" "hydra.lib.sets" "null"
setsSingleton    = defineName "setsSingleton" "hydra.lib.sets" "singleton"
setsSize         = defineName "setsSize" "hydra.lib.sets" "size"
setsToList       = defineName "setsToList" "hydra.lib.sets" "toList"
setsUnion        = defineName "setsUnion" "hydra.lib.sets" "union"
setsUnions       = defineName "setsUnions" "hydra.lib.sets" "unions"

-- Regex primitives

regexFind       = defineName "regexFind" "hydra.lib.regex" "find"
regexFindAll    = defineName "regexFindAll" "hydra.lib.regex" "findAll"
regexMatches    = defineName "regexMatches" "hydra.lib.regex" "matches"
regexReplace    = defineName "regexReplace" "hydra.lib.regex" "replace"
regexReplaceAll = defineName "regexReplaceAll" "hydra.lib.regex" "replaceAll"
regexSplit      = defineName "regexSplit" "hydra.lib.regex" "split"

-- Strings primitives

stringsCat         = defineName "stringsCat" "hydra.lib.strings" "cat"
stringsCat2        = defineName "stringsCat2" "hydra.lib.strings" "cat2"
stringsCharAt      = defineName "stringsCharAt" "hydra.lib.strings" "charAt"
stringsFromList    = defineName "stringsFromList" "hydra.lib.strings" "fromList"
stringsIntercalate = defineName "stringsIntercalate" "hydra.lib.strings" "intercalate"
stringsNull        = defineName "stringsNull" "hydra.lib.strings" "null"
stringsLength      = defineName "stringsLength" "hydra.lib.strings" "length"
stringsLines       = defineName "stringsLines" "hydra.lib.strings" "lines"
stringsSplitOn     = defineName "stringsSplitOn" "hydra.lib.strings" "splitOn"
stringsToList      = defineName "stringsToList" "hydra.lib.strings" "toList"
stringsToLower     = defineName "stringsToLower" "hydra.lib.strings" "toLower"
stringsToUpper     = defineName "stringsToUpper" "hydra.lib.strings" "toUpper"
stringsUnlines     = defineName "stringsUnlines" "hydra.lib.strings" "unlines"

-- Type class names

typeclassEq  = defineName "typeclassEq" "hydra.typeclass" "Eq"
typeclassOrd = defineName "typeclassOrd" "hydra.typeclass" "Ord"
