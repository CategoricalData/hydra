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
      toTermDefinition chars,
      toTermDefinition eithers,
      toTermDefinition equality,
      toTermDefinition lists,
      toTermDefinition literals,
      toTermDefinition logic,
      toTermDefinition maps,
      toTermDefinition math,
      toTermDefinition maybes,
      toTermDefinition pairs,
      toTermDefinition regex,
      toTermDefinition sets,
      toTermDefinition strings,
      toTermDefinition typeclass,

      -- chars primitives
      toTermDefinition charsIsAlphaNum,
      toTermDefinition charsIsLower,
      toTermDefinition charsIsSpace,
      toTermDefinition charsIsUpper,
      toTermDefinition charsToLower,
      toTermDefinition charsToUpper,

      -- eithers primitives
      toTermDefinition eithersBind,
      toTermDefinition eithersBimap,
      toTermDefinition eithersEither,
      toTermDefinition eithersFoldl,
      toTermDefinition eithersFromLeft,
      toTermDefinition eithersFromRight,
      toTermDefinition eithersIsLeft,
      toTermDefinition eithersIsRight,
      toTermDefinition eithersLefts,
      toTermDefinition eithersMap,
      toTermDefinition eithersMapList,
      toTermDefinition eithersMapMaybe,
      toTermDefinition eithersMapSet,
      toTermDefinition eithersPartitionEithers,
      toTermDefinition eithersRights,

      -- equality primitives
      toTermDefinition equalityCompare,
      toTermDefinition equalityEqual,
      toTermDefinition equalityGt,
      toTermDefinition equalityGte,
      toTermDefinition equalityIdentity,
      toTermDefinition equalityLt,
      toTermDefinition equalityLte,
      toTermDefinition equalityMax,
      toTermDefinition equalityMin,

      -- lists primitives
      toTermDefinition listsApply,
      toTermDefinition listsAt,
      toTermDefinition listsBind,
      toTermDefinition listsConcat,
      toTermDefinition listsConcat2,
      toTermDefinition listsCons,
      toTermDefinition listsDrop,
      toTermDefinition listsDropWhile,
      toTermDefinition listsElem,
      toTermDefinition listsFilter,
      toTermDefinition listsFind,
      toTermDefinition listsFoldl,
      toTermDefinition listsFoldr,
      toTermDefinition listsGroup,
      toTermDefinition listsHead,
      toTermDefinition listsInit,
      toTermDefinition listsIntercalate,
      toTermDefinition listsIntersperse,
      toTermDefinition listsLast,
      toTermDefinition listsLength,
      toTermDefinition listsMap,
      toTermDefinition listsNub,
      toTermDefinition listsNull,
      toTermDefinition listsPartition,
      toTermDefinition listsPure,
      toTermDefinition listsReplicate,
      toTermDefinition listsReverse,
      toTermDefinition listsSafeHead,
      toTermDefinition listsSingleton,
      toTermDefinition listsSort,
      toTermDefinition listsSortOn,
      toTermDefinition listsSpan,
      toTermDefinition listsTail,
      toTermDefinition listsTake,
      toTermDefinition listsTranspose,
      toTermDefinition listsZip,
      toTermDefinition listsZipWith,

      -- literals primitives
      toTermDefinition literalsBigfloatToBigint,
      toTermDefinition literalsBigfloatToFloat32,
      toTermDefinition literalsBigfloatToFloat64,
      toTermDefinition literalsBigintToBigfloat,
      toTermDefinition literalsBigintToInt8,
      toTermDefinition literalsBigintToInt16,
      toTermDefinition literalsBigintToInt32,
      toTermDefinition literalsBigintToInt64,
      toTermDefinition literalsBigintToUint8,
      toTermDefinition literalsBigintToUint16,
      toTermDefinition literalsBigintToUint32,
      toTermDefinition literalsBigintToUint64,
      toTermDefinition literalsBinaryToBytes,
      toTermDefinition literalsBinaryToString,
      toTermDefinition literalsFloat32ToBigfloat,
      toTermDefinition literalsFloat64ToBigfloat,
      toTermDefinition literalsInt8ToBigint,
      toTermDefinition literalsInt16ToBigint,
      toTermDefinition literalsInt32ToBigint,
      toTermDefinition literalsInt64ToBigint,
      toTermDefinition literalsReadBigfloat,
      toTermDefinition literalsReadBigint,
      toTermDefinition literalsReadBoolean,
      toTermDefinition literalsReadFloat32,
      toTermDefinition literalsReadFloat64,
      toTermDefinition literalsReadInt8,
      toTermDefinition literalsReadInt16,
      toTermDefinition literalsReadInt32,
      toTermDefinition literalsReadInt64,
      toTermDefinition literalsReadString,
      toTermDefinition literalsReadUint8,
      toTermDefinition literalsReadUint16,
      toTermDefinition literalsReadUint32,
      toTermDefinition literalsReadUint64,
      toTermDefinition literalsShowBigfloat,
      toTermDefinition literalsShowBigint,
      toTermDefinition literalsShowBoolean,
      toTermDefinition literalsShowFloat32,
      toTermDefinition literalsShowFloat64,
      toTermDefinition literalsShowInt8,
      toTermDefinition literalsShowInt16,
      toTermDefinition literalsShowInt32,
      toTermDefinition literalsShowInt64,
      toTermDefinition literalsShowUint8,
      toTermDefinition literalsShowUint16,
      toTermDefinition literalsShowUint32,
      toTermDefinition literalsShowUint64,
      toTermDefinition literalsShowString,
      toTermDefinition literalsStringToBinary,
      toTermDefinition literalsUint8ToBigint,
      toTermDefinition literalsUint16ToBigint,
      toTermDefinition literalsUint32ToBigint,
      toTermDefinition literalsUint64ToBigint,

      -- logic primitives
      toTermDefinition logicAnd,
      toTermDefinition logicIfElse,
      toTermDefinition logicNot,
      toTermDefinition logicOr,

      -- maps primitives
      toTermDefinition mapsAlter,
      toTermDefinition mapsBimap,
      toTermDefinition mapsDelete,
      toTermDefinition mapsElems,
      toTermDefinition mapsEmpty,
      toTermDefinition mapsFilter,
      toTermDefinition mapsFilterWithKey,
      toTermDefinition mapsFindWithDefault,
      toTermDefinition mapsFromList,
      toTermDefinition mapsInsert,
      toTermDefinition mapsKeys,
      toTermDefinition mapsLookup,
      toTermDefinition mapsMap,
      toTermDefinition mapsMapKeys,
      toTermDefinition mapsMember,
      toTermDefinition mapsNull,
      toTermDefinition mapsSingleton,
      toTermDefinition mapsSize,
      toTermDefinition mapsToList,
      toTermDefinition mapsUnion,

      -- math primitives
      toTermDefinition mathAbs,
      toTermDefinition mathAcos,
      toTermDefinition mathAcosh,
      toTermDefinition mathAdd,
      toTermDefinition mathAsin,
      toTermDefinition mathAsinh,
      toTermDefinition mathAtan,
      toTermDefinition mathAtan2,
      toTermDefinition mathAtanh,
      toTermDefinition mathCeiling,
      toTermDefinition mathCos,
      toTermDefinition mathCosh,
      toTermDefinition mathDiv,
      toTermDefinition mathE,
      toTermDefinition mathEven,
      toTermDefinition mathExp,
      toTermDefinition mathFloor,
      toTermDefinition mathLog,
      toTermDefinition mathLogBase,
      toTermDefinition mathMax,
      toTermDefinition mathMin,
      toTermDefinition mathMod,
      toTermDefinition mathMul,
      toTermDefinition mathNegate,
      toTermDefinition mathOdd,
      toTermDefinition mathPi,
      toTermDefinition mathPow,
      toTermDefinition mathPred,
      toTermDefinition mathRange,
      toTermDefinition mathRem,
      toTermDefinition mathRound,
      toTermDefinition mathRoundBigfloat,
      toTermDefinition mathRoundFloat32,
      toTermDefinition mathRoundFloat64,
      toTermDefinition mathSignum,
      toTermDefinition mathSin,
      toTermDefinition mathSinh,
      toTermDefinition mathSqrt,
      toTermDefinition mathSub,
      toTermDefinition mathSucc,
      toTermDefinition mathTan,
      toTermDefinition mathTanh,
      toTermDefinition mathTruncate,

      -- maybes primitives
      toTermDefinition maybesApply,
      toTermDefinition maybesBind,
      toTermDefinition maybesCases,
      toTermDefinition maybesCat,
      toTermDefinition maybesCompose,
      toTermDefinition maybesFromJust,
      toTermDefinition maybesFromMaybe,
      toTermDefinition maybesIsJust,
      toTermDefinition maybesIsNothing,
      toTermDefinition maybesMap,
      toTermDefinition maybesMapMaybe,
      toTermDefinition maybesMaybe,
      toTermDefinition maybesPure,
      toTermDefinition maybesToList,

      -- pairs primitives
      toTermDefinition pairsBimap,
      toTermDefinition pairsFirst,
      toTermDefinition pairsSecond,

      -- sets primitives
      toTermDefinition setsDelete,
      toTermDefinition setsDifference,
      toTermDefinition setsEmpty,
      toTermDefinition setsFromList,
      toTermDefinition setsInsert,
      toTermDefinition setsIntersection,
      toTermDefinition setsMap,
      toTermDefinition setsMember,
      toTermDefinition setsNull,
      toTermDefinition setsSingleton,
      toTermDefinition setsSize,
      toTermDefinition setsToList,
      toTermDefinition setsUnion,
      toTermDefinition setsUnions,

      -- regex primitives
      toTermDefinition regexFind,
      toTermDefinition regexFindAll,
      toTermDefinition regexMatches,
      toTermDefinition regexReplace,
      toTermDefinition regexReplaceAll,
      toTermDefinition regexSplit,

      -- strings primitives
      toTermDefinition stringsCat,
      toTermDefinition stringsCat2,
      toTermDefinition stringsCharAt,
      toTermDefinition stringsFromList,
      toTermDefinition stringsIntercalate,
      toTermDefinition stringsNull,
      toTermDefinition stringsLength,
      toTermDefinition stringsLines,
      toTermDefinition stringsSplitOn,
      toTermDefinition stringsToList,
      toTermDefinition stringsToLower,
      toTermDefinition stringsToUpper,
      toTermDefinition stringsUnlines,

      -- typeclass names
      toTermDefinition typeclassEq,
      toTermDefinition typeclassOrd]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- | Helper: define a namespace constant
defineNs :: String -> String -> TBinding Namespace
defineNs name nsStr = define name $
  wrap _Namespace $ string nsStr

-- | Helper: define a primitive name constant as a simple Name data constructor
defineName :: String -> String -> String -> TBinding Name
defineName name nsStr localName = define name $
  wrap _Name $ string (nsStr <> "." <> localName)

-- Namespace constants

chars :: TBinding Namespace
chars = defineNs "chars" "hydra.lib.chars"

eithers :: TBinding Namespace
eithers = defineNs "eithers" "hydra.lib.eithers"

equality :: TBinding Namespace
equality = defineNs "equality" "hydra.lib.equality"

lists :: TBinding Namespace
lists = defineNs "lists" "hydra.lib.lists"

literals :: TBinding Namespace
literals = defineNs "literals" "hydra.lib.literals"

logic :: TBinding Namespace
logic = defineNs "logic" "hydra.lib.logic"

maps :: TBinding Namespace
maps = defineNs "maps" "hydra.lib.maps"

math :: TBinding Namespace
math = defineNs "math" "hydra.lib.math"

maybes :: TBinding Namespace
maybes = defineNs "maybes" "hydra.lib.maybes"

pairs :: TBinding Namespace
pairs = defineNs "pairs" "hydra.lib.pairs"

regex :: TBinding Namespace
regex = defineNs "regex" "hydra.lib.regex"

sets :: TBinding Namespace
sets = defineNs "sets" "hydra.lib.sets"

strings :: TBinding Namespace
strings = defineNs "strings" "hydra.lib.strings"

typeclass :: TBinding Namespace
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
