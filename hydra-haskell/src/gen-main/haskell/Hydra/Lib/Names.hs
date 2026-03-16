-- Note: this is an automatically generated file. Do not edit.

-- | Namespaces and primitive names for the Hydra standard library

module Hydra.Lib.Names where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

chars :: Module.Namespace
chars = Module.Namespace "hydra.lib.chars"

eithers :: Module.Namespace
eithers = Module.Namespace "hydra.lib.eithers"

equality :: Module.Namespace
equality = Module.Namespace "hydra.lib.equality"

lists :: Module.Namespace
lists = Module.Namespace "hydra.lib.lists"

literals :: Module.Namespace
literals = Module.Namespace "hydra.lib.literals"

logic :: Module.Namespace
logic = Module.Namespace "hydra.lib.logic"

maps :: Module.Namespace
maps = Module.Namespace "hydra.lib.maps"

math :: Module.Namespace
math = Module.Namespace "hydra.lib.math"

maybes :: Module.Namespace
maybes = Module.Namespace "hydra.lib.maybes"

pairs :: Module.Namespace
pairs = Module.Namespace "hydra.lib.pairs"

sets :: Module.Namespace
sets = Module.Namespace "hydra.lib.sets"

strings :: Module.Namespace
strings = Module.Namespace "hydra.lib.strings"

typeclass :: Module.Namespace
typeclass = Module.Namespace "hydra.typeclass"

charsIsAlphaNum :: Core.Name
charsIsAlphaNum = Core.Name "hydra.lib.chars.isAlphaNum"

charsIsLower :: Core.Name
charsIsLower = Core.Name "hydra.lib.chars.isLower"

charsIsSpace :: Core.Name
charsIsSpace = Core.Name "hydra.lib.chars.isSpace"

charsIsUpper :: Core.Name
charsIsUpper = Core.Name "hydra.lib.chars.isUpper"

charsToLower :: Core.Name
charsToLower = Core.Name "hydra.lib.chars.toLower"

charsToUpper :: Core.Name
charsToUpper = Core.Name "hydra.lib.chars.toUpper"

eithersBind :: Core.Name
eithersBind = Core.Name "hydra.lib.eithers.bind"

eithersBimap :: Core.Name
eithersBimap = Core.Name "hydra.lib.eithers.bimap"

eithersEither :: Core.Name
eithersEither = Core.Name "hydra.lib.eithers.either"

eithersFoldl :: Core.Name
eithersFoldl = Core.Name "hydra.lib.eithers.foldl"

eithersFromLeft :: Core.Name
eithersFromLeft = Core.Name "hydra.lib.eithers.fromLeft"

eithersFromRight :: Core.Name
eithersFromRight = Core.Name "hydra.lib.eithers.fromRight"

eithersIsLeft :: Core.Name
eithersIsLeft = Core.Name "hydra.lib.eithers.isLeft"

eithersIsRight :: Core.Name
eithersIsRight = Core.Name "hydra.lib.eithers.isRight"

eithersLefts :: Core.Name
eithersLefts = Core.Name "hydra.lib.eithers.lefts"

eithersMap :: Core.Name
eithersMap = Core.Name "hydra.lib.eithers.map"

eithersMapList :: Core.Name
eithersMapList = Core.Name "hydra.lib.eithers.mapList"

eithersMapMaybe :: Core.Name
eithersMapMaybe = Core.Name "hydra.lib.eithers.mapMaybe"

eithersMapSet :: Core.Name
eithersMapSet = Core.Name "hydra.lib.eithers.mapSet"

eithersPartitionEithers :: Core.Name
eithersPartitionEithers = Core.Name "hydra.lib.eithers.partitionEithers"

eithersRights :: Core.Name
eithersRights = Core.Name "hydra.lib.eithers.rights"

equalityCompare :: Core.Name
equalityCompare = Core.Name "hydra.lib.equality.compare"

equalityEqual :: Core.Name
equalityEqual = Core.Name "hydra.lib.equality.equal"

equalityGt :: Core.Name
equalityGt = Core.Name "hydra.lib.equality.gt"

equalityGte :: Core.Name
equalityGte = Core.Name "hydra.lib.equality.gte"

equalityIdentity :: Core.Name
equalityIdentity = Core.Name "hydra.lib.equality.identity"

equalityLt :: Core.Name
equalityLt = Core.Name "hydra.lib.equality.lt"

equalityLte :: Core.Name
equalityLte = Core.Name "hydra.lib.equality.lte"

equalityMax :: Core.Name
equalityMax = Core.Name "hydra.lib.equality.max"

equalityMin :: Core.Name
equalityMin = Core.Name "hydra.lib.equality.min"

listsApply :: Core.Name
listsApply = Core.Name "hydra.lib.lists.apply"

listsAt :: Core.Name
listsAt = Core.Name "hydra.lib.lists.at"

listsBind :: Core.Name
listsBind = Core.Name "hydra.lib.lists.bind"

listsConcat :: Core.Name
listsConcat = Core.Name "hydra.lib.lists.concat"

listsConcat2 :: Core.Name
listsConcat2 = Core.Name "hydra.lib.lists.concat2"

listsCons :: Core.Name
listsCons = Core.Name "hydra.lib.lists.cons"

listsDrop :: Core.Name
listsDrop = Core.Name "hydra.lib.lists.drop"

listsDropWhile :: Core.Name
listsDropWhile = Core.Name "hydra.lib.lists.dropWhile"

listsElem :: Core.Name
listsElem = Core.Name "hydra.lib.lists.elem"

listsFilter :: Core.Name
listsFilter = Core.Name "hydra.lib.lists.filter"

listsFind :: Core.Name
listsFind = Core.Name "hydra.lib.lists.find"

listsFoldl :: Core.Name
listsFoldl = Core.Name "hydra.lib.lists.foldl"

listsFoldr :: Core.Name
listsFoldr = Core.Name "hydra.lib.lists.foldr"

listsGroup :: Core.Name
listsGroup = Core.Name "hydra.lib.lists.group"

listsHead :: Core.Name
listsHead = Core.Name "hydra.lib.lists.head"

listsInit :: Core.Name
listsInit = Core.Name "hydra.lib.lists.init"

listsIntercalate :: Core.Name
listsIntercalate = Core.Name "hydra.lib.lists.intercalate"

listsIntersperse :: Core.Name
listsIntersperse = Core.Name "hydra.lib.lists.intersperse"

listsLast :: Core.Name
listsLast = Core.Name "hydra.lib.lists.last"

listsLength :: Core.Name
listsLength = Core.Name "hydra.lib.lists.length"

listsMap :: Core.Name
listsMap = Core.Name "hydra.lib.lists.map"

listsNub :: Core.Name
listsNub = Core.Name "hydra.lib.lists.nub"

listsNull :: Core.Name
listsNull = Core.Name "hydra.lib.lists.null"

listsPartition :: Core.Name
listsPartition = Core.Name "hydra.lib.lists.partition"

listsPure :: Core.Name
listsPure = Core.Name "hydra.lib.lists.pure"

listsReplicate :: Core.Name
listsReplicate = Core.Name "hydra.lib.lists.replicate"

listsReverse :: Core.Name
listsReverse = Core.Name "hydra.lib.lists.reverse"

listsSafeHead :: Core.Name
listsSafeHead = Core.Name "hydra.lib.lists.safeHead"

listsSingleton :: Core.Name
listsSingleton = Core.Name "hydra.lib.lists.singleton"

listsSort :: Core.Name
listsSort = Core.Name "hydra.lib.lists.sort"

listsSortOn :: Core.Name
listsSortOn = Core.Name "hydra.lib.lists.sortOn"

listsSpan :: Core.Name
listsSpan = Core.Name "hydra.lib.lists.span"

listsTail :: Core.Name
listsTail = Core.Name "hydra.lib.lists.tail"

listsTake :: Core.Name
listsTake = Core.Name "hydra.lib.lists.take"

listsTranspose :: Core.Name
listsTranspose = Core.Name "hydra.lib.lists.transpose"

listsZip :: Core.Name
listsZip = Core.Name "hydra.lib.lists.zip"

listsZipWith :: Core.Name
listsZipWith = Core.Name "hydra.lib.lists.zipWith"

literalsBigfloatToBigint :: Core.Name
literalsBigfloatToBigint = Core.Name "hydra.lib.literals.bigfloatToBigint"

literalsBigfloatToFloat32 :: Core.Name
literalsBigfloatToFloat32 = Core.Name "hydra.lib.literals.bigfloatToFloat32"

literalsBigfloatToFloat64 :: Core.Name
literalsBigfloatToFloat64 = Core.Name "hydra.lib.literals.bigfloatToFloat64"

literalsBigintToBigfloat :: Core.Name
literalsBigintToBigfloat = Core.Name "hydra.lib.literals.bigintToBigfloat"

literalsBigintToInt8 :: Core.Name
literalsBigintToInt8 = Core.Name "hydra.lib.literals.bigintToInt8"

literalsBigintToInt16 :: Core.Name
literalsBigintToInt16 = Core.Name "hydra.lib.literals.bigintToInt16"

literalsBigintToInt32 :: Core.Name
literalsBigintToInt32 = Core.Name "hydra.lib.literals.bigintToInt32"

literalsBigintToInt64 :: Core.Name
literalsBigintToInt64 = Core.Name "hydra.lib.literals.bigintToInt64"

literalsBigintToUint8 :: Core.Name
literalsBigintToUint8 = Core.Name "hydra.lib.literals.bigintToUint8"

literalsBigintToUint16 :: Core.Name
literalsBigintToUint16 = Core.Name "hydra.lib.literals.bigintToUint16"

literalsBigintToUint32 :: Core.Name
literalsBigintToUint32 = Core.Name "hydra.lib.literals.bigintToUint32"

literalsBigintToUint64 :: Core.Name
literalsBigintToUint64 = Core.Name "hydra.lib.literals.bigintToUint64"

literalsBinaryToBytes :: Core.Name
literalsBinaryToBytes = Core.Name "hydra.lib.literals.binaryToBytes"

literalsBinaryToString :: Core.Name
literalsBinaryToString = Core.Name "hydra.lib.literals.binaryToString"

literalsFloat32ToBigfloat :: Core.Name
literalsFloat32ToBigfloat = Core.Name "hydra.lib.literals.float32ToBigfloat"

literalsFloat64ToBigfloat :: Core.Name
literalsFloat64ToBigfloat = Core.Name "hydra.lib.literals.float64ToBigfloat"

literalsInt8ToBigint :: Core.Name
literalsInt8ToBigint = Core.Name "hydra.lib.literals.int8ToBigint"

literalsInt16ToBigint :: Core.Name
literalsInt16ToBigint = Core.Name "hydra.lib.literals.int16ToBigint"

literalsInt32ToBigint :: Core.Name
literalsInt32ToBigint = Core.Name "hydra.lib.literals.int32ToBigint"

literalsInt64ToBigint :: Core.Name
literalsInt64ToBigint = Core.Name "hydra.lib.literals.int64ToBigint"

literalsReadBigfloat :: Core.Name
literalsReadBigfloat = Core.Name "hydra.lib.literals.readBigfloat"

literalsReadBigint :: Core.Name
literalsReadBigint = Core.Name "hydra.lib.literals.readBigint"

literalsReadBoolean :: Core.Name
literalsReadBoolean = Core.Name "hydra.lib.literals.readBoolean"

literalsReadFloat32 :: Core.Name
literalsReadFloat32 = Core.Name "hydra.lib.literals.readFloat32"

literalsReadFloat64 :: Core.Name
literalsReadFloat64 = Core.Name "hydra.lib.literals.readFloat64"

literalsReadInt8 :: Core.Name
literalsReadInt8 = Core.Name "hydra.lib.literals.readInt8"

literalsReadInt16 :: Core.Name
literalsReadInt16 = Core.Name "hydra.lib.literals.readInt16"

literalsReadInt32 :: Core.Name
literalsReadInt32 = Core.Name "hydra.lib.literals.readInt32"

literalsReadInt64 :: Core.Name
literalsReadInt64 = Core.Name "hydra.lib.literals.readInt64"

literalsReadString :: Core.Name
literalsReadString = Core.Name "hydra.lib.literals.readString"

literalsReadUint8 :: Core.Name
literalsReadUint8 = Core.Name "hydra.lib.literals.readUint8"

literalsReadUint16 :: Core.Name
literalsReadUint16 = Core.Name "hydra.lib.literals.readUint16"

literalsReadUint32 :: Core.Name
literalsReadUint32 = Core.Name "hydra.lib.literals.readUint32"

literalsReadUint64 :: Core.Name
literalsReadUint64 = Core.Name "hydra.lib.literals.readUint64"

literalsShowBigfloat :: Core.Name
literalsShowBigfloat = Core.Name "hydra.lib.literals.showBigfloat"

literalsShowBigint :: Core.Name
literalsShowBigint = Core.Name "hydra.lib.literals.showBigint"

literalsShowBoolean :: Core.Name
literalsShowBoolean = Core.Name "hydra.lib.literals.showBoolean"

literalsShowFloat32 :: Core.Name
literalsShowFloat32 = Core.Name "hydra.lib.literals.showFloat32"

literalsShowFloat64 :: Core.Name
literalsShowFloat64 = Core.Name "hydra.lib.literals.showFloat64"

literalsShowInt8 :: Core.Name
literalsShowInt8 = Core.Name "hydra.lib.literals.showInt8"

literalsShowInt16 :: Core.Name
literalsShowInt16 = Core.Name "hydra.lib.literals.showInt16"

literalsShowInt32 :: Core.Name
literalsShowInt32 = Core.Name "hydra.lib.literals.showInt32"

literalsShowInt64 :: Core.Name
literalsShowInt64 = Core.Name "hydra.lib.literals.showInt64"

literalsShowUint8 :: Core.Name
literalsShowUint8 = Core.Name "hydra.lib.literals.showUint8"

literalsShowUint16 :: Core.Name
literalsShowUint16 = Core.Name "hydra.lib.literals.showUint16"

literalsShowUint32 :: Core.Name
literalsShowUint32 = Core.Name "hydra.lib.literals.showUint32"

literalsShowUint64 :: Core.Name
literalsShowUint64 = Core.Name "hydra.lib.literals.showUint64"

literalsShowString :: Core.Name
literalsShowString = Core.Name "hydra.lib.literals.showString"

literalsStringToBinary :: Core.Name
literalsStringToBinary = Core.Name "hydra.lib.literals.stringToBinary"

literalsUint8ToBigint :: Core.Name
literalsUint8ToBigint = Core.Name "hydra.lib.literals.uint8ToBigint"

literalsUint16ToBigint :: Core.Name
literalsUint16ToBigint = Core.Name "hydra.lib.literals.uint16ToBigint"

literalsUint32ToBigint :: Core.Name
literalsUint32ToBigint = Core.Name "hydra.lib.literals.uint32ToBigint"

literalsUint64ToBigint :: Core.Name
literalsUint64ToBigint = Core.Name "hydra.lib.literals.uint64ToBigint"

logicAnd :: Core.Name
logicAnd = Core.Name "hydra.lib.logic.and"

logicIfElse :: Core.Name
logicIfElse = Core.Name "hydra.lib.logic.ifElse"

logicNot :: Core.Name
logicNot = Core.Name "hydra.lib.logic.not"

logicOr :: Core.Name
logicOr = Core.Name "hydra.lib.logic.or"

mapsAlter :: Core.Name
mapsAlter = Core.Name "hydra.lib.maps.alter"

mapsBimap :: Core.Name
mapsBimap = Core.Name "hydra.lib.maps.bimap"

mapsDelete :: Core.Name
mapsDelete = Core.Name "hydra.lib.maps.delete"

mapsElems :: Core.Name
mapsElems = Core.Name "hydra.lib.maps.elems"

mapsEmpty :: Core.Name
mapsEmpty = Core.Name "hydra.lib.maps.empty"

mapsFilter :: Core.Name
mapsFilter = Core.Name "hydra.lib.maps.filter"

mapsFilterWithKey :: Core.Name
mapsFilterWithKey = Core.Name "hydra.lib.maps.filterWithKey"

mapsFindWithDefault :: Core.Name
mapsFindWithDefault = Core.Name "hydra.lib.maps.findWithDefault"

mapsFromList :: Core.Name
mapsFromList = Core.Name "hydra.lib.maps.fromList"

mapsInsert :: Core.Name
mapsInsert = Core.Name "hydra.lib.maps.insert"

mapsKeys :: Core.Name
mapsKeys = Core.Name "hydra.lib.maps.keys"

mapsLookup :: Core.Name
mapsLookup = Core.Name "hydra.lib.maps.lookup"

mapsMap :: Core.Name
mapsMap = Core.Name "hydra.lib.maps.map"

mapsMapKeys :: Core.Name
mapsMapKeys = Core.Name "hydra.lib.maps.mapKeys"

mapsMember :: Core.Name
mapsMember = Core.Name "hydra.lib.maps.member"

mapsNull :: Core.Name
mapsNull = Core.Name "hydra.lib.maps.null"

mapsSingleton :: Core.Name
mapsSingleton = Core.Name "hydra.lib.maps.singleton"

mapsSize :: Core.Name
mapsSize = Core.Name "hydra.lib.maps.size"

mapsToList :: Core.Name
mapsToList = Core.Name "hydra.lib.maps.toList"

mapsUnion :: Core.Name
mapsUnion = Core.Name "hydra.lib.maps.union"

mathAbs :: Core.Name
mathAbs = Core.Name "hydra.lib.math.abs"

mathAcos :: Core.Name
mathAcos = Core.Name "hydra.lib.math.acos"

mathAcosh :: Core.Name
mathAcosh = Core.Name "hydra.lib.math.acosh"

mathAdd :: Core.Name
mathAdd = Core.Name "hydra.lib.math.add"

mathAsin :: Core.Name
mathAsin = Core.Name "hydra.lib.math.asin"

mathAsinh :: Core.Name
mathAsinh = Core.Name "hydra.lib.math.asinh"

mathAtan :: Core.Name
mathAtan = Core.Name "hydra.lib.math.atan"

mathAtan2 :: Core.Name
mathAtan2 = Core.Name "hydra.lib.math.atan2"

mathAtanh :: Core.Name
mathAtanh = Core.Name "hydra.lib.math.atanh"

mathCeiling :: Core.Name
mathCeiling = Core.Name "hydra.lib.math.ceiling"

mathCos :: Core.Name
mathCos = Core.Name "hydra.lib.math.cos"

mathCosh :: Core.Name
mathCosh = Core.Name "hydra.lib.math.cosh"

mathDiv :: Core.Name
mathDiv = Core.Name "hydra.lib.math.div"

mathE :: Core.Name
mathE = Core.Name "hydra.lib.math.e"

mathEven :: Core.Name
mathEven = Core.Name "hydra.lib.math.even"

mathExp :: Core.Name
mathExp = Core.Name "hydra.lib.math.exp"

mathFloor :: Core.Name
mathFloor = Core.Name "hydra.lib.math.floor"

mathLog :: Core.Name
mathLog = Core.Name "hydra.lib.math.log"

mathLogBase :: Core.Name
mathLogBase = Core.Name "hydra.lib.math.logBase"

mathMax :: Core.Name
mathMax = Core.Name "hydra.lib.math.max"

mathMin :: Core.Name
mathMin = Core.Name "hydra.lib.math.min"

mathMod :: Core.Name
mathMod = Core.Name "hydra.lib.math.mod"

mathMul :: Core.Name
mathMul = Core.Name "hydra.lib.math.mul"

mathNegate :: Core.Name
mathNegate = Core.Name "hydra.lib.math.negate"

mathOdd :: Core.Name
mathOdd = Core.Name "hydra.lib.math.odd"

mathPi :: Core.Name
mathPi = Core.Name "hydra.lib.math.pi"

mathPow :: Core.Name
mathPow = Core.Name "hydra.lib.math.pow"

mathPred :: Core.Name
mathPred = Core.Name "hydra.lib.math.pred"

mathRange :: Core.Name
mathRange = Core.Name "hydra.lib.math.range"

mathRem :: Core.Name
mathRem = Core.Name "hydra.lib.math.rem"

mathRound :: Core.Name
mathRound = Core.Name "hydra.lib.math.round"

mathSignum :: Core.Name
mathSignum = Core.Name "hydra.lib.math.signum"

mathSin :: Core.Name
mathSin = Core.Name "hydra.lib.math.sin"

mathSinh :: Core.Name
mathSinh = Core.Name "hydra.lib.math.sinh"

mathSqrt :: Core.Name
mathSqrt = Core.Name "hydra.lib.math.sqrt"

mathSub :: Core.Name
mathSub = Core.Name "hydra.lib.math.sub"

mathSucc :: Core.Name
mathSucc = Core.Name "hydra.lib.math.succ"

mathTan :: Core.Name
mathTan = Core.Name "hydra.lib.math.tan"

mathTanh :: Core.Name
mathTanh = Core.Name "hydra.lib.math.tanh"

mathTruncate :: Core.Name
mathTruncate = Core.Name "hydra.lib.math.truncate"

maybesApply :: Core.Name
maybesApply = Core.Name "hydra.lib.maybes.apply"

maybesBind :: Core.Name
maybesBind = Core.Name "hydra.lib.maybes.bind"

maybesCases :: Core.Name
maybesCases = Core.Name "hydra.lib.maybes.cases"

maybesCat :: Core.Name
maybesCat = Core.Name "hydra.lib.maybes.cat"

maybesCompose :: Core.Name
maybesCompose = Core.Name "hydra.lib.maybes.compose"

maybesFromJust :: Core.Name
maybesFromJust = Core.Name "hydra.lib.maybes.fromJust"

maybesFromMaybe :: Core.Name
maybesFromMaybe = Core.Name "hydra.lib.maybes.fromMaybe"

maybesIsJust :: Core.Name
maybesIsJust = Core.Name "hydra.lib.maybes.isJust"

maybesIsNothing :: Core.Name
maybesIsNothing = Core.Name "hydra.lib.maybes.isNothing"

maybesMap :: Core.Name
maybesMap = Core.Name "hydra.lib.maybes.map"

maybesMapMaybe :: Core.Name
maybesMapMaybe = Core.Name "hydra.lib.maybes.mapMaybe"

maybesMaybe :: Core.Name
maybesMaybe = Core.Name "hydra.lib.maybes.maybe"

maybesPure :: Core.Name
maybesPure = Core.Name "hydra.lib.maybes.pure"

maybesToList :: Core.Name
maybesToList = Core.Name "hydra.lib.maybes.toList"

pairsBimap :: Core.Name
pairsBimap = Core.Name "hydra.lib.pairs.bimap"

pairsFirst :: Core.Name
pairsFirst = Core.Name "hydra.lib.pairs.first"

pairsSecond :: Core.Name
pairsSecond = Core.Name "hydra.lib.pairs.second"

setsDelete :: Core.Name
setsDelete = Core.Name "hydra.lib.sets.delete"

setsDifference :: Core.Name
setsDifference = Core.Name "hydra.lib.sets.difference"

setsEmpty :: Core.Name
setsEmpty = Core.Name "hydra.lib.sets.empty"

setsFromList :: Core.Name
setsFromList = Core.Name "hydra.lib.sets.fromList"

setsInsert :: Core.Name
setsInsert = Core.Name "hydra.lib.sets.insert"

setsIntersection :: Core.Name
setsIntersection = Core.Name "hydra.lib.sets.intersection"

setsMap :: Core.Name
setsMap = Core.Name "hydra.lib.sets.map"

setsMember :: Core.Name
setsMember = Core.Name "hydra.lib.sets.member"

setsNull :: Core.Name
setsNull = Core.Name "hydra.lib.sets.null"

setsSingleton :: Core.Name
setsSingleton = Core.Name "hydra.lib.sets.singleton"

setsSize :: Core.Name
setsSize = Core.Name "hydra.lib.sets.size"

setsToList :: Core.Name
setsToList = Core.Name "hydra.lib.sets.toList"

setsUnion :: Core.Name
setsUnion = Core.Name "hydra.lib.sets.union"

setsUnions :: Core.Name
setsUnions = Core.Name "hydra.lib.sets.unions"

stringsCat :: Core.Name
stringsCat = Core.Name "hydra.lib.strings.cat"

stringsCat2 :: Core.Name
stringsCat2 = Core.Name "hydra.lib.strings.cat2"

stringsCharAt :: Core.Name
stringsCharAt = Core.Name "hydra.lib.strings.charAt"

stringsFromList :: Core.Name
stringsFromList = Core.Name "hydra.lib.strings.fromList"

stringsIntercalate :: Core.Name
stringsIntercalate = Core.Name "hydra.lib.strings.intercalate"

stringsNull :: Core.Name
stringsNull = Core.Name "hydra.lib.strings.null"

stringsLength :: Core.Name
stringsLength = Core.Name "hydra.lib.strings.length"

stringsLines :: Core.Name
stringsLines = Core.Name "hydra.lib.strings.lines"

stringsSplitOn :: Core.Name
stringsSplitOn = Core.Name "hydra.lib.strings.splitOn"

stringsToList :: Core.Name
stringsToList = Core.Name "hydra.lib.strings.toList"

stringsToLower :: Core.Name
stringsToLower = Core.Name "hydra.lib.strings.toLower"

stringsToUpper :: Core.Name
stringsToUpper = Core.Name "hydra.lib.strings.toUpper"

stringsUnlines :: Core.Name
stringsUnlines = Core.Name "hydra.lib.strings.unlines"

typeclassEq :: Core.Name
typeclassEq = Core.Name "hydra.typeclass.Eq"

typeclassOrd :: Core.Name
typeclassOrd = Core.Name "hydra.typeclass.Ord"
