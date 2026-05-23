-- Note: this is an automatically generated file. Do not edit.
-- | Namespaces and primitive names for the Hydra standard library

module Hydra.Lib.Names where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | ModuleName of the chars library
chars :: Packaging.ModuleName
chars = Packaging.ModuleName "hydra.lib.chars"
-- | Name of the chars.isAlphaNum primitive
charsIsAlphaNum :: Core.Name
charsIsAlphaNum = Core.Name "hydra.lib.chars.isAlphaNum"
-- | Name of the chars.isLower primitive
charsIsLower :: Core.Name
charsIsLower = Core.Name "hydra.lib.chars.isLower"
-- | Name of the chars.isSpace primitive
charsIsSpace :: Core.Name
charsIsSpace = Core.Name "hydra.lib.chars.isSpace"
-- | Name of the chars.isUpper primitive
charsIsUpper :: Core.Name
charsIsUpper = Core.Name "hydra.lib.chars.isUpper"
-- | Name of the chars.toLower primitive
charsToLower :: Core.Name
charsToLower = Core.Name "hydra.lib.chars.toLower"
-- | Name of the chars.toUpper primitive
charsToUpper :: Core.Name
charsToUpper = Core.Name "hydra.lib.chars.toUpper"
-- | ModuleName of the eithers library
eithers :: Packaging.ModuleName
eithers = Packaging.ModuleName "hydra.lib.eithers"
-- | Name of the eithers.bimap primitive
eithersBimap :: Core.Name
eithersBimap = Core.Name "hydra.lib.eithers.bimap"
-- | Name of the eithers.bind primitive
eithersBind :: Core.Name
eithersBind = Core.Name "hydra.lib.eithers.bind"
-- | Name of the eithers.either primitive
eithersEither :: Core.Name
eithersEither = Core.Name "hydra.lib.eithers.either"
-- | Name of the eithers.foldl primitive
eithersFoldl :: Core.Name
eithersFoldl = Core.Name "hydra.lib.eithers.foldl"
-- | Name of the eithers.fromLeft primitive
eithersFromLeft :: Core.Name
eithersFromLeft = Core.Name "hydra.lib.eithers.fromLeft"
-- | Name of the eithers.fromRight primitive
eithersFromRight :: Core.Name
eithersFromRight = Core.Name "hydra.lib.eithers.fromRight"
-- | Name of the eithers.isLeft primitive
eithersIsLeft :: Core.Name
eithersIsLeft = Core.Name "hydra.lib.eithers.isLeft"
-- | Name of the eithers.isRight primitive
eithersIsRight :: Core.Name
eithersIsRight = Core.Name "hydra.lib.eithers.isRight"
-- | Name of the eithers.lefts primitive
eithersLefts :: Core.Name
eithersLefts = Core.Name "hydra.lib.eithers.lefts"
-- | Name of the eithers.map primitive
eithersMap :: Core.Name
eithersMap = Core.Name "hydra.lib.eithers.map"
-- | Name of the eithers.mapList primitive
eithersMapList :: Core.Name
eithersMapList = Core.Name "hydra.lib.eithers.mapList"
-- | Name of the eithers.mapMaybe primitive
eithersMapMaybe :: Core.Name
eithersMapMaybe = Core.Name "hydra.lib.eithers.mapMaybe"
-- | Name of the eithers.mapSet primitive
eithersMapSet :: Core.Name
eithersMapSet = Core.Name "hydra.lib.eithers.mapSet"
-- | Name of the eithers.partitionEithers primitive
eithersPartitionEithers :: Core.Name
eithersPartitionEithers = Core.Name "hydra.lib.eithers.partitionEithers"
-- | Name of the eithers.rights primitive
eithersRights :: Core.Name
eithersRights = Core.Name "hydra.lib.eithers.rights"
-- | ModuleName of the equality library
equality :: Packaging.ModuleName
equality = Packaging.ModuleName "hydra.lib.equality"
-- | Name of the equality.compare primitive
equalityCompare :: Core.Name
equalityCompare = Core.Name "hydra.lib.equality.compare"
-- | Name of the equality.equal primitive
equalityEqual :: Core.Name
equalityEqual = Core.Name "hydra.lib.equality.equal"
-- | Name of the equality.gt primitive
equalityGt :: Core.Name
equalityGt = Core.Name "hydra.lib.equality.gt"
-- | Name of the equality.gte primitive
equalityGte :: Core.Name
equalityGte = Core.Name "hydra.lib.equality.gte"
-- | Name of the equality.identity primitive
equalityIdentity :: Core.Name
equalityIdentity = Core.Name "hydra.lib.equality.identity"
-- | Name of the equality.lt primitive
equalityLt :: Core.Name
equalityLt = Core.Name "hydra.lib.equality.lt"
-- | Name of the equality.lte primitive
equalityLte :: Core.Name
equalityLte = Core.Name "hydra.lib.equality.lte"
-- | Name of the equality.max primitive
equalityMax :: Core.Name
equalityMax = Core.Name "hydra.lib.equality.max"
-- | Name of the equality.min primitive
equalityMin :: Core.Name
equalityMin = Core.Name "hydra.lib.equality.min"
-- | ModuleName of the lists library
lists :: Packaging.ModuleName
lists = Packaging.ModuleName "hydra.lib.lists"
-- | Name of the lists.apply primitive
listsApply :: Core.Name
listsApply = Core.Name "hydra.lib.lists.apply"
-- | Name of the lists.bind primitive
listsBind :: Core.Name
listsBind = Core.Name "hydra.lib.lists.bind"
-- | Name of the lists.concat primitive
listsConcat :: Core.Name
listsConcat = Core.Name "hydra.lib.lists.concat"
-- | Name of the lists.concat2 primitive
listsConcat2 :: Core.Name
listsConcat2 = Core.Name "hydra.lib.lists.concat2"
-- | Name of the lists.cons primitive
listsCons :: Core.Name
listsCons = Core.Name "hydra.lib.lists.cons"
-- | Name of the lists.drop primitive
listsDrop :: Core.Name
listsDrop = Core.Name "hydra.lib.lists.drop"
-- | Name of the lists.dropWhile primitive
listsDropWhile :: Core.Name
listsDropWhile = Core.Name "hydra.lib.lists.dropWhile"
-- | Name of the lists.elem primitive
listsElem :: Core.Name
listsElem = Core.Name "hydra.lib.lists.elem"
-- | Name of the lists.filter primitive
listsFilter :: Core.Name
listsFilter = Core.Name "hydra.lib.lists.filter"
-- | Name of the lists.find primitive
listsFind :: Core.Name
listsFind = Core.Name "hydra.lib.lists.find"
-- | Name of the lists.foldl primitive
listsFoldl :: Core.Name
listsFoldl = Core.Name "hydra.lib.lists.foldl"
-- | Name of the lists.foldr primitive
listsFoldr :: Core.Name
listsFoldr = Core.Name "hydra.lib.lists.foldr"
-- | Name of the lists.group primitive
listsGroup :: Core.Name
listsGroup = Core.Name "hydra.lib.lists.group"
-- | Name of the lists.intercalate primitive
listsIntercalate :: Core.Name
listsIntercalate = Core.Name "hydra.lib.lists.intercalate"
-- | Name of the lists.intersperse primitive
listsIntersperse :: Core.Name
listsIntersperse = Core.Name "hydra.lib.lists.intersperse"
-- | Name of the lists.length primitive
listsLength :: Core.Name
listsLength = Core.Name "hydra.lib.lists.length"
-- | Name of the lists.map primitive
listsMap :: Core.Name
listsMap = Core.Name "hydra.lib.lists.map"
-- | Name of the lists.maybeAt primitive
listsMaybeAt :: Core.Name
listsMaybeAt = Core.Name "hydra.lib.lists.maybeAt"
-- | Name of the lists.maybeHead primitive
listsMaybeHead :: Core.Name
listsMaybeHead = Core.Name "hydra.lib.lists.maybeHead"
-- | Name of the lists.maybeInit primitive
listsMaybeInit :: Core.Name
listsMaybeInit = Core.Name "hydra.lib.lists.maybeInit"
-- | Name of the lists.maybeLast primitive
listsMaybeLast :: Core.Name
listsMaybeLast = Core.Name "hydra.lib.lists.maybeLast"
-- | Name of the lists.maybeTail primitive
listsMaybeTail :: Core.Name
listsMaybeTail = Core.Name "hydra.lib.lists.maybeTail"
-- | Name of the lists.nub primitive
listsNub :: Core.Name
listsNub = Core.Name "hydra.lib.lists.nub"
-- | Name of the lists.null primitive
listsNull :: Core.Name
listsNull = Core.Name "hydra.lib.lists.null"
-- | Name of the lists.partition primitive
listsPartition :: Core.Name
listsPartition = Core.Name "hydra.lib.lists.partition"
-- | Name of the lists.pure primitive
listsPure :: Core.Name
listsPure = Core.Name "hydra.lib.lists.pure"
-- | Name of the lists.replicate primitive
listsReplicate :: Core.Name
listsReplicate = Core.Name "hydra.lib.lists.replicate"
-- | Name of the lists.reverse primitive
listsReverse :: Core.Name
listsReverse = Core.Name "hydra.lib.lists.reverse"
-- | Name of the lists.singleton primitive
listsSingleton :: Core.Name
listsSingleton = Core.Name "hydra.lib.lists.singleton"
-- | Name of the lists.sort primitive
listsSort :: Core.Name
listsSort = Core.Name "hydra.lib.lists.sort"
-- | Name of the lists.sortOn primitive
listsSortOn :: Core.Name
listsSortOn = Core.Name "hydra.lib.lists.sortOn"
-- | Name of the lists.span primitive
listsSpan :: Core.Name
listsSpan = Core.Name "hydra.lib.lists.span"
-- | Name of the lists.take primitive
listsTake :: Core.Name
listsTake = Core.Name "hydra.lib.lists.take"
-- | Name of the lists.transpose primitive
listsTranspose :: Core.Name
listsTranspose = Core.Name "hydra.lib.lists.transpose"
-- | Name of the lists.uncons primitive
listsUncons :: Core.Name
listsUncons = Core.Name "hydra.lib.lists.uncons"
-- | Name of the lists.zip primitive
listsZip :: Core.Name
listsZip = Core.Name "hydra.lib.lists.zip"
-- | Name of the lists.zipWith primitive
listsZipWith :: Core.Name
listsZipWith = Core.Name "hydra.lib.lists.zipWith"
-- | ModuleName of the literals library
literals :: Packaging.ModuleName
literals = Packaging.ModuleName "hydra.lib.literals"
-- | Name of the literals.bigintToDecimal primitive
literalsBigintToDecimal :: Core.Name
literalsBigintToDecimal = Core.Name "hydra.lib.literals.bigintToDecimal"
-- | Name of the literals.bigintToInt16 primitive
literalsBigintToInt16 :: Core.Name
literalsBigintToInt16 = Core.Name "hydra.lib.literals.bigintToInt16"
-- | Name of the literals.bigintToInt32 primitive
literalsBigintToInt32 :: Core.Name
literalsBigintToInt32 = Core.Name "hydra.lib.literals.bigintToInt32"
-- | Name of the literals.bigintToInt64 primitive
literalsBigintToInt64 :: Core.Name
literalsBigintToInt64 = Core.Name "hydra.lib.literals.bigintToInt64"
-- | Name of the literals.bigintToInt8 primitive
literalsBigintToInt8 :: Core.Name
literalsBigintToInt8 = Core.Name "hydra.lib.literals.bigintToInt8"
-- | Name of the literals.bigintToUint16 primitive
literalsBigintToUint16 :: Core.Name
literalsBigintToUint16 = Core.Name "hydra.lib.literals.bigintToUint16"
-- | Name of the literals.bigintToUint32 primitive
literalsBigintToUint32 :: Core.Name
literalsBigintToUint32 = Core.Name "hydra.lib.literals.bigintToUint32"
-- | Name of the literals.bigintToUint64 primitive
literalsBigintToUint64 :: Core.Name
literalsBigintToUint64 = Core.Name "hydra.lib.literals.bigintToUint64"
-- | Name of the literals.bigintToUint8 primitive
literalsBigintToUint8 :: Core.Name
literalsBigintToUint8 = Core.Name "hydra.lib.literals.bigintToUint8"
-- | Name of the literals.binaryToBytes primitive
literalsBinaryToBytes :: Core.Name
literalsBinaryToBytes = Core.Name "hydra.lib.literals.binaryToBytes"
-- | Name of the literals.binaryToString primitive
literalsBinaryToString :: Core.Name
literalsBinaryToString = Core.Name "hydra.lib.literals.binaryToString"
-- | Name of the literals.decimalToBigint primitive
literalsDecimalToBigint :: Core.Name
literalsDecimalToBigint = Core.Name "hydra.lib.literals.decimalToBigint"
-- | Name of the literals.decimalToFloat32 primitive
literalsDecimalToFloat32 :: Core.Name
literalsDecimalToFloat32 = Core.Name "hydra.lib.literals.decimalToFloat32"
-- | Name of the literals.decimalToFloat64 primitive
literalsDecimalToFloat64 :: Core.Name
literalsDecimalToFloat64 = Core.Name "hydra.lib.literals.decimalToFloat64"
-- | Name of the literals.float32ToDecimal primitive
literalsFloat32ToDecimal :: Core.Name
literalsFloat32ToDecimal = Core.Name "hydra.lib.literals.float32ToDecimal"
-- | Name of the literals.float32ToFloat64 primitive
literalsFloat32ToFloat64 :: Core.Name
literalsFloat32ToFloat64 = Core.Name "hydra.lib.literals.float32ToFloat64"
-- | Name of the literals.float64ToDecimal primitive
literalsFloat64ToDecimal :: Core.Name
literalsFloat64ToDecimal = Core.Name "hydra.lib.literals.float64ToDecimal"
-- | Name of the literals.float64ToFloat32 primitive
literalsFloat64ToFloat32 :: Core.Name
literalsFloat64ToFloat32 = Core.Name "hydra.lib.literals.float64ToFloat32"
-- | Name of the literals.int16ToBigint primitive
literalsInt16ToBigint :: Core.Name
literalsInt16ToBigint = Core.Name "hydra.lib.literals.int16ToBigint"
-- | Name of the literals.int32ToBigint primitive
literalsInt32ToBigint :: Core.Name
literalsInt32ToBigint = Core.Name "hydra.lib.literals.int32ToBigint"
-- | Name of the literals.int64ToBigint primitive
literalsInt64ToBigint :: Core.Name
literalsInt64ToBigint = Core.Name "hydra.lib.literals.int64ToBigint"
-- | Name of the literals.int8ToBigint primitive
literalsInt8ToBigint :: Core.Name
literalsInt8ToBigint = Core.Name "hydra.lib.literals.int8ToBigint"
-- | Name of the literals.readBigint primitive
literalsReadBigint :: Core.Name
literalsReadBigint = Core.Name "hydra.lib.literals.readBigint"
-- | Name of the literals.readBoolean primitive
literalsReadBoolean :: Core.Name
literalsReadBoolean = Core.Name "hydra.lib.literals.readBoolean"
-- | Name of the literals.readDecimal primitive
literalsReadDecimal :: Core.Name
literalsReadDecimal = Core.Name "hydra.lib.literals.readDecimal"
-- | Name of the literals.readFloat32 primitive
literalsReadFloat32 :: Core.Name
literalsReadFloat32 = Core.Name "hydra.lib.literals.readFloat32"
-- | Name of the literals.readFloat64 primitive
literalsReadFloat64 :: Core.Name
literalsReadFloat64 = Core.Name "hydra.lib.literals.readFloat64"
-- | Name of the literals.readInt16 primitive
literalsReadInt16 :: Core.Name
literalsReadInt16 = Core.Name "hydra.lib.literals.readInt16"
-- | Name of the literals.readInt32 primitive
literalsReadInt32 :: Core.Name
literalsReadInt32 = Core.Name "hydra.lib.literals.readInt32"
-- | Name of the literals.readInt64 primitive
literalsReadInt64 :: Core.Name
literalsReadInt64 = Core.Name "hydra.lib.literals.readInt64"
-- | Name of the literals.readInt8 primitive
literalsReadInt8 :: Core.Name
literalsReadInt8 = Core.Name "hydra.lib.literals.readInt8"
-- | Name of the literals.readString primitive
literalsReadString :: Core.Name
literalsReadString = Core.Name "hydra.lib.literals.readString"
-- | Name of the literals.readUint16 primitive
literalsReadUint16 :: Core.Name
literalsReadUint16 = Core.Name "hydra.lib.literals.readUint16"
-- | Name of the literals.readUint32 primitive
literalsReadUint32 :: Core.Name
literalsReadUint32 = Core.Name "hydra.lib.literals.readUint32"
-- | Name of the literals.readUint64 primitive
literalsReadUint64 :: Core.Name
literalsReadUint64 = Core.Name "hydra.lib.literals.readUint64"
-- | Name of the literals.readUint8 primitive
literalsReadUint8 :: Core.Name
literalsReadUint8 = Core.Name "hydra.lib.literals.readUint8"
-- | Name of the literals.showBigint primitive
literalsShowBigint :: Core.Name
literalsShowBigint = Core.Name "hydra.lib.literals.showBigint"
-- | Name of the literals.showBoolean primitive
literalsShowBoolean :: Core.Name
literalsShowBoolean = Core.Name "hydra.lib.literals.showBoolean"
-- | Name of the literals.showDecimal primitive
literalsShowDecimal :: Core.Name
literalsShowDecimal = Core.Name "hydra.lib.literals.showDecimal"
-- | Name of the literals.showFloat32 primitive
literalsShowFloat32 :: Core.Name
literalsShowFloat32 = Core.Name "hydra.lib.literals.showFloat32"
-- | Name of the literals.showFloat64 primitive
literalsShowFloat64 :: Core.Name
literalsShowFloat64 = Core.Name "hydra.lib.literals.showFloat64"
-- | Name of the literals.showInt16 primitive
literalsShowInt16 :: Core.Name
literalsShowInt16 = Core.Name "hydra.lib.literals.showInt16"
-- | Name of the literals.showInt32 primitive
literalsShowInt32 :: Core.Name
literalsShowInt32 = Core.Name "hydra.lib.literals.showInt32"
-- | Name of the literals.showInt64 primitive
literalsShowInt64 :: Core.Name
literalsShowInt64 = Core.Name "hydra.lib.literals.showInt64"
-- | Name of the literals.showInt8 primitive
literalsShowInt8 :: Core.Name
literalsShowInt8 = Core.Name "hydra.lib.literals.showInt8"
-- | Name of the literals.showString primitive
literalsShowString :: Core.Name
literalsShowString = Core.Name "hydra.lib.literals.showString"
-- | Name of the literals.showUint16 primitive
literalsShowUint16 :: Core.Name
literalsShowUint16 = Core.Name "hydra.lib.literals.showUint16"
-- | Name of the literals.showUint32 primitive
literalsShowUint32 :: Core.Name
literalsShowUint32 = Core.Name "hydra.lib.literals.showUint32"
-- | Name of the literals.showUint64 primitive
literalsShowUint64 :: Core.Name
literalsShowUint64 = Core.Name "hydra.lib.literals.showUint64"
-- | Name of the literals.showUint8 primitive
literalsShowUint8 :: Core.Name
literalsShowUint8 = Core.Name "hydra.lib.literals.showUint8"
-- | Name of the literals.stringToBinary primitive
literalsStringToBinary :: Core.Name
literalsStringToBinary = Core.Name "hydra.lib.literals.stringToBinary"
-- | Name of the literals.uint16ToBigint primitive
literalsUint16ToBigint :: Core.Name
literalsUint16ToBigint = Core.Name "hydra.lib.literals.uint16ToBigint"
-- | Name of the literals.uint32ToBigint primitive
literalsUint32ToBigint :: Core.Name
literalsUint32ToBigint = Core.Name "hydra.lib.literals.uint32ToBigint"
-- | Name of the literals.uint64ToBigint primitive
literalsUint64ToBigint :: Core.Name
literalsUint64ToBigint = Core.Name "hydra.lib.literals.uint64ToBigint"
-- | Name of the literals.uint8ToBigint primitive
literalsUint8ToBigint :: Core.Name
literalsUint8ToBigint = Core.Name "hydra.lib.literals.uint8ToBigint"
-- | ModuleName of the logic library
logic :: Packaging.ModuleName
logic = Packaging.ModuleName "hydra.lib.logic"
-- | Name of the logic.and primitive
logicAnd :: Core.Name
logicAnd = Core.Name "hydra.lib.logic.and"
-- | Name of the logic.ifElse primitive
logicIfElse :: Core.Name
logicIfElse = Core.Name "hydra.lib.logic.ifElse"
-- | Name of the logic.not primitive
logicNot :: Core.Name
logicNot = Core.Name "hydra.lib.logic.not"
-- | Name of the logic.or primitive
logicOr :: Core.Name
logicOr = Core.Name "hydra.lib.logic.or"
-- | ModuleName of the maps library
maps :: Packaging.ModuleName
maps = Packaging.ModuleName "hydra.lib.maps"
-- | Name of the maps.alter primitive
mapsAlter :: Core.Name
mapsAlter = Core.Name "hydra.lib.maps.alter"
-- | Name of the maps.bimap primitive
mapsBimap :: Core.Name
mapsBimap = Core.Name "hydra.lib.maps.bimap"
-- | Name of the maps.delete primitive
mapsDelete :: Core.Name
mapsDelete = Core.Name "hydra.lib.maps.delete"
-- | Name of the maps.elems primitive
mapsElems :: Core.Name
mapsElems = Core.Name "hydra.lib.maps.elems"
-- | Name of the maps.empty primitive
mapsEmpty :: Core.Name
mapsEmpty = Core.Name "hydra.lib.maps.empty"
-- | Name of the maps.filter primitive
mapsFilter :: Core.Name
mapsFilter = Core.Name "hydra.lib.maps.filter"
-- | Name of the maps.filterWithKey primitive
mapsFilterWithKey :: Core.Name
mapsFilterWithKey = Core.Name "hydra.lib.maps.filterWithKey"
-- | Name of the maps.findWithDefault primitive
mapsFindWithDefault :: Core.Name
mapsFindWithDefault = Core.Name "hydra.lib.maps.findWithDefault"
-- | Name of the maps.fromList primitive
mapsFromList :: Core.Name
mapsFromList = Core.Name "hydra.lib.maps.fromList"
-- | Name of the maps.insert primitive
mapsInsert :: Core.Name
mapsInsert = Core.Name "hydra.lib.maps.insert"
-- | Name of the maps.keys primitive
mapsKeys :: Core.Name
mapsKeys = Core.Name "hydra.lib.maps.keys"
-- | Name of the maps.lookup primitive
mapsLookup :: Core.Name
mapsLookup = Core.Name "hydra.lib.maps.lookup"
-- | Name of the maps.map primitive
mapsMap :: Core.Name
mapsMap = Core.Name "hydra.lib.maps.map"
-- | Name of the maps.mapKeys primitive
mapsMapKeys :: Core.Name
mapsMapKeys = Core.Name "hydra.lib.maps.mapKeys"
-- | Name of the maps.member primitive
mapsMember :: Core.Name
mapsMember = Core.Name "hydra.lib.maps.member"
-- | Name of the maps.null primitive
mapsNull :: Core.Name
mapsNull = Core.Name "hydra.lib.maps.null"
-- | Name of the maps.singleton primitive
mapsSingleton :: Core.Name
mapsSingleton = Core.Name "hydra.lib.maps.singleton"
-- | Name of the maps.size primitive
mapsSize :: Core.Name
mapsSize = Core.Name "hydra.lib.maps.size"
-- | Name of the maps.toList primitive
mapsToList :: Core.Name
mapsToList = Core.Name "hydra.lib.maps.toList"
-- | Name of the maps.union primitive
mapsUnion :: Core.Name
mapsUnion = Core.Name "hydra.lib.maps.union"
-- | ModuleName of the math library
math :: Packaging.ModuleName
math = Packaging.ModuleName "hydra.lib.math"
-- | Name of the math.abs primitive
mathAbs :: Core.Name
mathAbs = Core.Name "hydra.lib.math.abs"
-- | Name of the math.acos primitive
mathAcos :: Core.Name
mathAcos = Core.Name "hydra.lib.math.acos"
-- | Name of the math.acosh primitive
mathAcosh :: Core.Name
mathAcosh = Core.Name "hydra.lib.math.acosh"
-- | Name of the math.add primitive
mathAdd :: Core.Name
mathAdd = Core.Name "hydra.lib.math.add"
-- | Name of the math.addFloat64 primitive
mathAddFloat64 :: Core.Name
mathAddFloat64 = Core.Name "hydra.lib.math.addFloat64"
-- | Name of the math.asin primitive
mathAsin :: Core.Name
mathAsin = Core.Name "hydra.lib.math.asin"
-- | Name of the math.asinh primitive
mathAsinh :: Core.Name
mathAsinh = Core.Name "hydra.lib.math.asinh"
-- | Name of the math.atan primitive
mathAtan :: Core.Name
mathAtan = Core.Name "hydra.lib.math.atan"
-- | Name of the math.atan2 primitive
mathAtan2 :: Core.Name
mathAtan2 = Core.Name "hydra.lib.math.atan2"
-- | Name of the math.atanh primitive
mathAtanh :: Core.Name
mathAtanh = Core.Name "hydra.lib.math.atanh"
-- | Name of the math.ceiling primitive
mathCeiling :: Core.Name
mathCeiling = Core.Name "hydra.lib.math.ceiling"
-- | Name of the math.cos primitive
mathCos :: Core.Name
mathCos = Core.Name "hydra.lib.math.cos"
-- | Name of the math.cosh primitive
mathCosh :: Core.Name
mathCosh = Core.Name "hydra.lib.math.cosh"
-- | Name of the math.e primitive
mathE :: Core.Name
mathE = Core.Name "hydra.lib.math.e"
-- | Name of the math.even primitive
mathEven :: Core.Name
mathEven = Core.Name "hydra.lib.math.even"
-- | Name of the math.exp primitive
mathExp :: Core.Name
mathExp = Core.Name "hydra.lib.math.exp"
-- | Name of the math.floor primitive
mathFloor :: Core.Name
mathFloor = Core.Name "hydra.lib.math.floor"
-- | Name of the math.log primitive
mathLog :: Core.Name
mathLog = Core.Name "hydra.lib.math.log"
-- | Name of the math.logBase primitive
mathLogBase :: Core.Name
mathLogBase = Core.Name "hydra.lib.math.logBase"
-- | Name of the math.max primitive
mathMax :: Core.Name
mathMax = Core.Name "hydra.lib.math.max"
-- | Name of the math.maybeDiv primitive
mathMaybeDiv :: Core.Name
mathMaybeDiv = Core.Name "hydra.lib.math.maybeDiv"
-- | Name of the math.maybeMod primitive
mathMaybeMod :: Core.Name
mathMaybeMod = Core.Name "hydra.lib.math.maybeMod"
-- | Name of the math.maybePred primitive
mathMaybePred :: Core.Name
mathMaybePred = Core.Name "hydra.lib.math.maybePred"
-- | Name of the math.maybeRem primitive
mathMaybeRem :: Core.Name
mathMaybeRem = Core.Name "hydra.lib.math.maybeRem"
-- | Name of the math.maybeSucc primitive
mathMaybeSucc :: Core.Name
mathMaybeSucc = Core.Name "hydra.lib.math.maybeSucc"
-- | Name of the math.min primitive
mathMin :: Core.Name
mathMin = Core.Name "hydra.lib.math.min"
-- | Name of the math.mul primitive
mathMul :: Core.Name
mathMul = Core.Name "hydra.lib.math.mul"
-- | Name of the math.mulFloat64 primitive
mathMulFloat64 :: Core.Name
mathMulFloat64 = Core.Name "hydra.lib.math.mulFloat64"
-- | Name of the math.negate primitive
mathNegate :: Core.Name
mathNegate = Core.Name "hydra.lib.math.negate"
-- | Name of the math.negateFloat64 primitive
mathNegateFloat64 :: Core.Name
mathNegateFloat64 = Core.Name "hydra.lib.math.negateFloat64"
-- | Name of the math.odd primitive
mathOdd :: Core.Name
mathOdd = Core.Name "hydra.lib.math.odd"
-- | Name of the math.pi primitive
mathPi :: Core.Name
mathPi = Core.Name "hydra.lib.math.pi"
-- | Name of the math.pow primitive
mathPow :: Core.Name
mathPow = Core.Name "hydra.lib.math.pow"
-- | Name of the math.range primitive
mathRange :: Core.Name
mathRange = Core.Name "hydra.lib.math.range"
-- | Name of the math.round primitive
mathRound :: Core.Name
mathRound = Core.Name "hydra.lib.math.round"
-- | Name of the math.roundFloat32 primitive
mathRoundFloat32 :: Core.Name
mathRoundFloat32 = Core.Name "hydra.lib.math.roundFloat32"
-- | Name of the math.roundFloat64 primitive
mathRoundFloat64 :: Core.Name
mathRoundFloat64 = Core.Name "hydra.lib.math.roundFloat64"
-- | Name of the math.signum primitive
mathSignum :: Core.Name
mathSignum = Core.Name "hydra.lib.math.signum"
-- | Name of the math.sin primitive
mathSin :: Core.Name
mathSin = Core.Name "hydra.lib.math.sin"
-- | Name of the math.sinh primitive
mathSinh :: Core.Name
mathSinh = Core.Name "hydra.lib.math.sinh"
-- | Name of the math.sqrt primitive
mathSqrt :: Core.Name
mathSqrt = Core.Name "hydra.lib.math.sqrt"
-- | Name of the math.sub primitive
mathSub :: Core.Name
mathSub = Core.Name "hydra.lib.math.sub"
-- | Name of the math.subFloat64 primitive
mathSubFloat64 :: Core.Name
mathSubFloat64 = Core.Name "hydra.lib.math.subFloat64"
-- | Name of the math.tan primitive
mathTan :: Core.Name
mathTan = Core.Name "hydra.lib.math.tan"
-- | Name of the math.tanh primitive
mathTanh :: Core.Name
mathTanh = Core.Name "hydra.lib.math.tanh"
-- | Name of the math.truncate primitive
mathTruncate :: Core.Name
mathTruncate = Core.Name "hydra.lib.math.truncate"
-- | ModuleName of the maybes library
maybes :: Packaging.ModuleName
maybes = Packaging.ModuleName "hydra.lib.maybes"
-- | Name of the maybes.apply primitive
maybesApply :: Core.Name
maybesApply = Core.Name "hydra.lib.maybes.apply"
-- | Name of the maybes.bind primitive
maybesBind :: Core.Name
maybesBind = Core.Name "hydra.lib.maybes.bind"
-- | Name of the maybes.cases primitive
maybesCases :: Core.Name
maybesCases = Core.Name "hydra.lib.maybes.cases"
-- | Name of the maybes.cat primitive
maybesCat :: Core.Name
maybesCat = Core.Name "hydra.lib.maybes.cat"
-- | Name of the maybes.compose primitive
maybesCompose :: Core.Name
maybesCompose = Core.Name "hydra.lib.maybes.compose"
-- | Name of the maybes.fromMaybe primitive
maybesFromMaybe :: Core.Name
maybesFromMaybe = Core.Name "hydra.lib.maybes.fromMaybe"
-- | Name of the maybes.isJust primitive
maybesIsJust :: Core.Name
maybesIsJust = Core.Name "hydra.lib.maybes.isJust"
-- | Name of the maybes.isNothing primitive
maybesIsNothing :: Core.Name
maybesIsNothing = Core.Name "hydra.lib.maybes.isNothing"
-- | Name of the maybes.map primitive
maybesMap :: Core.Name
maybesMap = Core.Name "hydra.lib.maybes.map"
-- | Name of the maybes.mapMaybe primitive
maybesMapMaybe :: Core.Name
maybesMapMaybe = Core.Name "hydra.lib.maybes.mapMaybe"
-- | Name of the maybes.maybe primitive
maybesMaybe :: Core.Name
maybesMaybe = Core.Name "hydra.lib.maybes.maybe"
-- | Name of the maybes.pure primitive
maybesPure :: Core.Name
maybesPure = Core.Name "hydra.lib.maybes.pure"
-- | Name of the maybes.toList primitive
maybesToList :: Core.Name
maybesToList = Core.Name "hydra.lib.maybes.toList"
-- | ModuleName of the pairs library
pairs :: Packaging.ModuleName
pairs = Packaging.ModuleName "hydra.lib.pairs"
-- | Name of the pairs.bimap primitive
pairsBimap :: Core.Name
pairsBimap = Core.Name "hydra.lib.pairs.bimap"
-- | Name of the pairs.first primitive
pairsFirst :: Core.Name
pairsFirst = Core.Name "hydra.lib.pairs.first"
-- | Name of the pairs.second primitive
pairsSecond :: Core.Name
pairsSecond = Core.Name "hydra.lib.pairs.second"
-- | ModuleName of the regex library
regex :: Packaging.ModuleName
regex = Packaging.ModuleName "hydra.lib.regex"
-- | Name of the regex.find primitive
regexFind :: Core.Name
regexFind = Core.Name "hydra.lib.regex.find"
-- | Name of the regex.findAll primitive
regexFindAll :: Core.Name
regexFindAll = Core.Name "hydra.lib.regex.findAll"
-- | Name of the regex.matches primitive
regexMatches :: Core.Name
regexMatches = Core.Name "hydra.lib.regex.matches"
-- | Name of the regex.replace primitive
regexReplace :: Core.Name
regexReplace = Core.Name "hydra.lib.regex.replace"
-- | Name of the regex.replaceAll primitive
regexReplaceAll :: Core.Name
regexReplaceAll = Core.Name "hydra.lib.regex.replaceAll"
-- | Name of the regex.split primitive
regexSplit :: Core.Name
regexSplit = Core.Name "hydra.lib.regex.split"
-- | ModuleName of the sets library
sets :: Packaging.ModuleName
sets = Packaging.ModuleName "hydra.lib.sets"
-- | Name of the sets.delete primitive
setsDelete :: Core.Name
setsDelete = Core.Name "hydra.lib.sets.delete"
-- | Name of the sets.difference primitive
setsDifference :: Core.Name
setsDifference = Core.Name "hydra.lib.sets.difference"
-- | Name of the sets.empty primitive
setsEmpty :: Core.Name
setsEmpty = Core.Name "hydra.lib.sets.empty"
-- | Name of the sets.fromList primitive
setsFromList :: Core.Name
setsFromList = Core.Name "hydra.lib.sets.fromList"
-- | Name of the sets.insert primitive
setsInsert :: Core.Name
setsInsert = Core.Name "hydra.lib.sets.insert"
-- | Name of the sets.intersection primitive
setsIntersection :: Core.Name
setsIntersection = Core.Name "hydra.lib.sets.intersection"
-- | Name of the sets.map primitive
setsMap :: Core.Name
setsMap = Core.Name "hydra.lib.sets.map"
-- | Name of the sets.member primitive
setsMember :: Core.Name
setsMember = Core.Name "hydra.lib.sets.member"
-- | Name of the sets.null primitive
setsNull :: Core.Name
setsNull = Core.Name "hydra.lib.sets.null"
-- | Name of the sets.singleton primitive
setsSingleton :: Core.Name
setsSingleton = Core.Name "hydra.lib.sets.singleton"
-- | Name of the sets.size primitive
setsSize :: Core.Name
setsSize = Core.Name "hydra.lib.sets.size"
-- | Name of the sets.toList primitive
setsToList :: Core.Name
setsToList = Core.Name "hydra.lib.sets.toList"
-- | Name of the sets.union primitive
setsUnion :: Core.Name
setsUnion = Core.Name "hydra.lib.sets.union"
-- | Name of the sets.unions primitive
setsUnions :: Core.Name
setsUnions = Core.Name "hydra.lib.sets.unions"
-- | ModuleName of the strings library
strings :: Packaging.ModuleName
strings = Packaging.ModuleName "hydra.lib.strings"
-- | Name of the strings.cat primitive
stringsCat :: Core.Name
stringsCat = Core.Name "hydra.lib.strings.cat"
-- | Name of the strings.cat2 primitive
stringsCat2 :: Core.Name
stringsCat2 = Core.Name "hydra.lib.strings.cat2"
-- | Name of the strings.fromList primitive
stringsFromList :: Core.Name
stringsFromList = Core.Name "hydra.lib.strings.fromList"
-- | Name of the strings.intercalate primitive
stringsIntercalate :: Core.Name
stringsIntercalate = Core.Name "hydra.lib.strings.intercalate"
-- | Name of the strings.length primitive
stringsLength :: Core.Name
stringsLength = Core.Name "hydra.lib.strings.length"
-- | Name of the strings.lines primitive
stringsLines :: Core.Name
stringsLines = Core.Name "hydra.lib.strings.lines"
-- | Name of the strings.maybeCharAt primitive
stringsMaybeCharAt :: Core.Name
stringsMaybeCharAt = Core.Name "hydra.lib.strings.maybeCharAt"
-- | Name of the strings.null primitive
stringsNull :: Core.Name
stringsNull = Core.Name "hydra.lib.strings.null"
-- | Name of the strings.splitOn primitive
stringsSplitOn :: Core.Name
stringsSplitOn = Core.Name "hydra.lib.strings.splitOn"
-- | Name of the strings.toList primitive
stringsToList :: Core.Name
stringsToList = Core.Name "hydra.lib.strings.toList"
-- | Name of the strings.toLower primitive
stringsToLower :: Core.Name
stringsToLower = Core.Name "hydra.lib.strings.toLower"
-- | Name of the strings.toUpper primitive
stringsToUpper :: Core.Name
stringsToUpper = Core.Name "hydra.lib.strings.toUpper"
-- | Name of the strings.unlines primitive
stringsUnlines :: Core.Name
stringsUnlines = Core.Name "hydra.lib.strings.unlines"
-- | ModuleName of the typeclass library
typeclass :: Packaging.ModuleName
typeclass = Packaging.ModuleName "hydra.typeclass"
-- | Name of the typeclass.Eq primitive
typeclassEq :: Core.Name
typeclassEq = Core.Name "hydra.typeclass.Eq"
-- | Name of the typeclass.Ord primitive
typeclassOrd :: Core.Name
typeclassOrd = Core.Name "hydra.typeclass.Ord"
