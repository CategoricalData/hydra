// Note: this is an automatically generated file. Do not edit.

/**
 * Namespaces and primitive names for the Hydra standard library
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const chars: Packaging.Namespace = "hydra.lib.chars";

export const charsIsAlphaNum: Core.Name = "hydra.lib.chars.isAlphaNum";

export const charsIsLower: Core.Name = "hydra.lib.chars.isLower";

export const charsIsSpace: Core.Name = "hydra.lib.chars.isSpace";

export const charsIsUpper: Core.Name = "hydra.lib.chars.isUpper";

export const charsToLower: Core.Name = "hydra.lib.chars.toLower";

export const charsToUpper: Core.Name = "hydra.lib.chars.toUpper";

export const eithers: Packaging.Namespace = "hydra.lib.eithers";

export const eithersBimap: Core.Name = "hydra.lib.eithers.bimap";

export const eithersBind: Core.Name = "hydra.lib.eithers.bind";

export const eithersEither: Core.Name = "hydra.lib.eithers.either";

export const eithersFoldl: Core.Name = "hydra.lib.eithers.foldl";

export const eithersFromLeft: Core.Name = "hydra.lib.eithers.fromLeft";

export const eithersFromRight: Core.Name = "hydra.lib.eithers.fromRight";

export const eithersIsLeft: Core.Name = "hydra.lib.eithers.isLeft";

export const eithersIsRight: Core.Name = "hydra.lib.eithers.isRight";

export const eithersLefts: Core.Name = "hydra.lib.eithers.lefts";

export const eithersMap: Core.Name = "hydra.lib.eithers.map";

export const eithersMapList: Core.Name = "hydra.lib.eithers.mapList";

export const eithersMapMaybe: Core.Name = "hydra.lib.eithers.mapMaybe";

export const eithersMapSet: Core.Name = "hydra.lib.eithers.mapSet";

export const eithersPartitionEithers: Core.Name = "hydra.lib.eithers.partitionEithers";

export const eithersRights: Core.Name = "hydra.lib.eithers.rights";

export const equality: Packaging.Namespace = "hydra.lib.equality";

export const equalityCompare: Core.Name = "hydra.lib.equality.compare";

export const equalityEqual: Core.Name = "hydra.lib.equality.equal";

export const equalityGt: Core.Name = "hydra.lib.equality.gt";

export const equalityGte: Core.Name = "hydra.lib.equality.gte";

export const equalityIdentity: Core.Name = "hydra.lib.equality.identity";

export const equalityLt: Core.Name = "hydra.lib.equality.lt";

export const equalityLte: Core.Name = "hydra.lib.equality.lte";

export const equalityMax: Core.Name = "hydra.lib.equality.max";

export const equalityMin: Core.Name = "hydra.lib.equality.min";

export const lists: Packaging.Namespace = "hydra.lib.lists";

export const listsApply: Core.Name = "hydra.lib.lists.apply";

export const listsAt: Core.Name = "hydra.lib.lists.at";

export const listsBind: Core.Name = "hydra.lib.lists.bind";

export const listsConcat: Core.Name = "hydra.lib.lists.concat";

export const listsConcat2: Core.Name = "hydra.lib.lists.concat2";

export const listsCons: Core.Name = "hydra.lib.lists.cons";

export const listsDrop: Core.Name = "hydra.lib.lists.drop";

export const listsDropWhile: Core.Name = "hydra.lib.lists.dropWhile";

export const listsElem: Core.Name = "hydra.lib.lists.elem";

export const listsFilter: Core.Name = "hydra.lib.lists.filter";

export const listsFind: Core.Name = "hydra.lib.lists.find";

export const listsFoldl: Core.Name = "hydra.lib.lists.foldl";

export const listsFoldr: Core.Name = "hydra.lib.lists.foldr";

export const listsGroup: Core.Name = "hydra.lib.lists.group";

export const listsHead: Core.Name = "hydra.lib.lists.head";

export const listsInit: Core.Name = "hydra.lib.lists.init";

export const listsIntercalate: Core.Name = "hydra.lib.lists.intercalate";

export const listsIntersperse: Core.Name = "hydra.lib.lists.intersperse";

export const listsLast: Core.Name = "hydra.lib.lists.last";

export const listsLength: Core.Name = "hydra.lib.lists.length";

export const listsMap: Core.Name = "hydra.lib.lists.map";

export const listsMaybeAt: Core.Name = "hydra.lib.lists.maybeAt";

export const listsMaybeHead: Core.Name = "hydra.lib.lists.maybeHead";

export const listsMaybeInit: Core.Name = "hydra.lib.lists.maybeInit";

export const listsMaybeLast: Core.Name = "hydra.lib.lists.maybeLast";

export const listsMaybeTail: Core.Name = "hydra.lib.lists.maybeTail";

export const listsNub: Core.Name = "hydra.lib.lists.nub";

export const listsNull: Core.Name = "hydra.lib.lists.null";

export const listsPartition: Core.Name = "hydra.lib.lists.partition";

export const listsPure: Core.Name = "hydra.lib.lists.pure";

export const listsReplicate: Core.Name = "hydra.lib.lists.replicate";

export const listsReverse: Core.Name = "hydra.lib.lists.reverse";

export const listsSafeHead: Core.Name = "hydra.lib.lists.safeHead";

export const listsSingleton: Core.Name = "hydra.lib.lists.singleton";

export const listsSort: Core.Name = "hydra.lib.lists.sort";

export const listsSortOn: Core.Name = "hydra.lib.lists.sortOn";

export const listsSpan: Core.Name = "hydra.lib.lists.span";

export const listsTail: Core.Name = "hydra.lib.lists.tail";

export const listsTake: Core.Name = "hydra.lib.lists.take";

export const listsTranspose: Core.Name = "hydra.lib.lists.transpose";

export const listsZip: Core.Name = "hydra.lib.lists.zip";

export const listsZipWith: Core.Name = "hydra.lib.lists.zipWith";

export const literals: Packaging.Namespace = "hydra.lib.literals";

export const literalsBigfloatToBigint: Core.Name = "hydra.lib.literals.bigfloatToBigint";

export const literalsBigfloatToFloat32: Core.Name = "hydra.lib.literals.bigfloatToFloat32";

export const literalsBigfloatToFloat64: Core.Name = "hydra.lib.literals.bigfloatToFloat64";

export const literalsBigintToBigfloat: Core.Name = "hydra.lib.literals.bigintToBigfloat";

export const literalsBigintToInt16: Core.Name = "hydra.lib.literals.bigintToInt16";

export const literalsBigintToInt32: Core.Name = "hydra.lib.literals.bigintToInt32";

export const literalsBigintToInt64: Core.Name = "hydra.lib.literals.bigintToInt64";

export const literalsBigintToInt8: Core.Name = "hydra.lib.literals.bigintToInt8";

export const literalsBigintToUint16: Core.Name = "hydra.lib.literals.bigintToUint16";

export const literalsBigintToUint32: Core.Name = "hydra.lib.literals.bigintToUint32";

export const literalsBigintToUint64: Core.Name = "hydra.lib.literals.bigintToUint64";

export const literalsBigintToUint8: Core.Name = "hydra.lib.literals.bigintToUint8";

export const literalsBinaryToBytes: Core.Name = "hydra.lib.literals.binaryToBytes";

export const literalsBinaryToString: Core.Name = "hydra.lib.literals.binaryToString";

export const literalsFloat32ToBigfloat: Core.Name = "hydra.lib.literals.float32ToBigfloat";

export const literalsFloat64ToBigfloat: Core.Name = "hydra.lib.literals.float64ToBigfloat";

export const literalsInt16ToBigint: Core.Name = "hydra.lib.literals.int16ToBigint";

export const literalsInt32ToBigint: Core.Name = "hydra.lib.literals.int32ToBigint";

export const literalsInt64ToBigint: Core.Name = "hydra.lib.literals.int64ToBigint";

export const literalsInt8ToBigint: Core.Name = "hydra.lib.literals.int8ToBigint";

export const literalsReadBigfloat: Core.Name = "hydra.lib.literals.readBigfloat";

export const literalsReadBigint: Core.Name = "hydra.lib.literals.readBigint";

export const literalsReadBoolean: Core.Name = "hydra.lib.literals.readBoolean";

export const literalsReadFloat32: Core.Name = "hydra.lib.literals.readFloat32";

export const literalsReadFloat64: Core.Name = "hydra.lib.literals.readFloat64";

export const literalsReadInt16: Core.Name = "hydra.lib.literals.readInt16";

export const literalsReadInt32: Core.Name = "hydra.lib.literals.readInt32";

export const literalsReadInt64: Core.Name = "hydra.lib.literals.readInt64";

export const literalsReadInt8: Core.Name = "hydra.lib.literals.readInt8";

export const literalsReadString: Core.Name = "hydra.lib.literals.readString";

export const literalsReadUint16: Core.Name = "hydra.lib.literals.readUint16";

export const literalsReadUint32: Core.Name = "hydra.lib.literals.readUint32";

export const literalsReadUint64: Core.Name = "hydra.lib.literals.readUint64";

export const literalsReadUint8: Core.Name = "hydra.lib.literals.readUint8";

export const literalsShowBigfloat: Core.Name = "hydra.lib.literals.showBigfloat";

export const literalsShowBigint: Core.Name = "hydra.lib.literals.showBigint";

export const literalsShowBoolean: Core.Name = "hydra.lib.literals.showBoolean";

export const literalsShowFloat32: Core.Name = "hydra.lib.literals.showFloat32";

export const literalsShowFloat64: Core.Name = "hydra.lib.literals.showFloat64";

export const literalsShowInt16: Core.Name = "hydra.lib.literals.showInt16";

export const literalsShowInt32: Core.Name = "hydra.lib.literals.showInt32";

export const literalsShowInt64: Core.Name = "hydra.lib.literals.showInt64";

export const literalsShowInt8: Core.Name = "hydra.lib.literals.showInt8";

export const literalsShowString: Core.Name = "hydra.lib.literals.showString";

export const literalsShowUint16: Core.Name = "hydra.lib.literals.showUint16";

export const literalsShowUint32: Core.Name = "hydra.lib.literals.showUint32";

export const literalsShowUint64: Core.Name = "hydra.lib.literals.showUint64";

export const literalsShowUint8: Core.Name = "hydra.lib.literals.showUint8";

export const literalsStringToBinary: Core.Name = "hydra.lib.literals.stringToBinary";

export const literalsUint16ToBigint: Core.Name = "hydra.lib.literals.uint16ToBigint";

export const literalsUint32ToBigint: Core.Name = "hydra.lib.literals.uint32ToBigint";

export const literalsUint64ToBigint: Core.Name = "hydra.lib.literals.uint64ToBigint";

export const literalsUint8ToBigint: Core.Name = "hydra.lib.literals.uint8ToBigint";

export const logic: Packaging.Namespace = "hydra.lib.logic";

export const logicAnd: Core.Name = "hydra.lib.logic.and";

export const logicIfElse: Core.Name = "hydra.lib.logic.ifElse";

export const logicNot: Core.Name = "hydra.lib.logic.not";

export const logicOr: Core.Name = "hydra.lib.logic.or";

export const maps: Packaging.Namespace = "hydra.lib.maps";

export const mapsAlter: Core.Name = "hydra.lib.maps.alter";

export const mapsBimap: Core.Name = "hydra.lib.maps.bimap";

export const mapsDelete: Core.Name = "hydra.lib.maps.delete";

export const mapsElems: Core.Name = "hydra.lib.maps.elems";

export const mapsEmpty: Core.Name = "hydra.lib.maps.empty";

export const mapsFilter: Core.Name = "hydra.lib.maps.filter";

export const mapsFilterWithKey: Core.Name = "hydra.lib.maps.filterWithKey";

export const mapsFindWithDefault: Core.Name = "hydra.lib.maps.findWithDefault";

export const mapsFromList: Core.Name = "hydra.lib.maps.fromList";

export const mapsInsert: Core.Name = "hydra.lib.maps.insert";

export const mapsKeys: Core.Name = "hydra.lib.maps.keys";

export const mapsLookup: Core.Name = "hydra.lib.maps.lookup";

export const mapsMap: Core.Name = "hydra.lib.maps.map";

export const mapsMapKeys: Core.Name = "hydra.lib.maps.mapKeys";

export const mapsMember: Core.Name = "hydra.lib.maps.member";

export const mapsNull: Core.Name = "hydra.lib.maps.null";

export const mapsSingleton: Core.Name = "hydra.lib.maps.singleton";

export const mapsSize: Core.Name = "hydra.lib.maps.size";

export const mapsToList: Core.Name = "hydra.lib.maps.toList";

export const mapsUnion: Core.Name = "hydra.lib.maps.union";

export const math: Packaging.Namespace = "hydra.lib.math";

export const mathAbs: Core.Name = "hydra.lib.math.abs";

export const mathAcos: Core.Name = "hydra.lib.math.acos";

export const mathAcosh: Core.Name = "hydra.lib.math.acosh";

export const mathAdd: Core.Name = "hydra.lib.math.add";

export const mathAddFloat64: Core.Name = "hydra.lib.math.addFloat64";

export const mathAsin: Core.Name = "hydra.lib.math.asin";

export const mathAsinh: Core.Name = "hydra.lib.math.asinh";

export const mathAtan: Core.Name = "hydra.lib.math.atan";

export const mathAtan2: Core.Name = "hydra.lib.math.atan2";

export const mathAtanh: Core.Name = "hydra.lib.math.atanh";

export const mathCeiling: Core.Name = "hydra.lib.math.ceiling";

export const mathCos: Core.Name = "hydra.lib.math.cos";

export const mathCosh: Core.Name = "hydra.lib.math.cosh";

export const mathDiv: Core.Name = "hydra.lib.math.div";

export const mathE: Core.Name = "hydra.lib.math.e";

export const mathEven: Core.Name = "hydra.lib.math.even";

export const mathExp: Core.Name = "hydra.lib.math.exp";

export const mathFloor: Core.Name = "hydra.lib.math.floor";

export const mathLog: Core.Name = "hydra.lib.math.log";

export const mathLogBase: Core.Name = "hydra.lib.math.logBase";

export const mathMax: Core.Name = "hydra.lib.math.max";

export const mathMaybeDiv: Core.Name = "hydra.lib.math.maybeDiv";

export const mathMaybeMod: Core.Name = "hydra.lib.math.maybeMod";

export const mathMaybePred: Core.Name = "hydra.lib.math.maybePred";

export const mathMaybeRem: Core.Name = "hydra.lib.math.maybeRem";

export const mathMaybeSucc: Core.Name = "hydra.lib.math.maybeSucc";

export const mathMin: Core.Name = "hydra.lib.math.min";

export const mathMod: Core.Name = "hydra.lib.math.mod";

export const mathMul: Core.Name = "hydra.lib.math.mul";

export const mathMulFloat64: Core.Name = "hydra.lib.math.mulFloat64";

export const mathNegate: Core.Name = "hydra.lib.math.negate";

export const mathNegateFloat64: Core.Name = "hydra.lib.math.negateFloat64";

export const mathOdd: Core.Name = "hydra.lib.math.odd";

export const mathPi: Core.Name = "hydra.lib.math.pi";

export const mathPow: Core.Name = "hydra.lib.math.pow";

export const mathPred: Core.Name = "hydra.lib.math.pred";

export const mathRange: Core.Name = "hydra.lib.math.range";

export const mathRem: Core.Name = "hydra.lib.math.rem";

export const mathRound: Core.Name = "hydra.lib.math.round";

export const mathRoundBigfloat: Core.Name = "hydra.lib.math.roundBigfloat";

export const mathRoundFloat32: Core.Name = "hydra.lib.math.roundFloat32";

export const mathRoundFloat64: Core.Name = "hydra.lib.math.roundFloat64";

export const mathSignum: Core.Name = "hydra.lib.math.signum";

export const mathSin: Core.Name = "hydra.lib.math.sin";

export const mathSinh: Core.Name = "hydra.lib.math.sinh";

export const mathSqrt: Core.Name = "hydra.lib.math.sqrt";

export const mathSub: Core.Name = "hydra.lib.math.sub";

export const mathSubFloat64: Core.Name = "hydra.lib.math.subFloat64";

export const mathSucc: Core.Name = "hydra.lib.math.succ";

export const mathTan: Core.Name = "hydra.lib.math.tan";

export const mathTanh: Core.Name = "hydra.lib.math.tanh";

export const mathTruncate: Core.Name = "hydra.lib.math.truncate";

export const maybes: Packaging.Namespace = "hydra.lib.maybes";

export const maybesApply: Core.Name = "hydra.lib.maybes.apply";

export const maybesBind: Core.Name = "hydra.lib.maybes.bind";

export const maybesCases: Core.Name = "hydra.lib.maybes.cases";

export const maybesCat: Core.Name = "hydra.lib.maybes.cat";

export const maybesCompose: Core.Name = "hydra.lib.maybes.compose";

export const maybesFromJust: Core.Name = "hydra.lib.maybes.fromJust";

export const maybesFromMaybe: Core.Name = "hydra.lib.maybes.fromMaybe";

export const maybesIsJust: Core.Name = "hydra.lib.maybes.isJust";

export const maybesIsNothing: Core.Name = "hydra.lib.maybes.isNothing";

export const maybesMap: Core.Name = "hydra.lib.maybes.map";

export const maybesMapMaybe: Core.Name = "hydra.lib.maybes.mapMaybe";

export const maybesMaybe: Core.Name = "hydra.lib.maybes.maybe";

export const maybesPure: Core.Name = "hydra.lib.maybes.pure";

export const maybesToList: Core.Name = "hydra.lib.maybes.toList";

export const pairs: Packaging.Namespace = "hydra.lib.pairs";

export const pairsBimap: Core.Name = "hydra.lib.pairs.bimap";

export const pairsFirst: Core.Name = "hydra.lib.pairs.first";

export const pairsSecond: Core.Name = "hydra.lib.pairs.second";

export const regex: Packaging.Namespace = "hydra.lib.regex";

export const regexFind: Core.Name = "hydra.lib.regex.find";

export const regexFindAll: Core.Name = "hydra.lib.regex.findAll";

export const regexMatches: Core.Name = "hydra.lib.regex.matches";

export const regexReplace: Core.Name = "hydra.lib.regex.replace";

export const regexReplaceAll: Core.Name = "hydra.lib.regex.replaceAll";

export const regexSplit: Core.Name = "hydra.lib.regex.split";

export const sets: Packaging.Namespace = "hydra.lib.sets";

export const setsDelete: Core.Name = "hydra.lib.sets.delete";

export const setsDifference: Core.Name = "hydra.lib.sets.difference";

export const setsEmpty: Core.Name = "hydra.lib.sets.empty";

export const setsFromList: Core.Name = "hydra.lib.sets.fromList";

export const setsInsert: Core.Name = "hydra.lib.sets.insert";

export const setsIntersection: Core.Name = "hydra.lib.sets.intersection";

export const setsMap: Core.Name = "hydra.lib.sets.map";

export const setsMember: Core.Name = "hydra.lib.sets.member";

export const setsNull: Core.Name = "hydra.lib.sets.null";

export const setsSingleton: Core.Name = "hydra.lib.sets.singleton";

export const setsSize: Core.Name = "hydra.lib.sets.size";

export const setsToList: Core.Name = "hydra.lib.sets.toList";

export const setsUnion: Core.Name = "hydra.lib.sets.union";

export const setsUnions: Core.Name = "hydra.lib.sets.unions";

export const strings: Packaging.Namespace = "hydra.lib.strings";

export const stringsCat: Core.Name = "hydra.lib.strings.cat";

export const stringsCat2: Core.Name = "hydra.lib.strings.cat2";

export const stringsCharAt: Core.Name = "hydra.lib.strings.charAt";

export const stringsFromList: Core.Name = "hydra.lib.strings.fromList";

export const stringsIntercalate: Core.Name = "hydra.lib.strings.intercalate";

export const stringsLength: Core.Name = "hydra.lib.strings.length";

export const stringsLines: Core.Name = "hydra.lib.strings.lines";

export const stringsMaybeCharAt: Core.Name = "hydra.lib.strings.maybeCharAt";

export const stringsNull: Core.Name = "hydra.lib.strings.null";

export const stringsSplitOn: Core.Name = "hydra.lib.strings.splitOn";

export const stringsToList: Core.Name = "hydra.lib.strings.toList";

export const stringsToLower: Core.Name = "hydra.lib.strings.toLower";

export const stringsToUpper: Core.Name = "hydra.lib.strings.toUpper";

export const stringsUnlines: Core.Name = "hydra.lib.strings.unlines";

export const typeclass: Packaging.Namespace = "hydra.typeclass";

export const typeclassEq: Core.Name = "hydra.typeclass.Eq";

export const typeclassOrd: Core.Name = "hydra.typeclass.Ord";
