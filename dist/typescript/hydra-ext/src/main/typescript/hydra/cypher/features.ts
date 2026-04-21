// Note: this is an automatically generated file. Do not edit.

/**
 * A model for characterizing OpenCypher queries and implementations in terms of included features.Based on the OpenCypher grammar and the list of standard Cypher functions at https://neo4j.com/docs/cypher-manual/current/functions. Current as of August 2024.
 */



import * as Core from "../core.js";

export interface CypherFeatures {
  readonly arithmetic: ArithmeticFeatures;
  readonly atom: AtomFeatures;
  readonly comparison: ComparisonFeatures;
  readonly delete: DeleteFeatures;
  readonly function: FunctionFeatures;
  readonly list: ListFeatures;
  readonly literal: LiteralFeatures;
  readonly logical: LogicalFeatures;
  readonly match: MatchFeatures;
  readonly merge: MergeFeatures;
  readonly nodePattern: NodePatternFeatures;
  readonly null: NullFeatures;
  readonly path: PathFeatures;
  readonly procedureCall: ProcedureCallFeatures;
  readonly projection: ProjectionFeatures;
  readonly quantifier: QuantifierFeatures;
  readonly rangeLiteral: RangeLiteralFeatures;
  readonly reading: ReadingFeatures;
  readonly relationshipDirection: RelationshipDirectionFeatures;
  readonly relationshipPattern: RelationshipPatternFeatures;
  readonly remove: RemoveFeatures;
  readonly set: SetFeatures;
  readonly string: StringFeatures;
  readonly updating: UpdatingFeatures;
}

export interface ArithmeticFeatures {
  readonly plus: boolean;
  readonly minus: boolean;
  readonly multiply: boolean;
  readonly divide: boolean;
  readonly modulus: boolean;
  readonly powerOf: boolean;
}

export interface AtomFeatures {
  readonly caseExpression: boolean;
  readonly count: boolean;
  readonly existentialSubquery: boolean;
  readonly functionInvocation: boolean;
  readonly parameter: boolean;
  readonly patternComprehension: boolean;
  readonly patternPredicate: boolean;
  readonly variable: boolean;
}

export interface ComparisonFeatures {
  readonly equal: boolean;
  readonly greaterThan: boolean;
  readonly greaterThanOrEqual: boolean;
  readonly lessThan: boolean;
  readonly lessThanOrEqual: boolean;
  readonly notEqual: boolean;
}

export interface DeleteFeatures {
  readonly delete: boolean;
  readonly detachDelete: boolean;
}

export interface FunctionFeatures {
  readonly aggregateFunction: AggregateFunctionFeatures;
  readonly databaseFunction: DatabaseFunctionFeatures;
  readonly genAIFunction: GenAIFunctionFeatures;
  readonly graphFunction: GraphFunctionFeatures;
  readonly listFunction: ListFunctionFeatures;
  readonly loadCSVFunction: LoadCSVFunctionFeatures;
  readonly logarithmicFunction: LogarithmicFunctionFeatures;
  readonly numericFunction: NumericFunctionFeatures;
  readonly predicateFunction: PredicateFunctionFeatures;
  readonly scalarFunction: ScalarFunctionFeatures;
  readonly spatialFunction: SpatialFunctionFeatures;
  readonly stringFunction: StringFunctionFeatures;
  readonly temporalDurationFunction: TemporalDurationFunctionFeatures;
  readonly temporalInstantFunction: TemporalInstantFunctionFeatures;
  readonly trigonometricFunction: TrigonometricFunctionFeatures;
  readonly vectorFunction: VectorFunctionFeatures;
}

export interface AggregateFunctionFeatures {
  readonly avg: boolean;
  readonly collect: boolean;
  readonly count: boolean;
  readonly max: boolean;
  readonly min: boolean;
  readonly percentileCont: boolean;
  readonly percentileDisc: boolean;
  readonly stdev: boolean;
  readonly stdevp: boolean;
  readonly sum: boolean;
}

export interface DatabaseFunctionFeatures {
  readonly dbNameFromElementId: boolean;
}

export interface GenAIFunctionFeatures {
  readonly genaiVectorEncode: boolean;
}

export interface GraphFunctionFeatures {
  readonly graphByElementId: boolean;
  readonly graphByName: boolean;
  readonly graphNames: boolean;
  readonly graphPropertiesByName: boolean;
}

export interface ListFunctionFeatures {
  readonly keys: boolean;
  readonly labels: boolean;
  readonly nodes: boolean;
  readonly range: boolean;
  readonly reduce: boolean;
  readonly relationships: boolean;
  readonly reverse: boolean;
  readonly tail: boolean;
  readonly toBooleanList: boolean;
  readonly toFloatList: boolean;
  readonly toIntegerList: boolean;
  readonly toStringList: boolean;
}

export interface LoadCSVFunctionFeatures {
  readonly file: boolean;
  readonly linenumber: boolean;
}

export interface LogarithmicFunctionFeatures {
  readonly e: boolean;
  readonly exp: boolean;
  readonly log: boolean;
  readonly log10: boolean;
  readonly sqrt: boolean;
}

export interface NumericFunctionFeatures {
  readonly abs: boolean;
  readonly ceil: boolean;
  readonly floor: boolean;
  readonly isNaN: boolean;
  readonly rand: boolean;
  readonly round: boolean;
  readonly sign: boolean;
}

export interface PredicateFunctionFeatures {
  readonly all: boolean;
  readonly any: boolean;
  readonly exists: boolean;
  readonly isEmpty: boolean;
  readonly none: boolean;
  readonly single: boolean;
}

export interface ScalarFunctionFeatures {
  readonly char_length: boolean;
  readonly character_length: boolean;
  readonly coalesce: boolean;
  readonly elementId: boolean;
  readonly endNode: boolean;
  readonly head: boolean;
  readonly id: boolean;
  readonly last: boolean;
  readonly length: boolean;
  readonly nullIf: boolean;
  readonly properties: boolean;
  readonly randomUUID: boolean;
  readonly size: boolean;
  readonly startNode: boolean;
  readonly toBoolean: boolean;
  readonly toBooleanOrNull: boolean;
  readonly toFloat: boolean;
  readonly toFloatOrNull: boolean;
  readonly toInteger: boolean;
  readonly toIntegerOrNull: boolean;
  readonly type: boolean;
  readonly valueType: boolean;
}

export interface SpatialFunctionFeatures {
  readonly pointDistance: boolean;
  readonly point: boolean;
  readonly pointWithinBBox: boolean;
}

export interface StringFunctionFeatures {
  readonly btrim: boolean;
  readonly left: boolean;
  readonly lower: boolean;
  readonly ltrim: boolean;
  readonly normalize: boolean;
  readonly replace: boolean;
  readonly reverse: boolean;
  readonly right: boolean;
  readonly rtrim: boolean;
  readonly split: boolean;
  readonly substring: boolean;
  readonly toLower: boolean;
  readonly toString: boolean;
  readonly toStringOrNull: boolean;
  readonly toUpper: boolean;
  readonly trim: boolean;
  readonly upper: boolean;
}

export interface TemporalDurationFunctionFeatures {
  readonly duration: boolean;
  readonly durationBetween: boolean;
  readonly durationInDays: boolean;
  readonly durationInMonths: boolean;
  readonly durationInSeconds: boolean;
}

export interface TemporalInstantFunctionFeatures {
  readonly date: boolean;
  readonly dateRealtime: boolean;
  readonly dateStatement: boolean;
  readonly dateTransaction: boolean;
  readonly dateTruncate: boolean;
  readonly datetime: boolean;
  readonly datetimeFromepoch: boolean;
  readonly datetimeFromepochmillis: boolean;
  readonly datetimeRealtime: boolean;
  readonly datetimeStatement: boolean;
  readonly datetimeTransaction: boolean;
  readonly datetimeTruncate: boolean;
  readonly localdatetime: boolean;
  readonly localdatetimeRealtime: boolean;
  readonly localdatetimeStatement: boolean;
  readonly localdatetimeTransaction: boolean;
  readonly localdatetimeTruncate: boolean;
  readonly localtime: boolean;
  readonly localtimeRealtime: boolean;
  readonly localtimeStatement: boolean;
  readonly localtimeTransaction: boolean;
  readonly localtimeTruncate: boolean;
  readonly time: boolean;
  readonly timeRealtime: boolean;
  readonly timeStatement: boolean;
  readonly timeTransaction: boolean;
  readonly timeTruncate: boolean;
}

export interface TrigonometricFunctionFeatures {
  readonly acos: boolean;
  readonly asin: boolean;
  readonly atan: boolean;
  readonly atan2: boolean;
  readonly cos: boolean;
  readonly cot: boolean;
  readonly degrees: boolean;
  readonly haversin: boolean;
  readonly pi: boolean;
  readonly radians: boolean;
  readonly sin: boolean;
  readonly tan: boolean;
}

export interface VectorFunctionFeatures {
  readonly vectorSimilarityCosine: boolean;
  readonly vectorSimilarityEuclidean: boolean;
}

export interface ListFeatures {
  readonly listComprehension: boolean;
  readonly listRange: boolean;
}

export interface LiteralFeatures {
  readonly boolean: boolean;
  readonly double: boolean;
  readonly integer: boolean;
  readonly list: boolean;
  readonly map: boolean;
  readonly null: boolean;
  readonly string: boolean;
}

export interface LogicalFeatures {
  readonly and: boolean;
  readonly not: boolean;
  readonly or: boolean;
  readonly xor: boolean;
}

export interface MatchFeatures {
  readonly match: boolean;
  readonly optionalMatch: boolean;
}

export interface MergeFeatures {
  readonly merge: boolean;
  readonly mergeOnCreate: boolean;
  readonly mergeOnMatch: boolean;
}

export interface NodePatternFeatures {
  readonly multipleLabels: boolean;
  readonly parameter: boolean;
  readonly propertyMap: boolean;
  readonly variableNode: boolean;
  readonly wildcardLabel: boolean;
}

export interface NullFeatures {
  readonly isNull: boolean;
  readonly isNotNull: boolean;
}

export interface PathFeatures {
  readonly shortestPath: boolean;
}

export interface ProcedureCallFeatures {
  readonly inQueryCall: boolean;
  readonly standaloneCall: boolean;
  readonly yield: boolean;
}

export interface ProjectionFeatures {
  readonly limit: boolean;
  readonly orderBy: boolean;
  readonly projectDistinct: boolean;
  readonly projectAll: boolean;
  readonly projectAs: boolean;
  readonly skip: boolean;
  readonly sortOrder: boolean;
}

export interface QuantifierFeatures {
  readonly all: boolean;
  readonly any: boolean;
  readonly none: boolean;
  readonly single: boolean;
}

export interface RangeLiteralFeatures {
  readonly bounds: boolean;
  readonly exactRange: boolean;
  readonly lowerBound: boolean;
  readonly starRange: boolean;
  readonly upperBound: boolean;
}

export interface ReadingFeatures {
  readonly union: boolean;
  readonly unionAll: boolean;
  readonly unwind: boolean;
}

export interface RelationshipDirectionFeatures {
  readonly both: boolean;
  readonly left: boolean;
  readonly neither: boolean;
  readonly right: boolean;
}

export interface RelationshipPatternFeatures {
  readonly multipleTypes: boolean;
  readonly variableRelationship: boolean;
  readonly wildcardType: boolean;
}

export interface RemoveFeatures {
  readonly byLabel: boolean;
  readonly byProperty: boolean;
}

export interface SetFeatures {
  readonly propertyEquals: boolean;
  readonly variableEquals: boolean;
  readonly variablePlusEquals: boolean;
  readonly variableWithNodeLabels: boolean;
}

export interface StringFeatures {
  readonly contains: boolean;
  readonly endsWith: boolean;
  readonly in: boolean;
  readonly startsWith: boolean;
}

export interface UpdatingFeatures {
  readonly create: boolean;
  readonly set: boolean;
  readonly with: boolean;
}
