// Note: this is an automatically generated file. Do not edit.

/**
 * A module defining the graph used in the test suite.
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
import * as Lexical from "../lexical.js";
import * as LibMaps from "../lib/maps.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as TestTestTerms from "./testTerms.js";
import * as TestTestTypes from "./testTypes.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const testContext: Context.Context = Lexical.emptyContext;

export const testGraph: Graph.Graph = Lexical.emptyGraph;

export const testNamespace: Packaging.Namespace = "testGraph";

export const testSchemaNamespace: Packaging.Namespace = "testSchemaGraph";

export const testTerms: ReadonlyMap<Core.Name, Core.Term> = LibMaps.fromList([["testDataArthur", TestTestTerms.testDataArthur]]);

export const testTypes: ReadonlyMap<Core.Name, Core.Type> = LibMaps.fromList([[TestTestTypes.testTypeBuddyListAName, TestTestTypes.testTypeBuddyListA], [TestTestTypes.testTypeBuddyListBName, TestTestTypes.testTypeBuddyListB], [TestTestTypes.testTypeComparisonName, TestTestTypes.testTypeComparison], [TestTestTypes.testTypeEitherName, TestTestTypes.testTypeEither], [TestTestTypes.testTypeHydraLiteralTypeName, TestTestTypes.testTypeHydraLiteralType], [TestTestTypes.testTypeHydraTypeName, TestTestTypes.testTypeHydraType], [TestTestTypes.testTypeIntListName, TestTestTypes.testTypeIntList], [TestTestTypes.testTypeLatLonName, TestTestTypes.testTypeLatLon], [TestTestTypes.testTypeLatLonPolyName, TestTestTypes.testTypeLatLonPoly], [TestTestTypes.testTypeListName, TestTestTypes.testTypeList], [TestTestTypes.testTypeNumberName, TestTestTypes.testTypeNumber], [TestTestTypes.testTypePersonName, TestTestTypes.testTypePerson], [TestTestTypes.testTypePersonOrSomethingName, TestTestTypes.testTypePersonOrSomething], [TestTestTypes.testTypePolymorphicWrapperName, TestTestTypes.testTypePolymorphicWrapper], [TestTestTypes.testTypeSimpleNumberName, TestTestTypes.testTypeSimpleNumber], [TestTestTypes.testTypeStringAliasName, TestTestTypes.testTypeStringAlias], [TestTestTypes.testTypeSymmetricTripleName, TestTestTypes.testTypeSymmetricTriple], [TestTestTypes.testTypeTimestampName, TestTestTypes.testTypeTimestamp], [TestTestTypes.testTypeTripleName, TestTestTypes.testTypeTriple], [TestTestTypes.testTypeUnionMonomorphicName, TestTestTypes.testTypeUnionMonomorphic], [TestTestTypes.testTypeUnionPolymorphicRecursiveName, TestTestTypes.testTypeUnionPolymorphicRecursive], [TestTestTypes.testTypeUnitName, TestTestTypes.testTypeUnit]]);
