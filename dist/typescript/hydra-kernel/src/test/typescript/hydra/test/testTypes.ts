// Note: this is an automatically generated file. Do not edit.

/**
 * Type definitions for the test suite
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

export const compareStringsType: Core.Type = ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) });

export const concatType: Core.Type = ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) });

export const eitherStringOrInt8Type: Core.Type = ({ tag: "union", value: [({
    name: "left",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "right",
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8" }) }) })
  })] });

export const eitherStringOrInt8TypeName: Core.Name = "EitherStringOrInt8";

export const exampleProjectionType: Core.Type = ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: testTypePersonName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) });

export const listOfInt16sType: Core.Type = ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16" }) }) }) });

export const listOfInt8sType: Core.Type = ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8" }) }) }) });

export const listOfListsOfStringsType: Core.Type = ({ tag: "list", value: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) });

export const listOfSetOfStringsType: Core.Type = ({ tag: "list", value: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) });

export const listOfStringsType: Core.Type = ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) });

export const mapOfStringsToIntsType: Core.Type = ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) });

export const optionalInt16Type: Core.Type = ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16" }) }) }) });

export const optionalInt8Type: Core.Type = ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8" }) }) }) });

export const optionalStringType: Core.Type = ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) });

export const setOfStringsType: Core.Type = ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "string" }) }) });

export const stringOrIntName: Core.Name = "StringOrInt";

export const stringOrIntType: Core.Type = ({ tag: "union", value: [({
    name: "left",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "right",
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  })] });

export const testTypeBuddyListA: Core.Type = ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "record", value: [({
    name: "head",
    type: ({ tag: "variable", value: "a" })
  }), ({
    name: "tail",
    type: ({ tag: "maybe", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: testTypeBuddyListBName }),
    argument: ({ tag: "variable", value: "a" })
  }) }) })
  })] })
  }) });

export const testTypeBuddyListAName: Core.Name = "BuddyListA";

export const testTypeBuddyListB: Core.Type = ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "record", value: [({
    name: "head",
    type: ({ tag: "variable", value: "a" })
  }), ({
    name: "tail",
    type: ({ tag: "maybe", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: testTypeBuddyListAName }),
    argument: ({ tag: "variable", value: "a" })
  }) }) })
  })] })
  }) });

export const testTypeBuddyListBName: Core.Name = "BuddyListB";

export const testTypeComparison: Core.Type = ({ tag: "union", value: [({
    name: "lessThan",
    type: ({ tag: "unit" })
  }), ({
    name: "equalTo",
    type: ({ tag: "unit" })
  }), ({
    name: "greaterThan",
    type: ({ tag: "unit" })
  })] });

export const testTypeComparisonName: Core.Name = "Comparison";

export const testTypeEither: Core.Type = ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "forall", value: ({
    parameter: "b",
    body: ({ tag: "union", value: [({
    name: "left",
    type: ({ tag: "variable", value: "a" })
  }), ({
    name: "right",
    type: ({ tag: "variable", value: "b" })
  })] })
  }) })
  }) });

export const testTypeEitherName: Core.Name = "Either";

export const testTypeHydraLiteralType: Core.Type = ({ tag: "union", value: [({
    name: "boolean",
    type: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }), ({
    name: "string",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  })] });

export const testTypeHydraLiteralTypeName: Core.Name = "HydraLiteralType";

export const testTypeHydraType: Core.Type = ({ tag: "union", value: [({
    name: "literal",
    type: ({ tag: "variable", value: testTypeHydraLiteralTypeName })
  }), ({
    name: "list",
    type: ({ tag: "variable", value: testTypeHydraTypeName })
  })] });

export const testTypeHydraTypeName: Core.Name = "HydraType";

export const testTypeIntList: Core.Type = ({ tag: "record", value: [({
    name: "head",
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }), ({
    name: "tail",
    type: ({ tag: "maybe", value: ({ tag: "variable", value: testTypeIntListName }) })
  })] });

export const testTypeIntListName: Core.Name = "IntList";

export const testTypeLatLon: Core.Type = ({ tag: "record", value: [({
    name: "lat",
    type: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  }), ({
    name: "lon",
    type: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  })] });

export const testTypeLatLonName: Core.Name = "LatLon";

export const testTypeLatLonPoly: Core.Type = ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "record", value: [({
    name: "lat",
    type: ({ tag: "variable", value: "a" })
  }), ({
    name: "lon",
    type: ({ tag: "variable", value: "a" })
  })] })
  }) });

export const testTypeLatLonPolyName: Core.Name = "LatLonPoly";

export const testTypeList: Core.Type = ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "record", value: [({
    name: "head",
    type: ({ tag: "variable", value: "a" })
  }), ({
    name: "tail",
    type: ({ tag: "maybe", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: testTypeListName }),
    argument: ({ tag: "variable", value: "a" })
  }) }) })
  })] })
  }) });

export const testTypeListName: Core.Name = "List";

export const testTypeName: Core.Name = "Test";

export const testTypeNumber: Core.Type = ({ tag: "union", value: [({
    name: "int",
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }), ({
    name: "float",
    type: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  })] });

export const testTypeNumberName: Core.Name = "Number";

export const testTypePerson: Core.Type = ({ tag: "record", value: [({
    name: "firstName",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "lastName",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "age",
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  })] });

export const testTypePersonName: Core.Name = "Person";

export const testTypePersonOrSomething: Core.Type = ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "union", value: [({
    name: "person",
    type: ({ tag: "variable", value: testTypePersonName })
  }), ({
    name: "other",
    type: ({ tag: "variable", value: "a" })
  })] })
  }) });

export const testTypePersonOrSomethingName: Core.Name = "PersonOrSomething";

export const testTypePolymorphicWrapper: Core.Type = ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "wrap", value: ({ tag: "list", value: ({ tag: "variable", value: "a" }) }) })
  }) });

export const testTypePolymorphicWrapperName: Core.Name = "PolymorphicWrapper";

export const testTypeSimpleNumber: Core.Type = ({ tag: "union", value: [({
    name: "int",
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }), ({
    name: "float",
    type: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  })] });

export const testTypeSimpleNumberName: Core.Name = "SimpleNumber";

export const testTypeStringAlias: Core.Type = ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) });

export const testTypeStringAliasName: Core.Name = "StringAlias";

export const testTypeSymmetricTriple: Core.Type = ({ tag: "forall", value: ({
    parameter: "v",
    body: ({ tag: "forall", value: ({
    parameter: "e",
    body: ({ tag: "wrap", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: testTypeTripleName }),
    argument: ({ tag: "variable", value: "v" })
  }) }),
    argument: ({ tag: "variable", value: "e" })
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) }) })
  }) })
  }) });

export const testTypeSymmetricTripleName: Core.Name = "SymmetricTriple";

export const testTypeTimestamp: Core.Type = ({ tag: "union", value: [({
    name: "unixTimeMillis",
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64" }) }) })
  }), ({
    name: "date",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  })] });

export const testTypeTimestampName: Core.Name = "Timestamp";

export const testTypeTriple: Core.Type = ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "forall", value: ({
    parameter: "b",
    body: ({ tag: "forall", value: ({
    parameter: "c",
    body: ({ tag: "record", value: [({
    name: "first",
    type: ({ tag: "variable", value: "a" })
  }), ({
    name: "second",
    type: ({ tag: "variable", value: "b" })
  }), ({
    name: "third",
    type: ({ tag: "variable", value: "c" })
  })] })
  }) })
  }) })
  }) });

export const testTypeTripleName: Core.Name = "Triple";

export const testTypeUnionMonomorphic: Core.Type = ({ tag: "union", value: [({
    name: "bool",
    type: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }), ({
    name: "string",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "unit",
    type: ({ tag: "unit" })
  })] });

export const testTypeUnionMonomorphicName: Core.Name = "UnionMonomorphic";

export const testTypeUnionPolymorphicRecursive: Core.Type = ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "union", value: [({
    name: "bool",
    type: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }), ({
    name: "value",
    type: ({ tag: "variable", value: "a" })
  }), ({
    name: "other",
    type: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  })] })
  }) });

export const testTypeUnionPolymorphicRecursiveName: Core.Name = "UnionPolymorphicRecursive";

export const testTypeUnit: Core.Type = ({ tag: "record", value: [] });

export const testTypeUnitName: Core.Name = "Unit";
