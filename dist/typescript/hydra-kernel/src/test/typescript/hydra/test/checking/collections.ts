// Note: this is an automatically generated file. Do not edit.

/**
 * Collection type checking test cases: lists, sets, maps
 */



import * as Ast from "../../ast.js";
import * as Classes from "../../classes.js";
import * as Coders from "../../coders.js";
import * as Context from "../../context.js";
import * as Core from "../../core.js";
import * as ErrorChecking from "../../error/checking.js";
import * as ErrorCore from "../../error/core.js";
import * as ErrorPackaging from "../../error/packaging.js";
import * as Errors from "../../errors.js";
import * as Graph from "../../graph.js";
import * as Inference from "../../inference.js";
import * as JsonModel from "../../json/model.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibPairs from "../../lib/pairs.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as Rewriting from "../../rewriting.js";
import * as Scoping from "../../scoping.js";
import * as ShowCore from "../../show/core.js";
import * as Tabular from "../../tabular.js";
import * as TestTestGraph from "../testGraph.js";
import * as TestTestTypes from "../testTypes.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "Collections",
    description: null,
    subgroups: [listsTests, setsTests, mapsTests],
    cases: []
  });

export const emptyListsTests: Testing.TestGroup = ({
    name: "Empty lists",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [] }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "pair of empty lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [] })] }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    second: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "literal", value: ({ tag: "string", value: "context" }) })] }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const listsInComplexContextsTests: Testing.TestGroup = ({
    name: "Lists in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "multiple lists in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    second: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const listsOfLiteralsTests: Testing.TestGroup = ({
    name: "Lists of literals",
    description: null,
    subgroups: [],
    cases: [({
    name: "int list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "world" }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed numeric types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 1.0 }) }) }), ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 2.5 }) }) }), ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 3.140000104904175 }) }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const listsTests: Testing.TestGroup = ({
    name: "Lists",
    description: null,
    subgroups: [listsOfLiteralsTests, emptyListsTests, polymorphicListsTests, nestedListsTests, listsInComplexContextsTests],
    cases: []
  });

export const mapsInComplexContextsTests: Testing.TestGroup = ({
    name: "Maps in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "map in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "one" }) })]]) }), ({ tag: "literal", value: ({ tag: "string", value: "context" }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    values: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested maps",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "outer" }) }), ({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "boolean", value: true }) })]]) })]]) }))),
    expected: ShowCore.type(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    values: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "map in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "lookup",
    term: ({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "key1" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 100 }) }) })], [({ tag: "literal", value: ({ tag: "string", value: "key2" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 200 }) }) })]]) }),
    type: null
  })],
    body: ({ tag: "variable", value: "lookup" })
  }) }))),
    expected: ShowCore.type(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const mapsTests: Testing.TestGroup = ({
    name: "Maps",
    description: null,
    subgroups: [monomorphicMapsTests, polymorphicMapsTests, mapsInComplexContextsTests, mapsWithComplexTypesTests],
    cases: []
  });

export const mapsWithComplexTypesTests: Testing.TestGroup = ({
    name: "Maps with complex types",
    description: null,
    subgroups: [],
    cases: [({
    name: "map of records",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "person1" }) }), ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Smith" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) })
  })]
  }) })]]) }))),
    expected: ShowCore.type(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "variable", value: TestTestTypes.testTypePersonName })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "map of lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })], [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "c" }) }), ({ tag: "literal", value: ({ tag: "string", value: "d" }) })] })]]) }))),
    expected: ShowCore.type(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    values: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "map of tuples",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "coords" }) }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) })] })]]) }))),
    expected: ShowCore.type(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const monomorphicMapsTests: Testing.TestGroup = ({
    name: "Monomorphic maps",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([]) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int to string map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "one" }) })], [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "two" }) })]]) }))),
    expected: ShowCore.type(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    values: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string to int map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })], [({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })]]) }))),
    expected: ShowCore.type(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single entry map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) }), ({ tag: "literal", value: ({ tag: "boolean", value: true }) })]]) }))),
    expected: ShowCore.type(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint" }) }) }),
    values: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const monomorphicSetsTests: Testing.TestGroup = ({
    name: "Monomorphic sets",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty set",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([]) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int set",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })]) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string set",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "string", value: "apple" }) }), ({ tag: "literal", value: ({ tag: "string", value: "banana" }) }), ({ tag: "literal", value: ({ tag: "string", value: "cherry" }) })]) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element set",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "boolean", value: true }) })]) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "boolean" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nestedListsTests: Testing.TestGroup = ({
    name: "Nested lists",
    description: null,
    subgroups: [],
    cases: [({
    name: "list of lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty nested lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [] })] }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested polymorphic",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "variable", value: "x" })] })] })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nestedSetsTests: Testing.TestGroup = ({
    name: "Nested sets",
    description: null,
    subgroups: [],
    cases: [({
    name: "set of lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "c" }) }), ({ tag: "literal", value: ({ tag: "string", value: "d" }) })] })]) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set of tuples",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })]) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set of sets",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "string", value: "nested" }) })]) })]) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicListsTests: Testing.TestGroup = ({
    name: "Polymorphic lists",
    description: null,
    subgroups: [],
    cases: [({
    name: "list from lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list with repeated var",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "x" })] })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list from two lambdas",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicMapsTests: Testing.TestGroup = ({
    name: "Polymorphic maps",
    description: null,
    subgroups: [],
    cases: [({
    name: "map from lambda keys",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "k",
    domain: null,
    body: ({ tag: "map", value: new Map([[({ tag: "variable", value: "k" }), ({ tag: "literal", value: ({ tag: "string", value: "value" }) })]]) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "map from lambda values",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "key" }) }), ({ tag: "variable", value: "v" })]]) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "map from lambda both",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "k",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "map", value: new Map([[({ tag: "variable", value: "k" }), ({ tag: "variable", value: "v" })]]) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "map with repeated variables",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "map", value: new Map([[({ tag: "variable", value: "x" }), ({ tag: "variable", value: "x" })]]) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicSetsTests: Testing.TestGroup = ({
    name: "Polymorphic sets",
    description: null,
    subgroups: [],
    cases: [({
    name: "set from lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "set", value: new Set([({ tag: "variable", value: "x" })]) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set with repeated variable",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "set", value: new Set([({ tag: "variable", value: "x" })]) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set from two variables",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "set", value: new Set([({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })]) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const setsInComplexContextsTests: Testing.TestGroup = ({
    name: "Sets in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "set in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })]) }), ({ tag: "literal", value: ({ tag: "string", value: "context" }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "numbers",
    term: ({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })]) }),
    type: null
  })],
    body: ({ tag: "variable", value: "numbers" })
  }) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const setsTests: Testing.TestGroup = ({
    name: "Sets",
    description: null,
    subgroups: [monomorphicSetsTests, polymorphicSetsTests, setsInComplexContextsTests, nestedSetsTests, setsWithComplexTypesTests],
    cases: []
  });

export const setsWithComplexTypesTests: Testing.TestGroup = ({
    name: "Sets with complex types",
    description: null,
    subgroups: [],
    cases: [({
    name: "set of records",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Smith" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  })]
  }) })]) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "variable", value: TestTestTypes.testTypePersonName }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set of optionals",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }), ({ tag: "maybe", value: null })]) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set of maps",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "key" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })]]) })]) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });
