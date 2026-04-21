// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for type unification operations
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
import * as LibEithers from "../lib/eithers.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibMaps from "../lib/maps.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowCore from "../show/core.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Unification from "../unification.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "unification",
    description: null,
    subgroups: [({
    name: "variableOccursInType",
    description: null,
    subgroups: [],
    cases: [({
    name: "variable occurs in itself",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "variable", value: "a" }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable does not occur in different variable",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "variable", value: "b" }))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable does not occur in int32",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable does not occur in string",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "literal", value: ({ tag: "string" }) }))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in list element type",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "list", value: ({ tag: "variable", value: "a" }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable does not occur in list of different type",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "list", value: ({ tag: "variable", value: "b" }) }))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in function domain",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in function codomain",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "a" })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable does not occur in function with different vars",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "c" })
  }) }))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in optional type",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "maybe", value: ({ tag: "variable", value: "a" }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in pair first",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "a" }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in pair second",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "variable", value: "a" })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in either left",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "either", value: ({
    left: ({ tag: "variable", value: "a" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in either right",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "variable", value: "a" })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in map key type",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "a" }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in map value type",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "variable", value: "a" })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in set type",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "set", value: ({ tag: "variable", value: "a" }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in nested list",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "list", value: ({ tag: "list", value: ({ tag: "variable", value: "a" }) }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in list of functions",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "list", value: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "a" })
  }) }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable does not occur in complex type without it",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "variable", value: "b" })
  }) }) })
  }) }))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs deep in complex type",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "variable", value: "a" })
  }) }) })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in forAll body",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "forall", value: ({
    parameter: "b",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "a" })
  }) })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable occurs in forAll bound position",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(Unification.variableOccursInType("a")(({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) })
  }) }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "unifyTypes",
    description: null,
    subgroups: [],
    cases: [({
    name: "unify identical int32 types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify identical string types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "literal", value: ({ tag: "string" }) }))(({ tag: "literal", value: ({ tag: "string" }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify identical variable types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "variable", value: "a" }))(({ tag: "variable", value: "a" }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify variable with int32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "variable", value: "a" }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify int32 with variable",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(({ tag: "variable", value: "a" }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify two different variables",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "variable", value: "a" }))(({ tag: "variable", value: "b" }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([["a", ({ tag: "variable", value: "b" })]]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify list of variables with list of int32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "list", value: ({ tag: "variable", value: "a" }) }))(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify identical list types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify function types with variables",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }))(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })], ["b", ({ tag: "literal", value: ({ tag: "string" }) })]]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify identical function types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify optional types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "maybe", value: ({ tag: "variable", value: "a" }) }))(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify pair types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "a" }),
    second: ({ tag: "variable", value: "b" })
  }) }))(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })], ["b", ({ tag: "literal", value: ({ tag: "string" }) })]]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify either types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "either", value: ({
    left: ({ tag: "variable", value: "a" }),
    right: ({ tag: "variable", value: "b" })
  }) }))(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })], ["b", ({ tag: "literal", value: ({ tag: "string" }) })]]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify map types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "k" }),
    values: ({ tag: "variable", value: "v" })
  }) }))(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([["k", ({ tag: "literal", value: ({ tag: "string" }) })], ["v", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify set types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "set", value: ({ tag: "variable", value: "a" }) }))(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unify unit types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "unit" }))(({ tag: "unit" }))("test")),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(new Map([]))))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fail to unify int32 with string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(({ tag: "literal", value: ({ tag: "string" }) }))("test")),
    expected: "failure"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fail to unify list with function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))("test")),
    expected: "failure"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "occur check: variable with list containing it",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((ts: Typing.TypeSubst) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((p: readonly [Core.Name, Core.Type]) => LibStrings.cat([((_x) => _x)(LibPairs.first(p)), ": ", ShowCore.type(LibPairs.second(p))])))(LibMaps.toList(((_x) => _x)(ts)))), "}"])))(Unification.unifyTypes(Lexical.emptyContext)(LibMaps.fromList(LibLists.map(((n: Core.Name) => [n, ({
    variables: [],
    type: ({ tag: "variable", value: n }),
    constraints: null
  })]))([])))(({ tag: "variable", value: "a" }))(({ tag: "list", value: ({ tag: "variable", value: "a" }) }))("test")),
    expected: "failure"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "joinTypes",
    description: null,
    subgroups: [],
    cases: [({
    name: "join identical int32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))("test")),
    expected: LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))([])), "]"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "join identical string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "literal", value: ({ tag: "string" }) }))(({ tag: "literal", value: ({ tag: "string" }) }))("test")),
    expected: LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))([])), "]"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "join list types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "list", value: ({ tag: "variable", value: "a" }) }))(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))("test")),
    expected: LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))([({
    left: ({ tag: "variable", value: "a" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    comment: "join types; test"
  })])), "]"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "join function types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }))(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))("test")),
    expected: LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))([({
    left: ({ tag: "variable", value: "a" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    comment: "join types; test"
  }), ({
    left: ({ tag: "variable", value: "b" }),
    right: ({ tag: "literal", value: ({ tag: "string" }) }),
    comment: "join types; test"
  })])), "]"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "join optional types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "maybe", value: ({ tag: "variable", value: "a" }) }))(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))("test")),
    expected: LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))([({
    left: ({ tag: "variable", value: "a" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    comment: "join types; test"
  })])), "]"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "join pair types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "a" }),
    second: ({ tag: "variable", value: "b" })
  }) }))(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))("test")),
    expected: LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))([({
    left: ({ tag: "variable", value: "a" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    comment: "join types; test"
  }), ({
    left: ({ tag: "variable", value: "b" }),
    right: ({ tag: "literal", value: ({ tag: "string" }) }),
    comment: "join types; test"
  })])), "]"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "join either types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "either", value: ({
    left: ({ tag: "variable", value: "a" }),
    right: ({ tag: "variable", value: "b" })
  }) }))(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))("test")),
    expected: LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))([({
    left: ({ tag: "variable", value: "a" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    comment: "join types; test"
  }), ({
    left: ({ tag: "variable", value: "b" }),
    right: ({ tag: "literal", value: ({ tag: "string" }) }),
    comment: "join types; test"
  })])), "]"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "join map types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "k" }),
    values: ({ tag: "variable", value: "v" })
  }) }))(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))("test")),
    expected: LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))([({
    left: ({ tag: "variable", value: "k" }),
    right: ({ tag: "literal", value: ({ tag: "string" }) }),
    comment: "join types; test"
  }), ({
    left: ({ tag: "variable", value: "v" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    comment: "join types; test"
  })])), "]"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "join set types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "set", value: ({ tag: "variable", value: "a" }) }))(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))("test")),
    expected: LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))([({
    left: ({ tag: "variable", value: "a" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    comment: "join types; test"
  })])), "]"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "join unit types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "unit" }))(({ tag: "unit" }))("test")),
    expected: LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))([])), "]"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fail to join int32 with string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(({ tag: "literal", value: ({ tag: "string" }) }))("test")),
    expected: "failure"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fail to join list with function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))("test")),
    expected: "failure"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fail to join pair with either",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: Errors.UnificationError) => "failure"))(((cs: ReadonlyArray<Typing.TypeConstraint>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(((c: Typing.TypeConstraint) => LibStrings.cat(["(", ShowCore.type(((_x) => _x.left)(c)), " ~ ", ShowCore.type(((_x) => _x.right)(c)), ")"])))(cs)), "]"])))(Unification.joinTypes(Lexical.emptyContext)(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))("test")),
    expected: "failure"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });
