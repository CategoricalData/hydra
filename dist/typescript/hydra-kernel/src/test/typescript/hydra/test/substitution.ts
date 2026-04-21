// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for type and term substitution operations
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
import * as LibMaps from "../lib/maps.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowCore from "../show/core.js";
import * as Substitution from "../substitution.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "substitution",
    description: null,
    subgroups: [({
    name: "substInType",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty substitution returns type unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([]))(({ tag: "literal", value: ({ tag: "string" }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "substitute type variable with int32",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))(({ tag: "variable", value: "a" }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "non-matching variable unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))(({ tag: "variable", value: "b" }))),
    expected: ShowCore.type(({ tag: "variable", value: "b" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "substitute in function domain",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "substitute in function codomain",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "string" }) })]]))(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "a" })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "substitute in list element type",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))(({ tag: "list", value: ({ tag: "variable", value: "a" }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "substitute in optional type",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "string" }) })]]))(({ tag: "maybe", value: ({ tag: "variable", value: "a" }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "substitute in pair type both sides",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))(({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "a" }),
    second: ({ tag: "variable", value: "a" })
  }) }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "substitute in either type",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "string" }) })]]))(({ tag: "either", value: ({
    left: ({ tag: "variable", value: "a" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "substitute in map key type",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["k", ({ tag: "literal", value: ({ tag: "string" }) })]]))(({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "k" }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "substitute in set type",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))(({ tag: "set", value: ({ tag: "variable", value: "a" }) }))),
    expected: ShowCore.type(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested substitution in list of pairs",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))(({ tag: "list", value: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "a" }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple substitutions",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })], ["b", ({ tag: "literal", value: ({ tag: "string" }) })]]))(({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "a" }),
    second: ({ tag: "variable", value: "b" })
  }) }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "forAll bound variable not substituted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })]]))(({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "forAll free variable substituted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Substitution.substInType(LibMaps.fromList([["b", ({ tag: "literal", value: ({ tag: "string" }) })]]))(({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "b" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });
