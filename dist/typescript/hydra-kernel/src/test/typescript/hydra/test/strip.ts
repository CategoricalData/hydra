// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for annotation and type stripping operations
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
import * as ShowCore from "../show/core.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as TestTestGraph from "./testGraph.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "strip",
    description: null,
    subgroups: [({
    name: "deannotateTerm",
    description: null,
    subgroups: [],
    cases: [({
    name: "unannotated literal unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Strip.deannotateTerm(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unannotated variable unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Strip.deannotateTerm(({ tag: "variable", value: "x" }))),
    expected: ShowCore.term(({ tag: "variable", value: "x" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unannotated lambda unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Strip.deannotateTerm(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single annotation stripped",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Strip.deannotateTerm(({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested annotations stripped",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Strip.deannotateTerm(({ tag: "annotated", value: ({
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    annotation: new Map([])
  }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated lambda stripped",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Strip.deannotateTerm(({ tag: "annotated", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated application stripped",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Strip.deannotateTerm(({ tag: "annotated", value: ({
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "deannotateType",
    description: null,
    subgroups: [],
    cases: [({
    name: "unannotated primitive type unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Strip.deannotateType(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unannotated string type unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Strip.deannotateType(({ tag: "literal", value: ({ tag: "string" }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unannotated function type unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Strip.deannotateType(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
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
    name: "single annotation stripped",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Strip.deannotateType(({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested annotations stripped",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Strip.deannotateType(({ tag: "annotated", value: ({
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string" }) }),
    annotation: new Map([])
  }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated list type stripped",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Strip.deannotateType(({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated function type stripped",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Strip.deannotateType(({ tag: "annotated", value: ({
    body: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });
