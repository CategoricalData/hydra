// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.query
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaybes from "../lib/maybes.js";
import * as Query from "../query.js";

export function comparisonConstraint(v1: Query.ComparisonConstraint): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "equal": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.ComparisonConstraint",
    field: ({
    name: "equal",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "notEqual": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.ComparisonConstraint",
    field: ({
    name: "notEqual",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "lessThan": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.ComparisonConstraint",
    field: ({
    name: "lessThan",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "greaterThan": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.ComparisonConstraint",
    field: ({
    name: "greaterThan",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "lessThanOrEqual": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.ComparisonConstraint",
    field: ({
    name: "lessThanOrEqual",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "greaterThanOrEqual": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.ComparisonConstraint",
    field: ({
    name: "greaterThanOrEqual",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function edge(x: Query.Edge): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.query.Edge",
    fields: [({
    name: "type",
    term: EncodeCore.name(((_x) => _x.type)(x))
  }), ({
    name: "out",
    term: ({ tag: "maybe", value: LibMaybes.map(EncodeCore.name)(((_x) => _x.out)(x)) })
  }), ({
    name: "in",
    term: ({ tag: "maybe", value: LibMaybes.map(EncodeCore.name)(((_x) => _x.in)(x)) })
  })]
  }) });
}

export function graphPattern(x: Query.GraphPattern): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.query.GraphPattern",
    fields: [({
    name: "graph",
    term: EncodeCore.name(((_x) => _x.graph)(x))
  }), ({
    name: "patterns",
    term: ({ tag: "list", value: LibLists.map(pattern)(((_x) => _x.patterns)(x)) })
  })]
  }) });
}

export function node(v1: Query.Node): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "term": return ((y: Core.Term) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Node",
    field: ({
    name: "term",
    term: EncodeCore.term(y)
  })
  }) }))((_m as any).value);
    case "variable": return ((y: Query.Variable) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Node",
    field: ({
    name: "variable",
    term: variable(y)
  })
  }) }))((_m as any).value);
    case "wildcard": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Node",
    field: ({
    name: "wildcard",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function path(v1: Query.Path): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "step": return ((y: Query.Step) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Path",
    field: ({
    name: "step",
    term: step(y)
  })
  }) }))((_m as any).value);
    case "regex": return ((y: Query.RegexSequence) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Path",
    field: ({
    name: "regex",
    term: regexSequence(y)
  })
  }) }))((_m as any).value);
    case "inverse": return ((y: Query.Path) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Path",
    field: ({
    name: "inverse",
    term: path(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function pathEquation(x: Query.PathEquation): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.query.PathEquation",
    fields: [({
    name: "left",
    term: path(((_x) => _x.left)(x))
  }), ({
    name: "right",
    term: path(((_x) => _x.right)(x))
  })]
  }) });
}

export function pattern(v1: Query.Pattern): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "triple": return ((y: Query.TriplePattern) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Pattern",
    field: ({
    name: "triple",
    term: triplePattern(y)
  })
  }) }))((_m as any).value);
    case "negation": return ((y: Query.Pattern) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Pattern",
    field: ({
    name: "negation",
    term: pattern(y)
  })
  }) }))((_m as any).value);
    case "conjunction": return ((y: ReadonlyArray<Query.Pattern>) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Pattern",
    field: ({
    name: "conjunction",
    term: ({ tag: "list", value: LibLists.map(pattern)(y) })
  })
  }) }))((_m as any).value);
    case "disjunction": return ((y: ReadonlyArray<Query.Pattern>) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Pattern",
    field: ({
    name: "disjunction",
    term: ({ tag: "list", value: LibLists.map(pattern)(y) })
  })
  }) }))((_m as any).value);
    case "graph": return ((y: Query.GraphPattern) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Pattern",
    field: ({
    name: "graph",
    term: graphPattern(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function patternImplication(x: Query.PatternImplication): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.query.PatternImplication",
    fields: [({
    name: "antecedent",
    term: pattern(((_x) => _x.antecedent)(x))
  }), ({
    name: "consequent",
    term: pattern(((_x) => _x.consequent)(x))
  })]
  }) });
}

export function query(x: Query.Query): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.query.Query",
    fields: [({
    name: "variables",
    term: ({ tag: "list", value: LibLists.map(variable)(((_x) => _x.variables)(x)) })
  }), ({
    name: "patterns",
    term: ({ tag: "list", value: LibLists.map(pattern)(((_x) => _x.patterns)(x)) })
  })]
  }) });
}

export function range(x: Query.Range): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.query.Range",
    fields: [({
    name: "min",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: ((_x) => _x.min)(x) }) }) })
  }), ({
    name: "max",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: ((_x) => _x.max)(x) }) }) })
  })]
  }) });
}

export function regexQuantifier(v1: Query.RegexQuantifier): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "one": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.RegexQuantifier",
    field: ({
    name: "one",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "zeroOrOne": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.RegexQuantifier",
    field: ({
    name: "zeroOrOne",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "zeroOrMore": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.RegexQuantifier",
    field: ({
    name: "zeroOrMore",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "oneOrMore": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.query.RegexQuantifier",
    field: ({
    name: "oneOrMore",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "exactly": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.query.RegexQuantifier",
    field: ({
    name: "exactly",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "atLeast": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.query.RegexQuantifier",
    field: ({
    name: "atLeast",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "range": return ((y: Query.Range) => ({ tag: "inject", value: ({
    typeName: "hydra.query.RegexQuantifier",
    field: ({
    name: "range",
    term: range(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function regexSequence(x: Query.RegexSequence): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.query.RegexSequence",
    fields: [({
    name: "path",
    term: path(((_x) => _x.path)(x))
  }), ({
    name: "quantifier",
    term: regexQuantifier(((_x) => _x.quantifier)(x))
  })]
  }) });
}

export function step(v1: Query.Step): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "edge": return ((y: Query.Edge) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Step",
    field: ({
    name: "edge",
    term: edge(y)
  })
  }) }))((_m as any).value);
    case "project": return ((y: Core.Projection) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Step",
    field: ({
    name: "project",
    term: EncodeCore.projection(y)
  })
  }) }))((_m as any).value);
    case "compare": return ((y: Query.ComparisonConstraint) => ({ tag: "inject", value: ({
    typeName: "hydra.query.Step",
    field: ({
    name: "compare",
    term: comparisonConstraint(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function triplePattern(x: Query.TriplePattern): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.query.TriplePattern",
    fields: [({
    name: "subject",
    term: node(((_x) => _x.subject)(x))
  }), ({
    name: "predicate",
    term: path(((_x) => _x.predicate)(x))
  }), ({
    name: "object",
    term: node(((_x) => _x.object)(x))
  })]
  }) });
}

export function variable(x: Query.Variable): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.query.Variable",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}
