// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.ast
 */



import * as Ast from "../ast.js";
import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaybes from "../lib/maybes.js";

export function associativity(v1: Ast.Associativity): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "none": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Associativity",
    field: ({
    name: "none",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "left": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Associativity",
    field: ({
    name: "left",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "right": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Associativity",
    field: ({
    name: "right",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "both": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Associativity",
    field: ({
    name: "both",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function blockStyle(x: Ast.BlockStyle): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.ast.BlockStyle",
    fields: [({
    name: "indent",
    term: ({ tag: "maybe", value: LibMaybes.map(((x2: string) => ({ tag: "literal", value: ({ tag: "string", value: x2 }) })))(((_x) => _x.indent)(x)) })
  }), ({
    name: "newlineBeforeContent",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: ((_x) => _x.newlineBeforeContent)(x) }) })
  }), ({
    name: "newlineAfterContent",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: ((_x) => _x.newlineAfterContent)(x) }) })
  })]
  }) });
}

export function bracketExpr(x: Ast.BracketExpr): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.ast.BracketExpr",
    fields: [({
    name: "brackets",
    term: brackets(((_x) => _x.brackets)(x))
  }), ({
    name: "enclosed",
    term: expr(((_x) => _x.enclosed)(x))
  }), ({
    name: "style",
    term: blockStyle(((_x) => _x.style)(x))
  })]
  }) });
}

export function brackets(x: Ast.Brackets): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.ast.Brackets",
    fields: [({
    name: "open",
    term: symbol(((_x) => _x.open)(x))
  }), ({
    name: "close",
    term: symbol(((_x) => _x.close)(x))
  })]
  }) });
}

export function expr(v1: Ast.Expr): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "const": return ((y: Ast.Symbol) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Expr",
    field: ({
    name: "const",
    term: symbol(y)
  })
  }) }))((_m as any).value);
    case "indent": return ((y: Ast.IndentedExpression) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Expr",
    field: ({
    name: "indent",
    term: indentedExpression(y)
  })
  }) }))((_m as any).value);
    case "op": return ((y: Ast.OpExpr) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Expr",
    field: ({
    name: "op",
    term: opExpr(y)
  })
  }) }))((_m as any).value);
    case "brackets": return ((y: Ast.BracketExpr) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Expr",
    field: ({
    name: "brackets",
    term: bracketExpr(y)
  })
  }) }))((_m as any).value);
    case "seq": return ((y: Ast.SeqExpr) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Expr",
    field: ({
    name: "seq",
    term: seqExpr(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function indentStyle(v1: Ast.IndentStyle): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "allLines": return ((y: string) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.IndentStyle",
    field: ({
    name: "allLines",
    term: ({ tag: "literal", value: ({ tag: "string", value: y }) })
  })
  }) }))((_m as any).value);
    case "subsequentLines": return ((y: string) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.IndentStyle",
    field: ({
    name: "subsequentLines",
    term: ({ tag: "literal", value: ({ tag: "string", value: y }) })
  })
  }) }))((_m as any).value);
  }
})();
}

export function indentedExpression(x: Ast.IndentedExpression): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.ast.IndentedExpression",
    fields: [({
    name: "style",
    term: indentStyle(((_x) => _x.style)(x))
  }), ({
    name: "expr",
    term: expr(((_x) => _x.expr)(x))
  })]
  }) });
}

export function op(x: Ast.Op): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.ast.Op",
    fields: [({
    name: "symbol",
    term: symbol(((_x) => _x.symbol)(x))
  }), ({
    name: "padding",
    term: padding(((_x) => _x.padding)(x))
  }), ({
    name: "precedence",
    term: precedence(((_x) => _x.precedence)(x))
  }), ({
    name: "associativity",
    term: associativity(((_x) => _x.associativity)(x))
  })]
  }) });
}

export function opExpr(x: Ast.OpExpr): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.ast.OpExpr",
    fields: [({
    name: "op",
    term: op(((_x) => _x.op)(x))
  }), ({
    name: "lhs",
    term: expr(((_x) => _x.lhs)(x))
  }), ({
    name: "rhs",
    term: expr(((_x) => _x.rhs)(x))
  })]
  }) });
}

export function padding(x: Ast.Padding): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.ast.Padding",
    fields: [({
    name: "left",
    term: ws(((_x) => _x.left)(x))
  }), ({
    name: "right",
    term: ws(((_x) => _x.right)(x))
  })]
  }) });
}

export function precedence(x: Ast.Precedence): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.ast.Precedence",
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: ((_x) => _x)(x) }) }) })
  }) });
}

export function seqExpr(x: Ast.SeqExpr): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.ast.SeqExpr",
    fields: [({
    name: "op",
    term: op(((_x) => _x.op)(x))
  }), ({
    name: "elements",
    term: ({ tag: "list", value: LibLists.map(expr)(((_x) => _x.elements)(x)) })
  })]
  }) });
}

export function symbol(x: Ast.Symbol): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.ast.Symbol",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function ws(v1: Ast.Ws): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "none": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Ws",
    field: ({
    name: "none",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "space": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Ws",
    field: ({
    name: "space",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "break": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Ws",
    field: ({
    name: "break",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "breakAndIndent": return ((y: string) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Ws",
    field: ({
    name: "breakAndIndent",
    term: ({ tag: "literal", value: ({ tag: "string", value: y }) })
  })
  }) }))((_m as any).value);
    case "doubleBreak": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.ast.Ws",
    field: ({
    name: "doubleBreak",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}
