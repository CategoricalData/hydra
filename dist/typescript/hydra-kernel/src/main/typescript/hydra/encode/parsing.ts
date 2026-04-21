// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.parsing
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as Parsing from "../parsing.js";

export function parseError(x: Parsing.ParseError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.parsing.ParseError",
    fields: [({
    name: "message",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.message)(x) }) })
  }), ({
    name: "remainder",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.remainder)(x) }) })
  })]
  }) });
}

export function parseResult<t0>(a: ((x: t0) => Core.Term)): ((x: Parsing.ParseResult<t0>) => Core.Term) {
  return ((v1: Parsing.ParseResult) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "success": return ((y: Parsing.ParseSuccess<t0>) => ({ tag: "inject", value: ({
    typeName: "hydra.parsing.ParseResult",
    field: ({
    name: "success",
    term: parseSuccess(a)(y)
  })
  }) }))((_m as any).value);
    case "failure": return ((y: Parsing.ParseError) => ({ tag: "inject", value: ({
    typeName: "hydra.parsing.ParseResult",
    field: ({
    name: "failure",
    term: parseError(y)
  })
  }) }))((_m as any).value);
  }
})());
}

export function parseSuccess<t0>(a: ((x: t0) => Core.Term)): ((x: Parsing.ParseSuccess<t0>) => Core.Term) {
  return ((x: Parsing.ParseSuccess<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.parsing.ParseSuccess",
    fields: [({
    name: "value",
    term: a(((_x) => _x.value)(x))
  }), ({
    name: "remainder",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.remainder)(x) }) })
  })]
  }) }));
}
