// Note: this is an automatically generated file. Do not edit.

/**
 * Source-to-source automatic differentiation for Float64 terms.
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Strip from "./strip.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function differentiateBinary(bfname: Core.Name): ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Core.Term)))) {
  return ((a: Core.Term) => ((b: Core.Term) => ((da: Core.Term) => ((db: Core.Term) => LibLogic.ifElse(LibLogic.or(LibEquality.equal(bfname)("hydra.lib.math.add"))(LibEquality.equal(bfname)("hydra.lib.math.addFloat64")))(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: da
  }) }),
    argument: db
  }) }))(LibLogic.ifElse(LibLogic.or(LibEquality.equal(bfname)("hydra.lib.math.sub"))(LibEquality.equal(bfname)("hydra.lib.math.subFloat64")))(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: da
  }) }),
    argument: db
  }) }))(LibLogic.ifElse(LibLogic.or(LibEquality.equal(bfname)("hydra.lib.math.mul"))(LibEquality.equal(bfname)("hydra.lib.math.mulFloat64")))(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: a
  }) }),
    argument: db
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: b
  }) }),
    argument: da
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(bfname)("hydra.lib.math.pow"))(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: a
  }) }),
    argument: b
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: db
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: a
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: b
  }) }),
    argument: da
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: a
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(bfname)("hydra.lib.math.atan2"))(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: b
  }) }),
    argument: da
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: a
  }) }),
    argument: db
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: a
  }) }),
    argument: a
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: b
  }) }),
    argument: b
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(bfname)("hydra.lib.math.logBase"))(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: a
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: db
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: b
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: b
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: da
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: a
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: a
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: a
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })))))))))));
}

export function differentiateFunction(term: Core.Term): Core.Term {
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => differentiateFunction(((_x) => _x.body)(at)))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => (() => {
  const paramName = ((_x) => _x.parameter)(l);
  return (() => {
  const body = ((_x) => _x.body)(l);
  return ({ tag: "lambda", value: ({
    parameter: paramName,
    domain: ((_x) => _x.domain)(l),
    body: differentiateTerm(paramName)(body)
  }) });
})();
})())((_m as any).value);
    default: return term(_m);
  }
})();
}

export function differentiateTerm(dx: Core.Name): ((x: Core.Term) => Core.Term) {
  return ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibLogic.ifElse(LibEquality.equal(v)(dx))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })))((_m as any).value);
    case "literal": return ((_: Core.Literal) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
    case "application": return ((app: Core.Application) => (() => {
  const func = ((_x) => _x.function)(app);
  return (() => {
  const arg = ((_x) => _x.argument)(app);
  return (() => {
  const _m = func;
  switch (_m.tag) {
    case "variable": return ((fname: Core.Name) => LibMaybes.maybe(differentiateTerm(dx)(({ tag: "application", value: ({
    function: func,
    argument: arg
  }) })))(((derivTerm: Core.Term) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: derivTerm,
    argument: arg
  }) })
  }) }),
    argument: differentiateTerm(dx)(arg)
  }) })))(primitiveDerivative(fname)))((_m as any).value);
    case "application": return ((innerApp: Core.Application) => (() => {
  const innerFunc = ((_x) => _x.function)(innerApp);
  return (() => {
  const innerArg = ((_x) => _x.argument)(innerApp);
  return (() => {
  const _m = innerFunc;
  switch (_m.tag) {
    case "variable": return ((bfname: Core.Name) => differentiateBinary(bfname)(innerArg)(arg)(differentiateTerm(dx)(innerArg))(differentiateTerm(dx)(arg)))((_m as any).value);
    default: return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: differentiateTerm(dx)(({ tag: "application", value: ({
    function: func,
    argument: arg
  }) }))
  }) }),
    argument: differentiateTerm(dx)(arg)
  }) })(_m);
  }
})();
})();
})())((_m as any).value);
    default: return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: differentiateTerm(dx)(({ tag: "application", value: ({
    function: func,
    argument: arg
  }) }))
  }) }),
    argument: differentiateTerm(dx)(arg)
  }) })(_m);
  }
})();
})();
})())((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => LibLogic.ifElse(LibEquality.equal(((_x) => _x.parameter)(l))(dx))(({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: ((_x) => _x.domain)(l),
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))(({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: ((_x) => _x.domain)(l),
    body: differentiateTerm(dx)(((_x) => _x.body)(l))
  }) })))((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
    case "project": return ((_: Core.Projection) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
    case "unwrap": return ((_: Core.Name) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
    case "let": return ((l: Core.Let) => ({ tag: "let", value: ({
    bindings: LibLists.map(((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: differentiateTerm(dx)(((_x) => _x.term)(b)),
    type: null
  })))(((_x) => _x.bindings)(l)),
    body: differentiateTerm(dx)(((_x) => _x.body)(l))
  }) }))((_m as any).value);
    case "annotated": return ((at: Core.AnnotatedTerm) => differentiateTerm(dx)(((_x) => _x.body)(at)))((_m as any).value);
    case "list": return ((elems: ReadonlyArray<Core.Term>) => ({ tag: "list", value: LibLists.map(((v1: Core.Term) => differentiateTerm(dx)(v1)))(elems) }))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => ({ tag: "pair", value: [differentiateTerm(dx)(LibPairs.first(p)), differentiateTerm(dx)(LibPairs.second(p))] }))((_m as any).value);
    case "record": return ((r: Core.Record) => ({ tag: "record", value: ({
    typeName: ((_x) => _x.typeName)(r),
    fields: LibLists.map(((fld: Core.Field) => ({
    name: ((_x) => _x.name)(fld),
    term: differentiateTerm(dx)(((_x) => _x.term)(fld))
  })))(((_x) => _x.fields)(r))
  }) }))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => differentiateTerm(dx)(((_x) => _x.body)(ta)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => differentiateTerm(dx)(((_x) => _x.body)(tl)))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
    case "set": return ((_: ReadonlySet<Core.Term>) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
    case "map": return ((_: ReadonlyMap<Core.Term, Core.Term>) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
    case "either": return ((_: Core.Term | Core.Term) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
    case "maybe": return ((_: Core.Term | null) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
    case "inject": return ((_: Core.Injection) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
    case "wrap": return ((_: Core.WrappedTerm) => ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))((_m as any).value);
  }
})());
}

export function gradient(typeName: Core.Name): ((x: ReadonlyArray<Core.Name>) => ((x: Core.Term) => Core.Term)) {
  return ((vars: ReadonlyArray<Core.Name>) => ((term: Core.Term) => ({ tag: "record", value: ({
    typeName: typeName,
    fields: LibLists.map(((v: Core.Name) => ({
    name: v,
    term: differentiateTerm(v)(term)
  })))(vars)
  }) })));
}

export function primitiveDerivative(name: Core.Name): Core.Term | null {
  return LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.sin"))(({ tag: "variable", value: "hydra.lib.math.cos" }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.cos"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negateFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.tan"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -2.0 }) }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.exp"))(({ tag: "variable", value: "hydra.lib.math.exp" }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.log"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "variable", value: "_x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.sqrt"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.asin"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "_x" })
  }) }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.acos"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negateFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "_x" })
  }) }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.atan"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "_x" })
  }) }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.sinh"))(({ tag: "variable", value: "hydra.lib.math.cosh" }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.cosh"))(({ tag: "variable", value: "hydra.lib.math.sinh" }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.tanh"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tanh" }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tanh" }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.asinh"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "_x" })
  }) }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.acosh"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "_x" })
  }) }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.atanh"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "_x" })
  }) }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.negate"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.abs"))(({ tag: "variable", value: "hydra.lib.math.signum" }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.ceiling"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.floor"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.round"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.truncate"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))(LibLogic.ifElse(LibEquality.equal(name)("hydra.lib.math.signum"))(({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))(null))))))))))))))))))))));
}
