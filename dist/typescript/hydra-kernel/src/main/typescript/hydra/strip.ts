// Note: this is an automatically generated file. Do not edit.

/**
 * Annotation and type stripping and normalization
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
import * as LibLists from "./lib/lists.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Rewriting from "./rewriting.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function deannotateAndDetypeTerm(t: Core.Term): Core.Term {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => deannotateAndDetypeTerm(((_x) => _x.body)(at)))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => deannotateAndDetypeTerm(((_x) => _x.body)(tt)))((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => deannotateAndDetypeTerm(((_x) => _x.body)(ta)))((_m as any).value);
    default: return t(_m);
  }
})();
}

export function deannotateTerm(t: Core.Term): Core.Term {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => deannotateTerm(((_x) => _x.body)(at)))((_m as any).value);
    default: return t(_m);
  }
})();
}

export function deannotateType(t: Core.Type): Core.Type {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((arg_: Core.AnnotatedType) => deannotateType(((_x) => _x.body)(arg_)))((_m as any).value);
    default: return t(_m);
  }
})();
}

export function deannotateTypeParameters(t: Core.Type): Core.Type {
  return (() => {
  const _m = deannotateType(t);
  switch (_m.tag) {
    case "forall": return ((lt: Core.ForallType) => deannotateTypeParameters(((_x) => _x.body)(lt)))((_m as any).value);
    default: return t(_m);
  }
})();
}

export function deannotateTypeRecursive(typ: Core.Type): Core.Type {
  return (() => {
  const strip = ((recurse: ((x: t0) => Core.Type)) => ((typ2: t0) => (() => {
  const rewritten = recurse(typ2);
  return (() => {
  const _m = rewritten;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => ((_x) => _x.body)(at))((_m as any).value);
    default: return rewritten(_m);
  }
})();
})()));
  return Rewriting.rewriteType(strip)(typ);
})();
}

export function deannotateTypeSchemeRecursive(ts: Core.TypeScheme): Core.TypeScheme {
  return (() => {
  const vars = ((_x) => _x.variables)(ts);
  return (() => {
  const typ = ((_x) => _x.type)(ts);
  return (() => {
  const constraints = ((_x) => _x.constraints)(ts);
  return ({
    variables: vars,
    type: deannotateTypeRecursive(typ),
    constraints: constraints
  });
})();
})();
})();
}

export function detypeTerm(t: Core.Term): Core.Term {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => (() => {
  const subj = ((_x) => _x.body)(at);
  return (() => {
  const ann = ((_x) => _x.annotation)(at);
  return ({ tag: "annotated", value: ({
    body: detypeTerm(subj),
    annotation: ann
  }) });
})();
})())((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => deannotateAndDetypeTerm(((_x) => _x.body)(tt)))((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => deannotateAndDetypeTerm(((_x) => _x.body)(ta)))((_m as any).value);
    default: return t(_m);
  }
})();
}

export function removeTermAnnotations(term: Core.Term): Core.Term {
  return (() => {
  const remove = ((recurse: ((x: Core.Term) => Core.Term)) => ((term2: Core.Term) => (() => {
  const rewritten = recurse(term2);
  return (() => {
  const _m = term2;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => ((_x) => _x.body)(at))((_m as any).value);
    default: return rewritten(_m);
  }
})();
})()));
  return Rewriting.rewriteTerm(remove)(term);
})();
}

export function removeTypeAnnotations(typ: Core.Type): Core.Type {
  return (() => {
  const remove = ((recurse: ((x: t0) => Core.Type)) => ((typ2: t0) => (() => {
  const rewritten = recurse(typ2);
  return (() => {
  const _m = rewritten;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => ((_x) => _x.body)(at))((_m as any).value);
    default: return rewritten(_m);
  }
})();
})()));
  return Rewriting.rewriteType(remove)(typ);
})();
}

export function removeTypeAnnotationsFromTerm(term: Core.Term): Core.Term {
  return (() => {
  const strip = ((recurse: ((x: t0) => Core.Term)) => ((term2: t0) => (() => {
  const rewritten = recurse(term2);
  return (() => {
  const stripBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: ((_x) => _x.term)(b),
    type: null
  }));
  return (() => {
  const _m = rewritten;
  switch (_m.tag) {
    case "let": return ((lt: Core.Let) => ({ tag: "let", value: ({
    bindings: LibLists.map(stripBinding)(((_x) => _x.bindings)(lt)),
    body: ((_x) => _x.body)(lt)
  }) }))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => ((_x) => _x.body)(tt))((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => ((_x) => _x.body)(ta))((_m as any).value);
    default: return rewritten(_m);
  }
})();
})();
})()));
  return Rewriting.rewriteTerm(strip)(term);
})();
}

export function removeTypesFromTerm(term: Core.Term): Core.Term {
  return (() => {
  const strip = ((recurse: ((x: t0) => Core.Term)) => ((term2: t0) => (() => {
  const rewritten = recurse(term2);
  return (() => {
  const stripBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: ((_x) => _x.term)(b),
    type: null
  }));
  return (() => {
  const _m = rewritten;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: null,
    body: ((_x) => _x.body)(l)
  }) }))((_m as any).value);
    case "let": return ((lt: Core.Let) => ({ tag: "let", value: ({
    bindings: LibLists.map(stripBinding)(((_x) => _x.bindings)(lt)),
    body: ((_x) => _x.body)(lt)
  }) }))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => ((_x) => _x.body)(tt))((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => ((_x) => _x.body)(ta))((_m as any).value);
    default: return rewritten(_m);
  }
})();
})();
})()));
  return Rewriting.rewriteTerm(strip)(term);
})();
}

export function stripTypeLambdas(t: Core.Term): Core.Term {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => (() => {
  const subj = ((_x) => _x.body)(at);
  return (() => {
  const ann = ((_x) => _x.annotation)(at);
  return ({ tag: "annotated", value: ({
    body: stripTypeLambdas(subj),
    annotation: ann
  }) });
})();
})())((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => stripTypeLambdas(((_x) => _x.body)(ta)))((_m as any).value);
    default: return t(_m);
  }
})();
}
