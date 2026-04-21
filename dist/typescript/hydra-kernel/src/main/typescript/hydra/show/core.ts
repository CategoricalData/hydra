// Note: this is an automatically generated file. Do not edit.

/**
 * String representations of hydra.core types
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
import * as LibEithers from "../lib/eithers.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
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

export function binding(el: Core.Binding): string {
  return (() => {
  const name = ((_x) => _x)(((_x) => _x.name)(el));
  return (() => {
  const t = ((_x) => _x.term)(el);
  return (() => {
  const typeStr = LibMaybes.maybe("")(((ts: Core.TypeScheme) => LibStrings.cat([":(", typeScheme(ts), ")"])))(((_x) => _x.type)(el));
  return LibStrings.cat([name, typeStr, " = ", term(t)]);
})();
})();
})();
}

export function caseStatement(cs: Core.CaseStatement): string {
  return (() => {
  const tname = ((_x) => _x)(((_x) => _x.typeName)(cs));
  return (() => {
  const mdef = ((_x) => _x.default)(cs);
  return (() => {
  const csCases = ((_x) => _x.cases)(cs);
  return (() => {
  const defaultField = LibMaybes.maybe([])(((d: Core.Term) => [({
    name: "[default]",
    term: d
  })]))(mdef);
  return (() => {
  const allFields = LibLists.concat([csCases, defaultField]);
  return LibStrings.cat(["case(", tname, ")", fields(allFields)]);
})();
})();
})();
})();
})();
}

export function either<t0, t1>(showA: ((x: t0) => string)): ((x: ((x: t1) => string)) => ((x: t0 | t1) => string)) {
  return ((showB: ((x: t1) => string)) => ((e: t0 | t1) => LibEithers.either(((a: t0) => LibStrings.cat2("left(")(LibStrings.cat2(showA(a))(")"))))(((b: t1) => LibStrings.cat2("right(")(LibStrings.cat2(showB(b))(")"))))(e)));
}

export function field(field: Core.Field): string {
  return (() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(field));
  return (() => {
  const fterm = ((_x) => _x.term)(field);
  return LibStrings.cat([fname, "=", term(fterm)]);
})();
})();
}

export function fieldType(ft: Core.FieldType): string {
  return (() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(ft));
  return (() => {
  const ftyp = ((_x) => _x.type)(ft);
  return LibStrings.cat([fname, ":", type(ftyp)]);
})();
})();
}

export function fields(flds: ReadonlyArray<Core.Field>): string {
  return (() => {
  const fieldStrs = LibLists.map(field)(flds);
  return LibStrings.cat(["{", LibStrings.intercalate(", ")(fieldStrs), "}"]);
})();
}

export function float_(fv: Core.FloatValue): string {
  return (() => {
  const _m = fv;
  switch (_m.tag) {
    case "bigfloat": return ((v: number) => LibStrings.cat2(LibLiterals.showBigfloat(v))(":bigfloat"))((_m as any).value);
    case "float32": return ((v: number) => LibStrings.cat2(LibLiterals.showFloat32(v))(":float32"))((_m as any).value);
    case "float64": return ((v: number) => LibStrings.cat2(LibLiterals.showFloat64(v))(":float64"))((_m as any).value);
  }
})();
}

export function floatType(ft: Core.FloatType): string {
  return (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => "bigfloat")((_m as any).value);
    case "float32": return ((_: void) => "float32")((_m as any).value);
    case "float64": return ((_: void) => "float64")((_m as any).value);
  }
})();
}

export function injection(inj: Core.Injection): string {
  return (() => {
  const tname = ((_x) => _x.typeName)(inj);
  return (() => {
  const f = ((_x) => _x.field)(inj);
  return LibStrings.cat(["inject(", ((_x) => _x)(tname), ")", fields([f])]);
})();
})();
}

export function integer(iv: Core.IntegerValue): string {
  return (() => {
  const _m = iv;
  switch (_m.tag) {
    case "bigint": return ((v: bigint) => LibStrings.cat2(LibLiterals.showBigint(v))(":bigint"))((_m as any).value);
    case "int8": return ((v: number) => LibStrings.cat2(LibLiterals.showInt8(v))(":int8"))((_m as any).value);
    case "int16": return ((v: bigint) => LibStrings.cat2(LibLiterals.showInt16(v))(":int16"))((_m as any).value);
    case "int32": return ((v: number) => LibStrings.cat2(LibLiterals.showInt32(v))(":int32"))((_m as any).value);
    case "int64": return ((v: bigint) => LibStrings.cat2(LibLiterals.showInt64(v))(":int64"))((_m as any).value);
    case "uint8": return ((v: bigint) => LibStrings.cat2(LibLiterals.showUint8(v))(":uint8"))((_m as any).value);
    case "uint16": return ((v: number) => LibStrings.cat2(LibLiterals.showUint16(v))(":uint16"))((_m as any).value);
    case "uint32": return ((v: bigint) => LibStrings.cat2(LibLiterals.showUint32(v))(":uint32"))((_m as any).value);
    case "uint64": return ((v: bigint) => LibStrings.cat2(LibLiterals.showUint64(v))(":uint64"))((_m as any).value);
  }
})();
}

export function integerType(it: Core.IntegerType): string {
  return (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => "bigint")((_m as any).value);
    case "int8": return ((_: void) => "int8")((_m as any).value);
    case "int16": return ((_: void) => "int16")((_m as any).value);
    case "int32": return ((_: void) => "int32")((_m as any).value);
    case "int64": return ((_: void) => "int64")((_m as any).value);
    case "uint8": return ((_: void) => "uint8")((_m as any).value);
    case "uint16": return ((_: void) => "uint16")((_m as any).value);
    case "uint32": return ((_: void) => "uint32")((_m as any).value);
    case "uint64": return ((_: void) => "uint64")((_m as any).value);
  }
})();
}

export function lambda(l: Core.Lambda): string {
  return (() => {
  const v = ((_x) => _x)(((_x) => _x.parameter)(l));
  return (() => {
  const mt = ((_x) => _x.domain)(l);
  return (() => {
  const body = ((_x) => _x.body)(l);
  return (() => {
  const typeStr = LibMaybes.maybe("")(((t: Core.Type) => LibStrings.cat2(":")(type(t))))(mt);
  return LibStrings.cat(["λ", v, typeStr, ".", term(body)]);
})();
})();
})();
})();
}

export function let_(l: Core.Let): string {
  return (() => {
  const bindings = ((_x) => _x.bindings)(l);
  return (() => {
  const env = ((_x) => _x.body)(l);
  return (() => {
  const bindingStrs = LibLists.map(binding)(bindings);
  return LibStrings.cat(["let ", LibStrings.intercalate(", ")(bindingStrs), " in ", term(env)]);
})();
})();
})();
}

export function list<t0>(f: ((x: t0) => string)): ((x: ReadonlyArray<t0>) => string) {
  return ((xs: ReadonlyArray<t0>) => (() => {
  const elementStrs = LibLists.map(f)(xs);
  return LibStrings.cat(["[", LibStrings.intercalate(", ")(elementStrs), "]"]);
})());
}

export function literal(l: Core.Literal): string {
  return (() => {
  const _m = l;
  switch (_m.tag) {
    case "binary": return ((_: Uint8Array) => "[binary]")((_m as any).value);
    case "boolean": return ((b: boolean) => LibLogic.ifElse(b)("true")("false"))((_m as any).value);
    case "float": return ((fv: Core.FloatValue) => float_(fv))((_m as any).value);
    case "integer": return ((iv: Core.IntegerValue) => integer(iv))((_m as any).value);
    case "string": return ((s: string) => LibLiterals.showString(s))((_m as any).value);
  }
})();
}

export function literalType(lt: Core.LiteralType): string {
  return (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => "binary")((_m as any).value);
    case "boolean": return ((_: void) => "boolean")((_m as any).value);
    case "float": return ((ft: Core.FloatType) => floatType(ft))((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => integerType(it))((_m as any).value);
    case "string": return ((_: void) => "string")((_m as any).value);
  }
})();
}

export function map<t0, t1>(showK: ((x: t0) => string)): ((x: ((x: t1) => string)) => ((x: ReadonlyMap<t0, t1>) => string)) {
  return ((showV: ((x: t1) => string)) => ((m: ReadonlyMap<t0, t1>) => (() => {
  const pairStrs = LibLists.map(((p: readonly [t0, t1]) => LibStrings.cat([showK(LibPairs.first(p)), ": ", showV(LibPairs.second(p))])))(LibMaps.toList(m));
  return LibStrings.cat(["{", LibStrings.intercalate(", ")(pairStrs), "}"]);
})()));
}

export function maybe<t0>(f: ((x: t0) => string)): ((x: t0 | null) => string) {
  return ((mx: t0 | null) => LibMaybes.maybe("nothing")(((x: t0) => LibStrings.cat2("just(")(LibStrings.cat2(f(x))(")"))))(mx));
}

export function pair<t0, t1>(showA: ((x: t0) => string)): ((x: ((x: t1) => string)) => ((x: readonly [t0, t1]) => string)) {
  return ((showB: ((x: t1) => string)) => ((p: readonly [t0, t1]) => LibStrings.cat(["(", showA(LibPairs.first(p)), ", ", showB(LibPairs.second(p)), ")"])));
}

export function projection(proj: Core.Projection): string {
  return (() => {
  const tname = ((_x) => _x)(((_x) => _x.typeName)(proj));
  return (() => {
  const fname = ((_x) => _x)(((_x) => _x.field)(proj));
  return LibStrings.cat(["project(", tname, "){", fname, "}"]);
})();
})();
}

export function readTerm(s: string): Core.Term | null {
  return ({ tag: "literal", value: ({ tag: "string", value: s }) });
}

export function set<t0>(f: ((x: t0) => string)): ((x: ReadonlySet<t0>) => string) {
  return ((xs: ReadonlySet<t0>) => (() => {
  const elementStrs = LibLists.map(f)(LibSets.toList(xs));
  return LibStrings.cat(["{", LibStrings.intercalate(", ")(elementStrs), "}"]);
})());
}

export function term(t: Core.Term): string {
  return (() => {
  const gatherTerms = ((prev: ReadonlyArray<Core.Term>) => ((app: Core.Application) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return (() => {
  const _m = lhs;
  switch (_m.tag) {
    case "application": return ((app2: Core.Application) => gatherTerms(LibLists.cons(rhs)(prev))(app2))((_m as any).value);
    default: return LibLists.cons(lhs)(LibLists.cons(rhs)(prev))(_m);
  }
})();
})();
})()));
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => term(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((app: Core.Application) => (() => {
  const terms = gatherTerms([])(app);
  return (() => {
  const termStrs = LibLists.map(term)(terms);
  return LibStrings.cat(["(", LibStrings.intercalate(" @ ")(termStrs), ")"]);
})();
})())((_m as any).value);
    case "cases": return ((v1: Core.CaseStatement) => caseStatement(v1))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => LibStrings.cat(["left(", term(l), ")"])))(((r: Core.Term) => LibStrings.cat(["right(", term(r), ")"])))(e))((_m as any).value);
    case "lambda": return ((v1: Core.Lambda) => lambda(v1))((_m as any).value);
    case "let": return ((l: Core.Let) => let_(l))((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => (() => {
  const termStrs = LibLists.map(term)(els);
  return LibStrings.cat(["[", LibStrings.intercalate(", ")(termStrs), "]"]);
})())((_m as any).value);
    case "literal": return ((lit: Core.Literal) => literal(lit))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const entry = ((p: readonly [Core.Term, Core.Term]) => LibStrings.cat([term(LibPairs.first(p)), "=", term(LibPairs.second(p))]));
  return LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(entry)(LibMaps.toList(m))), "}"]);
})())((_m as any).value);
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.maybe("nothing")(((t2: Core.Term) => LibStrings.cat(["just(", term(t2), ")"])))(mt))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => LibStrings.cat(["(", term(LibPairs.first(p)), ", ", term(LibPairs.second(p)), ")"]))((_m as any).value);
    case "project": return ((v1: Core.Projection) => projection(v1))((_m as any).value);
    case "record": return ((rec: Core.Record) => (() => {
  const tname = ((_x) => _x)(((_x) => _x.typeName)(rec));
  return (() => {
  const flds = ((_x) => _x.fields)(rec);
  return LibStrings.cat(["record(", tname, ")", fields(flds)]);
})();
})())((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(term)(LibSets.toList(s))), "}"]))((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => (() => {
  const param = ((_x) => _x)(((_x) => _x.parameter)(ta));
  return (() => {
  const body = ((_x) => _x.body)(ta);
  return LibStrings.cat(["Λ", param, ".", term(body)]);
})();
})())((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => (() => {
  const t2 = ((_x) => _x.body)(tt);
  return (() => {
  const typ = ((_x) => _x.type)(tt);
  return LibStrings.cat([term(t2), "⟨", type(typ), "⟩"]);
})();
})())((_m as any).value);
    case "inject": return ((v1: Core.Injection) => injection(v1))((_m as any).value);
    case "unit": return ((_: void) => "unit")((_m as any).value);
    case "unwrap": return ((tname: Core.Name) => LibStrings.cat(["unwrap(", ((_x) => _x)(tname), ")"]))((_m as any).value);
    case "variable": return ((name: Core.Name) => ((_x) => _x)(name))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => (() => {
  const tname = ((_x) => _x)(((_x) => _x.typeName)(wt));
  return (() => {
  const term1 = ((_x) => _x.body)(wt);
  return LibStrings.cat(["wrap(", tname, "){", term(term1), "}"]);
})();
})())((_m as any).value);
  }
})();
})();
}

export function type(typ: Core.Type): string {
  return (() => {
  const showRowType = ((flds: ReadonlyArray<Core.FieldType>) => (() => {
  const fieldStrs = LibLists.map(fieldType)(flds);
  return LibStrings.cat(["{", LibStrings.intercalate(", ")(fieldStrs), "}"]);
})());
  return (() => {
  const gatherTypes = ((prev: ReadonlyArray<Core.Type>) => ((app: Core.ApplicationType) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return (() => {
  const _m = lhs;
  switch (_m.tag) {
    case "application": return ((app2: Core.ApplicationType) => gatherTypes(LibLists.cons(rhs)(prev))(app2))((_m as any).value);
    default: return LibLists.cons(lhs)(LibLists.cons(rhs)(prev))(_m);
  }
})();
})();
})()));
  return (() => {
  const gatherFunctionTypes = ((prev: ReadonlyArray<Core.Type>) => ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => (() => {
  const dom = ((_x) => _x.domain)(ft);
  return (() => {
  const cod = ((_x) => _x.codomain)(ft);
  return gatherFunctionTypes(LibLists.cons(dom)(prev))(cod);
})();
})())((_m as any).value);
    default: return LibLists.reverse(LibLists.cons(t)(prev))(_m);
  }
})()));
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => type(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((app: Core.ApplicationType) => (() => {
  const types = gatherTypes([])(app);
  return (() => {
  const typeStrs = LibLists.map(type)(types);
  return LibStrings.cat(["(", LibStrings.intercalate(" @ ")(typeStrs), ")"]);
})();
})())((_m as any).value);
    case "either": return ((et: Core.EitherType) => (() => {
  const leftTyp = ((_x) => _x.left)(et);
  return (() => {
  const rightTyp = ((_x) => _x.right)(et);
  return LibStrings.cat(["either<", type(leftTyp), ", ", type(rightTyp), ">"]);
})();
})())((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => (() => {
  const var_ = ((_x) => _x)(((_x) => _x.parameter)(ft));
  return (() => {
  const body = ((_x) => _x.body)(ft);
  return LibStrings.cat(["(∀", var_, ".", type(body), ")"]);
})();
})())((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => (() => {
  const types = gatherFunctionTypes([])(typ);
  return (() => {
  const typeStrs = LibLists.map(type)(types);
  return LibStrings.cat(["(", LibStrings.intercalate(" → ")(typeStrs), ")"]);
})();
})())((_m as any).value);
    case "list": return ((etyp: Core.Type) => LibStrings.cat(["list<", type(etyp), ">"]))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => literalType(lt))((_m as any).value);
    case "map": return ((mt: Core.MapType) => (() => {
  const keyTyp = ((_x) => _x.keys)(mt);
  return (() => {
  const valTyp = ((_x) => _x.values)(mt);
  return LibStrings.cat(["map<", type(keyTyp), ", ", type(valTyp), ">"]);
})();
})())((_m as any).value);
    case "maybe": return ((etyp: Core.Type) => LibStrings.cat(["maybe<", type(etyp), ">"]))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => (() => {
  const firstTyp = ((_x) => _x.first)(pt);
  return (() => {
  const secondTyp = ((_x) => _x.second)(pt);
  return LibStrings.cat(["(", type(firstTyp), ", ", type(secondTyp), ")"]);
})();
})())((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibStrings.cat2("record")(showRowType(rt)))((_m as any).value);
    case "set": return ((etyp: Core.Type) => LibStrings.cat(["set<", type(etyp), ">"]))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => LibStrings.cat2("union")(showRowType(rt)))((_m as any).value);
    case "unit": return ((_: void) => "unit")((_m as any).value);
    case "variable": return ((name: Core.Name) => ((_x) => _x)(name))((_m as any).value);
    case "void": return ((_: void) => "void")((_m as any).value);
    case "wrap": return ((wt: Core.Type) => LibStrings.cat(["wrap(", type(wt), ")"]))((_m as any).value);
  }
})();
})();
})();
})();
}

export function typeScheme(ts: Core.TypeScheme): string {
  return (() => {
  const vars = ((_x) => _x.variables)(ts);
  return (() => {
  const body = ((_x) => _x.type)(ts);
  return (() => {
  const varNames = LibLists.map(((_x) => _x))(vars);
  return (() => {
  const fa = LibLogic.ifElse(LibLists.null_(vars))("")(LibStrings.cat(["forall ", LibStrings.intercalate(",")(varNames), ". "]));
  return (() => {
  const toConstraintPair = ((v: Core.Name) => ((c: Core.Name) => LibStrings.cat([((_x) => _x)(c), " ", ((_x) => _x)(v)])));
  return (() => {
  const toConstraintPairs = ((p: readonly [Core.Name, Core.TypeVariableMetadata]) => LibLists.map(((v1: Core.Name) => toConstraintPair(LibPairs.first(p))(v1)))(LibSets.toList(((_x) => _x.classes)(LibPairs.second(p)))));
  return (() => {
  const tc = LibMaybes.maybe([])(((m: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => LibLists.concat(LibLists.map(toConstraintPairs)(LibMaps.toList(m)))))(((_x) => _x.constraints)(ts));
  return LibStrings.cat(["(", fa, LibLogic.ifElse(LibLists.null_(tc))("")(LibStrings.cat(["(", LibStrings.intercalate(", ")(tc), ") => "])), type(body), ")"]);
})();
})();
})();
})();
})();
})();
})();
}
