// Note: this is an automatically generated file. Do not edit.

/**
 * Validation functions for core terms and types
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
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Rewriting from "../rewriting.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variables from "../variables.js";
import * as Variants from "../variants.js";

export function checkDuplicateBindings(path: Paths.SubtermPath): ((x: ReadonlyArray<Core.Binding>) => ErrorCore.InvalidTermError | null) {
  return ((bindings: ReadonlyArray<Core.Binding>) => (() => {
  const names = LibLists.map(((_x) => _x.name))(bindings);
  return (() => {
  const dup = findDuplicate(names);
  return LibMaybes.map(((name: Core.Name) => ({ tag: "duplicateBinding", value: ({
    location: path,
    name: name
  }) })))(dup);
})();
})());
}

export function checkDuplicateFieldTypes<t0>(fields: ReadonlyArray<Core.FieldType>): ((x: ((x: Core.Name) => t0 | null)) => t0 | null) {
  return ((mkError: ((x: Core.Name) => t0 | null)) => (() => {
  const names = LibLists.map(((_x) => _x.name))(fields);
  return (() => {
  const dup = findDuplicateFieldType(names);
  return LibMaybes.cases(dup)(null)(((name: Core.Name) => mkError(name)));
})();
})());
}

export function checkDuplicateFields(path: Paths.SubtermPath): ((x: ReadonlyArray<Core.Name>) => ErrorCore.InvalidTermError | null) {
  return ((names: ReadonlyArray<Core.Name>) => (() => {
  const dup = findDuplicate(names);
  return LibMaybes.map(((name: Core.Name) => ({ tag: "duplicateField", value: ({
    location: path,
    name: name
  }) })))(dup);
})());
}

export function checkShadowing(path: Paths.SubtermPath): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Name>) => ErrorCore.InvalidTermError | null)) {
  return ((cx: Graph.Graph) => ((names: ReadonlyArray<Core.Name>) => (() => {
  const result = LibLists.foldl(((acc: ErrorCore.InvalidTermError | null) => ((name: Core.Name) => LibMaybes.cases(acc)(LibLogic.ifElse(LibLogic.or(LibMaybes.isJust(LibMaps.lookup(name)(((_x) => _x.boundTerms)(cx))))(LibSets.member(name)(((_x) => _x.lambdaVariables)(cx))))(({ tag: "termVariableShadowing", value: ({
    location: path,
    name: name
  }) }))(null))(((_: ErrorCore.InvalidTermError) => acc)))))(null)(names);
  return result;
})()));
}

export function checkTerm(typed: boolean): ((x: Paths.SubtermPath) => ((x: Graph.Graph) => ((x: Core.Term) => ErrorCore.InvalidTermError | null))) {
  return ((path: Paths.SubtermPath) => ((cx: Graph.Graph) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((ann: Core.AnnotatedTerm) => (() => {
  const body = ((_x) => _x.body)(ann);
  return (() => {
  const annMap = ((_x) => _x.annotation)(ann);
  return firstError([LibLogic.ifElse(LibMaps.null_(annMap))(({ tag: "emptyTermAnnotation", value: ({
    location: path
  }) }))(null), (() => {
  const _m = body;
  switch (_m.tag) {
    case "annotated": return ((_: Core.AnnotatedTerm) => ({ tag: "nestedTermAnnotation", value: ({
    location: path
  }) }))((_m as any).value);
    default: return null(_m);
  }
})()]);
})();
})())((_m as any).value);
    case "application": return ((app: Core.Application) => (() => {
  const fun = ((_x) => _x.function)(app);
  return (() => {
  const arg = ((_x) => _x.argument)(app);
  return firstError([(() => {
  const _m = fun;
  switch (_m.tag) {
    case "variable": return ((primName: Core.Name) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(primName))("hydra.lib.logic.ifElse"))((() => {
  const _m = arg;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "boolean": return ((boolVal: boolean) => ({ tag: "constantCondition", value: ({
    location: path,
    value: boolVal
  }) }))((_m as any).value);
    default: return null(_m);
  }
})())((_m as any).value);
    default: return null(_m);
  }
})())(null))((_m as any).value);
    default: return null(_m);
  }
})(), (() => {
  const _m = fun;
  switch (_m.tag) {
    case "variable": return ((funName: Core.Name) => (() => {
  const _m = arg;
  switch (_m.tag) {
    case "variable": return ((argName: Core.Name) => LibLogic.ifElse(LibEquality.equal(funName)(argName))(({ tag: "selfApplication", value: ({
    location: path,
    name: funName
  }) }))(null))((_m as any).value);
    default: return null(_m);
  }
})())((_m as any).value);
    default: return null(_m);
  }
})(), (() => {
  const _m = fun;
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const param = ((_x) => _x.parameter)(lam);
  return (() => {
  const body = ((_x) => _x.body)(lam);
  return (() => {
  const _m = body;
  switch (_m.tag) {
    case "variable": return ((bodyVar: Core.Name) => LibLogic.ifElse(LibEquality.equal(param)(bodyVar))(({ tag: "unnecessaryIdentityApplication", value: ({
    location: path
  }) }))(null))((_m as any).value);
    default: return null(_m);
  }
})();
})();
})())((_m as any).value);
    default: return null(_m);
  }
})(), (() => {
  const _m = fun;
  switch (_m.tag) {
    case "unwrap": return ((unwrapName: Core.Name) => (() => {
  const _m = arg;
  switch (_m.tag) {
    case "wrap": return ((wt: Core.WrappedTerm) => (() => {
  const wrapName = ((_x) => _x.typeName)(wt);
  return LibLogic.ifElse(LibEquality.equal(unwrapName)(wrapName))(({ tag: "redundantWrapUnwrap", value: ({
    location: path,
    typeName: unwrapName
  }) }))(null);
})())((_m as any).value);
    default: return null(_m);
  }
})())((_m as any).value);
    default: return null(_m);
  }
})()]);
})();
})())((_m as any).value);
    case "record": return ((rec: Core.Record) => (() => {
  const tname = ((_x) => _x.typeName)(rec);
  return (() => {
  const flds = ((_x) => _x.fields)(rec);
  return firstError([LibLogic.ifElse(LibEquality.equal(((_x) => _x)(tname))(""))(({ tag: "emptyTypeNameInTerm", value: ({
    location: path
  }) }))(null), checkDuplicateFields(path)(LibLists.map(((_x) => _x.name))(flds))]);
})();
})())((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const names = LibLists.map(((_x) => _x.name))(bindings);
  return firstError([LibLogic.ifElse(LibLists.null_(bindings))(({ tag: "emptyLetBindings", value: ({
    location: path
  }) }))(null), checkDuplicateBindings(path)(bindings), null, firstError(LibLists.map(((bname: Core.Name) => LibLogic.ifElse(isValidName(bname))(null)(({ tag: "invalidLetBindingName", value: ({
    location: path,
    name: bname
  }) }))))(names)), LibLogic.ifElse(typed)(firstError(LibLists.map(((b: Core.Binding) => LibMaybes.cases(((_x) => _x.type)(b))(null)(((ts: Core.TypeScheme) => checkUndefinedTypeVariablesInTypeScheme(path)(cx)(ts)(((uvName: Core.Name) => ({ tag: "undefinedTypeVariableInBindingType", value: ({
    location: path,
    name: uvName
  }) })))))))(bindings)))(null)]);
})();
})())((_m as any).value);
    case "inject": return ((inj: Core.Injection) => (() => {
  const tname = ((_x) => _x.typeName)(inj);
  return LibLogic.ifElse(LibEquality.equal(((_x) => _x)(tname))(""))(({ tag: "emptyTypeNameInTerm", value: ({
    location: path
  }) }))(null);
})())((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const paramName = ((_x) => _x.parameter)(lam);
  return firstError([LibLogic.ifElse(LibMaybes.isJust(LibMaps.lookup(paramName)(((_x) => _x.boundTerms)(cx))))(({ tag: "termVariableShadowing", value: ({
    location: path,
    name: paramName
  }) }))(null), LibLogic.ifElse(isValidName(paramName))(null)(({ tag: "invalidLambdaParameterName", value: ({
    location: path,
    name: paramName
  }) })), LibLogic.ifElse(typed)(LibMaybes.cases(((_x) => _x.domain)(lam))(null)(((dom: Core.Type) => checkUndefinedTypeVariablesInType(path)(cx)(dom)(((uvName: Core.Name) => ({ tag: "undefinedTypeVariableInLambdaDomain", value: ({
    location: path,
    name: uvName
  }) }))))))(null)]);
})())((_m as any).value);
    case "project": return ((proj: Core.Projection) => (() => {
  const tname = ((_x) => _x.typeName)(proj);
  return LibLogic.ifElse(LibEquality.equal(((_x) => _x)(tname))(""))(({ tag: "emptyTypeNameInTerm", value: ({
    location: path
  }) }))(null);
})())((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const tname = ((_x) => _x.typeName)(cs);
  return (() => {
  const csDefault = ((_x) => _x.default)(cs);
  return (() => {
  const csCases = ((_x) => _x.cases)(cs);
  return firstError([LibLogic.ifElse(LibEquality.equal(((_x) => _x)(tname))(""))(({ tag: "emptyTypeNameInTerm", value: ({
    location: path
  }) }))(null), LibLogic.ifElse(LibLogic.and(LibLists.null_(csCases))(LibMaybes.isNothing(csDefault)))(({ tag: "emptyCaseStatement", value: ({
    location: path,
    typeName: tname
  }) }))(null), checkDuplicateFields(path)(LibLists.map(((_x) => _x.name))(csCases))]);
})();
})();
})())((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => LibLogic.ifElse(typed)(checkUndefinedTypeVariablesInType(path)(cx)(((_x) => _x.type)(ta))(((uvName: Core.Name) => ({ tag: "undefinedTypeVariableInTypeApplication", value: ({
    location: path,
    name: uvName
  }) }))))(null))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const tvName = ((_x) => _x.parameter)(tl);
  return firstError([LibLogic.ifElse(LibSets.member(tvName)(LibSets.delete_(tvName)(((_x) => _x.typeVariables)(cx))))(({ tag: "typeVariableShadowingInTypeLambda", value: ({
    location: path,
    name: tvName
  }) }))(null), LibLogic.ifElse(isValidName(tvName))(null)(({ tag: "invalidTypeLambdaParameterName", value: ({
    location: path,
    name: tvName
  }) }))]);
})())((_m as any).value);
    case "variable": return ((varName: Core.Name) => LibLogic.ifElse(LibLogic.or(LibMaybes.isJust(LibMaps.lookup(varName)(((_x) => _x.boundTerms)(cx))))(LibLogic.or(LibSets.member(varName)(((_x) => _x.lambdaVariables)(cx)))(LibMaybes.isJust(LibMaps.lookup(varName)(((_x) => _x.primitives)(cx))))))(null)(({ tag: "undefinedTermVariable", value: ({
    location: path,
    name: varName
  }) })))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => (() => {
  const tname = ((_x) => _x.typeName)(wt);
  return LibLogic.ifElse(LibEquality.equal(((_x) => _x)(tname))(""))(({ tag: "emptyTypeNameInTerm", value: ({
    location: path
  }) }))(null);
})())((_m as any).value);
    default: return null(_m);
  }
})())));
}

export function checkUndefinedTypeVariablesInType<t0, t1>(path: t0): ((x: Graph.Graph) => ((x: Core.Type) => ((x: ((x: Core.Name) => t1 | null)) => t1 | null))) {
  return ((cx: Graph.Graph) => ((typ: Core.Type) => ((mkError: ((x: Core.Name) => t1 | null)) => (() => {
  const freeVars = Variables.freeVariablesInType(typ);
  return (() => {
  const undefined_ = LibSets.difference(freeVars)(((_x) => _x.typeVariables)(cx));
  return LibLogic.ifElse(LibSets.null_(undefined_))(null)((() => {
  const firstUndefined = LibLists.head(LibSets.toList(undefined_));
  return mkError(firstUndefined);
})());
})();
})())));
}

export function checkUndefinedTypeVariablesInTypeScheme<t0, t1>(path: t0): ((x: Graph.Graph) => ((x: Core.TypeScheme) => ((x: ((x: Core.Name) => t1 | null)) => t1 | null))) {
  return ((cx: Graph.Graph) => ((ts: Core.TypeScheme) => ((mkError: ((x: Core.Name) => t1 | null)) => (() => {
  const freeVars = Variables.freeVariablesInTypeScheme(ts);
  return (() => {
  const undefined_ = LibSets.difference(freeVars)(((_x) => _x.typeVariables)(cx));
  return LibLogic.ifElse(LibSets.null_(undefined_))(null)((() => {
  const firstUndefined = LibLists.head(LibSets.toList(undefined_));
  return mkError(firstUndefined);
})());
})();
})())));
}

export function checkVoid(typ: Core.Type): ErrorCore.InvalidTypeError | null {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "void": return ((_: void) => ({ tag: "voidInNonBottomPosition", value: ({
    location: []
  }) }))((_m as any).value);
    default: return null(_m);
  }
})();
}

export function findDuplicate<t0>(names: ReadonlyArray<t0>): t0 | null {
  return (() => {
  const result = LibLists.foldl(((acc: readonly [ReadonlySet<t0>, t0 | null]) => ((name: t0) => (() => {
  const seen = LibPairs.first(acc);
  return (() => {
  const dup = LibPairs.second(acc);
  return LibMaybes.cases(dup)(LibLogic.ifElse(LibSets.member(name)(seen))([seen, name])([LibSets.insert(name)(seen), null]))(((_: t0) => acc));
})();
})())))([LibSets.empty, null])(names);
  return LibPairs.second(result);
})();
}

export function findDuplicateFieldType<t0>(names: ReadonlyArray<t0>): t0 | null {
  return (() => {
  const result = LibLists.foldl(((acc: readonly [ReadonlySet<t0>, t0 | null]) => ((name: t0) => (() => {
  const seen = LibPairs.first(acc);
  return (() => {
  const dup = LibPairs.second(acc);
  return LibMaybes.cases(dup)(LibLogic.ifElse(LibSets.member(name)(seen))([seen, name])([LibSets.insert(name)(seen), null]))(((_: t0) => acc));
})();
})())))([LibSets.empty, null])(names);
  return LibPairs.second(result);
})();
}

export function firstError<t0>(checks: ReadonlyArray<t0 | null>): t0 | null {
  return LibLists.foldl(((acc: t0 | null) => ((check: t0 | null) => LibMaybes.cases(acc)(check)(((_: t0) => acc)))))(null)(checks);
}

export function firstTypeError<t0>(checks: ReadonlyArray<t0 | null>): t0 | null {
  return LibLists.foldl(((acc: t0 | null) => ((check: t0 | null) => LibMaybes.cases(acc)(check)(((_: t0) => acc)))))(null)(checks);
}

export function isValidName(name: Core.Name): boolean {
  return LibLogic.not(LibEquality.equal(((_x) => _x)(name))(""));
}

export function term(typed: boolean): ((x: Graph.Graph) => ((x: Core.Term) => ErrorCore.InvalidTermError | null)) {
  return ((g: Graph.Graph) => ((t: Core.Term) => Rewriting.foldTermWithGraphAndPath(((recurse: ((x: ErrorCore.InvalidTermError | null) => ((x: Core.Term) => ErrorCore.InvalidTermError | null))) => ((path: ReadonlyArray<Paths.SubtermStep>) => ((cx: Graph.Graph) => ((acc: ErrorCore.InvalidTermError | null) => ((trm: Core.Term) => LibMaybes.cases(acc)((() => {
  const checkResult = checkTerm(typed)(path)(cx)(trm);
  return LibMaybes.cases(checkResult)(recurse(null)(trm))(((err: ErrorCore.InvalidTermError) => err));
})())(((_: ErrorCore.InvalidTermError) => acc))))))))(g)(null)(t)));
}

export function type(boundVars: ReadonlySet<Core.Name>): ((x: Core.Type) => ErrorCore.InvalidTypeError | null) {
  return ((typ: Core.Type) => (() => {
  const checkResult = validateTypeNode(boundVars)(typ);
  return LibMaybes.cases(checkResult)((() => {
  const _m = typ;
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => (() => {
  const newBound = LibSets.insert(((_x) => _x.parameter)(ft))(boundVars);
  return type(newBound)(((_x) => _x.body)(ft));
})())((_m as any).value);
    case "annotated": return ((ann: Core.AnnotatedType) => type(boundVars)(((_x) => _x.body)(ann)))((_m as any).value);
    case "application": return ((at: Core.ApplicationType) => firstTypeError([type(boundVars)(((_x) => _x.function)(at)), type(boundVars)(((_x) => _x.argument)(at))]))((_m as any).value);
    case "either": return ((et: Core.EitherType) => firstTypeError([type(boundVars)(((_x) => _x.left)(et)), type(boundVars)(((_x) => _x.right)(et))]))((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => firstTypeError([type(boundVars)(((_x) => _x.domain)(ft)), type(boundVars)(((_x) => _x.codomain)(ft))]))((_m as any).value);
    case "list": return ((lt: Core.Type) => type(boundVars)(lt))((_m as any).value);
    case "map": return ((mt: Core.MapType) => firstTypeError([type(boundVars)(((_x) => _x.keys)(mt)), type(boundVars)(((_x) => _x.values)(mt))]))((_m as any).value);
    case "maybe": return ((mt: Core.Type) => type(boundVars)(mt))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => firstTypeError([type(boundVars)(((_x) => _x.first)(pt)), type(boundVars)(((_x) => _x.second)(pt))]))((_m as any).value);
    case "record": return ((fields: ReadonlyArray<Core.FieldType>) => firstTypeError(LibLists.map(((f: Core.FieldType) => type(boundVars)(((_x) => _x.type)(f))))(fields)))((_m as any).value);
    case "set": return ((st: Core.Type) => type(boundVars)(st))((_m as any).value);
    case "union": return ((fields: ReadonlyArray<Core.FieldType>) => firstTypeError(LibLists.map(((f: Core.FieldType) => type(boundVars)(((_x) => _x.type)(f))))(fields)))((_m as any).value);
    case "wrap": return ((wt: Core.Type) => type(boundVars)(wt))((_m as any).value);
    default: return null(_m);
  }
})())(((err: ErrorCore.InvalidTypeError) => err));
})());
}

export function validateTypeNode(boundVars: ReadonlySet<Core.Name>): ((x: Core.Type) => ErrorCore.InvalidTypeError | null) {
  return ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((ann: Core.AnnotatedType) => (() => {
  const body = ((_x) => _x.body)(ann);
  return (() => {
  const annMap = ((_x) => _x.annotation)(ann);
  return firstTypeError([LibLogic.ifElse(LibMaps.null_(annMap))(({ tag: "emptyTypeAnnotation", value: ({
    location: []
  }) }))(null), (() => {
  const _m = body;
  switch (_m.tag) {
    case "annotated": return ((_: Core.AnnotatedType) => ({ tag: "nestedTypeAnnotation", value: ({
    location: []
  }) }))((_m as any).value);
    default: return null(_m);
  }
})()]);
})();
})())((_m as any).value);
    case "either": return ((et: Core.EitherType) => firstTypeError([checkVoid(((_x) => _x.left)(et)), checkVoid(((_x) => _x.right)(et))]))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => (() => {
  const paramName = ((_x) => _x.parameter)(ft);
  return firstTypeError([LibLogic.ifElse(LibSets.member(paramName)(boundVars))(({ tag: "typeVariableShadowingInForall", value: ({
    location: [],
    name: paramName
  }) }))(null), LibLogic.ifElse(isValidName(paramName))(null)(({ tag: "invalidForallParameterName", value: ({
    location: [],
    name: paramName
  }) }))]);
})())((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => checkVoid(((_x) => _x.codomain)(ft)))((_m as any).value);
    case "list": return ((lt: Core.Type) => checkVoid(lt))((_m as any).value);
    case "map": return ((mt: Core.MapType) => (() => {
  const keyType = ((_x) => _x.keys)(mt);
  return firstTypeError([(() => {
  const _m = keyType;
  switch (_m.tag) {
    case "function": return ((_: Core.FunctionType) => ({ tag: "nonComparableMapKeyType", value: ({
    location: [],
    keyType: keyType
  }) }))((_m as any).value);
    default: return null(_m);
  }
})(), checkVoid(keyType), checkVoid(((_x) => _x.values)(mt))]);
})())((_m as any).value);
    case "pair": return ((pt: Core.PairType) => firstTypeError([checkVoid(((_x) => _x.first)(pt)), checkVoid(((_x) => _x.second)(pt))]))((_m as any).value);
    case "record": return ((fields: ReadonlyArray<Core.FieldType>) => firstTypeError([LibLogic.ifElse(LibLists.null_(fields))(({ tag: "emptyRecordType", value: ({
    location: []
  }) }))(null), checkDuplicateFieldTypes(fields)(((dupName: Core.Name) => ({ tag: "duplicateRecordTypeFieldNames", value: ({
    location: [],
    name: dupName
  }) }))), firstTypeError(LibLists.map(((f: Core.FieldType) => checkVoid(((_x) => _x.type)(f))))(fields))]))((_m as any).value);
    case "set": return ((elemType: Core.Type) => firstTypeError([(() => {
  const _m = elemType;
  switch (_m.tag) {
    case "function": return ((_: Core.FunctionType) => ({ tag: "nonComparableSetElementType", value: ({
    location: [],
    elementType: elemType
  }) }))((_m as any).value);
    default: return null(_m);
  }
})(), checkVoid(elemType)]))((_m as any).value);
    case "union": return ((fields: ReadonlyArray<Core.FieldType>) => firstTypeError([LibLogic.ifElse(LibLists.null_(fields))(({ tag: "emptyUnionType", value: ({
    location: []
  }) }))(null), LibLogic.ifElse(LibEquality.equal(LibLists.length(fields))(1))((() => {
  const singleField = LibLists.head(fields);
  return ({ tag: "singleVariantUnion", value: ({
    location: [],
    fieldName: ((_x) => _x.name)(singleField)
  }) });
})())(null), checkDuplicateFieldTypes(fields)(((dupName: Core.Name) => ({ tag: "duplicateUnionTypeFieldNames", value: ({
    location: [],
    name: dupName
  }) }))), firstTypeError(LibLists.map(((f: Core.FieldType) => checkVoid(((_x) => _x.type)(f))))(fields))]))((_m as any).value);
    case "variable": return ((varName: Core.Name) => LibLogic.ifElse(LibSets.member(varName)(boundVars))(null)(({ tag: "undefinedTypeVariable", value: ({
    location: [],
    name: varName
  }) })))((_m as any).value);
    default: return null(_m);
  }
})());
}
