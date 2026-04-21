// Note: this is an automatically generated file. Do not edit.

/**
 * Type dereference, lookup, requirements, and instantiation
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as DecodeCore from "./decode/core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as Lexical from "./lexical.js";
import * as LibEithers from "./lib/eithers.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLiterals from "./lib/literals.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMath from "./lib/math.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibStrings from "./lib/strings.js";
import * as Names from "./names.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Scoping from "./scoping.js";
import * as ShowCore from "./show/core.js";
import * as Strip from "./strip.js";
import * as Substitution from "./substitution.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function dereferenceType<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Name) => Errors.Error | Core.Type | null)) {
  return ((graph: Graph.Graph) => ((name: Core.Name) => (() => {
  const mel = Lexical.lookupBinding(graph)(name);
  return LibMaybes.maybe(({ tag: "right", value: null }))(((el: Core.Binding) => LibEithers.map(LibMaybes.pure)(LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "resolution", value: ({ tag: "unexpectedShape", value: ({
    expected: "type",
    actual: ((_x) => _x)(_e)
  }) }) })))(((_a: Core.Type) => _a))(DecodeCore.type(graph)(((_x) => _x.term)(el))))))(mel);
})()));
}

export function fTypeIsPolymorphic(typ: Core.Type): boolean {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => fTypeIsPolymorphic(((_x) => _x.body)(at)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => true)((_m as any).value);
    default: return false(_m);
  }
})();
}

export function fieldMap(fields: ReadonlyArray<Core.Field>): ReadonlyMap<Core.Name, Core.Term> {
  return (() => {
  const toPair = ((f: Core.Field) => [((_x) => _x.name)(f), ((_x) => _x.term)(f)]);
  return LibMaps.fromList(LibLists.map(toPair)(fields));
})();
}

export function fieldTypeMap(fields: ReadonlyArray<Core.FieldType>): ReadonlyMap<Core.Name, Core.Type> {
  return (() => {
  const toPair = ((f: Core.FieldType) => [((_x) => _x.name)(f), ((_x) => _x.type)(f)]);
  return LibMaps.fromList(LibLists.map(toPair)(fields));
})();
}

export function fieldTypes<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Type) => Errors.Error | ReadonlyMap<Core.Name, Core.Type>)) {
  return ((graph: Graph.Graph) => ((t: Core.Type) => (() => {
  const toMap = ((fields: ReadonlyArray<Core.FieldType>) => LibMaps.fromList(LibLists.map(((ft: Core.FieldType) => [((_x) => _x.name)(ft), ((_x) => _x.type)(ft)]))(fields)));
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => fieldTypes(cx)(graph)(((_x) => _x.body)(ft)))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: toMap(rt) }))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: toMap(rt) }))((_m as any).value);
    case "variable": return ((name: Core.Name) => LibMaybes.maybe(LibEithers.bind(Lexical.requireBinding(graph)(name))(((el: Core.Binding) => LibEithers.bind(LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "resolution", value: ({ tag: "unexpectedShape", value: ({
    expected: "type",
    actual: ((_x) => _x)(_e)
  }) }) })))(((_a: Core.Type) => _a))(DecodeCore.type(graph)(((_x) => _x.term)(el))))(((decodedType: Core.Type) => fieldTypes(cx)(graph)(decodedType))))))(((ts: Core.TypeScheme) => fieldTypes(cx)(graph)(((_x) => _x.type)(ts))))(LibMaps.lookup(name)(((_x) => _x.schemaTypes)(graph))))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "unexpectedShape", value: ({
    expected: "record or union type",
    actual: ShowCore.type(t)
  }) }) }) })(_m);
  }
})();
})()));
}

export function findFieldType<t0>(cx: t0): ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => Errors.Error | Core.Type)) {
  return ((fname: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => (() => {
  const matchingFields = LibLists.filter(((ft: Core.FieldType) => LibEquality.equal(((_x) => _x)(((_x) => _x.name)(ft)))(((_x) => _x)(fname))))(fields);
  return LibLogic.ifElse(LibLists.null_(matchingFields))(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noMatchingField", value: ({
    fieldName: fname
  }) }) }) }))(LibLogic.ifElse(LibEquality.equal(LibLists.length(matchingFields))(1))(({ tag: "right", value: ((_x) => _x.type)(LibLists.head(matchingFields)) }))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "multipleFields", value: ({
    fieldName: fname
  }) }) }) })));
})()));
}

export function fullyStripAndNormalizeType(typ: Core.Type): Core.Type {
  return (() => {
  const go = ((depth: number) => ((subst: ReadonlyMap<Core.Name, Core.Name>) => ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => (() => {
  const oldVar = ((_x) => _x.parameter)(ft);
  return (() => {
  const newVar = LibStrings.cat2("_")(LibLiterals.showInt32(depth));
  return go(LibMath.add(depth)(1))(LibMaps.insert(oldVar)(newVar)(subst))(((_x) => _x.body)(ft));
})();
})())((_m as any).value);
    default: return [subst, t](_m);
  }
})())));
  return (() => {
  const result = go(0)(LibMaps.empty)(typ);
  return (() => {
  const subst = LibPairs.first(result);
  return (() => {
  const body = LibPairs.second(result);
  return Variables.substituteTypeVariables(subst)(body);
})();
})();
})();
})();
}

export function fullyStripType(typ: Core.Type): Core.Type {
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => fullyStripType(((_x) => _x.body)(ft)))((_m as any).value);
    default: return typ(_m);
  }
})();
}

export function instantiateType(cx: Context.Context): ((x: Core.Type) => readonly [Core.Type, Context.Context]) {
  return ((typ: Core.Type) => (() => {
  const result = instantiateTypeScheme(cx)(typeToTypeScheme(typ));
  return [Scoping.typeSchemeToFType(LibPairs.first(result)), LibPairs.second(result)];
})());
}

export function instantiateTypeScheme(cx: Context.Context): ((x: Core.TypeScheme) => readonly [Core.TypeScheme, Context.Context]) {
  return ((scheme: Core.TypeScheme) => (() => {
  const oldVars = ((_x) => _x.variables)(scheme);
  return (() => {
  const result = Names.freshNames(LibLists.length(oldVars))(cx);
  return (() => {
  const newVars = LibPairs.first(result);
  return (() => {
  const cx2 = LibPairs.second(result);
  return (() => {
  const subst = LibMaps.fromList(LibLists.zip(oldVars)(LibLists.map(((x: Core.Name) => ({ tag: "variable", value: x })))(newVars)));
  return (() => {
  const nameSubst = LibMaps.fromList(LibLists.zip(oldVars)(newVars));
  return (() => {
  const renamedConstraints = LibMaybes.map(((oldConstraints: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => LibMaps.fromList(LibLists.map(((kv: readonly [Core.Name, Core.TypeVariableMetadata]) => [LibMaybes.fromMaybe(LibPairs.first(kv))(LibMaps.lookup(LibPairs.first(kv))(nameSubst)), LibPairs.second(kv)]))(LibMaps.toList(oldConstraints)))))(((_x) => _x.constraints)(scheme));
  return [({
    variables: newVars,
    type: Substitution.substInType(subst)(((_x) => _x.type)(scheme)),
    constraints: renamedConstraints
  }), cx2];
})();
})();
})();
})();
})();
})();
})());
}

export function nominalApplication(tname: Core.Name): ((x: ReadonlyArray<Core.Type>) => Core.Type) {
  return ((args: ReadonlyArray<Core.Type>) => LibLists.foldl(((t: Core.Type) => ((a: Core.Type) => ({ tag: "application", value: ({
    function: t,
    argument: a
  }) }))))(({ tag: "variable", value: tname }))(args));
}

export function requireRecordType<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Name) => Errors.Error | ReadonlyArray<Core.FieldType>)) {
  return ((graph: Graph.Graph) => ((name: Core.Name) => (() => {
  const toRecord = ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => rt)((_m as any).value);
    default: return null(_m);
  }
})());
  return requireRowType(cx)("record type")(toRecord)(graph)(name);
})()));
}

export function requireRowType<t0, t1>(cx: t0): ((x: string) => ((x: ((x: Core.Type) => t1 | null)) => ((x: Graph.Graph) => ((x: Core.Name) => Errors.Error | t1)))) {
  return ((label: string) => ((getter: ((x: Core.Type) => t1 | null)) => ((graph: Graph.Graph) => ((name: Core.Name) => (() => {
  const rawType = ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => rawType(((_x) => _x.body)(at)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => rawType(((_x) => _x.body)(ft)))((_m as any).value);
    default: return t(_m);
  }
})());
  return LibEithers.bind(requireType(cx)(graph)(name))(((t: Core.Type) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "unexpectedShape", value: ({
    expected: LibStrings.cat2(label)(" type"),
    actual: LibStrings.cat2(((_x) => _x)(name))(LibStrings.cat2(": ")(ShowCore.type(t)))
  }) }) }) }))(((x: t1) => ({ tag: "right", value: x })))(getter(rawType(t)))));
})()))));
}

export function requireSchemaType(cx: Context.Context): ((x: ReadonlyMap<Core.Name, Core.TypeScheme>) => ((x: Core.Name) => Errors.Error | readonly [Core.TypeScheme, Context.Context])) {
  return ((types: ReadonlyMap<Core.Name, Core.TypeScheme>) => ((tname: Core.Name) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noSuchBinding", value: ({
    name: tname
  }) }) }) }))(((ts: Core.TypeScheme) => ({ tag: "right", value: instantiateTypeScheme(cx)(Strip.deannotateTypeSchemeRecursive(ts)) })))(LibMaps.lookup(tname)(types))));
}

export function requireType<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Name) => Errors.Error | Core.Type)) {
  return ((graph: Graph.Graph) => ((name: Core.Name) => LibMaybes.maybe(LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noSuchBinding", value: ({
    name: name
  }) }) }) }))(((ts: Core.TypeScheme) => ({ tag: "right", value: Scoping.typeSchemeToFType(ts) })))(LibMaps.lookup(name)(((_x) => _x.boundTypes)(graph))))(((ts: Core.TypeScheme) => ({ tag: "right", value: Scoping.typeSchemeToFType(ts) })))(LibMaps.lookup(name)(((_x) => _x.schemaTypes)(graph)))));
}

export function requireUnionField<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Name) => ((x: Core.Name) => Errors.Error | Core.Type))) {
  return ((graph: Graph.Graph) => ((tname: Core.Name) => ((fname: Core.Name) => (() => {
  const withRowType = ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const matches = LibLists.filter(((ft: Core.FieldType) => LibEquality.equal(((_x) => _x.name)(ft))(fname)))(rt);
  return LibLogic.ifElse(LibLists.null_(matches))(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noMatchingField", value: ({
    fieldName: fname
  }) }) }) }))(({ tag: "right", value: ((_x) => _x.type)(LibLists.head(matches)) }));
})());
  return LibEithers.bind(requireUnionType(cx)(graph)(tname))(withRowType);
})())));
}

export function requireUnionType<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Name) => Errors.Error | ReadonlyArray<Core.FieldType>)) {
  return ((graph: Graph.Graph) => ((name: Core.Name) => (() => {
  const toUnion = ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => rt)((_m as any).value);
    default: return null(_m);
  }
})());
  return requireRowType(cx)("union")(toUnion)(graph)(name);
})()));
}

export function resolveType(graph: Graph.Graph): ((x: Core.Type) => Core.Type | null) {
  return ((typ: Core.Type) => (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "variable": return ((name: Core.Name) => LibMaybes.maybe(LibMaybes.map(((ts: Core.TypeScheme) => Scoping.typeSchemeToFType(ts)))(LibMaps.lookup(name)(((_x) => _x.boundTypes)(graph))))(((ts: Core.TypeScheme) => Scoping.typeSchemeToFType(ts)))(LibMaps.lookup(name)(((_x) => _x.schemaTypes)(graph))))((_m as any).value);
    default: return typ(_m);
  }
})());
}

export function typeToTypeScheme(t0: Core.Type): Core.TypeScheme {
  return (() => {
  const helper = ((vars: ReadonlyArray<Core.Name>) => ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => helper(LibLists.cons(((_x) => _x.parameter)(ft))(vars))(((_x) => _x.body)(ft)))((_m as any).value);
    default: return ({
    variables: LibLists.reverse(vars),
    type: t,
    constraints: null
  })(_m);
  }
})()));
  return helper([])(t0);
})();
}
