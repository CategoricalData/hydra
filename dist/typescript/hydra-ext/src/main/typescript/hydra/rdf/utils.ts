// Note: this is an automatically generated file. Do not edit.

/**
 * Utility functions for working with RDF graphs and descriptions
 */



import * as Annotations from "../annotations.js";
import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Names from "../names.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as RdfSyntax from "./syntax.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function descriptionsToGraph(ds: ReadonlyArray<RdfSyntax.Description>): RdfSyntax.Graph {
  return LibSets.fromList(triplesOf(ds));
}

export function emptyDescription(node: RdfSyntax.Node): RdfSyntax.Description {
  return ({
    subject: node,
    graph: emptyRdfGraph
  });
}

export const emptyLangStrings: RdfSyntax.LangStrings = LibMaps.empty;

export const emptyRdfGraph: RdfSyntax.Graph = LibSets.empty;

export function encodeLiteral(lit: Core.Literal): RdfSyntax.Literal {
  return (() => {
  const _m = lit;
  switch (_m.tag) {
    case "binary": return ((s: Uint8Array) => ({
    lexicalForm: LibLiterals.binaryToString(s),
    datatypeIri: xmlSchemaDatatypeIri("base64Binary"),
    languageTag: null
  }))((_m as any).value);
    case "boolean": return ((b: boolean) => ({
    lexicalForm: LibLogic.ifElse(b)("true")("false"),
    datatypeIri: xmlSchemaDatatypeIri("boolean"),
    languageTag: null
  }))((_m as any).value);
    case "float": return ((f: Core.FloatValue) => (() => {
  const _m = f;
  switch (_m.tag) {
    case "bigfloat": return ((v: number) => ({
    lexicalForm: LibLiterals.showBigfloat(v),
    datatypeIri: xmlSchemaDatatypeIri("decimal"),
    languageTag: null
  }))((_m as any).value);
    case "float32": return ((v: number) => ({
    lexicalForm: LibLiterals.showFloat32(v),
    datatypeIri: xmlSchemaDatatypeIri("float"),
    languageTag: null
  }))((_m as any).value);
    case "float64": return ((v: number) => ({
    lexicalForm: LibLiterals.showFloat64(v),
    datatypeIri: xmlSchemaDatatypeIri("double"),
    languageTag: null
  }))((_m as any).value);
  }
})())((_m as any).value);
    case "integer": return ((i: Core.IntegerValue) => (() => {
  const _m = i;
  switch (_m.tag) {
    case "bigint": return ((v: bigint) => ({
    lexicalForm: LibLiterals.showBigint(v),
    datatypeIri: xmlSchemaDatatypeIri("integer"),
    languageTag: null
  }))((_m as any).value);
    case "int8": return ((v: number) => ({
    lexicalForm: LibLiterals.showInt8(v),
    datatypeIri: xmlSchemaDatatypeIri("byte"),
    languageTag: null
  }))((_m as any).value);
    case "int16": return ((v: bigint) => ({
    lexicalForm: LibLiterals.showInt16(v),
    datatypeIri: xmlSchemaDatatypeIri("short"),
    languageTag: null
  }))((_m as any).value);
    case "int32": return ((v: number) => ({
    lexicalForm: LibLiterals.showInt32(v),
    datatypeIri: xmlSchemaDatatypeIri("int"),
    languageTag: null
  }))((_m as any).value);
    case "int64": return ((v: bigint) => ({
    lexicalForm: LibLiterals.showInt64(v),
    datatypeIri: xmlSchemaDatatypeIri("long"),
    languageTag: null
  }))((_m as any).value);
    case "uint8": return ((v: bigint) => ({
    lexicalForm: LibLiterals.showUint8(v),
    datatypeIri: xmlSchemaDatatypeIri("unsignedByte"),
    languageTag: null
  }))((_m as any).value);
    case "uint16": return ((v: number) => ({
    lexicalForm: LibLiterals.showUint16(v),
    datatypeIri: xmlSchemaDatatypeIri("unsignedShort"),
    languageTag: null
  }))((_m as any).value);
    case "uint32": return ((v: bigint) => ({
    lexicalForm: LibLiterals.showUint32(v),
    datatypeIri: xmlSchemaDatatypeIri("unsignedInt"),
    languageTag: null
  }))((_m as any).value);
    case "uint64": return ((v: bigint) => ({
    lexicalForm: LibLiterals.showUint64(v),
    datatypeIri: xmlSchemaDatatypeIri("unsignedLong"),
    languageTag: null
  }))((_m as any).value);
  }
})())((_m as any).value);
    case "string": return ((s: string) => ({
    lexicalForm: s,
    datatypeIri: xmlSchemaDatatypeIri("string"),
    languageTag: null
  }))((_m as any).value);
  }
})();
}

export function forObjects(subj: RdfSyntax.Resource): ((x: RdfSyntax.Iri) => ((x: ReadonlyArray<RdfSyntax.Node>) => ReadonlyArray<RdfSyntax.Triple>)) {
  return ((pred: RdfSyntax.Iri) => ((objs: ReadonlyArray<RdfSyntax.Node>) => LibLists.map(((obj: RdfSyntax.Node) => ({
    subject: subj,
    predicate: pred,
    object: obj
  })))(objs)));
}

export function iri(ns: string): ((x: string) => RdfSyntax.Iri) {
  return ((local: string) => LibStrings.cat2(ns)(local));
}

export function keyIri(local: string): RdfSyntax.Iri {
  return iri("urn:key:")(local);
}

export const key_rdfBlankNodeCounter: Core.Name = "rdfBlankNodeCounter";

export function mergeGraphs(graphs: ReadonlyArray<RdfSyntax.Graph>): RdfSyntax.Graph {
  return LibSets.unions(LibLists.map(((_x) => _x))(graphs));
}

export function nameToIri(name: Core.Name): RdfSyntax.Iri {
  return LibStrings.cat2("urn:")(((_x) => _x)(name));
}

export function nextBlankNode(cx: Context.Context): readonly [RdfSyntax.Resource, Context.Context] {
  return (() => {
  const result = Annotations.nextCount(key_rdfBlankNodeCounter)(cx);
  const count = LibPairs.first(result);
  const cx_ = LibPairs.second(result);
  return [({ tag: "bnode", value: LibStrings.cat2("b")(LibLiterals.showInt32(count)) }), cx_];
})();
}

export function propertyIri(rname: Core.Name): ((x: Core.Name) => RdfSyntax.Iri) {
  return ((fname: Core.Name) => (() => {
  const qualName = Names.qualifyName(rname);
  const gname = ((_x) => _x.namespace)(qualName);
  const local_ = ((_x) => _x.local)(qualName);
  return LibStrings.cat(["urn:", LibMaybes.maybe("")(((_x) => _x))(gname), "#", Formatting.decapitalize(local_), Formatting.capitalize(((_x) => _x)(fname))]);
})());
}

export function rdfIri(local: string): RdfSyntax.Iri {
  return iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#")(local);
}

export function resourceToNode(r: RdfSyntax.Resource): RdfSyntax.Node {
  return (() => {
  const _m = r;
  switch (_m.tag) {
    case "iri": return ((i: RdfSyntax.Iri) => ({ tag: "iri", value: i }))((_m as any).value);
    case "bnode": return ((b: RdfSyntax.BlankNode) => ({ tag: "bnode", value: b }))((_m as any).value);
  }
})();
}

export function subjectsOf(descs: ReadonlyArray<RdfSyntax.Description>): ReadonlyArray<RdfSyntax.Node> {
  return LibLists.map(((_x) => _x.subject))(descs);
}

export function triplesOf(descs: ReadonlyArray<RdfSyntax.Description>): ReadonlyArray<RdfSyntax.Triple> {
  return LibLists.concat(LibLists.map(((d: RdfSyntax.Description) => LibSets.toList(((_x) => _x)(((_x) => _x.graph)(d)))))(descs));
}

export function xmlSchemaDatatypeIri(local: string): RdfSyntax.Iri {
  return iri("http://www.w3.org/2001/XMLSchema#")(local);
}
