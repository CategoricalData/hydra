// Note: this is an automatically generated file. Do not edit.

/**
 * Serialization functions for converting RDF graphs to N-Triples format expressions
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
import * as LibMaybes from "../lib/maybes.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as RdfSyntax from "./syntax.js";
import * as Relational from "../relational.js";
import * as Serialization from "../serialization.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function escapeIriChar(c: number): string {
  return LibLogic.ifElse(LibLogic.or(LibEquality.gte(c)(128))(LibLogic.or(LibEquality.lte(c)(32))(LibLogic.or(LibEquality.equal(c)(60))(LibLogic.or(LibEquality.equal(c)(62))(LibLogic.or(LibEquality.equal(c)(34))(LibLogic.or(LibEquality.equal(c)(123))(LibLogic.or(LibEquality.equal(c)(125))(LibLogic.or(LibEquality.equal(c)(124))(LibLogic.or(LibEquality.equal(c)(94))(LibLogic.or(LibEquality.equal(c)(96))(LibEquality.equal(c)(92))))))))))))("?")(LibStrings.fromList([c]));
}

export function escapeIriStr(s: string): string {
  return LibStrings.cat(LibLists.map(escapeIriChar)(LibStrings.toList(s)));
}

export function escapeLiteralChar(c: number): string {
  return LibLogic.ifElse(LibEquality.gte(c)(128))("?")(LibLogic.ifElse(LibEquality.equal(c)(34))("\\\"")(LibLogic.ifElse(LibEquality.equal(c)(92))("\\\\")(LibLogic.ifElse(LibEquality.equal(c)(10))("\\n")(LibLogic.ifElse(LibEquality.equal(c)(13))("\\r")(LibStrings.fromList([c]))))));
}

export function escapeLiteralString(s: string): string {
  return LibStrings.cat(LibLists.map(escapeLiteralChar)(LibStrings.toList(s)));
}

export function rdfGraphToNtriples(g: RdfSyntax.Graph): string {
  return Serialization.printExpr(writeGraph(g));
}

export function writeBlankNode(bnode: RdfSyntax.BlankNode): Ast.Expr {
  return Serialization.noSep([Serialization.cst("_:"), Serialization.cst(((_x) => _x)(bnode))]);
}

export function writeGraph(g: RdfSyntax.Graph): Ast.Expr {
  return Serialization.newlineSep(LibLists.map(writeTriple)(LibSets.toList(((_x) => _x)(g))));
}

export function writeIri(iri: RdfSyntax.Iri): Ast.Expr {
  return Serialization.noSep([Serialization.cst("<"), Serialization.cst(escapeIriStr(((_x) => _x)(iri))), Serialization.cst(">")]);
}

export function writeLanguageTag(lang: RdfSyntax.LanguageTag): Ast.Expr {
  return Serialization.noSep([Serialization.cst("@"), Serialization.cst(((_x) => _x)(lang))]);
}

export function writeLiteral(lit: RdfSyntax.Literal): Ast.Expr {
  return (() => {
  const lex = ((_x) => _x.lexicalForm)(lit);
  const dt = ((_x) => _x.datatypeIri)(lit);
  const lang = ((_x) => _x.languageTag)(lit);
  const lexExpr = Serialization.cst(LibStrings.cat(["\"", escapeLiteralString(lex), "\""]));
  const suffix = LibMaybes.maybe(Serialization.noSep([Serialization.cst("^^"), writeIri(dt)]))(writeLanguageTag)(lang);
  return Serialization.noSep([lexExpr, suffix]);
})();
}

export function writeNode(n: RdfSyntax.Node): Ast.Expr {
  return (() => {
  const _m = n;
  switch (_m.tag) {
    case "iri": return ((iri: RdfSyntax.Iri) => writeIri(iri))((_m as any).value);
    case "bnode": return ((bnode: RdfSyntax.BlankNode) => writeBlankNode(bnode))((_m as any).value);
    case "literal": return ((lit: RdfSyntax.Literal) => writeLiteral(lit))((_m as any).value);
  }
})();
}

export function writeResource(r: RdfSyntax.Resource): Ast.Expr {
  return (() => {
  const _m = r;
  switch (_m.tag) {
    case "iri": return ((iri: RdfSyntax.Iri) => writeIri(iri))((_m as any).value);
    case "bnode": return ((bnode: RdfSyntax.BlankNode) => writeBlankNode(bnode))((_m as any).value);
  }
})();
}

export function writeTriple(t: RdfSyntax.Triple): Ast.Expr {
  return (() => {
  const subj = ((_x) => _x.subject)(t);
  const pred = ((_x) => _x.predicate)(t);
  const obj = ((_x) => _x.object)(t);
  return Serialization.spaceSep([writeResource(subj), writeIri(pred), writeNode(obj), Serialization.cst(".")]);
})();
}
