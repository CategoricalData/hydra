// Note: this is an automatically generated file. Do not edit.

/**
 * Functions for transforming property graph mappings into property graph elements.
 */



import * as Ast from "../../ast.js";
import * as Classes from "../../classes.js";
import * as Coders from "../../coders.js";
import * as Context from "../../context.js";
import * as Core from "../../core.js";
import * as ErrorChecking from "../../error/checking.js";
import * as ErrorCore from "../../error/core.js";
import * as ErrorPackaging from "../../error/packaging.js";
import * as Errors from "../../errors.js";
import * as ExtractCore from "../../extract/core.js";
import * as Graph from "../../graph.js";
import * as JsonModel from "../../json/model.js";
import * as LibChars from "../../lib/chars.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibEquality from "../../lib/equality.js";
import * as LibLists from "../../lib/lists.js";
import * as LibLiterals from "../../lib/literals.js";
import * as LibLogic from "../../lib/logic.js";
import * as LibMaps from "../../lib/maps.js";
import * as LibMaybes from "../../lib/maybes.js";
import * as LibPairs from "../../lib/pairs.js";
import * as LibSets from "../../lib/sets.js";
import * as LibStrings from "../../lib/strings.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as PgModel from "../../pg/model.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Reduction from "../../reduction.js";
import * as Relational from "../../relational.js";
import * as Rewriting from "../../rewriting.js";
import * as Strip from "../../strip.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export function concatPairs<t0, t1>(acc: readonly [ReadonlyArray<t0>, ReadonlyArray<t1>]): ((x: readonly [ReadonlyArray<t0>, ReadonlyArray<t1>]) => readonly [ReadonlyArray<t0>, ReadonlyArray<t1>]) {
  return ((p: readonly [ReadonlyArray<t0>, ReadonlyArray<t1>]) => [LibLists.concat2(LibPairs.first(acc))(LibPairs.first(p)), LibLists.concat2(LibPairs.second(acc))(LibPairs.second(p))]);
}

export function decodeCell(colType: Tabular.ColumnType): ((x: string | null) => string | Core.Term | null) {
  return ((mvalue: string | null) => (() => {
  const cname = ((_x) => _x)(((_x) => _x.name)(colType));
  return (() => {
  const typ = ((_x) => _x.type)(colType);
  return (() => {
  const decodeValue = ((value: string) => (() => {
  const parseError = LibStrings.cat(["Invalid value for column ", cname, ": ", value]);
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "literal": return ((lt: Core.LiteralType) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "boolean": return ((_: void) => LibMaybes.maybe(({ tag: "left", value: parseError }))(((parsed: boolean) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "boolean", value: parsed }) }) })))(LibLiterals.readBoolean(value)))((_m as any).value);
    case "float": return ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => LibMaybes.maybe(({ tag: "left", value: parseError }))(((parsed: number) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: parsed }) }) }) })))(LibLiterals.readBigfloat(value)))((_m as any).value);
    case "float32": return ((_: void) => LibMaybes.maybe(({ tag: "left", value: parseError }))(((parsed: number) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: parsed }) }) }) })))(LibLiterals.readFloat32(value)))((_m as any).value);
    case "float64": return ((_: void) => LibMaybes.maybe(({ tag: "left", value: parseError }))(((parsed: number) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: parsed }) }) }) })))(LibLiterals.readFloat64(value)))((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat(["Unsupported float type for column ", cname]) })(_m);
  }
})())((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "int32": return ((_: void) => LibMaybes.maybe(({ tag: "left", value: parseError }))(((parsed: number) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: parsed }) }) }) })))(LibLiterals.readInt32(value)))((_m as any).value);
    case "int64": return ((_: void) => LibMaybes.maybe(({ tag: "left", value: parseError }))(((parsed: bigint) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: parsed }) }) }) })))(LibLiterals.readInt64(value)))((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat(["Unsupported integer type for column ", cname]) })(_m);
  }
})())((_m as any).value);
    case "string": return ((_: void) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "string", value: value }) }) }))((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat(["Unsupported literal type for column ", cname]) })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat(["Unsupported type for column ", cname]) })(_m);
  }
})();
})());
  return LibMaybes.maybe(({ tag: "right", value: null }))(decodeValue)(mvalue);
})();
})();
})());
}

export function decodeRow(colTypes: ReadonlyArray<Tabular.ColumnType>): ((x: Tabular.DataRow<string>) => string | Tabular.DataRow<Core.Term>) {
  return ((row: Tabular.DataRow<string>) => (() => {
  const cells = ((_x) => _x)(row);
  return LibEithers.map(((decodedCells: ReadonlyArray<Core.Term | null>) => decodedCells))(LibEithers.mapList(((pair: readonly [Tabular.ColumnType, string | null]) => (() => {
  const colType = LibPairs.first(pair);
  return (() => {
  const mvalue = LibPairs.second(pair);
  return decodeCell(colType)(mvalue);
})();
})()))(LibLists.zip(colTypes)(cells)));
})());
}

export function decodeTable(tableType: Tabular.TableType): ((x: Tabular.Table<string>) => string | Tabular.Table<Core.Term>) {
  return ((table: Tabular.Table<string>) => (() => {
  const colTypes = ((_x) => _x.columns)(tableType);
  return (() => {
  const header = ((_x) => _x.header)(table);
  return (() => {
  const rows = ((_x) => _x.data)(table);
  return LibEithers.map(((decodedRows: ReadonlyArray<Tabular.DataRow<Core.Term>>) => ({
    header: header,
    data: decodedRows
  })))(LibEithers.mapList(((row: Tabular.DataRow<string>) => decodeRow(colTypes)(row)))(rows));
})();
})();
})());
}

export function elementIsEdge<t0>(el: PgModel.Element<t0>): boolean {
  return ((v1: PgModel.Element) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "edge": return ((_: PgModel.Edge<t0>) => true)((_m as any).value);
    default: return false(_m);
  }
})())(el);
}

export function elementIsVertex<t0>(el: PgModel.Element<t0>): boolean {
  return ((v1: PgModel.Element) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "vertex": return ((_: PgModel.Vertex<t0>) => true)((_m as any).value);
    default: return false(_m);
  }
})())(el);
}

export function elementSpecsByTable(graph: PgModel.LazyGraph<Core.Term>): string | ReadonlyMap<string, readonly [ReadonlyArray<PgModel.Vertex<Core.Term>>, ReadonlyArray<PgModel.Edge<Core.Term>>]> {
  return (() => {
  const vertices = ((_x) => _x.vertices)(graph);
  return (() => {
  const edges = ((_x) => _x.edges)(graph);
  return LibEithers.bind(LibEithers.mapList(((v: PgModel.Vertex<Core.Term>) => LibEithers.map(((t: string) => [t, v]))(tableForVertex(v))))(vertices))(((vertexPairs: ReadonlyArray<readonly [string, PgModel.Vertex<Core.Term>]>) => LibEithers.bind(LibEithers.mapList(((e: PgModel.Edge<Core.Term>) => LibEithers.map(((t: string) => [t, e]))(tableForEdge(e))))(edges))(((edgePairs: ReadonlyArray<readonly [string, PgModel.Edge<Core.Term>]>) => (() => {
  const addVertex = ((m: ReadonlyMap<t0, readonly [ReadonlyArray<t1>, ReadonlyArray<t2>]>) => ((p: readonly [t0, t1]) => (() => {
  const table = LibPairs.first(p);
  return (() => {
  const v = LibPairs.second(p);
  return (() => {
  const existing = LibMaps.lookup(table)(m);
  return (() => {
  const current = LibMaybes.fromMaybe([[], []])(existing);
  return LibMaps.insert(table)([LibLists.cons(v)(LibPairs.first(current)), LibPairs.second(current)])(m);
})();
})();
})();
})()));
  return (() => {
  const addEdge = ((m: ReadonlyMap<t0, readonly [ReadonlyArray<t1>, ReadonlyArray<t2>]>) => ((p: readonly [t0, t2]) => (() => {
  const table = LibPairs.first(p);
  return (() => {
  const e = LibPairs.second(p);
  return (() => {
  const existing = LibMaps.lookup(table)(m);
  return (() => {
  const current = LibMaybes.fromMaybe([[], []])(existing);
  return LibMaps.insert(table)([LibPairs.first(current), LibLists.cons(e)(LibPairs.second(current))])(m);
})();
})();
})();
})()));
  return (() => {
  const vertexMap = LibLists.foldl(addVertex)(LibMaps.empty)(vertexPairs);
  return ({ tag: "right", value: LibLists.foldl(addEdge)(vertexMap)(edgePairs) });
})();
})();
})()))));
})();
})();
}

export function evaluateEdge(cx: Context.Context): ((x: Graph.Graph) => ((x: PgModel.Edge<Core.Term>) => ((x: Core.Term) => Errors.Error | PgModel.Edge<Core.Term> | null))) {
  return ((g: Graph.Graph) => ((edgeSpec: PgModel.Edge<Core.Term>) => ((record: Core.Term) => (() => {
  const label = ((_x) => _x.label)(edgeSpec);
  return (() => {
  const idSpec = ((_x) => _x.id)(edgeSpec);
  return (() => {
  const outSpec = ((_x) => _x.out)(edgeSpec);
  return (() => {
  const inSpec = ((_x) => _x.in)(edgeSpec);
  return (() => {
  const propSpecs = ((_x) => _x.properties)(edgeSpec);
  return LibEithers.bind(Reduction.reduceTerm(cx)(g)(true)(({ tag: "application", value: ({
    function: idSpec,
    argument: record
  }) })))(((id: Core.Term) => LibEithers.bind(LibEithers.bind(Reduction.reduceTerm(cx)(g)(true)(({ tag: "application", value: ({
    function: outSpec,
    argument: record
  }) })))(((__term: Core.Term) => ExtractCore.maybeTerm(((t: Core.Term) => ({ tag: "right", value: t })))(g)(__term))))(((mOutId: Core.Term | null) => LibEithers.bind(LibEithers.bind(Reduction.reduceTerm(cx)(g)(true)(({ tag: "application", value: ({
    function: inSpec,
    argument: record
  }) })))(((__term: Core.Term) => ExtractCore.maybeTerm(((t: Core.Term) => ({ tag: "right", value: t })))(g)(__term))))(((mInId: Core.Term | null) => LibEithers.bind(evaluateProperties(cx)(g)(propSpecs)(record))(((props: ReadonlyMap<PgModel.PropertyKey, Core.Term>) => ({ tag: "right", value: LibMaybes.bind(mOutId)(((outId: Core.Term) => LibMaybes.map(((inId: Core.Term) => ({
    label: label,
    id: id,
    out: outId,
    in: inId,
    properties: props
  })))(mInId))) })))))))));
})();
})();
})();
})();
})())));
}

export function evaluateProperties<t0>(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyMap<t0, Core.Term>) => ((x: Core.Term) => Errors.Error | ReadonlyMap<t0, Core.Term>))) {
  return ((g: Graph.Graph) => ((specs: ReadonlyMap<t0, Core.Term>) => ((record: Core.Term) => (() => {
  const extractMaybe = ((k: t1) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "maybe": return ((mv: Core.Term | null) => ({ tag: "right", value: LibMaybes.map(((v: Core.Term) => [k, v]))(mv) }))((_m as any).value);
  }
})()));
  return LibEithers.map(((pairs: ReadonlyArray<readonly [t0, Core.Term] | null>) => LibMaps.fromList(LibMaybes.cat(pairs))))(LibEithers.mapList(((pair: readonly [t0, Core.Term]) => (() => {
  const k = LibPairs.first(pair);
  return (() => {
  const spec = LibPairs.second(pair);
  return LibEithers.bind(Reduction.reduceTerm(cx)(g)(true)(({ tag: "application", value: ({
    function: spec,
    argument: record
  }) })))(((value: Core.Term) => extractMaybe(k)(Strip.deannotateTerm(value))));
})();
})()))(LibMaps.toList(specs)));
})())));
}

export function evaluateVertex(cx: Context.Context): ((x: Graph.Graph) => ((x: PgModel.Vertex<Core.Term>) => ((x: Core.Term) => Errors.Error | PgModel.Vertex<Core.Term> | null))) {
  return ((g: Graph.Graph) => ((vertexSpec: PgModel.Vertex<Core.Term>) => ((record: Core.Term) => (() => {
  const label = ((_x) => _x.label)(vertexSpec);
  return (() => {
  const idSpec = ((_x) => _x.id)(vertexSpec);
  return (() => {
  const propSpecs = ((_x) => _x.properties)(vertexSpec);
  return LibEithers.bind(LibEithers.bind(Reduction.reduceTerm(cx)(g)(true)(({ tag: "application", value: ({
    function: idSpec,
    argument: record
  }) })))(((__term: Core.Term) => ExtractCore.maybeTerm(((t: Core.Term) => ({ tag: "right", value: t })))(g)(__term))))(((mId: Core.Term | null) => LibEithers.bind(evaluateProperties(cx)(g)(propSpecs)(record))(((props: ReadonlyMap<PgModel.PropertyKey, Core.Term>) => ({ tag: "right", value: LibMaybes.map(((id: Core.Term) => ({
    label: label,
    id: id,
    properties: props
  })))(mId) })))));
})();
})();
})())));
}

export function findTablesInTerm(term: Core.Term): ReadonlySet<string> {
  return Rewriting.foldOverTerm(({ tag: "pre" }))(((names: ReadonlySet<string>) => ((t: Core.Term) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "project": return ((proj: Core.Projection) => LibSets.insert(((_x) => _x)(((_x) => _x.typeName)(proj)))(names))((_m as any).value);
    default: return names(_m);
  }
})())))(LibSets.empty)(term);
}

export function findTablesInTerms(terms: ReadonlyArray<Core.Term>): ReadonlySet<string> {
  return LibSets.unions(LibLists.map(findTablesInTerm)(terms));
}

export function listAny<t0>(pred: ((x: t0) => boolean)): ((x: ReadonlyArray<t0>) => boolean) {
  return ((xs: ReadonlyArray<t0>) => LibLogic.not(LibLists.null_(LibLists.filter(pred)(xs))));
}

export function makeLazyGraph<t0>(vertices: ReadonlyArray<PgModel.Vertex<t0>>): ((x: ReadonlyArray<PgModel.Edge<t0>>) => PgModel.LazyGraph<t0>) {
  return ((edges: ReadonlyArray<PgModel.Edge<t0>>) => ({
    vertices: vertices,
    edges: edges
  }));
}

export function normalizeField(s: string): string | null {
  return LibLogic.ifElse(LibStrings.null_(s))(null)(s);
}

export function parseCsvChar(state: readonly [readonly [ReadonlyArray<string | null>, string], boolean]): ((x: number) => readonly [readonly [ReadonlyArray<string | null>, string], boolean]) {
  return ((c: number) => (() => {
  const acc = LibPairs.first(LibPairs.first(state));
  return (() => {
  const field = LibPairs.second(LibPairs.first(state));
  return (() => {
  const inQuotes = LibPairs.second(state);
  return LibLogic.ifElse(LibEquality.equal(c)(34))(LibLogic.ifElse(inQuotes)([[acc, field], false])(LibLogic.ifElse(LibStrings.null_(field))([[acc, field], true])([[acc, LibStrings.cat2(field)("\"")], inQuotes])))(LibLogic.ifElse(LibLogic.and(LibEquality.equal(c)(44))(LibLogic.not(inQuotes)))([[LibLists.cons(normalizeField(field))(acc), ""], false])([[acc, LibStrings.cat2(field)(LibStrings.fromList([c]))], inQuotes]));
})();
})();
})());
}

export function parseCsvLine(line: string): string | ReadonlyArray<string | null> {
  return (() => {
  const chars = LibStrings.toList(line);
  return (() => {
  const initState = [[[], ""], false];
  return (() => {
  const finalState = LibLists.foldl(parseCsvChar)(initState)(chars);
  return (() => {
  const acc = LibPairs.first(LibPairs.first(finalState));
  return (() => {
  const field = LibPairs.second(LibPairs.first(finalState));
  return (() => {
  const inQuotes = LibPairs.second(finalState);
  return LibLogic.ifElse(inQuotes)(({ tag: "left", value: "Unclosed quoted field" }))(({ tag: "right", value: LibLists.reverse(LibLists.cons(normalizeField(field))(acc)) }));
})();
})();
})();
})();
})();
})();
}

export function parseSingleLine(line: string): string | ReadonlyArray<string | null> | null {
  return (() => {
  const trimmed = stripWhitespace(line);
  return LibLogic.ifElse(LibStrings.null_(trimmed))(({ tag: "right", value: null }))(LibEithers.map(((x: ReadonlyArray<string | null>) => x))(parseCsvLine(trimmed)));
})();
}

export function parseTableLines(hasHeader: boolean): ((x: ReadonlyArray<string>) => string | Tabular.Table<string>) {
  return ((rawLines: ReadonlyArray<string>) => LibEithers.bind(LibEithers.mapList(((ln: string) => parseSingleLine(ln)))(rawLines))(((parsedRows: ReadonlyArray<ReadonlyArray<string | null> | null>) => (() => {
  const rows = LibMaybes.cat(parsedRows);
  return LibLogic.ifElse(hasHeader)((() => {
  const headerRow = LibLists.head(rows);
  return (() => {
  const dataRows = LibLists.tail(rows);
  return LibLogic.ifElse(listAny(((m: string | null) => LibMaybes.isNothing(m)))(headerRow))(({ tag: "left", value: "null header column(s)" }))(({ tag: "right", value: ({
    header: LibMaybes.cat(headerRow),
    data: LibLists.map(((r: ReadonlyArray<string | null>) => r))(dataRows)
  }) }));
})();
})())(({ tag: "right", value: ({
    header: null,
    data: LibLists.map(((r: ReadonlyArray<string | null>) => r))(rows)
  }) }));
})())));
}

export function stripWhitespace(s: string): string {
  return (() => {
  const chars = LibStrings.toList(s);
  return (() => {
  const isSpaceChar = ((c: number) => LibChars.isSpace(c));
  return (() => {
  const trimLeft = LibLists.dropWhile(isSpaceChar)(chars);
  return (() => {
  const trimRight = LibLists.reverse(LibLists.dropWhile(isSpaceChar)(LibLists.reverse(trimLeft)));
  return LibStrings.fromList(trimRight);
})();
})();
})();
})();
}

export function tableForEdge(edge: PgModel.Edge<Core.Term>): string | string {
  return (() => {
  const label = ((_x) => _x.label)(edge);
  return (() => {
  const id = ((_x) => _x.id)(edge);
  return (() => {
  const outId = ((_x) => _x.out)(edge);
  return (() => {
  const inId = ((_x) => _x.in)(edge);
  return (() => {
  const props = ((_x) => _x.properties)(edge);
  return (() => {
  const tables = findTablesInTerms(LibLists.concat2([id, outId, inId])(LibMaps.elems(props)));
  return LibLogic.ifElse(LibEquality.equal(LibSets.size(tables))(1))(({ tag: "right", value: LibLists.head(LibSets.toList(tables)) }))(({ tag: "left", value: LibStrings.cat(["Specification for ", ((_x) => _x)(label), " edges has wrong number of tables"]) }));
})();
})();
})();
})();
})();
})();
}

export function tableForVertex(vertex: PgModel.Vertex<Core.Term>): string | string {
  return (() => {
  const label = ((_x) => _x.label)(vertex);
  return (() => {
  const id = ((_x) => _x.id)(vertex);
  return (() => {
  const props = ((_x) => _x.properties)(vertex);
  return (() => {
  const tables = findTablesInTerms(LibLists.cons(id)(LibMaps.elems(props)));
  return LibLogic.ifElse(LibEquality.equal(LibSets.size(tables))(1))(({ tag: "right", value: LibLists.head(LibSets.toList(tables)) }))(({ tag: "left", value: LibStrings.cat(["Specification for ", ((_x) => _x)(label), " vertices has wrong number of tables"]) }));
})();
})();
})();
})();
}

export function tableTypesByName(tableTypes: ReadonlyArray<Tabular.TableType>): ReadonlyMap<Relational.RelationName, Tabular.TableType> {
  return LibMaps.fromList(LibLists.map(((t: Tabular.TableType) => [((_x) => _x.name)(t), t]))(tableTypes));
}

export function termRowToRecord(tableType: Tabular.TableType): ((x: Tabular.DataRow<Core.Term>) => Core.Term) {
  return ((row: Tabular.DataRow<Core.Term>) => (() => {
  const tname = ((_x) => _x)(((_x) => _x.name)(tableType));
  return (() => {
  const colTypes = ((_x) => _x.columns)(tableType);
  return (() => {
  const cells = ((_x) => _x)(row);
  return ({ tag: "record", value: ({
    typeName: tname,
    fields: LibLists.zipWith(((colType: Tabular.ColumnType) => ((mvalue: Core.Term | null) => (() => {
  const cname = ((_x) => _x)(((_x) => _x.name)(colType));
  return ({
    name: cname,
    term: ({ tag: "maybe", value: mvalue })
  });
})())))(colTypes)(cells)
  }) });
})();
})();
})());
}

export function transformRecord(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<PgModel.Vertex<Core.Term>>) => ((x: ReadonlyArray<PgModel.Edge<Core.Term>>) => ((x: Core.Term) => Errors.Error | readonly [ReadonlyArray<PgModel.Vertex<Core.Term>>, ReadonlyArray<PgModel.Edge<Core.Term>>])))) {
  return ((g: Graph.Graph) => ((vspecs: ReadonlyArray<PgModel.Vertex<Core.Term>>) => ((especs: ReadonlyArray<PgModel.Edge<Core.Term>>) => ((record: Core.Term) => LibEithers.bind(LibEithers.mapList(((spec: PgModel.Vertex<Core.Term>) => evaluateVertex(cx)(g)(spec)(record)))(vspecs))(((mVertices: ReadonlyArray<PgModel.Vertex<Core.Term> | null>) => LibEithers.bind(LibEithers.mapList(((spec: PgModel.Edge<Core.Term>) => evaluateEdge(cx)(g)(spec)(record)))(especs))(((mEdges: ReadonlyArray<PgModel.Edge<Core.Term> | null>) => ({ tag: "right", value: [LibMaybes.cat(mVertices), LibMaybes.cat(mEdges)] })))))))));
}

export function transformTableRows(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<PgModel.Vertex<Core.Term>>) => ((x: ReadonlyArray<PgModel.Edge<Core.Term>>) => ((x: Tabular.TableType) => ((x: ReadonlyArray<Tabular.DataRow<Core.Term>>) => Errors.Error | readonly [ReadonlyArray<PgModel.Vertex<Core.Term>>, ReadonlyArray<PgModel.Edge<Core.Term>>]))))) {
  return ((g: Graph.Graph) => ((vspecs: ReadonlyArray<PgModel.Vertex<Core.Term>>) => ((especs: ReadonlyArray<PgModel.Edge<Core.Term>>) => ((tableType: Tabular.TableType) => ((rows: ReadonlyArray<Tabular.DataRow<Core.Term>>) => LibEithers.map(((pairs: ReadonlyArray<readonly [ReadonlyArray<PgModel.Vertex<Core.Term>>, ReadonlyArray<PgModel.Edge<Core.Term>>]>) => LibLists.foldl(concatPairs)([[], []])(pairs)))(LibEithers.mapList(((row: Tabular.DataRow<Core.Term>) => transformRecord(cx)(g)(vspecs)(especs)(termRowToRecord(tableType)(row))))(rows)))))));
}
