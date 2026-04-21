// Note: this is an automatically generated file. Do not edit.

/**
 * Utilities for sorting. This module includes an implementation of Tarjan's algorithm, originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC.
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Constants from "./constants.js";
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
import * as LibMaps from "./lib/maps.js";
import * as LibMath from "./lib/math.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibSets from "./lib/sets.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function adjacencyListToMap<t0, t1>(pairs: ReadonlyArray<readonly [t0, ReadonlyArray<t1>]>): ReadonlyMap<t0, ReadonlyArray<t1>> {
  return LibLists.foldl(((mp: ReadonlyMap<t0, ReadonlyArray<t1>>) => ((p: readonly [t0, ReadonlyArray<t1>]) => (() => {
  const k = LibPairs.first(p);
  return (() => {
  const vs = LibPairs.second(p);
  return (() => {
  const existing = LibMaybes.maybe([])(LibEquality.identity)(LibMaps.lookup(k)(mp));
  return LibMaps.insert(k)(LibLists.concat2(existing)(vs))(mp);
})();
})();
})())))(LibMaps.empty)(pairs);
}

export function adjacencyListsToGraph<t0>(edges0: ReadonlyArray<readonly [t0, ReadonlyArray<t0>]>): readonly [ReadonlyMap<number, ReadonlyArray<number>>, ((x: number) => t0)] {
  return (() => {
  const sortedEdges = LibLists.sortOn(LibPairs.first)(edges0);
  return (() => {
  const indexedEdges = LibLists.zip(LibMath.range(0)(LibLists.length(sortedEdges)))(sortedEdges);
  return (() => {
  const keyToVertex = LibMaps.fromList(LibLists.map(((vkNeighbors: readonly [number, readonly [t0, ReadonlyArray<t0>]]) => (() => {
  const v = LibPairs.first(vkNeighbors);
  return (() => {
  const kNeighbors = LibPairs.second(vkNeighbors);
  return (() => {
  const k = LibPairs.first(kNeighbors);
  return [k, v];
})();
})();
})()))(indexedEdges));
  return (() => {
  const vertexMap = LibMaps.fromList(LibLists.map(((vkNeighbors: readonly [number, readonly [t0, ReadonlyArray<t0>]]) => (() => {
  const v = LibPairs.first(vkNeighbors);
  return (() => {
  const kNeighbors = LibPairs.second(vkNeighbors);
  return (() => {
  const k = LibPairs.first(kNeighbors);
  return [v, k];
})();
})();
})()))(indexedEdges));
  return (() => {
  const graph = LibMaps.fromList(LibLists.map(((vkNeighbors: readonly [number, readonly [t0, ReadonlyArray<t0>]]) => (() => {
  const v = LibPairs.first(vkNeighbors);
  return (() => {
  const kNeighbors = LibPairs.second(vkNeighbors);
  return (() => {
  const neighbors = LibPairs.second(kNeighbors);
  return [v, LibMaybes.mapMaybe(((k: t0) => LibMaps.lookup(k)(keyToVertex)))(neighbors)];
})();
})();
})()))(indexedEdges));
  return (() => {
  const vertexToKey = ((v: number) => LibMaybes.fromJust(LibMaps.lookup(v)(vertexMap)));
  return [graph, vertexToKey];
})();
})();
})();
})();
})();
})();
}

export function createOrderingIsomorphism<t0, t1>(sourceOrd: ReadonlyArray<t0>): ((x: ReadonlyArray<t0>) => Topology.OrderingIsomorphism<t1>) {
  return ((targetOrd: ReadonlyArray<t0>) => (() => {
  const sourceToTargetMapping = ((els: ReadonlyArray<t2>) => (() => {
  const mp = LibMaps.fromList(LibLists.zip(sourceOrd)(els));
  return LibMaybes.cat(LibLists.map(((n: t0) => LibMaps.lookup(n)(mp)))(targetOrd));
})());
  return (() => {
  const targetToSourceMapping = ((els: ReadonlyArray<t2>) => (() => {
  const mp = LibMaps.fromList(LibLists.zip(targetOrd)(els));
  return LibMaybes.cat(LibLists.map(((n: t0) => LibMaps.lookup(n)(mp)))(sourceOrd));
})());
  return ({
    encode: sourceToTargetMapping,
    decode: targetToSourceMapping
  });
})();
})());
}

export function findReachableNodes<t0>(adj: ((x: t0) => ReadonlySet<t0>)): ((x: t0) => ReadonlySet<t0>) {
  return ((root: t0) => (() => {
  const visit = ((visited: ReadonlySet<t0>) => ((node: t0) => (() => {
  const toVisit = LibSets.difference(adj(node))(visited);
  return LibLogic.ifElse(LibSets.null_(toVisit))(visited)(LibLists.foldl(((v: ReadonlySet<t0>) => ((n: t0) => visit(LibSets.insert(n)(v))(n))))(visited)(LibSets.toList(toVisit)));
})()));
  return visit(LibSets.singleton(root))(root);
})());
}

export const initialState: Topology.TarjanState = ({
    counter: 0,
    indices: LibMaps.empty,
    lowLinks: LibMaps.empty,
    stack: [],
    onStack: LibSets.empty,
    sccs: []
  });

export function popStackUntil(v: number): ((x: Topology.TarjanState) => readonly [ReadonlyArray<number>, Topology.TarjanState]) {
  return ((st0: Topology.TarjanState) => (() => {
  const go = ((acc: ReadonlyArray<number>) => ((st: Topology.TarjanState) => (() => {
  const x = LibLists.head(((_x) => _x.stack)(st));
  return (() => {
  const xs = LibLists.tail(((_x) => _x.stack)(st));
  return (() => {
  const newSt = ({
    counter: ((_x) => _x.counter)(st),
    indices: ((_x) => _x.indices)(st),
    lowLinks: ((_x) => _x.lowLinks)(st),
    stack: xs,
    onStack: ((_x) => _x.onStack)(st),
    sccs: ((_x) => _x.sccs)(st)
  });
  return (() => {
  const newSt2 = ({
    counter: ((_x) => _x.counter)(newSt),
    indices: ((_x) => _x.indices)(newSt),
    lowLinks: ((_x) => _x.lowLinks)(newSt),
    stack: ((_x) => _x.stack)(newSt),
    onStack: LibSets.delete_(x)(((_x) => _x.onStack)(st)),
    sccs: ((_x) => _x.sccs)(newSt)
  });
  return (() => {
  const acc_ = LibLists.cons(x)(acc);
  return LibLogic.ifElse(LibEquality.equal(x)(v))([LibLists.reverse(acc_), newSt2])(go(acc_)(newSt2));
})();
})();
})();
})();
})()));
  return go([])(st0);
})());
}

export function propagateTags<t0, t1>(edges: ReadonlyArray<readonly [t0, ReadonlyArray<t0>]>): ((x: ReadonlyArray<readonly [t0, ReadonlyArray<t1>]>) => ReadonlyArray<readonly [t0, ReadonlySet<t1>]>) {
  return ((nodeTags: ReadonlyArray<readonly [t0, ReadonlyArray<t1>]>) => (() => {
  const adjMap = adjacencyListToMap(edges);
  return (() => {
  const tagMap = LibMaps.map(LibSets.fromList)(adjacencyListToMap(nodeTags));
  return (() => {
  const allNodes = LibSets.toList(LibSets.fromList(LibLists.concat2(LibLists.map(LibPairs.first)(edges))(LibLists.map(LibPairs.first)(nodeTags))));
  return (() => {
  const getTagsForNode = ((node: t0) => (() => {
  const reachable = findReachableNodes(((n: t0) => LibSets.fromList(LibMaybes.maybe([])(LibEquality.identity)(LibMaps.lookup(n)(adjMap)))))(node);
  return LibSets.unions(LibLists.map(((n: t0) => LibMaybes.maybe(LibSets.empty)(LibEquality.identity)(LibMaps.lookup(n)(tagMap))))(LibSets.toList(reachable)));
})());
  return LibLists.map(((n: t0) => [n, getTagsForNode(n)]))(allNodes);
})();
})();
})();
})());
}

export function strongConnect(graph: ReadonlyMap<number, ReadonlyArray<number>>): ((x: number) => ((x: Topology.TarjanState) => Topology.TarjanState)) {
  return ((v: number) => ((st: Topology.TarjanState) => (() => {
  const i = ((_x) => _x.counter)(st);
  return (() => {
  const newSt = ({
    counter: LibMath.add(i)(1),
    indices: LibMaps.insert(v)(i)(((_x) => _x.indices)(st)),
    lowLinks: LibMaps.insert(v)(i)(((_x) => _x.lowLinks)(st)),
    stack: LibLists.cons(v)(((_x) => _x.stack)(st)),
    onStack: LibSets.insert(v)(((_x) => _x.onStack)(st)),
    sccs: ((_x) => _x.sccs)(st)
  });
  return (() => {
  const neighbors = LibMaps.findWithDefault([])(v)(graph);
  return (() => {
  const processNeighbor = ((st_: Topology.TarjanState) => ((w: number) => (() => {
  const lowLink = ((s: Topology.TarjanState) => (() => {
  const lowV1 = LibMaps.findWithDefault(Constants.maxInt32)(v)(((_x) => _x.lowLinks)(s));
  return (() => {
  const idx_w = LibMaps.findWithDefault(Constants.maxInt32)(w)(((_x) => _x.indices)(s));
  return ({
    counter: ((_x) => _x.counter)(s),
    indices: ((_x) => _x.indices)(s),
    lowLinks: LibMaps.insert(v)(LibEquality.min(lowV1)(idx_w))(((_x) => _x.lowLinks)(s)),
    stack: ((_x) => _x.stack)(s),
    onStack: ((_x) => _x.onStack)(s),
    sccs: ((_x) => _x.sccs)(s)
  });
})();
})());
  return LibLogic.ifElse(LibLogic.not(LibMaps.member(w)(((_x) => _x.indices)(st_))))((() => {
  const stAfter = strongConnect(graph)(w)(st_);
  return (() => {
  const lowV2 = LibMaps.findWithDefault(Constants.maxInt32)(v)(((_x) => _x.lowLinks)(stAfter));
  return (() => {
  const low_w = LibMaps.findWithDefault(Constants.maxInt32)(w)(((_x) => _x.lowLinks)(stAfter));
  return ({
    counter: ((_x) => _x.counter)(stAfter),
    indices: ((_x) => _x.indices)(stAfter),
    lowLinks: LibMaps.insert(v)(LibEquality.min(lowV2)(low_w))(((_x) => _x.lowLinks)(stAfter)),
    stack: ((_x) => _x.stack)(stAfter),
    onStack: ((_x) => _x.onStack)(stAfter),
    sccs: ((_x) => _x.sccs)(stAfter)
  });
})();
})();
})())(LibLogic.ifElse(LibSets.member(w)(((_x) => _x.onStack)(st_)))(lowLink(st_))(st_));
})()));
  return (() => {
  const stAfterNeighbors = LibLists.foldl(processNeighbor)(newSt)(neighbors);
  return (() => {
  const low_v = LibMaps.findWithDefault(Constants.maxInt32)(v)(((_x) => _x.lowLinks)(stAfterNeighbors));
  return (() => {
  const idx_v = LibMaps.findWithDefault(Constants.maxInt32)(v)(((_x) => _x.indices)(stAfterNeighbors));
  return LibLogic.ifElse(LibEquality.equal(low_v)(idx_v))((() => {
  const compResult = popStackUntil(v)(stAfterNeighbors);
  return (() => {
  const comp = LibPairs.first(compResult);
  return (() => {
  const stPopped = LibPairs.second(compResult);
  return ({
    counter: ((_x) => _x.counter)(stPopped),
    indices: ((_x) => _x.indices)(stPopped),
    lowLinks: ((_x) => _x.lowLinks)(stPopped),
    stack: ((_x) => _x.stack)(stPopped),
    onStack: ((_x) => _x.onStack)(stPopped),
    sccs: LibLists.cons(comp)(((_x) => _x.sccs)(stPopped))
  });
})();
})();
})())(stAfterNeighbors);
})();
})();
})();
})();
})();
})();
})()));
}

export function stronglyConnectedComponents(graph: ReadonlyMap<number, ReadonlyArray<number>>): ReadonlyArray<ReadonlyArray<number>> {
  return (() => {
  const verts = LibMaps.keys(graph);
  return (() => {
  const finalState = LibLists.foldl(((st: Topology.TarjanState) => ((v: number) => LibLogic.ifElse(LibMaps.member(v)(((_x) => _x.indices)(st)))(st)(strongConnect(graph)(v)(st)))))(initialState)(verts);
  return LibLists.reverse(LibLists.map(LibLists.sort)(((_x) => _x.sccs)(finalState)));
})();
})();
}

export function topologicalSort<t0>(pairs: ReadonlyArray<readonly [t0, ReadonlyArray<t0>]>): ReadonlyArray<ReadonlyArray<t0>> | ReadonlyArray<t0> {
  return (() => {
  const sccs = topologicalSortComponents(pairs);
  return (() => {
  const isCycle = ((scc: ReadonlyArray<t1>) => LibLogic.not(LibLists.null_(LibLists.tail(scc))));
  return (() => {
  const withCycles = LibLists.filter(isCycle)(sccs);
  return LibLogic.ifElse(LibLists.null_(withCycles))(({ tag: "right", value: LibLists.concat(sccs) }))(({ tag: "left", value: withCycles }));
})();
})();
})();
}

export function topologicalSortComponents<t0>(pairs: ReadonlyArray<readonly [t0, ReadonlyArray<t0>]>): ReadonlyArray<ReadonlyArray<t0>> {
  return (() => {
  const graphResult = adjacencyListsToGraph(pairs);
  return (() => {
  const g = LibPairs.first(graphResult);
  return LibLists.map(((comp: ReadonlyArray<number>) => LibLists.map(LibPairs.second(graphResult))(comp)))(stronglyConnectedComponents(g));
})();
})();
}

export function topologicalSortNodes<t0, t1>(getKey: ((x: t0) => t1)): ((x: ((x: t0) => ReadonlyArray<t1>)) => ((x: ReadonlyArray<t0>) => ReadonlyArray<ReadonlyArray<t0>>)) {
  return ((getAdj: ((x: t0) => ReadonlyArray<t1>)) => ((nodes: ReadonlyArray<t0>) => (() => {
  const nodesByKey = LibMaps.fromList(LibLists.map(((n: t0) => [getKey(n), n]))(nodes));
  return (() => {
  const pairs = LibLists.map(((n: t0) => [getKey(n), getAdj(n)]))(nodes);
  return (() => {
  const comps = topologicalSortComponents(pairs);
  return LibLists.map(((c: ReadonlyArray<t1>) => LibMaybes.cat(LibLists.map(((k: t1) => LibMaps.lookup(k)(nodesByKey)))(c))))(comps);
})();
})();
})()));
}
