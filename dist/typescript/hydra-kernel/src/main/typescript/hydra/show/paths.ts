// Note: this is an automatically generated file. Do not edit.

/**
 * Utilities for working with subterm steps and paths.
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
import * as LibLists from "../lib/lists.js";
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
import * as Relational from "../relational.js";
import * as Rewriting from "../rewriting.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function subtermStep(step: Paths.SubtermStep): string | null {
  return (() => {
  const idx = ((i: t0) => null);
  return (() => {
  const idxSuff = ((suffix: string) => ((i: t0) => LibMaybes.map(((s: string) => LibStrings.cat2(s)(suffix)))(idx(i))));
  return (() => {
  const _m = step;
  switch (_m.tag) {
    case "annotatedBody": return ((_: void) => null)((_m as any).value);
    case "applicationFunction": return ((_: void) => "fun")((_m as any).value);
    case "applicationArgument": return ((_: void) => "arg")((_m as any).value);
    case "lambdaBody": return ((_: void) => "body")((_m as any).value);
    case "unionCasesDefault": return ((_: void) => "default")((_m as any).value);
    case "unionCasesBranch": return ((name: Core.Name) => LibStrings.cat2(".")(((_x) => _x)(name)))((_m as any).value);
    case "letBody": return ((_: void) => "in")((_m as any).value);
    case "letBinding": return ((name: Core.Name) => LibStrings.cat2(((_x) => _x)(name))("="))((_m as any).value);
    case "listElement": return ((i: number) => idx(i))((_m as any).value);
    case "mapKey": return ((i: number) => idxSuff(".key")(i))((_m as any).value);
    case "mapValue": return ((i: number) => idxSuff(".value")(i))((_m as any).value);
    case "maybeTerm": return ((_: void) => "just")((_m as any).value);
    case "productTerm": return ((i: number) => idx(i))((_m as any).value);
    case "recordField": return ((name: Core.Name) => LibStrings.cat2(".")(((_x) => _x)(name)))((_m as any).value);
    case "setElement": return ((i: number) => idx(i))((_m as any).value);
    case "sumTerm": return ((_: void) => null)((_m as any).value);
    case "typeLambdaBody": return ((_: void) => null)((_m as any).value);
    case "typeApplicationTerm": return ((_: void) => null)((_m as any).value);
    case "injectionTerm": return ((_: void) => null)((_m as any).value);
    case "wrappedTerm": return ((_: void) => null)((_m as any).value);
  }
})();
})();
})();
}

export function termToSubtermGraph(namespaces: ReadonlyMap<Packaging.Namespace, string>): ((x: Core.Term) => Paths.SubtermGraph) {
  return ((term: Core.Term) => (() => {
  const dontCareStep = ({ tag: "annotatedBody" });
  const helper = ((ids: ReadonlyMap<Core.Name, Paths.SubtermNode>) => ((mroot: Paths.SubtermNode | null) => ((path: ReadonlyArray<Paths.SubtermStep>) => ((state: readonly [readonly [ReadonlyArray<Paths.SubtermNode>, ReadonlyArray<Paths.SubtermEdge>], ReadonlySet<string>]) => ((stepTerm: readonly [Paths.SubtermStep, Core.Term]) => (() => {
  const step = LibPairs.first(stepTerm);
  const currentTerm = LibPairs.second(stepTerm);
  const nodesEdges = LibPairs.first(state);
  const visited = LibPairs.second(state);
  const nodes = LibPairs.first(nodesEdges);
  const edges = LibPairs.second(nodesEdges);
  const nextPath = LibLists.cons(step)(path);
  return (() => {
  const _m = currentTerm;
  switch (_m.tag) {
    case "let": return ((letExpr: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(letExpr);
  const env = ((_x) => _x.body)(letExpr);
  const bindingNames = LibLists.map(((_x) => _x.name))(bindings);
  const addBindingName = ((nodesVisitedIds: readonly [readonly [ReadonlyArray<Paths.SubtermNode>, ReadonlySet<string>], ReadonlyMap<Core.Name, Paths.SubtermNode>]) => ((name: Core.Name) => (() => {
  const currentNodesVisited = LibPairs.first(nodesVisitedIds);
  const currentIds = LibPairs.second(nodesVisitedIds);
  const currentNodes = LibPairs.first(currentNodesVisited);
  const currentVisited = LibPairs.second(currentNodesVisited);
  const rawLabel = Names.compactName(namespaces)(name);
  const uniqueLabel = Names.uniqueLabel(currentVisited)(rawLabel);
  const node = ({
    name: name,
    label: rawLabel,
    id: uniqueLabel
  });
  const newVisited = LibSets.insert(uniqueLabel)(currentVisited);
  const newNodes = LibLists.cons(node)(currentNodes);
  const newIds = LibMaps.insert(name)(node)(currentIds);
  return [[newNodes, newVisited], newIds];
})()));
  const nodesVisitedIds1 = LibLists.foldl(addBindingName)([[[], visited], ids])(bindingNames);
  const nodes1 = LibPairs.first(LibPairs.first(nodesVisitedIds1));
  const visited1 = LibPairs.second(LibPairs.first(nodesVisitedIds1));
  const ids1 = LibPairs.second(nodesVisitedIds1);
  const addBindingTerm = ((currentState: readonly [readonly [ReadonlyArray<Paths.SubtermNode>, ReadonlyArray<Paths.SubtermEdge>], ReadonlySet<string>]) => ((nodeBinding: readonly [Paths.SubtermNode, Core.Binding]) => (() => {
  const root = LibPairs.first(nodeBinding);
  const binding = LibPairs.second(nodeBinding);
  const term1 = ((_x) => _x.term)(binding);
  return helper(ids1)(root)([])(currentState)([dontCareStep, term1]);
})()));
  const nodeBindingPairs = LibLists.zip(nodes1)(bindings);
  const stateAfterBindings = LibLists.foldl(addBindingTerm)([[LibLists.concat2(nodes1)(nodes), edges], visited1])(nodeBindingPairs);
  return helper(ids1)(mroot)(nextPath)(stateAfterBindings)([({ tag: "letBody" }), env]);
})())((_m as any).value);
    case "variable": return ((name: Core.Name) => LibMaybes.maybe(state)(((root: Paths.SubtermNode) => LibMaybes.maybe(state)(((node: Paths.SubtermNode) => (() => {
  const edge = ({
    source: root,
    path: LibLists.reverse(nextPath),
    target: node
  });
  const newEdges = LibLists.cons(edge)(edges);
  return [[nodes, newEdges], visited];
})()))(LibMaps.lookup(name)(ids))))(mroot))((_m as any).value);
    default: return LibLists.foldl(((v1: readonly [readonly [ReadonlyArray<Paths.SubtermNode>, ReadonlyArray<Paths.SubtermEdge>], ReadonlySet<string>]) => ((v2: readonly [Paths.SubtermStep, Core.Term]) => helper(ids)(mroot)(nextPath)(v1)(v2))))(state)(Rewriting.subtermsWithSteps(currentTerm))(_m);
  }
})();
})())))));
  const initialState = [[[], []], LibSets.empty];
  const result = helper(LibMaps.empty)(null)([])(initialState)([dontCareStep, term]);
  const finalNodesEdges = LibPairs.first(result);
  const finalNodes = LibPairs.first(finalNodesEdges);
  const finalEdges = LibPairs.second(finalNodesEdges);
  return ({
    nodes: finalNodes,
    edges: finalEdges
  });
})());
}
