// Note: this is an automatically generated file. Do not edit.

/**
 * Haskell test code generation for HSpec-based generation tests
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Constants from "../constants.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as DecodeCore from "../decode/core.js";
import * as Dependencies from "../dependencies.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as HaskellSyntax from "./syntax.js";
import * as HaskellUtils from "./utils.js";
import * as JsonModel from "../json/model.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMath from "../lib/math.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Names from "../names.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Predicates from "../predicates.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Rewriting from "../rewriting.js";
import * as ShowErrors from "../show/errors.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function addNamespacesToNamespaces(ns0: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: ReadonlySet<Core.Name>) => Packaging.Namespaces<HaskellSyntax.ModuleName>) {
  return ((names: ReadonlySet<Core.Name>) => (() => {
  const newNamespaces = LibSets.fromList(LibMaybes.cat(LibLists.map(Names.namespaceOf)(LibSets.toList(names))));
  const toModuleName = ((namespace: Packaging.Namespace) => Formatting.capitalize(LibLists.last(LibStrings.splitOn(".")(((_x) => _x)(namespace)))));
  const newMappings = LibMaps.fromList(LibLists.map(((ns_: Packaging.Namespace) => [ns_, toModuleName(ns_)]))(LibSets.toList(newNamespaces)));
  return ({
    focus: ((_x) => _x.focus)(ns0),
    mapping: LibMaps.union(((_x) => _x.mapping)(ns0))(newMappings)
  });
})());
}

export function buildNamespacesForTestGroup(mod: Packaging.Module): ((x: Testing.TestGroup) => ((x: Graph.Graph) => string | Packaging.Namespaces<HaskellSyntax.ModuleName>)) {
  return ((tgroup: Testing.TestGroup) => ((graph_: Graph.Graph) => (() => {
  const testCases_ = collectTestCases(tgroup);
  const testTerms = LibLists.concat(LibLists.map(extractTestTerms)(testCases_));
  const testBindings = LibLists.map(((term: Core.Term) => ({
    name: "_test_",
    term: term,
    type: null
  })))(testTerms);
  const tempModule = ({
    namespace: ((_x) => _x.namespace)(mod),
    definitions: LibLists.map(((b: Core.Binding) => ({ tag: "term", value: ({
    name: ((_x) => _x.name)(b),
    term: ((_x) => _x.term)(b),
    type: ((_x) => _x.type)(b)
  }) })))(testBindings),
    termDependencies: ((_x) => _x.termDependencies)(mod),
    typeDependencies: ((_x) => _x.typeDependencies)(mod),
    description: ((_x) => _x.description)(mod)
  });
  return LibEithers.bind(LibEithers.bimap(((e: Errors.Error) => ShowErrors.error(e)))(((a: Packaging.Namespaces<HaskellSyntax.ModuleName>) => a))(HaskellUtils.namespacesForModule(tempModule)(Lexical.emptyContext)(graph_)))(((baseNamespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>) => (() => {
  const encodedNames = LibSets.unions(LibLists.map(((t: Core.Term) => extractEncodedTermVariableNames(graph_)(t)))(testTerms));
  return ({ tag: "right", value: addNamespacesToNamespaces(baseNamespaces)(encodedNames) });
})()));
})()));
}

export function buildTestModule(testModule: Packaging.Module): ((x: Testing.TestGroup) => ((x: string) => ((x: Packaging.Namespaces<HaskellSyntax.ModuleName>) => string))) {
  return ((testGroup: Testing.TestGroup) => ((testBody: string) => ((namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>) => (() => {
  const ns_ = ((_x) => _x.namespace)(testModule);
  const specNs = LibStrings.cat2(((_x) => _x)(ns_))("Spec");
  const moduleNameString = namespaceToModuleName(specNs);
  const groupName_ = ((_x) => _x.name)(testGroup);
  const domainImports = findHaskellImports(namespaces)(LibSets.empty);
  const standardImports = ["import Hydra.Kernel", "import qualified Test.Hspec as H", "import qualified Data.List as L", "import qualified Data.Map as M", "import qualified Data.Set as S", "import qualified Data.Maybe as Y"];
  const allImports = LibLists.concat2(standardImports)(domainImports);
  const header = LibStrings.intercalate("\n")(LibLists.concat([[LibStrings.cat2("-- ")(Constants.warningAutoGeneratedFile), ""], ["", LibStrings.cat(["module ", moduleNameString, " where"]), ""], allImports, ["", "spec :: H.Spec", LibStrings.cat(["spec = H.describe ", LibLiterals.showString(groupName_), " $ do"])]]));
  return LibStrings.cat([header, "\n", testBody, "\n"]);
})())));
}

export function collectNames(graf: Graph.Graph): ((x: ReadonlySet<Core.Name>) => ((x: Core.Term) => ReadonlySet<Core.Name>)) {
  return ((names: ReadonlySet<Core.Name>) => ((t: Core.Term) => LibLogic.ifElse(Predicates.isEncodedTerm(Strip.deannotateTerm(t)))(LibEithers.either(((_: Errors.DecodingError) => names))(((decodedTerm: Core.Term) => LibSets.union(names)(Dependencies.termDependencyNames(true)(true)(true)(decodedTerm))))(LibEithers.bimap(((_e: Errors.DecodingError) => _e))(((_a: Core.Term) => _a))(DecodeCore.term(graf)(t))))(names)));
}

export function collectTestCases(tg: Testing.TestGroup): ReadonlyArray<Testing.TestCaseWithMetadata> {
  return LibLists.concat2(((_x) => _x.cases)(tg))(LibLists.concat(LibLists.map(collectTestCases)(((_x) => _x.subgroups)(tg))));
}

export function extractEncodedTermVariableNames(graf: Graph.Graph): ((x: Core.Term) => ReadonlySet<Core.Name>) {
  return ((term: Core.Term) => Rewriting.foldOverTerm(({ tag: "pre" }))(((v1: ReadonlySet<Core.Name>) => ((v2: Core.Term) => collectNames(graf)(v1)(v2))))(LibSets.empty)(term));
}

export function extractTestTerms<t0, t1>(tcm: t0): ReadonlyArray<t1> {
  return [];
}

export function findHaskellImports<t0>(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: t0) => ReadonlyArray<string>) {
  return ((names_: t0) => (() => {
  const mapping_ = ((_x) => _x.mapping)(namespaces);
  const filtered = LibMaps.filterWithKey(((ns_: Packaging.Namespace) => ((_v: HaskellSyntax.ModuleName) => LibLogic.not(LibEquality.equal(LibLists.head(LibStrings.splitOn("hydra.test.")(((_x) => _x)(ns_))))("")))))(mapping_);
  return LibLists.map(((entry: readonly [Packaging.Namespace, HaskellSyntax.ModuleName]) => LibStrings.cat(["import qualified ", LibStrings.intercalate(".")(LibLists.map(Formatting.capitalize)(LibStrings.splitOn(".")(((_x) => _x)(LibPairs.first(entry))))), " as ", ((_x) => _x)(LibPairs.second(entry))])))(LibMaps.toList(filtered));
})());
}

export function generateHaskellTestFile(testModule: Packaging.Module): ((x: Testing.TestGroup) => ((x: Graph.Graph) => string | readonly [string, string])) {
  return ((testGroup: Testing.TestGroup) => ((g: Graph.Graph) => LibEithers.bind(buildNamespacesForTestGroup(testModule)(testGroup)(g))(((namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>) => generateTestFile(testModule)(testGroup)(namespaces)))));
}

export function generateTestCase<t0, t1>(depth: t0): ((x: Testing.TestCaseWithMetadata) => t1 | ReadonlyArray<string>) {
  return ((tcm: Testing.TestCaseWithMetadata) => (() => {
  const name_ = ((_x) => _x.name)(tcm);
  const tcase = ((_x) => _x.case)(tcm);
  const universal = (() => {
  const _m = tcase;
  switch (_m.tag) {
    case "universal": return ((u: Testing.UniversalTestCase) => u)((_m as any).value);
  }
})();
  const actual_ = ((_x) => _x.actual)(universal);
  const expected_ = ((_x) => _x.expected)(universal);
  return ({ tag: "right", value: [LibStrings.cat(["H.it ", LibLiterals.showString(name_), " $ H.shouldBe"]), LibStrings.cat(["  (", actual_, ")"]), LibStrings.cat(["  (", expected_, ")"])] });
})());
}

export function generateTestFile<t0>(testModule: Packaging.Module): ((x: Testing.TestGroup) => ((x: Packaging.Namespaces<HaskellSyntax.ModuleName>) => t0 | readonly [string, string])) {
  return ((testGroup: Testing.TestGroup) => ((namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>) => LibEithers.map(((testBody: string) => (() => {
  const testModuleContent = buildTestModule(testModule)(testGroup)(testBody)(namespaces);
  const ns_ = ((_x) => _x.namespace)(testModule);
  const specNs = LibStrings.cat2(((_x) => _x)(ns_))("Spec");
  const filePath = Names.namespaceToFilePath(({ tag: "pascal" }))("hs")(specNs);
  return [filePath, testModuleContent];
})()))(generateTestGroupHierarchy(1)(testGroup))));
}

export function generateTestGroupHierarchy<t0>(depth: number): ((x: Testing.TestGroup) => t0 | string) {
  return ((testGroup: Testing.TestGroup) => (() => {
  const cases_ = ((_x) => _x.cases)(testGroup);
  const subgroups = ((_x) => _x.subgroups)(testGroup);
  const indent = LibStrings.fromList(LibLists.replicate(LibMath.mul(depth)(2))(32));
  return LibEithers.bind(LibEithers.mapList(((tc: Testing.TestCaseWithMetadata) => generateTestCase(depth)(tc)))(cases_))(((testCaseLinesRaw: ReadonlyArray<ReadonlyArray<string>>) => (() => {
  const testCaseLines = LibLists.map(((lines_: ReadonlyArray<string>) => LibLists.map(((line: string) => LibStrings.cat2(indent)(line)))(lines_)))(testCaseLinesRaw);
  const testCasesStr = LibStrings.intercalate("\n")(LibLists.concat(testCaseLines));
  return LibEithers.map(((subgroupsStr: string) => LibStrings.cat([testCasesStr, LibLogic.ifElse(LibLogic.or(LibEquality.equal(testCasesStr)(""))(LibEquality.equal(subgroupsStr)("")))("")("\n"), subgroupsStr])))(LibEithers.map(((blocks: ReadonlyArray<string>) => LibStrings.intercalate("\n")(blocks)))(LibEithers.mapList(((subgroup: Testing.TestGroup) => (() => {
  const groupName_ = ((_x) => _x.name)(subgroup);
  return LibEithers.map(((content: string) => LibStrings.cat([indent, "H.describe ", LibLiterals.showString(groupName_), " $ do\n", content])))(generateTestGroupHierarchy(LibMath.add(depth)(1))(subgroup));
})()))(subgroups)));
})()));
})());
}

export function namespaceToModuleName(ns_: Packaging.Namespace): string {
  return LibStrings.intercalate(".")(LibLists.map(Formatting.capitalize)(LibStrings.splitOn(".")(((_x) => _x)(ns_))));
}
