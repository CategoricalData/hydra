package hydra.ext.haskell.testing

import hydra.coders.*

import hydra.core.*

import hydra.ext.haskell.syntax.*

import hydra.packaging.*

import hydra.testing.*

import hydra.util.*

def addNamespacesToNamespaces(ns0: hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName])(names: scala.collection.immutable.Set[hydra.core.Name]): hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName] =
  {
  lazy val newNamespaces: scala.collection.immutable.Set[hydra.packaging.Namespace] = hydra.lib.sets.fromList[hydra.packaging.Namespace](hydra.lib.maybes.cat[hydra.packaging.Namespace](hydra.lib.lists.map[hydra.core.Name,
     Option[hydra.packaging.Namespace]](hydra.names.namespaceOf)(hydra.lib.sets.toList[hydra.core.Name](names))))
  def toModuleName(namespace: hydra.packaging.Namespace): hydra.ext.haskell.syntax.ModuleName =
    hydra.formatting.capitalize(hydra.lib.lists.last[scala.Predef.String](hydra.lib.strings.splitOn(".")(namespace)))
  lazy val newMappings: Map[hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName] = hydra.lib.maps.fromList[hydra.packaging.Namespace,
     hydra.ext.haskell.syntax.ModuleName](hydra.lib.lists.map[hydra.packaging.Namespace, Tuple2[hydra.packaging.Namespace,
     hydra.ext.haskell.syntax.ModuleName]]((`ns_`: hydra.packaging.Namespace) => Tuple2(`ns_`, toModuleName(`ns_`)))(hydra.lib.sets.toList[hydra.packaging.Namespace](newNamespaces)))
  hydra.packaging.Namespaces(ns0.focus, hydra.lib.maps.union[hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName](ns0.mapping)(newMappings))
}

def buildNamespacesForTestGroup(mod: hydra.packaging.Module)(tgroup: hydra.testing.TestGroup)(`graph_`: hydra.graph.Graph): Either[scala.Predef.String,
   hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName]] =
  {
  lazy val `testCases_`: Seq[hydra.testing.TestCaseWithMetadata] = hydra.ext.haskell.testing.collectTestCases(tgroup)
  def testTerms[T0]: Seq[T0] =
    hydra.lib.lists.concat[T0](hydra.lib.lists.map[hydra.testing.TestCaseWithMetadata, Seq[T0]](hydra.ext.haskell.testing.extractTestTerms)(`testCases_`))
  lazy val testBindings: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Term, hydra.core.Binding]((term: hydra.core.Term) => hydra.core.Binding("_test_",
     term, None))(testTerms)
  lazy val tempModule: hydra.packaging.Module = hydra.packaging.Module(mod.namespace, hydra.lib.lists.map[hydra.core.Binding,
     hydra.packaging.Definition]((b: hydra.core.Binding) =>
    hydra.packaging.Definition.term(hydra.packaging.TermDefinition(b.name, (b.term), (b.`type`))))(testBindings),
       (mod.termDependencies), (mod.typeDependencies), (mod.description))
  hydra.lib.eithers.bind[scala.Predef.String, hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName],
     hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName]](hydra.lib.eithers.bimap[hydra.errors.Error,
     hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName], scala.Predef.String, hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName]]((e: hydra.errors.Error) => hydra.show.errors.error(e))((a: hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName]) => a)(hydra.ext.haskell.utils.namespacesForModule(tempModule)(hydra.lexical.emptyContext)(`graph_`)))((baseNamespaces: hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName]) =>
    {
    lazy val encodedNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.core.Term,
       scala.collection.immutable.Set[hydra.core.Name]]((t: hydra.core.Term) =>
      hydra.ext.haskell.testing.extractEncodedTermVariableNames(`graph_`)(t))(testTerms))
    Right(hydra.ext.haskell.testing.addNamespacesToNamespaces(baseNamespaces)(encodedNames))
  })
}

def buildTestModule(testModule: hydra.packaging.Module)(testGroup: hydra.testing.TestGroup)(testBody: scala.Predef.String)(namespaces: hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName]): scala.Predef.String =
  {
  lazy val `ns_`: hydra.packaging.Namespace = (testModule.namespace)
  lazy val specNs: hydra.packaging.Namespace = hydra.lib.strings.cat2(`ns_`)("Spec")
  lazy val moduleNameString: scala.Predef.String = hydra.ext.haskell.testing.namespaceToModuleName(specNs)
  lazy val `groupName_`: scala.Predef.String = (testGroup.name)
  lazy val domainImports: Seq[scala.Predef.String] = hydra.ext.haskell.testing.findHaskellImports(namespaces)(hydra.lib.sets.empty)
  lazy val standardImports: Seq[scala.Predef.String] = Seq("import Hydra.Kernel", "import qualified Test.Hspec as H",
     "import qualified Data.List as L", "import qualified Data.Map as M", "import qualified Data.Set as S",
     "import qualified Data.Maybe as Y")
  lazy val allImports: Seq[scala.Predef.String] = hydra.lib.lists.concat2[scala.Predef.String](standardImports)(domainImports)
  lazy val header: scala.Predef.String = hydra.lib.strings.intercalate("\n")(hydra.lib.lists.concat[scala.Predef.String](Seq(Seq(hydra.lib.strings.cat2("-- ")(hydra.constants.warningAutoGeneratedFile),
     ""), Seq("", hydra.lib.strings.cat(Seq("module ", moduleNameString, " where")), ""), allImports,
     Seq("", "spec :: H.Spec", hydra.lib.strings.cat(Seq("spec = H.describe ", hydra.lib.literals.showString(`groupName_`),
     " $ do"))))))
  hydra.lib.strings.cat(Seq(header, "\n", testBody, "\n"))
}

def collectNames(graf: hydra.graph.Graph)(names: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] =
  hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](hydra.predicates.isEncodedTerm(hydra.strip.deannotateTerm(t)))(hydra.lib.eithers.either[hydra.errors.DecodingError,
     hydra.core.Term, scala.collection.immutable.Set[hydra.core.Name]]((_x: hydra.errors.DecodingError) => names)((decodedTerm: hydra.core.Term) =>
  hydra.lib.sets.union[hydra.core.Name](names)(hydra.dependencies.termDependencyNames(true)(true)(true)(decodedTerm)))(hydra.lib.eithers.bimap[hydra.errors.DecodingError,
     hydra.core.Term, hydra.errors.DecodingError, hydra.core.Term]((_e: hydra.errors.DecodingError) => _e)((_a: hydra.core.Term) => _a)(hydra.decode.core.term(graf)(t))))(names)

def collectTestCases(tg: hydra.testing.TestGroup): Seq[hydra.testing.TestCaseWithMetadata] =
  hydra.lib.lists.concat2[hydra.testing.TestCaseWithMetadata](tg.cases)(hydra.lib.lists.concat[hydra.testing.TestCaseWithMetadata](hydra.lib.lists.map[hydra.testing.TestGroup,
     Seq[hydra.testing.TestCaseWithMetadata]](hydra.ext.haskell.testing.collectTestCases)(tg.subgroups)))

def extractEncodedTermVariableNames(graf: hydra.graph.Graph)(term: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] =
  hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((v1: scala.collection.immutable.Set[hydra.core.Name]) =>
  (v2: hydra.core.Term) => hydra.ext.haskell.testing.collectNames(graf)(v1)(v2))(hydra.lib.sets.empty[hydra.core.Name])(term)

def extractTestTerms[T0, T1](tcm: T0): Seq[T1] = Seq()

def findHaskellImports[T0](namespaces: hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName])(`names_`: T0): Seq[scala.Predef.String] =
  {
  lazy val `mapping_`: Map[hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName] = (namespaces.mapping)
  lazy val filtered: Map[hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName] = hydra.lib.maps.filterWithKey[hydra.packaging.Namespace,
     hydra.ext.haskell.syntax.ModuleName]((`ns_`: hydra.packaging.Namespace) =>
    (_v: hydra.ext.haskell.syntax.ModuleName) =>
    hydra.lib.logic.not(hydra.lib.equality.equal[scala.Predef.String](hydra.lib.lists.head[scala.Predef.String](hydra.lib.strings.splitOn("hydra.test.")(`ns_`)))("")))(`mapping_`)
  hydra.lib.lists.map[Tuple2[hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName], scala.Predef.String]((entry: Tuple2[hydra.packaging.Namespace,
     hydra.ext.haskell.syntax.ModuleName]) =>
    hydra.lib.strings.cat(Seq("import qualified ", hydra.lib.strings.intercalate(".")(hydra.lib.lists.map[scala.Predef.String,
       scala.Predef.String](hydra.formatting.capitalize)(hydra.lib.strings.splitOn(".")(hydra.lib.pairs.first[hydra.packaging.Namespace,
       hydra.ext.haskell.syntax.ModuleName](entry)))), " as ", hydra.lib.pairs.second[hydra.packaging.Namespace,
       hydra.ext.haskell.syntax.ModuleName](entry))))(hydra.lib.maps.toList[hydra.packaging.Namespace,
       hydra.ext.haskell.syntax.ModuleName](filtered))
}

def generateHaskellTestFile(testModule: hydra.packaging.Module)(testGroup: hydra.testing.TestGroup)(g: hydra.graph.Graph): Either[scala.Predef.String,
   Tuple2[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.bind[scala.Predef.String, hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName],
     Tuple2[scala.Predef.String, scala.Predef.String]](hydra.ext.haskell.testing.buildNamespacesForTestGroup(testModule)(testGroup)(g))((namespaces: hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName]) =>
  hydra.ext.haskell.testing.generateTestFile(testModule)(testGroup)(namespaces))

def generateTestCase[T0, T1](depth: T0)(tcm: hydra.testing.TestCaseWithMetadata): Either[T1, Seq[scala.Predef.String]] =
  {
  lazy val `name_`: scala.Predef.String = (tcm.name)
  lazy val tcase: hydra.testing.TestCase = (tcm.`case`)
  lazy val universal: hydra.testing.UniversalTestCase = tcase match
    case hydra.testing.TestCase.universal(v_TestCase_universal_u) => v_TestCase_universal_u
  lazy val `actual_`: scala.Predef.String = (universal.actual)
  lazy val `expected_`: scala.Predef.String = (universal.expected)
  Right(Seq(hydra.lib.strings.cat(Seq("H.it ", hydra.lib.literals.showString(`name_`), " $ H.shouldBe")),
     hydra.lib.strings.cat(Seq("  (", `actual_`, ")")), hydra.lib.strings.cat(Seq("  (", `expected_`,
     ")"))))
}

def generateTestFile[T0](testModule: hydra.packaging.Module)(testGroup: hydra.testing.TestGroup)(namespaces: hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName]): Either[T0,
   Tuple2[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.map[scala.Predef.String, Tuple2[scala.Predef.String, scala.Predef.String], T0]((testBody: scala.Predef.String) =>
  {
  lazy val testModuleContent: scala.Predef.String = hydra.ext.haskell.testing.buildTestModule(testModule)(testGroup)(testBody)(namespaces)
  lazy val `ns_`: hydra.packaging.Namespace = (testModule.namespace)
  lazy val specNs: hydra.packaging.Namespace = hydra.lib.strings.cat2(`ns_`)("Spec")
  lazy val filePath: scala.Predef.String = hydra.names.namespaceToFilePath(hydra.util.CaseConvention.pascal)("hs")(specNs)
  Tuple2(filePath, testModuleContent)
})(hydra.ext.haskell.testing.generateTestGroupHierarchy(1)(testGroup))

def generateTestGroupHierarchy[T0](depth: Int)(testGroup: hydra.testing.TestGroup): Either[T0, scala.Predef.String] =
  {
  lazy val `cases_`: Seq[hydra.testing.TestCaseWithMetadata] = (testGroup.cases)
  lazy val subgroups: Seq[hydra.testing.TestGroup] = (testGroup.subgroups)
  lazy val indent: scala.Predef.String = hydra.lib.strings.fromList(hydra.lib.lists.replicate[Int](hydra.lib.math.mul(depth)(2))(32))
  hydra.lib.eithers.bind[T0, Seq[Seq[scala.Predef.String]], scala.Predef.String](hydra.lib.eithers.mapList[hydra.testing.TestCaseWithMetadata,
     Seq[scala.Predef.String], T0]((tc: hydra.testing.TestCaseWithMetadata) => hydra.ext.haskell.testing.generateTestCase(depth)(tc))(`cases_`))((testCaseLinesRaw: Seq[Seq[scala.Predef.String]]) =>
    {
    lazy val testCaseLines: Seq[Seq[scala.Predef.String]] = hydra.lib.lists.map[Seq[scala.Predef.String],
       Seq[scala.Predef.String]]((`lines_`: Seq[scala.Predef.String]) =>
      hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((line: scala.Predef.String) => hydra.lib.strings.cat2(indent)(line))(`lines_`))(testCaseLinesRaw)
    lazy val testCasesStr: scala.Predef.String = hydra.lib.strings.intercalate("\n")(hydra.lib.lists.concat[scala.Predef.String](testCaseLines))
    hydra.lib.eithers.map[scala.Predef.String, scala.Predef.String, T0]((subgroupsStr: scala.Predef.String) =>
      hydra.lib.strings.cat(Seq(testCasesStr, hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.logic.or(hydra.lib.equality.equal[scala.Predef.String](testCasesStr)(""))(hydra.lib.equality.equal[scala.Predef.String](subgroupsStr)("")))("")("\n"),
         subgroupsStr)))(hydra.lib.eithers.map[Seq[scala.Predef.String], scala.Predef.String, T0]((blocks: Seq[scala.Predef.String]) => hydra.lib.strings.intercalate("\n")(blocks))(hydra.lib.eithers.mapList[hydra.testing.TestGroup,
         scala.Predef.String, T0]((subgroup: hydra.testing.TestGroup) =>
      {
      lazy val `groupName_`: scala.Predef.String = (subgroup.name)
      hydra.lib.eithers.map[scala.Predef.String, scala.Predef.String, T0]((content: scala.Predef.String) =>
        hydra.lib.strings.cat(Seq(indent, "H.describe ", hydra.lib.literals.showString(`groupName_`),
           " $ do\n", content)))(hydra.ext.haskell.testing.generateTestGroupHierarchy(hydra.lib.math.add(depth)(1))(subgroup))
    })(subgroups)))
  })
}

def namespaceToModuleName(`ns_`: hydra.packaging.Namespace): scala.Predef.String =
  hydra.lib.strings.intercalate(".")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String](hydra.formatting.capitalize)(hydra.lib.strings.splitOn(".")(`ns_`)))
