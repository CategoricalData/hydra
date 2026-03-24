package hydra.ext.python.testing

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.ext.python.environment.*

import hydra.module.*

import hydra.testing.*

import hydra.lib.chars

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def emptyPythonModuleMetadata(`ns_`: hydra.module.Namespace): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(hydra.module.Namespaces(Tuple2(`ns_`, hydra.ext.python.names.encodeNamespace(`ns_`)), hydra.lib.maps.empty[hydra.module.Namespace, hydra.ext.python.syntax.DottedName]), hydra.lib.sets.empty[hydra.core.Name], false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)

def termToPythonWithContext[T0](`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(graph0: hydra.graph.Graph)(skipCasts: Boolean)(term: hydra.core.Term)(_g: T0): Either[scala.Predef.String, scala.Predef.String] =
  hydra.lib.eithers.bimap[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, scala.Predef.String, scala.Predef.String]((ic: hydra.context.InContext[hydra.errors.Error]) => hydra.show.errors.error(ic.`object`))((`arg_`: hydra.ext.python.syntax.Expression) =>
  hydra.serialization.printExpr(hydra.ext.python.serde.encodeExpression(`arg_`)))(hydra.ext.python.coder.encodeTermInline(hydra.lexical.emptyContext)(hydra.ext.python.environment.PythonEnvironment(`namespaces_`, Tuple2(Seq(), hydra.lib.maps.empty[hydra.core.Name, hydra.ext.python.syntax.Name]), graph0, hydra.lib.sets.empty[hydra.core.Name], hydra.ext.python.utils.targetPythonVersion, skipCasts, hydra.lib.sets.empty[hydra.core.Name]))(skipCasts)(term))

def termToPython(`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(term: hydra.core.Term)(g: hydra.graph.Graph): Either[scala.Predef.String, scala.Predef.String] =
  hydra.ext.python.testing.termToPythonWithContext(`namespaces_`)(g)(false)(term)(g)

def typeToPython(`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(typ: hydra.core.Type)(g: hydra.graph.Graph): Either[scala.Predef.String, scala.Predef.String] =
  hydra.lib.eithers.bimap[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, scala.Predef.String, scala.Predef.String]((ic: hydra.context.InContext[hydra.errors.Error]) => hydra.show.errors.error(ic.`object`))((`arg_`: hydra.ext.python.syntax.Expression) =>
  hydra.serialization.printExpr(hydra.ext.python.serde.encodeExpression(`arg_`)))(hydra.ext.python.coder.encodeType(hydra.ext.python.environment.PythonEnvironment(`namespaces_`, Tuple2(Seq(), hydra.lib.maps.empty[hydra.core.Name, hydra.ext.python.syntax.Name]), g, hydra.lib.sets.empty[hydra.core.Name], hydra.ext.python.utils.targetPythonVersion, false, hydra.lib.sets.empty[hydra.core.Name]))(typ))

def pythonTestCodec(`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]): hydra.testing.TestCodec =
  hydra.testing.TestCodec("python", "py", (v1: hydra.core.Term) =>
  (v2: hydra.graph.Graph) => hydra.ext.python.testing.termToPython(`namespaces_`)(v1)(v2), (v1: hydra.core.Type) =>
  (v2: hydra.graph.Graph) => hydra.ext.python.testing.typeToPython(`namespaces_`)(v1)(v2), hydra.ext.python.testing.formatPythonTestName, hydra.ext.python.testing.namespaceToPythonModuleName, hydra.ext.python.testing.pythonTestCaseTemplate, hydra.ext.python.testing.pythonTestGroupTemplate, hydra.ext.python.testing.pythonModuleTemplate, hydra.ext.python.testing.pythonImportTemplate, (v1: scala.collection.immutable.Set[hydra.core.Name]) =>
  hydra.ext.python.testing.findPythonImports(`namespaces_`)(v1))

def pythonTestCodecWithContext(`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(tcontext: hydra.graph.Graph): hydra.testing.TestCodec =
  hydra.testing.TestCodec("python", "py", (v1: hydra.core.Term) =>
  (v2: hydra.graph.Graph) =>
  hydra.ext.python.testing.termToPythonWithContext(`namespaces_`)(tcontext)(true)(v1)(v2), (v1: hydra.core.Type) =>
  (v2: hydra.graph.Graph) => hydra.ext.python.testing.typeToPython(`namespaces_`)(v1)(v2), hydra.ext.python.testing.formatPythonTestName, hydra.ext.python.testing.namespaceToPythonModuleName, hydra.ext.python.testing.pythonTestCaseTemplate, hydra.ext.python.testing.pythonTestGroupTemplate, hydra.ext.python.testing.pythonModuleTemplate, hydra.ext.python.testing.pythonImportTemplate, (v1: scala.collection.immutable.Set[hydra.core.Name]) =>
  hydra.ext.python.testing.findPythonImports(`namespaces_`)(v1))

def formatPythonTestName(name: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.cat2("test_")(hydra.lib.strings.fromList(hydra.lib.lists.map[Int, Int]((c: Int) =>
  hydra.lib.logic.ifElse[Int](hydra.lib.chars.isAlphaNum(c))(hydra.lib.chars.toLower(c))(95))(hydra.lib.strings.toList(name))))

def namespaceToPythonModuleName(`ns_`: hydra.module.Namespace): scala.Predef.String = `ns_`

lazy val pythonTestCaseTemplate: scala.Predef.String = hydra.lib.strings.intercalate("\n")(Seq("def {name}():", "    assert ({input}) == ({output})"))

lazy val pythonTestGroupTemplate: scala.Predef.String = "# {groupName}"

lazy val pythonModuleTemplate: scala.Predef.String = hydra.lib.strings.intercalate("\n")(Seq(hydra.lib.strings.cat2("# ")(hydra.constants.warningAutoGeneratedFile), "", "{imports}", "", "{testGroup}", "", "{testCases}"))

lazy val pythonImportTemplate: scala.Predef.String = "import {namespace}"

def findPythonImports[T0, T1](`namespaces_`: hydra.module.Namespaces[T0])(`names_`: T1): Seq[scala.Predef.String] =
  {
  lazy val `mapping_`: Map[hydra.module.Namespace, T0] = (`namespaces_`.mapping)
  lazy val filtered: Map[hydra.module.Namespace, T0] = hydra.lib.maps.filterWithKey[hydra.module.Namespace, T0]((`ns_`: hydra.module.Namespace) =>
    (_v: T0) =>
    hydra.lib.logic.not(hydra.lib.equality.equal[scala.Predef.String](hydra.lib.lists.head[scala.Predef.String](hydra.lib.strings.splitOn("hydra.test.")(`ns_`)))("")))(`mapping_`)
  hydra.lib.lists.map[Tuple2[hydra.module.Namespace, T0], scala.Predef.String]((entry: Tuple2[hydra.module.Namespace, T0]) =>
    hydra.lib.strings.cat2("import ")(hydra.lib.pairs.first[hydra.module.Namespace, T0](entry)))(hydra.lib.maps.toList[hydra.module.Namespace, T0](filtered))
}

def namespacesForPythonModule[T0](mod: hydra.module.Module)(`graph_`: hydra.graph.Graph): Either[T0, hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]] =
  {
  lazy val bindings: Seq[hydra.core.Binding] = hydra.lexical.graphToBindings(`graph_`)
  lazy val defs: Seq[hydra.module.Definition] = hydra.lib.maybes.mapMaybe[hydra.core.Binding, hydra.module.Definition]((b: hydra.core.Binding) =>
    hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.module.Definition]((ts: hydra.core.TypeScheme) =>
    hydra.module.Definition.term(hydra.module.TermDefinition(b.name, (b.term), Some(ts))))(b.`type`))(bindings)
  Right(hydra.ext.python.utils.findNamespaces(mod.namespace)(defs))
}

def generatePythonTestGroupHierarchy[T0](g: hydra.graph.Graph)(`namespaces_`: T0)(codec: hydra.testing.TestCodec)(groupPath: Seq[scala.Predef.String])(testGroup: hydra.testing.TestGroup): Either[scala.Predef.String, scala.Predef.String] =
  {
  lazy val `cases_`: Seq[hydra.testing.TestCaseWithMetadata] = (testGroup.cases)
  lazy val subgroups: Seq[hydra.testing.TestGroup] = (testGroup.subgroups)
  hydra.lib.eithers.bind[scala.Predef.String, Seq[Seq[scala.Predef.String]], scala.Predef.String](hydra.lib.eithers.mapList[hydra.testing.TestCaseWithMetadata, Seq[scala.Predef.String], scala.Predef.String]((tc: hydra.testing.TestCaseWithMetadata) =>
    hydra.ext.python.testing.generatePythonTestCase(g)(`namespaces_`)(codec)(groupPath)(tc))(`cases_`))((testCaseLines: Seq[Seq[scala.Predef.String]]) =>
    hydra.lib.eithers.bind[scala.Predef.String, Seq[scala.Predef.String], scala.Predef.String](hydra.lib.eithers.mapList[hydra.testing.TestGroup, scala.Predef.String, scala.Predef.String]((subgroup: hydra.testing.TestGroup) =>
    {
    lazy val groupName: scala.Predef.String = (subgroup.name)
    lazy val header: scala.Predef.String = hydra.lib.strings.cat2("# ")(groupName)
    hydra.lib.eithers.map[scala.Predef.String, scala.Predef.String, scala.Predef.String]((content: scala.Predef.String) => hydra.lib.strings.cat(Seq(header, "\n\n", content)))(hydra.ext.python.testing.generatePythonTestGroupHierarchy(g)(`namespaces_`)(codec)(hydra.lib.lists.concat2[scala.Predef.String](groupPath)(Seq(groupName)))(subgroup))
  })(subgroups))((subgroupBlocks: Seq[scala.Predef.String]) =>
    {
    lazy val testCasesStr: scala.Predef.String = hydra.lib.strings.intercalate("\n\n")(hydra.lib.lists.concat[scala.Predef.String](testCaseLines))
    lazy val subgroupsStr: scala.Predef.String = hydra.lib.strings.intercalate("\n\n")(subgroupBlocks)
    Right(hydra.lib.strings.cat(Seq(testCasesStr, hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.logic.or(hydra.lib.equality.equal[scala.Predef.String](testCasesStr)(""))(hydra.lib.equality.equal[scala.Predef.String](subgroupsStr)("")))("")("\n\n"), subgroupsStr)))
  }))
}

def generatePythonTestCase[T0](g: hydra.graph.Graph)(`namespaces_`: T0)(codec: hydra.testing.TestCodec)(groupPath: Seq[scala.Predef.String])(tcm: hydra.testing.TestCaseWithMetadata): Either[scala.Predef.String, Seq[scala.Predef.String]] =
  {
  lazy val `name_`: scala.Predef.String = (tcm.name)
  lazy val tcase: hydra.testing.TestCase = (tcm.`case`)
  tcase match
    case hydra.testing.TestCase.delegatedEvaluation(v_TestCase_delegatedEvaluation_delCase) => {
      lazy val `input_`: hydra.core.Term = (v_TestCase_delegatedEvaluation_delCase.input)
      lazy val `output_`: hydra.core.Term = (v_TestCase_delegatedEvaluation_delCase.output)
      lazy val fullName: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.lists.`null`[scala.Predef.String](groupPath))(`name_`)(hydra.lib.strings.intercalate("__")(hydra.lib.lists.concat2[scala.Predef.String](groupPath)(Seq(`name_`))))
      lazy val formattedName: scala.Predef.String = codec.formatTestName(fullName)
      hydra.lib.eithers.bind[scala.Predef.String, scala.Predef.String, Seq[scala.Predef.String]](codec.encodeTerm(`input_`)(g))((inputCode: scala.Predef.String) =>
        hydra.lib.eithers.bind[scala.Predef.String, scala.Predef.String, Seq[scala.Predef.String]](codec.encodeTerm(`output_`)(g))((outputCode: scala.Predef.String) =>
        Right(Seq(hydra.lib.strings.cat(Seq("def ", formattedName, "():")), hydra.lib.strings.cat(Seq("    assert (", inputCode, ") == (", outputCode, ")"))))))
    }
    case _ => Right(Seq())
}

def generateTestFileWithPythonCodec[T0](codec: hydra.testing.TestCodec)(testModule: hydra.module.Module)(testGroup: hydra.testing.TestGroup)(`namespaces_`: T0)(g: hydra.graph.Graph): Either[scala.Predef.String, Tuple2[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.map[scala.Predef.String, Tuple2[scala.Predef.String, scala.Predef.String], scala.Predef.String]((testBody: scala.Predef.String) =>
  {
  lazy val testModuleContent: scala.Predef.String = hydra.ext.python.testing.buildPythonTestModule(codec)(testModule)(testGroup)(testBody)(`namespaces_`)
  lazy val `ns_`: hydra.module.Namespace = (testModule.namespace)
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(`ns_`)
  lazy val dirParts: Seq[scala.Predef.String] = hydra.lib.lists.init[scala.Predef.String](parts)
  lazy val fileName: scala.Predef.String = hydra.lib.strings.cat(Seq("test_", hydra.lib.lists.last[scala.Predef.String](parts), ".py"))
  lazy val filePath: scala.Predef.String = hydra.lib.strings.cat(Seq(hydra.lib.strings.intercalate("/")(dirParts), "/", fileName))
  Tuple2(filePath, testModuleContent)
})(hydra.ext.python.testing.generatePythonTestGroupHierarchy(g)(`namespaces_`)(codec)(Seq())(testGroup))

def buildPythonTestModule[T0, T1](codec: hydra.testing.TestCodec)(testModule: T0)(testGroup: hydra.testing.TestGroup)(testBody: scala.Predef.String)(`namespaces_`: T1): scala.Predef.String =
  {
  lazy val `groupName_`: scala.Predef.String = (testGroup.name)
  lazy val domainImports: Seq[scala.Predef.String] = codec.findImports(hydra.lib.sets.empty[hydra.core.Name])
  lazy val standardImports: Seq[scala.Predef.String] = Seq("from __future__ import annotations", "from typing import cast", "from decimal import Decimal", "from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing")
  lazy val allImports: Seq[scala.Predef.String] = hydra.lib.lists.concat2[scala.Predef.String](standardImports)(domainImports)
  lazy val header: scala.Predef.String = hydra.lib.strings.cat(Seq(hydra.lib.strings.cat2("# ")(hydra.constants.warningAutoGeneratedFile), "\n", hydra.lib.strings.cat2("# ")(`groupName_`), "\n\n", hydra.lib.strings.intercalate("\n")(allImports), "\n\n"))
  hydra.lib.strings.cat(Seq(header, testBody, "\n"))
}

def generatePythonTestFile(testModule: hydra.module.Module)(testGroup: hydra.testing.TestGroup)(g: hydra.graph.Graph): Either[scala.Predef.String, Tuple2[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.bind[scala.Predef.String, hydra.testing.TestGroup, Tuple2[scala.Predef.String, scala.Predef.String]](hydra.test.utils.inferTestGroupTerms(g)(testGroup))((inferredTestGroup: hydra.testing.TestGroup) =>
  hydra.lib.eithers.bind[scala.Predef.String, hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], Tuple2[scala.Predef.String, scala.Predef.String]](hydra.ext.python.testing.namespacesForPythonModule(testModule)(g))((`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]) =>
  hydra.ext.python.testing.generateTestFileWithPythonCodec(hydra.ext.python.testing.pythonTestCodecWithContext(`namespaces_`)(g))(testModule)(inferredTestGroup)(`namespaces_`)(g)))
