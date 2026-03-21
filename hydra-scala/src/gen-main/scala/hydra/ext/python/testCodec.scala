package hydra.ext.python.testCodec

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.ext.python.helpers.*

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

def emptyPythonModuleMetadata(`ns_`: hydra.module.Namespace): hydra.ext.python.helpers.PythonModuleMetadata =
  hydra.ext.python.helpers.PythonModuleMetadata(hydra.module.Namespaces(Tuple2(`ns_`, hydra.ext.python.names.encodeNamespace(`ns_`)),
     maps.empty[hydra.module.Namespace, hydra.ext.python.syntax.DottedName]), sets.empty[hydra.core.Name],
     false, false, false, false, false, false, false, false, false, false, false, false, false, false,
     false, false, false, false, false, false)

def termToPythonWithContext[T0](`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(graph0: hydra.graph.Graph)(skipCasts: Boolean)(term: hydra.core.Term)(_g: T0): Either[scala.Predef.String,
   scala.Predef.String] =
  eithers.bimap[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Expression, scala.Predef.String,
     scala.Predef.String]((ic: hydra.context.InContext[hydra.error.Error]) => hydra.show.error.error(ic.`object`))((`arg_`: hydra.ext.python.syntax.Expression) =>
  hydra.serialization.printExpr(hydra.ext.python.serde.encodeExpression(`arg_`)))(hydra.ext.python.coder.encodeTermInline(hydra.lexical.emptyContext)(hydra.ext.python.helpers.PythonEnvironment(`namespaces_`,
     Tuple2(Seq(), maps.empty[hydra.core.Name, hydra.ext.python.syntax.Name]), graph0, sets.empty[hydra.core.Name],
     hydra.ext.python.utils.targetPythonVersion, skipCasts, sets.empty[hydra.core.Name]))(skipCasts)(term))

def termToPython(`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(term: hydra.core.Term)(g: hydra.graph.Graph): Either[scala.Predef.String,
   scala.Predef.String] =
  hydra.ext.python.testCodec.termToPythonWithContext(`namespaces_`)(g)(false)(term)(g)

def typeToPython(`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(typ: hydra.core.Type)(g: hydra.graph.Graph): Either[scala.Predef.String,
   scala.Predef.String] =
  eithers.bimap[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Expression, scala.Predef.String,
     scala.Predef.String]((ic: hydra.context.InContext[hydra.error.Error]) => hydra.show.error.error(ic.`object`))((`arg_`: hydra.ext.python.syntax.Expression) =>
  hydra.serialization.printExpr(hydra.ext.python.serde.encodeExpression(`arg_`)))(hydra.ext.python.coder.encodeType(hydra.ext.python.helpers.PythonEnvironment(`namespaces_`,
     Tuple2(Seq(), maps.empty[hydra.core.Name, hydra.ext.python.syntax.Name]), g, sets.empty[hydra.core.Name],
     hydra.ext.python.utils.targetPythonVersion, false, sets.empty[hydra.core.Name]))(typ))

def pythonTestCodec(`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]): hydra.testing.TestCodec =
  hydra.testing.TestCodec("python", "py", (v1: hydra.core.Term) =>
  (v2: hydra.graph.Graph) =>
  hydra.ext.python.testCodec.termToPython(`namespaces_`)(v1)(v2), (v1: hydra.core.Type) =>
  (v2: hydra.graph.Graph) =>
  hydra.ext.python.testCodec.typeToPython(`namespaces_`)(v1)(v2), hydra.ext.python.testCodec.formatPythonTestName,
     hydra.ext.python.testCodec.namespaceToPythonModuleName, hydra.ext.python.testCodec.pythonTestCaseTemplate,
     hydra.ext.python.testCodec.pythonTestGroupTemplate, hydra.ext.python.testCodec.pythonModuleTemplate,
     hydra.ext.python.testCodec.pythonImportTemplate, (v1: scala.collection.immutable.Set[hydra.core.Name]) =>
  hydra.ext.python.testCodec.findPythonImports(`namespaces_`)(v1))

def pythonTestCodecWithContext(`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(tcontext: hydra.graph.Graph): hydra.testing.TestCodec =
  hydra.testing.TestCodec("python", "py", (v1: hydra.core.Term) =>
  (v2: hydra.graph.Graph) =>
  hydra.ext.python.testCodec.termToPythonWithContext(`namespaces_`)(tcontext)(true)(v1)(v2), (v1: hydra.core.Type) =>
  (v2: hydra.graph.Graph) =>
  hydra.ext.python.testCodec.typeToPython(`namespaces_`)(v1)(v2), hydra.ext.python.testCodec.formatPythonTestName,
     hydra.ext.python.testCodec.namespaceToPythonModuleName, hydra.ext.python.testCodec.pythonTestCaseTemplate,
     hydra.ext.python.testCodec.pythonTestGroupTemplate, hydra.ext.python.testCodec.pythonModuleTemplate,
     hydra.ext.python.testCodec.pythonImportTemplate, (v1: scala.collection.immutable.Set[hydra.core.Name]) =>
  hydra.ext.python.testCodec.findPythonImports(`namespaces_`)(v1))

def formatPythonTestName(name: scala.Predef.String): scala.Predef.String =
  strings.cat2("test_")(strings.fromList(lists.map[Int, Int]((c: Int) => logic.ifElse[Int](chars.isAlphaNum(c))(chars.toLower(c))(95))(strings.toList(name))))

def namespaceToPythonModuleName(`ns_`: hydra.module.Namespace): scala.Predef.String = `ns_`

val pythonTestCaseTemplate: scala.Predef.String = strings.intercalate("\n")(Seq("def {name}():", "    assert ({input}) == ({output})"))

val pythonTestGroupTemplate: scala.Predef.String = "# {groupName}"

val pythonModuleTemplate: scala.Predef.String = strings.intercalate("\n")(Seq(strings.cat2("# ")(hydra.constants.warningAutoGeneratedFile),
   "", "{imports}", "", "{testGroup}", "", "{testCases}"))

val pythonImportTemplate: scala.Predef.String = "import {namespace}"

def findPythonImports[T0, T1](`namespaces_`: hydra.module.Namespaces[T0])(`names_`: T1): Seq[scala.Predef.String] =
  {
  val `mapping_`: Map[hydra.module.Namespace, T0] = (`namespaces_`.mapping)
  val filtered: Map[hydra.module.Namespace, T0] = maps.filterWithKey[hydra.module.Namespace, T0]((`ns_`: hydra.module.Namespace) =>
    (_v: T0) =>
    logic.not(equality.equal[scala.Predef.String](lists.head[scala.Predef.String](strings.splitOn("hydra.test.")(`ns_`)))("")))(`mapping_`)
  lists.map[Tuple2[hydra.module.Namespace, T0], scala.Predef.String]((entry: Tuple2[hydra.module.Namespace, T0]) =>
    strings.cat2("import ")(pairs.first[hydra.module.Namespace, T0](entry)))(maps.toList[hydra.module.Namespace, T0](filtered))
}

def namespacesForPythonModule[T0](mod: hydra.module.Module)(`graph_`: hydra.graph.Graph): Either[T0, hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]] =
  {
  val bindings: Seq[hydra.core.Binding] = hydra.lexical.graphToBindings(`graph_`)
  val defs: Seq[hydra.module.Definition] = maybes.mapMaybe[hydra.core.Binding, hydra.module.Definition]((b: hydra.core.Binding) =>
    maybes.map[hydra.core.TypeScheme, hydra.module.Definition]((ts: hydra.core.TypeScheme) =>
    hydra.module.Definition.term(hydra.module.TermDefinition(b.name, (b.term), ts)))(b.`type`))(bindings)
  Right(hydra.ext.python.utils.findNamespaces(mod.namespace)(defs))
}

def generatePythonTestGroupHierarchy[T0](g: hydra.graph.Graph)(`namespaces_`: T0)(codec: hydra.testing.TestCodec)(groupPath: Seq[scala.Predef.String])(testGroup: hydra.testing.TestGroup): Either[scala.Predef.String,
   scala.Predef.String] =
  {
  val `cases_`: Seq[hydra.testing.TestCaseWithMetadata] = (testGroup.cases)
  val subgroups: Seq[hydra.testing.TestGroup] = (testGroup.subgroups)
  eithers.bind[scala.Predef.String, Seq[Seq[scala.Predef.String]], scala.Predef.String](eithers.mapList[hydra.testing.TestCaseWithMetadata,
     Seq[scala.Predef.String], scala.Predef.String]((tc: hydra.testing.TestCaseWithMetadata) =>
    hydra.ext.python.testCodec.generatePythonTestCase(g)(`namespaces_`)(codec)(groupPath)(tc))(`cases_`))((testCaseLines: Seq[Seq[scala.Predef.String]]) =>
    eithers.bind[scala.Predef.String, Seq[scala.Predef.String], scala.Predef.String](eithers.mapList[hydra.testing.TestGroup,
       scala.Predef.String, scala.Predef.String]((subgroup: hydra.testing.TestGroup) =>
    {
    val groupName: scala.Predef.String = (subgroup.name)
    val header: scala.Predef.String = strings.cat2("# ")(groupName)
    eithers.map[scala.Predef.String, scala.Predef.String, scala.Predef.String]((content: scala.Predef.String) => strings.cat(Seq(header,
       "\n\n", content)))(hydra.ext.python.testCodec.generatePythonTestGroupHierarchy(g)(`namespaces_`)(codec)(lists.concat2[scala.Predef.String](groupPath)(Seq(groupName)))(subgroup))
  })(subgroups))((subgroupBlocks: Seq[scala.Predef.String]) =>
    {
    val testCasesStr: scala.Predef.String = strings.intercalate("\n\n")(lists.concat[scala.Predef.String](testCaseLines))
    val subgroupsStr: scala.Predef.String = strings.intercalate("\n\n")(subgroupBlocks)
    Right(strings.cat(Seq(testCasesStr, logic.ifElse[scala.Predef.String](logic.or(equality.equal[scala.Predef.String](testCasesStr)(""))(equality.equal[scala.Predef.String](subgroupsStr)("")))("")("\n\n"),
       subgroupsStr)))
  }))
}

def generatePythonTestCase[T0](g: hydra.graph.Graph)(`namespaces_`: T0)(codec: hydra.testing.TestCodec)(groupPath: Seq[scala.Predef.String])(tcm: hydra.testing.TestCaseWithMetadata): Either[scala.Predef.String,
   Seq[scala.Predef.String]] =
  {
  val `name_`: scala.Predef.String = (tcm.name)
  val tcase: hydra.testing.TestCase = (tcm.`case`)
  tcase match
    case hydra.testing.TestCase.delegatedEvaluation(v_TestCase_delegatedEvaluation_delCase) => {
      val `input_`: hydra.core.Term = (v_TestCase_delegatedEvaluation_delCase.input)
      val `output_`: hydra.core.Term = (v_TestCase_delegatedEvaluation_delCase.output)
      val fullName: scala.Predef.String = logic.ifElse[scala.Predef.String](lists.`null`[scala.Predef.String](groupPath))(`name_`)(strings.intercalate("__")(lists.concat2[scala.Predef.String](groupPath)(Seq(`name_`))))
      val formattedName: scala.Predef.String = codec.formatTestName(fullName)
      eithers.bind[scala.Predef.String, scala.Predef.String, Seq[scala.Predef.String]](codec.encodeTerm(`input_`)(g))((inputCode: scala.Predef.String) =>
        eithers.bind[scala.Predef.String, scala.Predef.String, Seq[scala.Predef.String]](codec.encodeTerm(`output_`)(g))((outputCode: scala.Predef.String) =>
        Right(Seq(strings.cat(Seq("def ", formattedName, "():")), strings.cat(Seq("    assert (", inputCode, ") == (", outputCode, ")"))))))
    }
    case _ => Right(Seq())
}

def generateTestFileWithPythonCodec[T0](codec: hydra.testing.TestCodec)(testModule: hydra.module.Module)(testGroup: hydra.testing.TestGroup)(`namespaces_`: T0)(g: hydra.graph.Graph): Either[scala.Predef.String,
   Tuple2[scala.Predef.String, scala.Predef.String]] =
  eithers.map[scala.Predef.String, Tuple2[scala.Predef.String, scala.Predef.String], scala.Predef.String]((testBody: scala.Predef.String) =>
  {
  val testModuleContent: scala.Predef.String = hydra.ext.python.testCodec.buildPythonTestModule(codec)(testModule)(testGroup)(testBody)(`namespaces_`)
  val `ns_`: hydra.module.Namespace = (testModule.namespace)
  val parts: Seq[scala.Predef.String] = strings.splitOn(".")(`ns_`)
  val dirParts: Seq[scala.Predef.String] = lists.init[scala.Predef.String](parts)
  val fileName: scala.Predef.String = strings.cat(Seq("test_", lists.last[scala.Predef.String](parts), ".py"))
  val filePath: scala.Predef.String = strings.cat(Seq(strings.intercalate("/")(dirParts), "/", fileName))
  Tuple2(filePath, testModuleContent)
})(hydra.ext.python.testCodec.generatePythonTestGroupHierarchy(g)(`namespaces_`)(codec)(Seq())(testGroup))

def buildPythonTestModule[T0, T1](codec: hydra.testing.TestCodec)(testModule: T0)(testGroup: hydra.testing.TestGroup)(testBody: scala.Predef.String)(`namespaces_`: T1): scala.Predef.String =
  {
  val `groupName_`: scala.Predef.String = (testGroup.name)
  val domainImports: Seq[scala.Predef.String] = codec.findImports(sets.empty[hydra.core.Name])
  val standardImports: Seq[scala.Predef.String] = Seq("from __future__ import annotations", "from typing import cast",
     "from decimal import Decimal", "from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing")
  val allImports: Seq[scala.Predef.String] = lists.concat2[scala.Predef.String](standardImports)(domainImports)
  val header: scala.Predef.String = strings.cat(Seq(strings.cat2("# ")(hydra.constants.warningAutoGeneratedFile),
     "\n", strings.cat2("# ")(`groupName_`), "\n\n", strings.intercalate("\n")(allImports), "\n\n"))
  strings.cat(Seq(header, testBody, "\n"))
}

def generatePythonTestFile(testModule: hydra.module.Module)(testGroup: hydra.testing.TestGroup)(g: hydra.graph.Graph): Either[scala.Predef.String,
   Tuple2[scala.Predef.String, scala.Predef.String]] =
  eithers.bind[scala.Predef.String, hydra.testing.TestGroup, Tuple2[scala.Predef.String, scala.Predef.String]](hydra.test.utils.inferTestGroupTerms(g)(testGroup))((inferredTestGroup: hydra.testing.TestGroup) =>
  eithers.bind[scala.Predef.String, hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], Tuple2[scala.Predef.String,
     scala.Predef.String]](hydra.ext.python.testCodec.namespacesForPythonModule(testModule)(g))((`namespaces_`: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]) =>
  hydra.ext.python.testCodec.generateTestFileWithPythonCodec(hydra.ext.python.testCodec.pythonTestCodecWithContext(`namespaces_`)(g))(testModule)(inferredTestGroup)(`namespaces_`)(g)))
