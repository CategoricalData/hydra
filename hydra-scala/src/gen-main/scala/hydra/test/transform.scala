package hydra.test.transform

import hydra.core.*

import hydra.module.*

import hydra.testing.*

import hydra.util.*

import hydra.lib.eithers

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.strings

def addGenerationPrefix(`ns_`: hydra.module.Namespace): hydra.module.Namespace = hydra.lib.strings.cat2("generation.")(`ns_`)

def buildConvertCaseCall(fromConv: hydra.util.CaseConvention)(toConv: hydra.util.CaseConvention)(`input_`: scala.Predef.String): hydra.core.Term =
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.formatting.convertCase"),
     hydra.test.transform.encodeCaseConvention(fromConv))), hydra.test.transform.encodeCaseConvention(toConv))),
     hydra.core.Term.literal(hydra.core.Literal.string(`input_`))))

def buildTopologicalSortCall(adjList: Seq[Tuple2[Int, Seq[Int]]]): hydra.core.Term =
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.sorting.topologicalSort"),
     hydra.test.transform.encodeAdjacencyList(adjList)))

def buildTopologicalSortSCCCall(adjList: Seq[Tuple2[Int, Seq[Int]]]): hydra.core.Term =
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.sorting.topologicalSortComponents"),
     hydra.test.transform.encodeAdjacencyList(adjList)))

def collectTestCases(tg: hydra.testing.TestGroup): Seq[hydra.testing.TestCaseWithMetadata] =
  hydra.lib.lists.concat2[hydra.testing.TestCaseWithMetadata](tg.cases)(hydra.lib.lists.concat[hydra.testing.TestCaseWithMetadata](hydra.lib.lists.map[hydra.testing.TestGroup,
     Seq[hydra.testing.TestCaseWithMetadata]]((sg: hydra.testing.TestGroup) => hydra.test.transform.collectTestCases(sg))(tg.subgroups)))

def encodeAdjacencyList(pairs: Seq[Tuple2[Int, Seq[Int]]]): hydra.core.Term =
  hydra.core.Term.list(hydra.lib.lists.map[Tuple2[Int, Seq[Int]], hydra.core.Term]((p: Tuple2[Int, Seq[Int]]) =>
  hydra.core.Term.pair(Tuple2(hydra.test.transform.encodeInt(hydra.lib.pairs.first[Int, Seq[Int]](p)),
     hydra.core.Term.list(hydra.lib.lists.map[Int, hydra.core.Term]((d: Int) => hydra.test.transform.encodeInt(d))(hydra.lib.pairs.second[Int,
     Seq[Int]](p))))))(pairs))

def encodeCaseConvention(conv: hydra.util.CaseConvention): hydra.core.Term =
  hydra.core.Term.union(hydra.core.Injection("hydra.util.CaseConvention", hydra.core.Field(conv match
  case hydra.util.CaseConvention.lowerSnake => "lowerSnake"
  case hydra.util.CaseConvention.upperSnake => "upperSnake"
  case hydra.util.CaseConvention.camel => "camel"
  case hydra.util.CaseConvention.pascal => "pascal", hydra.core.Term.unit)))

def encodeEitherListList(e: Either[Seq[Seq[Int]], Seq[Int]]): hydra.core.Term =
  hydra.core.Term.either(hydra.lib.eithers.bimap[Seq[Seq[Int]], Seq[Int], hydra.core.Term, hydra.core.Term]((cycles: Seq[Seq[Int]]) => hydra.test.transform.encodeListList(cycles))((sorted: Seq[Int]) => hydra.test.transform.encodeIntList(sorted))(e))

def encodeInt(n: Int): hydra.core.Term =
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(n)))

def encodeIntList(ints: Seq[Int]): hydra.core.Term =
  hydra.core.Term.list(hydra.lib.lists.map[Int, hydra.core.Term]((n: Int) => hydra.test.transform.encodeInt(n))(ints))

def encodeListList(lists: Seq[Seq[Int]]): hydra.core.Term =
  hydra.core.Term.list(hydra.lib.lists.map[Seq[Int], hydra.core.Term]((l: Seq[Int]) => hydra.test.transform.encodeIntList(l))(lists))

def transformModule(m: hydra.module.Module): hydra.module.Module =
  hydra.module.Module(hydra.test.transform.addGenerationPrefix(m.namespace), (m.definitions), (m.termDependencies), (m.typeDependencies), (m.description))

def transformTestCase(tcm: hydra.testing.TestCaseWithMetadata): Option[hydra.testing.TestCaseWithMetadata] =
  {
  lazy val `name_`: scala.Predef.String = (tcm.name)
  lazy val tc: hydra.testing.TestCase = (tcm.`case`)
  lazy val desc: Option[scala.Predef.String] = (tcm.description)
  lazy val `tags_`: Seq[hydra.testing.Tag] = (tcm.tags)
  tc match
    case hydra.testing.TestCase.caseConversion(v_TestCase_caseConversion_ccase) => {
      lazy val fromConv: hydra.util.CaseConvention = (v_TestCase_caseConversion_ccase.fromConvention)
      lazy val toConv: hydra.util.CaseConvention = (v_TestCase_caseConversion_ccase.toConvention)
      lazy val fromStr: scala.Predef.String = (v_TestCase_caseConversion_ccase.fromString)
      lazy val toStr: scala.Predef.String = (v_TestCase_caseConversion_ccase.`toString_`)
      Some(hydra.testing.TestCaseWithMetadata(`name_`, hydra.testing.TestCase.delegatedEvaluation(hydra.testing.DelegatedEvaluationTestCase(hydra.test.transform.buildConvertCaseCall(fromConv)(toConv)(fromStr),
         hydra.core.Term.literal(hydra.core.Literal.string(toStr)))), desc, `tags_`))
    }
    case hydra.testing.TestCase.evaluation(v_TestCase_evaluation_ecase) => {
      lazy val `input_`: hydra.core.Term = (v_TestCase_evaluation_ecase.input)
      lazy val `output_`: hydra.core.Term = (v_TestCase_evaluation_ecase.output)
      Some(hydra.testing.TestCaseWithMetadata(`name_`, hydra.testing.TestCase.delegatedEvaluation(hydra.testing.DelegatedEvaluationTestCase(`input_`,
         `output_`)), desc, `tags_`))
    }
    case hydra.testing.TestCase.delegatedEvaluation(v_TestCase_delegatedEvaluation__) => Some(tcm)
    case hydra.testing.TestCase.topologicalSort(v_TestCase_topologicalSort_tscase) => {
      lazy val adjList: Seq[Tuple2[Int, Seq[Int]]] = (v_TestCase_topologicalSort_tscase.adjacencyList)
      lazy val expected: Either[Seq[Seq[Int]], Seq[Int]] = (v_TestCase_topologicalSort_tscase.expected)
      Some(hydra.testing.TestCaseWithMetadata(`name_`, hydra.testing.TestCase.delegatedEvaluation(hydra.testing.DelegatedEvaluationTestCase(hydra.test.transform.buildTopologicalSortCall(adjList),
         hydra.test.transform.encodeEitherListList(expected))), desc, `tags_`))
    }
    case hydra.testing.TestCase.topologicalSortSCC(v_TestCase_topologicalSortSCC_scccase) => {
      lazy val adjList: Seq[Tuple2[Int, Seq[Int]]] = (v_TestCase_topologicalSortSCC_scccase.adjacencyList)
      lazy val expected: Seq[Seq[Int]] = (v_TestCase_topologicalSortSCC_scccase.expected)
      Some(hydra.testing.TestCaseWithMetadata(`name_`, hydra.testing.TestCase.delegatedEvaluation(hydra.testing.DelegatedEvaluationTestCase(hydra.test.transform.buildTopologicalSortSCCCall(adjList),
         hydra.test.transform.encodeListList(expected))), desc, `tags_`))
    }
    case hydra.testing.TestCase.validateCoreTerm(v_TestCase_validateCoreTerm__) => Some(tcm)
    case _ => None
}

def transformToCompiledTests(tg: hydra.testing.TestGroup): Option[hydra.testing.TestGroup] =
  {
  lazy val `name_`: scala.Predef.String = (tg.name)
  lazy val desc: Option[scala.Predef.String] = (tg.description)
  lazy val subgroups: Seq[hydra.testing.TestGroup] = (tg.subgroups)
  lazy val `cases_`: Seq[hydra.testing.TestCaseWithMetadata] = (tg.cases)
  lazy val transformedCases: Seq[hydra.testing.TestCaseWithMetadata] = hydra.lib.maybes.cat[hydra.testing.TestCaseWithMetadata](hydra.lib.lists.map[hydra.testing.TestCaseWithMetadata,
     Option[hydra.testing.TestCaseWithMetadata]]((tc: hydra.testing.TestCaseWithMetadata) => hydra.test.transform.transformTestCase(tc))(`cases_`))
  lazy val transformedSubgroups: Seq[hydra.testing.TestGroup] = hydra.lib.maybes.cat[hydra.testing.TestGroup](hydra.lib.lists.map[hydra.testing.TestGroup,
     Option[hydra.testing.TestGroup]]((sg: hydra.testing.TestGroup) => hydra.test.transform.transformToCompiledTests(sg))(subgroups))
  hydra.lib.logic.ifElse[Option[hydra.testing.TestGroup]](hydra.lib.logic.and(hydra.lib.lists.`null`[hydra.testing.TestCaseWithMetadata](transformedCases))(hydra.lib.lists.`null`[hydra.testing.TestGroup](transformedSubgroups)))(None)(Some(hydra.testing.TestGroup(`name_`,
     desc, transformedSubgroups, transformedCases)))
}
