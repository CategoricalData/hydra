package hydra.test.utils

import hydra.context.*

import hydra.testing.*

import hydra.typing.*

import hydra.lib.eithers

def inferTerm(g: hydra.graph.Graph)(term: hydra.core.Term): Either[scala.Predef.String, hydra.core.Term] =
  hydra.lib.eithers.bimap[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult, scala.Predef.String, hydra.core.Term]((ic: hydra.context.InContext[hydra.errors.Error]) => hydra.show.errors.error(ic.`object`))((x: hydra.typing.InferenceResult) => (x.term))(hydra.inference.inferInGraphContext(hydra.lexical.emptyContext)(g)(term))

def inferTestCase(g: hydra.graph.Graph)(tcm: hydra.testing.TestCaseWithMetadata): Either[scala.Predef.String, hydra.testing.TestCaseWithMetadata] =
  {
  lazy val `name_`: scala.Predef.String = (tcm.name)
  lazy val tcase: hydra.testing.TestCase = (tcm.`case`)
  lazy val desc: Option[scala.Predef.String] = (tcm.description)
  lazy val `tags_`: Seq[hydra.testing.Tag] = (tcm.tags)
  hydra.lib.eithers.map[hydra.testing.TestCase, hydra.testing.TestCaseWithMetadata, scala.Predef.String]((inferredCase: hydra.testing.TestCase) =>
    hydra.testing.TestCaseWithMetadata(`name_`, inferredCase, desc, `tags_`))(tcase match
    case hydra.testing.TestCase.delegatedEvaluation(v_TestCase_delegatedEvaluation_delCase) => {
      lazy val `input_`: hydra.core.Term = (v_TestCase_delegatedEvaluation_delCase.input)
      lazy val `output_`: hydra.core.Term = (v_TestCase_delegatedEvaluation_delCase.output)
      hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Term, hydra.testing.TestCase](hydra.test.utils.inferTerm(g)(`input_`))((inferredInput: hydra.core.Term) =>
        hydra.lib.eithers.map[hydra.core.Term, hydra.testing.TestCase, scala.Predef.String]((inferredOutput: hydra.core.Term) =>
        hydra.testing.TestCase.delegatedEvaluation(hydra.testing.DelegatedEvaluationTestCase(inferredInput, inferredOutput)))(hydra.test.utils.inferTerm(g)(`output_`)))
    }
    case _ => Right(tcase))
}

def inferTestGroupTerms(g: hydra.graph.Graph)(tg: hydra.testing.TestGroup): Either[scala.Predef.String, hydra.testing.TestGroup] =
  {
  lazy val `name_`: scala.Predef.String = (tg.name)
  lazy val desc: Option[scala.Predef.String] = (tg.description)
  lazy val subgroups: Seq[hydra.testing.TestGroup] = (tg.subgroups)
  lazy val `cases_`: Seq[hydra.testing.TestCaseWithMetadata] = (tg.cases)
  hydra.lib.eithers.bind[scala.Predef.String, Seq[hydra.testing.TestGroup], hydra.testing.TestGroup](hydra.lib.eithers.mapList[hydra.testing.TestGroup, hydra.testing.TestGroup, scala.Predef.String]((sg: hydra.testing.TestGroup) => hydra.test.utils.inferTestGroupTerms(g)(sg))(subgroups))((inferredSubgroups: Seq[hydra.testing.TestGroup]) =>
    hydra.lib.eithers.map[Seq[hydra.testing.TestCaseWithMetadata], hydra.testing.TestGroup, scala.Predef.String]((inferredCases: Seq[hydra.testing.TestCaseWithMetadata]) =>
    hydra.testing.TestGroup(`name_`, desc, inferredSubgroups, inferredCases))(hydra.lib.eithers.mapList[hydra.testing.TestCaseWithMetadata, hydra.testing.TestCaseWithMetadata, scala.Predef.String]((tc: hydra.testing.TestCaseWithMetadata) => hydra.test.utils.inferTestCase(g)(tc))(`cases_`)))
}
