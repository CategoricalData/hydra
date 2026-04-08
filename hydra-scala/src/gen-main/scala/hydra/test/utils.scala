package hydra.test.utils

import hydra.testing.*

import hydra.typing.*

def inferTerm(g: hydra.graph.Graph)(term: hydra.core.Term): Either[scala.Predef.String, hydra.core.Term] =
  hydra.lib.eithers.bimap[hydra.errors.Error, hydra.typing.InferenceResult, scala.Predef.String, hydra.core.Term]((e: hydra.errors.Error) => hydra.show.errors.error(e))((x: hydra.typing.InferenceResult) => (x.term))(hydra.inference.inferInGraphContext(hydra.lexical.emptyContext)(g)(term))

def inferTestCase[T0, T1](g: T0)(tcm: hydra.testing.TestCaseWithMetadata): Either[T1, hydra.testing.TestCaseWithMetadata] =
  {
  lazy val `name_`: scala.Predef.String = (tcm.name)
  lazy val tcase: hydra.testing.TestCase = (tcm.`case`)
  lazy val desc: Option[scala.Predef.String] = (tcm.description)
  lazy val `tags_`: Seq[hydra.testing.Tag] = (tcm.tags)
  hydra.lib.eithers.map[hydra.testing.TestCase, hydra.testing.TestCaseWithMetadata, T1]((inferredCase: hydra.testing.TestCase) =>
    hydra.testing.TestCaseWithMetadata(`name_`, inferredCase, desc, `tags_`))(Right(tcase))
}

def inferTestGroupTerms[T0, T1](g: T0)(tg: hydra.testing.TestGroup): Either[T1, hydra.testing.TestGroup] =
  {
  lazy val `name_`: scala.Predef.String = (tg.name)
  lazy val desc: Option[scala.Predef.String] = (tg.description)
  lazy val subgroups: Seq[hydra.testing.TestGroup] = (tg.subgroups)
  lazy val `cases_`: Seq[hydra.testing.TestCaseWithMetadata] = (tg.cases)
  hydra.lib.eithers.bind[T1, Seq[hydra.testing.TestGroup], hydra.testing.TestGroup](hydra.lib.eithers.mapList[hydra.testing.TestGroup,
     hydra.testing.TestGroup, T1]((sg: hydra.testing.TestGroup) => hydra.test.utils.inferTestGroupTerms(g)(sg))(subgroups))((inferredSubgroups: Seq[hydra.testing.TestGroup]) =>
    hydra.lib.eithers.map[Seq[hydra.testing.TestCaseWithMetadata], hydra.testing.TestGroup, T1]((inferredCases: Seq[hydra.testing.TestCaseWithMetadata]) =>
    hydra.testing.TestGroup(`name_`, desc, inferredSubgroups, inferredCases))(hydra.lib.eithers.mapList[hydra.testing.TestCaseWithMetadata,
       hydra.testing.TestCaseWithMetadata, T1]((tc: hydra.testing.TestCaseWithMetadata) => hydra.test.utils.inferTestCase(g)(tc))(`cases_`)))
}
