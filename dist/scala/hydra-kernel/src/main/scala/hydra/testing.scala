package hydra.testing

import hydra.core.*

type Tag = scala.Predef.String

enum TestCase :
   case universal(value: hydra.testing.UniversalTestCase) extends TestCase

case class TestCaseWithMetadata(name: scala.Predef.String, `case`: hydra.testing.TestCase, description: Option[scala.Predef.String],
   tags: Seq[hydra.testing.Tag])

case class TestGroup(name: scala.Predef.String, description: Option[scala.Predef.String], subgroups: Seq[hydra.testing.TestGroup],
   cases: Seq[hydra.testing.TestCaseWithMetadata])

case class UniversalTestCase(actual: scala.Predef.String, expected: scala.Predef.String)
