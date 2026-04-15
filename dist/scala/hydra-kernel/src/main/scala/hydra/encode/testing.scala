package hydra.encode.testing

import hydra.core.*

import hydra.testing.*

def tag(x: hydra.testing.Tag): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.testing.Tag", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def testCase(v1: hydra.testing.TestCase): hydra.core.Term =
  v1 match
  case hydra.testing.TestCase.universal(v_TestCase_universal_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.testing.TestCase",
     hydra.core.Field("universal", hydra.encode.testing.universalTestCase(v_TestCase_universal_y))))

def testCaseWithMetadata(x: hydra.testing.TestCaseWithMetadata): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.TestCaseWithMetadata", Seq(hydra.core.Field("name",
     hydra.core.Term.literal(hydra.core.Literal.string(x.name))), hydra.core.Field("case",
     hydra.encode.testing.testCase(x.`case`)), hydra.core.Field("description", hydra.core.Term.maybe(hydra.lib.maybes.map[scala.Predef.String,
     hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x.description))),
     hydra.core.Field("tags", hydra.core.Term.list(hydra.lib.lists.map[hydra.testing.Tag,
     hydra.core.Term](hydra.encode.testing.tag)(x.tags))))))

def testGroup(x: hydra.testing.TestGroup): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.TestGroup", Seq(hydra.core.Field("name",
     hydra.core.Term.literal(hydra.core.Literal.string(x.name))), hydra.core.Field("description",
     hydra.core.Term.maybe(hydra.lib.maybes.map[scala.Predef.String, hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x.description))),
     hydra.core.Field("subgroups", hydra.core.Term.list(hydra.lib.lists.map[hydra.testing.TestGroup,
     hydra.core.Term](hydra.encode.testing.testGroup)(x.subgroups))), hydra.core.Field("cases",
     hydra.core.Term.list(hydra.lib.lists.map[hydra.testing.TestCaseWithMetadata,
     hydra.core.Term](hydra.encode.testing.testCaseWithMetadata)(x.cases))))))

def universalTestCase(x: hydra.testing.UniversalTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.UniversalTestCase", Seq(hydra.core.Field("actual",
     hydra.core.Term.literal(hydra.core.Literal.string(x.actual))), hydra.core.Field("expected",
     hydra.core.Term.literal(hydra.core.Literal.string(x.expected))))))
