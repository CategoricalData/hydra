package hydra.test.generation

import hydra.core.*

import hydra.packaging.*

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("generation",
   None, Seq(hydra.testing.TestGroup("inferModulesGiven", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("incremental inference of subset matches full inference",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   Seq[hydra.packaging.Module], scala.Predef.String]((e: hydra.errors.Error) => "<<inference error>>")((ms: Seq[hydra.packaging.Module]) =>
  hydra.lib.strings.cat(hydra.lib.lists.map[hydra.packaging.Module, scala.Predef.String]((m: hydra.packaging.Module) =>
  hydra.lib.strings.cat(hydra.lib.lists.map[hydra.packaging.Definition, scala.Predef.String]((d: hydra.packaging.Definition) =>
  d match
  case hydra.packaging.Definition.`type`(v_Definition_type_td) => ""
  case hydra.packaging.Definition.term(v_Definition_term_td) => hydra.lib.strings.cat(Seq(v_Definition_term_td.name,
     " :: ", hydra.lib.maybes.maybe[scala.Predef.String, hydra.core.TypeScheme]("<no scheme>")((ts: hydra.core.TypeScheme) => hydra.show.core.typeScheme(ts))(v_Definition_term_td.`type`),
     "\n")))(m.definitions)))(ms)))(hydra.codegen.inferModulesGiven(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(Seq(hydra.packaging.Module("hydra.testInput.a",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.a.idA",
     hydra.core.Term.lambda(hydra.core.Lambda("x", None, hydra.core.Term.variable("x"))),
     Some(hydra.core.TypeScheme(Seq("a"), hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
     hydra.core.Type.variable("a"))), None))))), Seq(), Seq(), None), hydra.packaging.Module("hydra.testInput.b",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.b.useId",
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.testInput.a.idA"),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))),
     None))), Seq("hydra.testInput.a"), Seq(), None)))(Seq(hydra.packaging.Module("hydra.testInput.b",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.b.useId",
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.testInput.a.idA"),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))),
     None))), Seq("hydra.testInput.a"), Seq(), None)))), hydra.lib.eithers.either[hydra.errors.Error,
     Seq[hydra.packaging.Module], scala.Predef.String]((e: hydra.errors.Error) => "<<inference error>>")((ms: Seq[hydra.packaging.Module]) =>
  hydra.lib.strings.cat(hydra.lib.lists.map[hydra.packaging.Module, scala.Predef.String]((m: hydra.packaging.Module) =>
  hydra.lib.strings.cat(hydra.lib.lists.map[hydra.packaging.Definition, scala.Predef.String]((d: hydra.packaging.Definition) =>
  d match
  case hydra.packaging.Definition.`type`(v_Definition_type_td) => ""
  case hydra.packaging.Definition.term(v_Definition_term_td) => hydra.lib.strings.cat(Seq(v_Definition_term_td.name,
     " :: ", hydra.lib.maybes.maybe[scala.Predef.String, hydra.core.TypeScheme]("<no scheme>")((ts: hydra.core.TypeScheme) => hydra.show.core.typeScheme(ts))(v_Definition_term_td.`type`),
     "\n")))(m.definitions)))(ms)))(hydra.codegen.inferModules(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(Seq(hydra.packaging.Module("hydra.testInput.a",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.a.idA",
     hydra.core.Term.lambda(hydra.core.Lambda("x", None, hydra.core.Term.variable("x"))),
     Some(hydra.core.TypeScheme(Seq("a"), hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
     hydra.core.Type.variable("a"))), None))))), Seq(), Seq(), None), hydra.packaging.Module("hydra.testInput.b",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.b.useId",
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.testInput.a.idA"),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))),
     None))), Seq("hydra.testInput.a"), Seq(), None)))(Seq(hydra.packaging.Module("hydra.testInput.b",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.b.useId",
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.testInput.a.idA"),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))),
     None))), Seq("hydra.testInput.a"), Seq(), None)))))), None, Seq()), hydra.testing.TestCaseWithMetadata("incremental inference of full universe matches full inference",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     Seq[hydra.packaging.Module], scala.Predef.String]((e: hydra.errors.Error) => "<<inference error>>")((ms: Seq[hydra.packaging.Module]) =>
  hydra.lib.strings.cat(hydra.lib.lists.map[hydra.packaging.Module, scala.Predef.String]((m: hydra.packaging.Module) =>
  hydra.lib.strings.cat(hydra.lib.lists.map[hydra.packaging.Definition, scala.Predef.String]((d: hydra.packaging.Definition) =>
  d match
  case hydra.packaging.Definition.`type`(v_Definition_type_td) => ""
  case hydra.packaging.Definition.term(v_Definition_term_td) => hydra.lib.strings.cat(Seq(v_Definition_term_td.name,
     " :: ", hydra.lib.maybes.maybe[scala.Predef.String, hydra.core.TypeScheme]("<no scheme>")((ts: hydra.core.TypeScheme) => hydra.show.core.typeScheme(ts))(v_Definition_term_td.`type`),
     "\n")))(m.definitions)))(ms)))(hydra.codegen.inferModulesGiven(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(Seq(hydra.packaging.Module("hydra.testInput.a",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.a.idA",
     hydra.core.Term.lambda(hydra.core.Lambda("x", None, hydra.core.Term.variable("x"))),
     Some(hydra.core.TypeScheme(Seq("a"), hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
     hydra.core.Type.variable("a"))), None))))), Seq(), Seq(), None), hydra.packaging.Module("hydra.testInput.b",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.b.useId",
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.testInput.a.idA"),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))),
     None))), Seq("hydra.testInput.a"), Seq(), None)))(Seq(hydra.packaging.Module("hydra.testInput.a",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.a.idA",
     hydra.core.Term.lambda(hydra.core.Lambda("x", None, hydra.core.Term.variable("x"))),
     Some(hydra.core.TypeScheme(Seq("a"), hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
     hydra.core.Type.variable("a"))), None))))), Seq(), Seq(), None), hydra.packaging.Module("hydra.testInput.b",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.b.useId",
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.testInput.a.idA"),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))),
     None))), Seq("hydra.testInput.a"), Seq(), None)))), hydra.lib.eithers.either[hydra.errors.Error,
     Seq[hydra.packaging.Module], scala.Predef.String]((e: hydra.errors.Error) => "<<inference error>>")((ms: Seq[hydra.packaging.Module]) =>
  hydra.lib.strings.cat(hydra.lib.lists.map[hydra.packaging.Module, scala.Predef.String]((m: hydra.packaging.Module) =>
  hydra.lib.strings.cat(hydra.lib.lists.map[hydra.packaging.Definition, scala.Predef.String]((d: hydra.packaging.Definition) =>
  d match
  case hydra.packaging.Definition.`type`(v_Definition_type_td) => ""
  case hydra.packaging.Definition.term(v_Definition_term_td) => hydra.lib.strings.cat(Seq(v_Definition_term_td.name,
     " :: ", hydra.lib.maybes.maybe[scala.Predef.String, hydra.core.TypeScheme]("<no scheme>")((ts: hydra.core.TypeScheme) => hydra.show.core.typeScheme(ts))(v_Definition_term_td.`type`),
     "\n")))(m.definitions)))(ms)))(hydra.codegen.inferModules(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(Seq(hydra.packaging.Module("hydra.testInput.a",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.a.idA",
     hydra.core.Term.lambda(hydra.core.Lambda("x", None, hydra.core.Term.variable("x"))),
     Some(hydra.core.TypeScheme(Seq("a"), hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
     hydra.core.Type.variable("a"))), None))))), Seq(), Seq(), None), hydra.packaging.Module("hydra.testInput.b",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.b.useId",
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.testInput.a.idA"),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))),
     None))), Seq("hydra.testInput.a"), Seq(), None)))(Seq(hydra.packaging.Module("hydra.testInput.a",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.a.idA",
     hydra.core.Term.lambda(hydra.core.Lambda("x", None, hydra.core.Term.variable("x"))),
     Some(hydra.core.TypeScheme(Seq("a"), hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
     hydra.core.Type.variable("a"))), None))))), Seq(), Seq(), None), hydra.packaging.Module("hydra.testInput.b",
     Seq(hydra.packaging.Definition.term(hydra.packaging.TermDefinition("hydra.testInput.b.useId",
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.testInput.a.idA"),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))),
     None))), Seq("hydra.testInput.a"), Seq(), None)))))), None, Seq())))), Seq())
