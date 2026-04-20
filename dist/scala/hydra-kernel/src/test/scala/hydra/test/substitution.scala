package hydra.test.substitution

import hydra.core.*

import hydra.testing.*

import hydra.typing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("substitution",
   None, Seq(hydra.testing.TestGroup("substInType", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("empty substitution returns type unchanged",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq()))(hydra.core.Type.literal(hydra.core.LiteralType.string))),
   hydra.show.core.`type`(hydra.core.Type.literal(hydra.core.LiteralType.string)))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("substitute type variable with int32",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))(hydra.core.Type.variable("a"))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.show.core.`type`(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("non-matching variable unchanged",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))(hydra.core.Type.variable("b"))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.show.core.`type`(hydra.core.Type.variable("b")))), None, Seq()), hydra.testing.TestCaseWithMetadata("substitute in function domain",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.string))))), hydra.show.core.`type`(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.string)))))), None, Seq()), hydra.testing.TestCaseWithMetadata("substitute in function codomain",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.string)))))(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("a"))))), hydra.show.core.`type`(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.string)))))), None, Seq()), hydra.testing.TestCaseWithMetadata("substitute in list element type",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))(hydra.core.Type.list(hydra.core.Type.variable("a")))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.show.core.`type`(hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("substitute in optional type",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.string)))))(hydra.core.Type.maybe(hydra.core.Type.variable("a")))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.show.core.`type`(hydra.core.Type.maybe(hydra.core.Type.literal(hydra.core.LiteralType.string))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("substitute in pair type both sides",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))(hydra.core.Type.pair(hydra.core.PairType(hydra.core.Type.variable("a"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("a"))))), hydra.show.core.`type`(hydra.core.Type.pair(hydra.core.PairType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("substitute in either type", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.string)))))(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.variable("a"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))))),
   hydra.show.core.`type`(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.string),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("substitute in map key type",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("k", hydra.core.Type.literal(hydra.core.LiteralType.string)))))(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("k"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))))),
   hydra.show.core.`type`(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.literal(hydra.core.LiteralType.string),
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("substitute in set type", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))(hydra.core.Type.set(hydra.core.Type.variable("a")))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.show.core.`type`(hydra.core.Type.set(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("nested substitution in list of pairs",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))(hydra.core.Type.list(hydra.core.Type.pair(hydra.core.PairType(hydra.core.Type.variable("a"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.string)))))), hydra.show.core.`type`(hydra.core.Type.list(hydra.core.Type.pair(hydra.core.PairType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.string))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("multiple substitutions",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Tuple2("b", hydra.core.Type.literal(hydra.core.LiteralType.string)))))(hydra.core.Type.pair(hydra.core.PairType(hydra.core.Type.variable("a"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("b"))))), hydra.show.core.`type`(hydra.core.Type.pair(hydra.core.PairType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.string)))))), None, Seq()), hydra.testing.TestCaseWithMetadata("forAll bound variable not substituted",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("a", hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))(hydra.core.Type.forall(hydra.core.ForallType("a",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
   hydra.core.Type.variable("a"))))))), hydra.show.core.`type`(hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
   hydra.core.Type.variable("a")))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("forAll free variable substituted",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.substitution.substInType(hydra.lib.maps.fromList[hydra.core.Name,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type](Seq(Tuple2("b", hydra.core.Type.literal(hydra.core.LiteralType.string)))))(hydra.core.Type.forall(hydra.core.ForallType("a",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
   hydra.core.Type.variable("b"))))))), hydra.show.core.`type`(hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
   hydra.core.Type.literal(hydra.core.LiteralType.string)))))))), None, Seq())))),
   Seq())
