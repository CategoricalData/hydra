package hydra.test.strip

import hydra.core.*

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("strip", None,
   Seq(hydra.testing.TestGroup("deannotateTerm", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("unannotated literal unchanged",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.strip.deannotateTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("unannotated variable unchanged",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.strip.deannotateTerm(hydra.core.Term.variable("x"))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.show.core.term(hydra.core.Term.variable("x")))), None, Seq()), hydra.testing.TestCaseWithMetadata("unannotated lambda unchanged",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.strip.deannotateTerm(hydra.core.Term.lambda(hydra.core.Lambda("x",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, hydra.core.Term.variable("x"))))), hydra.show.core.term(hydra.core.Term.lambda(hydra.core.Lambda("x",
   None, hydra.core.Term.variable("x")))))), None, Seq()), hydra.testing.TestCaseWithMetadata("single annotation stripped",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.strip.deannotateTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map())))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("nested annotations stripped",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.strip.deannotateTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map())), Map())))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("annotated lambda stripped", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.strip.deannotateTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.lambda(hydra.core.Lambda("x",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, hydra.core.Term.variable("x"))), Map())))), hydra.show.core.term(hydra.core.Term.lambda(hydra.core.Lambda("x",
   None, hydra.core.Term.variable("x")))))), None, Seq()), hydra.testing.TestCaseWithMetadata("annotated application stripped",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.strip.deannotateTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("f"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.variable("x"))), Map())))), hydra.show.core.term(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("f"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.variable("x")))))), None, Seq()))), hydra.testing.TestGroup("deannotateType",
   None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("unannotated primitive type unchanged",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.strip.deannotateType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.show.core.`type`(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("unannotated string type unchanged",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.strip.deannotateType(hydra.core.Type.literal(hydra.core.LiteralType.string))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.show.core.`type`(hydra.core.Type.literal(hydra.core.LiteralType.string)))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("unannotated function type unchanged",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.strip.deannotateType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.string))))), hydra.show.core.`type`(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.string)))))), None, Seq()), hydra.testing.TestCaseWithMetadata("single annotation stripped",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.strip.deannotateType(hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map())))), hydra.show.core.`type`(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("nested annotations stripped",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.strip.deannotateType(hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.string),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map())), Map())))), hydra.show.core.`type`(hydra.core.Type.literal(hydra.core.LiteralType.string)))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("annotated list type stripped",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.strip.deannotateType(hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map())))), hydra.show.core.`type`(hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("annotated function type stripped",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.strip.deannotateType(hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.string))), Map())))), hydra.show.core.`type`(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.string)))))), None, Seq())))), Seq())
