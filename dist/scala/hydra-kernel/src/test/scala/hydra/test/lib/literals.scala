package hydra.test.lib.literals

import hydra.core.*

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("hydra.lib.literals primitives",
   None, Seq(hydra.testing.TestGroup("bigintToInt8", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToInt8"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(42.toByte)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToInt8"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-42")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(-42.toByte)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("bigintToInt16", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToInt16"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("1000")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(1000.toShort)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToInt16"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-1000")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(-1000.toShort)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("bigintToInt32", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToInt32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToInt32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-42")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(-42)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToInt32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0)))))),
   None, Seq()))), hydra.testing.TestGroup("bigintToInt64", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToInt64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("1000000")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(1000000L)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToInt64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-1000000")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(-1000000L)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("bigintToUint8", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToUint8"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(0.toByte)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("typical value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToUint8"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("100")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(100.toByte)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("bigintToUint16", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToUint16"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(0)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("typical value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToUint16"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("1000")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(1000)))))),
   None, Seq()))), hydra.testing.TestGroup("bigintToUint32", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToUint32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(0L)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("typical value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToUint32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("100000")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(100000L)))))),
   None, Seq()))), hydra.testing.TestGroup("bigintToUint64", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToUint64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(BigInt("0"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("typical value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToUint64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("1000000")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(BigInt("1000000"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("int8ToBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int8ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(42.toByte))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int8ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(-42.toByte))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-42"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("max value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int8ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(127.toByte))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("127"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("min value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int8ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(-128.toByte))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-128"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("int16ToBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int16ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(1000.toShort))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("1000"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int16ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(-1000.toShort))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-1000"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("int32ToBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int32ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int32ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(-42))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-42"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int32ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("int64ToBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int64ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(1000000L))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("1000000"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.int64ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(-1000000L))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-1000000"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("uint8ToBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.uint8ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(0.toByte))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("max value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.uint8ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(-1.toByte))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("255"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("uint16ToBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.uint16ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(0))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("typical value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.uint16ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(1000))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("1000"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("uint32ToBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.uint32ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(0L))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("typical value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.uint32ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(100000L))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("100000"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("uint64ToBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.uint64ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(BigInt("0")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("typical value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.uint64ToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(BigInt("1000000")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("1000000"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("float32ToBigfloat", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float32ToBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(2.5f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(2.5))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float32ToBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(-2.5f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(-2.5))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float32ToBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(0.0f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(0.0))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("float64ToBigfloat", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float64ToBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(3.14159))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(3.14159))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float64ToBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-2.71828))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(-2.71828))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float64ToBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(0.0))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("bigfloatToFloat32", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(3.14)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(3.14f)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(-2.5)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(-2.5f)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(0.0)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(0.0f)))))),
   None, Seq()))), hydra.testing.TestGroup("bigfloatToFloat64", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(3.14159)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(3.14159)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(-2.71828)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-2.71828)))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(0.0)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))))),
   None, Seq()))), hydra.testing.TestGroup("bigintToBigfloat", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(42.0))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-42")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(-42.0))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(0.0))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("bigfloatToBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(42.7)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("43"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(-42.7)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-43"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(0.0)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("round down", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(42.3)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("half even up", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(42.5)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("half even down", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigfloatToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(43.5)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("44"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("bigintToDecimal", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("42.0")))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-42")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("-42.0")))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.bigintToDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("0.0")))))),
   None, Seq()))), hydra.testing.TestGroup("decimalToBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive whole",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("42.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("-42.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-42"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("0.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("round down", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("42.3"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("round up", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("42.7"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("43"))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("decimalToFloat32", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("0.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(0.0f)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("positive whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("2.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(2.0f)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("-2.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(-2.0f)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("decimalToFloat64", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("0.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("positive whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("2.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(2.0)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.decimalToFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("-2.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-2.0)))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("float32ToDecimal", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float32ToDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(0.0f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("0.0")))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("positive whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float32ToDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(2.0f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("2.0")))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float32ToDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(-2.0f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("-2.0")))))),
   None, Seq()))), hydra.testing.TestGroup("float64ToDecimal", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float64ToDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("0.0")))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("positive whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float64ToDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(2.0))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("2.0")))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.float64ToDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-2.0))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("-2.0")))))),
   None, Seq()))), hydra.testing.TestGroup("showDecimal", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("0.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0.0"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("positive whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("42.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("42.0"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("-42.0"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("-42.0"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("positive fraction", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("3.14"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("3.14"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative fraction", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("-2.5"))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("-2.5"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("readDecimal", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("3.14")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("3.14")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("0")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("0.0")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("-42")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.decimal(BigDecimal("-42.0")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readDecimal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))),
   None, Seq()))), hydra.testing.TestGroup("showInt8", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showInt8"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(42.toByte))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("42"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showInt8"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(-42.toByte))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("-42"))))),
   None, Seq()))), hydra.testing.TestGroup("showInt16", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showInt16"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(1000.toShort))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("1000"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showInt16"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(-1000.toShort))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("-1000"))))),
   None, Seq()))), hydra.testing.TestGroup("showInt32", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showInt32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("42"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showInt32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(-42))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("-42"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showInt32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0"))))),
   None, Seq()))), hydra.testing.TestGroup("showInt64", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showInt64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(1000000L))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("1000000"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showInt64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(-1000000L))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("-1000000"))))),
   None, Seq()))), hydra.testing.TestGroup("showUint8", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showUint8"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(0.toByte))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("max value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showUint8"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(-1.toByte))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("255"))))),
   None, Seq()))), hydra.testing.TestGroup("showUint16", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showUint16"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(0))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("typical value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showUint16"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(1000))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("1000"))))),
   None, Seq()))), hydra.testing.TestGroup("showUint32", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showUint32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(0L))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("typical value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showUint32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(100000L))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("100000"))))),
   None, Seq()))), hydra.testing.TestGroup("showUint64", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showUint64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(BigInt("0")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("typical value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showUint64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(BigInt("1000000")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("1000000"))))),
   None, Seq()))), hydra.testing.TestGroup("showBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("42"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-42")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("-42"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBigint"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0")))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0"))))),
   None, Seq()))), hydra.testing.TestGroup("showFloat32", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(3.14f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("3.14"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(-2.5f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("-2.5"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(0.0f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0.0"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("small positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(5.0e-2f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("5.0e-2"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("small positive 2", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(3.0e-2f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("3.0e-2"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("very small", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(1.0e-3f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("1.0e-3"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("normal decimal", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat32"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(0.1f))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0.1"))))),
   None, Seq()))), hydra.testing.TestGroup("showFloat64", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(3.14159))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("3.14159"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0.0"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("small positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(5.0e-2))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("5.0e-2"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("small positive 2", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(3.0e-2))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("3.0e-2"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("very small", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(1.0e-3))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("1.0e-3"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("normal decimal", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showFloat64"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.1))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0.1"))))),
   None, Seq()))), hydra.testing.TestGroup("showBigfloat", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(3.14)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("3.14"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(0.0)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0.0"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("small positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(5.0e-2)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("5.0e-2"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("small positive 2", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(3.0e-2)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("3.0e-2"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("very small", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(1.0e-3)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("1.0e-3"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("normal decimal", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBigfloat"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(0.1)))))))),
   hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("0.1"))))),
   None, Seq()))), hydra.testing.TestGroup("showBoolean", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("true",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBoolean"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.boolean(true)))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("true"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("false", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showBoolean"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.boolean(false)))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("false"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()))), hydra.testing.TestGroup("showString", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("simple",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("hello")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"hello\""))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("empty", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\""))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("latin accented", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("caf\u00E9")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"caf\\233\""))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("greek lambda", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("\u03BB")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\955\""))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("mixed ascii and non-ascii", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("A\u00E9B")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"A\\233B\""))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("tab", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("\t")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\t\""))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("newline", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("\n")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\n\""))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("carriage return", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("\r")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\r\""))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   None, Seq()), hydra.testing.TestCaseWithMetadata("backslash", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term.literal(hydra.core.Literal.string("\\")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\\\\""))))), None, Seq()), hydra.testing.TestCaseWithMetadata("double quote", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"), hydra.core.Term.literal(hydra.core.Literal.string("\"")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\\"\""))))), None, Seq()), hydra.testing.TestCaseWithMetadata("null", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"), hydra.core.Term.literal(hydra.core.Literal.string("\u0000")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\NUL\""))))), None, Seq()), hydra.testing.TestCaseWithMetadata("bell", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"), hydra.core.Term.literal(hydra.core.Literal.string("\u0007")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\a\""))))), None, Seq()), hydra.testing.TestCaseWithMetadata("backspace", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"), hydra.core.Term.literal(hydra.core.Literal.string("\b")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\b\""))))), None, Seq()), hydra.testing.TestCaseWithMetadata("form feed", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"), hydra.core.Term.literal(hydra.core.Literal.string("\f")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\f\""))))), None, Seq()), hydra.testing.TestCaseWithMetadata("vertical tab", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"), hydra.core.Term.literal(hydra.core.Literal.string("\u000B")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\v\""))))), None, Seq()), hydra.testing.TestCaseWithMetadata("delete", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.showString"), hydra.core.Term.literal(hydra.core.Literal.string("\u007F")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("\"\\DEL\""))))), None, Seq()))), hydra.testing.TestGroup("readInt8", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt8"), hydra.core.Term.literal(hydra.core.Literal.string("42")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(42.toByte)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt8"), hydra.core.Term.literal(hydra.core.Literal.string("-42")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(-42.toByte)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("max value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt8"), hydra.core.Term.literal(hydra.core.Literal.string("127")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(127.toByte)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("min value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt8"), hydra.core.Term.literal(hydra.core.Literal.string("-128")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(-128.toByte)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt8"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()), hydra.testing.TestCaseWithMetadata("overflow", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt8"), hydra.core.Term.literal(hydra.core.Literal.string("128")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readInt16", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt16"), hydra.core.Term.literal(hydra.core.Literal.string("1000")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(1000.toShort)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt16"), hydra.core.Term.literal(hydra.core.Literal.string("-1000")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(-1000.toShort)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt16"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readInt32", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt32"), hydra.core.Term.literal(hydra.core.Literal.string("42")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt32"), hydra.core.Term.literal(hydra.core.Literal.string("-42")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(-42)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt32"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readInt64", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt64"), hydra.core.Term.literal(hydra.core.Literal.string("1000000")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(1000000L)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt64"), hydra.core.Term.literal(hydra.core.Literal.string("-1000000")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(-1000000L)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readInt64"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readUint8", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint8"), hydra.core.Term.literal(hydra.core.Literal.string("0")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(0.toByte)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("typical", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint8"), hydra.core.Term.literal(hydra.core.Literal.string("100")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(100.toByte)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("max value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint8"), hydra.core.Term.literal(hydra.core.Literal.string("255")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(-1.toByte)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint8"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint8"), hydra.core.Term.literal(hydra.core.Literal.string("-1")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readUint16", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint16"), hydra.core.Term.literal(hydra.core.Literal.string("0")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(0)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("typical", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint16"), hydra.core.Term.literal(hydra.core.Literal.string("1000")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(1000)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint16"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint16"), hydra.core.Term.literal(hydra.core.Literal.string("-1")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readUint32", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint32"), hydra.core.Term.literal(hydra.core.Literal.string("0")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(0L)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("typical", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint32"), hydra.core.Term.literal(hydra.core.Literal.string("100000")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(100000L)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint32"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint32"), hydra.core.Term.literal(hydra.core.Literal.string("-1")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readUint64", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint64"), hydra.core.Term.literal(hydra.core.Literal.string("0")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(BigInt("0"))))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("typical", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint64"), hydra.core.Term.literal(hydra.core.Literal.string("1000000")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(BigInt("1000000"))))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint64"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readUint64"), hydra.core.Term.literal(hydra.core.Literal.string("-1")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readBigint", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readBigint"), hydra.core.Term.literal(hydra.core.Literal.string("42")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("42"))))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readBigint"), hydra.core.Term.literal(hydra.core.Literal.string("-42")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("-42"))))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("zero", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readBigint"), hydra.core.Term.literal(hydra.core.Literal.string("0")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("0"))))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("large", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readBigint"), hydra.core.Term.literal(hydra.core.Literal.string("123456789012345678901234567890")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(BigInt("123456789012345678901234567890"))))))))), None, Seq("disabled")), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readBigint"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readFloat32", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readFloat32"), hydra.core.Term.literal(hydra.core.Literal.string("3.14")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(3.14f)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readFloat32"), hydra.core.Term.literal(hydra.core.Literal.string("-2.5")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(-2.5f)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readFloat32"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readFloat64", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readFloat64"), hydra.core.Term.literal(hydra.core.Literal.string("3.14159")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(3.14159)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("negative", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readFloat64"), hydra.core.Term.literal(hydra.core.Literal.string("-2.71828")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-2.71828)))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readFloat64"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readBigfloat", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("positive", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readBigfloat"), hydra.core.Term.literal(hydra.core.Literal.string("3.14")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(BigDecimal(3.14))))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readBigfloat"), hydra.core.Term.literal(hydra.core.Literal.string("abc")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readBoolean", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("true", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readBoolean"), hydra.core.Term.literal(hydra.core.Literal.string("true")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.boolean(true))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("false", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readBoolean"), hydra.core.Term.literal(hydra.core.Literal.string("false")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.boolean(false))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("invalid", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readBoolean"), hydra.core.Term.literal(hydra.core.Literal.string("yes")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("readString", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("quoted string", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readString"), hydra.core.Term.literal(hydra.core.Literal.string("\"hello\"")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.string("hello"))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("empty quoted", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readString"), hydra.core.Term.literal(hydra.core.Literal.string("\"\"")))))), hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.string(""))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("unquoted", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.readString"), hydra.core.Term.literal(hydra.core.Literal.string("hello")))))), hydra.show.core.term(hydra.core.Term.maybe(None)))), None, Seq()))), hydra.testing.TestGroup("stringToBinary", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("simple base64", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.stringToBinary"), hydra.core.Term.literal(hydra.core.Literal.string("aGVsbG8=")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.binary("aGVsbG8="))))), None, Seq()), hydra.testing.TestCaseWithMetadata("empty string", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.stringToBinary"), hydra.core.Term.literal(hydra.core.Literal.string("")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.binary(""))))), None, Seq()))), hydra.testing.TestGroup("binaryToString", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("simple binary", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.binaryToString"), hydra.core.Term.literal(hydra.core.Literal.binary("aGVsbG8=")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("aGVsbG8="))))), None, Seq()), hydra.testing.TestCaseWithMetadata("empty binary", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Term, scala.Predef.String]((e: hydra.errors.Error) => "<<eval error>>")((t: hydra.core.Term) => hydra.show.core.term(t))(hydra.reduction.reduceTerm(hydra.test.testGraph.testContext)(hydra.test.testGraph.testGraph)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.literals.binaryToString"), hydra.core.Term.literal(hydra.core.Literal.binary("")))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string(""))))), None, Seq())))), Seq())
