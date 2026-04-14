package hydra.decode.testing

import hydra.core.*

import hydra.errors.*

import hydra.testing.*

def tag(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.Tag] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.testing.Tag]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String,
     hydra.testing.Tag, hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[hydra.errors.DecodingError,
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def testCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TestCase] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.testing.TestCase]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.testing.TestCase])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.testing.TestCase]](Seq(Tuple2("universal", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.UniversalTestCase, hydra.testing.TestCase,
         hydra.errors.DecodingError]((t: hydra.testing.UniversalTestCase) => hydra.testing.TestCase.universal(t))(hydra.decode.testing.universalTestCase(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.testing.TestCase],
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TestCase]](Left(hydra.lib.strings.cat(Seq("no such field ",
       fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.testing.TestCase])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TestCase]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def testCaseWithMetadata(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.testing.TestCaseWithMetadata] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.testing.TestCaseWithMetadata]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.TestCaseWithMetadata](hydra.extract.core.requireField("name")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_name: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.testing.TestCase, hydra.testing.TestCaseWithMetadata](hydra.extract.core.requireField("case")(hydra.decode.testing.testCase)(fieldMap)(cx))((field_case: hydra.testing.TestCase) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[scala.Predef.String],
         hydra.testing.TestCaseWithMetadata](hydra.extract.core.requireField("description")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMaybe((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_description: Option[scala.Predef.String]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.testing.Tag], hydra.testing.TestCaseWithMetadata](hydra.extract.core.requireField("tags")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.testing.tag)(v1)(v2))(fieldMap)(cx))((field_tags: Seq[hydra.testing.Tag]) =>
      Right(hydra.testing.TestCaseWithMetadata(field_name, field_case, field_description, field_tags))))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def testGroup(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.testing.TestGroup] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.testing.TestGroup]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.TestGroup](hydra.extract.core.requireField("name")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_name: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[scala.Predef.String],
         hydra.testing.TestGroup](hydra.extract.core.requireField("description")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMaybe((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_description: Option[scala.Predef.String]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.testing.TestGroup],
         hydra.testing.TestGroup](hydra.extract.core.requireField("subgroups")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.testing.testGroup)(v1)(v2))(fieldMap)(cx))((field_subgroups: Seq[hydra.testing.TestGroup]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.testing.TestCaseWithMetadata],
         hydra.testing.TestGroup](hydra.extract.core.requireField("cases")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.testing.testCaseWithMetadata)(v1)(v2))(fieldMap)(cx))((field_cases: Seq[hydra.testing.TestCaseWithMetadata]) =>
      Right(hydra.testing.TestGroup(field_name, field_description, field_subgroups, field_cases))))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def universalTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.testing.UniversalTestCase] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.testing.UniversalTestCase]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.UniversalTestCase](hydra.extract.core.requireField("actual")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_actual: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.UniversalTestCase](hydra.extract.core.requireField("expected")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_expected: scala.Predef.String) =>
      Right(hydra.testing.UniversalTestCase(field_actual, field_expected))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
