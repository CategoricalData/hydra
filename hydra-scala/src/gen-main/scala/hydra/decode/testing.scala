package hydra.decode.testing

import hydra.core.*

import hydra.errors.*

import hydra.testing.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def alphaConversionTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.AlphaConversionTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.AlphaConversionTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.AlphaConversionTestCase](hydra.extract.helpers.requireField("term")(hydra.decode.core.term)(fieldMap)(cx))((field_term: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.testing.AlphaConversionTestCase](hydra.extract.helpers.requireField("oldVariable")(hydra.decode.core.name)(fieldMap)(cx))((field_oldVariable: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.testing.AlphaConversionTestCase](hydra.extract.helpers.requireField("newVariable")(hydra.decode.core.name)(fieldMap)(cx))((field_newVariable: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.AlphaConversionTestCase](hydra.extract.helpers.requireField("result")(hydra.decode.core.term)(fieldMap)(cx))((field_result: hydra.core.Term) =>
      Right(hydra.testing.AlphaConversionTestCase(field_term, field_oldVariable, field_newVariable, field_result))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def evaluationStyle(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.EvaluationStyle] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.EvaluationStyle]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.EvaluationStyle])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.EvaluationStyle]](Seq(Tuple2("eager", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.EvaluationStyle, hydra.errors.DecodingError]((t: Unit) => hydra.testing.EvaluationStyle.eager(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("lazy", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.EvaluationStyle, hydra.errors.DecodingError]((t: Unit) => hydra.testing.EvaluationStyle.`lazy`(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.testing.EvaluationStyle], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.EvaluationStyle]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.EvaluationStyle])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.EvaluationStyle]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def caseConversionTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.CaseConversionTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.CaseConversionTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.util.CaseConvention, hydra.testing.CaseConversionTestCase](hydra.extract.helpers.requireField("fromConvention")(hydra.decode.util.caseConvention)(fieldMap)(cx))((field_fromConvention: hydra.util.CaseConvention) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.util.CaseConvention, hydra.testing.CaseConversionTestCase](hydra.extract.helpers.requireField("toConvention")(hydra.decode.util.caseConvention)(fieldMap)(cx))((field_toConvention: hydra.util.CaseConvention) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.CaseConversionTestCase](hydra.extract.helpers.requireField("fromString")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_fromString: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.CaseConversionTestCase](hydra.extract.helpers.requireField("toString")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_toString: scala.Predef.String) =>
      Right(hydra.testing.CaseConversionTestCase(field_fromConvention, field_toConvention, field_fromString, field_toString))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def delegatedEvaluationTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.DelegatedEvaluationTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.DelegatedEvaluationTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.DelegatedEvaluationTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.DelegatedEvaluationTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.DelegatedEvaluationTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def etaExpansionTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.EtaExpansionTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.EtaExpansionTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.EtaExpansionTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.EtaExpansionTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.EtaExpansionTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def deannotateTermTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.DeannotateTermTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.DeannotateTermTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.DeannotateTermTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.DeannotateTermTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.DeannotateTermTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def deannotateTypeTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.DeannotateTypeTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.DeannotateTypeTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.DeannotateTypeTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.`type`)(fieldMap)(cx))((field_input: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.DeannotateTypeTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.`type`)(fieldMap)(cx))((field_output: hydra.core.Type) =>
      Right(hydra.testing.DeannotateTypeTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def flattenLetTermsTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.FlattenLetTermsTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.FlattenLetTermsTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.FlattenLetTermsTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.FlattenLetTermsTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.FlattenLetTermsTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def foldOperation(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.FoldOperation] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.FoldOperation]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.FoldOperation])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.FoldOperation]](Seq(Tuple2("sumInt32Literals", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.FoldOperation, hydra.errors.DecodingError]((t: Unit) => hydra.testing.FoldOperation.sumInt32Literals(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("collectListLengths", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.FoldOperation, hydra.errors.DecodingError]((t: Unit) => hydra.testing.FoldOperation.collectListLengths(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("collectLabels", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.FoldOperation, hydra.errors.DecodingError]((t: Unit) => hydra.testing.FoldOperation.collectLabels(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.testing.FoldOperation], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.FoldOperation]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.FoldOperation])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.FoldOperation]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def foldOverTermTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.FoldOverTermTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.FoldOverTermTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.FoldOverTermTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.coders.TraversalOrder, hydra.testing.FoldOverTermTestCase](hydra.extract.helpers.requireField("traversalOrder")(hydra.decode.coders.traversalOrder)(fieldMap)(cx))((field_traversalOrder: hydra.coders.TraversalOrder) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.testing.FoldOperation, hydra.testing.FoldOverTermTestCase](hydra.extract.helpers.requireField("operation")(hydra.decode.testing.foldOperation)(fieldMap)(cx))((field_operation: hydra.testing.FoldOperation) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.FoldOverTermTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.FoldOverTermTestCase(field_input, field_traversalOrder, field_operation, field_output))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def freeVariablesTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.FreeVariablesTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.FreeVariablesTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.FreeVariablesTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.collection.immutable.Set[hydra.core.Name], hydra.testing.FreeVariablesTestCase](hydra.extract.helpers.requireField("output")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeSet(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_output: scala.collection.immutable.Set[hydra.core.Name]) =>
      Right(hydra.testing.FreeVariablesTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def hoistPredicate(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.HoistPredicate] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.HoistPredicate]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.HoistPredicate])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.HoistPredicate]](Seq(Tuple2("caseStatements", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.HoistPredicate, hydra.errors.DecodingError]((t: Unit) => hydra.testing.HoistPredicate.caseStatements(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("applications", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.HoistPredicate, hydra.errors.DecodingError]((t: Unit) => hydra.testing.HoistPredicate.applications(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("lists", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.HoistPredicate, hydra.errors.DecodingError]((t: Unit) => hydra.testing.HoistPredicate.lists(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("nothing", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.HoistPredicate, hydra.errors.DecodingError]((t: Unit) => hydra.testing.HoistPredicate.nothing(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.testing.HoistPredicate], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.HoistPredicate]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.HoistPredicate])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.HoistPredicate]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def hoistLetBindingsTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.HoistLetBindingsTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.HoistLetBindingsTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Let, hydra.testing.HoistLetBindingsTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.let)(fieldMap)(cx))((field_input: hydra.core.Let) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Let, hydra.testing.HoistLetBindingsTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.let)(fieldMap)(cx))((field_output: hydra.core.Let) =>
      Right(hydra.testing.HoistLetBindingsTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def hoistPolymorphicLetBindingsTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Let, hydra.testing.HoistPolymorphicLetBindingsTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.let)(fieldMap)(cx))((field_input: hydra.core.Let) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Let, hydra.testing.HoistPolymorphicLetBindingsTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.let)(fieldMap)(cx))((field_output: hydra.core.Let) =>
      Right(hydra.testing.HoistPolymorphicLetBindingsTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def hoistSubtermsTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.HoistSubtermsTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.HoistSubtermsTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.testing.HoistPredicate, hydra.testing.HoistSubtermsTestCase](hydra.extract.helpers.requireField("predicate")(hydra.decode.testing.hoistPredicate)(fieldMap)(cx))((field_predicate: hydra.testing.HoistPredicate) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.HoistSubtermsTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.HoistSubtermsTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.HoistSubtermsTestCase(field_predicate, field_input, field_output)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def hoistCaseStatementsTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.HoistCaseStatementsTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.HoistCaseStatementsTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.HoistCaseStatementsTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.HoistCaseStatementsTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.HoistCaseStatementsTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def termRewriter(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TermRewriter] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TermRewriter]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.TermRewriter])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TermRewriter]](Seq(Tuple2("replaceFooWithBar", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.TermRewriter, hydra.errors.DecodingError]((t: Unit) => hydra.testing.TermRewriter.replaceFooWithBar(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("replaceInt32WithInt64", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.TermRewriter, hydra.errors.DecodingError]((t: Unit) => hydra.testing.TermRewriter.replaceInt32WithInt64(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.testing.TermRewriter], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TermRewriter]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.TermRewriter])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TermRewriter]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def rewriteTermTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.RewriteTermTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.RewriteTermTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.RewriteTermTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.testing.TermRewriter, hydra.testing.RewriteTermTestCase](hydra.extract.helpers.requireField("rewriter")(hydra.decode.testing.termRewriter)(fieldMap)(cx))((field_rewriter: hydra.testing.TermRewriter) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.RewriteTermTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.RewriteTermTestCase(field_input, field_rewriter, field_output)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeRewriter(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TypeRewriter] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TypeRewriter]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.TypeRewriter])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TypeRewriter]](Seq(Tuple2("replaceStringWithInt32", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.testing.TypeRewriter, hydra.errors.DecodingError]((t: Unit) => hydra.testing.TypeRewriter.replaceStringWithInt32(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.testing.TypeRewriter], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TypeRewriter]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.TypeRewriter])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TypeRewriter]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def rewriteTypeTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.RewriteTypeTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.RewriteTypeTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.RewriteTypeTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.`type`)(fieldMap)(cx))((field_input: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.testing.TypeRewriter, hydra.testing.RewriteTypeTestCase](hydra.extract.helpers.requireField("rewriter")(hydra.decode.testing.typeRewriter)(fieldMap)(cx))((field_rewriter: hydra.testing.TypeRewriter) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.RewriteTypeTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.`type`)(fieldMap)(cx))((field_output: hydra.core.Type) =>
      Right(hydra.testing.RewriteTypeTestCase(field_input, field_rewriter, field_output)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def evaluationTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.EvaluationTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.EvaluationTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.testing.EvaluationStyle, hydra.testing.EvaluationTestCase](hydra.extract.helpers.requireField("evaluationStyle")(hydra.decode.testing.evaluationStyle)(fieldMap)(cx))((field_evaluationStyle: hydra.testing.EvaluationStyle) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.EvaluationTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.EvaluationTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.EvaluationTestCase(field_evaluationStyle, field_input, field_output)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def inferenceFailureTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.InferenceFailureTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.InferenceFailureTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.InferenceFailureTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) => Right(hydra.testing.InferenceFailureTestCase(field_input)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def inferenceTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.InferenceTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.InferenceTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.InferenceTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.TypeScheme, hydra.testing.InferenceTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.typeScheme)(fieldMap)(cx))((field_output: hydra.core.TypeScheme) =>
      Right(hydra.testing.InferenceTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def jsonDecodeTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.JsonDecodeTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.JsonDecodeTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.JsonDecodeTestCase](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.json.model.Value, hydra.testing.JsonDecodeTestCase](hydra.extract.helpers.requireField("json")(hydra.decode.json.model.value)(fieldMap)(cx))((field_json: hydra.json.model.Value) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Either[scala.Predef.String, hydra.core.Term], hydra.testing.JsonDecodeTestCase](hydra.extract.helpers.requireField("expected")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeEither((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(hydra.decode.core.term)(v1)(v2))(fieldMap)(cx))((field_expected: Either[scala.Predef.String, hydra.core.Term]) =>
      Right(hydra.testing.JsonDecodeTestCase(field_type, field_json, field_expected)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def jsonEncodeTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.JsonEncodeTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.JsonEncodeTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.JsonEncodeTestCase](hydra.extract.helpers.requireField("term")(hydra.decode.core.term)(fieldMap)(cx))((field_term: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Either[scala.Predef.String, hydra.json.model.Value], hydra.testing.JsonEncodeTestCase](hydra.extract.helpers.requireField("expected")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeEither((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(hydra.decode.json.model.value)(v1)(v2))(fieldMap)(cx))((field_expected: Either[scala.Predef.String, hydra.json.model.Value]) =>
      Right(hydra.testing.JsonEncodeTestCase(field_term, field_expected))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def jsonParserTestCase(v1: hydra.graph.Graph)(v2: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.ParserTestCase[hydra.json.model.Value]] = hydra.decode.testing.parserTestCase(hydra.decode.json.model.value)(v1)(v2)

def jsonRoundtripTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.JsonRoundtripTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.JsonRoundtripTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.JsonRoundtripTestCase](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.JsonRoundtripTestCase](hydra.extract.helpers.requireField("term")(hydra.decode.core.term)(fieldMap)(cx))((field_term: hydra.core.Term) =>
      Right(hydra.testing.JsonRoundtripTestCase(field_type, field_term))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def liftLambdaAboveLetTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.LiftLambdaAboveLetTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.LiftLambdaAboveLetTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.LiftLambdaAboveLetTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def jsonWriterTestCase(v1: hydra.graph.Graph)(v2: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.WriterTestCase[hydra.json.model.Value]] = hydra.decode.testing.writerTestCase(hydra.decode.json.model.value)(v1)(v2)

def parserTestCase[T0](a: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.ParserTestCase[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.ParserTestCase[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.ParserTestCase[T0]](hydra.extract.helpers.requireField("input")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_input: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.parsing.ParseResult[T0], hydra.testing.ParserTestCase[T0]](hydra.extract.helpers.requireField("output")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) => hydra.decode.parsing.parseResult(a)(v1)(v2))(fieldMap)(cx))((field_output: hydra.parsing.ParseResult[T0]) =>
      Right(hydra.testing.ParserTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def tag(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.Tag] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.Tag]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.testing.Tag, hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def testCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.TestCase])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TestCase]](Seq(Tuple2("alphaConversion", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.AlphaConversionTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.AlphaConversionTestCase) => hydra.testing.TestCase.alphaConversion(t))(hydra.decode.testing.alphaConversionTestCase(cx)(input))), Tuple2("caseConversion", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.CaseConversionTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.CaseConversionTestCase) => hydra.testing.TestCase.caseConversion(t))(hydra.decode.testing.caseConversionTestCase(cx)(input))), Tuple2("deannotateTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.DeannotateTermTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.DeannotateTermTestCase) => hydra.testing.TestCase.deannotateTerm(t))(hydra.decode.testing.deannotateTermTestCase(cx)(input))), Tuple2("deannotateType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.DeannotateTypeTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.DeannotateTypeTestCase) => hydra.testing.TestCase.deannotateType(t))(hydra.decode.testing.deannotateTypeTestCase(cx)(input))), Tuple2("delegatedEvaluation", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.DelegatedEvaluationTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.DelegatedEvaluationTestCase) => hydra.testing.TestCase.delegatedEvaluation(t))(hydra.decode.testing.delegatedEvaluationTestCase(cx)(input))), Tuple2("etaExpansion", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.EtaExpansionTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.EtaExpansionTestCase) => hydra.testing.TestCase.etaExpansion(t))(hydra.decode.testing.etaExpansionTestCase(cx)(input))), Tuple2("flattenLetTerms", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.FlattenLetTermsTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.FlattenLetTermsTestCase) => hydra.testing.TestCase.flattenLetTerms(t))(hydra.decode.testing.flattenLetTermsTestCase(cx)(input))), Tuple2("freeVariables", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.FreeVariablesTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.FreeVariablesTestCase) => hydra.testing.TestCase.freeVariables(t))(hydra.decode.testing.freeVariablesTestCase(cx)(input))), Tuple2("evaluation", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.EvaluationTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.EvaluationTestCase) => hydra.testing.TestCase.evaluation(t))(hydra.decode.testing.evaluationTestCase(cx)(input))), Tuple2("inference", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.InferenceTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.InferenceTestCase) => hydra.testing.TestCase.inference(t))(hydra.decode.testing.inferenceTestCase(cx)(input))), Tuple2("inferenceFailure", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.InferenceFailureTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.InferenceFailureTestCase) => hydra.testing.TestCase.inferenceFailure(t))(hydra.decode.testing.inferenceFailureTestCase(cx)(input))), Tuple2("jsonDecode", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.JsonDecodeTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.JsonDecodeTestCase) => hydra.testing.TestCase.jsonDecode(t))(hydra.decode.testing.jsonDecodeTestCase(cx)(input))), Tuple2("jsonEncode", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.JsonEncodeTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.JsonEncodeTestCase) => hydra.testing.TestCase.jsonEncode(t))(hydra.decode.testing.jsonEncodeTestCase(cx)(input))), Tuple2("jsonParser", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.ParserTestCase[hydra.json.model.Value], hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.ParserTestCase[hydra.json.model.Value]) => hydra.testing.TestCase.jsonParser(t))(hydra.decode.testing.jsonParserTestCase(cx)(input))), Tuple2("jsonRoundtrip", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.JsonRoundtripTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.JsonRoundtripTestCase) => hydra.testing.TestCase.jsonRoundtrip(t))(hydra.decode.testing.jsonRoundtripTestCase(cx)(input))), Tuple2("jsonWriter", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.WriterTestCase[hydra.json.model.Value], hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.WriterTestCase[hydra.json.model.Value]) => hydra.testing.TestCase.jsonWriter(t))(hydra.decode.testing.jsonWriterTestCase(cx)(input))), Tuple2("liftLambdaAboveLet", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.LiftLambdaAboveLetTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.LiftLambdaAboveLetTestCase) => hydra.testing.TestCase.liftLambdaAboveLet(t))(hydra.decode.testing.liftLambdaAboveLetTestCase(cx)(input))), Tuple2("serialization", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.SerializationTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.SerializationTestCase) => hydra.testing.TestCase.serialization(t))(hydra.decode.testing.serializationTestCase(cx)(input))), Tuple2("simplifyTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.SimplifyTermTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.SimplifyTermTestCase) => hydra.testing.TestCase.simplifyTerm(t))(hydra.decode.testing.simplifyTermTestCase(cx)(input))), Tuple2("topologicalSort", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.TopologicalSortTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.TopologicalSortTestCase) => hydra.testing.TestCase.topologicalSort(t))(hydra.decode.testing.topologicalSortTestCase(cx)(input))), Tuple2("topologicalSortBindings", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.TopologicalSortBindingsTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.TopologicalSortBindingsTestCase) => hydra.testing.TestCase.topologicalSortBindings(t))(hydra.decode.testing.topologicalSortBindingsTestCase(cx)(input))), Tuple2("topologicalSortSCC", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.TopologicalSortSCCTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.TopologicalSortSCCTestCase) => hydra.testing.TestCase.topologicalSortSCC(t))(hydra.decode.testing.topologicalSortSCCTestCase(cx)(input))), Tuple2("typeChecking", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.TypeCheckingTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.TypeCheckingTestCase) => hydra.testing.TestCase.typeChecking(t))(hydra.decode.testing.typeCheckingTestCase(cx)(input))), Tuple2("typeCheckingFailure", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.TypeCheckingFailureTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.TypeCheckingFailureTestCase) => hydra.testing.TestCase.typeCheckingFailure(t))(hydra.decode.testing.typeCheckingFailureTestCase(cx)(input))), Tuple2("typeReduction", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.TypeReductionTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.TypeReductionTestCase) => hydra.testing.TestCase.typeReduction(t))(hydra.decode.testing.typeReductionTestCase(cx)(input))), Tuple2("normalizeTypeVariables", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.NormalizeTypeVariablesTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.NormalizeTypeVariablesTestCase) => hydra.testing.TestCase.normalizeTypeVariables(t))(hydra.decode.testing.normalizeTypeVariablesTestCase(cx)(input))), Tuple2("foldOverTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.FoldOverTermTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.FoldOverTermTestCase) => hydra.testing.TestCase.foldOverTerm(t))(hydra.decode.testing.foldOverTermTestCase(cx)(input))), Tuple2("rewriteTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.RewriteTermTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.RewriteTermTestCase) => hydra.testing.TestCase.rewriteTerm(t))(hydra.decode.testing.rewriteTermTestCase(cx)(input))), Tuple2("rewriteType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.RewriteTypeTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.RewriteTypeTestCase) => hydra.testing.TestCase.rewriteType(t))(hydra.decode.testing.rewriteTypeTestCase(cx)(input))), Tuple2("hoistSubterms", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.HoistSubtermsTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.HoistSubtermsTestCase) => hydra.testing.TestCase.hoistSubterms(t))(hydra.decode.testing.hoistSubtermsTestCase(cx)(input))), Tuple2("hoistCaseStatements", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.HoistCaseStatementsTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.HoistCaseStatementsTestCase) => hydra.testing.TestCase.hoistCaseStatements(t))(hydra.decode.testing.hoistCaseStatementsTestCase(cx)(input))), Tuple2("hoistLetBindings", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.HoistLetBindingsTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.HoistLetBindingsTestCase) => hydra.testing.TestCase.hoistLetBindings(t))(hydra.decode.testing.hoistLetBindingsTestCase(cx)(input))), Tuple2("hoistPolymorphicLetBindings", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.HoistPolymorphicLetBindingsTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.HoistPolymorphicLetBindingsTestCase) => hydra.testing.TestCase.hoistPolymorphicLetBindings(t))(hydra.decode.testing.hoistPolymorphicLetBindingsTestCase(cx)(input))), Tuple2("substInType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.SubstInTypeTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.SubstInTypeTestCase) => hydra.testing.TestCase.substInType(t))(hydra.decode.testing.substInTypeTestCase(cx)(input))), Tuple2("variableOccursInType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.VariableOccursInTypeTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.VariableOccursInTypeTestCase) => hydra.testing.TestCase.variableOccursInType(t))(hydra.decode.testing.variableOccursInTypeTestCase(cx)(input))), Tuple2("unifyTypes", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.UnifyTypesTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.UnifyTypesTestCase) => hydra.testing.TestCase.unifyTypes(t))(hydra.decode.testing.unifyTypesTestCase(cx)(input))), Tuple2("joinTypes", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.JoinTypesTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.JoinTypesTestCase) => hydra.testing.TestCase.joinTypes(t))(hydra.decode.testing.joinTypesTestCase(cx)(input))), Tuple2("unshadowVariables", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.UnshadowVariablesTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.UnshadowVariablesTestCase) => hydra.testing.TestCase.unshadowVariables(t))(hydra.decode.testing.unshadowVariablesTestCase(cx)(input))), Tuple2("validateCoreTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.testing.ValidateCoreTermTestCase, hydra.testing.TestCase, hydra.errors.DecodingError]((t: hydra.testing.ValidateCoreTermTestCase) => hydra.testing.TestCase.validateCoreTerm(t))(hydra.decode.testing.validateCoreTermTestCase(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.testing.TestCase], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TestCase]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.testing.TestCase])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.testing.TestCase]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def testCaseWithMetadata(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.TestCaseWithMetadata](hydra.extract.helpers.requireField("name")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_name: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.testing.TestCase, hydra.testing.TestCaseWithMetadata](hydra.extract.helpers.requireField("case")(hydra.decode.testing.testCase)(fieldMap)(cx))((field_case: hydra.testing.TestCase) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[scala.Predef.String], hydra.testing.TestCaseWithMetadata](hydra.extract.helpers.requireField("description")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_description: Option[scala.Predef.String]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.testing.Tag], hydra.testing.TestCaseWithMetadata](hydra.extract.helpers.requireField("tags")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.testing.tag)(v1)(v2))(fieldMap)(cx))((field_tags: Seq[hydra.testing.Tag]) =>
      Right(hydra.testing.TestCaseWithMetadata(field_name, field_case, field_description, field_tags))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def testGroup(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TestGroup] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TestGroup]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.TestGroup](hydra.extract.helpers.requireField("name")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_name: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[scala.Predef.String], hydra.testing.TestGroup](hydra.extract.helpers.requireField("description")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_description: Option[scala.Predef.String]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.testing.TestGroup], hydra.testing.TestGroup](hydra.extract.helpers.requireField("subgroups")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.testing.testGroup)(v1)(v2))(fieldMap)(cx))((field_subgroups: Seq[hydra.testing.TestGroup]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.testing.TestCaseWithMetadata], hydra.testing.TestGroup](hydra.extract.helpers.requireField("cases")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.testing.testCaseWithMetadata)(v1)(v2))(fieldMap)(cx))((field_cases: Seq[hydra.testing.TestCaseWithMetadata]) =>
      Right(hydra.testing.TestGroup(field_name, field_description, field_subgroups, field_cases))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeCheckingTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TypeCheckingTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TypeCheckingTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.TypeCheckingTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.TypeCheckingTestCase](hydra.extract.helpers.requireField("outputTerm")(hydra.decode.core.term)(fieldMap)(cx))((field_outputTerm: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.TypeCheckingTestCase](hydra.extract.helpers.requireField("outputType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_outputType: hydra.core.Type) =>
      Right(hydra.testing.TypeCheckingTestCase(field_input, field_outputTerm, field_outputType)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeCheckingFailureTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TypeCheckingFailureTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TypeCheckingFailureTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.TypeCheckingFailureTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      Right(hydra.testing.TypeCheckingFailureTestCase(field_input)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def topologicalSortBindingsTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TopologicalSortBindingsTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TopologicalSortBindingsTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[Tuple2[hydra.core.Name, hydra.core.Term]], hydra.testing.TopologicalSortBindingsTestCase](hydra.extract.helpers.requireField("bindings")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodePair(hydra.decode.core.name)(hydra.decode.core.term)(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_bindings: Seq[Tuple2[hydra.core.Name, hydra.core.Term]]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[Seq[Tuple2[hydra.core.Name, hydra.core.Term]]], hydra.testing.TopologicalSortBindingsTestCase](hydra.extract.helpers.requireField("expected")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v13: hydra.graph.Graph) =>
      (v23: hydra.core.Term) =>
      hydra.extract.helpers.decodePair(hydra.decode.core.name)(hydra.decode.core.term)(v13)(v23))(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_expected: Seq[Seq[Tuple2[hydra.core.Name, hydra.core.Term]]]) =>
      Right(hydra.testing.TopologicalSortBindingsTestCase(field_bindings, field_expected))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def topologicalSortTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TopologicalSortTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TopologicalSortTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[Tuple2[Int, Seq[Int]]], hydra.testing.TopologicalSortTestCase](hydra.extract.helpers.requireField("adjacencyList")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodePair((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v13) => v_Literal_integer_v13 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))((v13: hydra.graph.Graph) =>
      (v23: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v14) => v_Literal_integer_v14 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v13)(v23))(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_adjacencyList: Seq[Tuple2[Int, Seq[Int]]]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Either[Seq[Seq[Int]], Seq[Int]], hydra.testing.TopologicalSortTestCase](hydra.extract.helpers.requireField("expected")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeEither((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v13: hydra.graph.Graph) =>
      (v23: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v14) => v_Literal_integer_v14 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v13)(v23))(v12)(v22))((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v13) => v_Literal_integer_v13 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_expected: Either[Seq[Seq[Int]], Seq[Int]]) =>
      Right(hydra.testing.TopologicalSortTestCase(field_adjacencyList, field_expected))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def topologicalSortSCCTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TopologicalSortSCCTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TopologicalSortSCCTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[Tuple2[Int, Seq[Int]]], hydra.testing.TopologicalSortSCCTestCase](hydra.extract.helpers.requireField("adjacencyList")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodePair((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v13) => v_Literal_integer_v13 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))((v13: hydra.graph.Graph) =>
      (v23: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v14) => v_Literal_integer_v14 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v13)(v23))(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_adjacencyList: Seq[Tuple2[Int, Seq[Int]]]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[Seq[Int]], hydra.testing.TopologicalSortSCCTestCase](hydra.extract.helpers.requireField("expected")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v13) => v_Literal_integer_v13 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_expected: Seq[Seq[Int]]) =>
      Right(hydra.testing.TopologicalSortSCCTestCase(field_adjacencyList, field_expected))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def serializationTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.SerializationTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.SerializationTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Expr, hydra.testing.SerializationTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.ast.expr)(fieldMap)(cx))((field_input: hydra.ast.Expr) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.SerializationTestCase](hydra.extract.helpers.requireField("output")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_output: scala.Predef.String) =>
      Right(hydra.testing.SerializationTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def simplifyTermTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.SimplifyTermTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.SimplifyTermTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.SimplifyTermTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.SimplifyTermTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.SimplifyTermTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def normalizeTypeVariablesTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.NormalizeTypeVariablesTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.NormalizeTypeVariablesTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.NormalizeTypeVariablesTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeReductionTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.TypeReductionTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.TypeReductionTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.TypeReductionTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.`type`)(fieldMap)(cx))((field_input: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.TypeReductionTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.`type`)(fieldMap)(cx))((field_output: hydra.core.Type) =>
      Right(hydra.testing.TypeReductionTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def writerTestCase[T0](a: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.WriterTestCase[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.WriterTestCase[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, T0, hydra.testing.WriterTestCase[T0]](hydra.extract.helpers.requireField("input")(a)(fieldMap)(cx))((field_input: T0) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.testing.WriterTestCase[T0]](hydra.extract.helpers.requireField("output")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_output: scala.Predef.String) =>
      Right(hydra.testing.WriterTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def substInTypeTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.SubstInTypeTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.SubstInTypeTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[Tuple2[hydra.core.Name, hydra.core.Type]], hydra.testing.SubstInTypeTestCase](hydra.extract.helpers.requireField("substitution")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodePair(hydra.decode.core.name)(hydra.decode.core.`type`)(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_substitution: Seq[Tuple2[hydra.core.Name, hydra.core.Type]]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.SubstInTypeTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.`type`)(fieldMap)(cx))((field_input: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.SubstInTypeTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.`type`)(fieldMap)(cx))((field_output: hydra.core.Type) =>
      Right(hydra.testing.SubstInTypeTestCase(field_substitution, field_input, field_output)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def variableOccursInTypeTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.VariableOccursInTypeTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.VariableOccursInTypeTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.testing.VariableOccursInTypeTestCase](hydra.extract.helpers.requireField("variable")(hydra.decode.core.name)(fieldMap)(cx))((field_variable: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.VariableOccursInTypeTestCase](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Boolean, hydra.testing.VariableOccursInTypeTestCase](hydra.extract.helpers.requireField("expected")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, Boolean]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(v_Literal_boolean_b)
        case _ => Left("expected boolean literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_expected: Boolean) =>
      Right(hydra.testing.VariableOccursInTypeTestCase(field_variable, field_type, field_expected)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unshadowVariablesTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.UnshadowVariablesTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.UnshadowVariablesTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.UnshadowVariablesTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.UnshadowVariablesTestCase](hydra.extract.helpers.requireField("output")(hydra.decode.core.term)(fieldMap)(cx))((field_output: hydra.core.Term) =>
      Right(hydra.testing.UnshadowVariablesTestCase(field_input, field_output))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unifyTypesTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.UnifyTypesTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.UnifyTypesTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.core.Name], hydra.testing.UnifyTypesTestCase](hydra.extract.helpers.requireField("schemaTypes")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_schemaTypes: Seq[hydra.core.Name]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.UnifyTypesTestCase](hydra.extract.helpers.requireField("left")(hydra.decode.core.`type`)(fieldMap)(cx))((field_left: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.UnifyTypesTestCase](hydra.extract.helpers.requireField("right")(hydra.decode.core.`type`)(fieldMap)(cx))((field_right: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Either[scala.Predef.String, hydra.typing.TypeSubst], hydra.testing.UnifyTypesTestCase](hydra.extract.helpers.requireField("expected")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeEither((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(hydra.decode.typing.typeSubst)(v1)(v2))(fieldMap)(cx))((field_expected: Either[scala.Predef.String, hydra.typing.TypeSubst]) =>
      Right(hydra.testing.UnifyTypesTestCase(field_schemaTypes, field_left, field_right, field_expected))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def joinTypesTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.JoinTypesTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.JoinTypesTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.JoinTypesTestCase](hydra.extract.helpers.requireField("left")(hydra.decode.core.`type`)(fieldMap)(cx))((field_left: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.testing.JoinTypesTestCase](hydra.extract.helpers.requireField("right")(hydra.decode.core.`type`)(fieldMap)(cx))((field_right: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Either[Unit, Seq[hydra.typing.TypeConstraint]], hydra.testing.JoinTypesTestCase](hydra.extract.helpers.requireField("expected")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeEither(hydra.extract.helpers.decodeUnit)((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.typing.typeConstraint)(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_expected: Either[Unit, Seq[hydra.typing.TypeConstraint]]) =>
      Right(hydra.testing.JoinTypesTestCase(field_left, field_right, field_expected)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def validateCoreTermTestCase(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.testing.ValidateCoreTermTestCase] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.testing.ValidateCoreTermTestCase]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Boolean, hydra.testing.ValidateCoreTermTestCase](hydra.extract.helpers.requireField("typed")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, Boolean]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(v_Literal_boolean_b)
        case _ => Left("expected boolean literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_typed: Boolean) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.testing.ValidateCoreTermTestCase](hydra.extract.helpers.requireField("input")(hydra.decode.core.term)(fieldMap)(cx))((field_input: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[hydra.error.core.InvalidTermError], hydra.testing.ValidateCoreTermTestCase](hydra.extract.helpers.requireField("output")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe(hydra.decode.error.core.invalidTermError)(v1)(v2))(fieldMap)(cx))((field_output: Option[hydra.error.core.InvalidTermError]) =>
      Right(hydra.testing.ValidateCoreTermTestCase(field_typed, field_input, field_output)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
