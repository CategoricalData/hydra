package hydra.decode.typing

import hydra.core.*

import hydra.error.*

import hydra.typing.*

import hydra.lib.eithers

def functionStructure[T0](env: (hydra.graph.Graph => hydra.core.Term => Either[hydra.error.DecodingError,
   T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.typing.FunctionStructure[T0]] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.typing.FunctionStructure[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, Seq[hydra.core.Name], hydra.typing.FunctionStructure[T0]](hydra.extract.helpers.requireField("typeParams")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_typeParams: Seq[hydra.core.Name]) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.core.Name], hydra.typing.FunctionStructure[T0]](hydra.extract.helpers.requireField("params")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_params: Seq[hydra.core.Name]) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.core.Binding], hydra.typing.FunctionStructure[T0]](hydra.extract.helpers.requireField("bindings")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.binding)(v1)(v2))(fieldMap)(cx))((field_bindings: Seq[hydra.core.Binding]) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.typing.FunctionStructure[T0]](hydra.extract.helpers.requireField("body")(hydra.decode.core.term)(fieldMap)(cx))((field_body: hydra.core.Term) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.core.Type], hydra.typing.FunctionStructure[T0]](hydra.extract.helpers.requireField("domains")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.`type`)(v1)(v2))(fieldMap)(cx))((field_domains: Seq[hydra.core.Type]) =>
      eithers.bind[hydra.error.DecodingError, Option[hydra.core.Type], hydra.typing.FunctionStructure[T0]](hydra.extract.helpers.requireField("codomain")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe(hydra.decode.core.`type`)(v1)(v2))(fieldMap)(cx))((field_codomain: Option[hydra.core.Type]) =>
      eithers.bind[hydra.error.DecodingError, T0, hydra.typing.FunctionStructure[T0]](hydra.extract.helpers.requireField("environment")(env)(fieldMap)(cx))((field_environment: T0) =>
      Right(hydra.typing.FunctionStructure(field_typeParams, field_params, field_bindings, field_body, field_domains, field_codomain, field_environment)))))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def inferenceResult(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.typing.InferenceResult] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.typing.InferenceResult]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.typing.InferenceResult](hydra.extract.helpers.requireField("term")(hydra.decode.core.term)(fieldMap)(cx))((field_term: hydra.core.Term) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.typing.InferenceResult](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      eithers.bind[hydra.error.DecodingError, hydra.typing.TypeSubst, hydra.typing.InferenceResult](hydra.extract.helpers.requireField("subst")(hydra.decode.typing.typeSubst)(fieldMap)(cx))((field_subst: hydra.typing.TypeSubst) =>
      eithers.bind[hydra.error.DecodingError, Map[hydra.core.Name, hydra.core.TypeVariableMetadata], hydra.typing.InferenceResult](hydra.extract.helpers.requireField("classConstraints")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMap(hydra.decode.core.name)(hydra.decode.core.typeVariableMetadata)(v1)(v2))(fieldMap)(cx))((field_classConstraints: Map[hydra.core.Name,
         hydra.core.TypeVariableMetadata]) =>
      eithers.bind[hydra.error.DecodingError, hydra.context.Context, hydra.typing.InferenceResult](hydra.extract.helpers.requireField("context")(hydra.decode.context.context)(fieldMap)(cx))((field_context: hydra.context.Context) =>
      Right(hydra.typing.InferenceResult(field_term, field_type, field_subst, field_classConstraints, field_context)))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def termSubst(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.typing.TermSubst] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.typing.TermSubst]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => eithers.map[Map[hydra.core.Name, hydra.core.Term],
     hydra.typing.TermSubst, hydra.error.DecodingError]((b: Map[hydra.core.Name, hydra.core.Term]) => b)(hydra.extract.helpers.decodeMap(hydra.decode.core.name)(hydra.decode.core.term)(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeConstraint(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.typing.TypeConstraint] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.typing.TypeConstraint]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.typing.TypeConstraint](hydra.extract.helpers.requireField("left")(hydra.decode.core.`type`)(fieldMap)(cx))((field_left: hydra.core.Type) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.typing.TypeConstraint](hydra.extract.helpers.requireField("right")(hydra.decode.core.`type`)(fieldMap)(cx))((field_right: hydra.core.Type) =>
      eithers.bind[hydra.error.DecodingError, scala.Predef.String, hydra.typing.TypeConstraint](hydra.extract.helpers.requireField("comment")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_comment: scala.Predef.String) =>
      Right(hydra.typing.TypeConstraint(field_left, field_right, field_comment)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeSubst(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.typing.TypeSubst] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.typing.TypeSubst]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => eithers.map[Map[hydra.core.Name, hydra.core.Type],
     hydra.typing.TypeSubst, hydra.error.DecodingError]((b: Map[hydra.core.Name, hydra.core.Type]) => b)(hydra.extract.helpers.decodeMap(hydra.decode.core.name)(hydra.decode.core.`type`)(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
