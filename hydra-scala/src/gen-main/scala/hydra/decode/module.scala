package hydra.decode.module

import hydra.core.*

import hydra.error.*

import hydra.module.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def definition(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.module.Definition] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.module.Definition]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.module.Definition])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.module.Definition]](Seq(Tuple2("term",
       (input: hydra.core.Term) =>
      eithers.map[hydra.module.TermDefinition, hydra.module.Definition, hydra.error.DecodingError]((t: hydra.module.TermDefinition) => hydra.module.Definition.term(t))(hydra.decode.module.termDefinition(cx)(input))),
         Tuple2("type", (input: hydra.core.Term) =>
      eithers.map[hydra.module.TypeDefinition, hydra.module.Definition, hydra.error.DecodingError]((t: hydra.module.TypeDefinition) => hydra.module.Definition.`type`(t))(hydra.decode.module.typeDefinition(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.module.Definition], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.module.Definition]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.module.Definition])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.module.Definition]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def fileExtension(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.module.FileExtension] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.module.FileExtension]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => eithers.map[scala.Predef.String, hydra.module.FileExtension,
     hydra.error.DecodingError]((b: scala.Predef.String) => b)(eithers.either[scala.Predef.String, hydra.core.Term,
     Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def module(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.module.Module] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.module.Module]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.module.Namespace, hydra.module.Module](hydra.extract.helpers.requireField("namespace")(hydra.decode.module.namespace)(fieldMap)(cx))((field_namespace: hydra.module.Namespace) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.core.Binding], hydra.module.Module](hydra.extract.helpers.requireField("elements")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.binding)(v1)(v2))(fieldMap)(cx))((field_elements: Seq[hydra.core.Binding]) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.module.Namespace], hydra.module.Module](hydra.extract.helpers.requireField("termDependencies")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.module.namespace)(v1)(v2))(fieldMap)(cx))((field_termDependencies: Seq[hydra.module.Namespace]) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.module.Namespace], hydra.module.Module](hydra.extract.helpers.requireField("typeDependencies")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.module.namespace)(v1)(v2))(fieldMap)(cx))((field_typeDependencies: Seq[hydra.module.Namespace]) =>
      eithers.bind[hydra.error.DecodingError, Option[scala.Predef.String], hydra.module.Module](hydra.extract.helpers.requireField("description")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_description: Option[scala.Predef.String]) =>
      Right(hydra.module.Module(field_namespace, field_elements, field_termDependencies, field_typeDependencies, field_description)))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def namespace(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.module.Namespace] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.module.Namespace]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => eithers.map[scala.Predef.String, hydra.module.Namespace,
     hydra.error.DecodingError]((b: scala.Predef.String) => b)(eithers.either[scala.Predef.String, hydra.core.Term,
     Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def namespaces[T0](n: (hydra.graph.Graph => hydra.core.Term => Either[hydra.error.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError,
   hydra.module.Namespaces[T0]] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.module.Namespaces[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, Tuple2[hydra.module.Namespace, T0], hydra.module.Namespaces[T0]](hydra.extract.helpers.requireField("focus")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodePair(hydra.decode.module.namespace)(n)(v1)(v2))(fieldMap)(cx))((field_focus: Tuple2[hydra.module.Namespace, T0]) =>
      eithers.bind[hydra.error.DecodingError, Map[hydra.module.Namespace, T0], hydra.module.Namespaces[T0]](hydra.extract.helpers.requireField("mapping")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMap(hydra.decode.module.namespace)(n)(v1)(v2))(fieldMap)(cx))((field_mapping: Map[hydra.module.Namespace,
         T0]) => Right(hydra.module.Namespaces(field_focus, field_mapping))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def qualifiedName(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.module.QualifiedName] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.module.QualifiedName]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, Option[hydra.module.Namespace], hydra.module.QualifiedName](hydra.extract.helpers.requireField("namespace")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe(hydra.decode.module.namespace)(v1)(v2))(fieldMap)(cx))((field_namespace: Option[hydra.module.Namespace]) =>
      eithers.bind[hydra.error.DecodingError, scala.Predef.String, hydra.module.QualifiedName](hydra.extract.helpers.requireField("local")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_local: scala.Predef.String) =>
      Right(hydra.module.QualifiedName(field_namespace, field_local))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def termDefinition(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.module.TermDefinition] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.module.TermDefinition]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.module.TermDefinition](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.module.TermDefinition](hydra.extract.helpers.requireField("term")(hydra.decode.core.term)(fieldMap)(cx))((field_term: hydra.core.Term) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.TypeScheme, hydra.module.TermDefinition](hydra.extract.helpers.requireField("type")(hydra.decode.core.typeScheme)(fieldMap)(cx))((field_type: hydra.core.TypeScheme) =>
      Right(hydra.module.TermDefinition(field_name, field_term, field_type)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeDefinition(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.module.TypeDefinition] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.module.TypeDefinition]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.module.TypeDefinition](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.module.TypeDefinition](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) => Right(hydra.module.TypeDefinition(field_name,
         field_type))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
