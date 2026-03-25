package hydra.decode.relational

import hydra.core.*

import hydra.errors.*

import hydra.relational.*

import hydra.lib.eithers

def columnName(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.relational.ColumnName] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.relational.ColumnName]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.relational.ColumnName, hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def columnSchema[T0](t: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.relational.ColumnSchema[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.relational.ColumnSchema[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.relational.ColumnName, hydra.relational.ColumnSchema[T0]](hydra.extract.helpers.requireField("name")(hydra.decode.relational.columnName)(fieldMap)(cx))((field_name: hydra.relational.ColumnName) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, T0, hydra.relational.ColumnSchema[T0]](hydra.extract.helpers.requireField("domain")(t)(fieldMap)(cx))((field_domain: T0) =>
      Right(hydra.relational.ColumnSchema(field_name, field_domain))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def foreignKey(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.relational.ForeignKey] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.relational.ForeignKey]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.relational.RelationName, hydra.relational.ForeignKey](hydra.extract.helpers.requireField("foreignRelation")(hydra.decode.relational.relationName)(fieldMap)(cx))((field_foreignRelation: hydra.relational.RelationName) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Map[hydra.relational.ColumnName, hydra.relational.ColumnName], hydra.relational.ForeignKey](hydra.extract.helpers.requireField("keys")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMap(hydra.decode.relational.columnName)(hydra.decode.relational.columnName)(v1)(v2))(fieldMap)(cx))((field_keys: Map[hydra.relational.ColumnName, hydra.relational.ColumnName]) =>
      Right(hydra.relational.ForeignKey(field_foreignRelation, field_keys))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def primaryKey(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.relational.PrimaryKey] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.relational.PrimaryKey]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Seq[hydra.relational.ColumnName], hydra.relational.PrimaryKey, hydra.errors.DecodingError]((b: Seq[hydra.relational.ColumnName]) => b)(hydra.extract.helpers.decodeList(hydra.decode.relational.columnName)(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def relation[T0](v: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.relational.Relation[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.relational.Relation[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Seq[hydra.relational.Row[T0]], hydra.relational.Relation[T0], hydra.errors.DecodingError]((b: Seq[hydra.relational.Row[T0]]) => b)(hydra.extract.helpers.decodeList((v1: hydra.graph.Graph) =>
    (v2: hydra.core.Term) => hydra.decode.relational.row(v)(v1)(v2))(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def relationName(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.relational.RelationName] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.relational.RelationName]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.relational.RelationName, hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def relationSchema[T0](t: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.relational.RelationSchema[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.relational.RelationSchema[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.relational.RelationName, hydra.relational.RelationSchema[T0]](hydra.extract.helpers.requireField("name")(hydra.decode.relational.relationName)(fieldMap)(cx))((field_name: hydra.relational.RelationName) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.relational.ColumnSchema[T0]], hydra.relational.RelationSchema[T0]](hydra.extract.helpers.requireField("columns")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) => hydra.decode.relational.columnSchema(t)(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_columns: Seq[hydra.relational.ColumnSchema[T0]]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.relational.PrimaryKey], hydra.relational.RelationSchema[T0]](hydra.extract.helpers.requireField("primaryKeys")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.relational.primaryKey)(v1)(v2))(fieldMap)(cx))((field_primaryKeys: Seq[hydra.relational.PrimaryKey]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.relational.ForeignKey], hydra.relational.RelationSchema[T0]](hydra.extract.helpers.requireField("foreignKeys")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.relational.foreignKey)(v1)(v2))(fieldMap)(cx))((field_foreignKeys: Seq[hydra.relational.ForeignKey]) =>
      Right(hydra.relational.RelationSchema(field_name, field_columns, field_primaryKeys, field_foreignKeys))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def relationship[T0](v: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.relational.Relationship[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.relational.Relationship[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.collection.immutable.Set[Map[hydra.relational.ColumnName, T0]], hydra.relational.Relationship[T0], hydra.errors.DecodingError]((b: scala.collection.immutable.Set[Map[hydra.relational.ColumnName, T0]]) => b)(hydra.extract.helpers.decodeSet((v1: hydra.graph.Graph) =>
    (v2: hydra.core.Term) =>
    hydra.extract.helpers.decodeMap(hydra.decode.relational.columnName)(v)(v1)(v2))(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def row[T0](v: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.relational.Row[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.relational.Row[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Seq[T0], hydra.relational.Row[T0], hydra.errors.DecodingError]((b: Seq[T0]) => b)(hydra.extract.helpers.decodeList(v)(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
