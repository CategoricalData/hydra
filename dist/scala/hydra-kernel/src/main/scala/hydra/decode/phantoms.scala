package hydra.decode.phantoms

import hydra.core.*

import hydra.errors.*

import hydra.phantoms.*

def tBinding[T0, T1](a: T0)(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.phantoms.TBinding[T1]] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.phantoms.TBinding[T1]]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.phantoms.TBinding[T1]](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.phantoms.TTerm[T1],
         hydra.phantoms.TBinding[T1]](hydra.extract.core.requireField("term")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) => hydra.decode.phantoms.tTerm(a)(v1)(v2))(fieldMap)(cx))((field_term: hydra.phantoms.TTerm[T1]) => Right(hydra.phantoms.TBinding(field_name,
         field_term))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def tTerm[T0, T1](a: T0)(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.phantoms.TTerm[T1]] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.phantoms.TTerm[T1]]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[hydra.core.Term,
     hydra.phantoms.TTerm[T1], hydra.errors.DecodingError]((b: hydra.core.Term) => b)(hydra.decode.core.term(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def tTermDefinition[T0, T1](a: T0)(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.phantoms.TTermDefinition[T1]] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.phantoms.TTermDefinition[T1]]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.phantoms.TTermDefinition[T1]](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.phantoms.TTerm[T1],
         hydra.phantoms.TTermDefinition[T1]](hydra.extract.core.requireField("term")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) => hydra.decode.phantoms.tTerm(a)(v1)(v2))(fieldMap)(cx))((field_term: hydra.phantoms.TTerm[T1]) =>
      Right(hydra.phantoms.TTermDefinition(field_name, field_term))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
