package hydra.decode.topology

import hydra.core.*

import hydra.errors.*

import hydra.topology.*

import hydra.lib.eithers

def graph(v1: hydra.graph.Graph)(v2: hydra.core.Term): Either[hydra.errors.DecodingError, Map[Int, Seq[Int]]] =
  hydra.extract.helpers.decodeMap(hydra.decode.topology.vertex)((v12: hydra.graph.Graph) =>
  (v22: hydra.core.Term) =>
  hydra.extract.helpers.decodeList(hydra.decode.topology.vertex)(v12)(v22))(v1)(v2)

def tarjanState(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.topology.TarjanState] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.topology.TarjanState]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Int, hydra.topology.TarjanState](hydra.extract.helpers.requireField("counter")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_counter: Int) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Map[Int, Int], hydra.topology.TarjanState](hydra.extract.helpers.requireField("indices")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMap(hydra.decode.topology.vertex)((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v12) => v_Literal_integer_v12 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_indices: Map[Int, Int]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Map[Int, Int], hydra.topology.TarjanState](hydra.extract.helpers.requireField("lowLinks")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMap(hydra.decode.topology.vertex)((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v12) => v_Literal_integer_v12 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_lowLinks: Map[Int, Int]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[Int], hydra.topology.TarjanState](hydra.extract.helpers.requireField("stack")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.topology.vertex)(v1)(v2))(fieldMap)(cx))((field_stack: Seq[Int]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.collection.immutable.Set[Int], hydra.topology.TarjanState](hydra.extract.helpers.requireField("onStack")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeSet(hydra.decode.topology.vertex)(v1)(v2))(fieldMap)(cx))((field_onStack: scala.collection.immutable.Set[Int]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[Seq[Int]], hydra.topology.TarjanState](hydra.extract.helpers.requireField("sccs")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.topology.vertex)(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_sccs: Seq[Seq[Int]]) =>
      Right(hydra.topology.TarjanState(field_counter, field_indices, field_lowLinks, field_stack, field_onStack, field_sccs))))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def vertex(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, Int] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
    case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
      case _ => Left("expected int32 value")
    case _ => Left("expected int32 literal")
  case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
