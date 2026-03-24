package hydra.decode.variants

import hydra.core.*

import hydra.errors.*

import hydra.variants.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def eliminationVariant(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.variants.EliminationVariant] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.variants.EliminationVariant]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.variants.EliminationVariant])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.EliminationVariant]](Seq(Tuple2("record", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.EliminationVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.EliminationVariant.record(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("union", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.EliminationVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.EliminationVariant.union(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("wrap", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.EliminationVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.EliminationVariant.wrap(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.variants.EliminationVariant], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.EliminationVariant]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.variants.EliminationVariant])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.EliminationVariant]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def functionVariant(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.variants.FunctionVariant] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.variants.FunctionVariant]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.variants.FunctionVariant])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.FunctionVariant]](Seq(Tuple2("elimination", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.FunctionVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.FunctionVariant.elimination(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("lambda", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.FunctionVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.FunctionVariant.lambda(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("primitive", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.FunctionVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.FunctionVariant.primitive(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.variants.FunctionVariant], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.FunctionVariant]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.variants.FunctionVariant])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.FunctionVariant]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def literalVariant(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.variants.LiteralVariant] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.variants.LiteralVariant]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.variants.LiteralVariant])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.LiteralVariant]](Seq(Tuple2("binary", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.LiteralVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.LiteralVariant.binary(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("boolean", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.LiteralVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.LiteralVariant.boolean(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("float", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.LiteralVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.LiteralVariant.float(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("integer", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.LiteralVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.LiteralVariant.integer(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("string", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.LiteralVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.LiteralVariant.string(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.variants.LiteralVariant], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.LiteralVariant]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.variants.LiteralVariant])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.LiteralVariant]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def termVariant(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.variants.TermVariant] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.variants.TermVariant]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.variants.TermVariant])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.TermVariant]](Seq(Tuple2("annotated", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.annotated(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("application", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.application(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("either", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.either(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("function", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.function(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("let", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.let(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("list", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.list(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("literal", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.literal(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("map", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.map(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("maybe", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.maybe(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("pair", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.pair(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("record", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.record(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("set", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.set(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("typeApplication", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.typeApplication(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("typeLambda", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.typeLambda(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("union", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.union(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("unit", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.unit(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("variable", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.variable(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("wrap", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TermVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TermVariant.wrap(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.variants.TermVariant], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.TermVariant]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.variants.TermVariant])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.TermVariant]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeVariant(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.variants.TypeVariant] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.variants.TypeVariant]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.variants.TypeVariant])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.TypeVariant]](Seq(Tuple2("annotated", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.annotated(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("application", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.application(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("either", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.either(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("forall", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.forall(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("function", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.function(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("list", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.list(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("literal", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.literal(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("map", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.map(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("maybe", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.maybe(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("pair", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.pair(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("record", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.record(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("set", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.set(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("union", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.union(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("unit", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.unit(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("variable", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.variable(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("void", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.void(t))(hydra.extract.helpers.decodeUnit(cx)(input))), Tuple2("wrap", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.variants.TypeVariant, hydra.errors.DecodingError]((t: Unit) => hydra.variants.TypeVariant.wrap(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.variants.TypeVariant], (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.TypeVariant]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.variants.TypeVariant])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.variants.TypeVariant]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
