package hydra.decode.classes

import hydra.classes.*

import hydra.core.*

import hydra.error.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def typeClass(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.classes.TypeClass] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.classes.TypeClass]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.classes.TypeClass])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.classes.TypeClass]](Seq(Tuple2("equality",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.classes.TypeClass, hydra.error.DecodingError]((t: Unit) => hydra.classes.TypeClass.equality)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("ordering", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.classes.TypeClass, hydra.error.DecodingError]((t: Unit) => hydra.classes.TypeClass.ordering)(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.error.DecodingError, hydra.classes.TypeClass], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.classes.TypeClass]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.classes.TypeClass])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.classes.TypeClass]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
