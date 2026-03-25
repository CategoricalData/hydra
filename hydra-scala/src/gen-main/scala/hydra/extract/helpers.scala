package hydra.extract.helpers

import hydra.core.*

import hydra.errors.*

import hydra.lib.eithers

import hydra.lib.lists

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def decodeEither[T0, T1](leftDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(rightDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T1]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError, Either[T0, T1]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Either[T0, T1]](hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Term, hydra.errors.DecodingError, hydra.core.Term]((x: scala.Predef.String) => x)((x: hydra.core.Term) => x)(hydra.lexical.stripAndDereferenceTermEither(g)(term)))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.errors.DecodingError, Either[T0, T1]]]((lv: hydra.core.Term) =>
    hydra.lib.eithers.map[T0, Either[T0, T1], hydra.errors.DecodingError]((x: T0) => Left(x))(leftDecoder(g)(lv)))((rv: hydra.core.Term) =>
    hydra.lib.eithers.map[T1, Either[T0, T1], hydra.errors.DecodingError]((x: T1) => Right(x))(rightDecoder(g)(rv)))(v_Term_either_e)
  case _ => Left("expected either value"))

def decodeList[T0](elemDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError, Seq[T0]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Seq[T0]](hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Term, hydra.errors.DecodingError, hydra.core.Term]((x: scala.Predef.String) => x)((x: hydra.core.Term) => x)(hydra.lexical.stripAndDereferenceTermEither(g)(term)))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.mapList[hydra.core.Term, T0, hydra.errors.DecodingError]((v1: hydra.core.Term) => elemDecoder(g)(v1))(v_Term_list_els)
  case _ => Left("expected list"))

def decodeMap[T0, T1](keyDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(valDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T1]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError, Map[T0, T1]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Map[T0, T1]](hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Term, hydra.errors.DecodingError, hydra.core.Term]((x: scala.Predef.String) => x)((x: hydra.core.Term) => x)(hydra.lexical.stripAndDereferenceTermEither(g)(term)))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.map[Seq[Tuple2[T0, T1]], Map[T0, T1], hydra.errors.DecodingError](hydra.lib.maps.fromList[T0, T1])(hydra.lib.eithers.mapList[Tuple2[hydra.core.Term, hydra.core.Term], Tuple2[T0, T1], hydra.errors.DecodingError]((kv: Tuple2[hydra.core.Term, hydra.core.Term]) =>
    hydra.lib.eithers.bind[hydra.errors.DecodingError, T0, Tuple2[T0, T1]](keyDecoder(g)(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv)))((k: T0) =>
    hydra.lib.eithers.map[T1, Tuple2[T0, T1], hydra.errors.DecodingError]((v: T1) => Tuple2(k, v))(valDecoder(g)(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv)))))(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))
  case _ => Left("expected map"))

def decodeMaybe[T0](elemDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError, Option[T0]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Option[T0]](hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Term, hydra.errors.DecodingError, hydra.core.Term]((x: scala.Predef.String) => x)((x: hydra.core.Term) => x)(hydra.lexical.stripAndDereferenceTermEither(g)(term)))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.maybe(v_Term_maybe_opt) => hydra.lib.eithers.mapMaybe[hydra.core.Term, T0, hydra.errors.DecodingError]((v1: hydra.core.Term) => elemDecoder(g)(v1))(v_Term_maybe_opt)
  case _ => Left("expected optional value"))

def decodePair[T0, T1](firstDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(secondDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T1]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError, Tuple2[T0, T1]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Tuple2[T0, T1]](hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Term, hydra.errors.DecodingError, hydra.core.Term]((x: scala.Predef.String) => x)((x: hydra.core.Term) => x)(hydra.lexical.stripAndDereferenceTermEither(g)(term)))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[hydra.errors.DecodingError, T0, Tuple2[T0, T1]](firstDecoder(g)(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((f: T0) =>
    hydra.lib.eithers.map[T1, Tuple2[T0, T1], hydra.errors.DecodingError]((s: T1) => Tuple2(f, s))(secondDecoder(g)(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))))
  case _ => Left("expected pair"))

def decodeSet[T0](elemDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError, scala.collection.immutable.Set[T0]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, scala.collection.immutable.Set[T0]](hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Term, hydra.errors.DecodingError, hydra.core.Term]((x: scala.Predef.String) => x)((x: hydra.core.Term) => x)(hydra.lexical.stripAndDereferenceTermEither(g)(term)))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.map[Seq[T0], scala.collection.immutable.Set[T0], hydra.errors.DecodingError](hydra.lib.sets.fromList[T0])(hydra.lib.eithers.mapList[hydra.core.Term, T0, hydra.errors.DecodingError]((v1: hydra.core.Term) => elemDecoder(g)(v1))(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))
  case _ => Left("expected set"))

def decodeUnit(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError, Unit] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Unit](hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Term, hydra.errors.DecodingError, hydra.core.Term]((x: scala.Predef.String) => x)((x: hydra.core.Term) => x)(hydra.lexical.stripAndDereferenceTermEither(g)(term)))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.unit => Right(())
  case _ => Left("expected a unit value"))

def decodeWrapped[T0](bodyDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError, T0] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, T0](hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Term, hydra.errors.DecodingError, hydra.core.Term]((x: scala.Predef.String) => x)((x: hydra.core.Term) => x)(hydra.lexical.stripAndDereferenceTermEither(g)(term)))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wt) => bodyDecoder(g)(v_Term_wrap_wt.body)
  case _ => Left("expected wrapped value"))

def requireField[T0, T1, T2](fieldName: scala.Predef.String)(decoder: (T0 => T1 => Either[hydra.errors.DecodingError, T2]))(fieldMap: Map[hydra.core.Name, T1])(g: T0): Either[hydra.errors.DecodingError, T2] =
  hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, T2], T1](Left(hydra.lib.strings.cat(Seq("missing field ", fieldName, " in record"))))((fieldTerm: T1) => decoder(g)(fieldTerm))(hydra.lib.maps.lookup[hydra.core.Name, T1](fieldName)(fieldMap))

def toFieldMap(record: hydra.core.Record): Map[hydra.core.Name, hydra.core.Term] =
  hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](hydra.lib.lists.map[hydra.core.Field, Tuple2[hydra.core.Name, hydra.core.Term]]((f: hydra.core.Field) => Tuple2(f.name, (f.term)))(record.fields))
