package hydra.json.encode

import hydra.core.*

import hydra.json.model.*

import hydra.lib.eithers

import hydra.lib.literals

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def toJson(term: hydra.core.Term): Either[scala.Predef.String, hydra.json.model.Value] =
  {
  lazy val stripped: hydra.core.Term = hydra.rewriting.deannotateTerm(term)
  stripped match
    case hydra.core.Term.literal(v_Term_literal_lit) => hydra.json.encode.encodeLiteral(v_Term_literal_lit)
    case hydra.core.Term.list(v_Term_list_terms) => {
      lazy val results: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.lib.eithers.mapList[hydra.core.Term, hydra.json.model.Value, scala.Predef.String]((t: hydra.core.Term) => hydra.json.encode.toJson(t))(v_Term_list_terms)
      hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value, scala.Predef.String]((vs: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(vs))(results)
    }
    case hydra.core.Term.set(v_Term_set_vals) => {
      lazy val terms: Seq[hydra.core.Term] = hydra.lib.sets.toList[hydra.core.Term](v_Term_set_vals)
      {
        lazy val results: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.lib.eithers.mapList[hydra.core.Term, hydra.json.model.Value, scala.Predef.String]((t: hydra.core.Term) => hydra.json.encode.toJson(t))(terms)
        hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value, scala.Predef.String]((vs: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(vs))(results)
      }
    }
    case hydra.core.Term.maybe(v_Term_maybe_opt) => hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.json.model.Value], hydra.core.Term](Right(hydra.json.model.Value.`null`))((v: hydra.core.Term) =>
      {
      lazy val encodedMaybe: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(v)
      hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value, scala.Predef.String]((encoded: hydra.json.model.Value) => hydra.json.model.Value.array(Seq(encoded)))(encodedMaybe)
    })(v_Term_maybe_opt)
    case hydra.core.Term.record(v_Term_record_r) => {
      def encodeField(f: hydra.core.Field): Either[scala.Predef.String, Tuple2[scala.Predef.String, hydra.json.model.Value]] =
        {
        lazy val fname: scala.Predef.String = (f.name)
        lazy val fterm: hydra.core.Term = (f.term)
        lazy val encodedField: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(fterm)
        hydra.lib.eithers.map[hydra.json.model.Value, Tuple2[scala.Predef.String, hydra.json.model.Value], scala.Predef.String]((v: hydra.json.model.Value) => Tuple2(fname, v))(encodedField)
      }
      {
        lazy val fields: Seq[hydra.core.Field] = (v_Term_record_r.fields)
        {
          lazy val encodedFields: Either[scala.Predef.String, Seq[Tuple2[scala.Predef.String, hydra.json.model.Value]]] = hydra.lib.eithers.mapList[hydra.core.Field, Tuple2[scala.Predef.String, hydra.json.model.Value], scala.Predef.String](encodeField)(fields)
          hydra.lib.eithers.map[Seq[Tuple2[scala.Predef.String, hydra.json.model.Value]], hydra.json.model.Value, scala.Predef.String]((fs: Seq[Tuple2[scala.Predef.String, hydra.json.model.Value]]) =>
            hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String, hydra.json.model.Value](fs)))(encodedFields)
        }
      }
    }
    case hydra.core.Term.union(v_Term_union_inj) => {
      lazy val field: hydra.core.Field = (v_Term_union_inj.field)
      {
        lazy val fname: scala.Predef.String = (field.name)
        {
          lazy val fterm: hydra.core.Term = (field.term)
          {
            lazy val encodedUnion: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(fterm)
            hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value, scala.Predef.String]((v: hydra.json.model.Value) =>
              hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String, hydra.json.model.Value](Seq(Tuple2(fname, v)))))(encodedUnion)
          }
        }
      }
    }
    case hydra.core.Term.unit() => Right(hydra.json.model.Value.`object`(hydra.lib.maps.empty[scala.Predef.String, hydra.json.model.Value]))
    case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.json.encode.toJson(v_Term_wrap_wt.body)
    case hydra.core.Term.map(v_Term_map_m) => {
      def encodeEntry(kv: Tuple2[hydra.core.Term, hydra.core.Term]): Either[scala.Predef.String, hydra.json.model.Value] =
        {
        lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv)
        lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv)
        lazy val encodedK: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(k)
        lazy val encodedV: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(v)
        hydra.lib.eithers.either[scala.Predef.String, hydra.json.model.Value, Either[scala.Predef.String, hydra.json.model.Value]]((err: scala.Predef.String) => Left(err))((ek: hydra.json.model.Value) =>
          hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value, scala.Predef.String]((ev: hydra.json.model.Value) =>
          hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String, hydra.json.model.Value](Seq(Tuple2("@key", ek), Tuple2("@value", ev)))))(encodedV))(encodedK)
      }
      {
        lazy val entries: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.lib.eithers.mapList[Tuple2[hydra.core.Term, hydra.core.Term], hydra.json.model.Value, scala.Predef.String](encodeEntry)(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m))
        hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value, scala.Predef.String]((es: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(es))(entries)
      }
    }
    case hydra.core.Term.pair(v_Term_pair_p) => {
      lazy val first: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
      {
        lazy val second: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
        {
          lazy val encodedFirst: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(first)
          {
            lazy val encodedSecond: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(second)
            hydra.lib.eithers.either[scala.Predef.String, hydra.json.model.Value, Either[scala.Predef.String, hydra.json.model.Value]]((err: scala.Predef.String) => Left(err))((ef: hydra.json.model.Value) =>
              hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value, scala.Predef.String]((es: hydra.json.model.Value) =>
              hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String, hydra.json.model.Value](Seq(Tuple2("@first", ef), Tuple2("@second", es)))))(encodedSecond))(encodedFirst)
          }
        }
      }
    }
    case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[scala.Predef.String, hydra.json.model.Value]]((l: hydra.core.Term) =>
      {
      lazy val encodedL: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(l)
      hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value, scala.Predef.String]((v: hydra.json.model.Value) =>
        hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String, hydra.json.model.Value](Seq(Tuple2("@left", v)))))(encodedL)
    })((r: hydra.core.Term) =>
      {
      lazy val encodedR: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(r)
      hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value, scala.Predef.String]((v: hydra.json.model.Value) =>
        hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String, hydra.json.model.Value](Seq(Tuple2("@right", v)))))(encodedR)
    })(v_Term_either_e)
    case _ => Left(hydra.lib.strings.cat(Seq("unsupported term variant for JSON encoding: ", hydra.show.core.term(term))))
}

def encodeLiteral[T0](lit: hydra.core.Literal): Either[T0, hydra.json.model.Value] =
  lit match
  case hydra.core.Literal.binary(v_Literal_binary_b) => Right(hydra.json.model.Value.string(hydra.lib.literals.binaryToString(v_Literal_binary_b)))
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(hydra.json.model.Value.boolean(v_Literal_boolean_b))
  case hydra.core.Literal.float(v_Literal_float_f) => hydra.json.encode.encodeFloat(v_Literal_float_f)
  case hydra.core.Literal.integer(v_Literal_integer_i) => hydra.json.encode.encodeInteger(v_Literal_integer_i)
  case hydra.core.Literal.string(v_Literal_string_s) => Right(hydra.json.model.Value.string(v_Literal_string_s))

def encodeFloat[T0](fv: hydra.core.FloatValue): Either[T0, hydra.json.model.Value] =
  fv match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_bf) => Right(hydra.json.model.Value.number(v_FloatValue_bigfloat_bf))
  case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => Right(hydra.json.model.Value.string(hydra.lib.literals.showFloat32(v_FloatValue_float32_f)))
  case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => Right(hydra.json.model.Value.number(hydra.lib.literals.float64ToBigfloat(v_FloatValue_float64_f)))

def encodeInteger[T0](iv: hydra.core.IntegerValue): Either[T0, hydra.json.model.Value] =
  iv match
  case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_bi) => Right(hydra.json.model.Value.string(hydra.lib.literals.showBigint(v_IntegerValue_bigint_bi)))
  case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => Right(hydra.json.model.Value.string(hydra.lib.literals.showInt64(v_IntegerValue_int64_i)))
  case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => Right(hydra.json.model.Value.string(hydra.lib.literals.showUint32(v_IntegerValue_uint32_i)))
  case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => Right(hydra.json.model.Value.string(hydra.lib.literals.showUint64(v_IntegerValue_uint64_i)))
  case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToBigfloat(hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_i))))
  case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToBigfloat(hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_i))))
  case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToBigfloat(hydra.lib.literals.int32ToBigint(v_IntegerValue_int32_i))))
  case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToBigfloat(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_i))))
  case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToBigfloat(hydra.lib.literals.uint16ToBigint(v_IntegerValue_uint16_i))))
