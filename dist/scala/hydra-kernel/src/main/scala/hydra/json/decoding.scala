package hydra.json.decoding

import hydra.json.model.*

def decodeArray[T0](decodeElem: (hydra.json.model.Value => Either[scala.Predef.String,
   T0]))(v1: hydra.json.model.Value): Either[scala.Predef.String, Seq[T0]] =
  v1 match
  case hydra.json.model.Value.array(v_Value_array_a) => hydra.lib.eithers.mapList[hydra.json.model.Value,
     T0, scala.Predef.String](decodeElem)(v_Value_array_a)
  case _ => Left("expected an array")

def decodeBoolean(v1: hydra.json.model.Value): Either[scala.Predef.String, Boolean] =
  v1 match
  case hydra.json.model.Value.boolean(v_Value_boolean_b) => Right(v_Value_boolean_b)
  case _ => Left("expected a boolean")

def decodeField[T0, T1](decodeValue: (T0 => Either[scala.Predef.String, T1]))(name: scala.Predef.String)(m: Map[scala.Predef.String,
   T0]): Either[scala.Predef.String, T1] =
  hydra.lib.eithers.bind[scala.Predef.String, Option[T1], T1](hydra.json.decoding.decodeOptionalField(decodeValue)(name)(m))((v1) =>
  hydra.lib.maybes.maybe[Either[scala.Predef.String, T1], T1](Left(hydra.lib.strings.cat2("missing field: ")(name)))((f: T1) => Right(f))(v1))

def decodeObject(v1: hydra.json.model.Value): Either[scala.Predef.String, Map[scala.Predef.String,
   hydra.json.model.Value]] =
  v1 match
  case hydra.json.model.Value.`object`(v_Value_object_o) => Right(v_Value_object_o)
  case _ => Left("expected an object")

def decodeOptionalField[T0, T1, T2, T3](decodeValue: (T0 => Either[T1, T2]))(name: T3)(m: Map[T3,
   T0]): Either[T1, Option[T2]] =
  hydra.lib.maybes.maybe[Either[T1, Option[T2]], T0](Right(None))((v: T0) =>
  hydra.lib.eithers.map[T2, Option[T2], T1]((x: T2) => Some(x))(decodeValue(v)))(hydra.lib.maps.lookup[T3, T0](name)(m))

def decodeString(v1: hydra.json.model.Value): Either[scala.Predef.String, scala.Predef.String] =
  v1 match
  case hydra.json.model.Value.string(v_Value_string_s) => Right(v_Value_string_s)
  case _ => Left("expected a string")
