package hydra.extract.json

import hydra.json.model.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def expectArray(value: hydra.json.model.Value): Either[scala.Predef.String, Seq[hydra.json.model.Value]] =
  value match
  case hydra.json.model.Value.array(v_Value_array_els) => Right(v_Value_array_els)
  case _ => Left(strings.cat2(strings.cat2("expected ")("JSON array"))(strings.cat2(" but found ")(hydra.extract.json.showValue(value))))

def expectNumber(value: hydra.json.model.Value): Either[scala.Predef.String, BigDecimal] =
  value match
  case hydra.json.model.Value.number(v_Value_number_d) => Right(v_Value_number_d)
  case _ => Left(strings.cat2(strings.cat2("expected ")("JSON number"))(strings.cat2(" but found ")(hydra.extract.json.showValue(value))))

def expectObject(value: hydra.json.model.Value): Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] =
  value match
  case hydra.json.model.Value.`object`(v_Value_object_m) => Right(v_Value_object_m)
  case _ => Left(strings.cat2(strings.cat2("expected ")("JSON object"))(strings.cat2(" but found ")(hydra.extract.json.showValue(value))))

def expectString(value: hydra.json.model.Value): Either[scala.Predef.String, scala.Predef.String] =
  value match
  case hydra.json.model.Value.string(v_Value_string_s) => Right(v_Value_string_s)
  case _ => Left(strings.cat2(strings.cat2("expected ")("JSON string"))(strings.cat2(" but found ")(hydra.extract.json.showValue(value))))

def opt[T0, T1](fname: T0)(m: Map[T0, T1]): Option[T1] = maps.lookup[T0, T1](fname)(m)

def optArray[T0](fname: T0)(m: Map[T0, hydra.json.model.Value]): Either[scala.Predef.String, Option[Seq[hydra.json.model.Value]]] =
  maybes.maybe[Either[scala.Predef.String, Option[Seq[hydra.json.model.Value]]], hydra.json.model.Value](Right(None))((a: hydra.json.model.Value) =>
  eithers.map[Seq[hydra.json.model.Value], Option[Seq[hydra.json.model.Value]], scala.Predef.String]((x: Seq[hydra.json.model.Value]) => Some(x))(hydra.extract.json.expectArray(a)))(hydra.extract.json.opt(fname)(m))

def optString[T0](fname: T0)(m: Map[T0, hydra.json.model.Value]): Either[scala.Predef.String, Option[scala.Predef.String]] =
  maybes.maybe[Either[scala.Predef.String, Option[scala.Predef.String]], hydra.json.model.Value](Right(None))((s: hydra.json.model.Value) =>
  eithers.map[scala.Predef.String, Option[scala.Predef.String], scala.Predef.String]((x: scala.Predef.String) => Some(x))(hydra.extract.json.expectString(s)))(hydra.extract.json.opt(fname)(m))

def require[T0, T1](fname: T0)(m: Map[T0, T1]): Either[scala.Predef.String, T1] =
  maybes.maybe[Either[scala.Predef.String, T1], T1](Left(strings.cat(Seq("required attribute ", hydra.extract.json.showValue(fname),
     " not found"))))((value: T1) => Right(value))(maps.lookup[T0, T1](fname)(m))

def requireArray[T0](fname: T0)(m: Map[T0, hydra.json.model.Value]): Either[scala.Predef.String, Seq[hydra.json.model.Value]] =
  eithers.bind[scala.Predef.String, hydra.json.model.Value, Seq[hydra.json.model.Value]](hydra.extract.json.require(fname)(m))(hydra.extract.json.expectArray)

def requireNumber[T0](fname: T0)(m: Map[T0, hydra.json.model.Value]): Either[scala.Predef.String, BigDecimal] =
  eithers.bind[scala.Predef.String, hydra.json.model.Value, BigDecimal](hydra.extract.json.require(fname)(m))(hydra.extract.json.expectNumber)

def requireString[T0](fname: T0)(m: Map[T0, hydra.json.model.Value]): Either[scala.Predef.String, scala.Predef.String] =
  eithers.bind[scala.Predef.String, hydra.json.model.Value, scala.Predef.String](hydra.extract.json.require(fname)(m))(hydra.extract.json.expectString)

def showValue[T0](value: T0): scala.Predef.String = "TODO: implement showValue"
