package hydra.json.writer

import hydra.ast.*

import hydra.json.model.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

import hydra.lib.pairs

import hydra.lib.strings

lazy val colonOp: hydra.ast.Op = hydra.ast.Op(":", hydra.ast.Padding(hydra.ast.Ws.none, hydra.ast.Ws.space), 0, hydra.ast.Associativity.none)

def jsonString(s: scala.Predef.String): scala.Predef.String =
  {
  def hexEscape(c: Int): scala.Predef.String =
    {
    lazy val hi: scala.Predef.String = hydra.lib.strings.fromList(hydra.lib.lists.pure[Int](hydra.lib.strings.charAt(hydra.lib.math.div(c)(16))("0123456789abcdef")))
    lazy val lo: scala.Predef.String = hydra.lib.strings.fromList(hydra.lib.lists.pure[Int](hydra.lib.strings.charAt(hydra.lib.math.mod(c)(16))("0123456789abcdef")))
    hydra.lib.strings.cat2(hydra.lib.strings.cat2("\\u00")(hi))(lo)
  }
  def escape(c: Int): scala.Predef.String =
    hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(34))("\\\"")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(92))("\\\\")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(8))("\\b")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(12))("\\f")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(10))("\\n")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(13))("\\r")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(9))("\\t")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.lt[Int](c)(32))(hexEscape(c))(hydra.lib.strings.fromList(hydra.lib.lists.pure[Int](c))))))))))
  lazy val escaped: scala.Predef.String = hydra.lib.strings.cat(hydra.lib.lists.map[Int, scala.Predef.String](escape)(hydra.lib.strings.toList(s)))
  hydra.lib.strings.cat2(hydra.lib.strings.cat2("\"")(escaped))("\"")
}

def keyValueToExpr(pair: Tuple2[scala.Predef.String, hydra.json.model.Value]): hydra.ast.Expr =
  {
  lazy val key: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String, hydra.json.model.Value](pair)
  lazy val value: hydra.json.model.Value = hydra.lib.pairs.second[scala.Predef.String, hydra.json.model.Value](pair)
  hydra.serialization.ifx(hydra.json.writer.colonOp)(hydra.serialization.cst(hydra.json.writer.jsonString(key)))(hydra.json.writer.valueToExpr(value))
}

def printJson(value: hydra.json.model.Value): scala.Predef.String = hydra.serialization.printExpr(hydra.json.writer.valueToExpr(value))

def valueToExpr(value: hydra.json.model.Value): hydra.ast.Expr =
  value match
  case hydra.json.model.Value.array(v_Value_array_arr) => hydra.serialization.bracketListAdaptive(hydra.lib.lists.map[hydra.json.model.Value,
     hydra.ast.Expr](hydra.json.writer.valueToExpr)(v_Value_array_arr))
  case hydra.json.model.Value.boolean(v_Value_boolean_b) => hydra.serialization.cst(hydra.lib.logic.ifElse[scala.Predef.String](v_Value_boolean_b)("true")("false"))
  case hydra.json.model.Value.`null` => hydra.serialization.cst("null")
  case hydra.json.model.Value.number(v_Value_number_n) => {
    lazy val rounded: BigInt = hydra.lib.literals.bigfloatToBigint(v_Value_number_n)
    hydra.serialization.cst(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[BigDecimal](v_Value_number_n)(hydra.lib.literals.bigintToBigfloat(rounded)))(hydra.lib.literals.showBigint(rounded))(hydra.lib.literals.showBigfloat(v_Value_number_n)))
  }
  case hydra.json.model.Value.`object`(v_Value_object_obj) => hydra.serialization.bracesListAdaptive(hydra.lib.lists.map[Tuple2[scala.Predef.String,
     hydra.json.model.Value], hydra.ast.Expr](hydra.json.writer.keyValueToExpr)(hydra.lib.maps.toList[scala.Predef.String,
     hydra.json.model.Value](v_Value_object_obj)))
  case hydra.json.model.Value.string(v_Value_string_s) => hydra.serialization.cst(hydra.json.writer.jsonString(v_Value_string_s))
