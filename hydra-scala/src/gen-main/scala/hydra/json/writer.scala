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

val colonOp: hydra.ast.Op = hydra.ast.Op(":", hydra.ast.Padding(hydra.ast.Ws.none, hydra.ast.Ws.space), 0, hydra.ast.Associativity.none)

def jsonString(s: scala.Predef.String): scala.Predef.String =
  {
  def hexEscape(c: Int): scala.Predef.String =
    {
    val hi: scala.Predef.String = strings.fromList(lists.pure[Int](strings.charAt(math.div(c)(16))("0123456789abcdef")))
    val lo: scala.Predef.String = strings.fromList(lists.pure[Int](strings.charAt(math.mod(c)(16))("0123456789abcdef")))
    strings.cat2(strings.cat2("\\u00")(hi))(lo)
  }
  def escape(c: Int): scala.Predef.String =
    logic.ifElse[scala.Predef.String](equality.equal[Int](c)(34))("\\\"")(logic.ifElse[scala.Predef.String](equality.equal[Int](c)(92))("\\\\")(logic.ifElse[scala.Predef.String](equality.equal[Int](c)(8))("\\b")(logic.ifElse[scala.Predef.String](equality.equal[Int](c)(12))("\\f")(logic.ifElse[scala.Predef.String](equality.equal[Int](c)(10))("\\n")(logic.ifElse[scala.Predef.String](equality.equal[Int](c)(13))("\\r")(logic.ifElse[scala.Predef.String](equality.equal[Int](c)(9))("\\t")(logic.ifElse[scala.Predef.String](equality.lt[Int](c)(32))(hexEscape(c))(strings.fromList(lists.pure[Int](c))))))))))
  val escaped: scala.Predef.String = strings.cat(lists.map[Int, scala.Predef.String](escape)(strings.toList(s)))
  strings.cat2(strings.cat2("\"")(escaped))("\"")
}

def keyValueToExpr(pair: Tuple2[scala.Predef.String, hydra.json.model.Value]): hydra.ast.Expr =
  {
  val key: scala.Predef.String = pairs.first[scala.Predef.String, hydra.json.model.Value](pair)
  val value: hydra.json.model.Value = pairs.second[scala.Predef.String, hydra.json.model.Value](pair)
  hydra.serialization.ifx(hydra.json.writer.colonOp)(hydra.serialization.cst(hydra.json.writer.jsonString(key)))(hydra.json.writer.valueToExpr(value))
}

def printJson(value: hydra.json.model.Value): scala.Predef.String = hydra.serialization.printExpr(hydra.json.writer.valueToExpr(value))

def valueToExpr(value: hydra.json.model.Value): hydra.ast.Expr =
  value match
  case hydra.json.model.Value.array(v_Value_array_arr) => hydra.serialization.bracketListAdaptive(lists.map[hydra.json.model.Value,
     hydra.ast.Expr](hydra.json.writer.valueToExpr)(v_Value_array_arr))
  case hydra.json.model.Value.boolean(v_Value_boolean_b) => hydra.serialization.cst(logic.ifElse[scala.Predef.String](v_Value_boolean_b)("true")("false"))
  case hydra.json.model.Value.`null` => hydra.serialization.cst("null")
  case hydra.json.model.Value.number(v_Value_number_n) => {
    val rounded: BigInt = literals.bigfloatToBigint(v_Value_number_n)
    hydra.serialization.cst(logic.ifElse[scala.Predef.String](equality.equal[BigDecimal](v_Value_number_n)(literals.bigintToBigfloat(rounded)))(literals.showBigint(rounded))(literals.showBigfloat(v_Value_number_n)))
  }
  case hydra.json.model.Value.`object`(v_Value_object_obj) => hydra.serialization.bracesListAdaptive(lists.map[Tuple2[scala.Predef.String,
     hydra.json.model.Value], hydra.ast.Expr](hydra.json.writer.keyValueToExpr)(maps.toList[scala.Predef.String,
     hydra.json.model.Value](v_Value_object_obj)))
  case hydra.json.model.Value.string(v_Value_string_s) => hydra.serialization.cst(hydra.json.writer.jsonString(v_Value_string_s))
