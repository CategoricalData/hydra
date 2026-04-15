package hydra.json.parser

import hydra.json.model.*

import hydra.parsing.*

lazy val digit: hydra.parsing.Parser[Int] = hydra.parsers.satisfy((c: Int) =>
  hydra.lib.logic.and(hydra.lib.equality.gte[Int](c)(48))(hydra.lib.equality.lte[Int](c)(57)))

lazy val digits: hydra.parsing.Parser[scala.Predef.String] = hydra.parsers.map(hydra.lib.strings.fromList)(hydra.parsers.some(hydra.json.parser.digit))

lazy val jsonArray: hydra.parsing.Parser[hydra.json.model.Value] = hydra.parsers.map((x: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(x))(hydra.parsers.between(hydra.json.parser.token(hydra.parsers.char(91)))(hydra.json.parser.token(hydra.parsers.char(93)))(hydra.parsers.sepBy(hydra.parsers.`lazy`((_x: Unit) => hydra.json.parser.jsonValue))(hydra.json.parser.token(hydra.parsers.char(44)))))

lazy val jsonBool: hydra.parsing.Parser[hydra.json.model.Value] = hydra.parsers.alt(hydra.parsers.map((_x: scala.Predef.String) => hydra.json.model.Value.boolean(true))(hydra.json.parser.token(hydra.parsers.string("true"))))(hydra.parsers.map((_x: scala.Predef.String) => hydra.json.model.Value.boolean(false))(hydra.json.parser.token(hydra.parsers.string("false"))))

lazy val jsonEscapeChar: hydra.parsing.Parser[Int] = hydra.parsers.choice(Seq(hydra.parsers.map((_x: Int) => 34)(hydra.parsers.char(34)),
   hydra.parsers.map((_x: Int) => 92)(hydra.parsers.char(92)), hydra.parsers.map((_x: Int) => 47)(hydra.parsers.char(47)),
   hydra.parsers.map((_x: Int) => 8)(hydra.parsers.char(98)), hydra.parsers.map((_x: Int) => 12)(hydra.parsers.char(102)),
   hydra.parsers.map((_x: Int) => 10)(hydra.parsers.char(110)), hydra.parsers.map((_x: Int) => 13)(hydra.parsers.char(114)),
   hydra.parsers.map((_x: Int) => 9)(hydra.parsers.char(116))))

lazy val jsonExponentPart: hydra.parsing.Parser[Option[scala.Predef.String]] = hydra.parsers.optional(hydra.parsers.bind(hydra.parsers.satisfy((c: Int) =>
  hydra.lib.logic.or(hydra.lib.equality.equal[Int](c)(101))(hydra.lib.equality.equal[Int](c)(69))))((_x: Int) =>
  hydra.parsers.bind(hydra.parsers.optional(hydra.parsers.satisfy((c: Int) =>
  hydra.lib.logic.or(hydra.lib.equality.equal[Int](c)(43))(hydra.lib.equality.equal[Int](c)(45)))))((sign: Option[Int]) =>
  hydra.parsers.map((digits: scala.Predef.String) =>
  hydra.lib.strings.cat2(hydra.lib.strings.cat2("e")(hydra.lib.maybes.maybe[scala.Predef.String,
     Int]("")((`arg_`: Int) =>
  hydra.lib.strings.fromList(hydra.lib.lists.pure[Int](`arg_`)))(sign)))(digits))(hydra.json.parser.digits))))

lazy val jsonFractionPart: hydra.parsing.Parser[Option[scala.Predef.String]] = hydra.parsers.optional(hydra.parsers.bind(hydra.parsers.char(46))((_x: Int) =>
  hydra.parsers.map((d: scala.Predef.String) => hydra.lib.strings.cat2(".")(d))(hydra.json.parser.digits)))

lazy val jsonIntegerPart: hydra.parsing.Parser[scala.Predef.String] = hydra.parsers.bind(hydra.parsers.optional(hydra.parsers.char(45)))((sign: Option[Int]) =>
  hydra.parsers.bind(hydra.json.parser.digits)((digits: scala.Predef.String) =>
  hydra.parsers.pure(hydra.lib.maybes.maybe[scala.Predef.String, Int](digits)((_x: Int) => hydra.lib.strings.cat2("-")(digits))(sign))))

lazy val jsonKeyValue: hydra.parsing.Parser[Tuple2[scala.Predef.String, hydra.json.model.Value]] = hydra.parsers.bind(hydra.json.parser.token(hydra.parsers.bind(hydra.parsers.char(34))((_x: Int) =>
  hydra.parsers.bind(hydra.parsers.many(hydra.json.parser.jsonStringChar))((chars: Seq[Int]) =>
  hydra.parsers.bind(hydra.parsers.char(34))((_2: Int) => hydra.parsers.pure(hydra.lib.strings.fromList(chars)))))))((key: scala.Predef.String) =>
  hydra.parsers.bind(hydra.json.parser.token(hydra.parsers.char(58)))((_x: Int) =>
  hydra.parsers.map((v: hydra.json.model.Value) => Tuple2(key, v))(hydra.parsers.`lazy`((_2: Unit) => hydra.json.parser.jsonValue))))

lazy val jsonNull: hydra.parsing.Parser[hydra.json.model.Value] = hydra.parsers.map((_x: scala.Predef.String) => hydra.json.model.Value.`null`)(hydra.json.parser.token(hydra.parsers.string("null")))

lazy val jsonNumber: hydra.parsing.Parser[hydra.json.model.Value] = hydra.json.parser.token(hydra.parsers.bind(hydra.json.parser.jsonIntegerPart)((intPart: scala.Predef.String) =>
  hydra.parsers.bind(hydra.json.parser.jsonFractionPart)((fracPart: Option[scala.Predef.String]) =>
  hydra.parsers.bind(hydra.json.parser.jsonExponentPart)((expPart: Option[scala.Predef.String]) =>
  {
  lazy val numStr: scala.Predef.String = hydra.lib.strings.cat2(hydra.lib.strings.cat2(intPart)(hydra.lib.maybes.maybe[scala.Predef.String,
     scala.Predef.String]("")(hydra.lib.equality.identity[scala.Predef.String])(fracPart)))(hydra.lib.maybes.maybe[scala.Predef.String,
     scala.Predef.String]("")(hydra.lib.equality.identity[scala.Predef.String])(expPart))
  hydra.parsers.pure(hydra.json.model.Value.number(hydra.lib.maybes.maybe[BigDecimal,
     BigDecimal](BigDecimal(0.0))(hydra.lib.equality.identity[BigDecimal])(hydra.lib.literals.readBigfloat(numStr))))
}))))

lazy val jsonObject: hydra.parsing.Parser[hydra.json.model.Value] = hydra.parsers.map((`arg_`: Seq[Tuple2[scala.Predef.String,
   hydra.json.model.Value]]) =>
  hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String, hydra.json.model.Value](`arg_`)))(hydra.parsers.between(hydra.json.parser.token(hydra.parsers.char(123)))(hydra.json.parser.token(hydra.parsers.char(125)))(hydra.parsers.sepBy(hydra.json.parser.jsonKeyValue)(hydra.json.parser.token(hydra.parsers.char(44)))))

lazy val jsonString: hydra.parsing.Parser[hydra.json.model.Value] = hydra.json.parser.token(hydra.parsers.bind(hydra.parsers.char(34))((_x: Int) =>
  hydra.parsers.bind(hydra.parsers.many(hydra.json.parser.jsonStringChar))((chars: Seq[Int]) =>
  hydra.parsers.bind(hydra.parsers.char(34))((_2: Int) =>
  hydra.parsers.pure(hydra.json.model.Value.string(hydra.lib.strings.fromList(chars)))))))

lazy val jsonStringChar: hydra.parsing.Parser[Int] = hydra.parsers.alt(hydra.parsers.bind(hydra.parsers.char(92))((_x: Int) => hydra.json.parser.jsonEscapeChar))(hydra.parsers.satisfy((c: Int) =>
  hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.equality.equal[Int](c)(34)))(hydra.lib.logic.not(hydra.lib.equality.equal[Int](c)(92)))))

lazy val jsonValue: hydra.parsing.Parser[hydra.json.model.Value] = hydra.parsers.choice(Seq(hydra.json.parser.jsonNull,
   hydra.json.parser.jsonBool, hydra.json.parser.jsonNumber, hydra.json.parser.jsonString,
   hydra.json.parser.jsonArray, hydra.json.parser.jsonObject))

def parseJson(input: scala.Predef.String): hydra.parsing.ParseResult[hydra.json.model.Value] =
  hydra.parsers.bind(hydra.json.parser.whitespace)((_x: Unit) =>
  hydra.parsers.bind(hydra.json.parser.jsonValue)((v: hydra.json.model.Value) =>
  hydra.parsers.bind(hydra.json.parser.whitespace)((_2: Unit) =>
  hydra.parsers.bind(hydra.parsers.eof)((_3: Unit) => hydra.parsers.pure(v)))))(input)

def token[T0](p: hydra.parsing.Parser[T0]): hydra.parsing.Parser[T0] =
  hydra.parsers.bind(p)((x: T0) =>
  hydra.parsers.bind(hydra.json.parser.whitespace)((_x: Unit) => hydra.parsers.pure(x)))

lazy val whitespace: hydra.parsing.Parser[Unit] = hydra.parsers.map((_x: Seq[Int]) => ())(hydra.parsers.many(hydra.parsers.satisfy((c: Int) =>
  hydra.lib.lists.foldl[Boolean, Boolean](hydra.lib.logic.or)(false)(Seq(hydra.lib.equality.equal[Int](c)(32),
     hydra.lib.equality.equal[Int](c)(9), hydra.lib.equality.equal[Int](c)(10), hydra.lib.equality.equal[Int](c)(13))))))
