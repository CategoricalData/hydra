package hydra.parsers

import hydra.parsing.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maybes

import hydra.lib.strings

def alt[T0](p1: hydra.parsing.Parser[T0])(p2: hydra.parsing.Parser[T0]): hydra.parsing.Parser[T0] =
  {
  def parse(input: scala.Predef.String): hydra.parsing.ParseResult[T0] =
    p1(input) match
    case hydra.parsing.ParseResult.success(v_ParseResult_success_s) => hydra.parsing.ParseResult.success(v_ParseResult_success_s)
    case hydra.parsing.ParseResult.failure(v_ParseResult_failure_e) => logic.ifElse[hydra.parsing.ParseResult[T0]](equality.equal[scala.Predef.String](v_ParseResult_failure_e.remainder)(input))(p2(input))(hydra.parsing.ParseResult.failure(v_ParseResult_failure_e))
  parse
}

val anyChar: hydra.parsing.Parser[Int] = hydra.parsers.satisfy((_x: Int) => true)

def apply[T0, T1](pf: hydra.parsing.Parser[T0 => T1])(pa: hydra.parsing.Parser[T0]): hydra.parsing.Parser[T1] =
  {
  def parse(input: scala.Predef.String): hydra.parsing.ParseResult[T1] =
    pf(input) match
    case hydra.parsing.ParseResult.success(v_ParseResult_success_sf) => pa(v_ParseResult_success_sf.remainder) match
      case hydra.parsing.ParseResult.success(v_ParseResult_success_sa) => hydra.parsing.ParseResult.success(hydra.parsing.ParseSuccess(v_ParseResult_success_sf.value(v_ParseResult_success_sa.value),
         (v_ParseResult_success_sa.remainder)))
      case hydra.parsing.ParseResult.failure(v_ParseResult_failure_e) => hydra.parsing.ParseResult.failure(v_ParseResult_failure_e)
    case hydra.parsing.ParseResult.failure(v_ParseResult_failure_e) => hydra.parsing.ParseResult.failure(v_ParseResult_failure_e)
  parse
}

def between[T0, T1, T2](open: hydra.parsing.Parser[T0])(close: hydra.parsing.Parser[T1])(p: hydra.parsing.Parser[T2]): hydra.parsing.Parser[T2] =
  hydra.parsers.bind(open)((_x: T0) =>
  hydra.parsers.bind(p)((x: T2) => hydra.parsers.bind(close)((_2: T1) => hydra.parsers.pure(x))))

def bind[T0, T1](pa: hydra.parsing.Parser[T0])(f: (T0 => hydra.parsing.Parser[T1])): hydra.parsing.Parser[T1] =
  {
  def parse(input: scala.Predef.String): hydra.parsing.ParseResult[T1] =
    pa(input) match
    case hydra.parsing.ParseResult.success(v_ParseResult_success_s) => f(v_ParseResult_success_s.value)(v_ParseResult_success_s.remainder)
    case hydra.parsing.ParseResult.failure(v_ParseResult_failure_e) => hydra.parsing.ParseResult.failure(v_ParseResult_failure_e)
  parse
}

def char(c: Int): hydra.parsing.Parser[Int] = hydra.parsers.satisfy((x: Int) => equality.equal[Int](x)(c))

def choice[T0](ps: Seq[hydra.parsing.Parser[T0]]): hydra.parsing.Parser[T0] =
  lists.foldl[hydra.parsing.Parser[T0], hydra.parsing.Parser[T0]](hydra.parsers.alt)(hydra.parsers.fail("no choice matched"))(ps)

val eof: hydra.parsing.Parser[Unit] = (input: scala.Predef.String) =>
  logic.ifElse[hydra.parsing.ParseResult[Unit]](equality.equal[scala.Predef.String](input)(""))(hydra.parsing.ParseResult.success(hydra.parsing.ParseSuccess((),
     "")))(hydra.parsing.ParseResult.failure(hydra.parsing.ParseError("expected end of input", input)))

def fail[T0](msg: scala.Predef.String): hydra.parsing.Parser[T0] =
  (input: scala.Predef.String) =>
  hydra.parsing.ParseResult.failure(hydra.parsing.ParseError(msg, input))

def `lazy`[T0](f: (Unit => hydra.parsing.Parser[T0])): hydra.parsing.Parser[T0] = (input: scala.Predef.String) => f(())(input)

def many[T0](p: hydra.parsing.Parser[T0]): hydra.parsing.Parser[Seq[T0]] = hydra.parsers.alt(hydra.parsers.some(p))(hydra.parsers.pure(Seq()))

def map[T0, T1](f: (T0 => T1))(pa: hydra.parsing.Parser[T0]): hydra.parsing.Parser[T1] =
  {
  def parse(input: scala.Predef.String): hydra.parsing.ParseResult[T1] =
    pa(input) match
    case hydra.parsing.ParseResult.success(v_ParseResult_success_s) => hydra.parsing.ParseResult.success(hydra.parsing.ParseSuccess(f(v_ParseResult_success_s.value),
       (v_ParseResult_success_s.remainder)))
    case hydra.parsing.ParseResult.failure(v_ParseResult_failure_e) => hydra.parsing.ParseResult.failure(v_ParseResult_failure_e)
  parse
}

def optional[T0](p: hydra.parsing.Parser[T0]): hydra.parsing.Parser[Option[T0]] =
  hydra.parsers.alt(hydra.parsers.map(maybes.pure[T0])(p))(hydra.parsers.pure(None))

def pure[T0](a: T0): hydra.parsing.Parser[T0] =
  (input: scala.Predef.String) =>
  hydra.parsing.ParseResult.success(hydra.parsing.ParseSuccess(a, input))

def runParser[T0](p: hydra.parsing.Parser[T0])(input: scala.Predef.String): hydra.parsing.ParseResult[T0] = p(input)

def satisfy(pred: (Int => Boolean)): hydra.parsing.Parser[Int] =
  {
  def parse(input: scala.Predef.String): hydra.parsing.ParseResult[Int] =
    {
    val codes: Seq[Int] = strings.toList(input)
    maybes.maybe[hydra.parsing.ParseResult[Int], Int](hydra.parsing.ParseResult.failure(hydra.parsing.ParseError("unexpected end of input", input)))((c: Int) =>
      {
      val rest: scala.Predef.String = strings.fromList(lists.drop[Int](1)(codes))
      logic.ifElse[hydra.parsing.ParseResult[Int]](pred(c))(hydra.parsing.ParseResult.success(hydra.parsing.ParseSuccess(c,
         rest)))(hydra.parsing.ParseResult.failure(hydra.parsing.ParseError("character did not satisfy predicate",
         input)))
    })(lists.safeHead[Int](codes))
  }
  parse
}

def sepBy[T0, T1](p: hydra.parsing.Parser[T0])(sep: hydra.parsing.Parser[T1]): hydra.parsing.Parser[Seq[T0]] = hydra.parsers.alt(hydra.parsers.sepBy1(p)(sep))(hydra.parsers.pure(Seq()))

def sepBy1[T0, T1](p: hydra.parsing.Parser[T0])(sep: hydra.parsing.Parser[T1]): hydra.parsing.Parser[Seq[T0]] =
  hydra.parsers.bind(p)((x: T0) =>
  hydra.parsers.bind(hydra.parsers.many(hydra.parsers.bind(sep)((_x: T1) => p)))((xs: Seq[T0]) => hydra.parsers.pure(lists.cons[T0](x)(xs))))

def some[T0](p: hydra.parsing.Parser[T0]): hydra.parsing.Parser[Seq[T0]] =
  hydra.parsers.bind(p)((x: T0) =>
  hydra.parsers.bind(hydra.parsers.many(p))((xs: Seq[T0]) => hydra.parsers.pure(lists.cons[T0](x)(xs))))

def string(str: scala.Predef.String): hydra.parsing.Parser[scala.Predef.String] =
  (input: scala.Predef.String) =>
  {
  val strCodes: Seq[Int] = strings.toList(str)
  {
    val inputCodes: Seq[Int] = strings.toList(input)
    {
      val strLen: Int = lists.length[Int](strCodes)
      {
        val inputPrefix: Seq[Int] = lists.take[Int](strLen)(inputCodes)
        logic.ifElse[hydra.parsing.ParseResult[scala.Predef.String]](equality.equal[Seq[Int]](strCodes)(inputPrefix))(hydra.parsing.ParseResult.success(hydra.parsing.ParseSuccess(str,
           strings.fromList(lists.drop[Int](strLen)(inputCodes)))))(hydra.parsing.ParseResult.failure(hydra.parsing.ParseError(strings.cat2("expected: ")(str),
           input)))
      }
    }
  }
}
