package hydra.formatting

import hydra.util.*

import hydra.lib.chars

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def capitalize(v1: scala.Predef.String): scala.Predef.String = hydra.formatting.mapFirstLetter(strings.toUpper)(v1)

def convertCase(from: hydra.util.CaseConvention)(to: hydra.util.CaseConvention)(original: scala.Predef.String): scala.Predef.String =
  {
  val parts: Seq[scala.Predef.String] = {
    val byCaps: Seq[scala.Predef.String] = {
      def splitOnUppercase(acc: Seq[Seq[Int]])(c: Int): Seq[Seq[Int]] =
        lists.concat2[Seq[Int]](logic.ifElse[Seq[Seq[Int]]](chars.isUpper(c))(Seq(Seq()))(Seq()))(lists.cons[Seq[Int]](lists.cons[Int](c)(lists.head[Seq[Int]](acc)))(lists.tail[Seq[Int]](acc)))
      lists.map[Seq[Int], scala.Predef.String](strings.fromList)(lists.foldl[Seq[Seq[Int]], Int](splitOnUppercase)(Seq(Seq()))(lists.reverse[Int](strings.toList(hydra.formatting.decapitalize(original)))))
    }
    val byUnderscores: Seq[scala.Predef.String] = strings.splitOn("_")(original)
    from match
      case hydra.util.CaseConvention.camel => byCaps
      case hydra.util.CaseConvention.pascal => byCaps
      case hydra.util.CaseConvention.lowerSnake => byUnderscores
      case hydra.util.CaseConvention.upperSnake => byUnderscores
  }
  to match
    case hydra.util.CaseConvention.camel => hydra.formatting.decapitalize(strings.cat(lists.map[scala.Predef.String,
       scala.Predef.String]((`arg_`: scala.Predef.String) => hydra.formatting.capitalize(strings.toLower(`arg_`)))(parts)))
    case hydra.util.CaseConvention.pascal => strings.cat(lists.map[scala.Predef.String, scala.Predef.String]((`arg_`: scala.Predef.String) => hydra.formatting.capitalize(strings.toLower(`arg_`)))(parts))
    case hydra.util.CaseConvention.lowerSnake => strings.intercalate("_")(lists.map[scala.Predef.String, scala.Predef.String](strings.toLower)(parts))
    case hydra.util.CaseConvention.upperSnake => strings.intercalate("_")(lists.map[scala.Predef.String, scala.Predef.String](strings.toUpper)(parts))
}

def convertCaseCamelToLowerSnake(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.lowerSnake)(v1)

def convertCaseCamelToUpperSnake(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.upperSnake)(v1)

def convertCasePascalToUpperSnake(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(hydra.util.CaseConvention.upperSnake)(v1)

def decapitalize(v1: scala.Predef.String): scala.Predef.String = hydra.formatting.mapFirstLetter(strings.toLower)(v1)

def escapeWithUnderscore(reserved: scala.collection.immutable.Set[scala.Predef.String])(s: scala.Predef.String): scala.Predef.String =
  logic.ifElse[scala.Predef.String](sets.member[scala.Predef.String](s)(reserved))(strings.cat2(s)("_"))(s)

def indentLines(s: scala.Predef.String): scala.Predef.String =
  {
  def indent(l: scala.Predef.String): scala.Predef.String = strings.cat2("    ")(l)
  strings.unlines(lists.map[scala.Predef.String, scala.Predef.String](indent)(strings.lines(s)))
}

def javaStyleComment(s: scala.Predef.String): scala.Predef.String = strings.cat2(strings.cat2(strings.cat2("/**\n")(" * "))(s))("\n */")

def mapFirstLetter(mapping: (scala.Predef.String => scala.Predef.String))(s: scala.Predef.String): scala.Predef.String =
  {
  val list: Seq[Int] = strings.toList(s)
  val firstLetter: scala.Predef.String = mapping(strings.fromList(lists.pure[Int](lists.head[Int](list))))
  logic.ifElse[scala.Predef.String](strings.`null`(s))(s)(strings.cat2(firstLetter)(strings.fromList(lists.tail[Int](list))))
}

def nonAlnumToUnderscores(input: scala.Predef.String): scala.Predef.String =
  {
  def isAlnum(c: Int): Boolean =
    logic.or(logic.and(equality.gte[Int](c)(65))(equality.lte[Int](c)(90)))(logic.or(logic.and(equality.gte[Int](c)(97))(equality.lte[Int](c)(122)))(logic.and(equality.gte[Int](c)(48))(equality.lte[Int](c)(57))))
  def replace(p: Tuple2[Seq[Int], Boolean])(c: Int): Tuple2[Seq[Int], Boolean] =
    {
    val s: Seq[Int] = pairs.first[Seq[Int], Boolean](p)
    val b: Boolean = pairs.second[Seq[Int], Boolean](p)
    logic.ifElse[Tuple2[Seq[Int], Boolean]](isAlnum(c))(Tuple2(lists.cons[Int](c)(s), false))(logic.ifElse[Tuple2[Seq[Int],
       Boolean]](b)(Tuple2(s, true))(Tuple2(lists.cons[Int](95)(s), true)))
  }
  val result: Tuple2[Seq[Int], Boolean] = lists.foldl[Tuple2[Seq[Int], Boolean], Int](replace)(Tuple2(Seq(), false))(strings.toList(input))
  strings.fromList(lists.reverse[Int](pairs.first[Seq[Int], Boolean](result)))
}

def sanitizeWithUnderscores(reserved: scala.collection.immutable.Set[scala.Predef.String])(s: scala.Predef.String): scala.Predef.String =
  hydra.formatting.escapeWithUnderscore(reserved)(hydra.formatting.nonAlnumToUnderscores(s))

def showList[T0](f: (T0 => scala.Predef.String))(els: Seq[T0]): scala.Predef.String =
  strings.cat(Seq("[", strings.intercalate(", ")(lists.map[T0, scala.Predef.String](f)(els)), "]"))

def stripLeadingAndTrailingWhitespace(s: scala.Predef.String): scala.Predef.String =
  strings.fromList(lists.dropWhile[Int](chars.isSpace)(lists.reverse[Int](lists.dropWhile[Int](chars.isSpace)(lists.reverse[Int](strings.toList(s))))))

def withCharacterAliases(original: scala.Predef.String): scala.Predef.String =
  {
  val aliases: Map[Int, scala.Predef.String] = maps.fromList[Int, scala.Predef.String](Seq(Tuple2(32,
     "sp"), Tuple2(33, "excl"), Tuple2(34, "quot"), Tuple2(35, "num"), Tuple2(36, "dollar"), Tuple2(37,
     "percnt"), Tuple2(38, "amp"), Tuple2(39, "apos"), Tuple2(40, "lpar"), Tuple2(41, "rpar"), Tuple2(42,
     "ast"), Tuple2(43, "plus"), Tuple2(44, "comma"), Tuple2(45, "minus"), Tuple2(46, "period"), Tuple2(47,
     "sol"), Tuple2(58, "colon"), Tuple2(59, "semi"), Tuple2(60, "lt"), Tuple2(61, "equals"), Tuple2(62,
     "gt"), Tuple2(63, "quest"), Tuple2(64, "commat"), Tuple2(91, "lsqb"), Tuple2(92, "bsol"), Tuple2(93,
     "rsqb"), Tuple2(94, "circ"), Tuple2(95, "lowbar"), Tuple2(96, "grave"), Tuple2(123, "lcub"), Tuple2(124,
     "verbar"), Tuple2(125, "rcub"), Tuple2(126, "tilde")))
  def alias(c: Int): Seq[Int] =
    maybes.fromMaybe[Seq[Int]](lists.pure[Int](c))(maybes.map[scala.Predef.String, Seq[Int]](strings.toList)(maps.lookup[Int, scala.Predef.String](c)(aliases)))
  strings.fromList(lists.filter[Int](chars.isAlphaNum)(lists.concat[Int](lists.map[Int, Seq[Int]](alias)(strings.toList(original)))))
}

def wrapLine(maxlen: Int)(input: scala.Predef.String): scala.Predef.String =
  {
  def helper(prev: Seq[Seq[Int]])(rem: Seq[Int]): Seq[Seq[Int]] =
    {
    val trunc: Seq[Int] = lists.take[Int](maxlen)(rem)
    val spanResult: Tuple2[Seq[Int], Seq[Int]] = lists.span[Int]((c: Int) =>
      logic.and(logic.not(equality.equal[Int](c)(32)))(logic.not(equality.equal[Int](c)(9))))(lists.reverse[Int](trunc))
    val prefix: Seq[Int] = lists.reverse[Int](pairs.second[Seq[Int], Seq[Int]](spanResult))
    val suffix: Seq[Int] = lists.reverse[Int](pairs.first[Seq[Int], Seq[Int]](spanResult))
    logic.ifElse[Seq[Seq[Int]]](equality.lte[Int](lists.length[Int](rem))(maxlen))(lists.reverse[Seq[Int]](lists.cons[Seq[Int]](rem)(prev)))(logic.ifElse[Seq[Seq[Int]]](lists.`null`[Int](prefix))(helper(lists.cons[Seq[Int]](trunc)(prev))(lists.drop[Int](maxlen)(rem)))(helper(lists.cons[Seq[Int]](lists.init[Int](prefix))(prev))(lists.concat2[Int](suffix)(lists.drop[Int](maxlen)(rem)))))
  }
  strings.fromList(lists.intercalate[Int](Seq(10))(helper(Seq())(strings.toList(input))))
}
