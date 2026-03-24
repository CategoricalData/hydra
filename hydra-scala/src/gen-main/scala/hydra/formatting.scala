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

def capitalize(v1: scala.Predef.String): scala.Predef.String = hydra.formatting.mapFirstLetter(hydra.lib.strings.toUpper)(v1)

def convertCase(from: hydra.util.CaseConvention)(to: hydra.util.CaseConvention)(original: scala.Predef.String): scala.Predef.String =
  {
  lazy val parts: Seq[scala.Predef.String] = {
    lazy val byCaps: Seq[scala.Predef.String] = {
      def splitOnUppercase(acc: Seq[Seq[Int]])(c: Int): Seq[Seq[Int]] =
        hydra.lib.lists.concat2[Seq[Int]](hydra.lib.logic.ifElse[Seq[Seq[Int]]](hydra.lib.chars.isUpper(c))(Seq(Seq()))(Seq()))(hydra.lib.lists.cons[Seq[Int]](hydra.lib.lists.cons[Int](c)(hydra.lib.lists.head[Seq[Int]](acc)))(hydra.lib.lists.tail[Seq[Int]](acc)))
      hydra.lib.lists.map[Seq[Int], scala.Predef.String](hydra.lib.strings.fromList)(hydra.lib.lists.foldl[Seq[Seq[Int]], Int](splitOnUppercase)(Seq(Seq()))(hydra.lib.lists.reverse[Int](hydra.lib.strings.toList(hydra.formatting.decapitalize(original)))))
    }
    lazy val byUnderscores: Seq[scala.Predef.String] = hydra.lib.strings.splitOn("_")(original)
    from match
      case hydra.util.CaseConvention.camel => byCaps
      case hydra.util.CaseConvention.pascal => byCaps
      case hydra.util.CaseConvention.lowerSnake => byUnderscores
      case hydra.util.CaseConvention.upperSnake => byUnderscores
  }
  to match
    case hydra.util.CaseConvention.camel => hydra.formatting.decapitalize(hydra.lib.strings.cat(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((`arg_`: scala.Predef.String) =>
      hydra.formatting.capitalize(hydra.lib.strings.toLower(`arg_`)))(parts)))
    case hydra.util.CaseConvention.pascal => hydra.lib.strings.cat(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((`arg_`: scala.Predef.String) =>
      hydra.formatting.capitalize(hydra.lib.strings.toLower(`arg_`)))(parts))
    case hydra.util.CaseConvention.lowerSnake => hydra.lib.strings.intercalate("_")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String](hydra.lib.strings.toLower)(parts))
    case hydra.util.CaseConvention.upperSnake => hydra.lib.strings.intercalate("_")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String](hydra.lib.strings.toUpper)(parts))
}

def convertCaseCamelOrUnderscoreToLowerSnake(s: scala.Predef.String): scala.Predef.String =
  {
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn("_")(s)
  lazy val snakeParts: Seq[scala.Predef.String] = hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((p: scala.Predef.String) => hydra.formatting.convertCaseCamelToLowerSnake(p))(parts)
  hydra.lib.strings.intercalate("_")(snakeParts)
}

def convertCaseCamelToLowerSnake(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.lowerSnake)(v1)

def convertCaseCamelToUpperSnake(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.upperSnake)(v1)

def convertCasePascalToUpperSnake(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(hydra.util.CaseConvention.upperSnake)(v1)

def decapitalize(v1: scala.Predef.String): scala.Predef.String = hydra.formatting.mapFirstLetter(hydra.lib.strings.toLower)(v1)

def escapeWithUnderscore(reserved: scala.collection.immutable.Set[scala.Predef.String])(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.sets.member[scala.Predef.String](s)(reserved))(hydra.lib.strings.cat2(s)("_"))(s)

def indentLines(s: scala.Predef.String): scala.Predef.String =
  {
  def indent(l: scala.Predef.String): scala.Predef.String = hydra.lib.strings.cat2("    ")(l)
  hydra.lib.strings.unlines(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String](indent)(hydra.lib.strings.lines(s)))
}

def javaStyleComment(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("/**\n")(" * "))(s))("\n */")

def mapFirstLetter(mapping: (scala.Predef.String => scala.Predef.String))(s: scala.Predef.String): scala.Predef.String =
  {
  lazy val list: Seq[Int] = hydra.lib.strings.toList(s)
  lazy val firstLetter: scala.Predef.String = mapping(hydra.lib.strings.fromList(hydra.lib.lists.pure[Int](hydra.lib.lists.head[Int](list))))
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.strings.`null`(s))(s)(hydra.lib.strings.cat2(firstLetter)(hydra.lib.strings.fromList(hydra.lib.lists.tail[Int](list))))
}

def nonAlnumToUnderscores(input: scala.Predef.String): scala.Predef.String =
  {
  def isAlnum(c: Int): Boolean =
    hydra.lib.logic.or(hydra.lib.logic.and(hydra.lib.equality.gte[Int](c)(65))(hydra.lib.equality.lte[Int](c)(90)))(hydra.lib.logic.or(hydra.lib.logic.and(hydra.lib.equality.gte[Int](c)(97))(hydra.lib.equality.lte[Int](c)(122)))(hydra.lib.logic.and(hydra.lib.equality.gte[Int](c)(48))(hydra.lib.equality.lte[Int](c)(57))))
  def replace(p: Tuple2[Seq[Int], Boolean])(c: Int): Tuple2[Seq[Int], Boolean] =
    {
    lazy val s: Seq[Int] = hydra.lib.pairs.first[Seq[Int], Boolean](p)
    lazy val b: Boolean = hydra.lib.pairs.second[Seq[Int], Boolean](p)
    hydra.lib.logic.ifElse[Tuple2[Seq[Int], Boolean]](isAlnum(c))(Tuple2(hydra.lib.lists.cons[Int](c)(s), false))(hydra.lib.logic.ifElse[Tuple2[Seq[Int], Boolean]](b)(Tuple2(s, true))(Tuple2(hydra.lib.lists.cons[Int](95)(s), true)))
  }
  lazy val result: Tuple2[Seq[Int], Boolean] = hydra.lib.lists.foldl[Tuple2[Seq[Int], Boolean], Int](replace)(Tuple2(Seq(), false))(hydra.lib.strings.toList(input))
  hydra.lib.strings.fromList(hydra.lib.lists.reverse[Int](hydra.lib.pairs.first[Seq[Int], Boolean](result)))
}

def sanitizeWithUnderscores(reserved: scala.collection.immutable.Set[scala.Predef.String])(s: scala.Predef.String): scala.Predef.String =
  hydra.formatting.escapeWithUnderscore(reserved)(hydra.formatting.nonAlnumToUnderscores(s))

def showList[T0](f: (T0 => scala.Predef.String))(els: Seq[T0]): scala.Predef.String =
  hydra.lib.strings.cat(Seq("[", hydra.lib.strings.intercalate(", ")(hydra.lib.lists.map[T0, scala.Predef.String](f)(els)), "]"))

def stripLeadingAndTrailingWhitespace(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.fromList(hydra.lib.lists.dropWhile[Int](hydra.lib.chars.isSpace)(hydra.lib.lists.reverse[Int](hydra.lib.lists.dropWhile[Int](hydra.lib.chars.isSpace)(hydra.lib.lists.reverse[Int](hydra.lib.strings.toList(s))))))

def withCharacterAliases(original: scala.Predef.String): scala.Predef.String =
  {
  lazy val aliases: Map[Int, scala.Predef.String] = hydra.lib.maps.fromList[Int, scala.Predef.String](Seq(Tuple2(32, "sp"), Tuple2(33, "excl"), Tuple2(34, "quot"), Tuple2(35, "num"), Tuple2(36, "dollar"), Tuple2(37, "percnt"), Tuple2(38, "amp"), Tuple2(39, "apos"), Tuple2(40, "lpar"), Tuple2(41, "rpar"), Tuple2(42, "ast"), Tuple2(43, "plus"), Tuple2(44, "comma"), Tuple2(45, "minus"), Tuple2(46, "period"), Tuple2(47, "sol"), Tuple2(58, "colon"), Tuple2(59, "semi"), Tuple2(60, "lt"), Tuple2(61, "equals"), Tuple2(62, "gt"), Tuple2(63, "quest"), Tuple2(64, "commat"), Tuple2(91, "lsqb"), Tuple2(92, "bsol"), Tuple2(93, "rsqb"), Tuple2(94, "circ"), Tuple2(95, "lowbar"), Tuple2(96, "grave"), Tuple2(123, "lcub"), Tuple2(124, "verbar"), Tuple2(125, "rcub"), Tuple2(126, "tilde")))
  def alias(c: Int): Seq[Int] =
    hydra.lib.maybes.fromMaybe[Seq[Int]](hydra.lib.lists.pure[Int](c))(hydra.lib.maybes.map[scala.Predef.String, Seq[Int]](hydra.lib.strings.toList)(hydra.lib.maps.lookup[Int, scala.Predef.String](c)(aliases)))
  hydra.lib.strings.fromList(hydra.lib.lists.filter[Int](hydra.lib.chars.isAlphaNum)(hydra.lib.lists.concat[Int](hydra.lib.lists.map[Int, Seq[Int]](alias)(hydra.lib.strings.toList(original)))))
}

def wrapLine(maxlen: Int)(input: scala.Predef.String): scala.Predef.String =
  {
  def helper(prev: Seq[Seq[Int]])(rem: Seq[Int]): Seq[Seq[Int]] =
    {
    lazy val trunc: Seq[Int] = hydra.lib.lists.take[Int](maxlen)(rem)
    lazy val spanResult: Tuple2[Seq[Int], Seq[Int]] = hydra.lib.lists.span[Int]((c: Int) =>
      hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.equality.equal[Int](c)(32)))(hydra.lib.logic.not(hydra.lib.equality.equal[Int](c)(9))))(hydra.lib.lists.reverse[Int](trunc))
    lazy val prefix: Seq[Int] = hydra.lib.lists.reverse[Int](hydra.lib.pairs.second[Seq[Int], Seq[Int]](spanResult))
    lazy val suffix: Seq[Int] = hydra.lib.lists.reverse[Int](hydra.lib.pairs.first[Seq[Int], Seq[Int]](spanResult))
    hydra.lib.logic.ifElse[Seq[Seq[Int]]](hydra.lib.equality.lte[Int](hydra.lib.lists.length[Int](rem))(maxlen))(hydra.lib.lists.reverse[Seq[Int]](hydra.lib.lists.cons[Seq[Int]](rem)(prev)))(hydra.lib.logic.ifElse[Seq[Seq[Int]]](hydra.lib.lists.`null`[Int](prefix))(helper(hydra.lib.lists.cons[Seq[Int]](trunc)(prev))(hydra.lib.lists.drop[Int](maxlen)(rem)))(helper(hydra.lib.lists.cons[Seq[Int]](hydra.lib.lists.init[Int](prefix))(prev))(hydra.lib.lists.concat2[Int](suffix)(hydra.lib.lists.drop[Int](maxlen)(rem)))))
  }
  hydra.lib.strings.fromList(hydra.lib.lists.intercalate[Int](Seq(10))(helper(Seq())(hydra.lib.strings.toList(input))))
}
