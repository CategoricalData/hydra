package hydra.grammars

import hydra.core.*

import hydra.grammar.*

import hydra.module.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.strings

def childName(lname: scala.Predef.String)(n: scala.Predef.String): scala.Predef.String = strings.cat(Seq(lname, "_", hydra.formatting.capitalize(n)))

def findNames(pats: Seq[hydra.grammar.Pattern]): Seq[scala.Predef.String] =
  {
  def nextName(acc: Tuple2[Seq[scala.Predef.String], Map[scala.Predef.String, Int]])(pat: hydra.grammar.Pattern): Tuple2[Seq[scala.Predef.String],
     Map[scala.Predef.String, Int]] =
    {
    val names: Seq[scala.Predef.String] = pairs.first[Seq[scala.Predef.String], Map[scala.Predef.String, Int]](acc)
    val nameMap: Map[scala.Predef.String, Int] = pairs.second[Seq[scala.Predef.String], Map[scala.Predef.String, Int]](acc)
    val rn: scala.Predef.String = hydra.grammars.rawName(pat)
    val nameAndIndex: Tuple2[scala.Predef.String, Int] = maybes.maybe[Tuple2[scala.Predef.String, Int], Int](Tuple2(rn, 1))((i: Int) =>
      Tuple2(strings.cat2(rn)(literals.showInt32(math.add(i)(1))), math.add(i)(1)))(maps.lookup[scala.Predef.String, Int](rn)(nameMap))
    val nn: scala.Predef.String = pairs.first[scala.Predef.String, Int](nameAndIndex)
    val ni: Int = pairs.second[scala.Predef.String, Int](nameAndIndex)
    Tuple2(lists.cons[scala.Predef.String](nn)(names), maps.insert[scala.Predef.String, Int](rn)(ni)(nameMap))
  }
  lists.reverse[scala.Predef.String](pairs.first[Seq[scala.Predef.String], Map[scala.Predef.String, Int]](lists.foldl[Tuple2[Seq[scala.Predef.String],
     Map[scala.Predef.String, Int]], hydra.grammar.Pattern](nextName)(Tuple2(Seq(), maps.empty[scala.Predef.String,
     Int]))(pats)))
}

def grammarToModule(ns: hydra.module.Namespace)(grammar: hydra.grammar.Grammar)(desc: Option[scala.Predef.String]): hydra.module.Module =
  {
  val prodPairs: Seq[Tuple2[scala.Predef.String, hydra.grammar.Pattern]] = lists.map[hydra.grammar.Production,
     Tuple2[scala.Predef.String, hydra.grammar.Pattern]]((prod: hydra.grammar.Production) => Tuple2(prod.symbol,
     (prod.pattern)))(grammar)
  val capitalizedNames: Seq[scala.Predef.String] = lists.map[Tuple2[scala.Predef.String, hydra.grammar.Pattern],
     scala.Predef.String]((pair: Tuple2[scala.Predef.String, hydra.grammar.Pattern]) =>
    hydra.formatting.capitalize(pairs.first[scala.Predef.String, hydra.grammar.Pattern](pair)))(prodPairs)
  val patterns: Seq[hydra.grammar.Pattern] = lists.map[Tuple2[scala.Predef.String, hydra.grammar.Pattern],
     hydra.grammar.Pattern]((pair: Tuple2[scala.Predef.String, hydra.grammar.Pattern]) =>
    pairs.second[scala.Predef.String, hydra.grammar.Pattern](pair))(prodPairs)
  val elementPairs: Seq[Tuple2[scala.Predef.String, hydra.core.Type]] = lists.concat[Tuple2[scala.Predef.String,
     hydra.core.Type]](lists.zipWith[scala.Predef.String, hydra.grammar.Pattern, Seq[Tuple2[scala.Predef.String,
     hydra.core.Type]]]((v1: scala.Predef.String) =>
    (v2: hydra.grammar.Pattern) => hydra.grammars.makeElements(false)(ns)(v1)(v2))(capitalizedNames)(patterns))
  val elements: Seq[hydra.core.Binding] = lists.map[Tuple2[scala.Predef.String, hydra.core.Type], hydra.core.Binding]((pair: Tuple2[scala.Predef.String,
     hydra.core.Type]) =>
    {
    val lname: scala.Predef.String = pairs.first[scala.Predef.String, hydra.core.Type](pair)
    {
      val elName: hydra.core.Name = hydra.grammars.toName(ns)(lname)
      {
        val typ: hydra.core.Type = hydra.grammars.replacePlaceholders(elName)(hydra.grammars.wrapType(pairs.second[scala.Predef.String, hydra.core.Type](pair)))
        hydra.annotations.typeElement(elName)(typ)
      }
    }
  })(elementPairs)
  hydra.module.Module(ns, elements, Seq(), Seq(), desc)
}

def isComplex(pat: hydra.grammar.Pattern): Boolean =
  pat match
  case hydra.grammar.Pattern.labeled(v_Pattern_labeled_lp) => hydra.grammars.isComplex(v_Pattern_labeled_lp.pattern)
  case hydra.grammar.Pattern.sequence(v_Pattern_sequence_pats) => hydra.grammars.isNontrivial(true)(v_Pattern_sequence_pats)
  case hydra.grammar.Pattern.alternatives(v_Pattern_alternatives_pats) => hydra.grammars.isNontrivial(false)(v_Pattern_alternatives_pats)
  case _ => false

def isNontrivial(isRecord: Boolean)(pats: Seq[hydra.grammar.Pattern]): Boolean =
  {
  val minPats: Seq[hydra.grammar.Pattern] = hydra.grammars.simplify(isRecord)(pats)
  def isLabeled(p: hydra.grammar.Pattern): Boolean =
    p match
    case hydra.grammar.Pattern.labeled(v_Pattern_labeled__) => true
    case _ => false
  logic.ifElse[Boolean](equality.equal[Int](lists.length[hydra.grammar.Pattern](minPats))(1))(isLabeled(lists.head[hydra.grammar.Pattern](minPats)))(true)
}

def makeElements(omitTrivial: Boolean)(ns: hydra.module.Namespace)(lname: scala.Predef.String)(pat: hydra.grammar.Pattern): Seq[Tuple2[scala.Predef.String,
   hydra.core.Type]] =
  {
  val trivial: Seq[Tuple2[scala.Predef.String, hydra.core.Type]] = logic.ifElse[Seq[Tuple2[scala.Predef.String,
     hydra.core.Type]]](omitTrivial)(Seq())(Seq(Tuple2(lname, hydra.core.Type.unit)))
  def descend[T0](n: scala.Predef.String)(f: (Seq[Tuple2[scala.Predef.String, hydra.core.Type]] => T0))(p: hydra.grammar.Pattern): T0 =
    {
    val cpairs: Seq[Tuple2[scala.Predef.String, hydra.core.Type]] = hydra.grammars.makeElements(false)(ns)(hydra.grammars.childName(lname)(n))(p)
    f(logic.ifElse[Seq[Tuple2[scala.Predef.String, hydra.core.Type]]](hydra.grammars.isComplex(p))(lists.cons[Tuple2[scala.Predef.String,
       hydra.core.Type]](Tuple2(lname, hydra.core.Type.variable(hydra.grammars.toName(ns)(pairs.first[scala.Predef.String,
       hydra.core.Type](lists.head[Tuple2[scala.Predef.String, hydra.core.Type]](cpairs))))))(cpairs))(logic.ifElse[Seq[Tuple2[scala.Predef.String,
       hydra.core.Type]]](lists.`null`[Tuple2[scala.Predef.String, hydra.core.Type]](cpairs))(Seq(Tuple2(lname,
       hydra.core.Type.unit)))(lists.cons[Tuple2[scala.Predef.String, hydra.core.Type]](Tuple2(lname,
       pairs.second[scala.Predef.String, hydra.core.Type](lists.head[Tuple2[scala.Predef.String, hydra.core.Type]](cpairs))))(lists.tail[Tuple2[scala.Predef.String,
       hydra.core.Type]](cpairs)))))
  }
  def mod(n: scala.Predef.String)(f: (hydra.core.Type => hydra.core.Type))(p: hydra.grammar.Pattern): Seq[Tuple2[scala.Predef.String, hydra.core.Type]] =
    descend(n)((pairs: Seq[Tuple2[scala.Predef.String, hydra.core.Type]]) =>
    lists.cons[Tuple2[scala.Predef.String, hydra.core.Type]](Tuple2(lname, f(pairs.second[scala.Predef.String,
       hydra.core.Type](lists.head[Tuple2[scala.Predef.String, hydra.core.Type]](pairs)))))(lists.tail[Tuple2[scala.Predef.String,
       hydra.core.Type]](pairs)))(p)
  def forPat(pat2: hydra.grammar.Pattern): Seq[Tuple2[scala.Predef.String, hydra.core.Type]] =
    pat2 match
    case hydra.grammar.Pattern.alternatives(v_Pattern_alternatives_pats) => forRecordOrUnion(false)((fields: Seq[hydra.core.FieldType]) => hydra.core.Type.union(fields))(v_Pattern_alternatives_pats)
    case hydra.grammar.Pattern.constant(v_Pattern_constant__) => trivial
    case hydra.grammar.Pattern.ignored(v_Pattern_ignored__) => Seq()
    case hydra.grammar.Pattern.labeled(v_Pattern_labeled_lp) => forPat(v_Pattern_labeled_lp.pattern)
    case hydra.grammar.Pattern.nil => trivial
    case hydra.grammar.Pattern.nonterminal(v_Pattern_nonterminal_s) => Seq(Tuple2(lname, hydra.core.Type.variable(hydra.grammars.toName(ns)(v_Pattern_nonterminal_s))))
    case hydra.grammar.Pattern.option(v_Pattern_option_p) => mod("Option")((x: hydra.core.Type) => hydra.core.Type.maybe(x))(v_Pattern_option_p)
    case hydra.grammar.Pattern.plus(v_Pattern_plus_p) => mod("Elmt")((x: hydra.core.Type) => hydra.core.Type.list(x))(v_Pattern_plus_p)
    case hydra.grammar.Pattern.regex(v_Pattern_regex__) => Seq(Tuple2(lname, hydra.core.Type.literal(hydra.core.LiteralType.string)))
    case hydra.grammar.Pattern.sequence(v_Pattern_sequence_pats) => forRecordOrUnion(true)((fields: Seq[hydra.core.FieldType]) => hydra.core.Type.record(fields))(v_Pattern_sequence_pats)
    case hydra.grammar.Pattern.star(v_Pattern_star_p) => mod("Elmt")((x: hydra.core.Type) => hydra.core.Type.list(x))(v_Pattern_star_p)
  def forRecordOrUnion(isRecord: Boolean)(construct: (Seq[hydra.core.FieldType] => hydra.core.Type))(pats: Seq[hydra.grammar.Pattern]): Seq[Tuple2[scala.Predef.String,
     hydra.core.Type]] =
    {
    val minPats: Seq[hydra.grammar.Pattern] = hydra.grammars.simplify(isRecord)(pats)
    val fieldNames: Seq[scala.Predef.String] = hydra.grammars.findNames(minPats)
    def toField(n: scala.Predef.String)(p: hydra.grammar.Pattern): Tuple2[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]] =
      descend(n)((pairs: Seq[Tuple2[scala.Predef.String, hydra.core.Type]]) =>
      Tuple2(hydra.core.FieldType(n, pairs.second[scala.Predef.String, hydra.core.Type](lists.head[Tuple2[scala.Predef.String,
         hydra.core.Type]](pairs))), lists.tail[Tuple2[scala.Predef.String, hydra.core.Type]](pairs)))(p)
    val fieldPairs: Seq[Tuple2[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]]] = lists.zipWith[scala.Predef.String,
       hydra.grammar.Pattern, Tuple2[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]]](toField)(fieldNames)(minPats)
    val fields: Seq[hydra.core.FieldType] = lists.map[Tuple2[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String,
       hydra.core.Type]]], hydra.core.FieldType](pairs.first[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String,
       hydra.core.Type]]])(fieldPairs)
    val els: Seq[Tuple2[scala.Predef.String, hydra.core.Type]] = lists.concat[Tuple2[scala.Predef.String,
       hydra.core.Type]](lists.map[Tuple2[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]],
       Seq[Tuple2[scala.Predef.String, hydra.core.Type]]](pairs.second[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String,
       hydra.core.Type]]])(fieldPairs))
    logic.ifElse[Seq[Tuple2[scala.Predef.String, hydra.core.Type]]](hydra.grammars.isNontrivial(isRecord)(pats))(lists.cons[Tuple2[scala.Predef.String,
       hydra.core.Type]](Tuple2(lname, construct(fields)))(els))(forPat(lists.head[hydra.grammar.Pattern](minPats)))
  }
  forPat(pat)
}

def rawName(pat: hydra.grammar.Pattern): scala.Predef.String =
  pat match
  case hydra.grammar.Pattern.alternatives(v_Pattern_alternatives__) => "alts"
  case hydra.grammar.Pattern.constant(v_Pattern_constant_c) => hydra.formatting.capitalize(hydra.formatting.withCharacterAliases(v_Pattern_constant_c))
  case hydra.grammar.Pattern.ignored(v_Pattern_ignored__) => "ignored"
  case hydra.grammar.Pattern.labeled(v_Pattern_labeled_lp) => (v_Pattern_labeled_lp.label)
  case hydra.grammar.Pattern.nil => "none"
  case hydra.grammar.Pattern.nonterminal(v_Pattern_nonterminal_s) => hydra.formatting.capitalize(v_Pattern_nonterminal_s)
  case hydra.grammar.Pattern.option(v_Pattern_option_p) => hydra.formatting.capitalize(hydra.grammars.rawName(v_Pattern_option_p))
  case hydra.grammar.Pattern.plus(v_Pattern_plus_p) => strings.cat2("listOf")(hydra.formatting.capitalize(hydra.grammars.rawName(v_Pattern_plus_p)))
  case hydra.grammar.Pattern.regex(v_Pattern_regex__) => "regex"
  case hydra.grammar.Pattern.sequence(v_Pattern_sequence__) => "sequence"
  case hydra.grammar.Pattern.star(v_Pattern_star_p) => strings.cat2("listOf")(hydra.formatting.capitalize(hydra.grammars.rawName(v_Pattern_star_p)))

def replacePlaceholders[T0, T1](elName: T0)(typ: T1): T1 = typ

def simplify(isRecord: Boolean)(pats: Seq[hydra.grammar.Pattern]): Seq[hydra.grammar.Pattern] =
  {
  def isConstant(p: hydra.grammar.Pattern): Boolean =
    p match
    case hydra.grammar.Pattern.constant(v_Pattern_constant__) => true
    case _ => false
  logic.ifElse[Seq[hydra.grammar.Pattern]](isRecord)(lists.filter[hydra.grammar.Pattern]((p: hydra.grammar.Pattern) => logic.not(isConstant(p)))(pats))(pats)
}

def toName(ns: hydra.module.Namespace)(local: scala.Predef.String): hydra.core.Name = hydra.names.unqualifyName(hydra.module.QualifiedName(Some(ns), local))

def wrapType(t: hydra.core.Type): hydra.core.Type =
  t match
  case hydra.core.Type.record(v_Type_record__) => t
  case hydra.core.Type.union(v_Type_union__) => t
  case hydra.core.Type.wrap(v_Type_wrap__) => t
  case _ => hydra.core.Type.wrap(t)
