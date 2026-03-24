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

def childName(lname: scala.Predef.String)(n: scala.Predef.String): scala.Predef.String = hydra.lib.strings.cat(Seq(lname, "_", hydra.formatting.capitalize(n)))

def findNames(pats: Seq[hydra.grammar.Pattern]): Seq[scala.Predef.String] =
  {
  def nextName(acc: Tuple2[Seq[scala.Predef.String], Map[scala.Predef.String, Int]])(pat: hydra.grammar.Pattern): Tuple2[Seq[scala.Predef.String], Map[scala.Predef.String, Int]] =
    {
    lazy val names: Seq[scala.Predef.String] = hydra.lib.pairs.first[Seq[scala.Predef.String], Map[scala.Predef.String, Int]](acc)
    lazy val nameMap: Map[scala.Predef.String, Int] = hydra.lib.pairs.second[Seq[scala.Predef.String], Map[scala.Predef.String, Int]](acc)
    lazy val rn: scala.Predef.String = hydra.grammars.rawName(pat)
    lazy val nameAndIndex: Tuple2[scala.Predef.String, Int] = hydra.lib.maybes.maybe[Tuple2[scala.Predef.String, Int], Int](Tuple2(rn, 1))((i: Int) =>
      Tuple2(hydra.lib.strings.cat2(rn)(hydra.lib.literals.showInt32(hydra.lib.math.add(i)(1))), hydra.lib.math.add(i)(1)))(hydra.lib.maps.lookup[scala.Predef.String, Int](rn)(nameMap))
    lazy val nn: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String, Int](nameAndIndex)
    lazy val ni: Int = hydra.lib.pairs.second[scala.Predef.String, Int](nameAndIndex)
    Tuple2(hydra.lib.lists.cons[scala.Predef.String](nn)(names), hydra.lib.maps.insert[scala.Predef.String, Int](rn)(ni)(nameMap))
  }
  hydra.lib.lists.reverse[scala.Predef.String](hydra.lib.pairs.first[Seq[scala.Predef.String], Map[scala.Predef.String, Int]](hydra.lib.lists.foldl[Tuple2[Seq[scala.Predef.String], Map[scala.Predef.String, Int]], hydra.grammar.Pattern](nextName)(Tuple2(Seq(), hydra.lib.maps.empty[scala.Predef.String, Int]))(pats)))
}

def grammarToModule(ns: hydra.module.Namespace)(grammar: hydra.grammar.Grammar)(desc: Option[scala.Predef.String]): hydra.module.Module =
  {
  lazy val prodPairs: Seq[Tuple2[scala.Predef.String, hydra.grammar.Pattern]] = hydra.lib.lists.map[hydra.grammar.Production, Tuple2[scala.Predef.String, hydra.grammar.Pattern]]((prod: hydra.grammar.Production) => Tuple2(prod.symbol, (prod.pattern)))(grammar)
  lazy val capitalizedNames: Seq[scala.Predef.String] = hydra.lib.lists.map[Tuple2[scala.Predef.String, hydra.grammar.Pattern], scala.Predef.String]((pair: Tuple2[scala.Predef.String, hydra.grammar.Pattern]) =>
    hydra.formatting.capitalize(hydra.lib.pairs.first[scala.Predef.String, hydra.grammar.Pattern](pair)))(prodPairs)
  lazy val patterns: Seq[hydra.grammar.Pattern] = hydra.lib.lists.map[Tuple2[scala.Predef.String, hydra.grammar.Pattern], hydra.grammar.Pattern]((pair: Tuple2[scala.Predef.String, hydra.grammar.Pattern]) =>
    hydra.lib.pairs.second[scala.Predef.String, hydra.grammar.Pattern](pair))(prodPairs)
  lazy val elementPairs: Seq[Tuple2[scala.Predef.String, hydra.core.Type]] = hydra.lib.lists.concat[Tuple2[scala.Predef.String, hydra.core.Type]](hydra.lib.lists.zipWith[scala.Predef.String, hydra.grammar.Pattern, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]]((v1: scala.Predef.String) =>
    (v2: hydra.grammar.Pattern) => hydra.grammars.makeElements(false)(ns)(v1)(v2))(capitalizedNames)(patterns))
  lazy val typeDefs: Seq[hydra.module.Definition] = hydra.lib.lists.map[Tuple2[scala.Predef.String, hydra.core.Type], hydra.module.Definition]((pair: Tuple2[scala.Predef.String, hydra.core.Type]) =>
    {
    lazy val lname: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String, hydra.core.Type](pair)
    {
      lazy val elName: hydra.core.Name = hydra.grammars.toName(ns)(lname)
      {
        lazy val typ: hydra.core.Type = hydra.grammars.replacePlaceholders(elName)(hydra.grammars.wrapType(hydra.lib.pairs.second[scala.Predef.String, hydra.core.Type](pair)))
        hydra.module.Definition.`type`(hydra.module.TypeDefinition(elName, typ))
      }
    }
  })(elementPairs)
  hydra.module.Module(ns, typeDefs, Seq(), Seq(), desc)
}

def isComplex(pat: hydra.grammar.Pattern): Boolean =
  pat match
  case hydra.grammar.Pattern.labeled(v_Pattern_labeled_lp) => hydra.grammars.isComplex(v_Pattern_labeled_lp.pattern)
  case hydra.grammar.Pattern.sequence(v_Pattern_sequence_pats) => hydra.grammars.isNontrivial(true)(v_Pattern_sequence_pats)
  case hydra.grammar.Pattern.alternatives(v_Pattern_alternatives_pats) => hydra.grammars.isNontrivial(false)(v_Pattern_alternatives_pats)
  case _ => false

def isNontrivial(isRecord: Boolean)(pats: Seq[hydra.grammar.Pattern]): Boolean =
  {
  lazy val minPats: Seq[hydra.grammar.Pattern] = hydra.grammars.simplify(isRecord)(pats)
  def isLabeled(p: hydra.grammar.Pattern): Boolean =
    p match
    case hydra.grammar.Pattern.labeled(_) => true
    case _ => false
  hydra.lib.logic.ifElse[Boolean](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.grammar.Pattern](minPats))(1))(isLabeled(hydra.lib.lists.head[hydra.grammar.Pattern](minPats)))(true)
}

def makeElements(omitTrivial: Boolean)(ns: hydra.module.Namespace)(lname: scala.Predef.String)(pat: hydra.grammar.Pattern): Seq[Tuple2[scala.Predef.String, hydra.core.Type]] =
  {
  lazy val trivial: Seq[Tuple2[scala.Predef.String, hydra.core.Type]] = hydra.lib.logic.ifElse[Seq[Tuple2[scala.Predef.String, hydra.core.Type]]](omitTrivial)(Seq())(Seq(Tuple2(lname, hydra.core.Type.unit)))
  def descend[T0](n: scala.Predef.String)(f: (Seq[Tuple2[scala.Predef.String, hydra.core.Type]] => T0))(p: hydra.grammar.Pattern): T0 =
    {
    lazy val cpairs: Seq[Tuple2[scala.Predef.String, hydra.core.Type]] = hydra.grammars.makeElements(false)(ns)(hydra.grammars.childName(lname)(n))(p)
    f(hydra.lib.logic.ifElse[Seq[Tuple2[scala.Predef.String, hydra.core.Type]]](hydra.grammars.isComplex(p))(hydra.lib.lists.cons[Tuple2[scala.Predef.String, hydra.core.Type]](Tuple2(lname, hydra.core.Type.variable(hydra.grammars.toName(ns)(hydra.lib.pairs.first[scala.Predef.String, hydra.core.Type](hydra.lib.lists.head[Tuple2[scala.Predef.String, hydra.core.Type]](cpairs))))))(cpairs))(hydra.lib.logic.ifElse[Seq[Tuple2[scala.Predef.String, hydra.core.Type]]](hydra.lib.lists.`null`[Tuple2[scala.Predef.String, hydra.core.Type]](cpairs))(Seq(Tuple2(lname, hydra.core.Type.unit)))(hydra.lib.lists.cons[Tuple2[scala.Predef.String, hydra.core.Type]](Tuple2(lname, hydra.lib.pairs.second[scala.Predef.String, hydra.core.Type](hydra.lib.lists.head[Tuple2[scala.Predef.String, hydra.core.Type]](cpairs))))(hydra.lib.lists.tail[Tuple2[scala.Predef.String, hydra.core.Type]](cpairs)))))
  }
  def mod(n: scala.Predef.String)(f: (hydra.core.Type => hydra.core.Type))(p: hydra.grammar.Pattern): Seq[Tuple2[scala.Predef.String, hydra.core.Type]] =
    descend(n)((pairs: Seq[Tuple2[scala.Predef.String, hydra.core.Type]]) =>
    hydra.lib.lists.cons[Tuple2[scala.Predef.String, hydra.core.Type]](Tuple2(lname, f(hydra.lib.pairs.second[scala.Predef.String, hydra.core.Type](hydra.lib.lists.head[Tuple2[scala.Predef.String, hydra.core.Type]](pairs)))))(hydra.lib.lists.tail[Tuple2[scala.Predef.String, hydra.core.Type]](pairs)))(p)
  def forPat(pat2: hydra.grammar.Pattern): Seq[Tuple2[scala.Predef.String, hydra.core.Type]] =
    pat2 match
    case hydra.grammar.Pattern.alternatives(v_Pattern_alternatives_pats) => forRecordOrUnion(false)((fields: Seq[hydra.core.FieldType]) => hydra.core.Type.union(fields))(v_Pattern_alternatives_pats)
    case hydra.grammar.Pattern.constant(_) => trivial
    case hydra.grammar.Pattern.ignored(_) => Seq()
    case hydra.grammar.Pattern.labeled(v_Pattern_labeled_lp) => forPat(v_Pattern_labeled_lp.pattern)
    case hydra.grammar.Pattern.nil => trivial
    case hydra.grammar.Pattern.nonterminal(v_Pattern_nonterminal_s) => Seq(Tuple2(lname, hydra.core.Type.variable(hydra.grammars.toName(ns)(v_Pattern_nonterminal_s))))
    case hydra.grammar.Pattern.option(v_Pattern_option_p) => mod("Option")((x: hydra.core.Type) => hydra.core.Type.maybe(x))(v_Pattern_option_p)
    case hydra.grammar.Pattern.plus(v_Pattern_plus_p) => mod("Elmt")((x: hydra.core.Type) => hydra.core.Type.list(x))(v_Pattern_plus_p)
    case hydra.grammar.Pattern.regex(_) => Seq(Tuple2(lname, hydra.core.Type.literal(hydra.core.LiteralType.string)))
    case hydra.grammar.Pattern.sequence(v_Pattern_sequence_pats) => forRecordOrUnion(true)((fields: Seq[hydra.core.FieldType]) => hydra.core.Type.record(fields))(v_Pattern_sequence_pats)
    case hydra.grammar.Pattern.star(v_Pattern_star_p) => mod("Elmt")((x: hydra.core.Type) => hydra.core.Type.list(x))(v_Pattern_star_p)
  def forRecordOrUnion(isRecord: Boolean)(construct: (Seq[hydra.core.FieldType] => hydra.core.Type))(pats: Seq[hydra.grammar.Pattern]): Seq[Tuple2[scala.Predef.String, hydra.core.Type]] =
    {
    lazy val minPats: Seq[hydra.grammar.Pattern] = hydra.grammars.simplify(isRecord)(pats)
    lazy val fieldNames: Seq[scala.Predef.String] = hydra.grammars.findNames(minPats)
    def toField(n: scala.Predef.String)(p: hydra.grammar.Pattern): Tuple2[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]] =
      descend(n)((pairs: Seq[Tuple2[scala.Predef.String, hydra.core.Type]]) =>
      Tuple2(hydra.core.FieldType(n, hydra.lib.pairs.second[scala.Predef.String, hydra.core.Type](hydra.lib.lists.head[Tuple2[scala.Predef.String, hydra.core.Type]](pairs))), hydra.lib.lists.tail[Tuple2[scala.Predef.String, hydra.core.Type]](pairs)))(p)
    lazy val fieldPairs: Seq[Tuple2[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]]] = hydra.lib.lists.zipWith[scala.Predef.String, hydra.grammar.Pattern, Tuple2[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]]](toField)(fieldNames)(minPats)
    lazy val fields: Seq[hydra.core.FieldType] = hydra.lib.lists.map[Tuple2[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]], hydra.core.FieldType](hydra.lib.pairs.first[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]])(fieldPairs)
    lazy val els: Seq[Tuple2[scala.Predef.String, hydra.core.Type]] = hydra.lib.lists.concat[Tuple2[scala.Predef.String, hydra.core.Type]](hydra.lib.lists.map[Tuple2[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]], Seq[Tuple2[scala.Predef.String, hydra.core.Type]]](hydra.lib.pairs.second[hydra.core.FieldType, Seq[Tuple2[scala.Predef.String, hydra.core.Type]]])(fieldPairs))
    hydra.lib.logic.ifElse[Seq[Tuple2[scala.Predef.String, hydra.core.Type]]](hydra.grammars.isNontrivial(isRecord)(pats))(hydra.lib.lists.cons[Tuple2[scala.Predef.String, hydra.core.Type]](Tuple2(lname, construct(fields)))(els))(forPat(hydra.lib.lists.head[hydra.grammar.Pattern](minPats)))
  }
  forPat(pat)
}

def rawName(pat: hydra.grammar.Pattern): scala.Predef.String =
  pat match
  case hydra.grammar.Pattern.alternatives(_) => "alts"
  case hydra.grammar.Pattern.constant(v_Pattern_constant_c) => hydra.formatting.capitalize(hydra.formatting.withCharacterAliases(v_Pattern_constant_c))
  case hydra.grammar.Pattern.ignored(_) => "ignored"
  case hydra.grammar.Pattern.labeled(v_Pattern_labeled_lp) => (v_Pattern_labeled_lp.label)
  case hydra.grammar.Pattern.nil => "none"
  case hydra.grammar.Pattern.nonterminal(v_Pattern_nonterminal_s) => hydra.formatting.capitalize(v_Pattern_nonterminal_s)
  case hydra.grammar.Pattern.option(v_Pattern_option_p) => hydra.formatting.capitalize(hydra.grammars.rawName(v_Pattern_option_p))
  case hydra.grammar.Pattern.plus(v_Pattern_plus_p) => hydra.lib.strings.cat2("listOf")(hydra.formatting.capitalize(hydra.grammars.rawName(v_Pattern_plus_p)))
  case hydra.grammar.Pattern.regex(_) => "regex"
  case hydra.grammar.Pattern.sequence(_) => "sequence"
  case hydra.grammar.Pattern.star(v_Pattern_star_p) => hydra.lib.strings.cat2("listOf")(hydra.formatting.capitalize(hydra.grammars.rawName(v_Pattern_star_p)))

def replacePlaceholders[T0, T1](elName: T0)(typ: T1): T1 = typ

def simplify(isRecord: Boolean)(pats: Seq[hydra.grammar.Pattern]): Seq[hydra.grammar.Pattern] =
  {
  def isConstant(p: hydra.grammar.Pattern): Boolean =
    p match
    case hydra.grammar.Pattern.constant(_) => true
    case _ => false
  hydra.lib.logic.ifElse[Seq[hydra.grammar.Pattern]](isRecord)(hydra.lib.lists.filter[hydra.grammar.Pattern]((p: hydra.grammar.Pattern) => hydra.lib.logic.not(isConstant(p)))(pats))(pats)
}

def toName(ns: hydra.module.Namespace)(local: scala.Predef.String): hydra.core.Name = hydra.names.unqualifyName(hydra.module.QualifiedName(Some(ns), local))

def wrapType(t: hydra.core.Type): hydra.core.Type =
  t match
  case hydra.core.Type.record(_) => t
  case hydra.core.Type.union(_) => t
  case hydra.core.Type.wrap(_) => t
  case _ => hydra.core.Type.wrap(t)
