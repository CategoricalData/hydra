package hydra.grammar

import hydra.core.*

type Constant = scala.Predef.String

type Grammar = Seq[hydra.grammar.Production]

type Label = scala.Predef.String

case class LabeledPattern(label: hydra.grammar.Label, pattern: hydra.grammar.Pattern)

enum Pattern :
   case alternatives(value: Seq[hydra.grammar.Pattern]) extends Pattern
   case constant(value: hydra.grammar.Constant) extends Pattern
   case ignored(value: hydra.grammar.Pattern) extends Pattern
   case labeled(value: hydra.grammar.LabeledPattern) extends Pattern
   case nil extends Pattern
   case nonterminal(value: hydra.grammar.Symbol) extends Pattern
   case option(value: hydra.grammar.Pattern) extends Pattern
   case plus(value: hydra.grammar.Pattern) extends Pattern
   case regex(value: hydra.grammar.Regex) extends Pattern
   case sequence(value: Seq[hydra.grammar.Pattern]) extends Pattern
   case star(value: hydra.grammar.Pattern) extends Pattern

case class Production(symbol: hydra.grammar.Symbol, pattern: hydra.grammar.Pattern)

type Regex = scala.Predef.String

type Symbol = scala.Predef.String
