package hydra.encode.grammar

import hydra.core.*

import hydra.grammar.*

import hydra.lib.lists

def constant(x: hydra.grammar.Constant): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.grammar.Constant", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def grammar(x: hydra.grammar.Grammar): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.grammar.Grammar", hydra.core.Term.list(hydra.lib.lists.map[hydra.grammar.Production,
     hydra.core.Term](hydra.encode.grammar.production)(x))))

def label(x: hydra.grammar.Label): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.grammar.Label", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def labeledPattern(x: hydra.grammar.LabeledPattern): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.grammar.LabeledPattern", Seq(hydra.core.Field("label",
     hydra.encode.grammar.label(x.label)), hydra.core.Field("pattern", hydra.encode.grammar.pattern(x.pattern)))))

def pattern(v1: hydra.grammar.Pattern): hydra.core.Term =
  v1 match
  case hydra.grammar.Pattern.alternatives(v_Pattern_alternatives_y) => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern",
     hydra.core.Field("alternatives", hydra.core.Term.list(hydra.lib.lists.map[hydra.grammar.Pattern,
     hydra.core.Term](hydra.encode.grammar.pattern)(v_Pattern_alternatives_y)))))
  case hydra.grammar.Pattern.constant(v_Pattern_constant_y) => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern",
     hydra.core.Field("constant", hydra.encode.grammar.constant(v_Pattern_constant_y))))
  case hydra.grammar.Pattern.ignored(v_Pattern_ignored_y) => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern",
     hydra.core.Field("ignored", hydra.encode.grammar.pattern(v_Pattern_ignored_y))))
  case hydra.grammar.Pattern.labeled(v_Pattern_labeled_y) => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern",
     hydra.core.Field("labeled", hydra.encode.grammar.labeledPattern(v_Pattern_labeled_y))))
  case hydra.grammar.Pattern.nil => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern", hydra.core.Field("nil", hydra.core.Term.unit)))
  case hydra.grammar.Pattern.nonterminal(v_Pattern_nonterminal_y) => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern",
     hydra.core.Field("nonterminal", hydra.encode.grammar.symbol(v_Pattern_nonterminal_y))))
  case hydra.grammar.Pattern.option(v_Pattern_option_y) => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern",
     hydra.core.Field("option", hydra.encode.grammar.pattern(v_Pattern_option_y))))
  case hydra.grammar.Pattern.plus(v_Pattern_plus_y) => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern",
     hydra.core.Field("plus", hydra.encode.grammar.pattern(v_Pattern_plus_y))))
  case hydra.grammar.Pattern.regex(v_Pattern_regex_y) => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern",
     hydra.core.Field("regex", hydra.encode.grammar.regex(v_Pattern_regex_y))))
  case hydra.grammar.Pattern.sequence(v_Pattern_sequence_y) => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern",
     hydra.core.Field("sequence", hydra.core.Term.list(hydra.lib.lists.map[hydra.grammar.Pattern, hydra.core.Term](hydra.encode.grammar.pattern)(v_Pattern_sequence_y)))))
  case hydra.grammar.Pattern.star(v_Pattern_star_y) => hydra.core.Term.union(hydra.core.Injection("hydra.grammar.Pattern",
     hydra.core.Field("star", hydra.encode.grammar.pattern(v_Pattern_star_y))))

def production(x: hydra.grammar.Production): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.grammar.Production", Seq(hydra.core.Field("symbol",
     hydra.encode.grammar.symbol(x.symbol)), hydra.core.Field("pattern", hydra.encode.grammar.pattern(x.pattern)))))

def regex(x: hydra.grammar.Regex): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.grammar.Regex", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def symbol(x: hydra.grammar.Symbol): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.grammar.Symbol", hydra.core.Term.literal(hydra.core.Literal.string(x))))
