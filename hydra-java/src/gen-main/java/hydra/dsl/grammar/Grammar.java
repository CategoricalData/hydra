// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.grammar;

/**
 * DSL functions for hydra.grammar
 */
public interface Grammar {
  static hydra.phantoms.TTerm<hydra.grammar.Constant> constant(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.grammar.Constant"), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unConstant(hydra.phantoms.TTerm<hydra.grammar.Constant> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.grammar.Constant")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Grammar> grammar(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.grammar.Production>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.grammar.Grammar"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.grammar.Production>> unGrammar(hydra.phantoms.TTerm<hydra.grammar.Grammar> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.grammar.Grammar")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Label> label(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.grammar.Label"), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unLabel(hydra.phantoms.TTerm<hydra.grammar.Label> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.grammar.Label")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.grammar.LabeledPattern> labeledPattern(hydra.phantoms.TTerm<hydra.grammar.Label> label, hydra.phantoms.TTerm<hydra.grammar.Pattern> pattern) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.grammar.LabeledPattern"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("label"), (label).value),
      new hydra.core.Field(new hydra.core.Name("pattern"), (pattern).value)))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Label> labeledPatternLabel(hydra.phantoms.TTerm<hydra.grammar.LabeledPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.grammar.LabeledPattern"), new hydra.core.Name("label"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> labeledPatternPattern(hydra.phantoms.TTerm<hydra.grammar.LabeledPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.grammar.LabeledPattern"), new hydra.core.Name("pattern"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.grammar.LabeledPattern> labeledPatternWithLabel(hydra.phantoms.TTerm<hydra.grammar.LabeledPattern> original, hydra.phantoms.TTerm<hydra.grammar.Label> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.grammar.LabeledPattern"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("label"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("pattern"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.grammar.LabeledPattern"), new hydra.core.Name("pattern"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.LabeledPattern> labeledPatternWithPattern(hydra.phantoms.TTerm<hydra.grammar.LabeledPattern> original, hydra.phantoms.TTerm<hydra.grammar.Pattern> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.grammar.LabeledPattern"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("label"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.grammar.LabeledPattern"), new hydra.core.Name("label"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("pattern"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternAlternatives(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.grammar.Pattern>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("alternatives"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternConstant(hydra.phantoms.TTerm<hydra.grammar.Constant> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("constant"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternIgnored(hydra.phantoms.TTerm<hydra.grammar.Pattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("ignored"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternLabeled(hydra.phantoms.TTerm<hydra.grammar.LabeledPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("labeled"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternNil() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("nil"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternNonterminal(hydra.phantoms.TTerm<hydra.grammar.Symbol> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("nonterminal"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternOption(hydra.phantoms.TTerm<hydra.grammar.Pattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("option"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternPlus(hydra.phantoms.TTerm<hydra.grammar.Pattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("plus"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternRegex(hydra.phantoms.TTerm<hydra.grammar.Regex> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("regex"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternSequence(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.grammar.Pattern>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("sequence"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> patternStar(hydra.phantoms.TTerm<hydra.grammar.Pattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("star"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Production> production(hydra.phantoms.TTerm<hydra.grammar.Symbol> symbol, hydra.phantoms.TTerm<hydra.grammar.Pattern> pattern) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.grammar.Production"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("symbol"), (symbol).value),
      new hydra.core.Field(new hydra.core.Name("pattern"), (pattern).value)))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Symbol> productionSymbol(hydra.phantoms.TTerm<hydra.grammar.Production> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.grammar.Production"), new hydra.core.Name("symbol"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Pattern> productionPattern(hydra.phantoms.TTerm<hydra.grammar.Production> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.grammar.Production"), new hydra.core.Name("pattern"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Production> productionWithSymbol(hydra.phantoms.TTerm<hydra.grammar.Production> original, hydra.phantoms.TTerm<hydra.grammar.Symbol> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.grammar.Production"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("symbol"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("pattern"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.grammar.Production"), new hydra.core.Name("pattern"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Production> productionWithPattern(hydra.phantoms.TTerm<hydra.grammar.Production> original, hydra.phantoms.TTerm<hydra.grammar.Pattern> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.grammar.Production"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("symbol"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.grammar.Production"), new hydra.core.Name("symbol"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("pattern"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Regex> regex(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.grammar.Regex"), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unRegex(hydra.phantoms.TTerm<hydra.grammar.Regex> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.grammar.Regex")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.grammar.Symbol> symbol(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.grammar.Symbol"), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unSymbol(hydra.phantoms.TTerm<hydra.grammar.Symbol> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.grammar.Symbol")))), (x).value)));
  }
}
