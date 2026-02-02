// Note: this is an automatically generated file. Do not edit.

package hydra.encode.grammar;

/**
 * Term encoders for hydra.grammar
 */
public interface Grammar {
  static hydra.core.Term constant(hydra.grammar.Constant x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.grammar.Constant"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).value))));
  }
  
  static hydra.core.Term grammar(hydra.grammar.Grammar x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.grammar.Grammar"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      (hydra.encode.grammar.Grammar::production),
      ((x)).value))));
  }
  
  static hydra.core.Term label(hydra.grammar.Label x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.grammar.Label"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).value))));
  }
  
  static hydra.core.Term labeledPattern(hydra.grammar.LabeledPattern x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.grammar.LabeledPattern"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("label"), hydra.encode.grammar.Grammar.label(((x)).label)),
      new hydra.core.Field(new hydra.core.Name("pattern"), hydra.encode.grammar.Grammar.pattern(((x)).pattern)))));
  }
  
  static hydra.core.Term pattern(hydra.grammar.Pattern v1) {
    return ((v1)).accept(new hydra.grammar.Pattern.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Alternatives y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("alternatives"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.encode.grammar.Grammar::pattern),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Constant y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("constant"), hydra.encode.grammar.Grammar.constant(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Ignored y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("ignored"), hydra.encode.grammar.Grammar.pattern(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Labeled y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("labeled"), hydra.encode.grammar.Grammar.labeledPattern(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Nil y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("nil"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Nonterminal y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("nonterminal"), hydra.encode.grammar.Grammar.symbol(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Option y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("option"), hydra.encode.grammar.Grammar.pattern(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Plus y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("plus"), hydra.encode.grammar.Grammar.pattern(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Regex y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("regex"), hydra.encode.grammar.Grammar.regex(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Sequence y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("sequence"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.encode.grammar.Grammar::pattern),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.grammar.Pattern.Star y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.grammar.Pattern"), new hydra.core.Field(new hydra.core.Name("star"), hydra.encode.grammar.Grammar.pattern(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term production(hydra.grammar.Production x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.grammar.Production"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("symbol"), hydra.encode.grammar.Grammar.symbol(((x)).symbol)),
      new hydra.core.Field(new hydra.core.Name("pattern"), hydra.encode.grammar.Grammar.pattern(((x)).pattern)))));
  }
  
  static hydra.core.Term regex(hydra.grammar.Regex x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.grammar.Regex"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).value))));
  }
  
  static hydra.core.Term symbol(hydra.grammar.Symbol x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.grammar.Symbol"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).value))));
  }
}
