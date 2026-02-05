// Note: this is an automatically generated file. Do not edit.

package hydra.encode.query;

/**
 * Term encoders for hydra.query
 */
public interface Query {
  static hydra.core.Term comparisonConstraint(hydra.query.ComparisonConstraint v1) {
    return ((v1)).accept(new hydra.query.ComparisonConstraint.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.query.ComparisonConstraint.Equal y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("equal"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.ComparisonConstraint.NotEqual y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("notEqual"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.ComparisonConstraint.LessThan y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.ComparisonConstraint.GreaterThan y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.ComparisonConstraint.LessThanOrEqual y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("lessThanOrEqual"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.ComparisonConstraint.GreaterThanOrEqual y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("greaterThanOrEqual"), new hydra.core.Term.Unit())));
      }
    });
  }
  
  static hydra.core.Term edge(hydra.query.Edge x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Edge"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.name(((x)).type)),
      new hydra.core.Field(new hydra.core.Name("out"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (hydra.encode.core.Core::name),
        ((x)).out))),
      new hydra.core.Field(new hydra.core.Name("in"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (hydra.encode.core.Core::name),
        ((x)).in))))));
  }
  
  static hydra.core.Term graphPattern(hydra.query.GraphPattern x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.GraphPattern"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("graph"), hydra.encode.core.Core.name(((x)).graph)),
      new hydra.core.Field(new hydra.core.Name("patterns"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.query.Query::pattern),
        ((x)).patterns))))));
  }
  
  static hydra.core.Term node(hydra.query.Node v1) {
    return ((v1)).accept(new hydra.query.Node.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.query.Node.Term y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Node"), new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.core.Core.term(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.Node.Variable y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Node"), new hydra.core.Field(new hydra.core.Name("variable"), hydra.encode.query.Query.variable(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.Node.Wildcard y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Node"), new hydra.core.Field(new hydra.core.Name("wildcard"), new hydra.core.Term.Unit())));
      }
    });
  }
  
  static hydra.core.Term path(hydra.query.Path v1) {
    return ((v1)).accept(new hydra.query.Path.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.query.Path.Step y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Path"), new hydra.core.Field(new hydra.core.Name("step"), hydra.encode.query.Query.step(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.Path.Regex y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Path"), new hydra.core.Field(new hydra.core.Name("regex"), hydra.encode.query.Query.regexSequence(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.Path.Inverse y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Path"), new hydra.core.Field(new hydra.core.Name("inverse"), hydra.encode.query.Query.path(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term pattern(hydra.query.Pattern v1) {
    return ((v1)).accept(new hydra.query.Pattern.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.query.Pattern.Triple y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Pattern"), new hydra.core.Field(new hydra.core.Name("triple"), hydra.encode.query.Query.triplePattern(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.Pattern.Negation y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Pattern"), new hydra.core.Field(new hydra.core.Name("negation"), hydra.encode.query.Query.pattern(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.Pattern.Conjunction y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Pattern"), new hydra.core.Field(new hydra.core.Name("conjunction"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.encode.query.Query::pattern),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.Pattern.Disjunction y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Pattern"), new hydra.core.Field(new hydra.core.Name("disjunction"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.encode.query.Query::pattern),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.Pattern.Graph y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Pattern"), new hydra.core.Field(new hydra.core.Name("graph"), hydra.encode.query.Query.graphPattern(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term query(hydra.query.Query x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Query"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("variables"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.query.Query::variable),
        ((x)).variables))),
      new hydra.core.Field(new hydra.core.Name("patterns"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.query.Query::pattern),
        ((x)).patterns))))));
  }
  
  static hydra.core.Term range(hydra.query.Range x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Range"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("min"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((x)).min)))),
      new hydra.core.Field(new hydra.core.Name("max"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((x)).max)))))));
  }
  
  static hydra.core.Term regexQuantifier(hydra.query.RegexQuantifier v1) {
    return ((v1)).accept(new hydra.query.RegexQuantifier.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.query.RegexQuantifier.One y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("one"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.RegexQuantifier.ZeroOrOne y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("zeroOrOne"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.RegexQuantifier.ZeroOrMore y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("zeroOrMore"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.RegexQuantifier.OneOrMore y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("oneOrMore"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.RegexQuantifier.Exactly y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("exactly"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.RegexQuantifier.AtLeast y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("atLeast"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.RegexQuantifier.Range y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("range"), hydra.encode.query.Query.range(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term regexSequence(hydra.query.RegexSequence x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.RegexSequence"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("path"), hydra.encode.query.Query.path(((x)).path)),
      new hydra.core.Field(new hydra.core.Name("quantifier"), hydra.encode.query.Query.regexQuantifier(((x)).quantifier)))));
  }
  
  static hydra.core.Term step(hydra.query.Step v1) {
    return ((v1)).accept(new hydra.query.Step.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.query.Step.Edge y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Step"), new hydra.core.Field(new hydra.core.Name("edge"), hydra.encode.query.Query.edge(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.Step.Project y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Step"), new hydra.core.Field(new hydra.core.Name("project"), hydra.encode.core.Core.projection(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.query.Step.Compare y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Step"), new hydra.core.Field(new hydra.core.Name("compare"), hydra.encode.query.Query.comparisonConstraint(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term triplePattern(hydra.query.TriplePattern x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.TriplePattern"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("subject"), hydra.encode.query.Query.node(((x)).subject)),
      new hydra.core.Field(new hydra.core.Name("predicate"), hydra.encode.query.Query.path(((x)).predicate)),
      new hydra.core.Field(new hydra.core.Name("object"), hydra.encode.query.Query.node(((x)).object)))));
  }
  
  static hydra.core.Term variable(hydra.query.Variable x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.query.Variable"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).value))));
  }
}
