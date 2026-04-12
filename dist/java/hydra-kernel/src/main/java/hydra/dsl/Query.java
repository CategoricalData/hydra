// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.query
 */
public interface Query {
  static hydra.phantoms.TTerm<hydra.query.ComparisonConstraint> comparisonConstraintEqual() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("equal"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.ComparisonConstraint> comparisonConstraintGreaterThan() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.ComparisonConstraint> comparisonConstraintGreaterThanOrEqual() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("greaterThanOrEqual"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.ComparisonConstraint> comparisonConstraintLessThan() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.ComparisonConstraint> comparisonConstraintLessThanOrEqual() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("lessThanOrEqual"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.ComparisonConstraint> comparisonConstraintNotEqual() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.ComparisonConstraint"), new hydra.core.Field(new hydra.core.Name("notEqual"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.Edge> edge(hydra.phantoms.TTerm<hydra.core.Name> type, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Name>> out, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Name>> in) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Edge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("out"), (out).value),
      new hydra.core.Field(new hydra.core.Name("in"), (in).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Name>> edgeIn(hydra.phantoms.TTerm<hydra.query.Edge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Edge"), new hydra.core.Name("in"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Name>> edgeOut(hydra.phantoms.TTerm<hydra.query.Edge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Edge"), new hydra.core.Name("out"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> edgeType(hydra.phantoms.TTerm<hydra.query.Edge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Edge"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.Edge> edgeWithIn(hydra.phantoms.TTerm<hydra.query.Edge> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Edge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Edge"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("out"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Edge"), new hydra.core.Name("out"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("in"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Edge> edgeWithOut(hydra.phantoms.TTerm<hydra.query.Edge> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Edge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Edge"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("out"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("in"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Edge"), new hydra.core.Name("in"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.query.Edge> edgeWithType(hydra.phantoms.TTerm<hydra.query.Edge> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Edge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("out"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Edge"), new hydra.core.Name("out"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("in"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Edge"), new hydra.core.Name("in"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.query.GraphPattern> graphPattern(hydra.phantoms.TTerm<hydra.core.Name> graph, hydra.phantoms.TTerm<java.util.List<hydra.query.Pattern>> patterns) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.GraphPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("graph"), (graph).value),
      new hydra.core.Field(new hydra.core.Name("patterns"), (patterns).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> graphPatternGraph(hydra.phantoms.TTerm<hydra.query.GraphPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.GraphPattern"), new hydra.core.Name("graph"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.query.Pattern>> graphPatternPatterns(hydra.phantoms.TTerm<hydra.query.GraphPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.GraphPattern"), new hydra.core.Name("patterns"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.GraphPattern> graphPatternWithGraph(hydra.phantoms.TTerm<hydra.query.GraphPattern> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.GraphPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("graph"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("patterns"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.GraphPattern"), new hydra.core.Name("patterns"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.query.GraphPattern> graphPatternWithPatterns(hydra.phantoms.TTerm<hydra.query.GraphPattern> original, hydra.phantoms.TTerm<java.util.List<hydra.query.Pattern>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.GraphPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("graph"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.GraphPattern"), new hydra.core.Name("graph"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("patterns"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Node> nodeTerm(hydra.phantoms.TTerm<hydra.core.Term> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Node"), new hydra.core.Field(new hydra.core.Name("term"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Node> nodeVariable(hydra.phantoms.TTerm<hydra.query.Variable> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Node"), new hydra.core.Field(new hydra.core.Name("variable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Node> nodeWildcard() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Node"), new hydra.core.Field(new hydra.core.Name("wildcard"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.PathEquation> pathEquation(hydra.phantoms.TTerm<hydra.query.Path> left, hydra.phantoms.TTerm<hydra.query.Path> right) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.PathEquation"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), (left).value),
      new hydra.core.Field(new hydra.core.Name("right"), (right).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Path> pathEquationLeft(hydra.phantoms.TTerm<hydra.query.PathEquation> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.PathEquation"), new hydra.core.Name("left"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.Path> pathEquationRight(hydra.phantoms.TTerm<hydra.query.PathEquation> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.PathEquation"), new hydra.core.Name("right"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.PathEquation> pathEquationWithLeft(hydra.phantoms.TTerm<hydra.query.PathEquation> original, hydra.phantoms.TTerm<hydra.query.Path> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.PathEquation"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.PathEquation"), new hydra.core.Name("right"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.query.PathEquation> pathEquationWithRight(hydra.phantoms.TTerm<hydra.query.PathEquation> original, hydra.phantoms.TTerm<hydra.query.Path> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.PathEquation"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.PathEquation"), new hydra.core.Name("left"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("right"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Path> pathInverse(hydra.phantoms.TTerm<hydra.query.Path> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Path"), new hydra.core.Field(new hydra.core.Name("inverse"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Path> pathRegex(hydra.phantoms.TTerm<hydra.query.RegexSequence> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Path"), new hydra.core.Field(new hydra.core.Name("regex"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Path> pathStep(hydra.phantoms.TTerm<hydra.query.Step> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Path"), new hydra.core.Field(new hydra.core.Name("step"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Pattern> patternConjunction(hydra.phantoms.TTerm<java.util.List<hydra.query.Pattern>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Pattern"), new hydra.core.Field(new hydra.core.Name("conjunction"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Pattern> patternDisjunction(hydra.phantoms.TTerm<java.util.List<hydra.query.Pattern>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Pattern"), new hydra.core.Field(new hydra.core.Name("disjunction"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Pattern> patternGraph(hydra.phantoms.TTerm<hydra.query.GraphPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Pattern"), new hydra.core.Field(new hydra.core.Name("graph"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.PatternImplication> patternImplication(hydra.phantoms.TTerm<hydra.query.Pattern> antecedent, hydra.phantoms.TTerm<hydra.query.Pattern> consequent) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.PatternImplication"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("antecedent"), (antecedent).value),
      new hydra.core.Field(new hydra.core.Name("consequent"), (consequent).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Pattern> patternImplicationAntecedent(hydra.phantoms.TTerm<hydra.query.PatternImplication> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.PatternImplication"), new hydra.core.Name("antecedent"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.Pattern> patternImplicationConsequent(hydra.phantoms.TTerm<hydra.query.PatternImplication> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.PatternImplication"), new hydra.core.Name("consequent"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.PatternImplication> patternImplicationWithAntecedent(hydra.phantoms.TTerm<hydra.query.PatternImplication> original, hydra.phantoms.TTerm<hydra.query.Pattern> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.PatternImplication"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("antecedent"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("consequent"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.PatternImplication"), new hydra.core.Name("consequent"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.query.PatternImplication> patternImplicationWithConsequent(hydra.phantoms.TTerm<hydra.query.PatternImplication> original, hydra.phantoms.TTerm<hydra.query.Pattern> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.PatternImplication"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("antecedent"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.PatternImplication"), new hydra.core.Name("antecedent"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("consequent"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Pattern> patternNegation(hydra.phantoms.TTerm<hydra.query.Pattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Pattern"), new hydra.core.Field(new hydra.core.Name("negation"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Pattern> patternTriple(hydra.phantoms.TTerm<hydra.query.TriplePattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Pattern"), new hydra.core.Field(new hydra.core.Name("triple"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Query> query(hydra.phantoms.TTerm<java.util.List<hydra.query.Variable>> variables, hydra.phantoms.TTerm<java.util.List<hydra.query.Pattern>> patterns) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Query"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), (variables).value),
      new hydra.core.Field(new hydra.core.Name("patterns"), (patterns).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.query.Pattern>> queryPatterns(hydra.phantoms.TTerm<hydra.query.Query> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Query"), new hydra.core.Name("patterns"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.query.Variable>> queryVariables(hydra.phantoms.TTerm<hydra.query.Query> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Query"), new hydra.core.Name("variables"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.Query> queryWithPatterns(hydra.phantoms.TTerm<hydra.query.Query> original, hydra.phantoms.TTerm<java.util.List<hydra.query.Pattern>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Query"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Query"), new hydra.core.Name("variables"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("patterns"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Query> queryWithVariables(hydra.phantoms.TTerm<hydra.query.Query> original, hydra.phantoms.TTerm<java.util.List<hydra.query.Variable>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Query"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("patterns"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Query"), new hydra.core.Name("patterns"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.query.Range> range(hydra.phantoms.TTerm<Integer> min, hydra.phantoms.TTerm<Integer> max) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Range"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("min"), (min).value),
      new hydra.core.Field(new hydra.core.Name("max"), (max).value)))));
  }

  static hydra.phantoms.TTerm<Integer> rangeMax(hydra.phantoms.TTerm<hydra.query.Range> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Range"), new hydra.core.Name("max"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<Integer> rangeMin(hydra.phantoms.TTerm<hydra.query.Range> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Range"), new hydra.core.Name("min"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.Range> rangeWithMax(hydra.phantoms.TTerm<hydra.query.Range> original, hydra.phantoms.TTerm<Integer> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Range"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("min"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Range"), new hydra.core.Name("min"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("max"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Range> rangeWithMin(hydra.phantoms.TTerm<hydra.query.Range> original, hydra.phantoms.TTerm<Integer> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.Range"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("min"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("max"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.Range"), new hydra.core.Name("max"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexQuantifier> regexQuantifierAtLeast(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("atLeast"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexQuantifier> regexQuantifierExactly(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("exactly"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexQuantifier> regexQuantifierOne() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("one"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexQuantifier> regexQuantifierOneOrMore() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("oneOrMore"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexQuantifier> regexQuantifierRange(hydra.phantoms.TTerm<hydra.query.Range> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("range"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexQuantifier> regexQuantifierZeroOrMore() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("zeroOrMore"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexQuantifier> regexQuantifierZeroOrOne() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.RegexQuantifier"), new hydra.core.Field(new hydra.core.Name("zeroOrOne"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexSequence> regexSequence(hydra.phantoms.TTerm<hydra.query.Path> path, hydra.phantoms.TTerm<hydra.query.RegexQuantifier> quantifier) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.RegexSequence"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (path).value),
      new hydra.core.Field(new hydra.core.Name("quantifier"), (quantifier).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Path> regexSequencePath(hydra.phantoms.TTerm<hydra.query.RegexSequence> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.RegexSequence"), new hydra.core.Name("path"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexQuantifier> regexSequenceQuantifier(hydra.phantoms.TTerm<hydra.query.RegexSequence> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.RegexSequence"), new hydra.core.Name("quantifier"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexSequence> regexSequenceWithPath(hydra.phantoms.TTerm<hydra.query.RegexSequence> original, hydra.phantoms.TTerm<hydra.query.Path> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.RegexSequence"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("quantifier"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.RegexSequence"), new hydra.core.Name("quantifier"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.query.RegexSequence> regexSequenceWithQuantifier(hydra.phantoms.TTerm<hydra.query.RegexSequence> original, hydra.phantoms.TTerm<hydra.query.RegexQuantifier> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.RegexSequence"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.RegexSequence"), new hydra.core.Name("path"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("quantifier"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Step> stepCompare(hydra.phantoms.TTerm<hydra.query.ComparisonConstraint> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Step"), new hydra.core.Field(new hydra.core.Name("compare"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Step> stepEdge(hydra.phantoms.TTerm<hydra.query.Edge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Step"), new hydra.core.Field(new hydra.core.Name("edge"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.Step> stepProject(hydra.phantoms.TTerm<hydra.core.Projection> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.query.Step"), new hydra.core.Field(new hydra.core.Name("project"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.query.TriplePattern> triplePattern(hydra.phantoms.TTerm<hydra.query.Node> subject, hydra.phantoms.TTerm<hydra.query.Path> predicate, hydra.phantoms.TTerm<hydra.query.Node> object) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.TriplePattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("subject"), (subject).value),
      new hydra.core.Field(new hydra.core.Name("predicate"), (predicate).value),
      new hydra.core.Field(new hydra.core.Name("object"), (object).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.Node> triplePatternObject(hydra.phantoms.TTerm<hydra.query.TriplePattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.TriplePattern"), new hydra.core.Name("object"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.Path> triplePatternPredicate(hydra.phantoms.TTerm<hydra.query.TriplePattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.TriplePattern"), new hydra.core.Name("predicate"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.Node> triplePatternSubject(hydra.phantoms.TTerm<hydra.query.TriplePattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.TriplePattern"), new hydra.core.Name("subject"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.TriplePattern> triplePatternWithObject(hydra.phantoms.TTerm<hydra.query.TriplePattern> original, hydra.phantoms.TTerm<hydra.query.Node> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.TriplePattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("subject"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.TriplePattern"), new hydra.core.Name("subject"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("predicate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.TriplePattern"), new hydra.core.Name("predicate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("object"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.query.TriplePattern> triplePatternWithPredicate(hydra.phantoms.TTerm<hydra.query.TriplePattern> original, hydra.phantoms.TTerm<hydra.query.Path> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.TriplePattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("subject"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.TriplePattern"), new hydra.core.Name("subject"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("predicate"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("object"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.TriplePattern"), new hydra.core.Name("object"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.query.TriplePattern> triplePatternWithSubject(hydra.phantoms.TTerm<hydra.query.TriplePattern> original, hydra.phantoms.TTerm<hydra.query.Node> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.query.TriplePattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("subject"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("predicate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.TriplePattern"), new hydra.core.Name("predicate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("object"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.query.TriplePattern"), new hydra.core.Name("object"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<String> unVariable(hydra.phantoms.TTerm<hydra.query.Variable> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.query.Variable")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.query.Variable> variable(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.query.Variable"), (x).value)));
  }
}
