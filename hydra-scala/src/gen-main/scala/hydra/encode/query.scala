package hydra.encode.query

import hydra.core.*

import hydra.query.*

import hydra.lib.lists

import hydra.lib.maybes

def comparisonConstraint(v1: hydra.query.ComparisonConstraint): hydra.core.Term =
  v1 match
  case hydra.query.ComparisonConstraint.equal() => hydra.core.Term.union(hydra.core.Injection("hydra.query.ComparisonConstraint", hydra.core.Field("equal", hydra.core.Term.unit)))
  case hydra.query.ComparisonConstraint.notEqual() => hydra.core.Term.union(hydra.core.Injection("hydra.query.ComparisonConstraint", hydra.core.Field("notEqual", hydra.core.Term.unit)))
  case hydra.query.ComparisonConstraint.lessThan() => hydra.core.Term.union(hydra.core.Injection("hydra.query.ComparisonConstraint", hydra.core.Field("lessThan", hydra.core.Term.unit)))
  case hydra.query.ComparisonConstraint.greaterThan() => hydra.core.Term.union(hydra.core.Injection("hydra.query.ComparisonConstraint", hydra.core.Field("greaterThan", hydra.core.Term.unit)))
  case hydra.query.ComparisonConstraint.lessThanOrEqual() => hydra.core.Term.union(hydra.core.Injection("hydra.query.ComparisonConstraint", hydra.core.Field("lessThanOrEqual", hydra.core.Term.unit)))
  case hydra.query.ComparisonConstraint.greaterThanOrEqual() => hydra.core.Term.union(hydra.core.Injection("hydra.query.ComparisonConstraint", hydra.core.Field("greaterThanOrEqual", hydra.core.Term.unit)))

def edge(x: hydra.query.Edge): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.query.Edge", Seq(hydra.core.Field("type", hydra.encode.core.name(x.`type`)), hydra.core.Field("out", hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Name, hydra.core.Term](hydra.encode.core.name)(x.out))), hydra.core.Field("in", hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Name, hydra.core.Term](hydra.encode.core.name)(x.in))))))

def graphPattern(x: hydra.query.GraphPattern): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.query.GraphPattern", Seq(hydra.core.Field("graph", hydra.encode.core.name(x.graph)), hydra.core.Field("patterns", hydra.core.Term.list(hydra.lib.lists.map[hydra.query.Pattern, hydra.core.Term](hydra.encode.query.pattern)(x.patterns))))))

def node(v1: hydra.query.Node): hydra.core.Term =
  v1 match
  case hydra.query.Node.term(v_Node_term_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Node", hydra.core.Field("term", hydra.encode.core.term(v_Node_term_y))))
  case hydra.query.Node.variable(v_Node_variable_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Node", hydra.core.Field("variable", hydra.encode.query.variable(v_Node_variable_y))))
  case hydra.query.Node.wildcard() => hydra.core.Term.union(hydra.core.Injection("hydra.query.Node", hydra.core.Field("wildcard", hydra.core.Term.unit)))

def path(v1: hydra.query.Path): hydra.core.Term =
  v1 match
  case hydra.query.Path.step(v_Path_step_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Path", hydra.core.Field("step", hydra.encode.query.step(v_Path_step_y))))
  case hydra.query.Path.regex(v_Path_regex_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Path", hydra.core.Field("regex", hydra.encode.query.regexSequence(v_Path_regex_y))))
  case hydra.query.Path.inverse(v_Path_inverse_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Path", hydra.core.Field("inverse", hydra.encode.query.path(v_Path_inverse_y))))

def pathEquation(x: hydra.query.PathEquation): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.query.PathEquation", Seq(hydra.core.Field("left", hydra.encode.query.path(x.left)), hydra.core.Field("right", hydra.encode.query.path(x.right)))))

def pattern(v1: hydra.query.Pattern): hydra.core.Term =
  v1 match
  case hydra.query.Pattern.triple(v_Pattern_triple_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Pattern", hydra.core.Field("triple", hydra.encode.query.triplePattern(v_Pattern_triple_y))))
  case hydra.query.Pattern.negation(v_Pattern_negation_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Pattern", hydra.core.Field("negation", hydra.encode.query.pattern(v_Pattern_negation_y))))
  case hydra.query.Pattern.conjunction(v_Pattern_conjunction_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Pattern", hydra.core.Field("conjunction", hydra.core.Term.list(hydra.lib.lists.map[hydra.query.Pattern, hydra.core.Term](hydra.encode.query.pattern)(v_Pattern_conjunction_y)))))
  case hydra.query.Pattern.disjunction(v_Pattern_disjunction_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Pattern", hydra.core.Field("disjunction", hydra.core.Term.list(hydra.lib.lists.map[hydra.query.Pattern, hydra.core.Term](hydra.encode.query.pattern)(v_Pattern_disjunction_y)))))
  case hydra.query.Pattern.graph(v_Pattern_graph_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Pattern", hydra.core.Field("graph", hydra.encode.query.graphPattern(v_Pattern_graph_y))))

def patternImplication(x: hydra.query.PatternImplication): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.query.PatternImplication", Seq(hydra.core.Field("antecedent", hydra.encode.query.pattern(x.antecedent)), hydra.core.Field("consequent", hydra.encode.query.pattern(x.consequent)))))

def query(x: hydra.query.Query): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.query.Query", Seq(hydra.core.Field("variables", hydra.core.Term.list(hydra.lib.lists.map[hydra.query.Variable, hydra.core.Term](hydra.encode.query.variable)(x.variables))), hydra.core.Field("patterns", hydra.core.Term.list(hydra.lib.lists.map[hydra.query.Pattern, hydra.core.Term](hydra.encode.query.pattern)(x.patterns))))))

def range(x: hydra.query.Range): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.query.Range", Seq(hydra.core.Field("min", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x.min)))), hydra.core.Field("max", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x.max)))))))

def regexQuantifier(v1: hydra.query.RegexQuantifier): hydra.core.Term =
  v1 match
  case hydra.query.RegexQuantifier.one() => hydra.core.Term.union(hydra.core.Injection("hydra.query.RegexQuantifier", hydra.core.Field("one", hydra.core.Term.unit)))
  case hydra.query.RegexQuantifier.zeroOrOne() => hydra.core.Term.union(hydra.core.Injection("hydra.query.RegexQuantifier", hydra.core.Field("zeroOrOne", hydra.core.Term.unit)))
  case hydra.query.RegexQuantifier.zeroOrMore() => hydra.core.Term.union(hydra.core.Injection("hydra.query.RegexQuantifier", hydra.core.Field("zeroOrMore", hydra.core.Term.unit)))
  case hydra.query.RegexQuantifier.oneOrMore() => hydra.core.Term.union(hydra.core.Injection("hydra.query.RegexQuantifier", hydra.core.Field("oneOrMore", hydra.core.Term.unit)))
  case hydra.query.RegexQuantifier.exactly(v_RegexQuantifier_exactly_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.RegexQuantifier", hydra.core.Field("exactly", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_RegexQuantifier_exactly_y))))))
  case hydra.query.RegexQuantifier.atLeast(v_RegexQuantifier_atLeast_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.RegexQuantifier", hydra.core.Field("atLeast", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_RegexQuantifier_atLeast_y))))))
  case hydra.query.RegexQuantifier.range(v_RegexQuantifier_range_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.RegexQuantifier", hydra.core.Field("range", hydra.encode.query.range(v_RegexQuantifier_range_y))))

def regexSequence(x: hydra.query.RegexSequence): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.query.RegexSequence", Seq(hydra.core.Field("path", hydra.encode.query.path(x.path)), hydra.core.Field("quantifier", hydra.encode.query.regexQuantifier(x.quantifier)))))

def step(v1: hydra.query.Step): hydra.core.Term =
  v1 match
  case hydra.query.Step.edge(v_Step_edge_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Step", hydra.core.Field("edge", hydra.encode.query.edge(v_Step_edge_y))))
  case hydra.query.Step.project(v_Step_project_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Step", hydra.core.Field("project", hydra.encode.core.projection(v_Step_project_y))))
  case hydra.query.Step.compare(v_Step_compare_y) => hydra.core.Term.union(hydra.core.Injection("hydra.query.Step", hydra.core.Field("compare", hydra.encode.query.comparisonConstraint(v_Step_compare_y))))

def triplePattern(x: hydra.query.TriplePattern): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.query.TriplePattern", Seq(hydra.core.Field("subject", hydra.encode.query.node(x.subject)), hydra.core.Field("predicate", hydra.encode.query.path(x.predicate)), hydra.core.Field("object", hydra.encode.query.node(x.`object`)))))

def variable(x: hydra.query.Variable): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.query.Variable", hydra.core.Term.literal(hydra.core.Literal.string(x))))
