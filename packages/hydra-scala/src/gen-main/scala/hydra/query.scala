package hydra.query

import hydra.core.*

import hydra.core

enum ComparisonConstraint :
   case equal extends ComparisonConstraint
   case notEqual extends ComparisonConstraint
   case lessThan extends ComparisonConstraint
   case greaterThan extends ComparisonConstraint
   case lessThanOrEqual extends ComparisonConstraint
   case greaterThanOrEqual extends ComparisonConstraint

case class Edge(`type`: hydra.core.Name, out: Option[hydra.core.Name], in: Option[hydra.core.Name])

case class GraphPattern(graph: hydra.core.Name, patterns: Seq[hydra.query.Pattern])

enum Node :
   case term(value: hydra.core.Term) extends Node
   case variable(value: hydra.query.Variable) extends Node
   case wildcard extends Node

enum Path :
   case step(value: hydra.query.Step) extends Path
   case regex(value: hydra.query.RegexSequence) extends Path
   case inverse(value: hydra.query.Path) extends Path

case class PathEquation(left: hydra.query.Path, right: hydra.query.Path)

enum Pattern :
   case triple(value: hydra.query.TriplePattern) extends Pattern
   case negation(value: hydra.query.Pattern) extends Pattern
   case conjunction(value: Seq[hydra.query.Pattern]) extends Pattern
   case disjunction(value: Seq[hydra.query.Pattern]) extends Pattern
   case graph(value: hydra.query.GraphPattern) extends Pattern

case class PatternImplication(antecedent: hydra.query.Pattern, consequent: hydra.query.Pattern)

case class Query(variables: Seq[hydra.query.Variable], patterns: Seq[hydra.query.Pattern])

case class Range(min: Int, max: Int)

enum RegexQuantifier :
   case one extends RegexQuantifier
   case zeroOrOne extends RegexQuantifier
   case zeroOrMore extends RegexQuantifier
   case oneOrMore extends RegexQuantifier
   case exactly(value: Int) extends RegexQuantifier
   case atLeast(value: Int) extends RegexQuantifier
   case range(value: hydra.query.Range) extends RegexQuantifier

case class RegexSequence(path: hydra.query.Path, quantifier: hydra.query.RegexQuantifier)

enum Step :
   case edge(value: hydra.query.Edge) extends Step
   case project(value: hydra.core.Projection) extends Step
   case compare(value: hydra.query.ComparisonConstraint) extends Step

case class TriplePattern(subject: hydra.query.Node, predicate: hydra.query.Path, `object`: hydra.query.Node)

type Variable = scala.Predef.String
