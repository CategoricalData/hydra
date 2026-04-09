// Note: this is an automatically generated file. Do not edit.

package query

import "hydra.dev/hydra/core"

type ComparisonConstraint interface {
  isComparisonConstraint()
}

type ComparisonConstraintEqual struct{}

func (ComparisonConstraintEqual) isComparisonConstraint() {

}

type ComparisonConstraintNotEqual struct{}

func (ComparisonConstraintNotEqual) isComparisonConstraint() {

}

type ComparisonConstraintLessThan struct{}

func (ComparisonConstraintLessThan) isComparisonConstraint() {

}

type ComparisonConstraintGreaterThan struct{}

func (ComparisonConstraintGreaterThan) isComparisonConstraint() {

}

type ComparisonConstraintLessThanOrEqual struct{}

func (ComparisonConstraintLessThanOrEqual) isComparisonConstraint() {

}

type ComparisonConstraintGreaterThanOrEqual struct{}

func (ComparisonConstraintGreaterThanOrEqual) isComparisonConstraint() {

}

type Edge struct {
  Type_ core.Name
  Out any
  In any
}

type GraphPattern struct {
  Graph core.Name
  Patterns []any
}

type Node_ interface {
  isNode_()
}

type Node_Term struct {
  Value core.Term
}

func (Node_Term) isNode_() {

}

type Node_Variable struct {
  Value Variable
}

func (Node_Variable) isNode_() {

}

type Node_Wildcard struct{}

func (Node_Wildcard) isNode_() {

}

type Path interface {
  isPath()
}

type PathStep struct {
  Value Step
}

func (PathStep) isPath() {

}

type PathRegex struct {
  Value RegexSequence
}

func (PathRegex) isPath() {

}

type PathInverse struct {
  Value Path
}

func (PathInverse) isPath() {

}

type PathEquation struct {
  Left Path
  Right Path
}

type Pattern interface {
  isPattern()
}

type PatternTriple struct {
  Value TriplePattern
}

func (PatternTriple) isPattern() {

}

type PatternNegation struct {
  Value Pattern
}

func (PatternNegation) isPattern() {

}

type PatternConjunction struct {
  Value []any
}

func (PatternConjunction) isPattern() {

}

type PatternDisjunction struct {
  Value []any
}

func (PatternDisjunction) isPattern() {

}

type PatternGraph struct {
  Value GraphPattern
}

func (PatternGraph) isPattern() {

}

type PatternImplication struct {
  Antecedent Pattern
  Consequent Pattern
}

type Query struct {
  Variables []any
  Patterns []any
}

type Range struct {
  Min_ int32
  Max_ int32
}

type RegexQuantifier interface {
  isRegexQuantifier()
}

type RegexQuantifierOne struct{}

func (RegexQuantifierOne) isRegexQuantifier() {

}

type RegexQuantifierZeroOrOne struct{}

func (RegexQuantifierZeroOrOne) isRegexQuantifier() {

}

type RegexQuantifierZeroOrMore struct{}

func (RegexQuantifierZeroOrMore) isRegexQuantifier() {

}

type RegexQuantifierOneOrMore struct{}

func (RegexQuantifierOneOrMore) isRegexQuantifier() {

}

type RegexQuantifierExactly struct {
  Value int32
}

func (RegexQuantifierExactly) isRegexQuantifier() {

}

type RegexQuantifierAtLeast struct {
  Value int32
}

func (RegexQuantifierAtLeast) isRegexQuantifier() {

}

type RegexQuantifierRange_ struct {
  Value Range
}

func (RegexQuantifierRange_) isRegexQuantifier() {

}

type RegexSequence struct {
  Path Path
  Quantifier RegexQuantifier
}

type Step interface {
  isStep()
}

type StepEdge struct {
  Value Edge
}

func (StepEdge) isStep() {

}

type StepProject struct {
  Value core.Projection
}

func (StepProject) isStep() {

}

type StepCompare struct {
  Value ComparisonConstraint
}

func (StepCompare) isStep() {

}

type TriplePattern struct {
  Subject Node_
  Predicate Path
  Object Node_
}

type Variable string
