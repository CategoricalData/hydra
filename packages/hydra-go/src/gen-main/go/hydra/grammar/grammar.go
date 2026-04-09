// Note: this is an automatically generated file. Do not edit.

package grammar

type Constant string

type Grammar []any

type Label string

type LabeledPattern struct {
  Label Label
  Pattern Pattern
}

type Pattern interface {
  isPattern()
}

type PatternAlternatives struct {
  Value []any
}

func (PatternAlternatives) isPattern() {

}

type PatternConstant struct {
  Value Constant
}

func (PatternConstant) isPattern() {

}

type PatternIgnored struct {
  Value Pattern
}

func (PatternIgnored) isPattern() {

}

type PatternLabeled struct {
  Value LabeledPattern
}

func (PatternLabeled) isPattern() {

}

type PatternNil_ struct{}

func (PatternNil_) isPattern() {

}

type PatternNonterminal struct {
  Value Symbol
}

func (PatternNonterminal) isPattern() {

}

type PatternOption struct {
  Value Pattern
}

func (PatternOption) isPattern() {

}

type PatternPlus struct {
  Value Pattern
}

func (PatternPlus) isPattern() {

}

type PatternRegex struct {
  Value Regex
}

func (PatternRegex) isPattern() {

}

type PatternSequence struct {
  Value []any
}

func (PatternSequence) isPattern() {

}

type PatternStar struct {
  Value Pattern
}

func (PatternStar) isPattern() {

}

type Production struct {
  Symbol Symbol
  Pattern Pattern
}

type Regex string

type Symbol string
