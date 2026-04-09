// Note: this is an automatically generated file. Do not edit.

package jsonmodel

type Value interface {
  isValue()
}

type ValueArray struct {
  Value []any
}

func (ValueArray) isValue() {

}

type ValueBoolean struct {
  Value bool
}

func (ValueBoolean) isValue() {

}

type ValueNull struct{}

func (ValueNull) isValue() {

}

type ValueNumber struct {
  Value float64
}

func (ValueNumber) isValue() {

}

type ValueObject struct {
  Value []any
}

func (ValueObject) isValue() {

}

type ValueString_ struct {
  Value string
}

func (ValueString_) isValue() {

}
