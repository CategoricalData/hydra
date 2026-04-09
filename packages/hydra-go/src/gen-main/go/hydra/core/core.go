// Note: this is an automatically generated file. Do not edit.

package core

import "math/big"

type AnnotatedTerm struct {
  Body Term
  Annotation []any
}

type AnnotatedType struct {
  Body Type
  Annotation []any
}

type Application struct {
  Function Term
  Argument Term
}

type ApplicationType struct {
  Function Type
  Argument Type
}

type Binding struct {
  Name Name
  Term Term
  Type_ any
}

type CaseStatement struct {
  TypeName Name
  Default_ any
  Cases []any
}

type EitherType struct {
  Left Type
  Right Type
}

type PairType struct {
  First Type
  Second Type
}

type Elimination interface {
  isElimination()
}

type EliminationRecord struct {
  Value Projection
}

func (EliminationRecord) isElimination() {

}

type EliminationUnion struct {
  Value CaseStatement
}

func (EliminationUnion) isElimination() {

}

type EliminationWrap struct {
  Value Name
}

func (EliminationWrap) isElimination() {

}

type Field struct {
  Name Name
  Term Term
}

type FieldType struct {
  Name Name
  Type_ Type
}

type FloatType interface {
  isFloatType()
}

type FloatTypeBigfloat struct{}

func (FloatTypeBigfloat) isFloatType() {

}

type FloatTypeFloat32_ struct{}

func (FloatTypeFloat32_) isFloatType() {

}

type FloatTypeFloat64_ struct{}

func (FloatTypeFloat64_) isFloatType() {

}

type FloatValue interface {
  isFloatValue()
}

type FloatValueBigfloat struct {
  Value float64
}

func (FloatValueBigfloat) isFloatValue() {

}

type FloatValueFloat32_ struct {
  Value float32
}

func (FloatValueFloat32_) isFloatValue() {

}

type FloatValueFloat64_ struct {
  Value float64
}

func (FloatValueFloat64_) isFloatValue() {

}

type ForallType struct {
  Parameter Name
  Body Type
}

type Function interface {
  isFunction()
}

type FunctionElimination struct {
  Value Elimination
}

func (FunctionElimination) isFunction() {

}

type FunctionLambda struct {
  Value Lambda
}

func (FunctionLambda) isFunction() {

}

type FunctionPrimitive struct {
  Value Name
}

func (FunctionPrimitive) isFunction() {

}

type FunctionType struct {
  Domain Type
  Codomain Type
}

type Injection struct {
  TypeName Name
  Field Field
}

type IntegerType interface {
  isIntegerType()
}

type IntegerTypeBigint struct{}

func (IntegerTypeBigint) isIntegerType() {

}

type IntegerTypeInt8_ struct{}

func (IntegerTypeInt8_) isIntegerType() {

}

type IntegerTypeInt16_ struct{}

func (IntegerTypeInt16_) isIntegerType() {

}

type IntegerTypeInt32_ struct{}

func (IntegerTypeInt32_) isIntegerType() {

}

type IntegerTypeInt64_ struct{}

func (IntegerTypeInt64_) isIntegerType() {

}

type IntegerTypeUint8_ struct{}

func (IntegerTypeUint8_) isIntegerType() {

}

type IntegerTypeUint16_ struct{}

func (IntegerTypeUint16_) isIntegerType() {

}

type IntegerTypeUint32_ struct{}

func (IntegerTypeUint32_) isIntegerType() {

}

type IntegerTypeUint64_ struct{}

func (IntegerTypeUint64_) isIntegerType() {

}

type IntegerValue interface {
  isIntegerValue()
}

type IntegerValueBigint struct {
  Value *big.Int
}

func (IntegerValueBigint) isIntegerValue() {

}

type IntegerValueInt8_ struct {
  Value int8
}

func (IntegerValueInt8_) isIntegerValue() {

}

type IntegerValueInt16_ struct {
  Value int16
}

func (IntegerValueInt16_) isIntegerValue() {

}

type IntegerValueInt32_ struct {
  Value int32
}

func (IntegerValueInt32_) isIntegerValue() {

}

type IntegerValueInt64_ struct {
  Value int64
}

func (IntegerValueInt64_) isIntegerValue() {

}

type IntegerValueUint8_ struct {
  Value uint8
}

func (IntegerValueUint8_) isIntegerValue() {

}

type IntegerValueUint16_ struct {
  Value uint16
}

func (IntegerValueUint16_) isIntegerValue() {

}

type IntegerValueUint32_ struct {
  Value uint32
}

func (IntegerValueUint32_) isIntegerValue() {

}

type IntegerValueUint64_ struct {
  Value uint64
}

func (IntegerValueUint64_) isIntegerValue() {

}

type Lambda struct {
  Parameter Name
  Domain any
  Body Term
}

type Let struct {
  Bindings []any
  Body Term
}

type Literal interface {
  isLiteral()
}

type LiteralBinary struct {
  Value []byte
}

func (LiteralBinary) isLiteral() {

}

type LiteralBoolean struct {
  Value bool
}

func (LiteralBoolean) isLiteral() {

}

type LiteralFloat struct {
  Value FloatValue
}

func (LiteralFloat) isLiteral() {

}

type LiteralInteger struct {
  Value IntegerValue
}

func (LiteralInteger) isLiteral() {

}

type LiteralString_ struct {
  Value string
}

func (LiteralString_) isLiteral() {

}

type LiteralType interface {
  isLiteralType()
}

type LiteralTypeBinary struct{}

func (LiteralTypeBinary) isLiteralType() {

}

type LiteralTypeBoolean struct{}

func (LiteralTypeBoolean) isLiteralType() {

}

type LiteralTypeFloat struct {
  Value FloatType
}

func (LiteralTypeFloat) isLiteralType() {

}

type LiteralTypeInteger struct {
  Value IntegerType
}

func (LiteralTypeInteger) isLiteralType() {

}

type LiteralTypeString_ struct{}

func (LiteralTypeString_) isLiteralType() {

}

type MapType struct {
  Keys Type
  Values Type
}

type Name string

type Projection struct {
  TypeName Name
  Field Name
}

type Record struct {
  TypeName Name
  Fields []any
}

type Term interface {
  isTerm()
}

type TermAnnotated struct {
  Value AnnotatedTerm
}

func (TermAnnotated) isTerm() {

}

type TermApplication struct {
  Value Application
}

func (TermApplication) isTerm() {

}

type TermEither struct {
  Value any
}

func (TermEither) isTerm() {

}

type TermFunction struct {
  Value Function
}

func (TermFunction) isTerm() {

}

type TermLet struct {
  Value Let
}

func (TermLet) isTerm() {

}

type TermList struct {
  Value []any
}

func (TermList) isTerm() {

}

type TermLiteral struct {
  Value Literal
}

func (TermLiteral) isTerm() {

}

type TermMap_ struct {
  Value []any
}

func (TermMap_) isTerm() {

}

type TermMaybe struct {
  Value any
}

func (TermMaybe) isTerm() {

}

type TermPair struct {
  Value any
}

func (TermPair) isTerm() {

}

type TermRecord struct {
  Value Record
}

func (TermRecord) isTerm() {

}

type TermSet struct {
  Value []any
}

func (TermSet) isTerm() {

}

type TermTypeApplication struct {
  Value TypeApplicationTerm
}

func (TermTypeApplication) isTerm() {

}

type TermTypeLambda struct {
  Value TypeLambda
}

func (TermTypeLambda) isTerm() {

}

type TermUnion struct {
  Value Injection
}

func (TermUnion) isTerm() {

}

type TermUnit struct{}

func (TermUnit) isTerm() {

}

type TermVariable struct {
  Value Name
}

func (TermVariable) isTerm() {

}

type TermWrap struct {
  Value WrappedTerm
}

func (TermWrap) isTerm() {

}

type Type interface {
  isType()
}

type TypeAnnotated struct {
  Value AnnotatedType
}

func (TypeAnnotated) isType() {

}

type TypeApplication struct {
  Value ApplicationType
}

func (TypeApplication) isType() {

}

type TypeEither struct {
  Value EitherType
}

func (TypeEither) isType() {

}

type TypeForall struct {
  Value ForallType
}

func (TypeForall) isType() {

}

type TypeFunction struct {
  Value FunctionType
}

func (TypeFunction) isType() {

}

type TypeList struct {
  Value Type
}

func (TypeList) isType() {

}

type TypeLiteral struct {
  Value LiteralType
}

func (TypeLiteral) isType() {

}

type TypeMap_ struct {
  Value MapType
}

func (TypeMap_) isType() {

}

type TypeMaybe struct {
  Value Type
}

func (TypeMaybe) isType() {

}

type TypePair struct {
  Value PairType
}

func (TypePair) isType() {

}

type TypeRecord struct {
  Value []any
}

func (TypeRecord) isType() {

}

type TypeSet struct {
  Value Type
}

func (TypeSet) isType() {

}

type TypeUnion struct {
  Value []any
}

func (TypeUnion) isType() {

}

type TypeUnit struct{}

func (TypeUnit) isType() {

}

type TypeVariable struct {
  Value Name
}

func (TypeVariable) isType() {

}

type TypeWrap struct {
  Value Type
}

func (TypeWrap) isType() {

}

type TypeApplicationTerm struct {
  Body Term
  Type_ Type
}

type TypeLambda struct {
  Parameter Name
  Body Term
}

type TypeScheme struct {
  Variables []any
  Type_ Type
  Constraints any
}

type TypeVariableMetadata struct {
  Classes []any
}

type WrappedTerm struct {
  TypeName Name
  Body Term
}
