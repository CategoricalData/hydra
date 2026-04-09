// Note: this is an automatically generated file. Do not edit.

package variants

type EliminationVariant interface {
  isEliminationVariant()
}

type EliminationVariantRecord struct{}

func (EliminationVariantRecord) isEliminationVariant() {

}

type EliminationVariantUnion struct{}

func (EliminationVariantUnion) isEliminationVariant() {

}

type EliminationVariantWrap struct{}

func (EliminationVariantWrap) isEliminationVariant() {

}

type FunctionVariant interface {
  isFunctionVariant()
}

type FunctionVariantElimination struct{}

func (FunctionVariantElimination) isFunctionVariant() {

}

type FunctionVariantLambda struct{}

func (FunctionVariantLambda) isFunctionVariant() {

}

type FunctionVariantPrimitive struct{}

func (FunctionVariantPrimitive) isFunctionVariant() {

}

type LiteralVariant interface {
  isLiteralVariant()
}

type LiteralVariantBinary struct{}

func (LiteralVariantBinary) isLiteralVariant() {

}

type LiteralVariantBoolean struct{}

func (LiteralVariantBoolean) isLiteralVariant() {

}

type LiteralVariantFloat struct{}

func (LiteralVariantFloat) isLiteralVariant() {

}

type LiteralVariantInteger struct{}

func (LiteralVariantInteger) isLiteralVariant() {

}

type LiteralVariantString_ struct{}

func (LiteralVariantString_) isLiteralVariant() {

}

type TermVariant interface {
  isTermVariant()
}

type TermVariantAnnotated struct{}

func (TermVariantAnnotated) isTermVariant() {

}

type TermVariantApplication struct{}

func (TermVariantApplication) isTermVariant() {

}

type TermVariantEither struct{}

func (TermVariantEither) isTermVariant() {

}

type TermVariantFunction struct{}

func (TermVariantFunction) isTermVariant() {

}

type TermVariantLet struct{}

func (TermVariantLet) isTermVariant() {

}

type TermVariantList struct{}

func (TermVariantList) isTermVariant() {

}

type TermVariantLiteral struct{}

func (TermVariantLiteral) isTermVariant() {

}

type TermVariantMap_ struct{}

func (TermVariantMap_) isTermVariant() {

}

type TermVariantMaybe struct{}

func (TermVariantMaybe) isTermVariant() {

}

type TermVariantPair struct{}

func (TermVariantPair) isTermVariant() {

}

type TermVariantRecord struct{}

func (TermVariantRecord) isTermVariant() {

}

type TermVariantSet struct{}

func (TermVariantSet) isTermVariant() {

}

type TermVariantTypeApplication struct{}

func (TermVariantTypeApplication) isTermVariant() {

}

type TermVariantTypeLambda struct{}

func (TermVariantTypeLambda) isTermVariant() {

}

type TermVariantUnion struct{}

func (TermVariantUnion) isTermVariant() {

}

type TermVariantUnit struct{}

func (TermVariantUnit) isTermVariant() {

}

type TermVariantVariable struct{}

func (TermVariantVariable) isTermVariant() {

}

type TermVariantWrap struct{}

func (TermVariantWrap) isTermVariant() {

}

type TypeVariant interface {
  isTypeVariant()
}

type TypeVariantAnnotated struct{}

func (TypeVariantAnnotated) isTypeVariant() {

}

type TypeVariantApplication struct{}

func (TypeVariantApplication) isTypeVariant() {

}

type TypeVariantEither struct{}

func (TypeVariantEither) isTypeVariant() {

}

type TypeVariantForall struct{}

func (TypeVariantForall) isTypeVariant() {

}

type TypeVariantFunction struct{}

func (TypeVariantFunction) isTypeVariant() {

}

type TypeVariantList struct{}

func (TypeVariantList) isTypeVariant() {

}

type TypeVariantLiteral struct{}

func (TypeVariantLiteral) isTypeVariant() {

}

type TypeVariantMap_ struct{}

func (TypeVariantMap_) isTypeVariant() {

}

type TypeVariantMaybe struct{}

func (TypeVariantMaybe) isTypeVariant() {

}

type TypeVariantPair struct{}

func (TypeVariantPair) isTypeVariant() {

}

type TypeVariantRecord struct{}

func (TypeVariantRecord) isTypeVariant() {

}

type TypeVariantSet struct{}

func (TypeVariantSet) isTypeVariant() {

}

type TypeVariantUnion struct{}

func (TypeVariantUnion) isTypeVariant() {

}

type TypeVariantUnit struct{}

func (TypeVariantUnit) isTypeVariant() {

}

type TypeVariantVariable struct{}

func (TypeVariantVariable) isTypeVariant() {

}

type TypeVariantWrap struct{}

func (TypeVariantWrap) isTypeVariant() {

}
