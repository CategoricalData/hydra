// Note: this is an automatically generated file. Do not edit.

package showmeta

import "hydra.dev/hydra/variants"

func TermVariant (v1 variants.TermVariant) string {
  return func (x any) any {
    switch v := x.(type) {
      case variants.TermVariantAnnotated:
      return func (_ struct{}) any {
        return "annotated"
      }(v)
      case variants.TermVariantApplication:
      return func (_ struct{}) any {
        return "application"
      }(v)
      case variants.TermVariantEither:
      return func (_ struct{}) any {
        return "either"
      }(v)
      case variants.TermVariantFunction:
      return func (_ struct{}) any {
        return "function"
      }(v)
      case variants.TermVariantLet:
      return func (_ struct{}) any {
        return "let"
      }(v)
      case variants.TermVariantList:
      return func (_ struct{}) any {
        return "list"
      }(v)
      case variants.TermVariantLiteral:
      return func (_ struct{}) any {
        return "literal"
      }(v)
      case variants.TermVariantMap_:
      return func (_ struct{}) any {
        return "map"
      }(v)
      case variants.TermVariantMaybe:
      return func (_ struct{}) any {
        return "maybe"
      }(v)
      case variants.TermVariantPair:
      return func (_ struct{}) any {
        return "pair"
      }(v)
      case variants.TermVariantRecord:
      return func (_ struct{}) any {
        return "record"
      }(v)
      case variants.TermVariantSet:
      return func (_ struct{}) any {
        return "set"
      }(v)
      case variants.TermVariantTypeLambda:
      return func (_ struct{}) any {
        return "typeLambda"
      }(v)
      case variants.TermVariantTypeApplication:
      return func (_ struct{}) any {
        return "typeApplication"
      }(v)
      case variants.TermVariantUnion:
      return func (_ struct{}) any {
        return "union"
      }(v)
      case variants.TermVariantUnit:
      return func (_ struct{}) any {
        return "unit"
      }(v)
      case variants.TermVariantVariable:
      return func (_ struct{}) any {
        return "variable"
      }(v)
      case variants.TermVariantWrap:
      return func (_ struct{}) any {
        return "wrap"
      }(v)
    }
    return nil
  }(v1).(string)
}

func TypeVariant (v1 variants.TypeVariant) string {
  return func (x any) any {
    switch v := x.(type) {
      case variants.TypeVariantAnnotated:
      return func (_ struct{}) any {
        return "annotated"
      }(v)
      case variants.TypeVariantApplication:
      return func (_ struct{}) any {
        return "application"
      }(v)
      case variants.TypeVariantEither:
      return func (_ struct{}) any {
        return "either"
      }(v)
      case variants.TypeVariantForall:
      return func (_ struct{}) any {
        return "forall"
      }(v)
      case variants.TypeVariantFunction:
      return func (_ struct{}) any {
        return "function"
      }(v)
      case variants.TypeVariantList:
      return func (_ struct{}) any {
        return "list"
      }(v)
      case variants.TypeVariantLiteral:
      return func (_ struct{}) any {
        return "literal"
      }(v)
      case variants.TypeVariantMap_:
      return func (_ struct{}) any {
        return "map"
      }(v)
      case variants.TypeVariantMaybe:
      return func (_ struct{}) any {
        return "maybe"
      }(v)
      case variants.TypeVariantPair:
      return func (_ struct{}) any {
        return "pair"
      }(v)
      case variants.TypeVariantRecord:
      return func (_ struct{}) any {
        return "record"
      }(v)
      case variants.TypeVariantSet:
      return func (_ struct{}) any {
        return "set"
      }(v)
      case variants.TypeVariantUnion:
      return func (_ struct{}) any {
        return "union"
      }(v)
      case variants.TypeVariantUnit:
      return func (_ struct{}) any {
        return "unit"
      }(v)
      case variants.TypeVariantVariable:
      return func (_ struct{}) any {
        return "variable"
      }(v)
      case variants.TypeVariantWrap:
      return func (_ struct{}) any {
        return "wrap"
      }(v)
    }
    return nil
  }(v1).(string)
}
