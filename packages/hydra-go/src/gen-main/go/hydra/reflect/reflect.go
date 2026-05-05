// Note: this is an automatically generated file. Do not edit.

package reflect

import (
  "hydra.dev/hydra/core"
  liblists "hydra.dev/hydra/lib/lists"
  "hydra.dev/hydra/util"
  "hydra.dev/hydra/variants"
  "math/big"
)

func EliminationVariant (v1 core.Elimination) variants.EliminationVariant {
  return func (x any) any {
    switch v := x.(type) {
      case core.EliminationRecord:
      return func (_ core.Projection) any {
        return variants.EliminationVariantRecord{}
      }(v.Value)
      case core.EliminationUnion:
      return func (_ core.CaseStatement) any {
        return variants.EliminationVariantUnion{}
      }(v.Value)
      case core.EliminationWrap:
      return func (_ core.Name) any {
        return variants.EliminationVariantWrap{}
      }(v.Value)
    }
    return nil
  }(v1).(variants.EliminationVariant)
}

var EliminationVariants = []any{variants.EliminationVariantRecord{}, variants.EliminationVariantUnion{}, variants.EliminationVariantWrap{}}

func FloatTypePrecision (v1 core.FloatType) util.Precision {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatTypeBigfloat:
      return func (_ struct{}) any {
        return util.PrecisionArbitrary{}
      }(v)
      case core.FloatTypeFloat32_:
      return func (_ struct{}) any {
        return util.PrecisionBits{Value: 32}
      }(v)
      case core.FloatTypeFloat64_:
      return func (_ struct{}) any {
        return util.PrecisionBits{Value: 64}
      }(v)
    }
    return nil
  }(v1).(util.Precision)
}

var FloatTypes = []any{core.FloatTypeBigfloat{}, core.FloatTypeFloat32_{}, core.FloatTypeFloat64_{}}

func FloatValueType (v1 core.FloatValue) core.FloatType {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatValueBigfloat:
      return func (_ float64) any {
        return core.FloatTypeBigfloat{}
      }(v.Value)
      case core.FloatValueFloat32_:
      return func (_ float32) any {
        return core.FloatTypeFloat32_{}
      }(v.Value)
      case core.FloatValueFloat64_:
      return func (_ float64) any {
        return core.FloatTypeFloat64_{}
      }(v.Value)
    }
    return nil
  }(v1).(core.FloatType)
}

func FunctionVariant (v1 core.Function) variants.FunctionVariant {
  return func (x any) any {
    switch v := x.(type) {
      case core.FunctionElimination:
      return func (_ core.Elimination) any {
        return variants.FunctionVariantElimination{}
      }(v.Value)
      case core.FunctionLambda:
      return func (_ core.Lambda) any {
        return variants.FunctionVariantLambda{}
      }(v.Value)
      case core.FunctionPrimitive:
      return func (_ core.Name) any {
        return variants.FunctionVariantPrimitive{}
      }(v.Value)
    }
    return nil
  }(v1).(variants.FunctionVariant)
}

var FunctionVariants = []any{variants.FunctionVariantElimination{}, variants.FunctionVariantLambda{}, variants.FunctionVariantPrimitive{}}

func IntegerTypeIsSigned (v1 core.IntegerType) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerTypeBigint:
      return func (_ struct{}) any {
        return true
      }(v)
      case core.IntegerTypeInt8_:
      return func (_ struct{}) any {
        return true
      }(v)
      case core.IntegerTypeInt16_:
      return func (_ struct{}) any {
        return true
      }(v)
      case core.IntegerTypeInt32_:
      return func (_ struct{}) any {
        return true
      }(v)
      case core.IntegerTypeInt64_:
      return func (_ struct{}) any {
        return true
      }(v)
      case core.IntegerTypeUint8_:
      return func (_ struct{}) any {
        return false
      }(v)
      case core.IntegerTypeUint16_:
      return func (_ struct{}) any {
        return false
      }(v)
      case core.IntegerTypeUint32_:
      return func (_ struct{}) any {
        return false
      }(v)
      case core.IntegerTypeUint64_:
      return func (_ struct{}) any {
        return false
      }(v)
    }
    return nil
  }(v1).(bool)
}

func IntegerTypePrecision (v1 core.IntegerType) util.Precision {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerTypeBigint:
      return func (_ struct{}) any {
        return util.PrecisionArbitrary{}
      }(v)
      case core.IntegerTypeInt8_:
      return func (_ struct{}) any {
        return util.PrecisionBits{Value: 8}
      }(v)
      case core.IntegerTypeInt16_:
      return func (_ struct{}) any {
        return util.PrecisionBits{Value: 16}
      }(v)
      case core.IntegerTypeInt32_:
      return func (_ struct{}) any {
        return util.PrecisionBits{Value: 32}
      }(v)
      case core.IntegerTypeInt64_:
      return func (_ struct{}) any {
        return util.PrecisionBits{Value: 64}
      }(v)
      case core.IntegerTypeUint8_:
      return func (_ struct{}) any {
        return util.PrecisionBits{Value: 8}
      }(v)
      case core.IntegerTypeUint16_:
      return func (_ struct{}) any {
        return util.PrecisionBits{Value: 16}
      }(v)
      case core.IntegerTypeUint32_:
      return func (_ struct{}) any {
        return util.PrecisionBits{Value: 32}
      }(v)
      case core.IntegerTypeUint64_:
      return func (_ struct{}) any {
        return util.PrecisionBits{Value: 64}
      }(v)
    }
    return nil
  }(v1).(util.Precision)
}

var IntegerTypes = []any{core.IntegerTypeBigint{}, core.IntegerTypeInt8_{}, core.IntegerTypeInt16_{}, core.IntegerTypeInt32_{}, core.IntegerTypeInt64_{}, core.IntegerTypeUint8_{}, core.IntegerTypeUint16_{}, core.IntegerTypeUint32_{}, core.IntegerTypeUint64_{}}

func IntegerValueType (v1 core.IntegerValue) core.IntegerType {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueBigint:
      return func (_ *big.Int) any {
        return core.IntegerTypeBigint{}
      }(v.Value)
      case core.IntegerValueInt8_:
      return func (_ int8) any {
        return core.IntegerTypeInt8_{}
      }(v.Value)
      case core.IntegerValueInt16_:
      return func (_ int16) any {
        return core.IntegerTypeInt16_{}
      }(v.Value)
      case core.IntegerValueInt32_:
      return func (_ int32) any {
        return core.IntegerTypeInt32_{}
      }(v.Value)
      case core.IntegerValueInt64_:
      return func (_ int64) any {
        return core.IntegerTypeInt64_{}
      }(v.Value)
      case core.IntegerValueUint8_:
      return func (_ uint8) any {
        return core.IntegerTypeUint8_{}
      }(v.Value)
      case core.IntegerValueUint16_:
      return func (_ uint16) any {
        return core.IntegerTypeUint16_{}
      }(v.Value)
      case core.IntegerValueUint32_:
      return func (_ uint32) any {
        return core.IntegerTypeUint32_{}
      }(v.Value)
      case core.IntegerValueUint64_:
      return func (_ uint64) any {
        return core.IntegerTypeUint64_{}
      }(v.Value)
    }
    return nil
  }(v1).(core.IntegerType)
}

func LiteralType (v1 core.Literal) core.LiteralType {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralBinary:
      return func (_ []byte) any {
        return core.LiteralTypeBinary{}
      }(v.Value)
      case core.LiteralBoolean:
      return func (_ bool) any {
        return core.LiteralTypeBoolean{}
      }(v.Value)
      case core.LiteralFloat:
      return func (arg_ core.FloatValue) any {
        return core.LiteralTypeFloat{Value: FloatValueType(arg_)}
      }(v.Value)
      case core.LiteralInteger:
      return func (arg_ core.IntegerValue) any {
        return core.LiteralTypeInteger{Value: IntegerValueType(arg_)}
      }(v.Value)
      case core.LiteralString_:
      return func (_ string) any {
        return core.LiteralTypeString_{}
      }(v.Value)
    }
    return nil
  }(v1).(core.LiteralType)
}

func LiteralTypeVariant (v1 core.LiteralType) variants.LiteralVariant {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralTypeBinary:
      return func (_ struct{}) any {
        return variants.LiteralVariantBinary{}
      }(v)
      case core.LiteralTypeBoolean:
      return func (_ struct{}) any {
        return variants.LiteralVariantBoolean{}
      }(v)
      case core.LiteralTypeFloat:
      return func (_ core.FloatType) any {
        return variants.LiteralVariantFloat{}
      }(v.Value)
      case core.LiteralTypeInteger:
      return func (_ core.IntegerType) any {
        return variants.LiteralVariantInteger{}
      }(v.Value)
      case core.LiteralTypeString_:
      return func (_ struct{}) any {
        return variants.LiteralVariantString_{}
      }(v)
    }
    return nil
  }(v1).(variants.LiteralVariant)
}

var LiteralTypes = liblists.Concat([]any{[]any{core.LiteralTypeBinary{}, core.LiteralTypeBoolean{}}, liblists.Map(func (x core.FloatType) any {
  return core.LiteralTypeFloat{Value: x}
}).(func(any) any)(FloatTypes), liblists.Map(func (x core.IntegerType) any {
  return core.LiteralTypeInteger{Value: x}
}).(func(any) any)(IntegerTypes), []any{core.LiteralTypeString_{}}}).([]any)

func LiteralVariant (arg_ core.Literal) variants.LiteralVariant {
  return LiteralTypeVariant(LiteralType(arg_))
}

var LiteralVariants = []any{variants.LiteralVariantBinary{}, variants.LiteralVariantBoolean{}, variants.LiteralVariantFloat{}, variants.LiteralVariantInteger{}, variants.LiteralVariantString_{}}

func TermVariant (v1 core.Term) variants.TermVariant {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (_ core.AnnotatedTerm) any {
        return variants.TermVariantAnnotated{}
      }(v.Value)
      case core.TermApplication:
      return func (_ core.Application) any {
        return variants.TermVariantApplication{}
      }(v.Value)
      case core.TermEither:
      return func (_ any) any {
        return variants.TermVariantEither{}
      }(v.Value)
      case core.TermFunction:
      return func (_ core.Function) any {
        return variants.TermVariantFunction{}
      }(v.Value)
      case core.TermLet:
      return func (_ core.Let) any {
        return variants.TermVariantLet{}
      }(v.Value)
      case core.TermList:
      return func (_ []any) any {
        return variants.TermVariantList{}
      }(v.Value)
      case core.TermLiteral:
      return func (_ core.Literal) any {
        return variants.TermVariantLiteral{}
      }(v.Value)
      case core.TermMap_:
      return func (_ []any) any {
        return variants.TermVariantMap_{}
      }(v.Value)
      case core.TermMaybe:
      return func (_ any) any {
        return variants.TermVariantMaybe{}
      }(v.Value)
      case core.TermPair:
      return func (_ any) any {
        return variants.TermVariantPair{}
      }(v.Value)
      case core.TermRecord:
      return func (_ core.Record) any {
        return variants.TermVariantRecord{}
      }(v.Value)
      case core.TermSet:
      return func (_ []any) any {
        return variants.TermVariantSet{}
      }(v.Value)
      case core.TermTypeApplication:
      return func (_ core.TypeApplicationTerm) any {
        return variants.TermVariantTypeApplication{}
      }(v.Value)
      case core.TermTypeLambda:
      return func (_ core.TypeLambda) any {
        return variants.TermVariantTypeLambda{}
      }(v.Value)
      case core.TermUnion:
      return func (_ core.Injection) any {
        return variants.TermVariantUnion{}
      }(v.Value)
      case core.TermUnit:
      return func (_ struct{}) any {
        return variants.TermVariantUnit{}
      }(v)
      case core.TermVariable:
      return func (_ core.Name) any {
        return variants.TermVariantVariable{}
      }(v.Value)
      case core.TermWrap:
      return func (_ core.WrappedTerm) any {
        return variants.TermVariantWrap{}
      }(v.Value)
    }
    return nil
  }(v1).(variants.TermVariant)
}

var TermVariants = []any{variants.TermVariantAnnotated{}, variants.TermVariantApplication{}, variants.TermVariantEither{}, variants.TermVariantFunction{}, variants.TermVariantList{}, variants.TermVariantLiteral{}, variants.TermVariantMap_{}, variants.TermVariantMaybe{}, variants.TermVariantPair{}, variants.TermVariantRecord{}, variants.TermVariantSet{}, variants.TermVariantTypeLambda{}, variants.TermVariantTypeApplication{}, variants.TermVariantUnion{}, variants.TermVariantUnit{}, variants.TermVariantVariable{}, variants.TermVariantWrap{}}

func TypeVariant (v1 core.Type) variants.TypeVariant {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (_ core.AnnotatedType) any {
        return variants.TypeVariantAnnotated{}
      }(v.Value)
      case core.TypeApplication:
      return func (_ core.ApplicationType) any {
        return variants.TypeVariantApplication{}
      }(v.Value)
      case core.TypeEither:
      return func (_ core.EitherType) any {
        return variants.TypeVariantEither{}
      }(v.Value)
      case core.TypeFunction:
      return func (_ core.FunctionType) any {
        return variants.TypeVariantFunction{}
      }(v.Value)
      case core.TypeForall:
      return func (_ core.ForallType) any {
        return variants.TypeVariantForall{}
      }(v.Value)
      case core.TypeList:
      return func (_ core.Type) any {
        return variants.TypeVariantList{}
      }(v.Value)
      case core.TypeLiteral:
      return func (_ core.LiteralType) any {
        return variants.TypeVariantLiteral{}
      }(v.Value)
      case core.TypeMap_:
      return func (_ core.MapType) any {
        return variants.TypeVariantMap_{}
      }(v.Value)
      case core.TypeMaybe:
      return func (_ core.Type) any {
        return variants.TypeVariantMaybe{}
      }(v.Value)
      case core.TypePair:
      return func (_ core.PairType) any {
        return variants.TypeVariantPair{}
      }(v.Value)
      case core.TypeRecord:
      return func (_ []any) any {
        return variants.TypeVariantRecord{}
      }(v.Value)
      case core.TypeSet:
      return func (_ core.Type) any {
        return variants.TypeVariantSet{}
      }(v.Value)
      case core.TypeUnion:
      return func (_ []any) any {
        return variants.TypeVariantUnion{}
      }(v.Value)
      case core.TypeUnit:
      return func (_ struct{}) any {
        return variants.TypeVariantUnit{}
      }(v)
      case core.TypeVariable:
      return func (_ core.Name) any {
        return variants.TypeVariantVariable{}
      }(v.Value)
      case core.TypeWrap:
      return func (_ core.Type) any {
        return variants.TypeVariantWrap{}
      }(v.Value)
    }
    return nil
  }(v1).(variants.TypeVariant)
}

var TypeVariants = []any{variants.TypeVariantAnnotated{}, variants.TypeVariantApplication{}, variants.TypeVariantEither{}, variants.TypeVariantFunction{}, variants.TypeVariantForall{}, variants.TypeVariantList{}, variants.TypeVariantLiteral{}, variants.TypeVariantMap_{}, variants.TypeVariantWrap{}, variants.TypeVariantMaybe{}, variants.TypeVariantPair{}, variants.TypeVariantRecord{}, variants.TypeVariantSet{}, variants.TypeVariantUnion{}, variants.TypeVariantUnit{}, variants.TypeVariantVariable{}}
