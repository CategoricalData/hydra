// Note: this is an automatically generated file. Do not edit.

package encodevariants

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/variants"
)

func EliminationVariant (v1 variants.EliminationVariant) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case variants.EliminationVariantRecord:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.EliminationVariant"), Field: core.Field{Name: core.Name("record"), Term: core.TermUnit{}}}}
      }(v)
      case variants.EliminationVariantUnion:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.EliminationVariant"), Field: core.Field{Name: core.Name("union"), Term: core.TermUnit{}}}}
      }(v)
      case variants.EliminationVariantWrap:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.EliminationVariant"), Field: core.Field{Name: core.Name("wrap"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func FunctionVariant (v1 variants.FunctionVariant) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case variants.FunctionVariantElimination:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.FunctionVariant"), Field: core.Field{Name: core.Name("elimination"), Term: core.TermUnit{}}}}
      }(v)
      case variants.FunctionVariantLambda:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.FunctionVariant"), Field: core.Field{Name: core.Name("lambda"), Term: core.TermUnit{}}}}
      }(v)
      case variants.FunctionVariantPrimitive:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.FunctionVariant"), Field: core.Field{Name: core.Name("primitive"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func LiteralVariant (v1 variants.LiteralVariant) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case variants.LiteralVariantBinary:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.LiteralVariant"), Field: core.Field{Name: core.Name("binary"), Term: core.TermUnit{}}}}
      }(v)
      case variants.LiteralVariantBoolean:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.LiteralVariant"), Field: core.Field{Name: core.Name("boolean"), Term: core.TermUnit{}}}}
      }(v)
      case variants.LiteralVariantFloat:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.LiteralVariant"), Field: core.Field{Name: core.Name("float"), Term: core.TermUnit{}}}}
      }(v)
      case variants.LiteralVariantInteger:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.LiteralVariant"), Field: core.Field{Name: core.Name("integer"), Term: core.TermUnit{}}}}
      }(v)
      case variants.LiteralVariantString_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.LiteralVariant"), Field: core.Field{Name: core.Name("string"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func TermVariant (v1 variants.TermVariant) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case variants.TermVariantAnnotated:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("annotated"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantApplication:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("application"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantEither:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("either"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantFunction:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("function"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantLet:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("let"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantList:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("list"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantLiteral:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("literal"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantMap_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("map"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantMaybe:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("maybe"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantPair:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("pair"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantRecord:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("record"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantSet:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("set"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantTypeApplication:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("typeApplication"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantTypeLambda:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("typeLambda"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantUnion:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("union"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantUnit:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("unit"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantVariable:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("variable"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TermVariantWrap:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TermVariant"), Field: core.Field{Name: core.Name("wrap"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func TypeVariant (v1 variants.TypeVariant) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case variants.TypeVariantAnnotated:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("annotated"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantApplication:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("application"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantEither:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("either"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantForall:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("forall"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantFunction:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("function"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantList:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("list"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantLiteral:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("literal"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantMap_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("map"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantMaybe:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("maybe"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantPair:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("pair"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantRecord:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("record"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantSet:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("set"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantUnion:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("union"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantUnit:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("unit"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantVariable:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("variable"), Term: core.TermUnit{}}}}
      }(v)
      case variants.TypeVariantWrap:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.variants.TypeVariant"), Field: core.Field{Name: core.Name("wrap"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}
