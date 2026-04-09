// Note: this is an automatically generated file. Do not edit.

package encodeutil

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/util"
)

func CaseConvention (v1 util.CaseConvention) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case util.CaseConventionCamel:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.util.CaseConvention"), Field: core.Field{Name: core.Name("camel"), Term: core.TermUnit{}}}}
      }(v)
      case util.CaseConventionPascal:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.util.CaseConvention"), Field: core.Field{Name: core.Name("pascal"), Term: core.TermUnit{}}}}
      }(v)
      case util.CaseConventionLowerSnake:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.util.CaseConvention"), Field: core.Field{Name: core.Name("lowerSnake"), Term: core.TermUnit{}}}}
      }(v)
      case util.CaseConventionUpperSnake:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.util.CaseConvention"), Field: core.Field{Name: core.Name("upperSnake"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func Comparison (v1 util.Comparison) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case util.ComparisonLessThan:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.util.Comparison"), Field: core.Field{Name: core.Name("lessThan"), Term: core.TermUnit{}}}}
      }(v)
      case util.ComparisonEqualTo:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.util.Comparison"), Field: core.Field{Name: core.Name("equalTo"), Term: core.TermUnit{}}}}
      }(v)
      case util.ComparisonGreaterThan:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.util.Comparison"), Field: core.Field{Name: core.Name("greaterThan"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func Precision (v1 util.Precision) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case util.PrecisionArbitrary:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.util.Precision"), Field: core.Field{Name: core.Name("arbitrary"), Term: core.TermUnit{}}}}
      }(v)
      case util.PrecisionBits:
      return func (y int32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.util.Precision"), Field: core.Field{Name: core.Name("bits"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: y}}}}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}
