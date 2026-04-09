// Note: this is an automatically generated file. Do not edit.

package encodejsonmodel

import (
  "hydra.dev/hydra/core"
  jsonmodel "hydra.dev/hydra/json/model"
  liblists "hydra.dev/hydra/lib/lists"
  libmaps "hydra.dev/hydra/lib/maps"
)

func Value (v1 jsonmodel.Value) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueArray:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.json.model.Value"), Field: core.Field{Name: core.Name("array"), Term: core.TermList{Value: liblists.Map(Value).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case jsonmodel.ValueBoolean:
      return func (y bool) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.json.model.Value"), Field: core.Field{Name: core.Name("boolean"), Term: core.TermLiteral{Value: core.LiteralBoolean{Value: y}}}}}
      }(v.Value)
      case jsonmodel.ValueNull:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.json.model.Value"), Field: core.Field{Name: core.Name("null"), Term: core.TermUnit{}}}}
      }(v)
      case jsonmodel.ValueNumber:
      return func (y float64) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.json.model.Value"), Field: core.Field{Name: core.Name("number"), Term: core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueBigfloat{Value: y}}}}}}
      }(v.Value)
      case jsonmodel.ValueObject:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.json.model.Value"), Field: core.Field{Name: core.Name("object"), Term: core.TermMap_{Value: libmaps.Bimap(func (x string) any {
          return core.TermLiteral{Value: core.LiteralString_{Value: x}}
        }).(func(any) any)(Value).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case jsonmodel.ValueString_:
      return func (y string) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.json.model.Value"), Field: core.Field{Name: core.Name("string"), Term: core.TermLiteral{Value: core.LiteralString_{Value: y}}}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}
