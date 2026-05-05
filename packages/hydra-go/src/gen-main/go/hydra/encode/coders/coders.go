// Note: this is an automatically generated file. Do not edit.

package encodecoders

import (
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/core"
)

func CoderDirection (v1 coders.CoderDirection) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case coders.CoderDirectionEncode:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.coders.CoderDirection"), Field: core.Field{Name: core.Name("encode"), Term: core.TermUnit{}}}}
      }(v)
      case coders.CoderDirectionDecode:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.coders.CoderDirection"), Field: core.Field{Name: core.Name("decode"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func LanguageName (x coders.LanguageName) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.coders.LanguageName"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func TraversalOrder (v1 coders.TraversalOrder) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case coders.TraversalOrderPre:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.coders.TraversalOrder"), Field: core.Field{Name: core.Name("pre"), Term: core.TermUnit{}}}}
      }(v)
      case coders.TraversalOrderPost:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.coders.TraversalOrder"), Field: core.Field{Name: core.Name("post"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}
