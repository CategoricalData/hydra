// Note: this is an automatically generated file. Do not edit.

package encodeparsing

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/parsing"
)

func ParseError (x parsing.ParseError) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.parsing.ParseError"), Fields: []any{core.Field{Name: core.Name("message"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(parsing.ParseError).Message
  }(x).(string)}}}, core.Field{Name: core.Name("remainder"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(parsing.ParseError).Remainder
  }(x).(string)}}}}}}
}

func ParseResult[T0 any] (a func(T0) core.Term, v1 parsing.ParseResult[T0]) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case parsing.ParseResultSuccess[T0]:
      return func (y parsing.ParseSuccess[T0]) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.parsing.ParseResult"), Field: core.Field{Name: core.Name("success"), Term: ParseSuccess[T0](a, y)}}}
      }(v.Value)
      case parsing.ParseResultFailure[T0]:
      return func (y parsing.ParseError) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.parsing.ParseResult"), Field: core.Field{Name: core.Name("failure"), Term: ParseError(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func ParseSuccess[T0 any] (a func(T0) core.Term, x parsing.ParseSuccess[T0]) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.parsing.ParseSuccess"), Fields: []any{core.Field{Name: core.Name("value"), Term: a(func (v any) any {
    return v.(parsing.ParseSuccess[T0]).Value
  }(x).(T0))}, core.Field{Name: core.Name("remainder"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(parsing.ParseSuccess[T0]).Remainder
  }(x).(string)}}}}}}
}
