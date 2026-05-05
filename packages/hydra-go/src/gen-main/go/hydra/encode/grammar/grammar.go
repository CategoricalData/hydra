// Note: this is an automatically generated file. Do not edit.

package encodegrammar

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/grammar"
  liblists "hydra.dev/hydra/lib/lists"
)

func Constant (x grammar.Constant) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.grammar.Constant"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func Grammar (x grammar.Grammar) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.grammar.Grammar"), Body: core.TermList{Value: liblists.Map(Production).(func(any) any)(func (v any) any {
    return v
  }(x)).([]any)}}}
}

func Label (x grammar.Label) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.grammar.Label"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func LabeledPattern (x grammar.LabeledPattern) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.grammar.LabeledPattern"), Fields: []any{core.Field{Name: core.Name("label"), Term: Label(func (v any) any {
    return v.(grammar.LabeledPattern).Label
  }(x).(grammar.Label))}, core.Field{Name: core.Name("pattern"), Term: Pattern(func (v any) any {
    return v.(grammar.LabeledPattern).Pattern
  }(x).(grammar.Pattern))}}}}
}

func Pattern (v1 grammar.Pattern) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case grammar.PatternAlternatives:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("alternatives"), Term: core.TermList{Value: liblists.Map(Pattern).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case grammar.PatternConstant:
      return func (y grammar.Constant) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("constant"), Term: Constant(y)}}}
      }(v.Value)
      case grammar.PatternIgnored:
      return func (y grammar.Pattern) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("ignored"), Term: Pattern(y)}}}
      }(v.Value)
      case grammar.PatternLabeled:
      return func (y grammar.LabeledPattern) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("labeled"), Term: LabeledPattern(y)}}}
      }(v.Value)
      case grammar.PatternNil_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("nil"), Term: core.TermUnit{}}}}
      }(v)
      case grammar.PatternNonterminal:
      return func (y grammar.Symbol) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("nonterminal"), Term: Symbol(y)}}}
      }(v.Value)
      case grammar.PatternOption:
      return func (y grammar.Pattern) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("option"), Term: Pattern(y)}}}
      }(v.Value)
      case grammar.PatternPlus:
      return func (y grammar.Pattern) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("plus"), Term: Pattern(y)}}}
      }(v.Value)
      case grammar.PatternRegex:
      return func (y grammar.Regex) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("regex"), Term: Regex(y)}}}
      }(v.Value)
      case grammar.PatternSequence:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("sequence"), Term: core.TermList{Value: liblists.Map(Pattern).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case grammar.PatternStar:
      return func (y grammar.Pattern) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.grammar.Pattern"), Field: core.Field{Name: core.Name("star"), Term: Pattern(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func Production (x grammar.Production) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.grammar.Production"), Fields: []any{core.Field{Name: core.Name("symbol"), Term: Symbol(func (v any) any {
    return v.(grammar.Production).Symbol
  }(x).(grammar.Symbol))}, core.Field{Name: core.Name("pattern"), Term: Pattern(func (v any) any {
    return v.(grammar.Production).Pattern
  }(x).(grammar.Pattern))}}}}
}

func Regex (x grammar.Regex) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.grammar.Regex"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func Symbol (x grammar.Symbol) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.grammar.Symbol"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}
