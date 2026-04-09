// Note: this is an automatically generated file. Do not edit.

package encodeast

import (
  "hydra.dev/hydra/ast"
  "hydra.dev/hydra/core"
  liblists "hydra.dev/hydra/lib/lists"
  libmaybes "hydra.dev/hydra/lib/maybes"
)

func Associativity (v1 ast.Associativity) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case ast.AssociativityNone:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Associativity"), Field: core.Field{Name: core.Name("none"), Term: core.TermUnit{}}}}
      }(v)
      case ast.AssociativityLeft:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Associativity"), Field: core.Field{Name: core.Name("left"), Term: core.TermUnit{}}}}
      }(v)
      case ast.AssociativityRight:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Associativity"), Field: core.Field{Name: core.Name("right"), Term: core.TermUnit{}}}}
      }(v)
      case ast.AssociativityBoth:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Associativity"), Field: core.Field{Name: core.Name("both"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func BlockStyle (x ast.BlockStyle) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.ast.BlockStyle"), Fields: []any{core.Field{Name: core.Name("indent"), Term: core.TermMaybe{Value: libmaybes.Map(func (x2 string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: x2}}
  }).(func(any) any)(func (v any) any {
    return v.(ast.BlockStyle).Indent
  }(x))}}, core.Field{Name: core.Name("newlineBeforeContent"), Term: core.TermLiteral{Value: core.LiteralBoolean{Value: func (v any) any {
    return v.(ast.BlockStyle).NewlineBeforeContent
  }(x).(bool)}}}, core.Field{Name: core.Name("newlineAfterContent"), Term: core.TermLiteral{Value: core.LiteralBoolean{Value: func (v any) any {
    return v.(ast.BlockStyle).NewlineAfterContent
  }(x).(bool)}}}}}}
}

func BracketExpr (x ast.BracketExpr) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.ast.BracketExpr"), Fields: []any{core.Field{Name: core.Name("brackets"), Term: Brackets(func (v any) any {
    return v.(ast.BracketExpr).Brackets
  }(x).(ast.Brackets))}, core.Field{Name: core.Name("enclosed"), Term: Expr(func (v any) any {
    return v.(ast.BracketExpr).Enclosed
  }(x).(ast.Expr))}, core.Field{Name: core.Name("style"), Term: BlockStyle(func (v any) any {
    return v.(ast.BracketExpr).Style
  }(x).(ast.BlockStyle))}}}}
}

func Brackets (x ast.Brackets) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.ast.Brackets"), Fields: []any{core.Field{Name: core.Name("open"), Term: Symbol(func (v any) any {
    return v.(ast.Brackets).Open
  }(x).(ast.Symbol))}, core.Field{Name: core.Name("close"), Term: Symbol(func (v any) any {
    return v.(ast.Brackets).Close_
  }(x).(ast.Symbol))}}}}
}

func Expr (v1 ast.Expr) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case ast.ExprConst_:
      return func (y ast.Symbol) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Expr"), Field: core.Field{Name: core.Name("const"), Term: Symbol(y)}}}
      }(v.Value)
      case ast.ExprIndent:
      return func (y ast.IndentedExpression) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Expr"), Field: core.Field{Name: core.Name("indent"), Term: IndentedExpression(y)}}}
      }(v.Value)
      case ast.ExprOp:
      return func (y ast.OpExpr) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Expr"), Field: core.Field{Name: core.Name("op"), Term: OpExpr(y)}}}
      }(v.Value)
      case ast.ExprBrackets:
      return func (y ast.BracketExpr) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Expr"), Field: core.Field{Name: core.Name("brackets"), Term: BracketExpr(y)}}}
      }(v.Value)
      case ast.ExprSeq:
      return func (y ast.SeqExpr) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Expr"), Field: core.Field{Name: core.Name("seq"), Term: SeqExpr(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func IndentedExpression (x ast.IndentedExpression) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.ast.IndentedExpression"), Fields: []any{core.Field{Name: core.Name("style"), Term: IndentStyle(func (v any) any {
    return v.(ast.IndentedExpression).Style
  }(x).(ast.IndentStyle))}, core.Field{Name: core.Name("expr"), Term: Expr(func (v any) any {
    return v.(ast.IndentedExpression).Expr
  }(x).(ast.Expr))}}}}
}

func IndentStyle (v1 ast.IndentStyle) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case ast.IndentStyleAllLines:
      return func (y string) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.IndentStyle"), Field: core.Field{Name: core.Name("allLines"), Term: core.TermLiteral{Value: core.LiteralString_{Value: y}}}}}
      }(v.Value)
      case ast.IndentStyleSubsequentLines:
      return func (y string) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.IndentStyle"), Field: core.Field{Name: core.Name("subsequentLines"), Term: core.TermLiteral{Value: core.LiteralString_{Value: y}}}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func Op (x ast.Op) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.ast.Op"), Fields: []any{core.Field{Name: core.Name("symbol"), Term: Symbol(func (v any) any {
    return v.(ast.Op).Symbol
  }(x).(ast.Symbol))}, core.Field{Name: core.Name("padding"), Term: Padding(func (v any) any {
    return v.(ast.Op).Padding
  }(x).(ast.Padding))}, core.Field{Name: core.Name("precedence"), Term: Precedence(func (v any) any {
    return v.(ast.Op).Precedence
  }(x).(ast.Precedence))}, core.Field{Name: core.Name("associativity"), Term: Associativity(func (v any) any {
    return v.(ast.Op).Associativity
  }(x).(ast.Associativity))}}}}
}

func OpExpr (x ast.OpExpr) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.ast.OpExpr"), Fields: []any{core.Field{Name: core.Name("op"), Term: Op(func (v any) any {
    return v.(ast.OpExpr).Op
  }(x).(ast.Op))}, core.Field{Name: core.Name("lhs"), Term: Expr(func (v any) any {
    return v.(ast.OpExpr).Lhs
  }(x).(ast.Expr))}, core.Field{Name: core.Name("rhs"), Term: Expr(func (v any) any {
    return v.(ast.OpExpr).Rhs
  }(x).(ast.Expr))}}}}
}

func Padding (x ast.Padding) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.ast.Padding"), Fields: []any{core.Field{Name: core.Name("left"), Term: Ws(func (v any) any {
    return v.(ast.Padding).Left
  }(x).(ast.Ws))}, core.Field{Name: core.Name("right"), Term: Ws(func (v any) any {
    return v.(ast.Padding).Right
  }(x).(ast.Ws))}}}}
}

func Precedence (x ast.Precedence) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.ast.Precedence"), Body: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: func (v any) any {
    return v
  }(x).(int32)}}}}}
}

func SeqExpr (x ast.SeqExpr) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.ast.SeqExpr"), Fields: []any{core.Field{Name: core.Name("op"), Term: Op(func (v any) any {
    return v.(ast.SeqExpr).Op
  }(x).(ast.Op))}, core.Field{Name: core.Name("elements"), Term: core.TermList{Value: liblists.Map(Expr).(func(any) any)(func (v any) any {
    return v.(ast.SeqExpr).Elements
  }(x)).([]any)}}}}}
}

func Symbol (x ast.Symbol) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.ast.Symbol"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func Ws (v1 ast.Ws) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case ast.WsNone:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Ws"), Field: core.Field{Name: core.Name("none"), Term: core.TermUnit{}}}}
      }(v)
      case ast.WsSpace:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Ws"), Field: core.Field{Name: core.Name("space"), Term: core.TermUnit{}}}}
      }(v)
      case ast.WsBreak_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Ws"), Field: core.Field{Name: core.Name("break"), Term: core.TermUnit{}}}}
      }(v)
      case ast.WsBreakAndIndent:
      return func (y string) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Ws"), Field: core.Field{Name: core.Name("breakAndIndent"), Term: core.TermLiteral{Value: core.LiteralString_{Value: y}}}}}
      }(v.Value)
      case ast.WsDoubleBreak:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.ast.Ws"), Field: core.Field{Name: core.Name("doubleBreak"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}
