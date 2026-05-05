// Note: this is an automatically generated file. Do not edit.

package jsonwriter

import (
  "hydra.dev/hydra/ast"
  jsonmodel "hydra.dev/hydra/json/model"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  libliterals "hydra.dev/hydra/lib/literals"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmath "hydra.dev/hydra/lib/math"
  libpairs "hydra.dev/hydra/lib/pairs"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/serialization"
)

var ColonOp = ast.Op{Symbol: ast.Symbol(":"), Padding: ast.Padding{Left: ast.WsNone{}, Right: ast.WsSpace{}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}

func JsonString (s string) string {
  return func () any {
    hexEscape := func (c int32) any {
      return func () any {
        var hi any = libstrings.FromList(liblists.Pure(libstrings.CharAt(libmath.Div(c).(func(any) any)(16)).(func(any) any)("0123456789abcdef")))
        return func () any {
          var lo any = libstrings.FromList(liblists.Pure(libstrings.CharAt(libmath.Mod(c).(func(any) any)(16)).(func(any) any)("0123456789abcdef")))
          return libstrings.Cat2(libstrings.Cat2("\\u00").(func(any) any)(hi)).(func(any) any)(lo)
        }()
      }()
    }
    return func () any {
      escape := func (c int32) any {
        return liblogic.IfElse(libequality.Equal(c).(func(any) any)(34)).(func(any) any)("\\\"").(func(any) any)(liblogic.IfElse(libequality.Equal(c).(func(any) any)(92)).(func(any) any)("\\\\").(func(any) any)(liblogic.IfElse(libequality.Equal(c).(func(any) any)(8)).(func(any) any)("\\b").(func(any) any)(liblogic.IfElse(libequality.Equal(c).(func(any) any)(12)).(func(any) any)("\\f").(func(any) any)(liblogic.IfElse(libequality.Equal(c).(func(any) any)(10)).(func(any) any)("\\n").(func(any) any)(liblogic.IfElse(libequality.Equal(c).(func(any) any)(13)).(func(any) any)("\\r").(func(any) any)(liblogic.IfElse(libequality.Equal(c).(func(any) any)(9)).(func(any) any)("\\t").(func(any) any)(liblogic.IfElse(libequality.Lt(c).(func(any) any)(32)).(func(any) any)(hexEscape(c)).(func(any) any)(libstrings.FromList(liblists.Pure(c))))))))))
      }
      return func () any {
        var escaped any = libstrings.Cat(liblists.Map(escape).(func(any) any)(libstrings.ToList(s)))
        return libstrings.Cat2(libstrings.Cat2("\"").(func(any) any)(escaped)).(func(any) any)("\"")
      }()
    }()
  }().(string)
}

func KeyValueToExpr (pair any) ast.Expr {
  return func () any {
    var key any = libpairs.First(pair)
    return func () any {
      var value any = libpairs.Second(pair)
      return serialization.Ifx(ColonOp, serialization.Cst(JsonString(key.(string))), ValueToExpr(value.(jsonmodel.Value)))
    }()
  }().(ast.Expr)
}

func PrintJson (value jsonmodel.Value) string {
  return serialization.PrintExpr(ValueToExpr(value))
}

func ValueToExpr (value jsonmodel.Value) ast.Expr {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueArray:
      return func (arr []any) any {
        return serialization.BracketListAdaptive(liblists.Map(ValueToExpr).(func(any) any)(arr).([]any))
      }(v.Value)
      case jsonmodel.ValueBoolean:
      return func (b bool) any {
        return serialization.Cst(liblogic.IfElse(b).(func(any) any)("true").(func(any) any)("false").(string))
      }(v.Value)
      case jsonmodel.ValueNull:
      return func (_ struct{}) any {
        return serialization.Cst("null")
      }(v)
      case jsonmodel.ValueNumber:
      return func (n float64) any {
        return func () any {
          var rounded any = libliterals.BigfloatToBigint(n)
          return serialization.Cst(liblogic.IfElse(libequality.Equal(n).(func(any) any)(libliterals.BigintToBigfloat(rounded))).(func(any) any)(libliterals.ShowBigint(rounded)).(func(any) any)(libliterals.ShowBigfloat(n)).(string))
        }()
      }(v.Value)
      case jsonmodel.ValueObject:
      return func (obj []any) any {
        return serialization.BracesListAdaptive(liblists.Map(KeyValueToExpr).(func(any) any)(libmaps.ToList(obj)).([]any))
      }(v.Value)
      case jsonmodel.ValueString_:
      return func (s string) any {
        return serialization.Cst(JsonString(s))
      }(v.Value)
    }
    return nil
  }(value).(ast.Expr)
}
