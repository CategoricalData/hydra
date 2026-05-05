// Note: this is an automatically generated file. Do not edit.

package serialization

import (
  "hydra.dev/hydra/ast"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  libliterals "hydra.dev/hydra/lib/literals"
  liblogic "hydra.dev/hydra/lib/logic"
  libmath "hydra.dev/hydra/lib/math"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/util"
)

var AngleBraces = ast.Brackets{Open: ast.Symbol("<"), Close_: ast.Symbol(">")}

func AngleBracesList (style ast.BlockStyle, els []any) ast.Expr {
  return liblogic.IfElse(liblists.Null(els)).(func(any) any)(Cst("<>")).(func(any) any)(Brackets(AngleBraces, style, CommaSep(style, els))).(ast.Expr)
}

func BracesListAdaptive (els []any) ast.Expr {
  return func () any {
    var inlineList any = CurlyBracesList(nil, InlineStyle, els)
    return liblogic.IfElse(libequality.Gt(ExpressionLength(inlineList.(ast.Expr))).(func(any) any)(70)).(func(any) any)(CurlyBracesList(nil, HalfBlockStyle, els)).(func(any) any)(inlineList)
  }().(ast.Expr)
}

func BracketList (style ast.BlockStyle, els []any) ast.Expr {
  return liblogic.IfElse(liblists.Null(els)).(func(any) any)(Cst("[]")).(func(any) any)(Brackets(SquareBrackets, style, CommaSep(style, els))).(ast.Expr)
}

func BracketListAdaptive (els []any) ast.Expr {
  return func () any {
    var inlineList any = BracketList(InlineStyle, els)
    return liblogic.IfElse(libequality.Gt(ExpressionLength(inlineList.(ast.Expr))).(func(any) any)(70)).(func(any) any)(BracketList(HalfBlockStyle, els)).(func(any) any)(inlineList)
  }().(ast.Expr)
}

func Brackets (br ast.Brackets, style ast.BlockStyle, e ast.Expr) ast.Expr {
  return ast.ExprBrackets{Value: ast.BracketExpr{Brackets: br, Enclosed: e, Style: style}}
}

func CommaSep (v1 ast.BlockStyle, v2 []any) ast.Expr {
  return SymbolSep(",", v1, v2)
}

func CurlyBlock (style ast.BlockStyle, e ast.Expr) ast.Expr {
  return CurlyBracesList(nil, style, []any{e})
}

var CurlyBraces = ast.Brackets{Open: ast.Symbol("{"), Close_: ast.Symbol("}")}

func CurlyBracesList (msymb any, style ast.BlockStyle, els []any) ast.Expr {
  return liblogic.IfElse(liblists.Null(els)).(func(any) any)(Cst("{}")).(func(any) any)(Brackets(CurlyBraces, style, SymbolSep(libmaybes.FromMaybe(",").(func(any) any)(msymb).(string), style, els))).(ast.Expr)
}

func Cst (s string) ast.Expr {
  return ast.ExprConst_{Value: Sym(s)}
}

func CustomIndent (idt string, s string) string {
  return libstrings.Cat(liblists.Intersperse("\n").(func(any) any)(liblists.Map(func (line string) any {
    return libstrings.Cat2(idt).(func(any) any)(line)
  }).(func(any) any)(libstrings.Lines(s)))).(string)
}

func CustomIndentBlock (idt string, els []any) ast.Expr {
  return func () any {
    var idtOp any = ast.Op{Symbol: Sym(""), Padding: ast.Padding{Left: ast.WsSpace{}, Right: ast.WsBreakAndIndent{Value: idt}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}
    return libmaybes.Maybe(Cst("")).(func(any) any)(func (head ast.Expr) any {
      return liblogic.IfElse(libequality.Equal(liblists.Length(els)).(func(any) any)(1)).(func(any) any)(head).(func(any) any)(Ifx(idtOp.(ast.Op), head, NewlineSep(liblists.Drop(1).(func(any) any)(els).([]any))))
    }).(func(any) any)(liblists.SafeHead(els))
  }().(ast.Expr)
}

func DotSep (v1 []any) ast.Expr {
  return Sep(ast.Op{Symbol: Sym("."), Padding: ast.Padding{Left: ast.WsNone{}, Right: ast.WsNone{}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}, v1)
}

func DoubleNewlineSep (v1 []any) ast.Expr {
  return Sep(ast.Op{Symbol: Sym(""), Padding: ast.Padding{Left: ast.WsBreak_{}, Right: ast.WsBreak_{}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}, v1)
}

var DoubleSpace = "  "

func ExpressionLength (e ast.Expr) int32 {
  return func () any {
    symbolLength := func (s ast.Symbol) any {
      return libstrings.Length(func (v any) any {
        return v
      }(s))
    }
    return func () any {
      wsLength := func (ws ast.Ws) any {
        return func (x any) any {
          switch v := x.(type) {
            case ast.WsNone:
            return func (_ struct{}) any {
              return 0
            }(v)
            case ast.WsSpace:
            return func (_ struct{}) any {
              return 1
            }(v)
            case ast.WsBreak_:
            return func (_ struct{}) any {
              return 10000
            }(v)
            case ast.WsBreakAndIndent:
            return func (s string) any {
              return 10000
            }(v.Value)
            case ast.WsDoubleBreak:
            return func (_ struct{}) any {
              return 10000
            }(v)
          }
          return nil
        }(ws)
      }
      return func () any {
        blockStyleLength := func (style ast.BlockStyle) any {
          return func () any {
            var mindentLen any = libmaybes.Maybe(0).(func(any) any)(libstrings.Length).(func(any) any)(func (v any) any {
              return v.(ast.BlockStyle).Indent
            }(style))
            return func () any {
              var nlBeforeLen any = liblogic.IfElse(func (v any) any {
                return v.(ast.BlockStyle).NewlineBeforeContent
              }(style)).(func(any) any)(1).(func(any) any)(0)
              return func () any {
                var nlAfterLen any = liblogic.IfElse(func (v any) any {
                  return v.(ast.BlockStyle).NewlineAfterContent
                }(style)).(func(any) any)(1).(func(any) any)(0)
                return libmath.Add(mindentLen).(func(any) any)(libmath.Add(nlBeforeLen).(func(any) any)(nlAfterLen))
              }()
            }()
          }()
        }
        return func () any {
          bracketsLength := func (brackets ast.Brackets) any {
            return libmath.Add(symbolLength(func (v any) any {
              return v.(ast.Brackets).Open
            }(brackets).(ast.Symbol))).(func(any) any)(symbolLength(func (v any) any {
              return v.(ast.Brackets).Close_
            }(brackets).(ast.Symbol)))
          }
          return func () any {
            bracketExprLength := func (be ast.BracketExpr) any {
              return libmath.Add(bracketsLength(func (v any) any {
                return v.(ast.BracketExpr).Brackets
              }(be).(ast.Brackets))).(func(any) any)(libmath.Add(ExpressionLength(func (v any) any {
                return v.(ast.BracketExpr).Enclosed
              }(be).(ast.Expr))).(func(any) any)(blockStyleLength(func (v any) any {
                return v.(ast.BracketExpr).Style
              }(be).(ast.BlockStyle))))
            }
            return func () any {
              indentedExpressionLength := func (ie ast.IndentedExpression) any {
                return func () any {
                  var baseLen any = ExpressionLength(func (v any) any {
                    return v.(ast.IndentedExpression).Expr
                  }(ie).(ast.Expr))
                  return func () any {
                    var indentLen any = func (x any) any {
                      switch v := x.(type) {
                        case ast.IndentStyleAllLines:
                        return func (s string) any {
                          return libstrings.Length(s)
                        }(v.Value)
                        case ast.IndentStyleSubsequentLines:
                        return func (s string) any {
                          return libstrings.Length(s)
                        }(v.Value)
                      }
                      return nil
                    }(func (v any) any {
                      return v.(ast.IndentedExpression).Style
                    }(ie))
                    return libmath.Add(baseLen).(func(any) any)(indentLen)
                  }()
                }()
              }
              return func () any {
                opLength := func (op ast.Op) any {
                  return func () any {
                    var symLen any = symbolLength(func (v any) any {
                      return v.(ast.Op).Symbol
                    }(op).(ast.Symbol))
                    return func () any {
                      var padding any = func (v any) any {
                        return v.(ast.Op).Padding
                      }(op)
                      return func () any {
                        var leftLen any = wsLength(padding.(ast.Padding).Left)
                        return func () any {
                          var rightLen any = wsLength(padding.(ast.Padding).Right)
                          return libmath.Add(symLen).(func(any) any)(libmath.Add(leftLen).(func(any) any)(rightLen))
                        }()
                      }()
                    }()
                  }()
                }
                return func () any {
                  opExprLength := func (oe ast.OpExpr) any {
                    return func () any {
                      var opLen any = opLength(func (v any) any {
                        return v.(ast.OpExpr).Op
                      }(oe).(ast.Op))
                      return func () any {
                        var leftLen any = ExpressionLength(func (v any) any {
                          return v.(ast.OpExpr).Lhs
                        }(oe).(ast.Expr))
                        return func () any {
                          var rightLen any = ExpressionLength(func (v any) any {
                            return v.(ast.OpExpr).Rhs
                          }(oe).(ast.Expr))
                          return libmath.Add(opLen).(func(any) any)(libmath.Add(leftLen).(func(any) any)(rightLen))
                        }()
                      }()
                    }()
                  }
                  return func () any {
                    seqExprLength := func (se ast.SeqExpr) any {
                      return func () any {
                        var sopLen any = opLength(func (v any) any {
                          return v.(ast.SeqExpr).Op
                        }(se).(ast.Op))
                        return func () any {
                          var elementLens any = liblists.Map(ExpressionLength).(func(any) any)(func (v any) any {
                            return v.(ast.SeqExpr).Elements
                          }(se))
                          return func () any {
                            var totalElLen any = liblists.Foldl(libmath.Add).(func(any) any)(0).(func(any) any)(elementLens)
                            return func () any {
                              var numSeps any = libmath.Sub(liblists.Length(func (v any) any {
                                return v.(ast.SeqExpr).Elements
                              }(se))).(func(any) any)(1)
                              return libmath.Add(totalElLen).(func(any) any)(libmath.Mul(sopLen).(func(any) any)(liblogic.IfElse(libequality.Gt(numSeps).(func(any) any)(0)).(func(any) any)(numSeps).(func(any) any)(0)))
                            }()
                          }()
                        }()
                      }()
                    }
                    return func (x any) any {
                      switch v := x.(type) {
                        case ast.ExprConst_:
                        return func (s ast.Symbol) any {
                          return symbolLength(s)
                        }(v.Value)
                        case ast.ExprIndent:
                        return func (ie ast.IndentedExpression) any {
                          return indentedExpressionLength(ie)
                        }(v.Value)
                        case ast.ExprOp:
                        return func (oe ast.OpExpr) any {
                          return opExprLength(oe)
                        }(v.Value)
                        case ast.ExprBrackets:
                        return func (be ast.BracketExpr) any {
                          return bracketExprLength(be)
                        }(v.Value)
                        case ast.ExprSeq:
                        return func (se ast.SeqExpr) any {
                          return seqExprLength(se)
                        }(v.Value)
                      }
                      return nil
                    }(e)
                  }()
                }()
              }()
            }()
          }()
        }()
      }()
    }()
  }().(int32)
}

var FullBlockStyle = ast.BlockStyle{Indent: func () any {
  _v := DoubleSpace
  return &_v
}(), NewlineBeforeContent: true, NewlineAfterContent: true}

var HalfBlockStyle = ast.BlockStyle{Indent: func () any {
  _v := DoubleSpace
  return &_v
}(), NewlineBeforeContent: true, NewlineAfterContent: false}

func Ifx (op ast.Op, lhs ast.Expr, rhs ast.Expr) ast.Expr {
  return ast.ExprOp{Value: ast.OpExpr{Op: op, Lhs: lhs, Rhs: rhs}}
}

func Indent (v1 string) string {
  return CustomIndent(DoubleSpace, v1)
}

func IndentBlock (v1 []any) ast.Expr {
  return CustomIndentBlock(DoubleSpace, v1)
}

func IndentSubsequentLines (idt string, e ast.Expr) ast.Expr {
  return ast.ExprIndent{Value: ast.IndentedExpression{Style: ast.IndentStyleSubsequentLines{Value: idt}, Expr: e}}
}

func InfixWs (op string, l ast.Expr, r ast.Expr) ast.Expr {
  return SpaceSep([]any{l, Cst(op), r})
}

func InfixWsList (op string, opers []any) ast.Expr {
  return func () any {
    var opExpr any = Cst(op)
    return func () any {
      foldFun := func (e []any) any {
        return func (r ast.Expr) any {
          return liblogic.IfElse(liblists.Null(e)).(func(any) any)([]any{r}).(func(any) any)(liblists.Cons(r).(func(any) any)(liblists.Cons(opExpr).(func(any) any)(e)))
        }
      }
      return SpaceSep(liblists.Foldl(foldFun).(func(any) any)([]any{}).(func(any) any)(liblists.Reverse(opers)).([]any))
    }()
  }().(ast.Expr)
}

var InlineStyle = ast.BlockStyle{Indent: nil, NewlineBeforeContent: false, NewlineAfterContent: false}

func NewlineSep (v1 []any) ast.Expr {
  return Sep(ast.Op{Symbol: Sym(""), Padding: ast.Padding{Left: ast.WsNone{}, Right: ast.WsBreak_{}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}, v1)
}

var NoPadding = ast.Padding{Left: ast.WsNone{}, Right: ast.WsNone{}}

func NoSep (v1 []any) ast.Expr {
  return Sep(ast.Op{Symbol: Sym(""), Padding: ast.Padding{Left: ast.WsNone{}, Right: ast.WsNone{}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}, v1)
}

func Num (i int32) ast.Expr {
  return Cst(libliterals.ShowInt32(i).(string))
}

func Op (s string, p int32, assoc ast.Associativity) ast.Op {
  return ast.Op{Symbol: Sym(s), Padding: ast.Padding{Left: ast.WsSpace{}, Right: ast.WsSpace{}}, Precedence: ast.Precedence(p), Associativity: assoc}
}

func OrOp (newlines bool) ast.Op {
  return ast.Op{Symbol: Sym("|"), Padding: ast.Padding{Left: ast.WsSpace{}, Right: liblogic.IfElse(newlines).(func(any) any)(ast.WsBreak_{}).(func(any) any)(ast.WsSpace{}).(ast.Ws)}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}
}

func OrSep (style ast.BlockStyle, l []any) ast.Expr {
  return func () any {
    var newlines any = func (v any) any {
      return v.(ast.BlockStyle).NewlineBeforeContent
    }(style)
    return libmaybes.Maybe(Cst("")).(func(any) any)(func (h ast.Expr) any {
      return liblists.Foldl(func (acc ast.Expr) any {
        return func (el ast.Expr) any {
          return Ifx(OrOp(newlines.(bool)), acc, el)
        }
      }).(func(any) any)(h).(func(any) any)(liblists.Drop(1).(func(any) any)(l))
    }).(func(any) any)(liblists.SafeHead(l))
  }().(ast.Expr)
}

func ParenList (newlines bool, els []any) ast.Expr {
  return func () any {
    var style any = liblogic.IfElse(liblogic.And(newlines).(func(any) any)(libequality.Gt(liblists.Length(els)).(func(any) any)(1))).(func(any) any)(HalfBlockStyle).(func(any) any)(InlineStyle)
    return liblogic.IfElse(liblists.Null(els)).(func(any) any)(Cst("()")).(func(any) any)(Brackets(Parentheses, style.(ast.BlockStyle), CommaSep(style.(ast.BlockStyle), els)))
  }().(ast.Expr)
}

func Parens (v1 ast.Expr) ast.Expr {
  return Brackets(Parentheses, InlineStyle, v1)
}

var Parentheses = ast.Brackets{Open: ast.Symbol("("), Close_: ast.Symbol(")")}

func Parenthesize (exp ast.Expr) ast.Expr {
  return func () any {
    assocLeft := func (a ast.Associativity) any {
      return func (x any) any {
        switch v := x.(type) {
          case ast.AssociativityRight:
          return func (_ struct{}) any {
            return false
          }(v)
          default:
          return true
        }
        return nil
      }(a)
    }
    return func () any {
      assocRight := func (a ast.Associativity) any {
        return func (x any) any {
          switch v := x.(type) {
            case ast.AssociativityLeft:
            return func (_ struct{}) any {
              return false
            }(v)
            default:
            return true
          }
          return nil
        }(a)
      }
      return func (x any) any {
        switch v := x.(type) {
          case ast.ExprBrackets:
          return func (bracketExpr ast.BracketExpr) any {
            return ast.ExprBrackets{Value: ast.BracketExpr{Brackets: func (v any) any {
              return v.(ast.BracketExpr).Brackets
            }(bracketExpr).(ast.Brackets), Enclosed: Parenthesize(func (v any) any {
              return v.(ast.BracketExpr).Enclosed
            }(bracketExpr).(ast.Expr)), Style: func (v any) any {
              return v.(ast.BracketExpr).Style
            }(bracketExpr).(ast.BlockStyle)}}
          }(v.Value)
          case ast.ExprConst_:
          return func (ignored ast.Symbol) any {
            return exp
          }(v.Value)
          case ast.ExprIndent:
          return func (indentExpr ast.IndentedExpression) any {
            return ast.ExprIndent{Value: ast.IndentedExpression{Style: func (v any) any {
              return v.(ast.IndentedExpression).Style
            }(indentExpr).(ast.IndentStyle), Expr: Parenthesize(func (v any) any {
              return v.(ast.IndentedExpression).Expr
            }(indentExpr).(ast.Expr))}}
          }(v.Value)
          case ast.ExprSeq:
          return func (seqExpr ast.SeqExpr) any {
            return ast.ExprSeq{Value: ast.SeqExpr{Op: func (v any) any {
              return v.(ast.SeqExpr).Op
            }(seqExpr).(ast.Op), Elements: liblists.Map(Parenthesize).(func(any) any)(func (v any) any {
              return v.(ast.SeqExpr).Elements
            }(seqExpr)).([]any)}}
          }(v.Value)
          case ast.ExprOp:
          return func (opExpr ast.OpExpr) any {
            return func () any {
              var op any = func (v any) any {
                return v.(ast.OpExpr).Op
              }(opExpr)
              return func () any {
                var prec any = func (v any) any {
                  return v
                }(op.(ast.Op).Precedence)
                return func () any {
                  var assoc any = op.(ast.Op).Associativity
                  return func () any {
                    var lhs any = func (v any) any {
                      return v.(ast.OpExpr).Lhs
                    }(opExpr)
                    return func () any {
                      var rhs any = func (v any) any {
                        return v.(ast.OpExpr).Rhs
                      }(opExpr)
                      return func () any {
                        var lhs_ any = Parenthesize(lhs.(ast.Expr))
                        return func () any {
                          var rhs_ any = Parenthesize(rhs.(ast.Expr))
                          return func () any {
                            var lhs2 any = func (x any) any {
                              switch v := x.(type) {
                                case ast.ExprOp:
                                return func (lopExpr ast.OpExpr) any {
                                  return func () any {
                                    var lop any = func (v any) any {
                                      return v.(ast.OpExpr).Op
                                    }(lopExpr)
                                    return func () any {
                                      var lprec any = func (v any) any {
                                        return v
                                      }(lop.(ast.Op).Precedence)
                                      return func () any {
                                        var lassoc any = lop.(ast.Op).Associativity
                                        return func () any {
                                          var comparison any = libequality.Compare(prec).(func(any) any)(lprec)
                                          return func (x any) any {
                                            switch v := x.(type) {
                                              case util.ComparisonLessThan:
                                              return func (_ struct{}) any {
                                                return lhs_
                                              }(v)
                                              case util.ComparisonGreaterThan:
                                              return func (_ struct{}) any {
                                                return Parens(lhs_.(ast.Expr))
                                              }(v)
                                              case util.ComparisonEqualTo:
                                              return func (_ struct{}) any {
                                                return liblogic.IfElse(liblogic.And(assocLeft(assoc.(ast.Associativity))).(func(any) any)(assocLeft(lassoc.(ast.Associativity)))).(func(any) any)(lhs_).(func(any) any)(Parens(lhs_.(ast.Expr)))
                                              }(v)
                                            }
                                            return nil
                                          }(comparison)
                                        }()
                                      }()
                                    }()
                                  }()
                                }(v.Value)
                                default:
                                return lhs_
                              }
                              return nil
                            }(lhs_)
                            return func () any {
                              var rhs2 any = func (x any) any {
                                switch v := x.(type) {
                                  case ast.ExprOp:
                                  return func (ropExpr ast.OpExpr) any {
                                    return func () any {
                                      var rop any = func (v any) any {
                                        return v.(ast.OpExpr).Op
                                      }(ropExpr)
                                      return func () any {
                                        var rprec any = func (v any) any {
                                          return v
                                        }(rop.(ast.Op).Precedence)
                                        return func () any {
                                          var rassoc any = rop.(ast.Op).Associativity
                                          return func () any {
                                            var comparison any = libequality.Compare(prec).(func(any) any)(rprec)
                                            return func (x any) any {
                                              switch v := x.(type) {
                                                case util.ComparisonLessThan:
                                                return func (_ struct{}) any {
                                                  return rhs_
                                                }(v)
                                                case util.ComparisonGreaterThan:
                                                return func (_ struct{}) any {
                                                  return Parens(rhs_.(ast.Expr))
                                                }(v)
                                                case util.ComparisonEqualTo:
                                                return func (_ struct{}) any {
                                                  return liblogic.IfElse(liblogic.And(assocRight(assoc.(ast.Associativity))).(func(any) any)(assocRight(rassoc.(ast.Associativity)))).(func(any) any)(rhs_).(func(any) any)(Parens(rhs_.(ast.Expr)))
                                                }(v)
                                              }
                                              return nil
                                            }(comparison)
                                          }()
                                        }()
                                      }()
                                    }()
                                  }(v.Value)
                                  default:
                                  return rhs_
                                }
                                return nil
                              }(rhs_)
                              return ast.ExprOp{Value: ast.OpExpr{Op: op.(ast.Op), Lhs: lhs2.(ast.Expr), Rhs: rhs2.(ast.Expr)}}
                            }()
                          }()
                        }()
                      }()
                    }()
                  }()
                }()
              }()
            }()
          }(v.Value)
        }
        return nil
      }(exp)
    }()
  }().(ast.Expr)
}

func Prefix (p string, expr ast.Expr) ast.Expr {
  return func () any {
    var preOp any = ast.Op{Symbol: Sym(p), Padding: ast.Padding{Left: ast.WsNone{}, Right: ast.WsNone{}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}
    return Ifx(preOp.(ast.Op), Cst(""), expr)
  }().(ast.Expr)
}

func PrintExpr (e ast.Expr) string {
  return func () any {
    pad := func (ws ast.Ws) any {
      return func (x any) any {
        switch v := x.(type) {
          case ast.WsNone:
          return func (_ struct{}) any {
            return ""
          }(v)
          case ast.WsSpace:
          return func (_ struct{}) any {
            return " "
          }(v)
          case ast.WsBreak_:
          return func (_ struct{}) any {
            return "\n"
          }(v)
          case ast.WsBreakAndIndent:
          return func (ignored string) any {
            return "\n"
          }(v.Value)
          case ast.WsDoubleBreak:
          return func (_ struct{}) any {
            return "\n\n"
          }(v)
        }
        return nil
      }(ws)
    }
    return func () any {
      idt := func (ws ast.Ws) any {
        return func (s string) any {
          return func (x any) any {
            switch v := x.(type) {
              case ast.WsBreakAndIndent:
              return func (indentStr string) any {
                return CustomIndent(indentStr, s)
              }(v.Value)
              default:
              return s
            }
            return nil
          }(ws)
        }
      }
      return func (x any) any {
        switch v := x.(type) {
          case ast.ExprConst_:
          return func (symbol ast.Symbol) any {
            return func (v any) any {
              return v
            }(symbol)
          }(v.Value)
          case ast.ExprIndent:
          return func (indentExpr ast.IndentedExpression) any {
            return func () any {
              var style any = func (v any) any {
                return v.(ast.IndentedExpression).Style
              }(indentExpr)
              return func () any {
                var expr any = func (v any) any {
                  return v.(ast.IndentedExpression).Expr
                }(indentExpr)
                return func () any {
                  var lns any = libstrings.Lines(PrintExpr(expr.(ast.Expr)))
                  return func () any {
                    var ilns any = func (x any) any {
                      switch v := x.(type) {
                        case ast.IndentStyleAllLines:
                        return func (idt2 string) any {
                          return liblists.Map(func (line string) any {
                            return libstrings.Cat2(idt2).(func(any) any)(line)
                          }).(func(any) any)(lns)
                        }(v.Value)
                        case ast.IndentStyleSubsequentLines:
                        return func (idt2 string) any {
                          return liblogic.IfElse(libequality.Equal(liblists.Length(lns)).(func(any) any)(1)).(func(any) any)(lns).(func(any) any)(liblists.Cons(liblists.Head(lns)).(func(any) any)(liblists.Map(func (line string) any {
                            return libstrings.Cat2(idt2).(func(any) any)(line)
                          }).(func(any) any)(liblists.Tail(lns))))
                        }(v.Value)
                      }
                      return nil
                    }(style)
                    return libstrings.Intercalate("\n").(func(any) any)(ilns)
                  }()
                }()
              }()
            }()
          }(v.Value)
          case ast.ExprSeq:
          return func (seqExpr ast.SeqExpr) any {
            return func () any {
              var sop any = func (v any) any {
                return v.(ast.SeqExpr).Op
              }(seqExpr)
              return func () any {
                var ssym any = func (v any) any {
                  return v
                }(sop.(ast.Op).Symbol)
                return func () any {
                  var spadding any = sop.(ast.Op).Padding
                  return func () any {
                    var spadl any = spadding.(ast.Padding).Left
                    return func () any {
                      var spadr any = spadding.(ast.Padding).Right
                      return func () any {
                        var selements any = func (v any) any {
                          return v.(ast.SeqExpr).Elements
                        }(seqExpr)
                        return func () any {
                          var separator any = libstrings.Cat2(libstrings.Cat2(pad(spadl.(ast.Ws))).(func(any) any)(ssym)).(func(any) any)(pad(spadr.(ast.Ws)))
                          return func () any {
                            var printedElements any = liblists.Map(func (el ast.Expr) any {
                              return idt(spadr.(ast.Ws)).(func(any) any)(PrintExpr(el))
                            }).(func(any) any)(selements)
                            return libstrings.Intercalate(separator).(func(any) any)(printedElements)
                          }()
                        }()
                      }()
                    }()
                  }()
                }()
              }()
            }()
          }(v.Value)
          case ast.ExprOp:
          return func (opExpr ast.OpExpr) any {
            return func () any {
              var op any = func (v any) any {
                return v.(ast.OpExpr).Op
              }(opExpr)
              return func () any {
                var sym any = func (v any) any {
                  return v
                }(op.(ast.Op).Symbol)
                return func () any {
                  var padding any = op.(ast.Op).Padding
                  return func () any {
                    var padl any = padding.(ast.Padding).Left
                    return func () any {
                      var padr any = padding.(ast.Padding).Right
                      return func () any {
                        var l any = func (v any) any {
                          return v.(ast.OpExpr).Lhs
                        }(opExpr)
                        return func () any {
                          var r any = func (v any) any {
                            return v.(ast.OpExpr).Rhs
                          }(opExpr)
                          return func () any {
                            var lhs any = idt(padl.(ast.Ws)).(func(any) any)(PrintExpr(l.(ast.Expr)))
                            return func () any {
                              var rhs any = idt(padr.(ast.Ws)).(func(any) any)(PrintExpr(r.(ast.Expr)))
                              return libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(lhs).(func(any) any)(pad(padl.(ast.Ws)))).(func(any) any)(sym)).(func(any) any)(pad(padr.(ast.Ws)))).(func(any) any)(rhs)
                            }()
                          }()
                        }()
                      }()
                    }()
                  }()
                }()
              }()
            }()
          }(v.Value)
          case ast.ExprBrackets:
          return func (bracketExpr ast.BracketExpr) any {
            return func () any {
              var brs any = func (v any) any {
                return v.(ast.BracketExpr).Brackets
              }(bracketExpr)
              return func () any {
                var l any = func (v any) any {
                  return v
                }(brs.(ast.Brackets).Open)
                return func () any {
                  var r any = func (v any) any {
                    return v
                  }(brs.(ast.Brackets).Close_)
                  return func () any {
                    var e any = func (v any) any {
                      return v.(ast.BracketExpr).Enclosed
                    }(bracketExpr)
                    return func () any {
                      var style any = func (v any) any {
                        return v.(ast.BracketExpr).Style
                      }(bracketExpr)
                      return func () any {
                        var body any = PrintExpr(e.(ast.Expr))
                        return func () any {
                          var doIndent any = style.(ast.BlockStyle).Indent
                          return func () any {
                            var nlBefore any = style.(ast.BlockStyle).NewlineBeforeContent
                            return func () any {
                              var nlAfter any = style.(ast.BlockStyle).NewlineAfterContent
                              return func () any {
                                var ibody any = libmaybes.Maybe(body).(func(any) any)(func (idt2 string) any {
                                  return CustomIndent(idt2, body.(string))
                                }).(func(any) any)(doIndent)
                                return func () any {
                                  var pre any = liblogic.IfElse(nlBefore).(func(any) any)("\n").(func(any) any)("")
                                  return func () any {
                                    var suf any = liblogic.IfElse(nlAfter).(func(any) any)("\n").(func(any) any)("")
                                    return libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(l).(func(any) any)(pre)).(func(any) any)(ibody)).(func(any) any)(suf)).(func(any) any)(r)
                                  }()
                                }()
                              }()
                            }()
                          }()
                        }()
                      }()
                    }()
                  }()
                }()
              }()
            }()
          }(v.Value)
        }
        return nil
      }(e)
    }()
  }().(string)
}

func SemicolonSep (v1 []any) ast.Expr {
  return SymbolSep(";", InlineStyle, v1)
}

func Sep (op ast.Op, els []any) ast.Expr {
  return libmaybes.Maybe(Cst("")).(func(any) any)(func (h ast.Expr) any {
    return liblists.Foldl(func (acc ast.Expr) any {
      return func (el ast.Expr) any {
        return Ifx(op, acc, el)
      }
    }).(func(any) any)(h).(func(any) any)(liblists.Drop(1).(func(any) any)(els))
  }).(func(any) any)(liblists.SafeHead(els)).(ast.Expr)
}

func SpaceSep (v1 []any) ast.Expr {
  return Sep(ast.Op{Symbol: Sym(""), Padding: ast.Padding{Left: ast.WsSpace{}, Right: ast.WsNone{}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}, v1)
}

func StructuralSep (op ast.Op, els []any) ast.Expr {
  return liblogic.IfElse(liblists.Null(els)).(func(any) any)(Cst("")).(func(any) any)(liblogic.IfElse(libequality.Equal(liblists.Length(els)).(func(any) any)(1)).(func(any) any)(liblists.Head(els)).(func(any) any)(ast.ExprSeq{Value: ast.SeqExpr{Op: op, Elements: els}})).(ast.Expr)
}

func StructuralSpaceSep (v1 []any) ast.Expr {
  return StructuralSep(ast.Op{Symbol: Sym(""), Padding: ast.Padding{Left: ast.WsSpace{}, Right: ast.WsNone{}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}, v1)
}

var SquareBrackets = ast.Brackets{Open: ast.Symbol("["), Close_: ast.Symbol("]")}

func Suffix (s string, expr ast.Expr) ast.Expr {
  return func () any {
    var sufOp any = ast.Op{Symbol: Sym(s), Padding: ast.Padding{Left: ast.WsNone{}, Right: ast.WsNone{}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}
    return Ifx(sufOp.(ast.Op), expr, Cst(""))
  }().(ast.Expr)
}

func Sym (s string) ast.Symbol {
  return ast.Symbol(s)
}

func SymbolSep (symb string, style ast.BlockStyle, l []any) ast.Expr {
  return func () any {
    var breakCount any = liblists.Length(liblists.Filter(func (x_ bool) any {
      return x_
    }).(func(any) any)([]any{func (v any) any {
      return v.(ast.BlockStyle).NewlineBeforeContent
    }(style), func (v any) any {
      return v.(ast.BlockStyle).NewlineAfterContent
    }(style)}))
    return func () any {
      var break_ any = liblogic.IfElse(libequality.Equal(breakCount).(func(any) any)(0)).(func(any) any)(ast.WsSpace{}).(func(any) any)(liblogic.IfElse(libequality.Equal(breakCount).(func(any) any)(1)).(func(any) any)(ast.WsBreak_{}).(func(any) any)(ast.WsDoubleBreak{}))
      return func () any {
        var commaOp any = ast.Op{Symbol: Sym(symb), Padding: ast.Padding{Left: ast.WsNone{}, Right: break_.(ast.Ws)}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityNone{}}
        return libmaybes.Maybe(Cst("")).(func(any) any)(func (h ast.Expr) any {
          return liblists.Foldl(func (acc ast.Expr) any {
            return func (el ast.Expr) any {
              return Ifx(commaOp.(ast.Op), acc, el)
            }
          }).(func(any) any)(h).(func(any) any)(liblists.Drop(1).(func(any) any)(l))
        }).(func(any) any)(liblists.SafeHead(l))
      }()
    }()
  }().(ast.Expr)
}

func TabIndent (e ast.Expr) ast.Expr {
  return ast.ExprIndent{Value: ast.IndentedExpression{Style: ast.IndentStyleAllLines{Value: "    "}, Expr: e}}
}

func TabIndentDoubleSpace (exprs []any) ast.Expr {
  return TabIndent(DoubleNewlineSep(exprs))
}

func TabIndentSingleSpace (exprs []any) ast.Expr {
  return TabIndent(NewlineSep(exprs))
}

func UnsupportedType (label string) ast.Expr {
  return Cst(libstrings.Cat2(libstrings.Cat2("[").(func(any) any)(label)).(func(any) any)("]").(string))
}

func UnsupportedVariant (label string, obj string) ast.Expr {
  return Cst(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("[unsupported ").(func(any) any)(label)).(func(any) any)(": ")).(func(any) any)(libliterals.ShowString(obj))).(func(any) any)("]").(string))
}

func WithComma (e ast.Expr) ast.Expr {
  return NoSep([]any{e, Cst(",")})
}

func WithSemi (e ast.Expr) ast.Expr {
  return NoSep([]any{e, Cst(";")})
}
