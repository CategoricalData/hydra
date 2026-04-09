// Note: this is an automatically generated file. Do not edit.

package decodeast

import (
  "hydra.dev/hydra/ast"
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
)

func Associativity (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            var fname any = field.(core.Field).Name
            var fterm any = field.(core.Field).Term
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("none"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return ast.AssociativityNone{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("left"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return ast.AssociativityLeft{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("right"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return ast.AssociativityRight{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("both"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return ast.AssociativityBoth{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}})
            return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"no such field ", fname, " in union"}).(string))}).(func(any) any)(func (f func(core.Term) any) any {
              return f(fterm.(core.Term))
            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(variantMap))
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected union")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func BlockStyle (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("indent", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeMaybe(func (_p graph.Graph) func(core.Term) any {
                    return func (cx2 graph.Graph) func(core.Term) any {
                      return func (raw2 core.Term) any {
                        return libeithers.Either(func (err string) any {
                          return [2]any{"left", error.DecodingError(err)}
                        }).(func(any) any)(func (stripped2 core.Term) any {
                          return func (x any) any {
                            switch v := x.(type) {
                              case core.TermLiteral:
                              return func (v core.Literal) any {
                                return func (x any) any {
                                  switch v := x.(type) {
                                    case core.LiteralString_:
                                    return func (s string) any {
                                      return [2]any{"right", s}
                                    }(v.Value)
                                    default:
                                    return [2]any{"left", error.DecodingError("expected string literal")}
                                  }
                                  return nil
                                }(v)
                              }(v.Value)
                              default:
                              return [2]any{"left", error.DecodingError("expected literal")}
                            }
                            return nil
                          }(stripped2)
                        }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2))
                      }
                    }(_p).(func(core.Term) any)
                  }, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_indent any) any {
              return libeithers.Bind(extracthelpers.RequireField("newlineBeforeContent", func (_p graph.Graph) func(any) any {
                return func (cx2 graph.Graph) func(any) any {
                  return func (raw2 any) any {
                    return libeithers.Either(func (err string) any {
                      return [2]any{"left", error.DecodingError(err)}
                    }).(func(any) any)(func (stripped2 core.Term) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TermLiteral:
                          return func (v core.Literal) any {
                            return func (x any) any {
                              switch v := x.(type) {
                                case core.LiteralBoolean:
                                return func (b bool) any {
                                  return [2]any{"right", b}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected boolean literal")}
                              }
                              return nil
                            }(v)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected literal")}
                        }
                        return nil
                      }(stripped2)
                    }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2.(core.Term)))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_newlineBeforeContent bool) any {
                return libeithers.Bind(extracthelpers.RequireField("newlineAfterContent", func (_p graph.Graph) func(any) any {
                  return func (cx2 graph.Graph) func(any) any {
                    return func (raw2 any) any {
                      return libeithers.Either(func (err string) any {
                        return [2]any{"left", error.DecodingError(err)}
                      }).(func(any) any)(func (stripped2 core.Term) any {
                        return func (x any) any {
                          switch v := x.(type) {
                            case core.TermLiteral:
                            return func (v core.Literal) any {
                              return func (x any) any {
                                switch v := x.(type) {
                                  case core.LiteralBoolean:
                                  return func (b bool) any {
                                    return [2]any{"right", b}
                                  }(v.Value)
                                  default:
                                  return [2]any{"left", error.DecodingError("expected boolean literal")}
                                }
                                return nil
                              }(v)
                            }(v.Value)
                            default:
                            return [2]any{"left", error.DecodingError("expected literal")}
                          }
                          return nil
                        }(stripped2)
                      }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2.(core.Term)))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_newlineAfterContent bool) any {
                  return [2]any{"right", ast.BlockStyle{Indent: field_indent, NewlineBeforeContent: field_newlineBeforeContent, NewlineAfterContent: field_newlineAfterContent}}
                })
              })
            })
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected record")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func BracketExpr (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("brackets", Brackets, fieldMap.([]any), cx)).(func(any) any)(func (field_brackets ast.Brackets) any {
              return libeithers.Bind(extracthelpers.RequireField("enclosed", Expr, fieldMap.([]any), cx)).(func(any) any)(func (field_enclosed ast.Expr) any {
                return libeithers.Bind(extracthelpers.RequireField("style", BlockStyle, fieldMap.([]any), cx)).(func(any) any)(func (field_style ast.BlockStyle) any {
                  return [2]any{"right", ast.BracketExpr{Brackets: field_brackets, Enclosed: field_enclosed, Style: field_style}}
                })
              })
            })
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected record")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func Brackets (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("open", Symbol, fieldMap.([]any), cx)).(func(any) any)(func (field_open ast.Symbol) any {
              return libeithers.Bind(extracthelpers.RequireField("close", Symbol, fieldMap.([]any), cx)).(func(any) any)(func (field_close ast.Symbol) any {
                return [2]any{"right", ast.Brackets{Open: field_open, Close_: field_close}}
              })
            })
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected record")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func Expr (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            var fname any = field.(core.Field).Name
            var fterm any = field.(core.Field).Term
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("const"), func (input core.Term) any {
              return libeithers.Map(func (t ast.Symbol) any {
                return ast.ExprConst_{Value: t}
              }).(func(any) any)(Symbol(cx, input))
            }}, [2]any{core.Name("indent"), func (input core.Term) any {
              return libeithers.Map(func (t ast.IndentedExpression) any {
                return ast.ExprIndent{Value: t}
              }).(func(any) any)(IndentedExpression(cx, input))
            }}, [2]any{core.Name("op"), func (input core.Term) any {
              return libeithers.Map(func (t ast.OpExpr) any {
                return ast.ExprOp{Value: t}
              }).(func(any) any)(OpExpr(cx, input))
            }}, [2]any{core.Name("brackets"), func (input core.Term) any {
              return libeithers.Map(func (t ast.BracketExpr) any {
                return ast.ExprBrackets{Value: t}
              }).(func(any) any)(BracketExpr(cx, input))
            }}, [2]any{core.Name("seq"), func (input core.Term) any {
              return libeithers.Map(func (t ast.SeqExpr) any {
                return ast.ExprSeq{Value: t}
              }).(func(any) any)(SeqExpr(cx, input))
            }}})
            return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"no such field ", fname, " in union"}).(string))}).(func(any) any)(func (f func(core.Term) any) any {
              return f(fterm.(core.Term))
            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(variantMap))
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected union")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func IndentedExpression (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("style", IndentStyle, fieldMap.([]any), cx)).(func(any) any)(func (field_style ast.IndentStyle) any {
              return libeithers.Bind(extracthelpers.RequireField("expr", Expr, fieldMap.([]any), cx)).(func(any) any)(func (field_expr ast.Expr) any {
                return [2]any{"right", ast.IndentedExpression{Style: field_style, Expr: field_expr}}
              })
            })
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected record")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func IndentStyle (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            var fname any = field.(core.Field).Name
            var fterm any = field.(core.Field).Term
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("allLines"), func (input core.Term) any {
              return libeithers.Map(func (t string) any {
                return ast.IndentStyleAllLines{Value: t}
              }).(func(any) any)(libeithers.Either(func (err string) any {
                return [2]any{"left", error.DecodingError(err)}
              }).(func(any) any)(func (stripped2 core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermLiteral:
                    return func (v core.Literal) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.LiteralString_:
                          return func (s string) any {
                            return [2]any{"right", s}
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected string literal")}
                        }
                        return nil
                      }(v)
                    }(v.Value)
                    default:
                    return [2]any{"left", error.DecodingError("expected literal")}
                  }
                  return nil
                }(stripped2)
              }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, input)))
            }}, [2]any{core.Name("subsequentLines"), func (input core.Term) any {
              return libeithers.Map(func (t string) any {
                return ast.IndentStyleSubsequentLines{Value: t}
              }).(func(any) any)(libeithers.Either(func (err string) any {
                return [2]any{"left", error.DecodingError(err)}
              }).(func(any) any)(func (stripped2 core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermLiteral:
                    return func (v core.Literal) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.LiteralString_:
                          return func (s string) any {
                            return [2]any{"right", s}
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected string literal")}
                        }
                        return nil
                      }(v)
                    }(v.Value)
                    default:
                    return [2]any{"left", error.DecodingError("expected literal")}
                  }
                  return nil
                }(stripped2)
              }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, input)))
            }}})
            return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"no such field ", fname, " in union"}).(string))}).(func(any) any)(func (f func(core.Term) any) any {
              return f(fterm.(core.Term))
            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(variantMap))
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected union")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func Op (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("symbol", Symbol, fieldMap.([]any), cx)).(func(any) any)(func (field_symbol ast.Symbol) any {
              return libeithers.Bind(extracthelpers.RequireField("padding", Padding, fieldMap.([]any), cx)).(func(any) any)(func (field_padding ast.Padding) any {
                return libeithers.Bind(extracthelpers.RequireField("precedence", Precedence, fieldMap.([]any), cx)).(func(any) any)(func (field_precedence ast.Precedence) any {
                  return libeithers.Bind(extracthelpers.RequireField("associativity", Associativity, fieldMap.([]any), cx)).(func(any) any)(func (field_associativity ast.Associativity) any {
                    return [2]any{"right", ast.Op{Symbol: field_symbol, Padding: field_padding, Precedence: field_precedence, Associativity: field_associativity}}
                  })
                })
              })
            })
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected record")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func OpExpr (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("op", Op, fieldMap.([]any), cx)).(func(any) any)(func (field_op ast.Op) any {
              return libeithers.Bind(extracthelpers.RequireField("lhs", Expr, fieldMap.([]any), cx)).(func(any) any)(func (field_lhs ast.Expr) any {
                return libeithers.Bind(extracthelpers.RequireField("rhs", Expr, fieldMap.([]any), cx)).(func(any) any)(func (field_rhs ast.Expr) any {
                  return [2]any{"right", ast.OpExpr{Op: field_op, Lhs: field_lhs, Rhs: field_rhs}}
                })
              })
            })
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected record")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func Padding (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("left", Ws, fieldMap.([]any), cx)).(func(any) any)(func (field_left ast.Ws) any {
              return libeithers.Bind(extracthelpers.RequireField("right", Ws, fieldMap.([]any), cx)).(func(any) any)(func (field_right ast.Ws) any {
                return [2]any{"right", ast.Padding{Left: field_left, Right: field_right}}
              })
            })
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected record")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func Precedence (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b int32) any {
            return ast.Precedence(b)
          }).(func(any) any)(libeithers.Either(func (err string) any {
            return [2]any{"left", error.DecodingError(err)}
          }).(func(any) any)(func (stripped2 core.Term) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.TermLiteral:
                return func (v core.Literal) any {
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.LiteralInteger:
                      return func (v1 core.IntegerValue) any {
                        return func (x any) any {
                          switch v := x.(type) {
                            case core.IntegerValueInt32_:
                            return func (i int32) any {
                              return [2]any{"right", i}
                            }(v.Value)
                            default:
                            return [2]any{"left", error.DecodingError("expected int32 value")}
                          }
                          return nil
                        }(v1)
                      }(v.Value)
                      default:
                      return [2]any{"left", error.DecodingError("expected int32 literal")}
                    }
                    return nil
                  }(v)
                }(v.Value)
                default:
                return [2]any{"left", error.DecodingError("expected literal")}
              }
              return nil
            }(stripped2)
          }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, func (v any) any {
            return v.(core.WrappedTerm).Body
          }(wrappedTerm).(core.Term))))
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected wrapped type")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func SeqExpr (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("op", Op, fieldMap.([]any), cx)).(func(any) any)(func (field_op ast.Op) any {
              return libeithers.Bind(extracthelpers.RequireField("elements", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(Expr, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_elements []any) any {
                return [2]any{"right", ast.SeqExpr{Op: field_op, Elements: field_elements}}
              })
            })
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected record")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func Symbol (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b string) any {
            return ast.Symbol(b)
          }).(func(any) any)(libeithers.Either(func (err string) any {
            return [2]any{"left", error.DecodingError(err)}
          }).(func(any) any)(func (stripped2 core.Term) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.TermLiteral:
                return func (v core.Literal) any {
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.LiteralString_:
                      return func (s string) any {
                        return [2]any{"right", s}
                      }(v.Value)
                      default:
                      return [2]any{"left", error.DecodingError("expected string literal")}
                    }
                    return nil
                  }(v)
                }(v.Value)
                default:
                return [2]any{"left", error.DecodingError("expected literal")}
              }
              return nil
            }(stripped2)
          }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, func (v any) any {
            return v.(core.WrappedTerm).Body
          }(wrappedTerm).(core.Term))))
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected wrapped type")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func Ws (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            var fname any = field.(core.Field).Name
            var fterm any = field.(core.Field).Term
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("none"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return ast.WsNone{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("space"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return ast.WsSpace{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("break"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return ast.WsBreak_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("breakAndIndent"), func (input core.Term) any {
              return libeithers.Map(func (t string) any {
                return ast.WsBreakAndIndent{Value: t}
              }).(func(any) any)(libeithers.Either(func (err string) any {
                return [2]any{"left", error.DecodingError(err)}
              }).(func(any) any)(func (stripped2 core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermLiteral:
                    return func (v core.Literal) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.LiteralString_:
                          return func (s string) any {
                            return [2]any{"right", s}
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected string literal")}
                        }
                        return nil
                      }(v)
                    }(v.Value)
                    default:
                    return [2]any{"left", error.DecodingError("expected literal")}
                  }
                  return nil
                }(stripped2)
              }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, input)))
            }}, [2]any{core.Name("doubleBreak"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return ast.WsDoubleBreak{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}})
            return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"no such field ", fname, " in union"}).(string))}).(func(any) any)(func (f func(core.Term) any) any {
              return f(fterm.(core.Term))
            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(variantMap))
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected union")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}
