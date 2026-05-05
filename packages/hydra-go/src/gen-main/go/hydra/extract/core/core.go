// Note: this is an automatically generated file. Do not edit.

package extractcore

import (
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  libliterals "hydra.dev/hydra/lib/literals"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/rewriting"
  showcore "hydra.dev/hydra/show/core"
  "math/big"
)

func Bigfloat (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(FloatLiteral(cx, l)).(func(any) any)(func (f core.FloatValue) any {
      return BigfloatValue(cx, f)
    })
  })
}

func BigfloatValue (cx context.Context, v core.FloatValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatValueBigfloat:
      return func (f float64) any {
        return [2]any{"right", f}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("bigfloat")).(func(any) any)(" but found ")).(func(any) any)(showcore.Float(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func Bigint (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(IntegerLiteral(cx, l)).(func(any) any)(func (i core.IntegerValue) any {
      return BigintValue(cx, i)
    })
  })
}

func BigintValue (cx context.Context, v core.IntegerValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueBigint:
      return func (i *big.Int) any {
        return [2]any{"right", i}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("bigint")).(func(any) any)(" but found ")).(func(any) any)(showcore.Integer(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func Binary (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return BinaryLiteral(cx, l)
  })
}

func BinaryLiteral (cx context.Context, v core.Literal) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralBinary:
      return func (b []byte) any {
        return [2]any{"right", b}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("binary")).(func(any) any)(" but found ")).(func(any) any)(showcore.Literal(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func Boolean (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return BooleanLiteral(cx, l)
  })
}

func BooleanLiteral (cx context.Context, v core.Literal) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralBoolean:
      return func (b bool) any {
        return [2]any{"right", b}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("boolean")).(func(any) any)(" but found ")).(func(any) any)(showcore.Literal(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func CaseField (cx context.Context, name core.Name, n string, graph graph.Graph, term core.Term) any {
  return func () any {
    var fieldName any = core.Name(n)
    return libeithers.Bind(Cases(cx, name, graph, term)).(func(any) any)(func (cs core.CaseStatement) any {
      return func () any {
        var matching any = liblists.Filter(func (f core.Field) any {
          return libequality.Equal(func (v any) any {
            return v.(core.Field).Name
          }(f)).(func(any) any)(fieldName)
        }).(func(any) any)(func (v any) any {
          return v.(core.CaseStatement).Cases
        }(cs))
        return liblogic.IfElse(liblists.Null(matching)).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError("not enough cases")}, Context: cx}}).(func(any) any)([2]any{"right", liblists.Head(matching)})
      }()
    })
  }()
}

func Cases (cx context.Context, name core.Name, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermFunction:
        return func (function core.Function) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.FunctionElimination:
              return func (elimination core.Elimination) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.EliminationUnion:
                    return func (cs core.CaseStatement) any {
                      return liblogic.IfElse(libequality.Equal(func (v any) any {
                        return v.(core.CaseStatement).TypeName
                      }(cs)).(func(any) any)(func (v any) any {
                        return v
                      }(name))).(func(any) any)([2]any{"right", cs}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)(libstrings.Cat2("case statement for type ").(func(any) any)(func (v any) any {
                        return v
                      }(name)))).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}})
                    }(v.Value)
                    default:
                    return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("case statement")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
                  }
                  return nil
                }(elimination)
              }(v.Value)
              default:
              return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("case statement")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
            }
            return nil
          }(function)
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("case statement")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(term)
  })
}

func Field (cx context.Context, fname core.Name, mapping func(core.Term) any, graph graph.Graph, fields []any) any {
  return func () any {
    var matchingFields any = liblists.Filter(func (f core.Field) any {
      return libequality.Equal(func (v any) any {
        return v.(core.Field).Name
      }(f)).(func(any) any)(func (v any) any {
        return v
      }(fname))
    }).(func(any) any)(fields)
    return liblogic.IfElse(liblists.Null(matchingFields)).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)(libstrings.Cat2("field ").(func(any) any)(func (v any) any {
      return v
    }(fname)))).(func(any) any)(" but found ")).(func(any) any)("no matching field").(string))}, Context: cx}}).(func(any) any)(liblogic.IfElse(libequality.Equal(liblists.Length(matchingFields)).(func(any) any)(1)).(func(any) any)(libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, liblists.Head(matchingFields).(core.Field).Term)).(func(any) any)(func (stripped core.Term) any {
      return mapping(stripped)
    })).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("single field")).(func(any) any)(" but found ")).(func(any) any)(libstrings.Cat2("multiple fields named ").(func(any) any)(func (v any) any {
      return v
    }(fname))).(string))}, Context: cx}}))
  }()
}

func Float32_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(FloatLiteral(cx, l)).(func(any) any)(func (f core.FloatValue) any {
      return Float32Value(cx, f)
    })
  })
}

func Float32Value (cx context.Context, v core.FloatValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatValueFloat32_:
      return func (f float32) any {
        return [2]any{"right", f}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("float32")).(func(any) any)(" but found ")).(func(any) any)(showcore.Float(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func Float64_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(FloatLiteral(cx, l)).(func(any) any)(func (f core.FloatValue) any {
      return Float64Value(cx, f)
    })
  })
}

func Float64Value (cx context.Context, v core.FloatValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatValueFloat64_:
      return func (f float64) any {
        return [2]any{"right", f}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("float64")).(func(any) any)(" but found ")).(func(any) any)(showcore.Float(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func FloatLiteral (cx context.Context, lit core.Literal) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralFloat:
      return func (v core.FloatValue) any {
        return [2]any{"right", v}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("floating-point value")).(func(any) any)(" but found ")).(func(any) any)(showcore.Literal(lit)).(string))}, Context: cx}}
    }
    return nil
  }(lit)
}

func FloatValue (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return FloatLiteral(cx, l)
  })
}

func EitherTerm (cx context.Context, leftFun func(core.Term) any, rightFun func(core.Term) any, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermEither:
        return func (et any) any {
          return libeithers.Either(func (l core.Term) any {
            return libeithers.Map(func (x any) any {
              return [2]any{"left", x}
            }).(func(any) any)(leftFun(l))
          }).(func(any) any)(func (r core.Term) any {
            return libeithers.Map(func (x any) any {
              return [2]any{"right", x}
            }).(func(any) any)(rightFun(r))
          }).(func(any) any)(et)
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("either value")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(term)
  })
}

func EitherType (cx context.Context, typ core.Type) any {
  return func () any {
    var stripped any = rewriting.DeannotateType(typ)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeEither:
        return func (et core.EitherType) any {
          return [2]any{"right", et}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("either type")).(func(any) any)(" but found ")).(func(any) any)(showcore.Type_(typ)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  }()
}

func FunctionType (cx context.Context, typ core.Type) any {
  return func () any {
    var stripped any = rewriting.DeannotateType(typ)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeFunction:
        return func (ft core.FunctionType) any {
          return [2]any{"right", ft}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("function type")).(func(any) any)(" but found ")).(func(any) any)(showcore.Type_(typ)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  }()
}

func Injection (cx context.Context, expected core.Name, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (injection core.Injection) any {
          return liblogic.IfElse(libequality.Equal(func (v any) any {
            return v.(core.Injection).TypeName
          }(injection)).(func(any) any)(func (v any) any {
            return v
          }(expected))).(func(any) any)([2]any{"right", func (v any) any {
            return v.(core.Injection).Field
          }(injection)}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)(libstrings.Cat2("injection of type ").(func(any) any)(func (v any) any {
            return v
          }(expected)))).(func(any) any)(" but found ")).(func(any) any)(func (v any) any {
            return v.(core.Injection).TypeName
          }(injection)).(string))}, Context: cx}})
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("injection")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(term)
  })
}

func Int16_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(IntegerLiteral(cx, l)).(func(any) any)(func (i core.IntegerValue) any {
      return Int16Value(cx, i)
    })
  })
}

func Int16Value (cx context.Context, v core.IntegerValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueInt16_:
      return func (i int16) any {
        return [2]any{"right", i}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("int16")).(func(any) any)(" but found ")).(func(any) any)(showcore.Integer(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func Int32_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(IntegerLiteral(cx, l)).(func(any) any)(func (i core.IntegerValue) any {
      return Int32Value(cx, i)
    })
  })
}

func Int32Value (cx context.Context, v core.IntegerValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueInt32_:
      return func (i int32) any {
        return [2]any{"right", i}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("int32")).(func(any) any)(" but found ")).(func(any) any)(showcore.Integer(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func Int64_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(IntegerLiteral(cx, l)).(func(any) any)(func (i core.IntegerValue) any {
      return Int64Value(cx, i)
    })
  })
}

func Int64Value (cx context.Context, v core.IntegerValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueInt64_:
      return func (i int64) any {
        return [2]any{"right", i}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("int64")).(func(any) any)(" but found ")).(func(any) any)(showcore.Integer(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func Int8_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(IntegerLiteral(cx, l)).(func(any) any)(func (i core.IntegerValue) any {
      return Int8Value(cx, i)
    })
  })
}

func Int8Value (cx context.Context, v core.IntegerValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueInt8_:
      return func (i int8) any {
        return [2]any{"right", i}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("int8")).(func(any) any)(" but found ")).(func(any) any)(showcore.Integer(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func IntegerLiteral (cx context.Context, lit core.Literal) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralInteger:
      return func (v core.IntegerValue) any {
        return [2]any{"right", v}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("integer value")).(func(any) any)(" but found ")).(func(any) any)(showcore.Literal(lit)).(string))}, Context: cx}}
    }
    return nil
  }(lit)
}

func IntegerValue (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return IntegerLiteral(cx, l)
  })
}

func LambdaBody (cx context.Context, graph graph.Graph, term core.Term) any {
  return libeithers.Map(func (v any) any {
    return v.(core.Lambda).Body
  }).(func(any) any)(Lambda(cx, graph, term))
}

func Lambda (cx context.Context, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermFunction:
        return func (function core.Function) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.FunctionLambda:
              return func (l core.Lambda) any {
                return [2]any{"right", l}
              }(v.Value)
              default:
              return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("lambda")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
            }
            return nil
          }(function)
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("lambda")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(term)
  })
}

func LetBinding (cx context.Context, n string, graph graph.Graph, term core.Term) any {
  return func () any {
    var name any = core.Name(n)
    return libeithers.Bind(Let(cx, graph, term)).(func(any) any)(func (letExpr core.Let) any {
      return func () any {
        var matchingBindings any = liblists.Filter(func (b core.Binding) any {
          return libequality.Equal(func (v any) any {
            return v.(core.Binding).Name
          }(b)).(func(any) any)(name)
        }).(func(any) any)(func (v any) any {
          return v.(core.Let).Bindings
        }(letExpr))
        return liblogic.IfElse(liblists.Null(matchingBindings)).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("no such binding: ").(func(any) any)(n).(string))}, Context: cx}}).(func(any) any)(liblogic.IfElse(libequality.Equal(liblists.Length(matchingBindings)).(func(any) any)(1)).(func(any) any)([2]any{"right", liblists.Head(matchingBindings).(core.Binding).Term}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("multiple bindings named ").(func(any) any)(n).(string))}, Context: cx}}))
      }()
    })
  }()
}

func Let (cx context.Context, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermLet:
        return func (lt core.Let) any {
          return [2]any{"right", lt}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("let term")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(term)
  })
}

func List (cx context.Context, graph graph.Graph, term core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term)).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermList:
        return func (l []any) any {
          return [2]any{"right", l}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("list")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(stripped)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  })
}

func ListHead (cx context.Context, graph graph.Graph, term core.Term) any {
  return libeithers.Bind(List(cx, graph, term)).(func(any) any)(func (l []any) any {
    return liblogic.IfElse(liblists.Null(l)).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError("empty list")}, Context: cx}}).(func(any) any)([2]any{"right", liblists.Head(l)})
  })
}

func ListOf (cx context.Context, f func(core.Term) any, graph graph.Graph, term core.Term) any {
  return libeithers.Bind(List(cx, graph, term)).(func(any) any)(func (els []any) any {
    return libeithers.MapList(f).(func(any) any)(els)
  })
}

func ListType (cx context.Context, typ core.Type) any {
  return func () any {
    var stripped any = rewriting.DeannotateType(typ)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeList:
        return func (t core.Type) any {
          return [2]any{"right", t}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("list type")).(func(any) any)(" but found ")).(func(any) any)(showcore.Type_(typ)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  }()
}

func Literal (cx context.Context, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermLiteral:
        return func (lit core.Literal) any {
          return [2]any{"right", lit}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("literal")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(term)
  })
}

func Map_ (cx context.Context, fk func(core.Term) any, fv func(core.Term) any, graph graph.Graph, term0 core.Term) any {
  return func () any {
    pair := func (kvPair any) any {
      return func () any {
        var kterm any = libpairs.First(kvPair)
        return func () any {
          var vterm any = libpairs.Second(kvPair)
          return libeithers.Bind(fk(kterm.(core.Term))).(func(any) any)(func (kval any) any {
            return libeithers.Bind(fv(vterm.(core.Term))).(func(any) any)(func (vval any) any {
              return [2]any{"right", [2]any{kval, vval}}
            })
          })
        }()
      }()
    }
    return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.TermMap_:
          return func (m []any) any {
            return libeithers.Map(libmaps.FromList).(func(any) any)(libeithers.MapList(pair).(func(any) any)(libmaps.ToList(m)))
          }(v.Value)
          default:
          return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("map")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
        }
        return nil
      }(term)
    })
  }()
}

func MapType (cx context.Context, typ core.Type) any {
  return func () any {
    var stripped any = rewriting.DeannotateType(typ)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeMap_:
        return func (mt core.MapType) any {
          return [2]any{"right", mt}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("map type")).(func(any) any)(" but found ")).(func(any) any)(showcore.Type_(typ)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  }()
}

func NArgs (cx context.Context, name core.Name, n int32, args []any) any {
  return liblogic.IfElse(libequality.Equal(liblists.Length(args)).(func(any) any)(n)).(func(any) any)([2]any{"right", struct{}{}}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)(libstrings.Cat([]any{libliterals.ShowInt32(n), " arguments to primitive ", libliterals.ShowString(func (v any) any {
    return v
  }(name))}))).(func(any) any)(" but found ")).(func(any) any)(libliterals.ShowInt32(liblists.Length(args))).(string))}, Context: cx}})
}

func MaybeTerm (cx context.Context, f func(core.Term) any, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermMaybe:
        return func (mt any) any {
          return libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (t core.Term) any {
            return libeithers.Map(libmaybes.Pure).(func(any) any)(f(t))
          }).(func(any) any)(mt)
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("maybe value")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(term)
  })
}

func MaybeType (cx context.Context, typ core.Type) any {
  return func () any {
    var stripped any = rewriting.DeannotateType(typ)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeMaybe:
        return func (t core.Type) any {
          return [2]any{"right", t}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("maybe type")).(func(any) any)(" but found ")).(func(any) any)(showcore.Type_(typ)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  }()
}

func Pair (cx context.Context, kf func(core.Term) any, vf func(core.Term) any, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermPair:
        return func (p any) any {
          return libeithers.Bind(kf(libpairs.First(p).(core.Term))).(func(any) any)(func (kVal any) any {
            return libeithers.Bind(vf(libpairs.Second(p).(core.Term))).(func(any) any)(func (vVal any) any {
              return [2]any{"right", [2]any{kVal, vVal}}
            })
          })
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("pair")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(term)
  })
}

func Record (cx context.Context, expected core.Name, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(TermRecord(cx, graph, term0)).(func(any) any)(func (record core.Record) any {
    return liblogic.IfElse(libequality.Equal(func (v any) any {
      return v.(core.Record).TypeName
    }(record)).(func(any) any)(expected)).(func(any) any)([2]any{"right", func (v any) any {
      return v.(core.Record).Fields
    }(record)}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)(libstrings.Cat2("record of type ").(func(any) any)(func (v any) any {
      return v
    }(expected)))).(func(any) any)(" but found ")).(func(any) any)(func (v any) any {
      return v.(core.Record).TypeName
    }(record)).(string))}, Context: cx}})
  })
}

func RecordType[T0 any] (cx context.Context, ename T0, typ core.Type) any {
  return func () any {
    var stripped any = rewriting.DeannotateType(typ)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeRecord:
        return func (fields []any) any {
          return [2]any{"right", fields}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("record type")).(func(any) any)(" but found ")).(func(any) any)(showcore.Type_(typ)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  }()
}

func Set (cx context.Context, graph graph.Graph, term core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term)).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermSet:
        return func (s []any) any {
          return [2]any{"right", s}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("set")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(stripped)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  })
}

func SetOf (cx context.Context, f func(core.Term) any, graph graph.Graph, term core.Term) any {
  return libeithers.Bind(Set(cx, graph, term)).(func(any) any)(func (els []any) any {
    return libeithers.MapSet(f).(func(any) any)(els)
  })
}

func SetType (cx context.Context, typ core.Type) any {
  return func () any {
    var stripped any = rewriting.DeannotateType(typ)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeSet:
        return func (t core.Type) any {
          return [2]any{"right", t}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("set type")).(func(any) any)(" but found ")).(func(any) any)(showcore.Type_(typ)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  }()
}

func String_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return StringLiteral(cx, l)
  })
}

func StringLiteral (cx context.Context, v core.Literal) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralString_:
      return func (s string) any {
        return [2]any{"right", s}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("string")).(func(any) any)(" but found ")).(func(any) any)(showcore.Literal(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func TermRecord (cx context.Context, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return [2]any{"right", record}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("record")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(term)
  })
}

func Uint16_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(IntegerLiteral(cx, l)).(func(any) any)(func (i core.IntegerValue) any {
      return Uint16Value(cx, i)
    })
  })
}

func Uint16Value (cx context.Context, v core.IntegerValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueUint16_:
      return func (i uint16) any {
        return [2]any{"right", i}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("uint16")).(func(any) any)(" but found ")).(func(any) any)(showcore.Integer(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func Uint32_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(IntegerLiteral(cx, l)).(func(any) any)(func (i core.IntegerValue) any {
      return Uint32Value(cx, i)
    })
  })
}

func Uint32Value (cx context.Context, v core.IntegerValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueUint32_:
      return func (i uint32) any {
        return [2]any{"right", i}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("uint32")).(func(any) any)(" but found ")).(func(any) any)(showcore.Integer(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func Uint64_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(IntegerLiteral(cx, l)).(func(any) any)(func (i core.IntegerValue) any {
      return Uint64Value(cx, i)
    })
  })
}

func Uint64Value (cx context.Context, v core.IntegerValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueUint64_:
      return func (i uint64) any {
        return [2]any{"right", i}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("uint64")).(func(any) any)(" but found ")).(func(any) any)(showcore.Integer(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func Uint8_ (cx context.Context, graph graph.Graph, t core.Term) any {
  return libeithers.Bind(Literal(cx, graph, t)).(func(any) any)(func (l core.Literal) any {
    return libeithers.Bind(IntegerLiteral(cx, l)).(func(any) any)(func (i core.IntegerValue) any {
      return Uint8Value(cx, i)
    })
  })
}

func Uint8Value (cx context.Context, v core.IntegerValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueUint8_:
      return func (i uint8) any {
        return [2]any{"right", i}
      }(v.Value)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("uint8")).(func(any) any)(" but found ")).(func(any) any)(showcore.Integer(v)).(string))}, Context: cx}}
    }
    return nil
  }(v)
}

func UnionType[T0 any] (cx context.Context, ename T0, typ core.Type) any {
  return func () any {
    var stripped any = rewriting.DeannotateType(typ)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeUnion:
        return func (fields []any) any {
          return [2]any{"right", fields}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("union type")).(func(any) any)(" but found ")).(func(any) any)(showcore.Type_(typ)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  }()
}

func Unit (cx context.Context, term core.Term) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermUnit:
      return func (_ struct{}) any {
        return [2]any{"right", struct{}{}}
      }(v)
      default:
      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("unit")).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
    }
    return nil
  }(term)
}

func UnitVariant (cx context.Context, tname core.Name, graph graph.Graph, term core.Term) any {
  return libeithers.Bind(Injection(cx, tname, graph, term)).(func(any) any)(func (field core.Field) any {
    return libeithers.Bind(Unit(cx, func (v any) any {
      return v.(core.Field).Term
    }(field).(core.Term))).(func(any) any)(func (ignored struct{}) any {
      return [2]any{"right", func (v any) any {
        return v.(core.Field).Name
      }(field)}
    })
  })
}

func Wrap (cx context.Context, expected core.Name, graph graph.Graph, term0 core.Term) any {
  return libeithers.Bind(lexical.StripAndDereferenceTerm(cx, graph, term0)).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return liblogic.IfElse(libequality.Equal(func (v any) any {
            return v.(core.WrappedTerm).TypeName
          }(wrappedTerm)).(func(any) any)(func (v any) any {
            return v
          }(expected))).(func(any) any)([2]any{"right", func (v any) any {
            return v.(core.WrappedTerm).Body
          }(wrappedTerm)}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)(libstrings.Cat2("wrapper of type ").(func(any) any)(func (v any) any {
            return v
          }(expected)))).(func(any) any)(" but found ")).(func(any) any)(func (v any) any {
            return v.(core.WrappedTerm).TypeName
          }(wrappedTerm)).(string))}, Context: cx}})
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)(libstrings.Cat2(libstrings.Cat2("wrap(").(func(any) any)(func (v any) any {
          return v
        }(expected))).(func(any) any)(")"))).(func(any) any)(" but found ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(term)
  })
}

func WrappedType[T0 any] (cx context.Context, ename T0, typ core.Type) any {
  return func () any {
    var stripped any = rewriting.DeannotateType(typ)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeWrap:
        return func (innerType core.Type) any {
          return [2]any{"right", innerType}
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("wrapped type")).(func(any) any)(" but found ")).(func(any) any)(showcore.Type_(typ)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  }()
}
