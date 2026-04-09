// Note: this is an automatically generated file. Do not edit.

package showcore

import (
  "hydra.dev/hydra/core"
  libeithers "hydra.dev/hydra/lib/eithers"
  liblists "hydra.dev/hydra/lib/lists"
  libliterals "hydra.dev/hydra/lib/literals"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  "math/big"
)

func ReadTerm (s string) any {
  return func () any {
    _v := core.TermLiteral{Value: core.LiteralString_{Value: s}}
    return &_v
  }()
}

func Binding (el core.Binding) string {
  return func () any {
    var name any = func (v any) any {
      return v.(core.Binding).Name
    }(el)
    return func () any {
      var t any = func (v any) any {
        return v.(core.Binding).Term
      }(el)
      return func () any {
        var typeStr any = libmaybes.Maybe("").(func(any) any)(func (ts core.TypeScheme) any {
          return libstrings.Cat([]any{":(", TypeScheme(ts), ")"})
        }).(func(any) any)(func (v any) any {
          return v.(core.Binding).Type_
        }(el))
        return libstrings.Cat([]any{name, typeStr, " = ", Term(t.(core.Term))})
      }()
    }()
  }().(string)
}

func Elimination (elm core.Elimination) string {
  return func (x any) any {
    switch v := x.(type) {
      case core.EliminationRecord:
      return func (proj core.Projection) any {
        return func () any {
          var tname any = func (v any) any {
            return v.(core.Projection).TypeName
          }(proj)
          return func () any {
            var fname any = func (v any) any {
              return v.(core.Projection).Field
            }(proj)
            return libstrings.Cat([]any{"project(", tname, "){", fname, "}"})
          }()
        }()
      }(v.Value)
      case core.EliminationUnion:
      return func (cs core.CaseStatement) any {
        return func () any {
          var tname any = func (v any) any {
            return v.(core.CaseStatement).TypeName
          }(cs)
          return func () any {
            var mdef any = func (v any) any {
              return v.(core.CaseStatement).Default_
            }(cs)
            return func () any {
              var cases any = func (v any) any {
                return v.(core.CaseStatement).Cases
              }(cs)
              return func () any {
                var defaultField any = libmaybes.Maybe([]any{}).(func(any) any)(func (d core.Term) any {
                  return []any{core.Field{Name: core.Name("[default]"), Term: d}}
                }).(func(any) any)(mdef)
                return func () any {
                  var allFields any = liblists.Concat([]any{cases, defaultField})
                  return libstrings.Cat([]any{"case(", tname, ")", Fields(allFields.([]any))})
                }()
              }()
            }()
          }()
        }()
      }(v.Value)
      case core.EliminationWrap:
      return func (tname core.Name) any {
        return libstrings.Cat([]any{"unwrap(", func (v any) any {
          return v
        }(tname), ")"})
      }(v.Value)
    }
    return nil
  }(elm).(string)
}

func Field (field core.Field) string {
  return func () any {
    var fname any = func (v any) any {
      return v.(core.Field).Name
    }(field)
    return func () any {
      var fterm any = func (v any) any {
        return v.(core.Field).Term
      }(field)
      return libstrings.Cat([]any{fname, "=", Term(fterm.(core.Term))})
    }()
  }().(string)
}

func FieldType (ft core.FieldType) string {
  return func () any {
    var fname any = func (v any) any {
      return v.(core.FieldType).Name
    }(ft)
    return func () any {
      var ftyp any = func (v any) any {
        return v.(core.FieldType).Type_
      }(ft)
      return libstrings.Cat([]any{fname, ":", Type_(ftyp.(core.Type))})
    }()
  }().(string)
}

func Fields (flds []any) string {
  return func () any {
    var fieldStrs any = liblists.Map(Field).(func(any) any)(flds)
    return libstrings.Cat([]any{"{", libstrings.Intercalate(", ").(func(any) any)(fieldStrs), "}"})
  }().(string)
}

func Float (fv core.FloatValue) string {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatValueBigfloat:
      return func (v float64) any {
        return libstrings.Cat2(libliterals.ShowBigfloat(v)).(func(any) any)(":bigfloat")
      }(v.Value)
      case core.FloatValueFloat32_:
      return func (v float32) any {
        return libstrings.Cat2(libliterals.ShowFloat32(v)).(func(any) any)(":float32")
      }(v.Value)
      case core.FloatValueFloat64_:
      return func (v float64) any {
        return libstrings.Cat2(libliterals.ShowFloat64(v)).(func(any) any)(":float64")
      }(v.Value)
    }
    return nil
  }(fv).(string)
}

func FloatType (ft core.FloatType) string {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatTypeBigfloat:
      return func (_ struct{}) any {
        return "bigfloat"
      }(v)
      case core.FloatTypeFloat32_:
      return func (_ struct{}) any {
        return "float32"
      }(v)
      case core.FloatTypeFloat64_:
      return func (_ struct{}) any {
        return "float64"
      }(v)
    }
    return nil
  }(ft).(string)
}

func Function (f core.Function) string {
  return func (x any) any {
    switch v := x.(type) {
      case core.FunctionElimination:
      return func (v1 core.Elimination) any {
        return Elimination(v1)
      }(v.Value)
      case core.FunctionLambda:
      return func (v1 core.Lambda) any {
        return Lambda(v1)
      }(v.Value)
      case core.FunctionPrimitive:
      return func (name core.Name) any {
        return libstrings.Cat2(func (v any) any {
          return v
        }(name)).(func(any) any)("!")
      }(v.Value)
    }
    return nil
  }(f).(string)
}

func Injection (inj core.Injection) string {
  return func () any {
    var tname any = func (v any) any {
      return v.(core.Injection).TypeName
    }(inj)
    return func () any {
      var f any = func (v any) any {
        return v.(core.Injection).Field
      }(inj)
      return libstrings.Cat([]any{"inject(", tname, ")", Fields([]any{f})})
    }()
  }().(string)
}

func Integer (iv core.IntegerValue) string {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueBigint:
      return func (v *big.Int) any {
        return libstrings.Cat2(libliterals.ShowBigint(v)).(func(any) any)(":bigint")
      }(v.Value)
      case core.IntegerValueInt8_:
      return func (v int8) any {
        return libstrings.Cat2(libliterals.ShowInt8(v)).(func(any) any)(":int8")
      }(v.Value)
      case core.IntegerValueInt16_:
      return func (v int16) any {
        return libstrings.Cat2(libliterals.ShowInt16(v)).(func(any) any)(":int16")
      }(v.Value)
      case core.IntegerValueInt32_:
      return func (v int32) any {
        return libstrings.Cat2(libliterals.ShowInt32(v)).(func(any) any)(":int32")
      }(v.Value)
      case core.IntegerValueInt64_:
      return func (v int64) any {
        return libstrings.Cat2(libliterals.ShowInt64(v)).(func(any) any)(":int64")
      }(v.Value)
      case core.IntegerValueUint8_:
      return func (v uint8) any {
        return libstrings.Cat2(libliterals.ShowUint8(v)).(func(any) any)(":uint8")
      }(v.Value)
      case core.IntegerValueUint16_:
      return func (v uint16) any {
        return libstrings.Cat2(libliterals.ShowUint16(v)).(func(any) any)(":uint16")
      }(v.Value)
      case core.IntegerValueUint32_:
      return func (v uint32) any {
        return libstrings.Cat2(libliterals.ShowUint32(v)).(func(any) any)(":uint32")
      }(v.Value)
      case core.IntegerValueUint64_:
      return func (v uint64) any {
        return libstrings.Cat2(libliterals.ShowUint64(v)).(func(any) any)(":uint64")
      }(v.Value)
    }
    return nil
  }(iv).(string)
}

func IntegerType (it core.IntegerType) string {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerTypeBigint:
      return func (_ struct{}) any {
        return "bigint"
      }(v)
      case core.IntegerTypeInt8_:
      return func (_ struct{}) any {
        return "int8"
      }(v)
      case core.IntegerTypeInt16_:
      return func (_ struct{}) any {
        return "int16"
      }(v)
      case core.IntegerTypeInt32_:
      return func (_ struct{}) any {
        return "int32"
      }(v)
      case core.IntegerTypeInt64_:
      return func (_ struct{}) any {
        return "int64"
      }(v)
      case core.IntegerTypeUint8_:
      return func (_ struct{}) any {
        return "uint8"
      }(v)
      case core.IntegerTypeUint16_:
      return func (_ struct{}) any {
        return "uint16"
      }(v)
      case core.IntegerTypeUint32_:
      return func (_ struct{}) any {
        return "uint32"
      }(v)
      case core.IntegerTypeUint64_:
      return func (_ struct{}) any {
        return "uint64"
      }(v)
    }
    return nil
  }(it).(string)
}

func Lambda (l core.Lambda) string {
  return func () any {
    var v any = func (v any) any {
      return v.(core.Lambda).Parameter
    }(l)
    return func () any {
      var mt any = func (v any) any {
        return v.(core.Lambda).Domain
      }(l)
      return func () any {
        var body any = func (v any) any {
          return v.(core.Lambda).Body
        }(l)
        return func () any {
          var typeStr any = libmaybes.Maybe("").(func(any) any)(func (t core.Type) any {
            return libstrings.Cat2(":").(func(any) any)(Type_(t))
          }).(func(any) any)(mt)
          return libstrings.Cat([]any{"\u03BB", v, typeStr, ".", Term(body.(core.Term))})
        }()
      }()
    }()
  }().(string)
}

func Let (l core.Let) string {
  return func () any {
    var bindings any = func (v any) any {
      return v.(core.Let).Bindings
    }(l)
    return func () any {
      var env any = func (v any) any {
        return v.(core.Let).Body
      }(l)
      return func () any {
        var bindingStrs any = liblists.Map(Binding).(func(any) any)(bindings)
        return libstrings.Cat([]any{"let ", libstrings.Intercalate(", ").(func(any) any)(bindingStrs), " in ", Term(env.(core.Term))})
      }()
    }()
  }().(string)
}

func List[T0 any] (f func(T0) string, xs []any) string {
  return func () any {
    var elementStrs any = liblists.Map(f).(func(any) any)(xs)
    return libstrings.Cat([]any{"[", libstrings.Intercalate(", ").(func(any) any)(elementStrs), "]"})
  }().(string)
}

func Literal (l core.Literal) string {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralBinary:
      return func (_ []byte) any {
        return "[binary]"
      }(v.Value)
      case core.LiteralBoolean:
      return func (b bool) any {
        return liblogic.IfElse(b).(func(any) any)("true").(func(any) any)("false")
      }(v.Value)
      case core.LiteralFloat:
      return func (fv core.FloatValue) any {
        return Float(fv)
      }(v.Value)
      case core.LiteralInteger:
      return func (iv core.IntegerValue) any {
        return Integer(iv)
      }(v.Value)
      case core.LiteralString_:
      return func (s string) any {
        return libliterals.ShowString(s)
      }(v.Value)
    }
    return nil
  }(l).(string)
}

func LiteralType (lt core.LiteralType) string {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralTypeBinary:
      return func (_ struct{}) any {
        return "binary"
      }(v)
      case core.LiteralTypeBoolean:
      return func (_ struct{}) any {
        return "boolean"
      }(v)
      case core.LiteralTypeFloat:
      return func (ft core.FloatType) any {
        return FloatType(ft)
      }(v.Value)
      case core.LiteralTypeInteger:
      return func (it core.IntegerType) any {
        return IntegerType(it)
      }(v.Value)
      case core.LiteralTypeString_:
      return func (_ struct{}) any {
        return "string"
      }(v)
    }
    return nil
  }(lt).(string)
}

func Term (t core.Term) string {
  return func () any {
    var gatherTerms func([]any) any
    gatherTerms = func (prev []any) any {
      return func (app core.Application) any {
        return func () any {
          var lhs any = func (v any) any {
            return v.(core.Application).Function
          }(app)
          return func () any {
            var rhs any = func (v any) any {
              return v.(core.Application).Argument
            }(app)
            return func (x any) any {
              switch v := x.(type) {
                case core.TermApplication:
                return func (app2 core.Application) any {
                  return gatherTerms(liblists.Cons(rhs).(func(any) any)(prev).([]any)).(func(any) any)(app2)
                }(v.Value)
                default:
                return liblists.Cons(lhs).(func(any) any)(liblists.Cons(rhs).(func(any) any)(prev))
              }
              return nil
            }(lhs)
          }()
        }()
      }
    }
    return func (x any) any {
      switch v := x.(type) {
        case core.TermAnnotated:
        return func (at core.AnnotatedTerm) any {
          return Term(func (v any) any {
            return v.(core.AnnotatedTerm).Body
          }(at).(core.Term))
        }(v.Value)
        case core.TermApplication:
        return func (app core.Application) any {
          return func () any {
            var terms any = gatherTerms([]any{}).(func(any) any)(app)
            return func () any {
              var termStrs any = liblists.Map(Term).(func(any) any)(terms)
              return libstrings.Cat([]any{"(", libstrings.Intercalate(" @ ").(func(any) any)(termStrs), ")"})
            }()
          }()
        }(v.Value)
        case core.TermEither:
        return func (e any) any {
          return libeithers.Either(func (l core.Term) any {
            return libstrings.Cat([]any{"left(", Term(l), ")"})
          }).(func(any) any)(func (r core.Term) any {
            return libstrings.Cat([]any{"right(", Term(r), ")"})
          }).(func(any) any)(e)
        }(v.Value)
        case core.TermFunction:
        return func (v1 core.Function) any {
          return Function(v1)
        }(v.Value)
        case core.TermLet:
        return func (l core.Let) any {
          return Let(l)
        }(v.Value)
        case core.TermList:
        return func (els []any) any {
          return func () any {
            var termStrs any = liblists.Map(Term).(func(any) any)(els)
            return libstrings.Cat([]any{"[", libstrings.Intercalate(", ").(func(any) any)(termStrs), "]"})
          }()
        }(v.Value)
        case core.TermLiteral:
        return func (lit core.Literal) any {
          return Literal(lit)
        }(v.Value)
        case core.TermMap_:
        return func (m []any) any {
          return func () any {
            entry := func (p any) any {
              return libstrings.Cat([]any{Term(libpairs.First(p).(core.Term)), "=", Term(libpairs.Second(p).(core.Term))})
            }
            return libstrings.Cat([]any{"{", libstrings.Intercalate(", ").(func(any) any)(liblists.Map(entry).(func(any) any)(libmaps.ToList(m))), "}"})
          }()
        }(v.Value)
        case core.TermMaybe:
        return func (mt any) any {
          return libmaybes.Maybe("nothing").(func(any) any)(func (t2 core.Term) any {
            return libstrings.Cat([]any{"just(", Term(t2), ")"})
          }).(func(any) any)(mt)
        }(v.Value)
        case core.TermPair:
        return func (p any) any {
          return libstrings.Cat([]any{"(", Term(libpairs.First(p).(core.Term)), ", ", Term(libpairs.Second(p).(core.Term)), ")"})
        }(v.Value)
        case core.TermRecord:
        return func (rec core.Record) any {
          return func () any {
            var tname any = func (v any) any {
              return v.(core.Record).TypeName
            }(rec)
            return func () any {
              var flds any = func (v any) any {
                return v.(core.Record).Fields
              }(rec)
              return libstrings.Cat([]any{"record(", tname, ")", Fields(flds.([]any))})
            }()
          }()
        }(v.Value)
        case core.TermSet:
        return func (s []any) any {
          return libstrings.Cat([]any{"{", libstrings.Intercalate(", ").(func(any) any)(liblists.Map(Term).(func(any) any)(libsets.ToList(s))), "}"})
        }(v.Value)
        case core.TermTypeLambda:
        return func (ta core.TypeLambda) any {
          return func () any {
            var param any = func (v any) any {
              return v.(core.TypeLambda).Parameter
            }(ta)
            return func () any {
              var body any = func (v any) any {
                return v.(core.TypeLambda).Body
              }(ta)
              return libstrings.Cat([]any{"\u039B", param, ".", Term(body.(core.Term))})
            }()
          }()
        }(v.Value)
        case core.TermTypeApplication:
        return func (tt core.TypeApplicationTerm) any {
          return func () any {
            var t2 any = func (v any) any {
              return v.(core.TypeApplicationTerm).Body
            }(tt)
            return func () any {
              var typ any = func (v any) any {
                return v.(core.TypeApplicationTerm).Type_
              }(tt)
              return libstrings.Cat([]any{Term(t2.(core.Term)), "\u27E8", Type_(typ.(core.Type)), "\u27E9"})
            }()
          }()
        }(v.Value)
        case core.TermUnion:
        return func (v1 core.Injection) any {
          return Injection(v1)
        }(v.Value)
        case core.TermUnit:
        return func (_ struct{}) any {
          return "unit"
        }(v)
        case core.TermVariable:
        return func (name core.Name) any {
          return func (v any) any {
            return v
          }(name)
        }(v.Value)
        case core.TermWrap:
        return func (wt core.WrappedTerm) any {
          return func () any {
            var tname any = func (v any) any {
              return v.(core.WrappedTerm).TypeName
            }(wt)
            return func () any {
              var term1 any = func (v any) any {
                return v.(core.WrappedTerm).Body
              }(wt)
              return libstrings.Cat([]any{"wrap(", tname, "){", Term(term1.(core.Term)), "}"})
            }()
          }()
        }(v.Value)
      }
      return nil
    }(t)
  }().(string)
}

func Type_ (typ core.Type) string {
  return func () any {
    showRowType := func (flds []any) any {
      return func () any {
        var fieldStrs any = liblists.Map(FieldType).(func(any) any)(flds)
        return libstrings.Cat([]any{"{", libstrings.Intercalate(", ").(func(any) any)(fieldStrs), "}"})
      }()
    }
    return func () any {
      var gatherTypes func([]any) any
      gatherTypes = func (prev []any) any {
        return func (app core.ApplicationType) any {
          return func () any {
            var lhs any = func (v any) any {
              return v.(core.ApplicationType).Function
            }(app)
            return func () any {
              var rhs any = func (v any) any {
                return v.(core.ApplicationType).Argument
              }(app)
              return func (x any) any {
                switch v := x.(type) {
                  case core.TypeApplication:
                  return func (app2 core.ApplicationType) any {
                    return gatherTypes(liblists.Cons(rhs).(func(any) any)(prev).([]any)).(func(any) any)(app2)
                  }(v.Value)
                  default:
                  return liblists.Cons(lhs).(func(any) any)(liblists.Cons(rhs).(func(any) any)(prev))
                }
                return nil
              }(lhs)
            }()
          }()
        }
      }
      return func () any {
        var gatherFunctionTypes func([]any) any
        gatherFunctionTypes = func (prev []any) any {
          return func (t core.Type) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.TypeFunction:
                return func (ft core.FunctionType) any {
                  return func () any {
                    var dom any = func (v any) any {
                      return v.(core.FunctionType).Domain
                    }(ft)
                    return func () any {
                      var cod any = func (v any) any {
                        return v.(core.FunctionType).Codomain
                      }(ft)
                      return gatherFunctionTypes(liblists.Cons(dom).(func(any) any)(prev).([]any)).(func(any) any)(cod)
                    }()
                  }()
                }(v.Value)
                default:
                return liblists.Reverse(liblists.Cons(t).(func(any) any)(prev))
              }
              return nil
            }(t)
          }
        }
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeAnnotated:
            return func (at core.AnnotatedType) any {
              return Type_(func (v any) any {
                return v.(core.AnnotatedType).Body
              }(at).(core.Type))
            }(v.Value)
            case core.TypeApplication:
            return func (app core.ApplicationType) any {
              return func () any {
                var types any = gatherTypes([]any{}).(func(any) any)(app)
                return func () any {
                  var typeStrs any = liblists.Map(Type_).(func(any) any)(types)
                  return libstrings.Cat([]any{"(", libstrings.Intercalate(" @ ").(func(any) any)(typeStrs), ")"})
                }()
              }()
            }(v.Value)
            case core.TypeEither:
            return func (et core.EitherType) any {
              return func () any {
                var leftTyp any = func (v any) any {
                  return v.(core.EitherType).Left
                }(et)
                return func () any {
                  var rightTyp any = func (v any) any {
                    return v.(core.EitherType).Right
                  }(et)
                  return libstrings.Cat([]any{"either<", Type_(leftTyp.(core.Type)), ", ", Type_(rightTyp.(core.Type)), ">"})
                }()
              }()
            }(v.Value)
            case core.TypeForall:
            return func (ft core.ForallType) any {
              return func () any {
                var var_ any = func (v any) any {
                  return v.(core.ForallType).Parameter
                }(ft)
                return func () any {
                  var body any = func (v any) any {
                    return v.(core.ForallType).Body
                  }(ft)
                  return libstrings.Cat([]any{"(\u2200", var_, ".", Type_(body.(core.Type)), ")"})
                }()
              }()
            }(v.Value)
            case core.TypeFunction:
            return func (ft core.FunctionType) any {
              return func () any {
                var types any = gatherFunctionTypes([]any{}).(func(any) any)(typ)
                return func () any {
                  var typeStrs any = liblists.Map(Type_).(func(any) any)(types)
                  return libstrings.Cat([]any{"(", libstrings.Intercalate(" \u2192 ").(func(any) any)(typeStrs), ")"})
                }()
              }()
            }(v.Value)
            case core.TypeList:
            return func (etyp core.Type) any {
              return libstrings.Cat([]any{"list<", Type_(etyp), ">"})
            }(v.Value)
            case core.TypeLiteral:
            return func (lt core.LiteralType) any {
              return LiteralType(lt)
            }(v.Value)
            case core.TypeMap_:
            return func (mt core.MapType) any {
              return func () any {
                var keyTyp any = func (v any) any {
                  return v.(core.MapType).Keys
                }(mt)
                return func () any {
                  var valTyp any = func (v any) any {
                    return v.(core.MapType).Values
                  }(mt)
                  return libstrings.Cat([]any{"map<", Type_(keyTyp.(core.Type)), ", ", Type_(valTyp.(core.Type)), ">"})
                }()
              }()
            }(v.Value)
            case core.TypeMaybe:
            return func (etyp core.Type) any {
              return libstrings.Cat([]any{"maybe<", Type_(etyp), ">"})
            }(v.Value)
            case core.TypePair:
            return func (pt core.PairType) any {
              return func () any {
                var firstTyp any = func (v any) any {
                  return v.(core.PairType).First
                }(pt)
                return func () any {
                  var secondTyp any = func (v any) any {
                    return v.(core.PairType).Second
                  }(pt)
                  return libstrings.Cat([]any{"(", Type_(firstTyp.(core.Type)), ", ", Type_(secondTyp.(core.Type)), ")"})
                }()
              }()
            }(v.Value)
            case core.TypeRecord:
            return func (rt []any) any {
              return libstrings.Cat2("record").(func(any) any)(showRowType(rt))
            }(v.Value)
            case core.TypeSet:
            return func (etyp core.Type) any {
              return libstrings.Cat([]any{"set<", Type_(etyp), ">"})
            }(v.Value)
            case core.TypeUnion:
            return func (rt []any) any {
              return libstrings.Cat2("union").(func(any) any)(showRowType(rt))
            }(v.Value)
            case core.TypeUnit:
            return func (_ struct{}) any {
              return "unit"
            }(v)
            case core.TypeVariable:
            return func (name core.Name) any {
              return func (v any) any {
                return v
              }(name)
            }(v.Value)
            case core.TypeWrap:
            return func (wt core.Type) any {
              return libstrings.Cat([]any{"wrap(", Type_(wt), ")"})
            }(v.Value)
          }
          return nil
        }(typ)
      }()
    }()
  }().(string)
}

func TypeScheme (ts core.TypeScheme) string {
  return func () any {
    var vars any = func (v any) any {
      return v.(core.TypeScheme).Variables
    }(ts)
    return func () any {
      var body any = func (v any) any {
        return v.(core.TypeScheme).Type_
      }(ts)
      return func () any {
        var varNames any = liblists.Map(func (v any) any {
          return v
        }).(func(any) any)(vars)
        return func () any {
          var fa any = liblogic.IfElse(liblists.Null(vars)).(func(any) any)("").(func(any) any)(libstrings.Cat([]any{"forall ", libstrings.Intercalate(",").(func(any) any)(varNames), ". "}))
          return func () any {
            toConstraintPair := func (v core.Name) any {
              return func (c core.Name) any {
                return libstrings.Cat([]any{func (v any) any {
                  return v
                }(c), " ", func (v any) any {
                  return v
                }(v)})
              }
            }
            return func () any {
              toConstraintPairs := func (p any) any {
                return liblists.Map(func (v1 core.Name) any {
                  return toConstraintPair(libpairs.First(p).(core.Name)).(func(any) any)(v1)
                }).(func(any) any)(libsets.ToList(libpairs.Second(p).(core.TypeVariableMetadata).Classes))
              }
              return func () any {
                var tc any = libmaybes.Maybe([]any{}).(func(any) any)(func (m []any) any {
                  return liblists.Concat(liblists.Map(toConstraintPairs).(func(any) any)(libmaps.ToList(m)))
                }).(func(any) any)(func (v any) any {
                  return v.(core.TypeScheme).Constraints
                }(ts))
                return libstrings.Cat([]any{"(", fa, liblogic.IfElse(liblists.Null(tc)).(func(any) any)("").(func(any) any)(libstrings.Cat([]any{"(", libstrings.Intercalate(", ").(func(any) any)(tc), ") => "})), Type_(body.(core.Type)), ")"})
              }()
            }()
          }()
        }()
      }()
    }()
  }().(string)
}
