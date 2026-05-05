// Note: this is an automatically generated file. Do not edit.

package checking

import (
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/constants"
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  extractcore "hydra.dev/hydra/extract/core"
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
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/reflect"
  "hydra.dev/hydra/rewriting"
  "hydra.dev/hydra/schemas"
  showcore "hydra.dev/hydra/show/core"
  "hydra.dev/hydra/substitution"
  "hydra.dev/hydra/typing"
)

func AllEqual (els []any) bool {
  return liblogic.IfElse(liblists.Null(els)).(func(any) any)(true).(func(any) any)(liblists.Foldl(func (b bool) any {
    return func (t any) any {
      return liblogic.And(b).(func(any) any)(libequality.Equal(t).(func(any) any)(liblists.Head(els)))
    }
  }).(func(any) any)(true).(func(any) any)(liblists.Tail(els))).(bool)
}

func ApplyTypeArgumentsToType[T0 any] (cx context.Context, tx T0, typeArgs []any, t core.Type) any {
  return func () any {
    var nonnull any = func (x any) any {
      switch v := x.(type) {
        case core.TypeForall:
        return func (ft core.ForallType) any {
          return func () any {
            var v any = func (v any) any {
              return v.(core.ForallType).Parameter
            }(ft)
            return func () any {
              var tbody any = func (v any) any {
                return v.(core.ForallType).Body
              }(ft)
              return ApplyTypeArgumentsToType[T0](cx, tx, liblists.Tail(typeArgs).([]any), substitution.SubstInType(typing.TypeSubst(libmaps.Singleton(v).(func(any) any)(liblists.Head(typeArgs)).([]any)), tbody.(core.Type)))
            }()
          }()
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorNotAForallType{Value: error.NotAForallTypeError{Type_: t, TypeArguments: typeArgs}}}, Context: cx}}
      }
      return nil
    }(t)
    return liblogic.IfElse(liblists.Null(typeArgs)).(func(any) any)([2]any{"right", t}).(func(any) any)(nonnull)
  }()
}

func CheckForUnboundTypeVariables (cx context.Context, tx graph.Graph, term0 core.Term) any {
  return func () any {
    var svars any = libsets.FromList(libmaps.Keys(func (v any) any {
      return v.(graph.Graph).SchemaTypes
    }(tx)))
    return func () any {
      var checkRecursive func([]any) any
      checkRecursive = func (vars []any) any {
        return func (trace []any) any {
          return func (lbinding any) any {
            return func (term core.Term) any {
              return func () any {
                recurse := func (v1 core.Term) any {
                  return checkRecursive(vars).(func(any) any)(trace).(func(any) any)(lbinding).(func(any) any)(v1)
                }
                return func () any {
                  var dflt any = libeithers.Bind(libeithers.MapList(recurse).(func(any) any)(rewriting.Subterms(term))).(func(any) any)(func (_ []any) any {
                    return [2]any{"right", struct{}{}}
                  })
                  return func () any {
                    check := func (typ core.Type) any {
                      return func () any {
                        var freevars any = rewriting.FreeVariablesInType(typ)
                        return func () any {
                          var badvars any = libsets.Difference(libsets.Difference(freevars).(func(any) any)(vars)).(func(any) any)(svars)
                          return liblogic.IfElse(libsets.Null(badvars)).(func(any) any)([2]any{"right", struct{}{}}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorUnboundTypeVariables{Value: error.UnboundTypeVariablesError{Variables: badvars.([]any), Type_: typ}}}, Context: cx}})
                        }()
                      }()
                    }
                    return func () any {
                      checkOptional := func (m any) any {
                        return libeithers.Bind(libeithers.MapMaybe(check).(func(any) any)(m)).(func(any) any)(func (_ any) any {
                          return [2]any{"right", struct{}{}}
                        })
                      }
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TermFunction:
                          return func (f core.Function) any {
                            return func (x any) any {
                              switch v := x.(type) {
                                case core.FunctionElimination:
                                return func (e core.Elimination) any {
                                  return dflt
                                }(v.Value)
                                case core.FunctionLambda:
                                return func (l core.Lambda) any {
                                  return libeithers.Bind(checkOptional(func (v any) any {
                                    return v.(core.Lambda).Domain
                                  }(l))).(func(any) any)(func (_ struct{}) any {
                                    return recurse(func (v any) any {
                                      return v.(core.Lambda).Body
                                    }(l).(core.Term))
                                  })
                                }(v.Value)
                                default:
                                return dflt
                              }
                              return nil
                            }(f)
                          }(v.Value)
                          case core.TermLet:
                          return func (l core.Let) any {
                            return func () any {
                              forBinding := func (b core.Binding) any {
                                return func () any {
                                  var bterm any = func (v any) any {
                                    return v.(core.Binding).Term
                                  }(b)
                                  return func () any {
                                    var newVars any = libmaybes.Maybe(vars).(func(any) any)(func (ts core.TypeScheme) any {
                                      return libsets.Union(vars).(func(any) any)(libsets.FromList(func (v any) any {
                                        return v.(core.TypeScheme).Variables
                                      }(ts)))
                                    }).(func(any) any)(func (v any) any {
                                      return v.(core.Binding).Type_
                                    }(b))
                                    return func () any {
                                      var newTrace any = liblists.Cons(func (v any) any {
                                        return v.(core.Binding).Name
                                      }(b)).(func(any) any)(trace)
                                      return checkRecursive(newVars.([]any)).(func(any) any)(newTrace).(func(any) any)(func () any {
                                        _v := b
                                        return &_v
                                      }()).(func(any) any)(bterm)
                                    }()
                                  }()
                                }()
                              }
                              return libeithers.Bind(libeithers.MapList(forBinding).(func(any) any)(func (v any) any {
                                return v.(core.Let).Bindings
                              }(l))).(func(any) any)(func (_ []any) any {
                                return recurse(func (v any) any {
                                  return v.(core.Let).Body
                                }(l).(core.Term))
                              })
                            }()
                          }(v.Value)
                          case core.TermTypeApplication:
                          return func (tt core.TypeApplicationTerm) any {
                            return libeithers.Bind(check(func (v any) any {
                              return v.(core.TypeApplicationTerm).Type_
                            }(tt).(core.Type))).(func(any) any)(func (_ struct{}) any {
                              return recurse(func (v any) any {
                                return v.(core.TypeApplicationTerm).Body
                              }(tt).(core.Term))
                            })
                          }(v.Value)
                          case core.TermTypeLambda:
                          return func (tl core.TypeLambda) any {
                            return libeithers.Bind(check(core.TypeVariable{Value: func (v any) any {
                              return v.(core.TypeLambda).Parameter
                            }(tl).(core.Name)})).(func(any) any)(func (_ struct{}) any {
                              return recurse(func (v any) any {
                                return v.(core.TypeLambda).Body
                              }(tl).(core.Term))
                            })
                          }(v.Value)
                          default:
                          return dflt
                        }
                        return nil
                      }(term)
                    }()
                  }()
                }()
              }()
            }
          }
        }
      }
      return checkRecursive(libsets.Empty).(func(any) any)([]any{"top level"}).(func(any) any)(nil).(func(any) any)(term0)
    }()
  }()
}

func CheckNominalApplication (cx context.Context, tx graph.Graph, tname core.Name, typeArgs []any) any {
  return libeithers.Bind(schemas.RequireSchemaType(cx, func (v any) any {
    return v.(graph.Graph).SchemaTypes
  }(tx).([]any), tname)).(func(any) any)(func (result any) any {
    return func () any {
      var schemaType any = libpairs.First(result)
      return func () any {
        var cx2 any = libpairs.Second(result)
        return func () any {
          var vars any = schemaType.(core.TypeScheme).Variables
          return func () any {
            var varslen any = liblists.Length(vars)
            return func () any {
              var argslen any = liblists.Length(typeArgs)
              return liblogic.IfElse(libequality.Equal(varslen).(func(any) any)(argslen)).(func(any) any)([2]any{"right", [2]any{struct{}{}, cx2}}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorTypeArityMismatch{Value: error.TypeArityMismatchError{Type_: core.TypeVariable{Value: tname}, ExpectedArity: varslen.(int32), ActualArity: argslen.(int32), TypeArguments: typeArgs}}}, Context: cx2.(context.Context)}})
            }()
          }()
        }()
      }()
    }()
  })
}

func CheckSameType (cx context.Context, tx graph.Graph, desc string, types []any) any {
  return liblogic.IfElse(TypesAllEffectivelyEqual(tx, types)).(func(any) any)([2]any{"right", liblists.Head(types)}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorUnequalTypes{Value: error.UnequalTypesError{Types: types, Description: desc}}}, Context: cx}})
}

func CheckType (cx context.Context, tx graph.Graph, term core.Term, typ core.Type) any {
  return func () any {
    var vars any = func (v any) any {
      return v.(graph.Graph).TypeVariables
    }(tx)
    return liblogic.IfElse(constants.DebugInference).(func(any) any)(libeithers.Bind(libeithers.Map(func (_p any) any {
      return libpairs.First(_p)
    }).(func(any) any)(TypeOf(cx, tx, []any{}, term))).(func(any) any)(func (t0 core.Type) any {
      return liblogic.IfElse(TypesEffectivelyEqual(tx, t0, typ)).(func(any) any)([2]any{"right", struct{}{}}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorTypeMismatch{Value: error.TypeMismatchError{ExpectedType: typ, ActualType: t0}}}, Context: cx}})
    })).(func(any) any)([2]any{"right", struct{}{}})
  }()
}

func CheckTypeSubst (cx context.Context, tx graph.Graph, subst typing.TypeSubst) any {
  return func () any {
    var s any = func (v any) any {
      return v
    }(subst)
    return func () any {
      var vars any = libsets.FromList(libmaps.Keys(s))
      return func () any {
        var suspectVars any = libsets.Intersection(vars).(func(any) any)(libsets.FromList(libmaps.Keys(func (v any) any {
          return v.(graph.Graph).SchemaTypes
        }(tx))))
        return func () any {
          isNominal := func (ts core.TypeScheme) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.TypeRecord:
                return func (_ []any) any {
                  return true
                }(v.Value)
                case core.TypeUnion:
                return func (_ []any) any {
                  return true
                }(v.Value)
                case core.TypeWrap:
                return func (_ core.Type) any {
                  return true
                }(v.Value)
                default:
                return false
              }
              return nil
            }(rewriting.DeannotateType(func (v any) any {
              return v.(core.TypeScheme).Type_
            }(ts).(core.Type)))
          }
          return func () any {
            var badVars any = libsets.FromList(liblists.Filter(func (v core.Name) any {
              return libmaybes.Maybe(false).(func(any) any)(isNominal).(func(any) any)(lexical.DereferenceSchemaType(v, func (v any) any {
                return v.(graph.Graph).SchemaTypes
              }(tx).([]any)))
            }).(func(any) any)(libsets.ToList(suspectVars)))
            return func () any {
              var badPairs any = liblists.Filter(func (p any) any {
                return libsets.Member(libpairs.First(p)).(func(any) any)(badVars)
              }).(func(any) any)(libmaps.ToList(s))
              return func () any {
                printPair := func (p any) any {
                  return libstrings.Cat2(libstrings.Cat2(libpairs.First(p)).(func(any) any)(" --> ")).(func(any) any)(showcore.Type_(libpairs.Second(p).(core.Type)))
                }
                return liblogic.IfElse(libsets.Null(badVars)).(func(any) any)([2]any{"right", subst}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorIncorrectUnification{Value: error.IncorrectUnificationError{Substitution: subst}}}, Context: cx}})
              }()
            }()
          }()
        }()
      }()
    }()
  }()
}

func CheckTypeVariables[T0, T1 any] (_tx T0, _typ T1) struct{} {
  return struct{}{}
}

func ContainsInScopeTypeVars (tx graph.Graph, t core.Type) bool {
  return func () any {
    var vars any = func (v any) any {
      return v.(graph.Graph).TypeVariables
    }(tx)
    return func () any {
      var freeVars any = rewriting.FreeVariablesInTypeSimple(t)
      return liblogic.Not(libsets.Null(libsets.Intersection(vars).(func(any) any)(freeVars)))
    }()
  }().(bool)
}

func NormalizeTypeFreeVars (typ core.Type) core.Type {
  return func () any {
    collectVars := func (acc []any) any {
      return func (t core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeVariable:
            return func (v core.Name) any {
              return liblogic.IfElse(libmaps.Member(v).(func(any) any)(acc)).(func(any) any)(acc).(func(any) any)(libmaps.Insert(v).(func(any) any)(core.Name(libstrings.Cat2("_tv").(func(any) any)(libliterals.ShowInt32(libmaps.Size(acc))).(string))).(func(any) any)(acc))
            }(v.Value)
            default:
            return acc
          }
          return nil
        }(t)
      }
    }
    return func () any {
      var subst any = rewriting.FoldOverType(coders.TraversalOrderPre{}, func (_p any) func(core.Type) any {
        return collectVars(_p.([]any)).(func(core.Type) any)
      }, libmaps.Empty, typ)
      return rewriting.SubstituteTypeVariables(subst.([]any), typ)
    }()
  }().(core.Type)
}

func ToFContext (cx graph.Graph) []any {
  return libmaps.Map(rewriting.TypeSchemeToFType).(func(any) any)(func (v any) any {
    return v.(graph.Graph).BoundTypes
  }(cx)).([]any)
}

func TypeListsEffectivelyEqual (tx graph.Graph, tlist1 []any, tlist2 []any) bool {
  return liblogic.IfElse(libequality.Equal(liblists.Length(tlist1)).(func(any) any)(liblists.Length(tlist2))).(func(any) any)(liblists.Foldl(liblogic.And).(func(any) any)(true).(func(any) any)(liblists.ZipWith(func (v1 core.Type) any {
    return func (v2 core.Type) any {
      return TypesEffectivelyEqual(tx, v1, v2)
    }
  }).(func(any) any)(tlist1).(func(any) any)(tlist2))).(func(any) any)(false).(bool)
}

func TypeOf (cx context.Context, tx graph.Graph, typeArgs []any, term core.Term) any {
  return func () any {
    var cx1 any = context.Context{Trace: liblists.Cons("typeOf").(func(any) any)(func (v any) any {
      return v.(context.Context).Trace
    }(cx)).([]any), Messages: func (v any) any {
      return v.(context.Context).Messages
    }(cx).([]any), Other: func (v any) any {
      return v.(context.Context).Other
    }(cx).([]any)}
    return func (x any) any {
      switch v := x.(type) {
        case core.TermAnnotated:
        return func (v1 core.AnnotatedTerm) any {
          return TypeOfAnnotatedTerm(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermApplication:
        return func (v1 core.Application) any {
          return TypeOfApplication(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermEither:
        return func (v1 any) any {
          return TypeOfEither(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermFunction:
        return func (f core.Function) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.FunctionElimination:
              return func (elm core.Elimination) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.EliminationRecord:
                    return func (v1 core.Projection) any {
                      return TypeOfProjection(cx1.(context.Context), tx, typeArgs, v1)
                    }(v.Value)
                    case core.EliminationUnion:
                    return func (v1 core.CaseStatement) any {
                      return TypeOfCaseStatement(cx1.(context.Context), tx, typeArgs, v1)
                    }(v.Value)
                    case core.EliminationWrap:
                    return func (v1 core.Name) any {
                      return TypeOfUnwrap(cx1.(context.Context), tx, typeArgs, v1)
                    }(v.Value)
                  }
                  return nil
                }(elm)
              }(v.Value)
              case core.FunctionLambda:
              return func (v1 core.Lambda) any {
                return TypeOfLambda(cx1.(context.Context), tx, typeArgs, v1)
              }(v.Value)
              case core.FunctionPrimitive:
              return func (v1 core.Name) any {
                return TypeOfPrimitive(cx1.(context.Context), tx, typeArgs, v1)
              }(v.Value)
            }
            return nil
          }(f)
        }(v.Value)
        case core.TermLet:
        return func (v1 core.Let) any {
          return TypeOfLet(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermList:
        return func (v1 []any) any {
          return TypeOfList(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermLiteral:
        return func (v1 core.Literal) any {
          return TypeOfLiteral[graph.Graph](cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermMap_:
        return func (v1 []any) any {
          return TypeOfMap(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermMaybe:
        return func (v1 any) any {
          return TypeOfMaybe(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermPair:
        return func (v1 any) any {
          return TypeOfPair(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermRecord:
        return func (v1 core.Record) any {
          return TypeOfRecord(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermSet:
        return func (v1 []any) any {
          return TypeOfSet(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermTypeApplication:
        return func (v1 core.TypeApplicationTerm) any {
          return TypeOfTypeApplication(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermTypeLambda:
        return func (v1 core.TypeLambda) any {
          return TypeOfTypeLambda(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermUnion:
        return func (v1 core.Injection) any {
          return TypeOfInjection(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermUnit:
        return func (_ struct{}) any {
          return TypeOfUnit[graph.Graph](cx1.(context.Context), tx, typeArgs)
        }(v)
        case core.TermVariable:
        return func (v1 core.Name) any {
          return TypeOfVariable(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        case core.TermWrap:
        return func (v1 core.WrappedTerm) any {
          return TypeOfWrappedTerm(cx1.(context.Context), tx, typeArgs, v1)
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorUnsupportedTermVariant{Value: error.UnsupportedTermVariantError{TermVariant: reflect.TermVariant(term)}}}, Context: cx1.(context.Context)}}
      }
      return nil
    }(term)
  }()
}

func TypeOfAnnotatedTerm (cx context.Context, tx graph.Graph, typeArgs []any, at core.AnnotatedTerm) any {
  return TypeOf(cx, tx, typeArgs, func (v any) any {
    return v.(core.AnnotatedTerm).Body
  }(at).(core.Term))
}

func TypeOfApplication (cx context.Context, tx graph.Graph, typeArgs []any, app core.Application) any {
  return func () any {
    var fun any = func (v any) any {
      return v.(core.Application).Function
    }(app)
    return func () any {
      var arg any = func (v any) any {
        return v.(core.Application).Argument
      }(app)
      return func () any {
        var tryType func(context.Context) any
        tryType = func (cx0 context.Context) any {
          return func (tfun core.Type) any {
            return func (targ core.Type) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.TypeForall:
                  return func (ft core.ForallType) any {
                    return tryType(cx0).(func(any) any)(func (v any) any {
                      return v.(core.ForallType).Body
                    }(ft)).(func(any) any)(targ)
                  }(v.Value)
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
                        return liblogic.IfElse(TypesEffectivelyEqual(tx, dom.(core.Type), targ)).(func(any) any)([2]any{"right", [2]any{cod, cx0}}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorTypeMismatch{Value: error.TypeMismatchError{ExpectedType: dom.(core.Type), ActualType: targ}}}, Context: cx0}})
                      }()
                    }()
                  }(v.Value)
                  case core.TypeVariable:
                  return func (v core.Name) any {
                    return func () any {
                      var nameResult any = schemas.FreshName(cx0)
                      return func () any {
                        var freshN any = libpairs.First(nameResult)
                        return func () any {
                          var cx1 any = libpairs.Second(nameResult)
                          return [2]any{"right", [2]any{core.TypeVariable{Value: freshN.(core.Name)}, cx1}}
                        }()
                      }()
                    }()
                  }(v.Value)
                  default:
                  return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorNotAFunctionType{Value: error.NotAFunctionTypeError{Type_: tfun}}}, Context: cx0}}
                }
                return nil
              }(tfun)
            }
          }
        }
        return libeithers.Bind(TypeOf(cx, tx, []any{}, fun.(core.Term))).(func(any) any)(func (result1 any) any {
          return func () any {
            var tfun any = libpairs.First(result1)
            return func () any {
              var cx2 any = libpairs.Second(result1)
              return libeithers.Bind(TypeOf(cx2.(context.Context), tx, []any{}, arg.(core.Term))).(func(any) any)(func (result2 any) any {
                return func () any {
                  var targ any = libpairs.First(result2)
                  return func () any {
                    var cx3 any = libpairs.Second(result2)
                    return libeithers.Bind(tryType(cx3.(context.Context)).(func(any) any)(tfun).(func(any) any)(targ)).(func(any) any)(func (result3 any) any {
                      return func () any {
                        var t any = libpairs.First(result3)
                        return func () any {
                          var cx4 any = libpairs.Second(result3)
                          return libeithers.Bind(ApplyTypeArgumentsToType[graph.Graph](cx4.(context.Context), tx, typeArgs, t.(core.Type))).(func(any) any)(func (applied core.Type) any {
                            return [2]any{"right", [2]any{applied, cx4}}
                          })
                        }()
                      }()
                    })
                  }()
                }()
              })
            }()
          }()
        })
      }()
    }()
  }()
}

func TypeOfCaseStatement (cx context.Context, tx graph.Graph, typeArgs []any, cs core.CaseStatement) any {
  return func () any {
    var tname any = func (v any) any {
      return v.(core.CaseStatement).TypeName
    }(cs)
    return func () any {
      var dflt any = func (v any) any {
        return v.(core.CaseStatement).Default_
      }(cs)
      return func () any {
        var cases any = func (v any) any {
          return v.(core.CaseStatement).Cases
        }(cs)
        return func () any {
          var cterms any = liblists.Map(func (v any) any {
            return v.(core.Field).Term
          }).(func(any) any)(cases)
          return libeithers.Bind(libeithers.MapMaybe(func (e core.Term) any {
            return TypeOf(cx, tx, []any{}, e)
          }).(func(any) any)(dflt)).(func(any) any)(func (dfltResult any) any {
            return func () any {
              var tdflt any = libmaybes.Map(libpairs.First).(func(any) any)(dfltResult)
              return func () any {
                var cx2 any = libmaybes.Maybe(cx).(func(any) any)(libpairs.Second).(func(any) any)(dfltResult)
                return func () any {
                  var foldResult any = liblists.Foldl(func (acc any) any {
                    return func (term core.Term) any {
                      return libeithers.Bind(acc).(func(any) any)(func (accR any) any {
                        return func () any {
                          var types any = libpairs.First(accR)
                          return func () any {
                            var cxA any = libpairs.Second(accR)
                            return libeithers.Bind(TypeOf(cxA.(context.Context), tx, []any{}, term)).(func(any) any)(func (tResult any) any {
                              return func () any {
                                var t any = libpairs.First(tResult)
                                return func () any {
                                  var cxB any = libpairs.Second(tResult)
                                  return [2]any{"right", [2]any{liblists.Concat2(types).(func(any) any)(liblists.Pure(t)), cxB}}
                                }()
                              }()
                            })
                          }()
                        }()
                      })
                    }
                  }).(func(any) any)([2]any{"right", [2]any{[]any{}, cx2}}).(func(any) any)(cterms)
                  return libeithers.Bind(foldResult).(func(any) any)(func (foldR any) any {
                    return func () any {
                      var tcterms any = libpairs.First(foldR)
                      return func () any {
                        var cx3 any = libpairs.Second(foldR)
                        return func () any {
                          var fcodsResult any = liblists.Foldl(func (acc any) any {
                            return func (t core.Type) any {
                              return libeithers.Bind(acc).(func(any) any)(func (accR any) any {
                                return func () any {
                                  var cods any = libpairs.First(accR)
                                  return libeithers.Bind(extractcore.FunctionType(cx3.(context.Context), t)).(func(any) any)(func (ft core.FunctionType) any {
                                    return [2]any{"right", [2]any{liblists.Concat2(cods).(func(any) any)(liblists.Pure(func (v any) any {
                                      return v.(core.FunctionType).Codomain
                                    }(ft))), cx3}}
                                  })
                                }()
                              })
                            }
                          }).(func(any) any)([2]any{"right", [2]any{[]any{}, cx3}}).(func(any) any)(tcterms)
                          return libeithers.Bind(fcodsResult).(func(any) any)(func (fcodsR any) any {
                            return func () any {
                              var fcods any = libpairs.First(fcodsR)
                              return func () any {
                                var cods any = libmaybes.Cat(liblists.Cons(tdflt).(func(any) any)(liblists.Map(libmaybes.Pure).(func(any) any)(fcods)))
                                return libeithers.Bind(CheckSameType(cx3.(context.Context), tx, "case branches", cods.([]any))).(func(any) any)(func (cod core.Type) any {
                                  return [2]any{"right", [2]any{core.TypeFunction{Value: core.FunctionType{Domain: schemas.NominalApplication(tname.(core.Name), typeArgs), Codomain: cod}}, cx3}}
                                })
                              }()
                            }()
                          })
                        }()
                      }()
                    }()
                  })
                }()
              }()
            }()
          })
        }()
      }()
    }()
  }()
}

func TypeOfEither (cx context.Context, tx graph.Graph, typeArgs []any, et any) any {
  return func () any {
    var n any = liblists.Length(typeArgs)
    return liblogic.IfElse(libequality.Equal(n).(func(any) any)(2)).(func(any) any)(libeithers.Either(func (leftTerm core.Term) any {
      return libeithers.Bind(TypeOf(cx, tx, []any{}, leftTerm)).(func(any) any)(func (result any) any {
        return func () any {
          var leftType any = libpairs.First(result)
          return func () any {
            var cx2 any = libpairs.Second(result)
            return [2]any{"right", [2]any{core.TypeEither{Value: core.EitherType{Left: leftType.(core.Type), Right: liblists.At(1).(func(any) any)(typeArgs).(core.Type)}}, cx2}}
          }()
        }()
      })
    }).(func(any) any)(func (rightTerm core.Term) any {
      return libeithers.Bind(TypeOf(cx, tx, []any{}, rightTerm)).(func(any) any)(func (result any) any {
        return func () any {
          var rightType any = libpairs.First(result)
          return func () any {
            var cx2 any = libpairs.Second(result)
            return [2]any{"right", [2]any{core.TypeEither{Value: core.EitherType{Left: liblists.At(0).(func(any) any)(typeArgs).(core.Type), Right: rightType.(core.Type)}}, cx2}}
          }()
        }()
      })
    }).(func(any) any)(et)).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorTypeArityMismatch{Value: error.TypeArityMismatchError{Type_: core.TypeEither{Value: core.EitherType{Left: core.TypeUnit{}, Right: core.TypeUnit{}}}, ExpectedArity: 2, ActualArity: n.(int32), TypeArguments: typeArgs}}}, Context: cx}})
  }()
}

func TypeOfInjection (cx context.Context, tx graph.Graph, typeArgs []any, injection core.Injection) any {
  return func () any {
    var tname any = func (v any) any {
      return v.(core.Injection).TypeName
    }(injection)
    return func () any {
      var field any = func (v any) any {
        return v.(core.Injection).Field
      }(injection)
      return func () any {
        var fname any = field.(core.Field).Name
        return func () any {
          var fterm any = field.(core.Field).Term
          return libeithers.Bind(schemas.RequireSchemaType(cx, func (v any) any {
            return v.(graph.Graph).SchemaTypes
          }(tx).([]any), tname.(core.Name))).(func(any) any)(func (schemaResult any) any {
            return func () any {
              var schemaType any = libpairs.First(schemaResult)
              return func () any {
                var cx2 any = libpairs.Second(schemaResult)
                return func () any {
                  var svars any = schemaType.(core.TypeScheme).Variables
                  return func () any {
                    var sbody any = schemaType.(core.TypeScheme).Type_
                    return libeithers.Bind(extractcore.UnionType[core.Name](cx2.(context.Context), tname, sbody.(core.Type))).(func(any) any)(func (sfields []any) any {
                      return libeithers.Bind(schemas.FindFieldType(cx2.(context.Context), fname.(core.Name), sfields)).(func(any) any)(func (ftyp core.Type) any {
                        return [2]any{"right", [2]any{schemas.NominalApplication(tname.(core.Name), typeArgs), cx2}}
                      })
                    })
                  }()
                }()
              }()
            }()
          })
        }()
      }()
    }()
  }()
}

func TypeOfLambda (cx context.Context, tx graph.Graph, typeArgs []any, l core.Lambda) any {
  return func () any {
    var v any = func (v any) any {
      return v.(core.Lambda).Parameter
    }(l)
    return func () any {
      var mdom any = func (v any) any {
        return v.(core.Lambda).Domain
      }(l)
      return func () any {
        var body any = func (v any) any {
          return v.(core.Lambda).Body
        }(l)
        return libeithers.Bind(libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorUntypedLambda{}}, Context: cx}}).(func(any) any)(func (dom core.Type) any {
          return func () any {
            var types2 any = libmaps.Insert(v).(func(any) any)(rewriting.FTypeToTypeScheme(dom)).(func(any) any)(func (v any) any {
              return v.(graph.Graph).BoundTypes
            }(tx))
            return libeithers.Bind(TypeOf(cx, graph.Graph{BoundTerms: func (v any) any {
              return v.(graph.Graph).BoundTerms
            }(tx).([]any), BoundTypes: types2.([]any), ClassConstraints: func (v any) any {
              return v.(graph.Graph).ClassConstraints
            }(tx).([]any), LambdaVariables: func (v any) any {
              return v.(graph.Graph).LambdaVariables
            }(tx).([]any), Metadata: func (v any) any {
              return v.(graph.Graph).Metadata
            }(tx).([]any), Primitives: func (v any) any {
              return v.(graph.Graph).Primitives
            }(tx).([]any), SchemaTypes: func (v any) any {
              return v.(graph.Graph).SchemaTypes
            }(tx).([]any), TypeVariables: func (v any) any {
              return v.(graph.Graph).TypeVariables
            }(tx).([]any)}, []any{}, body.(core.Term))).(func(any) any)(func (codResult any) any {
              return func () any {
                var cod any = libpairs.First(codResult)
                return func () any {
                  var cx2 any = libpairs.Second(codResult)
                  return [2]any{"right", [2]any{core.TypeFunction{Value: core.FunctionType{Domain: dom, Codomain: cod.(core.Type)}}, cx2}}
                }()
              }()
            })
          }()
        }).(func(any) any)(mdom)).(func(any) any)(func (tbodyResult any) any {
          return func () any {
            var tbody any = libpairs.First(tbodyResult)
            return func () any {
              var cx3 any = libpairs.Second(tbodyResult)
              return libeithers.Bind(ApplyTypeArgumentsToType[graph.Graph](cx3.(context.Context), tx, typeArgs, tbody.(core.Type))).(func(any) any)(func (applied core.Type) any {
                return [2]any{"right", [2]any{applied, cx3}}
              })
            }()
          }()
        })
      }()
    }()
  }()
}

func TypeOfLet (cx context.Context, tx graph.Graph, typeArgs []any, letTerm core.Let) any {
  return func () any {
    var bs any = func (v any) any {
      return v.(core.Let).Bindings
    }(letTerm)
    return func () any {
      var body any = func (v any) any {
        return v.(core.Let).Body
      }(letTerm)
      return func () any {
        var bnames any = liblists.Map(func (v any) any {
          return v.(core.Binding).Name
        }).(func(any) any)(bs)
        return func () any {
          bindingType := func (b core.Binding) any {
            return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorUntypedLetBinding{Value: error.UntypedLetBindingError{Binding: b}}}, Context: cx}}).(func(any) any)(func (ts core.TypeScheme) any {
              return [2]any{"right", rewriting.TypeSchemeToFType(ts)}
            }).(func(any) any)(func (v any) any {
              return v.(core.Binding).Type_
            }(b))
          }
          return func () any {
            var btypesResult any = liblists.Foldl(func (acc any) any {
              return func (b core.Binding) any {
                return libeithers.Bind(acc).(func(any) any)(func (accR any) any {
                  return func () any {
                    var types any = libpairs.First(accR)
                    return libeithers.Bind(bindingType(b)).(func(any) any)(func (btype core.Type) any {
                      return [2]any{"right", [2]any{liblists.Concat2(types).(func(any) any)(liblists.Pure(btype)), struct{}{}}}
                    })
                  }()
                })
              }
            }).(func(any) any)([2]any{"right", [2]any{[]any{}, struct{}{}}}).(func(any) any)(bs)
            return libeithers.Bind(btypesResult).(func(any) any)(func (btypesR any) any {
              return func () any {
                var btypes any = libpairs.First(btypesR)
                return func () any {
                  var tx2 any = graph.Graph{BoundTerms: func (v any) any {
                    return v.(graph.Graph).BoundTerms
                  }(tx).([]any), BoundTypes: libmaps.Union(libmaps.FromList(liblists.Zip(bnames).(func(any) any)(liblists.Map(rewriting.FTypeToTypeScheme).(func(any) any)(btypes)))).(func(any) any)(func (v any) any {
                    return v.(graph.Graph).BoundTypes
                  }(tx)).([]any), ClassConstraints: func (v any) any {
                    return v.(graph.Graph).ClassConstraints
                  }(tx).([]any), LambdaVariables: func (v any) any {
                    return v.(graph.Graph).LambdaVariables
                  }(tx).([]any), Metadata: func (v any) any {
                    return v.(graph.Graph).Metadata
                  }(tx).([]any), Primitives: func (v any) any {
                    return v.(graph.Graph).Primitives
                  }(tx).([]any), SchemaTypes: func (v any) any {
                    return v.(graph.Graph).SchemaTypes
                  }(tx).([]any), TypeVariables: func (v any) any {
                    return v.(graph.Graph).TypeVariables
                  }(tx).([]any)}
                  return libeithers.Bind(TypeOf(cx, tx2.(graph.Graph), []any{}, body.(core.Term))).(func(any) any)(func (tResult any) any {
                    return func () any {
                      var t any = libpairs.First(tResult)
                      return func () any {
                        var cx2 any = libpairs.Second(tResult)
                        return libeithers.Bind(ApplyTypeArgumentsToType[graph.Graph](cx2.(context.Context), tx, typeArgs, t.(core.Type))).(func(any) any)(func (applied core.Type) any {
                          return [2]any{"right", [2]any{applied, cx2}}
                        })
                      }()
                    }()
                  })
                }()
              }()
            })
          }()
        }()
      }()
    }()
  }()
}

func TypeOfList (cx context.Context, tx graph.Graph, typeArgs []any, els []any) any {
  return liblogic.IfElse(liblists.Null(els)).(func(any) any)(liblogic.IfElse(libequality.Equal(liblists.Length(typeArgs)).(func(any) any)(1)).(func(any) any)([2]any{"right", [2]any{core.TypeList{Value: liblists.Head(typeArgs).(core.Type)}, cx}}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorTypeArityMismatch{Value: error.TypeArityMismatchError{Type_: core.TypeList{Value: core.TypeUnit{}}, ExpectedArity: 1, ActualArity: liblists.Length(typeArgs).(int32), TypeArguments: typeArgs}}}, Context: cx}})).(func(any) any)(func () any {
    var foldResult any = liblists.Foldl(func (acc any) any {
      return func (term core.Term) any {
        return libeithers.Bind(acc).(func(any) any)(func (accR any) any {
          return func () any {
            var types any = libpairs.First(accR)
            return func () any {
              var cxA any = libpairs.Second(accR)
              return libeithers.Bind(TypeOf(cxA.(context.Context), tx, []any{}, term)).(func(any) any)(func (tResult any) any {
                return func () any {
                  var t any = libpairs.First(tResult)
                  return func () any {
                    var cxB any = libpairs.Second(tResult)
                    return [2]any{"right", [2]any{liblists.Concat2(types).(func(any) any)(liblists.Pure(t)), cxB}}
                  }()
                }()
              })
            }()
          }()
        })
      }
    }).(func(any) any)([2]any{"right", [2]any{[]any{}, cx}}).(func(any) any)(els)
    return libeithers.Bind(foldResult).(func(any) any)(func (foldR any) any {
      return func () any {
        var eltypes any = libpairs.First(foldR)
        return func () any {
          var cx2 any = libpairs.Second(foldR)
          return libeithers.Bind(CheckSameType(cx2.(context.Context), tx, "list elements", eltypes.([]any))).(func(any) any)(func (unifiedType core.Type) any {
            return [2]any{"right", [2]any{core.TypeList{Value: unifiedType}, cx2}}
          })
        }()
      }()
    })
  }())
}

func TypeOfLiteral[T0 any] (cx context.Context, tx T0, typeArgs []any, lit core.Literal) any {
  return func () any {
    var t any = core.TypeLiteral{Value: reflect.LiteralType(lit)}
    return libeithers.Bind(ApplyTypeArgumentsToType[T0](cx, tx, typeArgs, t.(core.Type))).(func(any) any)(func (applied core.Type) any {
      return [2]any{"right", [2]any{applied, cx}}
    })
  }()
}

func TypeOfMap (cx context.Context, tx graph.Graph, typeArgs []any, m []any) any {
  return liblogic.IfElse(libmaps.Null(m)).(func(any) any)(liblogic.IfElse(libequality.Equal(liblists.Length(typeArgs)).(func(any) any)(2)).(func(any) any)([2]any{"right", [2]any{core.TypeMap_{Value: core.MapType{Keys: liblists.At(0).(func(any) any)(typeArgs).(core.Type), Values: liblists.At(1).(func(any) any)(typeArgs).(core.Type)}}, cx}}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorTypeArityMismatch{Value: error.TypeArityMismatchError{Type_: core.TypeMap_{Value: core.MapType{Keys: core.TypeUnit{}, Values: core.TypeUnit{}}}, ExpectedArity: 2, ActualArity: liblists.Length(typeArgs).(int32), TypeArguments: typeArgs}}}, Context: cx}})).(func(any) any)(func () any {
    var pairs any = libmaps.ToList(m)
    return func () any {
      var keyFoldResult any = liblists.Foldl(func (acc any) any {
        return func (p any) any {
          return libeithers.Bind(acc).(func(any) any)(func (accR any) any {
            return func () any {
              var types any = libpairs.First(accR)
              return func () any {
                var cxA any = libpairs.Second(accR)
                return libeithers.Bind(TypeOf(cxA.(context.Context), tx, []any{}, libpairs.First(p).(core.Term))).(func(any) any)(func (tResult any) any {
                  return func () any {
                    var t any = libpairs.First(tResult)
                    return func () any {
                      var cxB any = libpairs.Second(tResult)
                      return [2]any{"right", [2]any{liblists.Concat2(types).(func(any) any)(liblists.Pure(t)), cxB}}
                    }()
                  }()
                })
              }()
            }()
          })
        }
      }).(func(any) any)([2]any{"right", [2]any{[]any{}, cx}}).(func(any) any)(pairs)
      return libeithers.Bind(keyFoldResult).(func(any) any)(func (keyFoldR any) any {
        return func () any {
          var keyTypes any = libpairs.First(keyFoldR)
          return func () any {
            var cx2 any = libpairs.Second(keyFoldR)
            return libeithers.Bind(CheckSameType(cx2.(context.Context), tx, "map keys", keyTypes.([]any))).(func(any) any)(func (kt core.Type) any {
              return func () any {
                var valFoldResult any = liblists.Foldl(func (acc any) any {
                  return func (p any) any {
                    return libeithers.Bind(acc).(func(any) any)(func (accR any) any {
                      return func () any {
                        var types any = libpairs.First(accR)
                        return func () any {
                          var cxA any = libpairs.Second(accR)
                          return libeithers.Bind(TypeOf(cxA.(context.Context), tx, []any{}, libpairs.Second(p).(core.Term))).(func(any) any)(func (tResult any) any {
                            return func () any {
                              var t any = libpairs.First(tResult)
                              return func () any {
                                var cxB any = libpairs.Second(tResult)
                                return [2]any{"right", [2]any{liblists.Concat2(types).(func(any) any)(liblists.Pure(t)), cxB}}
                              }()
                            }()
                          })
                        }()
                      }()
                    })
                  }
                }).(func(any) any)([2]any{"right", [2]any{[]any{}, cx2}}).(func(any) any)(pairs)
                return libeithers.Bind(valFoldResult).(func(any) any)(func (valFoldR any) any {
                  return func () any {
                    var valTypes any = libpairs.First(valFoldR)
                    return func () any {
                      var cx3 any = libpairs.Second(valFoldR)
                      return libeithers.Bind(CheckSameType(cx3.(context.Context), tx, "map values", valTypes.([]any))).(func(any) any)(func (vt core.Type) any {
                        return libeithers.Bind(ApplyTypeArgumentsToType[graph.Graph](cx3.(context.Context), tx, typeArgs, core.TypeMap_{Value: core.MapType{Keys: kt, Values: vt}})).(func(any) any)(func (applied core.Type) any {
                          return [2]any{"right", [2]any{applied, cx3}}
                        })
                      })
                    }()
                  }()
                })
              }()
            })
          }()
        }()
      })
    }()
  }())
}

func TypeOfMaybe (cx context.Context, tx graph.Graph, typeArgs []any, mt any) any {
  return func () any {
    var forNothing any = func () any {
      var n any = liblists.Length(typeArgs)
      return liblogic.IfElse(libequality.Equal(n).(func(any) any)(1)).(func(any) any)([2]any{"right", [2]any{core.TypeMaybe{Value: liblists.Head(typeArgs).(core.Type)}, cx}}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorTypeArityMismatch{Value: error.TypeArityMismatchError{Type_: core.TypeMaybe{Value: core.TypeUnit{}}, ExpectedArity: 1, ActualArity: n.(int32), TypeArguments: typeArgs}}}, Context: cx}})
    }()
    return func () any {
      forJust := func (term core.Term) any {
        return libeithers.Bind(TypeOf(cx, tx, []any{}, term)).(func(any) any)(func (tResult any) any {
          return func () any {
            var termType any = libpairs.First(tResult)
            return func () any {
              var cx2 any = libpairs.Second(tResult)
              return func () any {
                var t any = core.TypeMaybe{Value: termType.(core.Type)}
                return libeithers.Bind(ApplyTypeArgumentsToType[graph.Graph](cx2.(context.Context), tx, typeArgs, t.(core.Type))).(func(any) any)(func (applied core.Type) any {
                  return [2]any{"right", [2]any{applied, cx2}}
                })
              }()
            }()
          }()
        })
      }
      return libmaybes.Maybe(forNothing).(func(any) any)(forJust).(func(any) any)(mt)
    }()
  }()
}

func TypeOfPair (cx context.Context, tx graph.Graph, typeArgs []any, p any) any {
  return func () any {
    var n any = liblists.Length(typeArgs)
    return liblogic.IfElse(libequality.Equal(n).(func(any) any)(2)).(func(any) any)(func () any {
      var pairFst any = libpairs.First(p)
      return func () any {
        var pairSnd any = libpairs.Second(p)
        return libeithers.Bind(TypeOf(cx, tx, []any{}, pairFst.(core.Term))).(func(any) any)(func (result1 any) any {
          return func () any {
            var firstType any = libpairs.First(result1)
            return func () any {
              var cx2 any = libpairs.Second(result1)
              return libeithers.Bind(TypeOf(cx2.(context.Context), tx, []any{}, pairSnd.(core.Term))).(func(any) any)(func (result2 any) any {
                return func () any {
                  var secondType any = libpairs.First(result2)
                  return func () any {
                    var cx3 any = libpairs.Second(result2)
                    return [2]any{"right", [2]any{core.TypePair{Value: core.PairType{First: firstType.(core.Type), Second: secondType.(core.Type)}}, cx3}}
                  }()
                }()
              })
            }()
          }()
        })
      }()
    }()).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorTypeArityMismatch{Value: error.TypeArityMismatchError{Type_: core.TypePair{Value: core.PairType{First: core.TypeUnit{}, Second: core.TypeUnit{}}}, ExpectedArity: 2, ActualArity: n.(int32), TypeArguments: typeArgs}}}, Context: cx}})
  }()
}

func TypeOfPrimitive (cx context.Context, tx graph.Graph, typeArgs []any, name core.Name) any {
  return func () any {
    var rawTs any = libmaps.Lookup(name).(func(any) any)(libmaps.FromList(liblists.Map(func (_gpt_p graph.Primitive) any {
      return [2]any{func (v any) any {
        return v.(graph.Primitive).Name
      }(_gpt_p), func (v any) any {
        return v.(graph.Primitive).Type_
      }(_gpt_p)}
    }).(func(any) any)(libmaps.Elems(func (v any) any {
      return v.(graph.Graph).Primitives
    }(tx)))))
    return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorUndefinedTerm{Value: error.UndefinedTermError{Name: name}}, Context: cx}}).(func(any) any)(func (tsRaw core.TypeScheme) any {
      return func () any {
        var instResult any = schemas.InstantiateTypeScheme(cx, tsRaw)
        return func () any {
          var ts any = libpairs.First(instResult)
          return func () any {
            var cx2 any = libpairs.Second(instResult)
            return func () any {
              var t any = rewriting.TypeSchemeToFType(ts.(core.TypeScheme))
              return libeithers.Bind(ApplyTypeArgumentsToType[graph.Graph](cx2.(context.Context), tx, typeArgs, t.(core.Type))).(func(any) any)(func (applied core.Type) any {
                return [2]any{"right", [2]any{applied, cx2}}
              })
            }()
          }()
        }()
      }()
    }).(func(any) any)(rawTs)
  }()
}

func TypeOfProjection (cx context.Context, tx graph.Graph, typeArgs []any, p core.Projection) any {
  return func () any {
    var tname any = func (v any) any {
      return v.(core.Projection).TypeName
    }(p)
    return func () any {
      var fname any = func (v any) any {
        return v.(core.Projection).Field
      }(p)
      return libeithers.Bind(schemas.RequireSchemaType(cx, func (v any) any {
        return v.(graph.Graph).SchemaTypes
      }(tx).([]any), tname.(core.Name))).(func(any) any)(func (schemaResult any) any {
        return func () any {
          var schemaType any = libpairs.First(schemaResult)
          return func () any {
            var cx2 any = libpairs.Second(schemaResult)
            return func () any {
              var svars any = schemaType.(core.TypeScheme).Variables
              return func () any {
                var sbody any = schemaType.(core.TypeScheme).Type_
                return libeithers.Bind(extractcore.RecordType[core.Name](cx2.(context.Context), tname, sbody.(core.Type))).(func(any) any)(func (sfields []any) any {
                  return libeithers.Bind(schemas.FindFieldType(cx2.(context.Context), fname.(core.Name), sfields)).(func(any) any)(func (ftyp core.Type) any {
                    return func () any {
                      var subst any = typing.TypeSubst(libmaps.FromList(liblists.Zip(svars).(func(any) any)(typeArgs)).([]any))
                      return func () any {
                        var sftyp any = substitution.SubstInType(subst.(typing.TypeSubst), ftyp)
                        return [2]any{"right", [2]any{core.TypeFunction{Value: core.FunctionType{Domain: schemas.NominalApplication(tname.(core.Name), typeArgs), Codomain: sftyp.(core.Type)}}, cx2}}
                      }()
                    }()
                  })
                })
              }()
            }()
          }()
        }()
      })
    }()
  }()
}

func TypeOfRecord (cx context.Context, tx graph.Graph, typeArgs []any, record core.Record) any {
  return func () any {
    var tname any = func (v any) any {
      return v.(core.Record).TypeName
    }(record)
    return func () any {
      var fields any = func (v any) any {
        return v.(core.Record).Fields
      }(record)
      return func () any {
        var foldResult any = liblists.Foldl(func (acc any) any {
          return func (term core.Term) any {
            return libeithers.Bind(acc).(func(any) any)(func (accR any) any {
              return func () any {
                var types any = libpairs.First(accR)
                return func () any {
                  var cxA any = libpairs.Second(accR)
                  return libeithers.Bind(TypeOf(cxA.(context.Context), tx, []any{}, term)).(func(any) any)(func (tResult any) any {
                    return func () any {
                      var t any = libpairs.First(tResult)
                      return func () any {
                        var cxB any = libpairs.Second(tResult)
                        return [2]any{"right", [2]any{liblists.Concat2(types).(func(any) any)(liblists.Pure(t)), cxB}}
                      }()
                    }()
                  })
                }()
              }()
            })
          }
        }).(func(any) any)([2]any{"right", [2]any{[]any{}, cx}}).(func(any) any)(liblists.Map(func (v any) any {
          return v.(core.Field).Term
        }).(func(any) any)(fields))
        return libeithers.Bind(foldResult).(func(any) any)(func (foldR any) any {
          return func () any {
            var cx2 any = libpairs.Second(foldR)
            return [2]any{"right", [2]any{schemas.NominalApplication(tname.(core.Name), typeArgs), cx2}}
          }()
        })
      }()
    }()
  }()
}

func TypeOfSet (cx context.Context, tx graph.Graph, typeArgs []any, els []any) any {
  return liblogic.IfElse(libsets.Null(els)).(func(any) any)(liblogic.IfElse(libequality.Equal(liblists.Length(typeArgs)).(func(any) any)(1)).(func(any) any)([2]any{"right", [2]any{core.TypeSet{Value: liblists.Head(typeArgs).(core.Type)}, cx}}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorChecking{Value: error.CheckingErrorTypeArityMismatch{Value: error.TypeArityMismatchError{Type_: core.TypeSet{Value: core.TypeUnit{}}, ExpectedArity: 1, ActualArity: liblists.Length(typeArgs).(int32), TypeArguments: typeArgs}}}, Context: cx}})).(func(any) any)(func () any {
    var foldResult any = liblists.Foldl(func (acc any) any {
      return func (term core.Term) any {
        return libeithers.Bind(acc).(func(any) any)(func (accR any) any {
          return func () any {
            var types any = libpairs.First(accR)
            return func () any {
              var cxA any = libpairs.Second(accR)
              return libeithers.Bind(TypeOf(cxA.(context.Context), tx, []any{}, term)).(func(any) any)(func (tResult any) any {
                return func () any {
                  var t any = libpairs.First(tResult)
                  return func () any {
                    var cxB any = libpairs.Second(tResult)
                    return [2]any{"right", [2]any{liblists.Concat2(types).(func(any) any)(liblists.Pure(t)), cxB}}
                  }()
                }()
              })
            }()
          }()
        })
      }
    }).(func(any) any)([2]any{"right", [2]any{[]any{}, cx}}).(func(any) any)(libsets.ToList(els))
    return libeithers.Bind(foldResult).(func(any) any)(func (foldR any) any {
      return func () any {
        var eltypes any = libpairs.First(foldR)
        return func () any {
          var cx2 any = libpairs.Second(foldR)
          return libeithers.Bind(CheckSameType(cx2.(context.Context), tx, "set elements", eltypes.([]any))).(func(any) any)(func (unifiedType core.Type) any {
            return [2]any{"right", [2]any{core.TypeSet{Value: unifiedType}, cx2}}
          })
        }()
      }()
    })
  }())
}

func TypeOfTypeApplication (cx context.Context, tx graph.Graph, typeArgs []any, tyapp core.TypeApplicationTerm) any {
  return func () any {
    var body any = func (v any) any {
      return v.(core.TypeApplicationTerm).Body
    }(tyapp)
    return func () any {
      var t any = func (v any) any {
        return v.(core.TypeApplicationTerm).Type_
      }(tyapp)
      return TypeOf(cx, tx, liblists.Cons(t).(func(any) any)(typeArgs).([]any), body.(core.Term))
    }()
  }()
}

func TypeOfTypeLambda (cx context.Context, tx graph.Graph, typeArgs []any, tl core.TypeLambda) any {
  return func () any {
    var v any = func (v any) any {
      return v.(core.TypeLambda).Parameter
    }(tl)
    return func () any {
      var body any = func (v any) any {
        return v.(core.TypeLambda).Body
      }(tl)
      return func () any {
        var vars any = func (v any) any {
          return v.(graph.Graph).TypeVariables
        }(tx)
        return func () any {
          var tx2 any = graph.Graph{BoundTerms: func (v any) any {
            return v.(graph.Graph).BoundTerms
          }(tx).([]any), BoundTypes: func (v any) any {
            return v.(graph.Graph).BoundTypes
          }(tx).([]any), ClassConstraints: func (v any) any {
            return v.(graph.Graph).ClassConstraints
          }(tx).([]any), LambdaVariables: func (v any) any {
            return v.(graph.Graph).LambdaVariables
          }(tx).([]any), Metadata: func (v any) any {
            return v.(graph.Graph).Metadata
          }(tx).([]any), Primitives: func (v any) any {
            return v.(graph.Graph).Primitives
          }(tx).([]any), SchemaTypes: func (v any) any {
            return v.(graph.Graph).SchemaTypes
          }(tx).([]any), TypeVariables: libsets.Insert(v).(func(any) any)(vars).([]any)}
          return libeithers.Bind(TypeOf(cx, tx2.(graph.Graph), []any{}, body.(core.Term))).(func(any) any)(func (result1 any) any {
            return func () any {
              var t1 any = libpairs.First(result1)
              return func () any {
                var cx2 any = libpairs.Second(result1)
                return libeithers.Bind(ApplyTypeArgumentsToType[graph.Graph](cx2.(context.Context), tx, typeArgs, core.TypeForall{Value: core.ForallType{Parameter: v.(core.Name), Body: t1.(core.Type)}})).(func(any) any)(func (applied core.Type) any {
                  return [2]any{"right", [2]any{applied, cx2}}
                })
              }()
            }()
          })
        }()
      }()
    }()
  }()
}

func TypeOfUnit[T0 any] (cx context.Context, tx T0, typeArgs []any) any {
  return libeithers.Bind(ApplyTypeArgumentsToType[T0](cx, tx, typeArgs, core.TypeUnit{})).(func(any) any)(func (applied core.Type) any {
    return [2]any{"right", [2]any{applied, cx}}
  })
}

func TypeOfUnwrap (cx context.Context, tx graph.Graph, typeArgs []any, tname core.Name) any {
  return libeithers.Bind(schemas.RequireSchemaType(cx, func (v any) any {
    return v.(graph.Graph).SchemaTypes
  }(tx).([]any), tname)).(func(any) any)(func (schemaResult any) any {
    return func () any {
      var schemaType any = libpairs.First(schemaResult)
      return func () any {
        var cx2 any = libpairs.Second(schemaResult)
        return func () any {
          var svars any = schemaType.(core.TypeScheme).Variables
          return func () any {
            var sbody any = schemaType.(core.TypeScheme).Type_
            return libeithers.Bind(extractcore.WrappedType[core.Name](cx2.(context.Context), tname, sbody.(core.Type))).(func(any) any)(func (wrapped core.Type) any {
              return func () any {
                var subst any = typing.TypeSubst(libmaps.FromList(liblists.Zip(svars).(func(any) any)(typeArgs)).([]any))
                return func () any {
                  var swrapped any = substitution.SubstInType(subst.(typing.TypeSubst), wrapped)
                  return [2]any{"right", [2]any{core.TypeFunction{Value: core.FunctionType{Domain: schemas.NominalApplication(tname, typeArgs), Codomain: swrapped.(core.Type)}}, cx2}}
                }()
              }()
            })
          }()
        }()
      }()
    }()
  })
}

func TypeOfVariable (cx context.Context, tx graph.Graph, typeArgs []any, name core.Name) any {
  return func () any {
    var rawTypeScheme any = libmaps.Lookup(name).(func(any) any)(func (v any) any {
      return v.(graph.Graph).BoundTypes
    }(tx))
    return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorUndefinedType{Value: error.UndefinedTypeError{Name: name}}, Context: cx}}).(func(any) any)(func (ts core.TypeScheme) any {
      return func () any {
        var tResult any = liblogic.IfElse(liblists.Null(typeArgs)).(func(any) any)(schemas.InstantiateType(cx, rewriting.TypeSchemeToFType(ts))).(func(any) any)([2]any{rewriting.TypeSchemeToFType(ts), cx})
        return func () any {
          var t any = libpairs.First(tResult)
          return func () any {
            var cx2 any = libpairs.Second(tResult)
            return libeithers.Bind(ApplyTypeArgumentsToType[graph.Graph](cx2.(context.Context), tx, typeArgs, t.(core.Type))).(func(any) any)(func (applied core.Type) any {
              return [2]any{"right", [2]any{applied, cx2}}
            })
          }()
        }()
      }()
    }).(func(any) any)(rawTypeScheme)
  }()
}

func TypeOfWrappedTerm (cx context.Context, tx graph.Graph, typeArgs []any, wt core.WrappedTerm) any {
  return func () any {
    var tname any = func (v any) any {
      return v.(core.WrappedTerm).TypeName
    }(wt)
    return func () any {
      var body any = func (v any) any {
        return v.(core.WrappedTerm).Body
      }(wt)
      return libeithers.Bind(TypeOf(cx, tx, []any{}, body.(core.Term))).(func(any) any)(func (result any) any {
        return func () any {
          var cx2 any = libpairs.Second(result)
          return [2]any{"right", [2]any{schemas.NominalApplication(tname.(core.Name), typeArgs), cx2}}
        }()
      })
    }()
  }()
}

func TypesAllEffectivelyEqual (tx graph.Graph, tlist []any) bool {
  return func () any {
    var types any = func (v any) any {
      return v.(graph.Graph).SchemaTypes
    }(tx)
    return func () any {
      containsFreeVar := func (t core.Type) any {
        return func () any {
          var allVars any = rewriting.FreeVariablesInTypeSimple(t)
          return func () any {
            var schemaNames any = libsets.FromList(libmaps.Keys(types))
            return liblogic.Not(libsets.Null(libsets.Difference(allVars).(func(any) any)(schemaNames)))
          }()
        }()
      }
      return func () any {
        var anyContainsFreeVar any = liblists.Foldl(func (acc bool) any {
          return func (t core.Type) any {
            return liblogic.Or(acc).(func(any) any)(containsFreeVar(t))
          }
        }).(func(any) any)(false).(func(any) any)(tlist)
        return liblogic.IfElse(anyContainsFreeVar).(func(any) any)(true).(func(any) any)(liblogic.IfElse(AllEqual(liblists.Map(func (t core.Type) any {
          return NormalizeTypeFreeVars(t)
        }).(func(any) any)(tlist).([]any))).(func(any) any)(true).(func(any) any)(AllEqual(liblists.Map(func (t core.Type) any {
          return NormalizeTypeFreeVars(rewriting.DeannotateTypeRecursive(rewriting.ReplaceTypedefs(types.([]any), t)))
        }).(func(any) any)(tlist).([]any))))
      }()
    }()
  }().(bool)
}

func TypesEffectivelyEqual (tx graph.Graph, t1 core.Type, t2 core.Type) bool {
  return liblogic.Or(ContainsInScopeTypeVars(tx, t1)).(func(any) any)(liblogic.Or(ContainsInScopeTypeVars(tx, t2)).(func(any) any)(TypesAllEffectivelyEqual(tx, []any{schemas.FullyStripAndNormalizeType(t1), schemas.FullyStripAndNormalizeType(t2)}))).(bool)
}
