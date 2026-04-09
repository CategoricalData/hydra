// Note: this is an automatically generated file. Do not edit.

package reduction

import (
  "hydra.dev/hydra/arity"
  "hydra.dev/hydra/checking"
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  encodecore "hydra.dev/hydra/encode/core"
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
  libmath "hydra.dev/hydra/lib/math"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/rewriting"
  "hydra.dev/hydra/schemas"
  showerror "hydra.dev/hydra/show/error"
)

func AlphaConvert (vold core.Name, vnew core.Name, term core.Term) core.Term {
  return rewriting.ReplaceFreeTermVariable(vold, core.TermVariable{Value: vnew}, term)
}

func BetaReduceType (cx context.Context, graph graph.Graph, typ core.Type) any {
  return func () any {
    var reduceApp func(core.ApplicationType) any
    reduceApp = func (app core.ApplicationType) any {
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
              case core.TypeAnnotated:
              return func (at core.AnnotatedType) any {
                return libeithers.Bind(reduceApp(core.ApplicationType{Function: func (v any) any {
                  return v.(core.AnnotatedType).Body
                }(at).(core.Type), Argument: rhs.(core.Type)})).(func(any) any)(func (a core.Type) any {
                  return [2]any{"right", core.TypeAnnotated{Value: core.AnnotatedType{Body: a, Annotation: func (v any) any {
                    return v.(core.AnnotatedType).Annotation
                  }(at).([]any)}}}
                })
              }(v.Value)
              case core.TypeForall:
              return func (ft core.ForallType) any {
                return BetaReduceType(cx, graph, rewriting.ReplaceFreeTypeVariable(func (v any) any {
                  return v.(core.ForallType).Parameter
                }(ft).(core.Name), rhs.(core.Type), func (v any) any {
                  return v.(core.ForallType).Body
                }(ft).(core.Type)))
              }(v.Value)
              case core.TypeVariable:
              return func (name core.Name) any {
                return libeithers.Bind(schemas.RequireType(cx, graph, name)).(func(any) any)(func (t_ core.Type) any {
                  return BetaReduceType(cx, graph, core.TypeApplication{Value: core.ApplicationType{Function: t_, Argument: rhs.(core.Type)}})
                })
              }(v.Value)
            }
            return nil
          }(lhs)
        }()
      }()
    }
    return func () any {
      mapExpr := func (recurse func(core.Type) any) any {
        return func (t core.Type) any {
          return func () any {
            findApp := func (r core.Type) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.TypeApplication:
                  return func (a core.ApplicationType) any {
                    return reduceApp(a)
                  }(v.Value)
                  default:
                  return [2]any{"right", r}
                }
                return nil
              }(r)
            }
            return libeithers.Bind(recurse(t)).(func(any) any)(func (r core.Type) any {
              return findApp(r)
            })
          }()
        }
      }
      return rewriting.RewriteTypeM(func (_p func(core.Type) any) func(core.Type) any {
        return mapExpr(_p).(func(core.Type) any)
      }, typ)
    }()
  }()
}

func ContractTerm (term core.Term) core.Term {
  return func () any {
    rewrite := func (recurse func(core.Term) core.Term) any {
      return func (t core.Term) any {
        return func () any {
          var rec any = recurse(t)
          return func (x any) any {
            switch v := x.(type) {
              case core.TermApplication:
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
                        case core.TermFunction:
                        return func (f core.Function) any {
                          return func (x any) any {
                            switch v := x.(type) {
                              case core.FunctionLambda:
                              return func (l core.Lambda) any {
                                return func () any {
                                  var v any = func (v any) any {
                                    return v.(core.Lambda).Parameter
                                  }(l)
                                  return func () any {
                                    var body any = func (v any) any {
                                      return v.(core.Lambda).Body
                                    }(l)
                                    return liblogic.IfElse(rewriting.IsFreeVariableInTerm(v.(core.Name), body.(core.Term))).(func(any) any)(body).(func(any) any)(rewriting.ReplaceFreeTermVariable(v.(core.Name), rhs.(core.Term), body.(core.Term)))
                                  }()
                                }()
                              }(v.Value)
                              default:
                              return rec
                            }
                            return nil
                          }(f)
                        }(v.Value)
                        default:
                        return rec
                      }
                      return nil
                    }(rewriting.DeannotateTerm(lhs.(core.Term)))
                  }()
                }()
              }(v.Value)
              default:
              return rec
            }
            return nil
          }(rec)
        }()
      }
    }
    return rewriting.RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return rewrite(_p).(func(core.Term) core.Term)
    }, term)
  }().(core.Term)
}

var CountPrimitiveInvocations = true

func EtaReduceTerm (term core.Term) core.Term {
  return func () any {
    var noChange any = term
    return func () any {
      var reduceLambda func(core.Lambda) any
      reduceLambda = func (l core.Lambda) any {
        return func () any {
          var v any = func (v any) any {
            return v.(core.Lambda).Parameter
          }(l)
          return func () any {
            var d any = func (v any) any {
              return v.(core.Lambda).Domain
            }(l)
            return func () any {
              var body any = func (v any) any {
                return v.(core.Lambda).Body
              }(l)
              return func (x any) any {
                switch v := x.(type) {
                  case core.TermAnnotated:
                  return func (at core.AnnotatedTerm) any {
                    return reduceLambda(core.Lambda{Parameter: v.(core.Name), Domain: d, Body: func (v any) any {
                      return v.(core.AnnotatedTerm).Body
                    }(at).(core.Term)})
                  }(v.Value)
                  case core.TermApplication:
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
                            case core.TermAnnotated:
                            return func (at core.AnnotatedTerm) any {
                              return reduceLambda(core.Lambda{Parameter: v.(core.Name), Domain: d, Body: core.TermApplication{Value: core.Application{Function: lhs.(core.Term), Argument: func (v any) any {
                                return v.(core.AnnotatedTerm).Body
                              }(at).(core.Term)}}})
                            }(v.Value)
                            case core.TermVariable:
                            return func (v1 core.Name) any {
                              return liblogic.IfElse(liblogic.And(libequality.Equal(v).(func(any) any)(func (v any) any {
                                return v
                              }(v1))).(func(any) any)(liblogic.Not(rewriting.IsFreeVariableInTerm(v.(core.Name), lhs.(core.Term))))).(func(any) any)(EtaReduceTerm(lhs.(core.Term))).(func(any) any)(noChange)
                            }(v.Value)
                            default:
                            return noChange
                          }
                          return nil
                        }(EtaReduceTerm(rhs.(core.Term)))
                      }()
                    }()
                  }(v.Value)
                  default:
                  return noChange
                }
                return nil
              }(EtaReduceTerm(body.(core.Term)))
            }()
          }()
        }()
      }
      return func (x any) any {
        switch v := x.(type) {
          case core.TermAnnotated:
          return func (at core.AnnotatedTerm) any {
            return core.TermAnnotated{Value: core.AnnotatedTerm{Body: EtaReduceTerm(func (v any) any {
              return v.(core.AnnotatedTerm).Body
            }(at).(core.Term)), Annotation: func (v any) any {
              return v.(core.AnnotatedTerm).Annotation
            }(at).([]any)}}
          }(v.Value)
          case core.TermFunction:
          return func (f core.Function) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.FunctionLambda:
                return func (l core.Lambda) any {
                  return reduceLambda(l)
                }(v.Value)
                default:
                return noChange
              }
              return nil
            }(f)
          }(v.Value)
          default:
          return noChange
        }
        return nil
      }(term)
    }()
  }().(core.Term)
}

func EtaExpandTerm (graph graph.Graph, term core.Term) core.Term {
  return func () any {
    expand := func (args []any) any {
      return func (arity int32) any {
        return func (t core.Term) any {
          return func () any {
            var apps any = liblists.Foldl(func (lhs core.Term) any {
              return func (arg core.Term) any {
                return core.TermApplication{Value: core.Application{Function: lhs, Argument: arg}}
              }
            }).(func(any) any)(t).(func(any) any)(args)
            return func () any {
              var is any = liblogic.IfElse(libequality.Lte(arity).(func(any) any)(liblists.Length(args))).(func(any) any)([]any{}).(func(any) any)(libmath.Range(1).(func(any) any)(libmath.Sub(arity).(func(any) any)(liblists.Length(args))))
              return func () any {
                var pad func([]any) any
                pad = func (indices []any) any {
                  return func (t2 core.Term) any {
                    return liblogic.IfElse(liblists.Null(indices)).(func(any) any)(t2).(func(any) any)(core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name(libstrings.Cat2("v").(func(any) any)(libliterals.ShowInt32(liblists.Head(indices))).(string)), Domain: nil, Body: pad(liblists.Tail(indices).([]any)).(func(any) any)(core.TermApplication{Value: core.Application{Function: t2, Argument: core.TermVariable{Value: core.Name(libstrings.Cat2("v").(func(any) any)(libliterals.ShowInt32(liblists.Head(indices))).(string))}}}).(core.Term)}}})
                  }
                }
                return pad(is.([]any)).(func(any) any)(apps)
              }()
            }()
          }()
        }
      }
    }
    return func () any {
      var rewrite func([]any) any
      rewrite = func (args []any) any {
        return func (recurse func(core.Term) core.Term) any {
          return func (t core.Term) any {
            return func () any {
              afterRecursion := func (term2 core.Term) any {
                return expand(args).(func(any) any)(EtaExpansionArity(graph, term2)).(func(any) any)(term2)
              }
              return func () any {
                var t2 any = rewriting.DetypeTerm(t)
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermApplication:
                    return func (app core.Application) any {
                      return func () any {
                        var lhs any = func (v any) any {
                          return v.(core.Application).Function
                        }(app)
                        return func () any {
                          var rhs any = func (v any) any {
                            return v.(core.Application).Argument
                          }(app)
                          return func () any {
                            var erhs any = rewrite([]any{}).(func(any) any)(recurse).(func(any) any)(rhs)
                            return rewrite(liblists.Cons(erhs).(func(any) any)(args).([]any)).(func(any) any)(recurse).(func(any) any)(lhs)
                          }()
                        }()
                      }()
                    }(v.Value)
                    default:
                    return afterRecursion(recurse(t2.(core.Term)))
                  }
                  return nil
                }(t2)
              }()
            }()
          }
        }
      }
      return ContractTerm(rewriting.RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
        return func (v1 func(core.Term) core.Term) func(core.Term) core.Term {
          return func (v2 core.Term) core.Term {
            return rewrite([]any{}).(func(any) any)(v1).(func(any) any)(v2)
          }
        }(_p).(func(core.Term) core.Term)
      }, term))
    }()
  }().(core.Term)
}

func EtaExpandTermNew (tx0 graph.Graph, term0 core.Term) core.Term {
  return func () any {
    var termArityWithContext func(graph.Graph) any
    termArityWithContext = func (tx graph.Graph) any {
      return func (term core.Term) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TermAnnotated:
            return func (at core.AnnotatedTerm) any {
              return termArityWithContext(tx).(func(any) any)(func (v any) any {
                return v.(core.AnnotatedTerm).Body
              }(at))
            }(v.Value)
            case core.TermApplication:
            return func (app core.Application) any {
              return libmath.Sub(termArityWithContext(tx).(func(any) any)(func (v any) any {
                return v.(core.Application).Function
              }(app))).(func(any) any)(1)
            }(v.Value)
            case core.TermFunction:
            return func (f core.Function) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.FunctionElimination:
                  return func (_ core.Elimination) any {
                    return 1
                  }(v.Value)
                  case core.FunctionLambda:
                  return func (_ core.Lambda) any {
                    return 0
                  }(v.Value)
                  case core.FunctionPrimitive:
                  return func (name core.Name) any {
                    return libmaybes.Maybe(0).(func(any) any)(arity.TypeSchemeArity).(func(any) any)(libmaps.Lookup(name).(func(any) any)(libmaps.FromList(liblists.Map(func (_gpt_p graph.Primitive) any {
                      return [2]any{func (v any) any {
                        return v.(graph.Primitive).Name
                      }(_gpt_p), func (v any) any {
                        return v.(graph.Primitive).Type_
                      }(_gpt_p)}
                    }).(func(any) any)(libmaps.Elems(func (v any) any {
                      return v.(graph.Graph).Primitives
                    }(tx))))))
                  }(v.Value)
                }
                return nil
              }(f)
            }(v.Value)
            case core.TermLet:
            return func (l core.Let) any {
              return termArityWithContext(schemas.ExtendGraphForLet(func (_p graph.Graph) func(core.Binding) any {
                return func (_ graph.Graph) func(core.Binding) any {
                  return func (_2 core.Binding) any {
                    return nil
                  }
                }(_p).(func(core.Binding) any)
              }, tx, l)).(func(any) any)(func (v any) any {
                return v.(core.Let).Body
              }(l))
            }(v.Value)
            case core.TermTypeLambda:
            return func (tl core.TypeLambda) any {
              return termArityWithContext(schemas.ExtendGraphForTypeLambda(tx, tl)).(func(any) any)(func (v any) any {
                return v.(core.TypeLambda).Body
              }(tl))
            }(v.Value)
            case core.TermTypeApplication:
            return func (tat core.TypeApplicationTerm) any {
              return termArityWithContext(tx).(func(any) any)(func (v any) any {
                return v.(core.TypeApplicationTerm).Body
              }(tat))
            }(v.Value)
            case core.TermVariable:
            return func (name core.Name) any {
              return libmaybes.Maybe(0).(func(any) any)(arity.TypeArity).(func(any) any)(libmaybes.Map(rewriting.TypeSchemeToFType).(func(any) any)(libmaps.Lookup(name).(func(any) any)(func (v any) any {
                return v.(graph.Graph).BoundTypes
              }(tx))))
            }(v.Value)
            default:
            return 0
          }
          return nil
        }(term)
      }
    }
    return func () any {
      var domainTypes func(int32) any
      domainTypes = func (n int32) any {
        return func (mt any) any {
          return liblogic.IfElse(libequality.Lte(n).(func(any) any)(0)).(func(any) any)([]any{}).(func(any) any)(libmaybes.Maybe(liblists.Map(func (_ int32) any {
            return nil
          }).(func(any) any)(libmath.Range(1).(func(any) any)(n))).(func(any) any)(func (typ core.Type) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.TypeFunction:
                return func (ftyp core.FunctionType) any {
                  return liblists.Cons(func () any {
                    _v := func (v any) any {
                      return v.(core.FunctionType).Domain
                    }(ftyp)
                    return &_v
                  }()).(func(any) any)(domainTypes(libmath.Sub(n).(func(any) any)(1).(int32)).(func(any) any)(func () any {
                    _v := func (v any) any {
                      return v.(core.FunctionType).Codomain
                    }(ftyp)
                    return &_v
                  }()))
                }(v.Value)
                case core.TypeAnnotated:
                return func (at core.AnnotatedType) any {
                  return domainTypes(n).(func(any) any)(func () any {
                    _v := func (v any) any {
                      return v.(core.AnnotatedType).Body
                    }(at)
                    return &_v
                  }())
                }(v.Value)
                case core.TypeApplication:
                return func (atyp core.ApplicationType) any {
                  return domainTypes(n).(func(any) any)(func () any {
                    _v := func (v any) any {
                      return v.(core.ApplicationType).Function
                    }(atyp)
                    return &_v
                  }())
                }(v.Value)
                case core.TypeForall:
                return func (ft core.ForallType) any {
                  return domainTypes(n).(func(any) any)(func () any {
                    _v := func (v any) any {
                      return v.(core.ForallType).Body
                    }(ft)
                    return &_v
                  }())
                }(v.Value)
                default:
                return liblists.Map(func (_ int32) any {
                  return nil
                }).(func(any) any)(libmath.Range(1).(func(any) any)(n))
              }
              return nil
            }(typ)
          }).(func(any) any)(mt))
        }
      }
      return func () any {
        var peelFunctionDomains func(any) any
        peelFunctionDomains = func (mtyp any) any {
          return func (n int32) any {
            return liblogic.IfElse(libequality.Lte(n).(func(any) any)(0)).(func(any) any)(mtyp).(func(any) any)(libmaybes.Maybe(nil).(func(any) any)(func (typ core.Type) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.TypeFunction:
                  return func (ftyp core.FunctionType) any {
                    return peelFunctionDomains(func () any {
                      _v := func (v any) any {
                        return v.(core.FunctionType).Codomain
                      }(ftyp)
                      return &_v
                    }()).(func(any) any)(libmath.Sub(n).(func(any) any)(1))
                  }(v.Value)
                  case core.TypeAnnotated:
                  return func (at core.AnnotatedType) any {
                    return peelFunctionDomains(func () any {
                      _v := func (v any) any {
                        return v.(core.AnnotatedType).Body
                      }(at)
                      return &_v
                    }()).(func(any) any)(n)
                  }(v.Value)
                  case core.TypeApplication:
                  return func (atyp core.ApplicationType) any {
                    return peelFunctionDomains(func () any {
                      _v := func (v any) any {
                        return v.(core.ApplicationType).Function
                      }(atyp)
                      return &_v
                    }()).(func(any) any)(n)
                  }(v.Value)
                  case core.TypeForall:
                  return func (ft core.ForallType) any {
                    return peelFunctionDomains(func () any {
                      _v := func (v any) any {
                        return v.(core.ForallType).Body
                      }(ft)
                      return &_v
                    }()).(func(any) any)(n)
                  }(v.Value)
                  default:
                  return nil
                }
                return nil
              }(typ)
            }).(func(any) any)(mtyp))
          }
        }
        return func () any {
          expand := func (alwaysPad bool) any {
            return func (args []any) any {
              return func (arity int32) any {
                return func (headTyp any) any {
                  return func (head core.Term) any {
                    return func () any {
                      var applied any = liblists.Foldl(func (lhs core.Term) any {
                        return func (arg core.Term) any {
                          return core.TermApplication{Value: core.Application{Function: lhs, Argument: arg}}
                        }
                      }).(func(any) any)(head).(func(any) any)(args)
                      return func () any {
                        var numArgs any = liblists.Length(args)
                        return func () any {
                          var needed any = libmath.Sub(arity).(func(any) any)(numArgs)
                          return liblogic.IfElse(liblogic.And(libequality.Gt(needed).(func(any) any)(0)).(func(any) any)(liblogic.Or(alwaysPad).(func(any) any)(libequality.Gt(numArgs).(func(any) any)(0)))).(func(any) any)(func () any {
                            var indices any = libmath.Range(1).(func(any) any)(needed)
                            return func () any {
                              var remainingType any = peelFunctionDomains(headTyp).(func(any) any)(numArgs)
                              return func () any {
                                var domains any = domainTypes(needed.(int32)).(func(any) any)(remainingType)
                                return func () any {
                                  var codomainType any = peelFunctionDomains(remainingType).(func(any) any)(needed)
                                  return func () any {
                                    var fullyAppliedRaw any = liblists.Foldl(func (body core.Term) any {
                                      return func (i int32) any {
                                        return func () any {
                                          var vn any = core.Name(libstrings.Cat2("v").(func(any) any)(libliterals.ShowInt32(i)).(string))
                                          return core.TermApplication{Value: core.Application{Function: body, Argument: core.TermVariable{Value: vn.(core.Name)}}}
                                        }()
                                      }
                                    }).(func(any) any)(applied).(func(any) any)(indices)
                                    return func () any {
                                      var fullyApplied any = libmaybes.Maybe(fullyAppliedRaw).(func(any) any)(func (ct core.Type) any {
                                        return core.TermAnnotated{Value: core.AnnotatedTerm{Body: fullyAppliedRaw.(core.Term), Annotation: libmaps.Singleton(core.Name("type")).(func(any) any)(encodecore.Type_(ct)).([]any)}}
                                      }).(func(any) any)(codomainType)
                                      return func () any {
                                        var indexedDomains any = liblists.Zip(indices).(func(any) any)(domains)
                                        return liblists.Foldl(func (body core.Term) any {
                                          return func (idPair any) any {
                                            return func () any {
                                              var i any = libpairs.First(idPair)
                                              return func () any {
                                                var dom any = libpairs.Second(idPair)
                                                return func () any {
                                                  var vn any = core.Name(libstrings.Cat2("v").(func(any) any)(libliterals.ShowInt32(i)).(string))
                                                  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: vn.(core.Name), Domain: dom, Body: body}}}
                                                }()
                                              }()
                                            }()
                                          }
                                        }).(func(any) any)(fullyApplied).(func(any) any)(liblists.Reverse(indexedDomains))
                                      }()
                                    }()
                                  }()
                                }()
                              }()
                            }()
                          }()).(func(any) any)(applied)
                        }()
                      }()
                    }()
                  }
                }
              }
            }
          }
          return func () any {
            var rewriteWithArgs func([]any) any
            rewriteWithArgs = func (args []any) any {
              return func (tx graph.Graph) any {
                return func (term core.Term) any {
                  return func () any {
                    recurse := func (tx1 graph.Graph) any {
                      return func (term1 core.Term) any {
                        return rewriteWithArgs([]any{}).(func(any) any)(tx1).(func(any) any)(term1)
                      }
                    }
                    return func () any {
                      var termHeadType func(graph.Graph) any
                      termHeadType = func (tx2 graph.Graph) any {
                        return func (trm2 core.Term) any {
                          return func (x any) any {
                            switch v := x.(type) {
                              case core.TermAnnotated:
                              return func (at2 core.AnnotatedTerm) any {
                                return termHeadType(tx2).(func(any) any)(func (v any) any {
                                  return v.(core.AnnotatedTerm).Body
                                }(at2))
                              }(v.Value)
                              case core.TermFunction:
                              return func (f2 core.Function) any {
                                return func (x any) any {
                                  switch v := x.(type) {
                                    case core.FunctionPrimitive:
                                    return func (pn2 core.Name) any {
                                      return libmaybes.Map(func (ts2 core.TypeScheme) any {
                                        return func (v any) any {
                                          return v.(core.TypeScheme).Type_
                                        }(ts2)
                                      }).(func(any) any)(libmaps.Lookup(pn2).(func(any) any)(libmaps.FromList(liblists.Map(func (_gpt_p graph.Primitive) any {
                                        return [2]any{func (v any) any {
                                          return v.(graph.Primitive).Name
                                        }(_gpt_p), func (v any) any {
                                          return v.(graph.Primitive).Type_
                                        }(_gpt_p)}
                                      }).(func(any) any)(libmaps.Elems(func (v any) any {
                                        return v.(graph.Graph).Primitives
                                      }(tx2))))))
                                    }(v.Value)
                                    default:
                                    return nil
                                  }
                                  return nil
                                }(f2)
                              }(v.Value)
                              case core.TermLet:
                              return func (l2 core.Let) any {
                                return termHeadType(schemas.ExtendGraphForLet(func (_p graph.Graph) func(core.Binding) any {
                                  return func (_ graph.Graph) func(core.Binding) any {
                                    return func (_2 core.Binding) any {
                                      return nil
                                    }
                                  }(_p).(func(core.Binding) any)
                                }, tx2, l2)).(func(any) any)(func (v any) any {
                                  return v.(core.Let).Body
                                }(l2))
                              }(v.Value)
                              case core.TermTypeLambda:
                              return func (tl2 core.TypeLambda) any {
                                return termHeadType(schemas.ExtendGraphForTypeLambda(tx2, tl2)).(func(any) any)(func (v any) any {
                                  return v.(core.TypeLambda).Body
                                }(tl2))
                              }(v.Value)
                              case core.TermTypeApplication:
                              return func (tat2 core.TypeApplicationTerm) any {
                                return libmaybes.Bind(termHeadType(tx2).(func(any) any)(func (v any) any {
                                  return v.(core.TypeApplicationTerm).Body
                                }(tat2))).(func(any) any)(func (htyp2 core.Type) any {
                                  return func (x any) any {
                                    switch v := x.(type) {
                                      case core.TypeForall:
                                      return func (ft2 core.ForallType) any {
                                        return func () any {
                                          _v := rewriting.ReplaceFreeTypeVariable(func (v any) any {
                                            return v.(core.ForallType).Parameter
                                          }(ft2).(core.Name), func (v any) any {
                                            return v.(core.TypeApplicationTerm).Type_
                                          }(tat2).(core.Type), func (v any) any {
                                            return v.(core.ForallType).Body
                                          }(ft2).(core.Type))
                                          return &_v
                                        }()
                                      }(v.Value)
                                      default:
                                      return func () any {
                                        _v := htyp2
                                        return &_v
                                      }()
                                    }
                                    return nil
                                  }(htyp2)
                                })
                              }(v.Value)
                              case core.TermVariable:
                              return func (vn2 core.Name) any {
                                return libmaybes.Map(rewriting.TypeSchemeToFType).(func(any) any)(libmaps.Lookup(vn2).(func(any) any)(func (v any) any {
                                  return v.(graph.Graph).BoundTypes
                                }(tx2)))
                              }(v.Value)
                              default:
                              return nil
                            }
                            return nil
                          }(trm2)
                        }
                      }
                      return func () any {
                        afterRecursion := func (trm core.Term) any {
                          return func () any {
                            var arity any = termArityWithContext(tx).(func(any) any)(trm)
                            return func () any {
                              var hType any = termHeadType(tx).(func(any) any)(trm)
                              return expand(false).(func(any) any)(args).(func(any) any)(arity).(func(any) any)(hType).(func(any) any)(trm)
                            }()
                          }()
                        }
                        return func () any {
                          forField := func (f core.Field) any {
                            return core.Field{Name: func (v any) any {
                              return v.(core.Field).Name
                            }(f).(core.Name), Term: recurse(tx).(func(any) any)(func (v any) any {
                              return v.(core.Field).Term
                            }(f)).(core.Term)}
                          }
                          return func () any {
                            forCaseBranch := func (f core.Field) any {
                              return func () any {
                                var branchBody any = recurse(tx).(func(any) any)(func (v any) any {
                                  return v.(core.Field).Term
                                }(f))
                                return func () any {
                                  var arty any = termArityWithContext(tx).(func(any) any)(branchBody)
                                  return func () any {
                                    var branchHType any = termHeadType(tx).(func(any) any)(branchBody)
                                    return core.Field{Name: func (v any) any {
                                      return v.(core.Field).Name
                                    }(f).(core.Name), Term: expand(true).(func(any) any)([]any{}).(func(any) any)(arty).(func(any) any)(branchHType).(func(any) any)(branchBody).(core.Term)}
                                  }()
                                }()
                              }()
                            }
                            return func () any {
                              forElimination := func (elm core.Elimination) any {
                                return func (x any) any {
                                  switch v := x.(type) {
                                    case core.EliminationRecord:
                                    return func (p core.Projection) any {
                                      return core.EliminationRecord{Value: p}
                                    }(v.Value)
                                    case core.EliminationUnion:
                                    return func (cs core.CaseStatement) any {
                                      return core.EliminationUnion{Value: core.CaseStatement{TypeName: func (v any) any {
                                        return v.(core.CaseStatement).TypeName
                                      }(cs).(core.Name), Default_: libmaybes.Map(func (t1 core.Term) any {
                                        return recurse(tx).(func(any) any)(t1)
                                      }).(func(any) any)(func (v any) any {
                                        return v.(core.CaseStatement).Default_
                                      }(cs)), Cases: liblists.Map(forCaseBranch).(func(any) any)(func (v any) any {
                                        return v.(core.CaseStatement).Cases
                                      }(cs)).([]any)}}
                                    }(v.Value)
                                    case core.EliminationWrap:
                                    return func (nm core.Name) any {
                                      return core.EliminationWrap{Value: nm}
                                    }(v.Value)
                                  }
                                  return nil
                                }(elm)
                              }
                              return func () any {
                                forMap := func (mp []any) any {
                                  return func () any {
                                    forPair := func (pr any) any {
                                      return [2]any{recurse(tx).(func(any) any)(libpairs.First(pr)), recurse(tx).(func(any) any)(libpairs.Second(pr))}
                                    }
                                    return libmaps.FromList(liblists.Map(forPair).(func(any) any)(libmaps.ToList(mp)))
                                  }()
                                }
                                return func (x any) any {
                                  switch v := x.(type) {
                                    case core.TermAnnotated:
                                    return func (at core.AnnotatedTerm) any {
                                      return afterRecursion(core.TermAnnotated{Value: core.AnnotatedTerm{Body: recurse(tx).(func(any) any)(func (v any) any {
                                        return v.(core.AnnotatedTerm).Body
                                      }(at)).(core.Term), Annotation: func (v any) any {
                                        return v.(core.AnnotatedTerm).Annotation
                                      }(at).([]any)}})
                                    }(v.Value)
                                    case core.TermApplication:
                                    return func (app core.Application) any {
                                      return func () any {
                                        var rhs any = rewriteWithArgs([]any{}).(func(any) any)(tx).(func(any) any)(func (v any) any {
                                          return v.(core.Application).Argument
                                        }(app))
                                        return rewriteWithArgs(liblists.Cons(rhs).(func(any) any)(args).([]any)).(func(any) any)(tx).(func(any) any)(func (v any) any {
                                          return v.(core.Application).Function
                                        }(app))
                                      }()
                                    }(v.Value)
                                    case core.TermEither:
                                    return func (e any) any {
                                      return afterRecursion(core.TermEither{Value: libeithers.Either(func (l core.Term) any {
                                        return [2]any{"left", recurse(tx).(func(any) any)(l)}
                                      }).(func(any) any)(func (r core.Term) any {
                                        return [2]any{"right", recurse(tx).(func(any) any)(r)}
                                      }).(func(any) any)(e)})
                                    }(v.Value)
                                    case core.TermFunction:
                                    return func (fn core.Function) any {
                                      return func (x any) any {
                                        switch v := x.(type) {
                                          case core.FunctionElimination:
                                          return func (elm core.Elimination) any {
                                            return func () any {
                                              var padElim any = func (x any) any {
                                                switch v := x.(type) {
                                                  case core.EliminationRecord:
                                                  return func (_ core.Projection) any {
                                                    return false
                                                  }(v.Value)
                                                  case core.EliminationUnion:
                                                  return func (_ core.CaseStatement) any {
                                                    return true
                                                  }(v.Value)
                                                  case core.EliminationWrap:
                                                  return func (_ core.Name) any {
                                                    return false
                                                  }(v.Value)
                                                }
                                                return nil
                                              }(elm)
                                              return func () any {
                                                var elimTerm any = core.TermFunction{Value: core.FunctionElimination{Value: forElimination(elm).(core.Elimination)}}
                                                return func () any {
                                                  var elimHeadType any = func (x any) any {
                                                    switch v := x.(type) {
                                                      case core.EliminationUnion:
                                                      return func (cs2 core.CaseStatement) any {
                                                        return func () any {
                                                          _v := core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: func (v any) any {
                                                            return v.(core.CaseStatement).TypeName
                                                          }(cs2).(core.Name)}, Codomain: core.TypeUnit{}}}
                                                          return &_v
                                                        }()
                                                      }(v.Value)
                                                      default:
                                                      return nil
                                                    }
                                                    return nil
                                                  }(elm)
                                                  return expand(padElim.(bool)).(func(any) any)(args).(func(any) any)(1).(func(any) any)(elimHeadType).(func(any) any)(elimTerm)
                                                }()
                                              }()
                                            }()
                                          }(v.Value)
                                          case core.FunctionLambda:
                                          return func (lm core.Lambda) any {
                                            return func () any {
                                              var tx1 any = schemas.ExtendGraphForLambda(tx, lm)
                                              return func () any {
                                                var body any = rewriteWithArgs([]any{}).(func(any) any)(tx1).(func(any) any)(func (v any) any {
                                                  return v.(core.Lambda).Body
                                                }(lm))
                                                return func () any {
                                                  var result any = core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
                                                    return v.(core.Lambda).Parameter
                                                  }(lm).(core.Name), Domain: func (v any) any {
                                                    return v.(core.Lambda).Domain
                                                  }(lm), Body: body.(core.Term)}}}
                                                  return func () any {
                                                    var arty any = termArityWithContext(tx).(func(any) any)(result)
                                                    return expand(false).(func(any) any)(args).(func(any) any)(arty).(func(any) any)(nil).(func(any) any)(result)
                                                  }()
                                                }()
                                              }()
                                            }()
                                          }(v.Value)
                                          case core.FunctionPrimitive:
                                          return func (pn core.Name) any {
                                            return func () any {
                                              var arty any = termArityWithContext(tx).(func(any) any)(term)
                                              return func () any {
                                                var primType any = libmaybes.Map(func (ts core.TypeScheme) any {
                                                  return func (v any) any {
                                                    return v.(core.TypeScheme).Type_
                                                  }(ts)
                                                }).(func(any) any)(libmaps.Lookup(pn).(func(any) any)(libmaps.FromList(liblists.Map(func (_gpt_p graph.Primitive) any {
                                                  return [2]any{func (v any) any {
                                                    return v.(graph.Primitive).Name
                                                  }(_gpt_p), func (v any) any {
                                                    return v.(graph.Primitive).Type_
                                                  }(_gpt_p)}
                                                }).(func(any) any)(libmaps.Elems(func (v any) any {
                                                  return v.(graph.Graph).Primitives
                                                }(tx))))))
                                                return expand(false).(func(any) any)(args).(func(any) any)(arty).(func(any) any)(primType).(func(any) any)(term)
                                              }()
                                            }()
                                          }(v.Value)
                                        }
                                        return nil
                                      }(fn)
                                    }(v.Value)
                                    case core.TermLet:
                                    return func (lt core.Let) any {
                                      return func () any {
                                        var tx1 any = schemas.ExtendGraphForLet(func (_p graph.Graph) func(core.Binding) any {
                                          return func (_ graph.Graph) func(core.Binding) any {
                                            return func (_2 core.Binding) any {
                                              return nil
                                            }
                                          }(_p).(func(core.Binding) any)
                                        }, tx, lt)
                                        return func () any {
                                          mapBinding := func (b core.Binding) any {
                                            return core.Binding{Name: func (v any) any {
                                              return v.(core.Binding).Name
                                            }(b).(core.Name), Term: rewriteWithArgs([]any{}).(func(any) any)(tx1).(func(any) any)(func (v any) any {
                                              return v.(core.Binding).Term
                                            }(b)).(core.Term), Type_: func (v any) any {
                                              return v.(core.Binding).Type_
                                            }(b)}
                                          }
                                          return func () any {
                                            var result any = core.TermLet{Value: core.Let{Bindings: liblists.Map(mapBinding).(func(any) any)(func (v any) any {
                                              return v.(core.Let).Bindings
                                            }(lt)).([]any), Body: rewriteWithArgs([]any{}).(func(any) any)(tx1).(func(any) any)(func (v any) any {
                                              return v.(core.Let).Body
                                            }(lt)).(core.Term)}}
                                            return afterRecursion(result.(core.Term))
                                          }()
                                        }()
                                      }()
                                    }(v.Value)
                                    case core.TermList:
                                    return func (els []any) any {
                                      return afterRecursion(core.TermList{Value: liblists.Map(func (el core.Term) any {
                                        return recurse(tx).(func(any) any)(el)
                                      }).(func(any) any)(els).([]any)})
                                    }(v.Value)
                                    case core.TermLiteral:
                                    return func (v core.Literal) any {
                                      return core.TermLiteral{Value: v}
                                    }(v.Value)
                                    case core.TermMap_:
                                    return func (mp []any) any {
                                      return afterRecursion(core.TermMap_{Value: forMap(mp).([]any)})
                                    }(v.Value)
                                    case core.TermMaybe:
                                    return func (mb any) any {
                                      return afterRecursion(core.TermMaybe{Value: libmaybes.Map(func (v core.Term) any {
                                        return recurse(tx).(func(any) any)(v)
                                      }).(func(any) any)(mb)})
                                    }(v.Value)
                                    case core.TermPair:
                                    return func (pr any) any {
                                      return afterRecursion(core.TermPair{Value: [2]any{recurse(tx).(func(any) any)(libpairs.First(pr)), recurse(tx).(func(any) any)(libpairs.Second(pr))}})
                                    }(v.Value)
                                    case core.TermRecord:
                                    return func (rc core.Record) any {
                                      return afterRecursion(core.TermRecord{Value: core.Record{TypeName: func (v any) any {
                                        return v.(core.Record).TypeName
                                      }(rc).(core.Name), Fields: liblists.Map(forField).(func(any) any)(func (v any) any {
                                        return v.(core.Record).Fields
                                      }(rc)).([]any)}})
                                    }(v.Value)
                                    case core.TermSet:
                                    return func (st []any) any {
                                      return afterRecursion(core.TermSet{Value: libsets.FromList(liblists.Map(func (el core.Term) any {
                                        return recurse(tx).(func(any) any)(el)
                                      }).(func(any) any)(libsets.ToList(st))).([]any)})
                                    }(v.Value)
                                    case core.TermTypeApplication:
                                    return func (tt core.TypeApplicationTerm) any {
                                      return afterRecursion(core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: recurse(tx).(func(any) any)(func (v any) any {
                                        return v.(core.TypeApplicationTerm).Body
                                      }(tt)).(core.Term), Type_: func (v any) any {
                                        return v.(core.TypeApplicationTerm).Type_
                                      }(tt).(core.Type)}})
                                    }(v.Value)
                                    case core.TermTypeLambda:
                                    return func (tl core.TypeLambda) any {
                                      return func () any {
                                        var tx1 any = schemas.ExtendGraphForTypeLambda(tx, tl)
                                        return func () any {
                                          var result any = core.TermTypeLambda{Value: core.TypeLambda{Parameter: func (v any) any {
                                            return v.(core.TypeLambda).Parameter
                                          }(tl).(core.Name), Body: rewriteWithArgs([]any{}).(func(any) any)(tx1).(func(any) any)(func (v any) any {
                                            return v.(core.TypeLambda).Body
                                          }(tl)).(core.Term)}}
                                          return afterRecursion(result.(core.Term))
                                        }()
                                      }()
                                    }(v.Value)
                                    case core.TermUnion:
                                    return func (inj core.Injection) any {
                                      return afterRecursion(core.TermUnion{Value: core.Injection{TypeName: func (v any) any {
                                        return v.(core.Injection).TypeName
                                      }(inj).(core.Name), Field: forField(func (v any) any {
                                        return v.(core.Injection).Field
                                      }(inj).(core.Field)).(core.Field)}})
                                    }(v.Value)
                                    case core.TermUnit:
                                    return func (_ struct{}) any {
                                      return core.TermUnit{}
                                    }(v)
                                    case core.TermVariable:
                                    return func (vn core.Name) any {
                                      return func () any {
                                        var arty any = termArityWithContext(tx).(func(any) any)(term)
                                        return func () any {
                                          var varType any = libmaybes.Map(rewriting.TypeSchemeToFType).(func(any) any)(libmaps.Lookup(vn).(func(any) any)(func (v any) any {
                                            return v.(graph.Graph).BoundTypes
                                          }(tx)))
                                          return expand(false).(func(any) any)(args).(func(any) any)(arty).(func(any) any)(varType).(func(any) any)(term)
                                        }()
                                      }()
                                    }(v.Value)
                                    case core.TermWrap:
                                    return func (wt core.WrappedTerm) any {
                                      return afterRecursion(core.TermWrap{Value: core.WrappedTerm{TypeName: func (v any) any {
                                        return v.(core.WrappedTerm).TypeName
                                      }(wt).(core.Name), Body: recurse(tx).(func(any) any)(func (v any) any {
                                        return v.(core.WrappedTerm).Body
                                      }(wt)).(core.Term)}})
                                    }(v.Value)
                                  }
                                  return nil
                                }(term)
                              }()
                            }()
                          }()
                        }()
                      }()
                    }()
                  }()
                }
              }
            }
            return ContractTerm(rewriteWithArgs([]any{}).(func(any) any)(tx0).(func(any) any)(term0).(core.Term))
          }()
        }()
      }()
    }()
  }().(core.Term)
}

func EtaExpansionArity (graph graph.Graph, term core.Term) int32 {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (at core.AnnotatedTerm) any {
        return EtaExpansionArity(graph, func (v any) any {
          return v.(core.AnnotatedTerm).Body
        }(at).(core.Term))
      }(v.Value)
      case core.TermApplication:
      return func (app core.Application) any {
        return libmath.Sub(EtaExpansionArity(graph, func (v any) any {
          return v.(core.Application).Function
        }(app).(core.Term))).(func(any) any)(1)
      }(v.Value)
      case core.TermFunction:
      return func (f core.Function) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.FunctionElimination:
            return func (_ core.Elimination) any {
              return 1
            }(v.Value)
            case core.FunctionLambda:
            return func (_ core.Lambda) any {
              return 0
            }(v.Value)
            case core.FunctionPrimitive:
            return func (name core.Name) any {
              return arity.PrimitiveArity(libmaybes.FromJust(lexical.LookupPrimitive(graph, name)).(graph.Primitive))
            }(v.Value)
          }
          return nil
        }(f)
      }(v.Value)
      case core.TermTypeLambda:
      return func (ta core.TypeLambda) any {
        return EtaExpansionArity(graph, func (v any) any {
          return v.(core.TypeLambda).Body
        }(ta).(core.Term))
      }(v.Value)
      case core.TermTypeApplication:
      return func (tt core.TypeApplicationTerm) any {
        return EtaExpansionArity(graph, func (v any) any {
          return v.(core.TypeApplicationTerm).Body
        }(tt).(core.Term))
      }(v.Value)
      case core.TermVariable:
      return func (name core.Name) any {
        return libmaybes.Maybe(0).(func(any) any)(func (ts core.TypeScheme) any {
          return arity.TypeArity(func (v any) any {
            return v.(core.TypeScheme).Type_
          }(ts).(core.Type))
        }).(func(any) any)(libmaybes.Bind(lexical.LookupElement(graph, name)).(func(any) any)(func (b core.Binding) any {
          return func (v any) any {
            return v.(core.Binding).Type_
          }(b)
        }))
      }(v.Value)
      default:
      return 0
    }
    return nil
  }(term).(int32)
}

func EtaExpandTypedTerm (cx context.Context, tx0 graph.Graph, term0 core.Term) any {
  return func () any {
    var rewrite func(bool) any
    rewrite = func (topLevel bool) any {
      return func (forced bool) any {
        return func (typeArgs []any) any {
          return func (recurse func(graph.Graph) func(core.Term) any) any {
            return func (tx graph.Graph) any {
              return func (term core.Term) any {
                return func () any {
                  var rewriteSpine func(core.Term) any
                  rewriteSpine = func (term2 core.Term) any {
                    return func (x any) any {
                      switch v := x.(type) {
                        case core.TermAnnotated:
                        return func (at core.AnnotatedTerm) any {
                          return libeithers.Bind(rewriteSpine(func (v any) any {
                            return v.(core.AnnotatedTerm).Body
                          }(at).(core.Term))).(func(any) any)(func (body core.Term) any {
                            return func () any {
                              var ann any = func (v any) any {
                                return v.(core.AnnotatedTerm).Annotation
                              }(at)
                              return [2]any{"right", core.TermAnnotated{Value: core.AnnotatedTerm{Body: body, Annotation: ann.([]any)}}}
                            }()
                          })
                        }(v.Value)
                        case core.TermApplication:
                        return func (a core.Application) any {
                          return func () any {
                            var l any = liblogic.IfElse(false).(func(any) any)([]any{core.TypeLiteral{Value: core.LiteralTypeString_{}}}).(func(any) any)([]any{})
                            return libeithers.Bind(rewriteSpine(func (v any) any {
                              return v.(core.Application).Function
                            }(a).(core.Term))).(func(any) any)(func (lhs core.Term) any {
                              return libeithers.Bind(rewrite(true).(func(any) any)(false).(func(any) any)(l).(func(any) any)(recurse).(func(any) any)(tx).(func(any) any)(func (v any) any {
                                return v.(core.Application).Argument
                              }(a))).(func(any) any)(func (rhs core.Term) any {
                                return [2]any{"right", core.TermApplication{Value: core.Application{Function: lhs, Argument: rhs}}}
                              })
                            })
                          }()
                        }(v.Value)
                        case core.TermTypeApplication:
                        return func (tat core.TypeApplicationTerm) any {
                          return libeithers.Bind(rewriteSpine(func (v any) any {
                            return v.(core.TypeApplicationTerm).Body
                          }(tat).(core.Term))).(func(any) any)(func (body core.Term) any {
                            return func () any {
                              var typ any = func (v any) any {
                                return v.(core.TypeApplicationTerm).Type_
                              }(tat)
                              return [2]any{"right", core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: body, Type_: typ.(core.Type)}}}
                            }()
                          })
                        }(v.Value)
                        default:
                        return rewrite(false).(func(any) any)(false).(func(any) any)([]any{}).(func(any) any)(recurse).(func(any) any)(tx).(func(any) any)(term2)
                      }
                      return nil
                    }(term2)
                  }
                  return func () any {
                    var arityOf func(graph.Graph) any
                    arityOf = func (tx2 graph.Graph) any {
                      return func (term2 core.Term) any {
                        return func () any {
                          var dflt any = libeithers.Map(func (_tc any) any {
                            return arity.TypeArity(libpairs.First(_tc).(core.Type))
                          }).(func(any) any)(checking.TypeOf(cx, tx2, []any{}, term2))
                          return func () any {
                            forFunction := func (tx3 graph.Graph) any {
                              return func (f core.Function) any {
                                return func (x any) any {
                                  switch v := x.(type) {
                                    case core.FunctionElimination:
                                    return func (_ core.Elimination) any {
                                      return [2]any{"right", 1}
                                    }(v.Value)
                                    case core.FunctionLambda:
                                    return func (l core.Lambda) any {
                                      return func () any {
                                        var txl any = schemas.ExtendGraphForLambda(tx3, l)
                                        return arityOf(txl.(graph.Graph)).(func(any) any)(func (v any) any {
                                          return v.(core.Lambda).Body
                                        }(l))
                                      }()
                                    }(v.Value)
                                    case core.FunctionPrimitive:
                                    return func (name core.Name) any {
                                      return libeithers.Map(func (_ts core.TypeScheme) any {
                                        return arity.TypeSchemeArity(_ts)
                                      }).(func(any) any)(lexical.RequirePrimitiveType(cx, tx3, name))
                                    }(v.Value)
                                  }
                                  return nil
                                }(f)
                              }
                            }
                            return func (x any) any {
                              switch v := x.(type) {
                                case core.TermAnnotated:
                                return func (at core.AnnotatedTerm) any {
                                  return arityOf(tx2).(func(any) any)(func (v any) any {
                                    return v.(core.AnnotatedTerm).Body
                                  }(at))
                                }(v.Value)
                                case core.TermFunction:
                                return func (f core.Function) any {
                                  return forFunction(tx2).(func(any) any)(f)
                                }(v.Value)
                                case core.TermLet:
                                return func (l core.Let) any {
                                  return func () any {
                                    var txl any = schemas.ExtendGraphForLet(func (_p graph.Graph) func(core.Binding) any {
                                      return func (_ graph.Graph) func(core.Binding) any {
                                        return func (_2 core.Binding) any {
                                          return nil
                                        }
                                      }(_p).(func(core.Binding) any)
                                    }, tx2, l)
                                    return arityOf(txl.(graph.Graph)).(func(any) any)(func (v any) any {
                                      return v.(core.Let).Body
                                    }(l))
                                  }()
                                }(v.Value)
                                case core.TermTypeApplication:
                                return func (tat core.TypeApplicationTerm) any {
                                  return arityOf(tx2).(func(any) any)(func (v any) any {
                                    return v.(core.TypeApplicationTerm).Body
                                  }(tat))
                                }(v.Value)
                                case core.TermTypeLambda:
                                return func (tl core.TypeLambda) any {
                                  return func () any {
                                    var txt any = schemas.ExtendGraphForTypeLambda(tx2, tl)
                                    return arityOf(txt.(graph.Graph)).(func(any) any)(func (v any) any {
                                      return v.(core.TypeLambda).Body
                                    }(tl))
                                  }()
                                }(v.Value)
                                case core.TermVariable:
                                return func (name core.Name) any {
                                  return libmaybes.Maybe(libeithers.Map(func (_tc any) any {
                                    return arity.TypeArity(libpairs.First(_tc).(core.Type))
                                  }).(func(any) any)(checking.TypeOf(cx, tx2, []any{}, core.TermVariable{Value: name}))).(func(any) any)(func (t core.Type) any {
                                    return [2]any{"right", arity.TypeArity(t)}
                                  }).(func(any) any)(libmaybes.Map(rewriting.TypeSchemeToFType).(func(any) any)(libmaps.Lookup(name).(func(any) any)(func (v any) any {
                                    return v.(graph.Graph).BoundTypes
                                  }(tx2))))
                                }(v.Value)
                                default:
                                return dflt
                              }
                              return nil
                            }(term2)
                          }()
                        }()
                      }
                    }
                    return func () any {
                      extraVariables := func (n int32) any {
                        return liblists.Map(func (i int32) any {
                          return core.Name(libstrings.Cat2("v").(func(any) any)(libliterals.ShowInt32(i)).(string))
                        }).(func(any) any)(libmath.Range(1).(func(any) any)(n))
                      }
                      return func () any {
                        var pad func([]any) any
                        pad = func (vars []any) any {
                          return func (body core.Term) any {
                            return liblogic.IfElse(liblists.Null(vars)).(func(any) any)(body).(func(any) any)(core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: liblists.Head(vars).(core.Name), Domain: nil, Body: pad(liblists.Tail(vars).([]any)).(func(any) any)(core.TermApplication{Value: core.Application{Function: body, Argument: core.TermVariable{Value: liblists.Head(vars).(core.Name)}}}).(core.Term)}}})
                          }
                        }
                        return func () any {
                          padn := func (n int32) any {
                            return func (body core.Term) any {
                              return pad(extraVariables(n).([]any)).(func(any) any)(body)
                            }
                          }
                          return func () any {
                            unwind := func (term2 core.Term) any {
                              return liblists.Foldl(func (e core.Term) any {
                                return func (t core.Type) any {
                                  return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: e, Type_: t}}
                                }
                              }).(func(any) any)(term2).(func(any) any)(typeArgs)
                            }
                            return func () any {
                              forceExpansion := func (t core.Term) any {
                                return libeithers.Bind(checking.TypeOf(cx, tx, []any{}, t)).(func(any) any)(func (typCx any) any {
                                  return func () any {
                                    var arity any = arity.TypeArity(libpairs.First(typCx).(core.Type))
                                    return [2]any{"right", padn(arity.(int32)).(func(any) any)(unwind(t))}
                                  }()
                                })
                              }
                              return func () any {
                                recurseOrForce := func (term2 core.Term) any {
                                  return liblogic.IfElse(forced).(func(any) any)(forceExpansion(term2)).(func(any) any)(recurse(tx)(unwind(term2)))
                                }
                                return func () any {
                                  forCase := func (f core.Field) any {
                                    return libeithers.Bind(rewrite(false).(func(any) any)(true).(func(any) any)([]any{}).(func(any) any)(recurse).(func(any) any)(tx).(func(any) any)(func (v any) any {
                                      return v.(core.Field).Term
                                    }(f))).(func(any) any)(func (r core.Term) any {
                                      return [2]any{"right", core.Field{Name: func (v any) any {
                                        return v.(core.Field).Name
                                      }(f).(core.Name), Term: r}}
                                    })
                                  }
                                  return func () any {
                                    forCaseStatement := func (cs core.CaseStatement) any {
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
                                            return libeithers.Bind(libeithers.MapMaybe(func (v1 core.Term) any {
                                              return rewrite(false).(func(any) any)(false).(func(any) any)([]any{}).(func(any) any)(recurse).(func(any) any)(tx).(func(any) any)(v1)
                                            }).(func(any) any)(dflt)).(func(any) any)(func (rdflt any) any {
                                              return libeithers.Bind(libeithers.MapList(forCase).(func(any) any)(cases)).(func(any) any)(func (rcases []any) any {
                                                return [2]any{"right", core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: tname.(core.Name), Default_: rdflt, Cases: rcases}}}}}
                                              })
                                            })
                                          }()
                                        }()
                                      }()
                                    }
                                    return func () any {
                                      forElimination := func (elm core.Elimination) any {
                                        return func () any {
                                          checkBase := func (elm2 core.Elimination) any {
                                            return func (x any) any {
                                              switch v := x.(type) {
                                                case core.EliminationUnion:
                                                return func (cs core.CaseStatement) any {
                                                  return forCaseStatement(cs)
                                                }(v.Value)
                                                default:
                                                return recurse(tx)(term)
                                              }
                                              return nil
                                            }(elm2)
                                          }
                                          return libeithers.Bind(libeithers.Map(unwind).(func(any) any)(checkBase(elm))).(func(any) any)(func (base core.Term) any {
                                            return [2]any{"right", liblogic.IfElse(liblogic.Or(topLevel).(func(any) any)(forced)).(func(any) any)(padn(1).(func(any) any)(base)).(func(any) any)(base)}
                                          })
                                        }()
                                      }
                                      return func (x any) any {
                                        switch v := x.(type) {
                                          case core.TermApplication:
                                          return func (a core.Application) any {
                                            return func () any {
                                              var lhs any = func (v any) any {
                                                return v.(core.Application).Function
                                              }(a)
                                              return func () any {
                                                var rhs any = func (v any) any {
                                                  return v.(core.Application).Argument
                                                }(a)
                                                return libeithers.Bind(rewrite(true).(func(any) any)(false).(func(any) any)([]any{}).(func(any) any)(recurse).(func(any) any)(tx).(func(any) any)(rhs)).(func(any) any)(func (rhs2 core.Term) any {
                                                  return libeithers.Bind(arityOf(tx).(func(any) any)(lhs)).(func(any) any)(func (lhsarity int32) any {
                                                    return libeithers.Bind(rewriteSpine(lhs.(core.Term))).(func(any) any)(func (lhs2 core.Term) any {
                                                      return func () any {
                                                        var a2 any = core.TermApplication{Value: core.Application{Function: lhs2, Argument: rhs2}}
                                                        return [2]any{"right", liblogic.IfElse(libequality.Gt(lhsarity).(func(any) any)(1)).(func(any) any)(padn(libmath.Sub(lhsarity).(func(any) any)(1).(int32)).(func(any) any)(a2)).(func(any) any)(a2)}
                                                      }()
                                                    })
                                                  })
                                                })
                                              }()
                                            }()
                                          }(v.Value)
                                          case core.TermFunction:
                                          return func (f core.Function) any {
                                            return func (x any) any {
                                              switch v := x.(type) {
                                                case core.FunctionElimination:
                                                return func (elm core.Elimination) any {
                                                  return forElimination(elm)
                                                }(v.Value)
                                                case core.FunctionLambda:
                                                return func (l core.Lambda) any {
                                                  return func () any {
                                                    var txl any = schemas.ExtendGraphForLambda(tx, l)
                                                    return libeithers.Map(unwind).(func(any) any)(recurse(txl.(graph.Graph))(term))
                                                  }()
                                                }(v.Value)
                                                default:
                                                return recurseOrForce(term)
                                              }
                                              return nil
                                            }(f)
                                          }(v.Value)
                                          case core.TermLet:
                                          return func (l core.Let) any {
                                            return func () any {
                                              var txlt any = schemas.ExtendGraphForLet(func (_p graph.Graph) func(core.Binding) any {
                                                return func (_ graph.Graph) func(core.Binding) any {
                                                  return func (_2 core.Binding) any {
                                                    return nil
                                                  }
                                                }(_p).(func(core.Binding) any)
                                              }, tx, l)
                                              return recurse(txlt.(graph.Graph))(term)
                                            }()
                                          }(v.Value)
                                          case core.TermTypeApplication:
                                          return func (tat core.TypeApplicationTerm) any {
                                            return rewrite(topLevel).(func(any) any)(forced).(func(any) any)(liblists.Cons(func (v any) any {
                                              return v.(core.TypeApplicationTerm).Type_
                                            }(tat)).(func(any) any)(typeArgs)).(func(any) any)(recurse).(func(any) any)(tx).(func(any) any)(func (v any) any {
                                              return v.(core.TypeApplicationTerm).Body
                                            }(tat))
                                          }(v.Value)
                                          case core.TermTypeLambda:
                                          return func (tl core.TypeLambda) any {
                                            return func () any {
                                              var txt any = schemas.ExtendGraphForTypeLambda(tx, tl)
                                              return recurse(txt.(graph.Graph))(term)
                                            }()
                                          }(v.Value)
                                          default:
                                          return recurseOrForce(term)
                                        }
                                        return nil
                                      }(term)
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
              }
            }
          }
        }
      }
    }
    return rewriting.RewriteTermWithContextM(func (_p func(graph.Graph) func(core.Term) any) func(graph.Graph) func(core.Term) any {
      return func (v1 func(graph.Graph) func(core.Term) any) func(graph.Graph) func(core.Term) any {
        return func (v2 graph.Graph) func(core.Term) any {
          return func (v3 core.Term) any {
            return rewrite(true).(func(any) any)(false).(func(any) any)([]any{}).(func(any) any)(v1).(func(any) any)(v2).(func(any) any)(v3)
          }
        }
      }(_p).(func(graph.Graph) func(core.Term) any)
    }, tx0, term0)
  }()
}

func ReduceTerm (cx context.Context, graph graph.Graph, eager bool, term core.Term) any {
  return func () any {
    reduce := func (eager2 bool) any {
      return func (v1 core.Term) any {
        return ReduceTerm(cx, graph, eager2, v1)
      }
    }
    return func () any {
      doRecurse := func (eager2 bool) any {
        return func (term2 core.Term) any {
          return func () any {
            isNonLambda := func (f core.Function) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.FunctionLambda:
                  return func (_ core.Lambda) any {
                    return false
                  }(v.Value)
                  default:
                  return true
                }
                return nil
              }(f)
            }
            return func () any {
              var isNonLambdaTerm any = func (x any) any {
                switch v := x.(type) {
                  case core.TermFunction:
                  return func (f core.Function) any {
                    return isNonLambda(f)
                  }(v.Value)
                  case core.TermLet:
                  return func (_ core.Let) any {
                    return false
                  }(v.Value)
                  default:
                  return true
                }
                return nil
              }(term2)
              return liblogic.And(eager2).(func(any) any)(isNonLambdaTerm)
            }()
          }()
        }
      }
      return func () any {
        reduceArg := func (eager2 bool) any {
          return func (arg core.Term) any {
            return liblogic.IfElse(eager2).(func(any) any)([2]any{"right", arg}).(func(any) any)(reduce(false).(func(any) any)(arg))
          }
        }
        return func () any {
          var applyToArguments func(core.Term) any
          applyToArguments = func (fun core.Term) any {
            return func (args []any) any {
              return liblogic.IfElse(liblists.Null(args)).(func(any) any)(fun).(func(any) any)(applyToArguments(core.TermApplication{Value: core.Application{Function: fun, Argument: liblists.Head(args).(core.Term)}}).(func(any) any)(liblists.Tail(args)))
            }
          }
          return func () any {
            mapErrorToString := func (ic context.InContext[error.Error]) any {
              return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(showerror.Error_(func (v any) any {
                return v.(context.InContext[error.Error]).Object
              }(ic).(error.Error)))}, Context: func (v any) any {
                return v.(context.InContext[error.Error]).Context
              }(ic).(context.Context)}
            }
            return func () any {
              applyElimination := func (elm core.Elimination) any {
                return func (reducedArg core.Term) any {
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.EliminationRecord:
                      return func (proj core.Projection) any {
                        return libeithers.Bind(extractcore.Record(cx, func (v any) any {
                          return v.(core.Projection).TypeName
                        }(proj).(core.Name), graph, rewriting.DeannotateTerm(reducedArg))).(func(any) any)(func (fields []any) any {
                          return func () any {
                            var matchingFields any = liblists.Filter(func (f core.Field) any {
                              return libequality.Equal(func (v any) any {
                                return v.(core.Field).Name
                              }(f)).(func(any) any)(func (v any) any {
                                return v.(core.Projection).Field
                              }(proj))
                            }).(func(any) any)(fields)
                            return liblogic.IfElse(liblists.Null(matchingFields)).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat([]any{"no such field: ", func (v any) any {
                              return v.(core.Projection).Field
                            }(proj), " in ", func (v any) any {
                              return v.(core.Projection).TypeName
                            }(proj), " record"}).(string))}, Context: cx}}).(func(any) any)([2]any{"right", liblists.Head(matchingFields).(core.Field).Term})
                          }()
                        })
                      }(v.Value)
                      case core.EliminationUnion:
                      return func (cs core.CaseStatement) any {
                        return libeithers.Bind(extractcore.Injection(cx, func (v any) any {
                          return v.(core.CaseStatement).TypeName
                        }(cs).(core.Name), graph, reducedArg)).(func(any) any)(func (field core.Field) any {
                          return func () any {
                            var matchingFields any = liblists.Filter(func (f core.Field) any {
                              return libequality.Equal(func (v any) any {
                                return v.(core.Field).Name
                              }(f)).(func(any) any)(func (v any) any {
                                return v.(core.Field).Name
                              }(field))
                            }).(func(any) any)(func (v any) any {
                              return v.(core.CaseStatement).Cases
                            }(cs))
                            return liblogic.IfElse(liblists.Null(matchingFields)).(func(any) any)(libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat([]any{"no such field ", func (v any) any {
                              return v.(core.Field).Name
                            }(field), " in ", func (v any) any {
                              return v.(core.CaseStatement).TypeName
                            }(cs), " case statement"}).(string))}, Context: cx}}).(func(any) any)(func (x core.Term) any {
                              return [2]any{"right", x}
                            }).(func(any) any)(func (v any) any {
                              return v.(core.CaseStatement).Default_
                            }(cs))).(func(any) any)([2]any{"right", core.TermApplication{Value: core.Application{Function: liblists.Head(matchingFields).(core.Field).Term, Argument: func (v any) any {
                              return v.(core.Field).Term
                            }(field).(core.Term)}}})
                          }()
                        })
                      }(v.Value)
                      case core.EliminationWrap:
                      return func (name core.Name) any {
                        return extractcore.Wrap(cx, name, graph, reducedArg)
                      }(v.Value)
                    }
                    return nil
                  }(elm)
                }
              }
              return func () any {
                var applyIfNullary func(bool) any
                applyIfNullary = func (eager2 bool) any {
                  return func (original core.Term) any {
                    return func (args []any) any {
                      return func () any {
                        var stripped any = rewriting.DeannotateTerm(original)
                        return func () any {
                          forElimination := func (elm core.Elimination) any {
                            return func (args2 []any) any {
                              return func () any {
                                var arg any = liblists.Head(args2)
                                return func () any {
                                  var remainingArgs any = liblists.Tail(args2)
                                  return libeithers.Bind(reduceArg(eager2).(func(any) any)(rewriting.DeannotateTerm(arg.(core.Term)))).(func(any) any)(func (reducedArg core.Term) any {
                                    return libeithers.Bind(libeithers.Bind(applyElimination(elm).(func(any) any)(reducedArg)).(func(any) any)(func (v1 core.Term) any {
                                      return reduce(eager2).(func(any) any)(v1)
                                    })).(func(any) any)(func (reducedResult core.Term) any {
                                      return applyIfNullary(eager2).(func(any) any)(reducedResult).(func(any) any)(remainingArgs)
                                    })
                                  })
                                }()
                              }()
                            }
                          }
                          return func () any {
                            forLambda := func (l core.Lambda) any {
                              return func (args2 []any) any {
                                return func () any {
                                  var param any = func (v any) any {
                                    return v.(core.Lambda).Parameter
                                  }(l)
                                  return func () any {
                                    var body any = func (v any) any {
                                      return v.(core.Lambda).Body
                                    }(l)
                                    return func () any {
                                      var arg any = liblists.Head(args2)
                                      return func () any {
                                        var remainingArgs any = liblists.Tail(args2)
                                        return libeithers.Bind(reduce(eager2).(func(any) any)(rewriting.DeannotateTerm(arg.(core.Term)))).(func(any) any)(func (reducedArg core.Term) any {
                                          return libeithers.Bind(reduce(eager2).(func(any) any)(rewriting.ReplaceFreeTermVariable(param.(core.Name), reducedArg, body.(core.Term)))).(func(any) any)(func (reducedResult core.Term) any {
                                            return applyIfNullary(eager2).(func(any) any)(reducedResult).(func(any) any)(remainingArgs)
                                          })
                                        })
                                      }()
                                    }()
                                  }()
                                }()
                              }
                            }
                            return func () any {
                              forPrimitive := func (prim graph.Primitive) any {
                                return func (arity int32) any {
                                  return func (args2 []any) any {
                                    return func () any {
                                      var argList any = liblists.Take(arity).(func(any) any)(args2)
                                      return func () any {
                                        var remainingArgs any = liblists.Drop(arity).(func(any) any)(args2)
                                        return libeithers.Bind(libeithers.MapList(func (v1 core.Term) any {
                                          return reduceArg(eager2).(func(any) any)(v1)
                                        }).(func(any) any)(argList)).(func(any) any)(func (reducedArgs []any) any {
                                          return func () any {
                                            var strippedArgs any = liblists.Map(rewriting.DeannotateTerm).(func(any) any)(reducedArgs)
                                            return libeithers.Bind(libeithers.Bimap(mapErrorToString).(func(any) any)(func (x core.Term) any {
                                              return x
                                            }).(func(any) any)(func (v any) any {
                                              return v.(graph.Primitive).Implementation
                                            }(prim).(func(any) any)(cx).(func(any) any)(graph).(func(any) any)(strippedArgs))).(func(any) any)(func (primResult core.Term) any {
                                              return libeithers.Bind(reduce(eager2).(func(any) any)(primResult)).(func(any) any)(func (reducedResult core.Term) any {
                                                return applyIfNullary(eager2).(func(any) any)(reducedResult).(func(any) any)(remainingArgs)
                                              })
                                            })
                                          }()
                                        })
                                      }()
                                    }()
                                  }
                                }
                              }
                              return func (x any) any {
                                switch v := x.(type) {
                                  case core.TermApplication:
                                  return func (app core.Application) any {
                                    return applyIfNullary(eager2).(func(any) any)(func (v any) any {
                                      return v.(core.Application).Function
                                    }(app)).(func(any) any)(liblists.Cons(func (v any) any {
                                      return v.(core.Application).Argument
                                    }(app)).(func(any) any)(args))
                                  }(v.Value)
                                  case core.TermFunction:
                                  return func (v1 core.Function) any {
                                    return func (x any) any {
                                      switch v := x.(type) {
                                        case core.FunctionElimination:
                                        return func (elm core.Elimination) any {
                                          return liblogic.IfElse(liblists.Null(args)).(func(any) any)([2]any{"right", original}).(func(any) any)(forElimination(elm).(func(any) any)(args))
                                        }(v.Value)
                                        case core.FunctionLambda:
                                        return func (l core.Lambda) any {
                                          return liblogic.IfElse(liblists.Null(args)).(func(any) any)([2]any{"right", original}).(func(any) any)(forLambda(l).(func(any) any)(args))
                                        }(v.Value)
                                        case core.FunctionPrimitive:
                                        return func (name core.Name) any {
                                          return libeithers.Bind(lexical.RequirePrimitive(cx, graph, name)).(func(any) any)(func (prim graph.Primitive) any {
                                            return func () any {
                                              var arity any = arity.PrimitiveArity(prim)
                                              return liblogic.IfElse(libequality.Gt(arity).(func(any) any)(liblists.Length(args))).(func(any) any)([2]any{"right", applyToArguments(original).(func(any) any)(args)}).(func(any) any)(forPrimitive(prim).(func(any) any)(arity).(func(any) any)(args))
                                            }()
                                          })
                                        }(v.Value)
                                      }
                                      return nil
                                    }(v1)
                                  }(v.Value)
                                  case core.TermVariable:
                                  return func (v core.Name) any {
                                    return func () any {
                                      var mBinding any = lexical.DereferenceElement(graph, v)
                                      return libmaybes.Maybe([2]any{"right", applyToArguments(original).(func(any) any)(args)}).(func(any) any)(func (binding core.Binding) any {
                                        return applyIfNullary(eager2).(func(any) any)(func (v any) any {
                                          return v.(core.Binding).Term
                                        }(binding)).(func(any) any)(args)
                                      }).(func(any) any)(mBinding)
                                    }()
                                  }(v.Value)
                                  case core.TermLet:
                                  return func (lt core.Let) any {
                                    return func () any {
                                      var bindings any = func (v any) any {
                                        return v.(core.Let).Bindings
                                      }(lt)
                                      return func () any {
                                        var body any = func (v any) any {
                                          return v.(core.Let).Body
                                        }(lt)
                                        return func () any {
                                          letExpr := func (b core.Binding) any {
                                            return core.TermLet{Value: core.Let{Bindings: []any{b}, Body: core.TermVariable{Value: func (v any) any {
                                              return v.(core.Binding).Name
                                            }(b).(core.Name)}}}
                                          }
                                          return func () any {
                                            expandBinding := func (b core.Binding) any {
                                              return core.Binding{Name: func (v any) any {
                                                return v.(core.Binding).Name
                                              }(b).(core.Name), Term: rewriting.ReplaceFreeTermVariable(func (v any) any {
                                                return v.(core.Binding).Name
                                              }(b).(core.Name), letExpr(b).(core.Term), func (v any) any {
                                                return v.(core.Binding).Term
                                              }(b).(core.Term)), Type_: func (v any) any {
                                                return v.(core.Binding).Type_
                                              }(b)}
                                            }
                                            return func () any {
                                              var expandedBindings any = liblists.Map(expandBinding).(func(any) any)(bindings)
                                              return func () any {
                                                substituteBinding := func (term2 core.Term) any {
                                                  return func (b core.Binding) any {
                                                    return rewriting.ReplaceFreeTermVariable(func (v any) any {
                                                      return v.(core.Binding).Name
                                                    }(b).(core.Name), func (v any) any {
                                                      return v.(core.Binding).Term
                                                    }(b).(core.Term), term2)
                                                  }
                                                }
                                                return func () any {
                                                  substituteAll := func (bs []any) any {
                                                    return func (term2 core.Term) any {
                                                      return liblists.Foldl(substituteBinding).(func(any) any)(term2).(func(any) any)(bs)
                                                    }
                                                  }
                                                  return func () any {
                                                    var expandedBody any = substituteAll(expandedBindings.([]any)).(func(any) any)(body)
                                                    return libeithers.Bind(reduce(eager2).(func(any) any)(expandedBody)).(func(any) any)(func (reducedBody core.Term) any {
                                                      return applyIfNullary(eager2).(func(any) any)(reducedBody).(func(any) any)(args)
                                                    })
                                                  }()
                                                }()
                                              }()
                                            }()
                                          }()
                                        }()
                                      }()
                                    }()
                                  }(v.Value)
                                  default:
                                  return [2]any{"right", applyToArguments(original).(func(any) any)(args)}
                                }
                                return nil
                              }(stripped)
                            }()
                          }()
                        }()
                      }()
                    }
                  }
                }
                return func () any {
                  mapping := func (recurse func(core.Term) any) any {
                    return func (mid core.Term) any {
                      return libeithers.Bind(liblogic.IfElse(doRecurse(eager).(func(any) any)(mid)).(func(any) any)(recurse(mid)).(func(any) any)([2]any{"right", mid})).(func(any) any)(func (inner core.Term) any {
                        return applyIfNullary(eager).(func(any) any)(inner).(func(any) any)([]any{})
                      })
                    }
                  }
                  return rewriting.RewriteTermM(func (_p func(core.Term) any) func(core.Term) any {
                    return mapping(_p).(func(core.Term) any)
                  }, term)
                }()
              }()
            }()
          }()
        }()
      }()
    }()
  }()
}

func TermIsClosed (term core.Term) bool {
  return libsets.Null(rewriting.FreeVariablesInTerm(term)).(bool)
}

func TermIsValue (term core.Term) bool {
  return func () any {
    forList := func (els []any) any {
      return liblists.Foldl(func (b bool) any {
        return func (t core.Term) any {
          return liblogic.And(b).(func(any) any)(TermIsValue(t))
        }
      }).(func(any) any)(true).(func(any) any)(els)
    }
    return func () any {
      checkField := func (f core.Field) any {
        return TermIsValue(func (v any) any {
          return v.(core.Field).Term
        }(f).(core.Term))
      }
      return func () any {
        checkFields := func (fields []any) any {
          return liblists.Foldl(func (b bool) any {
            return func (f core.Field) any {
              return liblogic.And(b).(func(any) any)(checkField(f))
            }
          }).(func(any) any)(true).(func(any) any)(fields)
        }
        return func () any {
          functionIsValue := func (f core.Function) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.FunctionElimination:
                return func (e core.Elimination) any {
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.EliminationWrap:
                      return func (_ core.Name) any {
                        return true
                      }(v.Value)
                      case core.EliminationRecord:
                      return func (_ core.Projection) any {
                        return true
                      }(v.Value)
                      case core.EliminationUnion:
                      return func (cs core.CaseStatement) any {
                        return liblogic.And(checkFields(func (v any) any {
                          return v.(core.CaseStatement).Cases
                        }(cs).([]any))).(func(any) any)(libmaybes.Maybe(true).(func(any) any)(TermIsValue).(func(any) any)(func (v any) any {
                          return v.(core.CaseStatement).Default_
                        }(cs)))
                      }(v.Value)
                    }
                    return nil
                  }(e)
                }(v.Value)
                case core.FunctionLambda:
                return func (l core.Lambda) any {
                  return TermIsValue(func (v any) any {
                    return v.(core.Lambda).Body
                  }(l).(core.Term))
                }(v.Value)
                case core.FunctionPrimitive:
                return func (_ core.Name) any {
                  return true
                }(v.Value)
              }
              return nil
            }(f)
          }
          return func (x any) any {
            switch v := x.(type) {
              case core.TermApplication:
              return func (_ core.Application) any {
                return false
              }(v.Value)
              case core.TermEither:
              return func (e any) any {
                return libeithers.Either(func (l core.Term) any {
                  return TermIsValue(l)
                }).(func(any) any)(func (r core.Term) any {
                  return TermIsValue(r)
                }).(func(any) any)(e)
              }(v.Value)
              case core.TermLiteral:
              return func (_ core.Literal) any {
                return true
              }(v.Value)
              case core.TermFunction:
              return func (f core.Function) any {
                return functionIsValue(f)
              }(v.Value)
              case core.TermList:
              return func (els []any) any {
                return forList(els)
              }(v.Value)
              case core.TermMap_:
              return func (m []any) any {
                return liblists.Foldl(func (b bool) any {
                  return func (kv any) any {
                    return liblogic.And(b).(func(any) any)(liblogic.And(TermIsValue(libpairs.First(kv).(core.Term))).(func(any) any)(TermIsValue(libpairs.Second(kv).(core.Term))))
                  }
                }).(func(any) any)(true).(func(any) any)(libmaps.ToList(m))
              }(v.Value)
              case core.TermMaybe:
              return func (m any) any {
                return libmaybes.Maybe(true).(func(any) any)(TermIsValue).(func(any) any)(m)
              }(v.Value)
              case core.TermRecord:
              return func (r core.Record) any {
                return checkFields(func (v any) any {
                  return v.(core.Record).Fields
                }(r).([]any))
              }(v.Value)
              case core.TermSet:
              return func (s []any) any {
                return forList(libsets.ToList(s).([]any))
              }(v.Value)
              case core.TermUnion:
              return func (i core.Injection) any {
                return checkField(func (v any) any {
                  return v.(core.Injection).Field
                }(i).(core.Field))
              }(v.Value)
              case core.TermUnit:
              return func (_ struct{}) any {
                return true
              }(v)
              case core.TermVariable:
              return func (_ core.Name) any {
                return false
              }(v.Value)
              default:
              return false
            }
            return nil
          }(rewriting.DeannotateTerm(term))
        }()
      }()
    }()
  }().(bool)
}
