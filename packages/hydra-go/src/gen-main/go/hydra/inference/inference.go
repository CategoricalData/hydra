// Note: this is an automatically generated file. Do not edit.

package inference

import (
  "hydra.dev/hydra/annotations"
  "hydra.dev/hydra/checking"
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
  libmath "hydra.dev/hydra/lib/math"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/reflect"
  "hydra.dev/hydra/rewriting"
  "hydra.dev/hydra/schemas"
  showcore "hydra.dev/hydra/show/core"
  showtyping "hydra.dev/hydra/show/typing"
  "hydra.dev/hydra/sorting"
  "hydra.dev/hydra/substitution"
  "hydra.dev/hydra/typing"
  "hydra.dev/hydra/unification"
)

func BindConstraints (flowCx context.Context, cx graph.Graph, constraints []any) any {
  return libeithers.Bind(libeithers.Bimap(func (_ic context.InContext[error.UnificationError]) any {
    return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(func (v any) any {
      return v.(context.InContext[error.Error]).Object
    }(_ic).(error.UnificationError).Message)}, Context: func (v any) any {
      return v.(context.InContext[error.Error]).Context
    }(_ic).(context.Context)}
  }).(func(any) any)(func (_a typing.TypeSubst) any {
    return _a
  }).(func(any) any)(unification.UnifyTypeConstraints(flowCx, func (v any) any {
    return v.(graph.Graph).SchemaTypes
  }(cx).([]any), constraints))).(func(any) any)(func (s typing.TypeSubst) any {
    return libeithers.Bind(checking.CheckTypeSubst(flowCx, cx, s)).(func(any) any)(func (_ typing.TypeSubst) any {
      return [2]any{"right", s}
    })
  })
}

func BindUnboundTypeVariables (cx graph.Graph, term0 core.Term) core.Term {
  return func () any {
    var svars any = libsets.FromList(libmaps.Keys(func (v any) any {
      return v.(graph.Graph).SchemaTypes
    }(cx)))
    return func () any {
      rewrite := func (recurse func(core.Term) core.Term) any {
        return func (term core.Term) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.TermLet:
              return func (l core.Let) any {
                return func () any {
                  forBinding := func (b core.Binding) any {
                    return func () any {
                      var bname any = func (v any) any {
                        return v.(core.Binding).Name
                      }(b)
                      return func () any {
                        var bterm any = func (v any) any {
                          return v.(core.Binding).Term
                        }(b)
                        return libmaybes.Maybe(core.Binding{Name: bname.(core.Name), Term: BindUnboundTypeVariables(cx, bterm.(core.Term)), Type_: nil}).(func(any) any)(func (ts core.TypeScheme) any {
                          return func () any {
                            var bvars any = libsets.FromList(func (v any) any {
                              return v.(core.TypeScheme).Variables
                            }(ts))
                            return func () any {
                              var unboundInType any = rewriting.FreeVariablesInType(func (v any) any {
                                return v.(core.TypeScheme).Type_
                              }(ts).(core.Type))
                              return func () any {
                                var unboundInTerm any = rewriting.FreeTypeVariablesInTerm(bterm.(core.Term))
                                return func () any {
                                  var unbound any = libsets.ToList(libsets.Difference(libsets.Union(unboundInType).(func(any) any)(unboundInTerm)).(func(any) any)(libsets.Union(svars).(func(any) any)(bvars)))
                                  return func () any {
                                    var ts2 any = core.TypeScheme{Variables: liblists.Concat2(func (v any) any {
                                      return v.(core.TypeScheme).Variables
                                    }(ts)).(func(any) any)(unbound).([]any), Type_: func (v any) any {
                                      return v.(core.TypeScheme).Type_
                                    }(ts).(core.Type), Constraints: func (v any) any {
                                      return v.(core.TypeScheme).Constraints
                                    }(ts)}
                                    return func () any {
                                      var bterm2 any = liblists.Foldl(func (t core.Term) any {
                                        return func (v core.Name) any {
                                          return core.TermTypeLambda{Value: core.TypeLambda{Parameter: v, Body: t}}
                                        }
                                      }).(func(any) any)(bterm).(func(any) any)(unbound)
                                      return core.Binding{Name: bname.(core.Name), Term: bterm2.(core.Term), Type_: func () any {
                                        _v := ts2
                                        return &_v
                                      }()}
                                    }()
                                  }()
                                }()
                              }()
                            }()
                          }()
                        }).(func(any) any)(func (v any) any {
                          return v.(core.Binding).Type_
                        }(b))
                      }()
                    }()
                  }
                  return core.TermLet{Value: core.Let{Bindings: liblists.Map(forBinding).(func(any) any)(func (v any) any {
                    return v.(core.Let).Bindings
                  }(l)).([]any), Body: BindUnboundTypeVariables(cx, func (v any) any {
                    return v.(core.Let).Body
                  }(l).(core.Term))}}
                }()
              }(v.Value)
              default:
              return recurse(term)
            }
            return nil
          }(term)
        }
      }
      return rewriting.RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
        return rewrite(_p).(func(core.Term) core.Term)
      }, term0)
    }()
  }().(core.Term)
}

func BuildTypeApplicationTerm (tvars []any, body core.Term) core.Term {
  return liblists.Foldl(func (t core.Term) any {
    return func (v core.Name) any {
      return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: t, Type_: core.TypeVariable{Value: v}}}
    }
  }).(func(any) any)(body).(func(any) any)(tvars).(core.Term)
}

func ExtendContext (pairs []any, cx graph.Graph) graph.Graph {
  return graph.Graph{BoundTerms: func (v any) any {
    return v.(graph.Graph).BoundTerms
  }(cx).([]any), BoundTypes: libmaps.Union(libmaps.FromList(pairs)).(func(any) any)(func (v any) any {
    return v.(graph.Graph).BoundTypes
  }(cx)).([]any), ClassConstraints: func (v any) any {
    return v.(graph.Graph).ClassConstraints
  }(cx).([]any), LambdaVariables: func (v any) any {
    return v.(graph.Graph).LambdaVariables
  }(cx).([]any), Metadata: func (v any) any {
    return v.(graph.Graph).Metadata
  }(cx).([]any), Primitives: func (v any) any {
    return v.(graph.Graph).Primitives
  }(cx).([]any), SchemaTypes: func (v any) any {
    return v.(graph.Graph).SchemaTypes
  }(cx).([]any), TypeVariables: func (v any) any {
    return v.(graph.Graph).TypeVariables
  }(cx).([]any)}
}

func FinalizeInferredTerm (flowCx context.Context, cx graph.Graph, term core.Term) any {
  return func () any {
    var term2 any = BindUnboundTypeVariables(cx, term)
    return libeithers.Bind(checking.CheckForUnboundTypeVariables(flowCx, cx, term2.(core.Term))).(func(any) any)(func (_ struct{}) any {
      return [2]any{"right", rewriting.NormalizeTypeVariablesInTerm(term2.(core.Term))}
    })
  }()
}

func ForInferredTerm[T0 any] (fcx context.Context, cx graph.Graph, term core.Term, desc string, f func(typing.InferenceResult) T0) any {
  return libeithers.Bind(InferTypeOfTerm(fcx, cx, term, desc)).(func(any) any)(func (rp typing.InferenceResult) any {
    return [2]any{"right", [2]any{f(rp), func (v any) any {
      return v.(typing.InferenceResult).Context
    }(rp)}}
  })
}

func FreeVariablesInContext (cx graph.Graph) []any {
  return liblists.Foldl(libsets.Union).(func(any) any)(libsets.Empty).(func(any) any)(liblists.Map(rewriting.FreeVariablesInTypeSchemeSimple).(func(any) any)(libmaps.Elems(func (v any) any {
    return v.(graph.Graph).BoundTypes
  }(cx)))).([]any)
}

func FreshVariableType (cx context.Context) any {
  return func () any {
    var result any = schemas.FreshName(cx)
    return func () any {
      var name any = libpairs.First(result)
      return func () any {
        var cx2 any = libpairs.Second(result)
        return [2]any{core.TypeVariable{Value: name.(core.Name)}, cx2}
      }()
    }()
  }()
}

func Generalize (cx graph.Graph, typ core.Type) core.TypeScheme {
  return func () any {
    isTypeVarName := func (name core.Name) any {
      return func () any {
        var parts any = libstrings.SplitOn(".").(func(any) any)(func (v any) any {
          return v
        }(name))
        return libequality.Lte(liblists.Length(parts)).(func(any) any)(1)
      }()
    }
    return func () any {
      var vars any = liblists.Nub(liblists.Filter(func (v core.Name) any {
        return liblogic.And(IsUnbound(cx, v)).(func(any) any)(isTypeVarName(v))
      }).(func(any) any)(rewriting.FreeVariablesInTypeOrdered(typ)))
      return func () any {
        var allConstraints any = func (v any) any {
          return v.(graph.Graph).ClassConstraints
        }(cx)
        return func () any {
          var relevantConstraints any = libmaps.FromList(libmaybes.Cat(liblists.Map(func (v core.Name) any {
            return libmaybes.Map(func (meta core.TypeVariableMetadata) any {
              return [2]any{v, meta}
            }).(func(any) any)(libmaps.Lookup(v).(func(any) any)(allConstraints))
          }).(func(any) any)(vars)))
          return func () any {
            var constraintsMaybe any = liblogic.IfElse(libmaps.Null(relevantConstraints)).(func(any) any)(nil).(func(any) any)(func () any {
              _v := relevantConstraints
              return &_v
            }())
            return core.TypeScheme{Variables: vars.([]any), Type_: typ, Constraints: constraintsMaybe}
          }()
        }()
      }()
    }()
  }().(core.TypeScheme)
}

func InferGraphTypes (fcx0 context.Context, bindings0 []any, g0 graph.Graph) any {
  return func () any {
    var fcx any = context.Context{Trace: liblists.Cons("graph inference").(func(any) any)(func (v any) any {
      return v.(context.Context).Trace
    }(fcx0)).([]any), Messages: func (v any) any {
      return v.(context.Context).Messages
    }(fcx0).([]any), Other: func (v any) any {
      return v.(context.Context).Other
    }(fcx0).([]any)}
    return func () any {
      var let0 any = core.Let{Bindings: bindings0, Body: core.TermUnit{}}
      return func () any {
        fromLetTerm := func (l core.Let) any {
          return func () any {
            var bindings any = func (v any) any {
              return v.(core.Let).Bindings
            }(l)
            return func () any {
              var prims any = func (v any) any {
                return v.(graph.Graph).Primitives
              }(g0)
              return func () any {
                var schemaTypes any = func (v any) any {
                  return v.(graph.Graph).SchemaTypes
                }(g0)
                return func () any {
                  var g any = graph.Graph{BoundTerms: func (v any) any {
                    return v.(graph.Graph).BoundTerms
                  }(lexical.BuildGraph(bindings.([]any), libmaps.Empty, prims.([]any))).([]any), BoundTypes: func (v any) any {
                    return v.(graph.Graph).BoundTypes
                  }(lexical.BuildGraph(bindings.([]any), libmaps.Empty, prims.([]any))).([]any), ClassConstraints: func (v any) any {
                    return v.(graph.Graph).ClassConstraints
                  }(lexical.BuildGraph(bindings.([]any), libmaps.Empty, prims.([]any))).([]any), LambdaVariables: func (v any) any {
                    return v.(graph.Graph).LambdaVariables
                  }(lexical.BuildGraph(bindings.([]any), libmaps.Empty, prims.([]any))).([]any), Metadata: func (v any) any {
                    return v.(graph.Graph).Metadata
                  }(lexical.BuildGraph(bindings.([]any), libmaps.Empty, prims.([]any))).([]any), Primitives: func (v any) any {
                    return v.(graph.Graph).Primitives
                  }(lexical.BuildGraph(bindings.([]any), libmaps.Empty, prims.([]any))).([]any), SchemaTypes: schemaTypes.([]any), TypeVariables: func (v any) any {
                    return v.(graph.Graph).TypeVariables
                  }(lexical.BuildGraph(bindings.([]any), libmaps.Empty, prims.([]any))).([]any)}
                  return [2]any{g, bindings}
                }()
              }()
            }()
          }()
        }
        return libeithers.Bind(InferTypeOfTerm(fcx.(context.Context), g0, core.TermLet{Value: let0.(core.Let)}, "graph term")).(func(any) any)(func (result typing.InferenceResult) any {
          return func () any {
            var fcx2 any = func (v any) any {
              return v.(typing.InferenceResult).Context
            }(result)
            return func () any {
              var term any = func (v any) any {
                return v.(typing.InferenceResult).Term
              }(result)
              return libeithers.Bind(FinalizeInferredTerm(fcx2.(context.Context), g0, term.(core.Term))).(func(any) any)(func (finalized core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermLet:
                    return func (l core.Let) any {
                      return [2]any{"right", [2]any{fromLetTerm(l), fcx2}}
                    }(v.Value)
                    case core.TermVariable:
                    return func (_ core.Name) any {
                      return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError("Expected inferred graph as let term")}, Context: fcx2.(context.Context)}}
                    }(v.Value)
                  }
                  return nil
                }(finalized)
              })
            }()
          }()
        })
      }()
    }()
  }()
}

func InferInGraphContext (fcx context.Context, cx graph.Graph, term core.Term) any {
  return InferTypeOfTerm(fcx, cx, term, "single term")
}

func InferMany (fcx context.Context, cx graph.Graph, pairs []any) any {
  return func () any {
    var dflt any = func () any {
      var e any = libpairs.First(liblists.Head(pairs))
      return func () any {
        var desc any = libpairs.Second(liblists.Head(pairs))
        return func () any {
          var tl any = liblists.Tail(pairs)
          return libeithers.Bind(InferTypeOfTerm(fcx, cx, e.(core.Term), desc.(string))).(func(any) any)(func (result1 typing.InferenceResult) any {
            return func () any {
              var fcx2 any = func (v any) any {
                return v.(typing.InferenceResult).Context
              }(result1)
              return func () any {
                var e1 any = func (v any) any {
                  return v.(typing.InferenceResult).Term
                }(result1)
                return func () any {
                  var t1 any = func (v any) any {
                    return v.(typing.InferenceResult).Type_
                  }(result1)
                  return func () any {
                    var s1 any = func (v any) any {
                      return v.(typing.InferenceResult).Subst
                    }(result1)
                    return func () any {
                      var c1 any = func (v any) any {
                        return v.(typing.InferenceResult).ClassConstraints
                      }(result1)
                      return libeithers.Bind(InferMany(fcx2.(context.Context), substitution.SubstInContext(s1.(typing.TypeSubst), cx), tl.([]any))).(func(any) any)(func (rp2 any) any {
                        return func () any {
                          var result2 any = libpairs.First(rp2)
                          return func () any {
                            var fcx3 any = libpairs.Second(rp2)
                            return func () any {
                              var e2 any = libpairs.First(result2)
                              return func () any {
                                var t2 any = libpairs.First(libpairs.Second(result2))
                                return func () any {
                                  var s2 any = libpairs.First(libpairs.Second(libpairs.Second(result2)))
                                  return func () any {
                                    var c2 any = libpairs.Second(libpairs.Second(libpairs.Second(result2)))
                                    return func () any {
                                      var c1Subst any = substitution.SubstInClassConstraints(s2.(typing.TypeSubst), c1.([]any))
                                      return func () any {
                                        var mergedConstraints any = MergeClassConstraints(c1Subst.([]any), c2.([]any))
                                        return [2]any{"right", [2]any{[2]any{liblists.Cons(substitution.SubstTypesInTerm(s2.(typing.TypeSubst), e1.(core.Term))).(func(any) any)(e2), [2]any{liblists.Cons(substitution.SubstInType(s2.(typing.TypeSubst), t1.(core.Type))).(func(any) any)(t2), [2]any{substitution.ComposeTypeSubst(s1.(typing.TypeSubst), s2.(typing.TypeSubst)), mergedConstraints}}}, fcx3}}
                                      }()
                                    }()
                                  }()
                                }()
                              }()
                            }()
                          }()
                        }()
                      })
                    }()
                  }()
                }()
              }()
            }()
          })
        }()
      }()
    }()
    return liblogic.IfElse(liblists.Null(pairs)).(func(any) any)([2]any{"right", [2]any{[2]any{[]any{}, [2]any{[]any{}, [2]any{substitution.IdTypeSubst, libmaps.Empty}}}, fcx}}).(func(any) any)(dflt)
  }()
}

func InferTypeOf (fcx context.Context, cx graph.Graph, term core.Term) any {
  return func () any {
    var letTerm any = core.TermLet{Value: core.Let{Bindings: []any{core.Binding{Name: core.Name("ignoredVariableName"), Term: term, Type_: nil}}, Body: core.TermLiteral{Value: core.LiteralString_{Value: "ignoredBody"}}}}
    return libeithers.Bind(InferTypeOfTerm(fcx, cx, letTerm.(core.Term), "infer type of term")).(func(any) any)(func (result typing.InferenceResult) any {
      return func () any {
        var fcx2 any = func (v any) any {
          return v.(typing.InferenceResult).Context
        }(result)
        return libeithers.Bind(FinalizeInferredTerm(fcx2.(context.Context), cx, func (v any) any {
          return v.(typing.InferenceResult).Term
        }(result).(core.Term))).(func(any) any)(func (finalized core.Term) any {
          return libeithers.Bind(extractcore.Let(fcx2.(context.Context), cx, finalized)).(func(any) any)(func (letResult core.Let) any {
            return func () any {
              var bindings any = func (v any) any {
                return v.(core.Let).Bindings
              }(letResult)
              return liblogic.IfElse(libequality.Equal(1).(func(any) any)(liblists.Length(bindings))).(func(any) any)(func () any {
                var binding any = liblists.Head(bindings)
                return func () any {
                  var term1 any = binding.(core.Binding).Term
                  return func () any {
                    var mts any = binding.(core.Binding).Type_
                    return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError("Expected a type scheme")}, Context: fcx2.(context.Context)}}).(func(any) any)(func (ts core.TypeScheme) any {
                      return [2]any{"right", [2]any{[2]any{term1, ts}, fcx2}}
                    }).(func(any) any)(mts)
                  }()
                }()
              }()).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat([]any{"Expected a single binding with a type scheme, but got: ", libliterals.ShowInt32(liblists.Length(bindings)), " bindings"}).(string))}, Context: fcx2.(context.Context)}})
            }()
          })
        })
      }()
    })
  }()
}

func InferTypeOfAnnotatedTerm (fcx context.Context, cx graph.Graph, at core.AnnotatedTerm) any {
  return func () any {
    var term any = func (v any) any {
      return v.(core.AnnotatedTerm).Body
    }(at)
    return func () any {
      var ann any = func (v any) any {
        return v.(core.AnnotatedTerm).Annotation
      }(at)
      return libeithers.Bind(InferTypeOfTerm(fcx, cx, term.(core.Term), "annotated term")).(func(any) any)(func (result typing.InferenceResult) any {
        return func () any {
          var fcx2 any = func (v any) any {
            return v.(typing.InferenceResult).Context
          }(result)
          return func () any {
            var iterm any = func (v any) any {
              return v.(typing.InferenceResult).Term
            }(result)
            return func () any {
              var itype any = func (v any) any {
                return v.(typing.InferenceResult).Type_
              }(result)
              return func () any {
                var isubst any = func (v any) any {
                  return v.(typing.InferenceResult).Subst
                }(result)
                return func () any {
                  var iconstraints any = func (v any) any {
                    return v.(typing.InferenceResult).ClassConstraints
                  }(result)
                  return [2]any{"right", typing.InferenceResult{Term: core.TermAnnotated{Value: core.AnnotatedTerm{Body: iterm.(core.Term), Annotation: ann.([]any)}}, Type_: itype.(core.Type), Subst: isubst.(typing.TypeSubst), ClassConstraints: iconstraints.([]any), Context: fcx2.(context.Context)}}
                }()
              }()
            }()
          }()
        }()
      })
    }()
  }()
}

func InferTypeOfApplication (fcx0 context.Context, cx graph.Graph, app core.Application) any {
  return func () any {
    var fcx any = context.Context{Trace: liblists.Cons("application").(func(any) any)(func (v any) any {
      return v.(context.Context).Trace
    }(fcx0)).([]any), Messages: func (v any) any {
      return v.(context.Context).Messages
    }(fcx0).([]any), Other: func (v any) any {
      return v.(context.Context).Other
    }(fcx0).([]any)}
    return func () any {
      var e0 any = func (v any) any {
        return v.(core.Application).Function
      }(app)
      return func () any {
        var e1 any = func (v any) any {
          return v.(core.Application).Argument
        }(app)
        return libeithers.Bind(InferTypeOfTerm(fcx.(context.Context), cx, e0.(core.Term), "lhs")).(func(any) any)(func (lhsResult typing.InferenceResult) any {
          return func () any {
            var fcx2 any = func (v any) any {
              return v.(typing.InferenceResult).Context
            }(lhsResult)
            return func () any {
              var a any = func (v any) any {
                return v.(typing.InferenceResult).Term
              }(lhsResult)
              return func () any {
                var t0 any = func (v any) any {
                  return v.(typing.InferenceResult).Type_
                }(lhsResult)
                return func () any {
                  var s0 any = func (v any) any {
                    return v.(typing.InferenceResult).Subst
                  }(lhsResult)
                  return func () any {
                    var c0 any = func (v any) any {
                      return v.(typing.InferenceResult).ClassConstraints
                    }(lhsResult)
                    return libeithers.Bind(InferTypeOfTerm(fcx2.(context.Context), substitution.SubstInContext(s0.(typing.TypeSubst), cx), e1.(core.Term), "rhs")).(func(any) any)(func (rhsResult typing.InferenceResult) any {
                      return func () any {
                        var fcx3 any = func (v any) any {
                          return v.(typing.InferenceResult).Context
                        }(rhsResult)
                        return func () any {
                          var b any = func (v any) any {
                            return v.(typing.InferenceResult).Term
                          }(rhsResult)
                          return func () any {
                            var t1 any = func (v any) any {
                              return v.(typing.InferenceResult).Type_
                            }(rhsResult)
                            return func () any {
                              var s1 any = func (v any) any {
                                return v.(typing.InferenceResult).Subst
                              }(rhsResult)
                              return func () any {
                                var c1 any = func (v any) any {
                                  return v.(typing.InferenceResult).ClassConstraints
                                }(rhsResult)
                                return func () any {
                                  var vResult any = schemas.FreshName(fcx3.(context.Context))
                                  return func () any {
                                    var v any = libpairs.First(vResult)
                                    return func () any {
                                      var fcx4 any = libpairs.Second(vResult)
                                      return libeithers.Bind(libeithers.Bimap(func (_ic context.InContext[error.UnificationError]) any {
                                        return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(func (v any) any {
                                          return v.(context.InContext[error.Error]).Object
                                        }(_ic).(error.UnificationError).Message)}, Context: func (v any) any {
                                          return v.(context.InContext[error.Error]).Context
                                        }(_ic).(context.Context)}
                                      }).(func(any) any)(func (_a typing.TypeSubst) any {
                                        return _a
                                      }).(func(any) any)(unification.UnifyTypes(fcx4.(context.Context), func (v any) any {
                                        return v.(graph.Graph).SchemaTypes
                                      }(cx).([]any), substitution.SubstInType(s1.(typing.TypeSubst), t0.(core.Type)), core.TypeFunction{Value: core.FunctionType{Domain: t1.(core.Type), Codomain: core.TypeVariable{Value: v.(core.Name)}}}, "application lhs"))).(func(any) any)(func (s2 typing.TypeSubst) any {
                                        return libeithers.Bind(checking.CheckTypeSubst(fcx4.(context.Context), cx, s2)).(func(any) any)(func (_ typing.TypeSubst) any {
                                          return func () any {
                                            var rExpr any = core.TermApplication{Value: core.Application{Function: substitution.SubstTypesInTerm(substitution.ComposeTypeSubst(s1.(typing.TypeSubst), s2), a.(core.Term)), Argument: substitution.SubstTypesInTerm(s2, b.(core.Term))}}
                                            return func () any {
                                              var rType any = substitution.SubstInType(s2, core.TypeVariable{Value: v.(core.Name)})
                                              return func () any {
                                                var rSubst any = substitution.ComposeTypeSubstList([]any{s0, s1, s2})
                                                return func () any {
                                                  var c0Subst any = substitution.SubstInClassConstraints(s2, substitution.SubstInClassConstraints(s1.(typing.TypeSubst), c0.([]any)))
                                                  return func () any {
                                                    var c1Subst any = substitution.SubstInClassConstraints(s2, c1.([]any))
                                                    return func () any {
                                                      var rConstraints any = MergeClassConstraints(c0Subst.([]any), c1Subst.([]any))
                                                      return [2]any{"right", typing.InferenceResult{Term: rExpr.(core.Term), Type_: rType.(core.Type), Subst: rSubst.(typing.TypeSubst), ClassConstraints: rConstraints.([]any), Context: fcx4.(context.Context)}}
                                                    }()
                                                  }()
                                                }()
                                              }()
                                            }()
                                          }()
                                        })
                                      })
                                    }()
                                  }()
                                }()
                              }()
                            }()
                          }()
                        }()
                      }()
                    })
                  }()
                }()
              }()
            }()
          }()
        })
      }()
    }()
  }()
}

func InferTypeOfCaseStatement (fcx context.Context, cx graph.Graph, caseStmt core.CaseStatement) any {
  return func () any {
    var tname any = func (v any) any {
      return v.(core.CaseStatement).TypeName
    }(caseStmt)
    return func () any {
      var dflt any = func (v any) any {
        return v.(core.CaseStatement).Default_
      }(caseStmt)
      return func () any {
        var cases any = func (v any) any {
          return v.(core.CaseStatement).Cases
        }(caseStmt)
        return func () any {
          var fnames any = liblists.Map(func (v any) any {
            return v.(core.Field).Name
          }).(func(any) any)(cases)
          return libeithers.Bind(schemas.RequireSchemaType(fcx, func (v any) any {
            return v.(graph.Graph).SchemaTypes
          }(cx).([]any), tname.(core.Name))).(func(any) any)(func (stRp any) any {
            return func () any {
              var schemaType any = libpairs.First(stRp)
              return func () any {
                var fcx2 any = libpairs.Second(stRp)
                return func () any {
                  var svars any = schemaType.(core.TypeScheme).Variables
                  return func () any {
                    var stype any = schemaType.(core.TypeScheme).Type_
                    return libeithers.Bind(extractcore.UnionType[core.Name](fcx2.(context.Context), tname, stype.(core.Type))).(func(any) any)(func (sfields []any) any {
                      return libeithers.Bind(libeithers.MapMaybe(func (t core.Term) any {
                        return InferTypeOfTerm(fcx2.(context.Context), cx, t, libstrings.Cat([]any{"case ", tname, ".<default>"}).(string))
                      }).(func(any) any)(dflt)).(func(any) any)(func (dfltRp any) any {
                        return func () any {
                          var dfltResult any = dfltRp
                          return func () any {
                            var fcx3 any = libmaybes.FromMaybe(fcx2).(func(any) any)(libmaybes.Map(func (v any) any {
                              return v.(typing.InferenceResult).Context
                            }).(func(any) any)(dfltRp))
                            return libeithers.Bind(InferMany(fcx3.(context.Context), cx, liblists.Map(func (f core.Field) any {
                              return [2]any{func (v any) any {
                                return v.(core.Field).Term
                              }(f), libstrings.Cat([]any{"case ", tname, ".", func (v any) any {
                                return v.(core.Field).Name
                              }(f)})}
                            }).(func(any) any)(cases).([]any))).(func(any) any)(func (caseRp any) any {
                              return func () any {
                                var caseResults any = libpairs.First(caseRp)
                                return func () any {
                                  var fcx4 any = libpairs.Second(caseRp)
                                  return func () any {
                                    var iterms any = libpairs.First(caseResults)
                                    return func () any {
                                      var itypes any = libpairs.First(libpairs.Second(caseResults))
                                      return func () any {
                                        var isubst any = libpairs.First(libpairs.Second(libpairs.Second(caseResults)))
                                        return func () any {
                                          var caseElemConstraints any = libpairs.Second(libpairs.Second(libpairs.Second(caseResults)))
                                          return func () any {
                                            var codvResult any = schemas.FreshName(fcx4.(context.Context))
                                            return func () any {
                                              var codv any = libpairs.First(codvResult)
                                              return func () any {
                                                var fcx5 any = libpairs.Second(codvResult)
                                                return func () any {
                                                  var cod any = core.TypeVariable{Value: codv.(core.Name)}
                                                  return func () any {
                                                    var caseMap any = libmaps.FromList(liblists.Map(func (ft core.FieldType) any {
                                                      return [2]any{func (v any) any {
                                                        return v.(core.FieldType).Name
                                                      }(ft), func (v any) any {
                                                        return v.(core.FieldType).Type_
                                                      }(ft)}
                                                    }).(func(any) any)(sfields))
                                                    return func () any {
                                                      var dfltConstraints any = libmaybes.ToList(libmaybes.Map(func (r typing.InferenceResult) any {
                                                        return typing.TypeConstraint{Left: cod.(core.Type), Right: substitution.SubstInType(isubst.(typing.TypeSubst), func (v any) any {
                                                          return v.(typing.InferenceResult).Type_
                                                        }(r).(core.Type)), Comment: "match default"}
                                                      }).(func(any) any)(dfltResult))
                                                      return func () any {
                                                        var caseConstraints any = libmaybes.Cat(liblists.ZipWith(func (fname core.Name) any {
                                                          return func (itype core.Type) any {
                                                            return libmaybes.Map(func (ftype core.Type) any {
                                                              return typing.TypeConstraint{Left: itype, Right: core.TypeFunction{Value: core.FunctionType{Domain: ftype, Codomain: cod.(core.Type)}}, Comment: "case type"}
                                                            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(caseMap))
                                                          }
                                                        }).(func(any) any)(fnames).(func(any) any)(itypes))
                                                        return func () any {
                                                          var dfltClassConstraints any = libmaybes.FromMaybe(libmaps.Empty).(func(any) any)(libmaybes.Map(func (v any) any {
                                                            return v.(typing.InferenceResult).ClassConstraints
                                                          }).(func(any) any)(dfltResult))
                                                          return func () any {
                                                            var allElemConstraints any = MergeClassConstraints(caseElemConstraints.([]any), dfltClassConstraints.([]any))
                                                            return libeithers.Bind(MapConstraints[typing.InferenceResult](fcx5.(context.Context), cx, func (_p typing.TypeSubst) any {
                                                              return func (subst typing.TypeSubst) any {
                                                                return YieldWithConstraints(fcx5.(context.Context), BuildTypeApplicationTerm(svars.([]any), core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: tname.(core.Name), Default_: libmaybes.Map(func (v any) any {
                                                                  return v.(typing.InferenceResult).Term
                                                                }).(func(any) any)(dfltResult), Cases: liblists.ZipWith(func (n core.Name) any {
                                                                  return func (t core.Term) any {
                                                                    return core.Field{Name: n, Term: t}
                                                                  }
                                                                }).(func(any) any)(fnames).(func(any) any)(iterms).([]any)}}}}), core.TypeFunction{Value: core.FunctionType{Domain: schemas.NominalApplication(tname.(core.Name), liblists.Map(func (x core.Name) any {
                                                                  return core.TypeVariable{Value: x}
                                                                }).(func(any) any)(svars).([]any)), Codomain: cod.(core.Type)}}, substitution.ComposeTypeSubstList(liblists.Concat([]any{libmaybes.ToList(libmaybes.Map(func (v any) any {
                                                                  return v.(typing.InferenceResult).Subst
                                                                }).(func(any) any)(dfltResult)), []any{isubst, subst}}).([]any)), substitution.SubstInClassConstraints(subst, allElemConstraints.([]any)))
                                                              }(_p)
                                                            }, liblists.Concat([]any{dfltConstraints, caseConstraints}).([]any))).(func(any) any)(func (mcResult typing.InferenceResult) any {
                                                              return [2]any{"right", mcResult}
                                                            })
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
                                  }()
                                }()
                              }()
                            })
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
    }()
  }()
}

func InferTypeOfCollection (fcx context.Context, cx graph.Graph, typCons func(core.Type) core.Type, trmCons func([]any) core.Term, desc string, classNames []any, els []any) any {
  return func () any {
    var varResult any = schemas.FreshName(fcx)
    return func () any {
      var var_ any = libpairs.First(varResult)
      return func () any {
        var fcx2 any = libpairs.Second(varResult)
        return func () any {
          var classConstraints any = liblogic.IfElse(libsets.Null(classNames)).(func(any) any)(libmaps.Empty).(func(any) any)(libmaps.Singleton(var_).(func(any) any)(core.TypeVariableMetadata{Classes: classNames}))
          return liblogic.IfElse(liblists.Null(els)).(func(any) any)([2]any{"right", YieldWithConstraints(fcx2.(context.Context), BuildTypeApplicationTerm([]any{var_}, trmCons([]any{})), typCons(core.TypeVariable{Value: var_.(core.Name)}), substitution.IdTypeSubst, classConstraints.([]any))}).(func(any) any)(libeithers.Bind(InferMany(fcx2.(context.Context), cx, liblists.Zip(els).(func(any) any)(liblists.Map(func (i int32) any {
            return libstrings.Cat([]any{"#", libliterals.ShowInt32(i)})
          }).(func(any) any)(libmath.Range(1).(func(any) any)(libmath.Add(liblists.Length(els)).(func(any) any)(1)))).([]any))).(func(any) any)(func (resultsRp any) any {
            return func () any {
              var results any = libpairs.First(resultsRp)
              return func () any {
                var fcx3 any = libpairs.Second(resultsRp)
                return func () any {
                  var terms any = libpairs.First(results)
                  return func () any {
                    var types any = libpairs.First(libpairs.Second(results))
                    return func () any {
                      var subst1 any = libpairs.First(libpairs.Second(libpairs.Second(results)))
                      return func () any {
                        var elemConstraints any = libpairs.Second(libpairs.Second(libpairs.Second(results)))
                        return func () any {
                          var constraints any = liblists.Map(func (t core.Type) any {
                            return typing.TypeConstraint{Left: core.TypeVariable{Value: var_.(core.Name)}, Right: t, Comment: desc}
                          }).(func(any) any)(types)
                          return func () any {
                            var allConstraints any = MergeClassConstraints(classConstraints.([]any), elemConstraints.([]any))
                            return libeithers.Bind(MapConstraints[typing.InferenceResult](fcx3.(context.Context), cx, func (_p typing.TypeSubst) any {
                              return func (subst2 typing.TypeSubst) any {
                                return func () any {
                                  var iterm any = trmCons(terms.([]any))
                                  return func () any {
                                    var itype any = typCons(core.TypeVariable{Value: var_.(core.Name)})
                                    return func () any {
                                      var isubst any = substitution.ComposeTypeSubst(subst1.(typing.TypeSubst), subst2)
                                      return YieldWithConstraints(fcx3.(context.Context), iterm.(core.Term), itype.(core.Type), isubst.(typing.TypeSubst), substitution.SubstInClassConstraints(subst2, allConstraints.([]any)))
                                    }()
                                  }()
                                }()
                              }(_p)
                            }, constraints.([]any))).(func(any) any)(func (mcResult typing.InferenceResult) any {
                              return [2]any{"right", mcResult}
                            })
                          }()
                        }()
                      }()
                    }()
                  }()
                }()
              }()
            }()
          }))
        }()
      }()
    }()
  }()
}

func InferTypeOfEither (fcx context.Context, cx graph.Graph, e any) any {
  return libeithers.Either(func (l core.Term) any {
    return libeithers.Bind(InferTypeOfTerm(fcx, cx, l, "either left value")).(func(any) any)(func (r1 typing.InferenceResult) any {
      return func () any {
        var fcx2 any = func (v any) any {
          return v.(typing.InferenceResult).Context
        }(r1)
        return func () any {
          var iterm any = func (v any) any {
            return v.(typing.InferenceResult).Term
          }(r1)
          return func () any {
            var leftType any = func (v any) any {
              return v.(typing.InferenceResult).Type_
            }(r1)
            return func () any {
              var subst any = func (v any) any {
                return v.(typing.InferenceResult).Subst
              }(r1)
              return func () any {
                var fvResult any = FreshVariableType(fcx2.(context.Context))
                return func () any {
                  var rightType any = libpairs.First(fvResult)
                  return func () any {
                    var fcx3 any = libpairs.Second(fvResult)
                    return func () any {
                      var eitherTerm any = core.TermEither{Value: [2]any{"left", iterm}}
                      return func () any {
                        var termWithLeftType any = core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: eitherTerm.(core.Term), Type_: leftType.(core.Type)}}
                        return func () any {
                          var termWithBothTypes any = core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: termWithLeftType.(core.Term), Type_: rightType.(core.Type)}}
                          return func () any {
                            var eitherType any = core.TypeEither{Value: core.EitherType{Left: leftType.(core.Type), Right: rightType.(core.Type)}}
                            return [2]any{"right", YieldChecked(fcx3.(context.Context), termWithBothTypes.(core.Term), eitherType.(core.Type), subst.(typing.TypeSubst))}
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
    })
  }).(func(any) any)(func (r core.Term) any {
    return libeithers.Bind(InferTypeOfTerm(fcx, cx, r, "either right value")).(func(any) any)(func (r1 typing.InferenceResult) any {
      return func () any {
        var fcx2 any = func (v any) any {
          return v.(typing.InferenceResult).Context
        }(r1)
        return func () any {
          var iterm any = func (v any) any {
            return v.(typing.InferenceResult).Term
          }(r1)
          return func () any {
            var rightType any = func (v any) any {
              return v.(typing.InferenceResult).Type_
            }(r1)
            return func () any {
              var subst any = func (v any) any {
                return v.(typing.InferenceResult).Subst
              }(r1)
              return func () any {
                var fvResult any = FreshVariableType(fcx2.(context.Context))
                return func () any {
                  var leftType any = libpairs.First(fvResult)
                  return func () any {
                    var fcx3 any = libpairs.Second(fvResult)
                    return func () any {
                      var eitherTerm any = core.TermEither{Value: [2]any{"right", iterm}}
                      return func () any {
                        var termWithLeftType any = core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: eitherTerm.(core.Term), Type_: leftType.(core.Type)}}
                        return func () any {
                          var termWithBothTypes any = core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: termWithLeftType.(core.Term), Type_: rightType.(core.Type)}}
                          return func () any {
                            var eitherType any = core.TypeEither{Value: core.EitherType{Left: leftType.(core.Type), Right: rightType.(core.Type)}}
                            return [2]any{"right", YieldChecked(fcx3.(context.Context), termWithBothTypes.(core.Term), eitherType.(core.Type), subst.(typing.TypeSubst))}
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
    })
  }).(func(any) any)(e)
}

func InferTypeOfElimination (fcx context.Context, cx graph.Graph, elm core.Elimination) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.EliminationRecord:
      return func (p core.Projection) any {
        return InferTypeOfProjection(fcx, cx, p)
      }(v.Value)
      case core.EliminationUnion:
      return func (c core.CaseStatement) any {
        return InferTypeOfCaseStatement(fcx, cx, c)
      }(v.Value)
      case core.EliminationWrap:
      return func (tname core.Name) any {
        return InferTypeOfUnwrap(fcx, cx, tname)
      }(v.Value)
    }
    return nil
  }(elm)
}

func InferTypeOfFunction (fcx context.Context, cx graph.Graph, f core.Function) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.FunctionElimination:
      return func (elm core.Elimination) any {
        return InferTypeOfElimination(fcx, cx, elm)
      }(v.Value)
      case core.FunctionLambda:
      return func (l core.Lambda) any {
        return InferTypeOfLambda(fcx, cx, l)
      }(v.Value)
      case core.FunctionPrimitive:
      return func (name core.Name) any {
        return InferTypeOfPrimitive(fcx, cx, name)
      }(v.Value)
    }
    return nil
  }(f)
}

func InferTypeOfInjection (fcx context.Context, cx graph.Graph, injection core.Injection) any {
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
          var term any = field.(core.Field).Term
          return libeithers.Bind(InferTypeOfTerm(fcx, cx, term.(core.Term), "injected term")).(func(any) any)(func (result typing.InferenceResult) any {
            return func () any {
              var fcx2 any = func (v any) any {
                return v.(typing.InferenceResult).Context
              }(result)
              return libeithers.Bind(schemas.RequireSchemaType(fcx2.(context.Context), func (v any) any {
                return v.(graph.Graph).SchemaTypes
              }(cx).([]any), tname.(core.Name))).(func(any) any)(func (stRp any) any {
                return func () any {
                  var schemaType any = libpairs.First(stRp)
                  return func () any {
                    var fcx3 any = libpairs.Second(stRp)
                    return func () any {
                      var svars any = schemaType.(core.TypeScheme).Variables
                      return func () any {
                        var stype any = schemaType.(core.TypeScheme).Type_
                        return func () any {
                          var iterm any = func (v any) any {
                            return v.(typing.InferenceResult).Term
                          }(result)
                          return func () any {
                            var ityp any = func (v any) any {
                              return v.(typing.InferenceResult).Type_
                            }(result)
                            return func () any {
                              var isubst any = func (v any) any {
                                return v.(typing.InferenceResult).Subst
                              }(result)
                              return libeithers.Bind(extractcore.UnionType[core.Name](fcx3.(context.Context), tname, stype.(core.Type))).(func(any) any)(func (sfields []any) any {
                                return libeithers.Bind(schemas.FindFieldType(fcx3.(context.Context), fname.(core.Name), sfields)).(func(any) any)(func (ftyp core.Type) any {
                                  return libeithers.Bind(MapConstraints[typing.InferenceResult](fcx3.(context.Context), cx, func (_p typing.TypeSubst) any {
                                    return func (subst typing.TypeSubst) any {
                                      return Yield(fcx3.(context.Context), BuildTypeApplicationTerm(svars.([]any), core.TermUnion{Value: core.Injection{TypeName: tname.(core.Name), Field: core.Field{Name: fname.(core.Name), Term: iterm.(core.Term)}}}), schemas.NominalApplication(tname.(core.Name), liblists.Map(func (x core.Name) any {
                                        return core.TypeVariable{Value: x}
                                      }).(func(any) any)(svars).([]any)), substitution.ComposeTypeSubst(isubst.(typing.TypeSubst), subst))
                                    }(_p)
                                  }, []any{typing.TypeConstraint{Left: ftyp, Right: ityp.(core.Type), Comment: "schema type of injected field"}})).(func(any) any)(func (mcResult typing.InferenceResult) any {
                                    return [2]any{"right", mcResult}
                                  })
                                })
                              })
                            }()
                          }()
                        }()
                      }()
                    }()
                  }()
                }()
              })
            }()
          })
        }()
      }()
    }()
  }()
}

func InferTypeOfLambda (fcx context.Context, cx graph.Graph, lambda core.Lambda) any {
  return func () any {
    var var_ any = func (v any) any {
      return v.(core.Lambda).Parameter
    }(lambda)
    return func () any {
      var body any = func (v any) any {
        return v.(core.Lambda).Body
      }(lambda)
      return func () any {
        var vdomResult any = schemas.FreshName(fcx)
        return func () any {
          var vdom any = libpairs.First(vdomResult)
          return func () any {
            var fcx2 any = libpairs.Second(vdomResult)
            return func () any {
              var dom any = core.TypeVariable{Value: vdom.(core.Name)}
              return func () any {
                var cx2 any = ExtendContext([]any{[2]any{var_, core.TypeScheme{Variables: []any{}, Type_: dom.(core.Type), Constraints: nil}}}, cx)
                return libeithers.Bind(InferTypeOfTerm(fcx2.(context.Context), cx2.(graph.Graph), body.(core.Term), "lambda body")).(func(any) any)(func (result typing.InferenceResult) any {
                  return func () any {
                    var fcx3 any = func (v any) any {
                      return v.(typing.InferenceResult).Context
                    }(result)
                    return func () any {
                      var iterm any = func (v any) any {
                        return v.(typing.InferenceResult).Term
                      }(result)
                      return func () any {
                        var icod any = func (v any) any {
                          return v.(typing.InferenceResult).Type_
                        }(result)
                        return func () any {
                          var isubst any = func (v any) any {
                            return v.(typing.InferenceResult).Subst
                          }(result)
                          return func () any {
                            var rdom any = substitution.SubstInType(isubst.(typing.TypeSubst), dom.(core.Type))
                            return func () any {
                              var rterm any = core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: var_.(core.Name), Domain: func () any {
                                _v := rdom
                                return &_v
                              }(), Body: iterm.(core.Term)}}}
                              return func () any {
                                var rtype any = core.TypeFunction{Value: core.FunctionType{Domain: rdom.(core.Type), Codomain: icod.(core.Type)}}
                                return func () any {
                                  var vars any = libsets.Unions([]any{rewriting.FreeVariablesInType(rdom.(core.Type)), rewriting.FreeVariablesInType(icod.(core.Type)), FreeVariablesInContext(substitution.SubstInContext(isubst.(typing.TypeSubst), cx2.(graph.Graph)))})
                                  return func () any {
                                    var cx3 any = substitution.SubstInContext(isubst.(typing.TypeSubst), cx)
                                    return func () any {
                                      var iconstraints any = substitution.SubstInClassConstraints(isubst.(typing.TypeSubst), func (v any) any {
                                        return v.(typing.InferenceResult).ClassConstraints
                                      }(result).([]any))
                                      return [2]any{"right", typing.InferenceResult{Term: rterm.(core.Term), Type_: rtype.(core.Type), Subst: isubst.(typing.TypeSubst), ClassConstraints: iconstraints.([]any), Context: fcx3.(context.Context)}}
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
                })
              }()
            }()
          }()
        }()
      }()
    }()
  }()
}

func InferTypeOfLet (fcx0 context.Context, cx graph.Graph, let0 core.Let) any {
  return func () any {
    var fcx any = context.Context{Trace: liblists.Cons("let").(func(any) any)(func (v any) any {
      return v.(context.Context).Trace
    }(fcx0)).([]any), Messages: func (v any) any {
      return v.(context.Context).Messages
    }(fcx0).([]any), Other: func (v any) any {
      return v.(context.Context).Other
    }(fcx0).([]any)}
    return func () any {
      var bindings0 any = func (v any) any {
        return v.(core.Let).Bindings
      }(let0)
      return func () any {
        var body0 any = func (v any) any {
          return v.(core.Let).Body
        }(let0)
        return func () any {
          var names any = liblists.Map(func (v any) any {
            return v.(core.Binding).Name
          }).(func(any) any)(bindings0)
          return func () any {
            var nameSet any = libsets.FromList(names)
            return func () any {
              toPair := func (binding core.Binding) any {
                return func () any {
                  var name any = func (v any) any {
                    return v.(core.Binding).Name
                  }(binding)
                  return func () any {
                    var term any = func (v any) any {
                      return v.(core.Binding).Term
                    }(binding)
                    return [2]any{name, liblists.Filter(func (n core.Name) any {
                      return libsets.Member(n).(func(any) any)(nameSet)
                    }).(func(any) any)(libsets.ToList(rewriting.FreeVariablesInTerm(term.(core.Term))))}
                  }()
                }()
              }
              return func () any {
                var adjList any = liblists.Map(toPair).(func(any) any)(bindings0)
                return func () any {
                  var groups any = sorting.TopologicalSortComponents(adjList.([]any))
                  return func () any {
                    var bindingMap any = libmaps.FromList(liblists.Zip(names).(func(any) any)(bindings0))
                    return func () any {
                      createLet := func (e core.Term) any {
                        return func (group []any) any {
                          return core.TermLet{Value: core.Let{Bindings: libmaybes.Cat(liblists.Map(func (n core.Name) any {
                            return libmaps.Lookup(n).(func(any) any)(bindingMap)
                          }).(func(any) any)(group)).([]any), Body: e}}
                        }
                      }
                      return func () any {
                        var rewrittenLet any = liblists.Foldl(createLet).(func(any) any)(body0).(func(any) any)(liblists.Reverse(groups))
                        return func () any {
                          restoreLet := func (iterm core.Term) any {
                            return func () any {
                              var helper func(int32) any
                              helper = func (level int32) any {
                                return func (bins []any) any {
                                  return func (term core.Term) any {
                                    return func () any {
                                      nonzero := func (term2 core.Term) any {
                                        return func (x any) any {
                                          switch v := x.(type) {
                                            case core.TermLet:
                                            return func (l core.Let) any {
                                              return func () any {
                                                var bs any = func (v any) any {
                                                  return v.(core.Let).Bindings
                                                }(l)
                                                return func () any {
                                                  var letBody any = func (v any) any {
                                                    return v.(core.Let).Body
                                                  }(l)
                                                  return helper(libmath.Sub(level).(func(any) any)(1).(int32)).(func(any) any)(liblists.Concat([]any{bs, bins})).(func(any) any)(letBody)
                                                }()
                                              }()
                                            }(v.Value)
                                          }
                                          return nil
                                        }(term2)
                                      }
                                      return liblogic.IfElse(libequality.Equal(level).(func(any) any)(0)).(func(any) any)([2]any{bins, term}).(func(any) any)(nonzero(term))
                                    }()
                                  }
                                }
                              }
                              return func () any {
                                var result any = helper(liblists.Length(groups).(int32)).(func(any) any)([]any{}).(func(any) any)(iterm)
                                return func () any {
                                  var bindingList any = libpairs.First(result)
                                  return func () any {
                                    var e any = libpairs.Second(result)
                                    return func () any {
                                      var bindingMap2 any = libmaps.FromList(liblists.Map(func (b core.Binding) any {
                                        return [2]any{func (v any) any {
                                          return v.(core.Binding).Name
                                        }(b), b}
                                      }).(func(any) any)(bindingList))
                                      return core.TermLet{Value: core.Let{Bindings: libmaybes.Cat(liblists.Map(func (n core.Name) any {
                                        return libmaps.Lookup(n).(func(any) any)(bindingMap2)
                                      }).(func(any) any)(names)).([]any), Body: e.(core.Term)}}
                                    }()
                                  }()
                                }()
                              }()
                            }()
                          }
                          return func () any {
                            rewriteResult := func (iresult typing.InferenceResult) any {
                              return func () any {
                                var fcxR any = func (v any) any {
                                  return v.(typing.InferenceResult).Context
                                }(iresult)
                                return func () any {
                                  var iterm any = func (v any) any {
                                    return v.(typing.InferenceResult).Term
                                  }(iresult)
                                  return func () any {
                                    var itype any = func (v any) any {
                                      return v.(typing.InferenceResult).Type_
                                    }(iresult)
                                    return func () any {
                                      var isubst any = func (v any) any {
                                        return v.(typing.InferenceResult).Subst
                                      }(iresult)
                                      return func () any {
                                        var iconstraints any = func (v any) any {
                                          return v.(typing.InferenceResult).ClassConstraints
                                        }(iresult)
                                        return typing.InferenceResult{Term: restoreLet(iterm.(core.Term)).(core.Term), Type_: itype.(core.Type), Subst: isubst.(typing.TypeSubst), ClassConstraints: iconstraints.([]any), Context: fcxR.(context.Context)}
                                      }()
                                    }()
                                  }()
                                }()
                              }()
                            }
                            return func () any {
                              var res any = func (x any) any {
                                switch v := x.(type) {
                                  case core.TermLet:
                                  return func (l core.Let) any {
                                    return InferTypeOfLetNormalized(fcx.(context.Context), cx, l)
                                  }(v.Value)
                                  default:
                                  return InferTypeOfTerm(fcx.(context.Context), cx, rewrittenLet.(core.Term), "empty let term")
                                }
                                return nil
                              }(rewrittenLet)
                              return libeithers.Map(rewriteResult).(func(any) any)(res)
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
    }()
  }()
}

func InferTypeOfLetNormalized (fcx0 context.Context, cx0 graph.Graph, letTerm core.Let) any {
  return func () any {
    var fcx any = context.Context{Trace: liblists.Cons("let-normalized").(func(any) any)(func (v any) any {
      return v.(context.Context).Trace
    }(fcx0)).([]any), Messages: func (v any) any {
      return v.(context.Context).Messages
    }(fcx0).([]any), Other: func (v any) any {
      return v.(context.Context).Other
    }(fcx0).([]any)}
    return func () any {
      var bins0 any = func (v any) any {
        return v.(core.Let).Bindings
      }(letTerm)
      return func () any {
        var body0 any = func (v any) any {
          return v.(core.Let).Body
        }(letTerm)
        return func () any {
          var bnames any = liblists.Map(func (v any) any {
            return v.(core.Binding).Name
          }).(func(any) any)(bins0)
          return func () any {
            var bvarsResult any = schemas.FreshNames(liblists.Length(bins0).(int32), fcx.(context.Context))
            return func () any {
              var bvars any = libpairs.First(bvarsResult)
              return func () any {
                var fcx2 any = libpairs.Second(bvarsResult)
                return func () any {
                  var tbins0 any = liblists.Map(func (x core.Name) any {
                    return core.TypeVariable{Value: x}
                  }).(func(any) any)(bvars)
                  return func () any {
                    var cx1 any = ExtendContext(liblists.Zip(bnames).(func(any) any)(liblists.Map(func (t core.Type) any {
                      return core.TypeScheme{Variables: []any{}, Type_: t, Constraints: nil}
                    }).(func(any) any)(tbins0)).([]any), cx0)
                    return libeithers.Bind(InferTypesOfTemporaryBindings(fcx2.(context.Context), cx1.(graph.Graph), bins0.([]any))).(func(any) any)(func (irRp any) any {
                      return func () any {
                        var inferredResult any = libpairs.First(irRp)
                        return func () any {
                          var fcx3 any = libpairs.Second(irRp)
                          return func () any {
                            var bterms1 any = libpairs.First(inferredResult)
                            return func () any {
                              var tbins1 any = libpairs.First(libpairs.Second(inferredResult))
                              return func () any {
                                var substAndConstraints any = libpairs.Second(libpairs.Second(inferredResult))
                                return func () any {
                                  var s1 any = libpairs.First(substAndConstraints)
                                  return func () any {
                                    var inferredConstraints any = libpairs.Second(substAndConstraints)
                                    return libeithers.Bind(libeithers.Bimap(func (_ic context.InContext[error.UnificationError]) any {
                                      return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(func (v any) any {
                                        return v.(context.InContext[error.Error]).Object
                                      }(_ic).(error.UnificationError).Message)}, Context: func (v any) any {
                                        return v.(context.InContext[error.Error]).Context
                                      }(_ic).(context.Context)}
                                    }).(func(any) any)(func (_a typing.TypeSubst) any {
                                      return _a
                                    }).(func(any) any)(unification.UnifyTypeLists(fcx3.(context.Context), func (v any) any {
                                      return v.(graph.Graph).SchemaTypes
                                    }(cx0).([]any), liblists.Map(func (v1 core.Type) any {
                                      return substitution.SubstInType(s1.(typing.TypeSubst), v1)
                                    }).(func(any) any)(tbins0).([]any), tbins1.([]any), "temporary type bindings"))).(func(any) any)(func (s2 typing.TypeSubst) any {
                                      return libeithers.Bind(checking.CheckTypeSubst(fcx3.(context.Context), cx0, s2)).(func(any) any)(func (_ typing.TypeSubst) any {
                                        return func () any {
                                          var g2base any = substitution.SubstInContext(substitution.ComposeTypeSubst(s1.(typing.TypeSubst), s2), cx0)
                                          return func () any {
                                            var constraintsWithS2 any = substitution.SubstInClassConstraints(s2, inferredConstraints.([]any))
                                            return func () any {
                                              var composedSubst any = substitution.ComposeTypeSubst(s1.(typing.TypeSubst), s2)
                                              return func () any {
                                                var originalBindingConstraints any = liblists.Foldl(func (acc []any) any {
                                                  return func (b core.Binding) any {
                                                    return libmaybes.Maybe(acc).(func(any) any)(func (ts core.TypeScheme) any {
                                                      return libmaybes.Maybe(acc).(func(any) any)(func (c []any) any {
                                                        return MergeClassConstraints(acc, c)
                                                      }).(func(any) any)(func (v any) any {
                                                        return v.(core.TypeScheme).Constraints
                                                      }(ts))
                                                    }).(func(any) any)(func (v any) any {
                                                      return v.(core.Binding).Type_
                                                    }(b))
                                                  }
                                                }).(func(any) any)(libmaps.Empty).(func(any) any)(bins0)
                                                return func () any {
                                                  var originalConstraintsSubst any = substitution.SubstInClassConstraints(composedSubst.(typing.TypeSubst), originalBindingConstraints.([]any))
                                                  return func () any {
                                                    var allInferredConstraints any = MergeClassConstraints(constraintsWithS2.([]any), originalConstraintsSubst.([]any))
                                                    return func () any {
                                                      var mergedConstraints any = MergeClassConstraints(g2base.(graph.Graph).ClassConstraints, allInferredConstraints.([]any))
                                                      return func () any {
                                                        var g2 any = graph.Graph{BoundTerms: g2base.(graph.Graph).BoundTerms, BoundTypes: g2base.(graph.Graph).BoundTypes, ClassConstraints: mergedConstraints.([]any), LambdaVariables: g2base.(graph.Graph).LambdaVariables, Metadata: g2base.(graph.Graph).Metadata, Primitives: g2base.(graph.Graph).Primitives, SchemaTypes: g2base.(graph.Graph).SchemaTypes, TypeVariables: g2base.(graph.Graph).TypeVariables}
                                                        return func () any {
                                                          var bterms1Subst any = liblists.Map(func (v1 core.Term) any {
                                                            return substitution.SubstTypesInTerm(s2, v1)
                                                          }).(func(any) any)(bterms1)
                                                          return func () any {
                                                            var tsbins1 any = liblists.Zip(bnames).(func(any) any)(liblists.Map(func (t core.Type) any {
                                                              return Generalize(g2.(graph.Graph), substitution.SubstInType(s2, t))
                                                            }).(func(any) any)(tbins1))
                                                            return libeithers.Bind(InferTypeOfTerm(fcx3.(context.Context), ExtendContext(tsbins1.([]any), g2.(graph.Graph)), body0.(core.Term), "let body")).(func(any) any)(func (bodyResult typing.InferenceResult) any {
                                                              return func () any {
                                                                var fcx4 any = func (v any) any {
                                                                  return v.(typing.InferenceResult).Context
                                                                }(bodyResult)
                                                                return func () any {
                                                                  var body1 any = func (v any) any {
                                                                    return v.(typing.InferenceResult).Term
                                                                  }(bodyResult)
                                                                  return func () any {
                                                                    var tbody any = func (v any) any {
                                                                      return v.(typing.InferenceResult).Type_
                                                                    }(bodyResult)
                                                                    return func () any {
                                                                      var sbody any = func (v any) any {
                                                                        return v.(typing.InferenceResult).Subst
                                                                      }(bodyResult)
                                                                      return func () any {
                                                                        var st1 any = typing.TermSubst(libmaps.FromList(liblists.Map(func (pair any) any {
                                                                          return func () any {
                                                                            var name any = libpairs.First(pair)
                                                                            return func () any {
                                                                              var ts any = libpairs.Second(pair)
                                                                              return [2]any{name, BuildTypeApplicationTerm(ts.(core.TypeScheme).Variables, core.TermVariable{Value: name.(core.Name)})}
                                                                            }()
                                                                          }()
                                                                        }).(func(any) any)(tsbins1)).([]any))
                                                                        return func () any {
                                                                          createBinding := func (bindingPair any) any {
                                                                            return func () any {
                                                                              var nameTsPair any = libpairs.First(bindingPair)
                                                                              return func () any {
                                                                                var term any = libpairs.Second(bindingPair)
                                                                                return func () any {
                                                                                  var name any = libpairs.First(nameTsPair)
                                                                                  return func () any {
                                                                                    var ts any = libpairs.Second(nameTsPair)
                                                                                    return func () any {
                                                                                      var finalTs any = substitution.SubstInTypeScheme(sbody.(typing.TypeSubst), ts.(core.TypeScheme))
                                                                                      return func () any {
                                                                                        var typeLambdaTerm any = liblists.Foldl(func (b core.Term) any {
                                                                                          return func (v core.Name) any {
                                                                                            return core.TermTypeLambda{Value: core.TypeLambda{Parameter: v, Body: b}}
                                                                                          }
                                                                                        }).(func(any) any)(substitution.SubstituteInTerm(st1.(typing.TermSubst), term.(core.Term))).(func(any) any)(liblists.Reverse(finalTs.(core.TypeScheme).Variables))
                                                                                        return core.Binding{Name: name.(core.Name), Term: substitution.SubstTypesInTerm(substitution.ComposeTypeSubst(sbody.(typing.TypeSubst), s2), typeLambdaTerm.(core.Term)), Type_: func () any {
                                                                                          _v := finalTs
                                                                                          return &_v
                                                                                        }()}
                                                                                      }()
                                                                                    }()
                                                                                  }()
                                                                                }()
                                                                              }()
                                                                            }()
                                                                          }
                                                                          return func () any {
                                                                            var bins1 any = liblists.Map(createBinding).(func(any) any)(liblists.Zip(tsbins1).(func(any) any)(bterms1Subst))
                                                                            return func () any {
                                                                              var bodyConstraints any = substitution.SubstInClassConstraints(sbody.(typing.TypeSubst), func (v any) any {
                                                                                return v.(typing.InferenceResult).ClassConstraints
                                                                              }(bodyResult).([]any))
                                                                              return func () any {
                                                                                var bindingConstraintsSubst any = substitution.SubstInClassConstraints(sbody.(typing.TypeSubst), constraintsWithS2.([]any))
                                                                                return func () any {
                                                                                  var allConstraints any = MergeClassConstraints(bindingConstraintsSubst.([]any), bodyConstraints.([]any))
                                                                                  return [2]any{"right", typing.InferenceResult{Term: core.TermLet{Value: core.Let{Bindings: bins1.([]any), Body: body1.(core.Term)}}, Type_: tbody.(core.Type), Subst: substitution.ComposeTypeSubstList([]any{s1, s2, sbody}), ClassConstraints: allConstraints.([]any), Context: fcx4.(context.Context)}}
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
                                                            })
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
                                      })
                                    })
                                  }()
                                }()
                              }()
                            }()
                          }()
                        }()
                      }()
                    })
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

func InferTypeOfList (fcx context.Context, cx graph.Graph, v1 []any) any {
  return InferTypeOfCollection(fcx, cx, func (_p core.Type) core.Type {
    return func (x core.Type) core.Type {
      return core.TypeList{Value: x}
    }(_p).(core.Type)
  }, func (_p []any) core.Term {
    return func (x []any) core.Term {
      return core.TermList{Value: x}
    }(_p).(core.Term)
  }, "list element", libsets.Empty, v1)
}

func InferTypeOfLiteral (fcx context.Context, lit core.Literal) typing.InferenceResult {
  return typing.InferenceResult{Term: core.TermLiteral{Value: lit}, Type_: core.TypeLiteral{Value: reflect.LiteralType(lit)}, Subst: substitution.IdTypeSubst, ClassConstraints: libmaps.Empty, Context: fcx}
}

func InferTypeOfMap (fcx context.Context, cx graph.Graph, m []any) any {
  return func () any {
    var kvarResult any = schemas.FreshName(fcx)
    return func () any {
      var kvar any = libpairs.First(kvarResult)
      return func () any {
        var fcx2 any = libpairs.Second(kvarResult)
        return func () any {
          var vvarResult any = schemas.FreshName(fcx2.(context.Context))
          return func () any {
            var vvar any = libpairs.First(vvarResult)
            return func () any {
              var fcx3 any = libpairs.Second(vvarResult)
              return func () any {
                var keyConstraints any = libmaps.Singleton(kvar).(func(any) any)(core.TypeVariableMetadata{Classes: libsets.Singleton(core.Name("ordering")).([]any)})
                return liblogic.IfElse(libmaps.Null(m)).(func(any) any)([2]any{"right", YieldWithConstraints(fcx3.(context.Context), BuildTypeApplicationTerm([]any{kvar, vvar}, core.TermMap_{Value: libmaps.Empty}), core.TypeMap_{Value: core.MapType{Keys: core.TypeVariable{Value: kvar.(core.Name)}, Values: core.TypeVariable{Value: vvar.(core.Name)}}}, substitution.IdTypeSubst, keyConstraints.([]any))}).(func(any) any)(libeithers.Bind(InferMany(fcx3.(context.Context), cx, liblists.Map(func (k core.Term) any {
                  return [2]any{k, "map key"}
                }).(func(any) any)(libmaps.Keys(m)).([]any))).(func(any) any)(func (kRp any) any {
                  return func () any {
                    var kResults any = libpairs.First(kRp)
                    return func () any {
                      var fcx4 any = libpairs.Second(kRp)
                      return func () any {
                        var kterms any = libpairs.First(kResults)
                        return func () any {
                          var ktypes any = libpairs.First(libpairs.Second(kResults))
                          return func () any {
                            var ksubst any = libpairs.First(libpairs.Second(libpairs.Second(kResults)))
                            return func () any {
                              var kElemConstraints any = libpairs.Second(libpairs.Second(libpairs.Second(kResults)))
                              return libeithers.Bind(InferMany(fcx4.(context.Context), substitution.SubstInContext(ksubst.(typing.TypeSubst), cx), liblists.Map(func (v core.Term) any {
                                return [2]any{v, "map value"}
                              }).(func(any) any)(libmaps.Elems(m)).([]any))).(func(any) any)(func (vRp any) any {
                                return func () any {
                                  var vResults any = libpairs.First(vRp)
                                  return func () any {
                                    var fcx5 any = libpairs.Second(vRp)
                                    return func () any {
                                      var vterms any = libpairs.First(vResults)
                                      return func () any {
                                        var vtypes any = libpairs.First(libpairs.Second(vResults))
                                        return func () any {
                                          var vsubst any = libpairs.First(libpairs.Second(libpairs.Second(vResults)))
                                          return func () any {
                                            var vElemConstraints any = libpairs.Second(libpairs.Second(libpairs.Second(vResults)))
                                            return func () any {
                                              var kcons any = liblists.Map(func (t core.Type) any {
                                                return typing.TypeConstraint{Left: core.TypeVariable{Value: kvar.(core.Name)}, Right: t, Comment: "map key"}
                                              }).(func(any) any)(ktypes)
                                              return func () any {
                                                var vcons any = liblists.Map(func (t core.Type) any {
                                                  return typing.TypeConstraint{Left: core.TypeVariable{Value: vvar.(core.Name)}, Right: t, Comment: "map value"}
                                                }).(func(any) any)(vtypes)
                                                return func () any {
                                                  var allMapConstraints any = MergeClassConstraints(keyConstraints.([]any), MergeClassConstraints(kElemConstraints.([]any), vElemConstraints.([]any)))
                                                  return libeithers.Bind(MapConstraints[typing.InferenceResult](fcx5.(context.Context), cx, func (_p typing.TypeSubst) any {
                                                    return func (subst typing.TypeSubst) any {
                                                      return YieldWithConstraints(fcx5.(context.Context), core.TermMap_{Value: libmaps.FromList(liblists.Zip(kterms).(func(any) any)(vterms)).([]any)}, core.TypeMap_{Value: core.MapType{Keys: core.TypeVariable{Value: kvar.(core.Name)}, Values: core.TypeVariable{Value: vvar.(core.Name)}}}, substitution.ComposeTypeSubstList([]any{ksubst, vsubst, subst}), substitution.SubstInClassConstraints(subst, allMapConstraints.([]any)))
                                                    }(_p)
                                                  }, liblists.Concat([]any{kcons, vcons}).([]any))).(func(any) any)(func (mcResult typing.InferenceResult) any {
                                                    return [2]any{"right", mcResult}
                                                  })
                                                }()
                                              }()
                                            }()
                                          }()
                                        }()
                                      }()
                                    }()
                                  }()
                                }()
                              })
                            }()
                          }()
                        }()
                      }()
                    }()
                  }()
                }))
              }()
            }()
          }()
        }()
      }()
    }()
  }()
}

func InferTypeOfOptional (fcx context.Context, cx graph.Graph, m any) any {
  return func () any {
    trmCons := func (terms []any) any {
      return liblogic.IfElse(liblists.Null(terms)).(func(any) any)(core.TermMaybe{Value: nil}).(func(any) any)(core.TermMaybe{Value: func () any {
        _v := liblists.Head(terms)
        return &_v
      }()})
    }
    return InferTypeOfCollection(fcx, cx, func (_p core.Type) core.Type {
      return func (x core.Type) core.Type {
        return core.TypeMaybe{Value: x}
      }(_p).(core.Type)
    }, trmCons, "optional element", libsets.Empty, libmaybes.Maybe([]any{}).(func(any) any)(liblists.Singleton).(func(any) any)(m).([]any))
  }()
}

func InferTypeOfPair (fcx context.Context, cx graph.Graph, p any) any {
  return libeithers.Bind(InferMany(fcx, cx, []any{[2]any{libpairs.First(p), "pair first element"}, [2]any{libpairs.Second(p), "pair second element"}})).(func(any) any)(func (rp any) any {
    return func () any {
      var results any = libpairs.First(rp)
      return func () any {
        var fcx2 any = libpairs.Second(rp)
        return func () any {
          var iterms any = libpairs.First(results)
          return func () any {
            var itypes any = libpairs.First(libpairs.Second(results))
            return func () any {
              var isubst any = libpairs.First(libpairs.Second(libpairs.Second(results)))
              return func () any {
                var pairElemConstraints any = libpairs.Second(libpairs.Second(libpairs.Second(results)))
                return func () any {
                  var ifst any = liblists.Head(iterms)
                  return func () any {
                    var isnd any = liblists.Head(liblists.Tail(iterms))
                    return func () any {
                      var tyFst any = liblists.Head(itypes)
                      return func () any {
                        var tySnd any = liblists.Head(liblists.Tail(itypes))
                        return func () any {
                          var pairTerm any = core.TermPair{Value: [2]any{ifst, isnd}}
                          return func () any {
                            var termWithTypes any = core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: pairTerm.(core.Term), Type_: tyFst.(core.Type)}}, Type_: tySnd.(core.Type)}}
                            return [2]any{"right", YieldWithConstraints(fcx2.(context.Context), termWithTypes.(core.Term), core.TypePair{Value: core.PairType{First: tyFst.(core.Type), Second: tySnd.(core.Type)}}, isubst.(typing.TypeSubst), pairElemConstraints.([]any))}
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
  })
}

func InferTypeOfPrimitive (fcx context.Context, cx graph.Graph, name core.Name) any {
  return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("No such primitive: ").(func(any) any)(func (v any) any {
    return v
  }(name)).(string))}, Context: fcx}}).(func(any) any)(func (scheme core.TypeScheme) any {
    return func () any {
      var tsResult any = schemas.InstantiateTypeScheme(fcx, scheme)
      return func () any {
        var ts any = libpairs.First(tsResult)
        return func () any {
          var fcx2 any = libpairs.Second(tsResult)
          return func () any {
            var constraints any = libmaybes.FromMaybe(libmaps.Empty).(func(any) any)(ts.(core.TypeScheme).Constraints)
            return [2]any{"right", YieldCheckedWithConstraints(fcx2.(context.Context), BuildTypeApplicationTerm(ts.(core.TypeScheme).Variables, core.TermFunction{Value: core.FunctionPrimitive{Value: name}}), ts.(core.TypeScheme).Type_, substitution.IdTypeSubst, constraints.([]any))}
          }()
        }()
      }()
    }()
  }).(func(any) any)(libmaybes.Map(func (v any) any {
    return v.(graph.Primitive).Type_
  }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(func (v any) any {
    return v.(graph.Graph).Primitives
  }(cx))))
}

func InferTypeOfProjection (fcx context.Context, cx graph.Graph, proj core.Projection) any {
  return func () any {
    var tname any = func (v any) any {
      return v.(core.Projection).TypeName
    }(proj)
    return func () any {
      var fname any = func (v any) any {
        return v.(core.Projection).Field
      }(proj)
      return libeithers.Bind(schemas.RequireSchemaType(fcx, func (v any) any {
        return v.(graph.Graph).SchemaTypes
      }(cx).([]any), tname.(core.Name))).(func(any) any)(func (stRp any) any {
        return func () any {
          var schemaType any = libpairs.First(stRp)
          return func () any {
            var fcx2 any = libpairs.Second(stRp)
            return func () any {
              var svars any = schemaType.(core.TypeScheme).Variables
              return func () any {
                var stype any = schemaType.(core.TypeScheme).Type_
                return libeithers.Bind(extractcore.RecordType[core.Name](fcx2.(context.Context), tname, stype.(core.Type))).(func(any) any)(func (sfields []any) any {
                  return libeithers.Bind(schemas.FindFieldType(fcx2.(context.Context), fname.(core.Name), sfields)).(func(any) any)(func (ftyp core.Type) any {
                    return [2]any{"right", Yield(fcx2.(context.Context), BuildTypeApplicationTerm(svars.([]any), core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationRecord{Value: core.Projection{TypeName: tname.(core.Name), Field: fname.(core.Name)}}}}), core.TypeFunction{Value: core.FunctionType{Domain: schemas.NominalApplication(tname.(core.Name), liblists.Map(func (x core.Name) any {
                      return core.TypeVariable{Value: x}
                    }).(func(any) any)(svars).([]any)), Codomain: ftyp}}, substitution.IdTypeSubst)}
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

func InferTypeOfRecord (fcx context.Context, cx graph.Graph, record core.Record) any {
  return func () any {
    var tname any = func (v any) any {
      return v.(core.Record).TypeName
    }(record)
    return func () any {
      var fields any = func (v any) any {
        return v.(core.Record).Fields
      }(record)
      return func () any {
        var fnames any = liblists.Map(func (v any) any {
          return v.(core.Field).Name
        }).(func(any) any)(fields)
        return libeithers.Bind(schemas.RequireSchemaType(fcx, func (v any) any {
          return v.(graph.Graph).SchemaTypes
        }(cx).([]any), tname.(core.Name))).(func(any) any)(func (stRp any) any {
          return func () any {
            var schemaType any = libpairs.First(stRp)
            return func () any {
              var fcx2 any = libpairs.Second(stRp)
              return libeithers.Bind(InferMany(fcx2.(context.Context), cx, liblists.Map(func (f core.Field) any {
                return [2]any{func (v any) any {
                  return v.(core.Field).Term
                }(f), libstrings.Cat2("field ").(func(any) any)(func (v any) any {
                  return v.(core.Field).Name
                }(f))}
              }).(func(any) any)(fields).([]any))).(func(any) any)(func (rp any) any {
                return func () any {
                  var results any = libpairs.First(rp)
                  return func () any {
                    var fcx3 any = libpairs.Second(rp)
                    return func () any {
                      var svars any = schemaType.(core.TypeScheme).Variables
                      return func () any {
                        var stype any = schemaType.(core.TypeScheme).Type_
                        return func () any {
                          var iterms any = libpairs.First(results)
                          return func () any {
                            var itypes any = libpairs.First(libpairs.Second(results))
                            return func () any {
                              var isubst any = libpairs.First(libpairs.Second(libpairs.Second(results)))
                              return func () any {
                                var recElemConstraints any = libpairs.Second(libpairs.Second(libpairs.Second(results)))
                                return func () any {
                                  var ityp any = core.TypeRecord{Value: liblists.ZipWith(func (n core.Name) any {
                                    return func (t core.Type) any {
                                      return core.FieldType{Name: n, Type_: t}
                                    }
                                  }).(func(any) any)(fnames).(func(any) any)(itypes).([]any)}
                                  return libeithers.Bind(MapConstraints[typing.InferenceResult](fcx3.(context.Context), cx, func (_p typing.TypeSubst) any {
                                    return func (subst typing.TypeSubst) any {
                                      return YieldWithConstraints(fcx3.(context.Context), BuildTypeApplicationTerm(svars.([]any), core.TermRecord{Value: core.Record{TypeName: tname.(core.Name), Fields: liblists.ZipWith(func (n core.Name) any {
                                        return func (t core.Term) any {
                                          return core.Field{Name: n, Term: t}
                                        }
                                      }).(func(any) any)(fnames).(func(any) any)(iterms).([]any)}}), schemas.NominalApplication(tname.(core.Name), liblists.Map(func (x core.Name) any {
                                        return core.TypeVariable{Value: x}
                                      }).(func(any) any)(svars).([]any)), substitution.ComposeTypeSubst(isubst.(typing.TypeSubst), subst), substitution.SubstInClassConstraints(subst, recElemConstraints.([]any)))
                                    }(_p)
                                  }, []any{typing.TypeConstraint{Left: stype.(core.Type), Right: ityp.(core.Type), Comment: "schema type of record"}})).(func(any) any)(func (mcResult typing.InferenceResult) any {
                                    return [2]any{"right", mcResult}
                                  })
                                }()
                              }()
                            }()
                          }()
                        }()
                      }()
                    }()
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

func InferTypeOfSet (fcx context.Context, cx graph.Graph, s []any) any {
  return InferTypeOfCollection(fcx, cx, func (_p core.Type) core.Type {
    return func (x core.Type) core.Type {
      return core.TypeSet{Value: x}
    }(_p).(core.Type)
  }, func (_p []any) core.Term {
    return func (terms []any) core.Term {
      return core.TermSet{Value: libsets.FromList(terms).([]any)}
    }(_p).(core.Term)
  }, "set element", libsets.Singleton(core.Name("ordering")).([]any), libsets.ToList(s).([]any))
}

func InferTypeOfTerm (fcx context.Context, cx graph.Graph, term core.Term, desc string) any {
  return func () any {
    var fcx2 any = context.Context{Trace: liblists.Cons(desc).(func(any) any)(func (v any) any {
      return v.(context.Context).Trace
    }(fcx)).([]any), Messages: func (v any) any {
      return v.(context.Context).Messages
    }(fcx).([]any), Other: func (v any) any {
      return v.(context.Context).Other
    }(fcx).([]any)}
    return func (x any) any {
      switch v := x.(type) {
        case core.TermAnnotated:
        return func (a core.AnnotatedTerm) any {
          return InferTypeOfAnnotatedTerm(fcx2.(context.Context), cx, a)
        }(v.Value)
        case core.TermApplication:
        return func (a core.Application) any {
          return InferTypeOfApplication(fcx2.(context.Context), cx, a)
        }(v.Value)
        case core.TermEither:
        return func (e any) any {
          return InferTypeOfEither(fcx2.(context.Context), cx, e)
        }(v.Value)
        case core.TermFunction:
        return func (f core.Function) any {
          return InferTypeOfFunction(fcx2.(context.Context), cx, f)
        }(v.Value)
        case core.TermLet:
        return func (l core.Let) any {
          return InferTypeOfLet(fcx2.(context.Context), cx, l)
        }(v.Value)
        case core.TermList:
        return func (els []any) any {
          return InferTypeOfList(fcx2.(context.Context), cx, els)
        }(v.Value)
        case core.TermLiteral:
        return func (l core.Literal) any {
          return [2]any{"right", InferTypeOfLiteral(fcx2.(context.Context), l)}
        }(v.Value)
        case core.TermMap_:
        return func (m []any) any {
          return InferTypeOfMap(fcx2.(context.Context), cx, m)
        }(v.Value)
        case core.TermMaybe:
        return func (m any) any {
          return InferTypeOfOptional(fcx2.(context.Context), cx, m)
        }(v.Value)
        case core.TermPair:
        return func (p any) any {
          return InferTypeOfPair(fcx2.(context.Context), cx, p)
        }(v.Value)
        case core.TermRecord:
        return func (r core.Record) any {
          return InferTypeOfRecord(fcx2.(context.Context), cx, r)
        }(v.Value)
        case core.TermSet:
        return func (s []any) any {
          return InferTypeOfSet(fcx2.(context.Context), cx, s)
        }(v.Value)
        case core.TermTypeApplication:
        return func (tt core.TypeApplicationTerm) any {
          return InferTypeOfTypeApplication(fcx2.(context.Context), cx, tt)
        }(v.Value)
        case core.TermTypeLambda:
        return func (ta core.TypeLambda) any {
          return InferTypeOfTypeLambda(fcx2.(context.Context), cx, ta)
        }(v.Value)
        case core.TermUnion:
        return func (i core.Injection) any {
          return InferTypeOfInjection(fcx2.(context.Context), cx, i)
        }(v.Value)
        case core.TermUnit:
        return func (_ struct{}) any {
          return [2]any{"right", InferTypeOfUnit(fcx2.(context.Context))}
        }(v)
        case core.TermVariable:
        return func (name core.Name) any {
          return InferTypeOfVariable(fcx2.(context.Context), cx, name)
        }(v.Value)
        case core.TermWrap:
        return func (w core.WrappedTerm) any {
          return InferTypeOfWrappedTerm(fcx2.(context.Context), cx, w)
        }(v.Value)
      }
      return nil
    }(term)
  }()
}

func InferTypeOfTypeLambda (fcx context.Context, cx graph.Graph, ta core.TypeLambda) any {
  return InferTypeOfTerm(fcx, cx, func (v any) any {
    return v.(core.TypeLambda).Body
  }(ta).(core.Term), "type abstraction")
}

func InferTypeOfTypeApplication (fcx context.Context, cx graph.Graph, tt core.TypeApplicationTerm) any {
  return InferTypeOfTerm(fcx, cx, func (v any) any {
    return v.(core.TypeApplicationTerm).Body
  }(tt).(core.Term), "type application term")
}

func InferTypeOfUnit (fcx context.Context) typing.InferenceResult {
  return typing.InferenceResult{Term: core.TermUnit{}, Type_: core.TypeUnit{}, Subst: substitution.IdTypeSubst, ClassConstraints: libmaps.Empty, Context: fcx}
}

func InferTypeOfUnwrap (fcx context.Context, cx graph.Graph, tname core.Name) any {
  return libeithers.Bind(schemas.RequireSchemaType(fcx, func (v any) any {
    return v.(graph.Graph).SchemaTypes
  }(cx).([]any), tname)).(func(any) any)(func (stRp any) any {
    return func () any {
      var schemaType any = libpairs.First(stRp)
      return func () any {
        var fcx2 any = libpairs.Second(stRp)
        return func () any {
          var svars any = schemaType.(core.TypeScheme).Variables
          return func () any {
            var stype any = schemaType.(core.TypeScheme).Type_
            return libeithers.Bind(extractcore.WrappedType[core.Name](fcx2.(context.Context), tname, stype.(core.Type))).(func(any) any)(func (wtyp core.Type) any {
              return [2]any{"right", Yield(fcx2.(context.Context), BuildTypeApplicationTerm(svars.([]any), core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationWrap{Value: tname}}}), core.TypeFunction{Value: core.FunctionType{Domain: schemas.NominalApplication(tname, liblists.Map(func (x core.Name) any {
                return core.TypeVariable{Value: x}
              }).(func(any) any)(svars).([]any)), Codomain: wtyp}}, substitution.IdTypeSubst)}
            })
          }()
        }()
      }()
    }()
  })
}

func InferTypeOfVariable (fcx context.Context, cx graph.Graph, name core.Name) any {
  return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("Variable not bound to type: ").(func(any) any)(func (v any) any {
    return v
  }(name)).(string))}, Context: fcx}}).(func(any) any)(func (scheme core.TypeScheme) any {
    return func () any {
      var tsResult any = schemas.InstantiateTypeScheme(fcx, scheme)
      return func () any {
        var ts any = libpairs.First(tsResult)
        return func () any {
          var fcx2 any = libpairs.Second(tsResult)
          return func () any {
            var constraints any = libmaybes.FromMaybe(libmaps.Empty).(func(any) any)(ts.(core.TypeScheme).Constraints)
            return [2]any{"right", typing.InferenceResult{Term: BuildTypeApplicationTerm(ts.(core.TypeScheme).Variables, core.TermVariable{Value: name}), Type_: ts.(core.TypeScheme).Type_, Subst: substitution.IdTypeSubst, ClassConstraints: constraints.([]any), Context: fcx2.(context.Context)}}
          }()
        }()
      }()
    }()
  }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(func (v any) any {
    return v.(graph.Graph).BoundTypes
  }(cx)))
}

func InferTypeOfWrappedTerm (fcx context.Context, cx graph.Graph, wt core.WrappedTerm) any {
  return func () any {
    var tname any = func (v any) any {
      return v.(core.WrappedTerm).TypeName
    }(wt)
    return func () any {
      var term any = func (v any) any {
        return v.(core.WrappedTerm).Body
      }(wt)
      return libeithers.Bind(schemas.RequireSchemaType(fcx, func (v any) any {
        return v.(graph.Graph).SchemaTypes
      }(cx).([]any), tname.(core.Name))).(func(any) any)(func (stRp any) any {
        return func () any {
          var schemaType any = libpairs.First(stRp)
          return func () any {
            var fcx2 any = libpairs.Second(stRp)
            return libeithers.Bind(InferTypeOfTerm(fcx2.(context.Context), cx, term.(core.Term), "wrapped term")).(func(any) any)(func (result typing.InferenceResult) any {
              return func () any {
                var fcx3 any = func (v any) any {
                  return v.(typing.InferenceResult).Context
                }(result)
                return func () any {
                  var svars any = schemaType.(core.TypeScheme).Variables
                  return func () any {
                    var stype any = schemaType.(core.TypeScheme).Type_
                    return func () any {
                      var iterm any = func (v any) any {
                        return v.(typing.InferenceResult).Term
                      }(result)
                      return func () any {
                        var itype any = func (v any) any {
                          return v.(typing.InferenceResult).Type_
                        }(result)
                        return func () any {
                          var isubst any = func (v any) any {
                            return v.(typing.InferenceResult).Subst
                          }(result)
                          return func () any {
                            var ityp any = core.TypeWrap{Value: itype.(core.Type)}
                            return libeithers.Bind(MapConstraints[typing.InferenceResult](fcx3.(context.Context), cx, func (_p typing.TypeSubst) any {
                              return func (subst typing.TypeSubst) any {
                                return Yield(fcx3.(context.Context), BuildTypeApplicationTerm(svars.([]any), core.TermWrap{Value: core.WrappedTerm{TypeName: tname.(core.Name), Body: iterm.(core.Term)}}), schemas.NominalApplication(tname.(core.Name), liblists.Map(func (x core.Name) any {
                                  return core.TypeVariable{Value: x}
                                }).(func(any) any)(svars).([]any)), substitution.ComposeTypeSubst(isubst.(typing.TypeSubst), subst))
                              }(_p)
                            }, []any{typing.TypeConstraint{Left: stype.(core.Type), Right: ityp.(core.Type), Comment: "schema type of wrapper"}})).(func(any) any)(func (mcResult typing.InferenceResult) any {
                              return [2]any{"right", mcResult}
                            })
                          }()
                        }()
                      }()
                    }()
                  }()
                }()
              }()
            })
          }()
        }()
      })
    }()
  }()
}

func InferTypesOfTemporaryBindings (fcx context.Context, cx graph.Graph, bins []any) any {
  return func () any {
    var dflt any = func () any {
      var binding any = liblists.Head(bins)
      return func () any {
        var k any = binding.(core.Binding).Name
        return func () any {
          var v any = binding.(core.Binding).Term
          return func () any {
            var tl any = liblists.Tail(bins)
            return libeithers.Bind(InferTypeOfTerm(fcx, cx, v.(core.Term), libstrings.Cat([]any{"temporary let binding '", k, "'"}).(string))).(func(any) any)(func (result1 typing.InferenceResult) any {
              return func () any {
                var fcx2 any = func (v any) any {
                  return v.(typing.InferenceResult).Context
                }(result1)
                return func () any {
                  var j any = func (v any) any {
                    return v.(typing.InferenceResult).Term
                  }(result1)
                  return func () any {
                    var u_prime any = func (v any) any {
                      return v.(typing.InferenceResult).Type_
                    }(result1)
                    return func () any {
                      var u any = func (v any) any {
                        return v.(typing.InferenceResult).Subst
                      }(result1)
                      return func () any {
                        var c1Inferred any = func (v any) any {
                          return v.(typing.InferenceResult).ClassConstraints
                        }(result1)
                        return libeithers.Bind(libmaybes.Maybe([2]any{"right", libmaps.Empty}).(func(any) any)(func (ts core.TypeScheme) any {
                          return func () any {
                            var tsResult any = schemas.InstantiateTypeScheme(fcx2.(context.Context), ts)
                            return func () any {
                              var instantiatedTs any = libpairs.First(tsResult)
                              return func () any {
                                var freshConstraints any = libmaybes.FromMaybe(libmaps.Empty).(func(any) any)(instantiatedTs.(core.TypeScheme).Constraints)
                                return libeithers.Bind(libeithers.Bimap(func (_ic context.InContext[error.UnificationError]) any {
                                  return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(func (v any) any {
                                    return v.(context.InContext[error.Error]).Object
                                  }(_ic).(error.UnificationError).Message)}, Context: func (v any) any {
                                    return v.(context.InContext[error.Error]).Context
                                  }(_ic).(context.Context)}
                                }).(func(any) any)(func (_a typing.TypeSubst) any {
                                  return _a
                                }).(func(any) any)(unification.UnifyTypes(fcx2.(context.Context), func (v any) any {
                                  return v.(graph.Graph).SchemaTypes
                                }(cx).([]any), instantiatedTs.(core.TypeScheme).Type_, u_prime.(core.Type), "original binding type"))).(func(any) any)(func (unifySubst typing.TypeSubst) any {
                                  return [2]any{"right", substitution.SubstInClassConstraints(unifySubst, freshConstraints.([]any))}
                                })
                              }()
                            }()
                          }()
                        }).(func(any) any)(binding.(core.Binding).Type_)).(func(any) any)(func (originalBindingConstraints []any) any {
                          return func () any {
                            var c1 any = MergeClassConstraints(c1Inferred.([]any), originalBindingConstraints)
                            return libeithers.Bind(InferTypesOfTemporaryBindings(fcx2.(context.Context), substitution.SubstInContext(u.(typing.TypeSubst), cx), tl.([]any))).(func(any) any)(func (rp2 any) any {
                              return func () any {
                                var result2 any = libpairs.First(rp2)
                                return func () any {
                                  var fcx3 any = libpairs.Second(rp2)
                                  return func () any {
                                    var h any = libpairs.First(result2)
                                    return func () any {
                                      var r_prime any = libpairs.First(libpairs.Second(result2))
                                      return func () any {
                                        var restPair any = libpairs.Second(libpairs.Second(result2))
                                        return func () any {
                                          var r any = libpairs.First(restPair)
                                          return func () any {
                                            var c2 any = libpairs.Second(restPair)
                                            return func () any {
                                              var c1Subst any = substitution.SubstInClassConstraints(r.(typing.TypeSubst), c1.([]any))
                                              return func () any {
                                                var mergedConstraints any = MergeClassConstraints(c1Subst.([]any), c2.([]any))
                                                return [2]any{"right", [2]any{[2]any{liblists.Cons(substitution.SubstTypesInTerm(r.(typing.TypeSubst), j.(core.Term))).(func(any) any)(h), [2]any{liblists.Cons(substitution.SubstInType(r.(typing.TypeSubst), u_prime.(core.Type))).(func(any) any)(r_prime), [2]any{substitution.ComposeTypeSubst(u.(typing.TypeSubst), r.(typing.TypeSubst)), mergedConstraints}}}, fcx3}}
                                              }()
                                            }()
                                          }()
                                        }()
                                      }()
                                    }()
                                  }()
                                }()
                              }()
                            })
                          }()
                        })
                      }()
                    }()
                  }()
                }()
              }()
            })
          }()
        }()
      }()
    }()
    return liblogic.IfElse(liblists.Null(bins)).(func(any) any)([2]any{"right", [2]any{[2]any{[]any{}, [2]any{[]any{}, [2]any{substitution.IdTypeSubst, libmaps.Empty}}}, fcx}}).(func(any) any)(dflt)
  }()
}

func IsUnbound (cx graph.Graph, v core.Name) bool {
  return liblogic.And(liblogic.Not(libsets.Member(v).(func(any) any)(FreeVariablesInContext(cx)))).(func(any) any)(liblogic.Not(libmaps.Member(v).(func(any) any)(func (v any) any {
    return v.(graph.Graph).SchemaTypes
  }(cx)))).(bool)
}

func MapConstraints[T0 any] (flowCx context.Context, cx graph.Graph, f func(typing.TypeSubst) T0, constraints []any) any {
  return libeithers.Bind(libeithers.Bimap(func (_ic context.InContext[error.UnificationError]) any {
    return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(func (v any) any {
      return v.(context.InContext[error.Error]).Object
    }(_ic).(error.UnificationError).Message)}, Context: func (v any) any {
      return v.(context.InContext[error.Error]).Context
    }(_ic).(context.Context)}
  }).(func(any) any)(func (_a typing.TypeSubst) any {
    return _a
  }).(func(any) any)(unification.UnifyTypeConstraints(flowCx, func (v any) any {
    return v.(graph.Graph).SchemaTypes
  }(cx).([]any), constraints))).(func(any) any)(func (s typing.TypeSubst) any {
    return libeithers.Bind(checking.CheckTypeSubst(flowCx, cx, s)).(func(any) any)(func (_ typing.TypeSubst) any {
      return [2]any{"right", f(s)}
    })
  })
}

func MergeClassConstraints (m1 []any, m2 []any) []any {
  return liblists.Foldl(func (acc []any) any {
    return func (pair any) any {
      return func () any {
        var k any = libpairs.First(pair)
        return func () any {
          var v any = libpairs.Second(pair)
          return libmaybes.Maybe(libmaps.Insert(k).(func(any) any)(v).(func(any) any)(acc)).(func(any) any)(func (existing core.TypeVariableMetadata) any {
            return func () any {
              var merged any = core.TypeVariableMetadata{Classes: libsets.Union(func (v any) any {
                return v.(core.TypeVariableMetadata).Classes
              }(existing)).(func(any) any)(v.(core.TypeVariableMetadata).Classes).([]any)}
              return libmaps.Insert(k).(func(any) any)(merged).(func(any) any)(acc)
            }()
          }).(func(any) any)(libmaps.Lookup(k).(func(any) any)(acc))
        }()
      }()
    }
  }).(func(any) any)(m1).(func(any) any)(libmaps.ToList(m2)).([]any)
}

func ShowInferenceResult (result typing.InferenceResult) string {
  return func () any {
    var term any = func (v any) any {
      return v.(typing.InferenceResult).Term
    }(result)
    return func () any {
      var typ any = func (v any) any {
        return v.(typing.InferenceResult).Type_
      }(result)
      return func () any {
        var subst any = func (v any) any {
          return v.(typing.InferenceResult).Subst
        }(result)
        return libstrings.Cat([]any{"{term=", showcore.Term(term.(core.Term)), ", type=", showcore.Type_(typ.(core.Type)), ", subst=", showtyping.TypeSubst(subst.(typing.TypeSubst)), "}"})
      }()
    }()
  }().(string)
}

func Yield (fcx context.Context, term core.Term, typ core.Type, subst typing.TypeSubst) typing.InferenceResult {
  return typing.InferenceResult{Term: substitution.SubstTypesInTerm(subst, term), Type_: substitution.SubstInType(subst, typ), Subst: subst, ClassConstraints: libmaps.Empty, Context: fcx}
}

func YieldChecked (fcx context.Context, term core.Term, typ core.Type, subst typing.TypeSubst) typing.InferenceResult {
  return func () any {
    var iterm any = substitution.SubstTypesInTerm(subst, term)
    return func () any {
      var itype any = substitution.SubstInType(subst, typ)
      return typing.InferenceResult{Term: iterm.(core.Term), Type_: itype.(core.Type), Subst: subst, ClassConstraints: libmaps.Empty, Context: fcx}
    }()
  }().(typing.InferenceResult)
}

func YieldCheckedWithConstraints (fcx context.Context, term core.Term, typ core.Type, subst typing.TypeSubst, constraints []any) typing.InferenceResult {
  return func () any {
    var iterm any = substitution.SubstTypesInTerm(subst, term)
    return func () any {
      var itype any = substitution.SubstInType(subst, typ)
      return func () any {
        var iconstraints any = substitution.SubstInClassConstraints(subst, constraints)
        return typing.InferenceResult{Term: iterm.(core.Term), Type_: itype.(core.Type), Subst: subst, ClassConstraints: iconstraints.([]any), Context: fcx}
      }()
    }()
  }().(typing.InferenceResult)
}

func YieldDebug[T0 any] (fcx context.Context, cx T0, debugId string, term core.Term, typ core.Type, subst typing.TypeSubst) any {
  return func () any {
    var rterm any = substitution.SubstTypesInTerm(subst, term)
    return func () any {
      var rtyp any = substitution.SubstInType(subst, typ)
      return libeithers.Bind(annotations.DebugIf(fcx, debugId, libstrings.Cat([]any{"\n\tterm: ", showcore.Term(term), "\n\ttyp: ", showcore.Type_(typ), "\n\tsubst: ", showtyping.TypeSubst(subst), "\n\trterm: ", showcore.Term(rterm.(core.Term)), "\n\trtyp: ", showcore.Type_(rtyp.(core.Type))}).(string))).(func(any) any)(func (result struct{}) any {
        return [2]any{"right", typing.InferenceResult{Term: rterm.(core.Term), Type_: rtyp.(core.Type), Subst: subst, ClassConstraints: libmaps.Empty, Context: fcx}}
      })
    }()
  }()
}

func YieldWithConstraints (fcx context.Context, term core.Term, typ core.Type, subst typing.TypeSubst, constraints []any) typing.InferenceResult {
  return typing.InferenceResult{Term: substitution.SubstTypesInTerm(subst, term), Type_: substitution.SubstInType(subst, typ), Subst: subst, ClassConstraints: constraints, Context: fcx}
}
