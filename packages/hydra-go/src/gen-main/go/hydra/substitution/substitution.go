// Note: this is an automatically generated file. Do not edit.

package substitution

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/graph"
  liblists "hydra.dev/hydra/lib/lists"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  "hydra.dev/hydra/rewriting"
  "hydra.dev/hydra/typing"
)

func ComposeTypeSubst (s1 typing.TypeSubst, s2 typing.TypeSubst) typing.TypeSubst {
  return liblogic.IfElse(libmaps.Null(func (v any) any {
    return v
  }(s1))).(func(any) any)(s2).(func(any) any)(liblogic.IfElse(libmaps.Null(func (v any) any {
    return v
  }(s2))).(func(any) any)(s1).(func(any) any)(ComposeTypeSubstNonEmpty(s1, s2))).(typing.TypeSubst)
}

func ComposeTypeSubstNonEmpty (s1 typing.TypeSubst, s2 typing.TypeSubst) typing.TypeSubst {
  return func () any {
    isExtra := func (k core.Name) any {
      return func (v any) any {
        return libmaybes.IsNothing(libmaps.Lookup(k).(func(any) any)(func (v any) any {
          return v
        }(s1)))
      }
    }
    var withExtra any = libmaps.FilterWithKey(isExtra).(func(any) any)(func (v any) any {
      return v
    }(s2))
    return typing.TypeSubst(libmaps.Union(withExtra).(func(any) any)(libmaps.Map(func (v1 core.Type) any {
      return SubstInType(s2, v1)
    }).(func(any) any)(func (v any) any {
      return v
    }(s1))).([]any))
  }().(typing.TypeSubst)
}

func ComposeTypeSubstList (v1 []any) typing.TypeSubst {
  return liblists.Foldl(ComposeTypeSubst).(func(any) any)(IdTypeSubst).(func(any) any)(v1).(typing.TypeSubst)
}

var IdTypeSubst = typing.TypeSubst(libmaps.Empty)

func SingletonTypeSubst (v core.Name, t core.Type) typing.TypeSubst {
  return typing.TypeSubst(libmaps.Singleton(v).(func(any) any)(t).([]any))
}

func SubstituteInBinding (subst typing.TermSubst, b core.Binding) core.Binding {
  return core.Binding{Name: func (v any) any {
    return v.(core.Binding).Name
  }(b).(core.Name), Term: SubstituteInTerm(subst, func (v any) any {
    return v.(core.Binding).Term
  }(b).(core.Term)), Type_: func (v any) any {
    return v.(core.Binding).Type_
  }(b)}
}

func SubstituteInConstraint (subst typing.TypeSubst, c typing.TypeConstraint) typing.TypeConstraint {
  return typing.TypeConstraint{Left: SubstInType(subst, func (v any) any {
    return v.(typing.TypeConstraint).Left
  }(c).(core.Type)), Right: SubstInType(subst, func (v any) any {
    return v.(typing.TypeConstraint).Right
  }(c).(core.Type)), Comment: func (v any) any {
    return v.(typing.TypeConstraint).Comment
  }(c).(string)}
}

func SubstituteInConstraints (subst typing.TypeSubst, cs []any) []any {
  return liblists.Map(func (v1 typing.TypeConstraint) any {
    return SubstituteInConstraint(subst, v1)
  }).(func(any) any)(cs).([]any)
}

func SubstInClassConstraints (subst typing.TypeSubst, constraints []any) []any {
  return func () any {
    var substMap any = func (v any) any {
      return v
    }(subst)
    return func () any {
      insertOrMerge := func (varName core.Name) any {
        return func (metadata core.TypeVariableMetadata) any {
          return func (acc []any) any {
            return libmaybes.Maybe(libmaps.Insert(varName).(func(any) any)(metadata).(func(any) any)(acc)).(func(any) any)(func (existing core.TypeVariableMetadata) any {
              return func () any {
                var merged any = core.TypeVariableMetadata{Classes: libsets.Union(func (v any) any {
                  return v.(core.TypeVariableMetadata).Classes
                }(existing)).(func(any) any)(func (v any) any {
                  return v.(core.TypeVariableMetadata).Classes
                }(metadata)).([]any)}
                return libmaps.Insert(varName).(func(any) any)(merged).(func(any) any)(acc)
              }()
            }).(func(any) any)(libmaps.Lookup(varName).(func(any) any)(acc))
          }
        }
      }
      return liblists.Foldl(func (acc []any) any {
        return func (pair any) any {
          return func () any {
            var varName any = libpairs.First(pair)
            return func () any {
              var metadata any = libpairs.Second(pair)
              return libmaybes.Maybe(insertOrMerge(varName.(core.Name)).(func(any) any)(metadata).(func(any) any)(acc)).(func(any) any)(func (targetType core.Type) any {
                return func () any {
                  var freeVars any = libsets.ToList(rewriting.FreeVariablesInType(targetType))
                  return liblists.Foldl(func (acc2 []any) any {
                    return func (freeVar core.Name) any {
                      return insertOrMerge(freeVar).(func(any) any)(metadata).(func(any) any)(acc2)
                    }
                  }).(func(any) any)(acc).(func(any) any)(freeVars)
                }()
              }).(func(any) any)(libmaps.Lookup(varName).(func(any) any)(substMap))
            }()
          }()
        }
      }).(func(any) any)(libmaps.Empty).(func(any) any)(libmaps.ToList(constraints))
    }()
  }().([]any)
}

func SubstInContext (subst typing.TypeSubst, cx graph.Graph) graph.Graph {
  return func () any {
    var newBoundTypes any = libmaps.Map(func (v1 core.TypeScheme) any {
      return SubstInTypeScheme(subst, v1)
    }).(func(any) any)(func (v any) any {
      return v.(graph.Graph).BoundTypes
    }(cx))
    return func () any {
      var newClassConstraints any = SubstInClassConstraints(subst, func (v any) any {
        return v.(graph.Graph).ClassConstraints
      }(cx).([]any))
      return func () any {
        var cx2 any = graph.Graph{BoundTerms: func (v any) any {
          return v.(graph.Graph).BoundTerms
        }(cx).([]any), BoundTypes: newBoundTypes.([]any), ClassConstraints: func (v any) any {
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
        return graph.Graph{BoundTerms: cx2.(graph.Graph).BoundTerms, BoundTypes: cx2.(graph.Graph).BoundTypes, ClassConstraints: newClassConstraints.([]any), LambdaVariables: cx2.(graph.Graph).LambdaVariables, Metadata: cx2.(graph.Graph).Metadata, Primitives: cx2.(graph.Graph).Primitives, SchemaTypes: cx2.(graph.Graph).SchemaTypes, TypeVariables: cx2.(graph.Graph).TypeVariables}
      }()
    }()
  }().(graph.Graph)
}

func SubstituteInTerm (subst typing.TermSubst, term0 core.Term) core.Term {
  return func () any {
    var s any = func (v any) any {
      return v
    }(subst)
    rewrite := func (recurse func(core.Term) core.Term) any {
      return func (term core.Term) any {
        return func () any {
          withLambda := func (l core.Lambda) any {
            return func () any {
              var v any = func (v any) any {
                return v.(core.Lambda).Parameter
              }(l)
              var subst2 any = typing.TermSubst(libmaps.Delete(v).(func(any) any)(s).([]any))
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: v.(core.Name), Domain: func (v any) any {
                return v.(core.Lambda).Domain
              }(l), Body: SubstituteInTerm(subst2.(typing.TermSubst), func (v any) any {
                return v.(core.Lambda).Body
              }(l).(core.Term))}}}
            }()
          }
          withLet := func (lt core.Let) any {
            return func () any {
              var bindings any = func (v any) any {
                return v.(core.Let).Bindings
              }(lt)
              var names any = libsets.FromList(liblists.Map(func (v any) any {
                return v.(core.Binding).Name
              }).(func(any) any)(bindings))
              var subst2 any = typing.TermSubst(libmaps.FilterWithKey(func (k core.Name) any {
                return func (v core.Term) any {
                  return liblogic.Not(libsets.Member(k).(func(any) any)(names))
                }
              }).(func(any) any)(s).([]any))
              rewriteBinding := func (b core.Binding) any {
                return core.Binding{Name: func (v any) any {
                  return v.(core.Binding).Name
                }(b).(core.Name), Term: SubstituteInTerm(subst2.(typing.TermSubst), func (v any) any {
                  return v.(core.Binding).Term
                }(b).(core.Term)), Type_: func (v any) any {
                  return v.(core.Binding).Type_
                }(b)}
              }
              return core.TermLet{Value: core.Let{Bindings: liblists.Map(rewriteBinding).(func(any) any)(bindings).([]any), Body: SubstituteInTerm(subst2.(typing.TermSubst), func (v any) any {
                return v.(core.Let).Body
              }(lt).(core.Term))}}
            }()
          }
          return func (x any) any {
            switch v := x.(type) {
              case core.TermFunction:
              return func (fun core.Function) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.FunctionLambda:
                    return func (l core.Lambda) any {
                      return withLambda(l)
                    }(v.Value)
                    default:
                    return recurse(term)
                  }
                  return nil
                }(fun)
              }(v.Value)
              case core.TermLet:
              return func (l core.Let) any {
                return withLet(l)
              }(v.Value)
              case core.TermVariable:
              return func (name core.Name) any {
                return libmaybes.Maybe(recurse(term)).(func(any) any)(func (sterm core.Term) any {
                  return sterm
                }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(s))
              }(v.Value)
              default:
              return recurse(term)
            }
            return nil
          }(term)
        }()
      }
    }
    return rewriting.RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return rewrite(_p).(func(core.Term) core.Term)
    }, term0)
  }().(core.Term)
}

func SubstInType (subst typing.TypeSubst, typ0 core.Type) core.Type {
  return liblogic.IfElse(libmaps.Null(func (v any) any {
    return v
  }(subst))).(func(any) any)(typ0).(func(any) any)(SubstInTypeNonEmpty(subst, typ0)).(core.Type)
}

func SubstInTypeNonEmpty (subst typing.TypeSubst, typ0 core.Type) core.Type {
  return func () any {
    rewrite := func (recurse func(core.Type) core.Type) any {
      return func (typ core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeForall:
            return func (lt core.ForallType) any {
              return libmaybes.Maybe(recurse(typ)).(func(any) any)(func (styp core.Type) any {
                return core.TypeForall{Value: core.ForallType{Parameter: func (v any) any {
                  return v.(core.ForallType).Parameter
                }(lt).(core.Name), Body: SubstInType(removeVar(func (v any) any {
                  return v.(core.ForallType).Parameter
                }(lt)).(typing.TypeSubst), func (v any) any {
                  return v.(core.ForallType).Body
                }(lt).(core.Type))}}
              }).(func(any) any)(libmaps.Lookup(func (v any) any {
                return v.(core.ForallType).Parameter
              }(lt)).(func(any) any)(func (v any) any {
                return v
              }(subst)))
            }(v.Value)
            case core.TypeVariable:
            return func (v core.Name) any {
              return libmaybes.Maybe(typ).(func(any) any)(func (styp core.Type) any {
                return styp
              }).(func(any) any)(libmaps.Lookup(v).(func(any) any)(func (v any) any {
                return v
              }(subst)))
            }(v.Value)
            default:
            return recurse(typ)
          }
          return nil
        }(typ)
      }
    }
    removeVar := func (v core.Name) any {
      return typing.TypeSubst(libmaps.Delete(v).(func(any) any)(func (v any) any {
        return v
      }(subst)).([]any))
    }
    return rewriting.RewriteType(func (_p func(core.Type) core.Type) func(core.Type) core.Type {
      return rewrite(_p).(func(core.Type) core.Type)
    }, typ0)
  }().(core.Type)
}

func SubstInTypeScheme (subst typing.TypeSubst, ts core.TypeScheme) core.TypeScheme {
  return core.TypeScheme{Variables: func (v any) any {
    return v.(core.TypeScheme).Variables
  }(ts).([]any), Type_: SubstInType(subst, func (v any) any {
    return v.(core.TypeScheme).Type_
  }(ts).(core.Type)), Constraints: libmaybes.Map(func (v1 []any) any {
    return SubstInClassConstraints(subst, v1)
  }).(func(any) any)(func (v any) any {
    return v.(core.TypeScheme).Constraints
  }(ts))}
}

func SubstTypesInTerm (subst typing.TypeSubst, term0 core.Term) core.Term {
  return func () any {
    rewrite := func (recurse func(core.Term) core.Term) any {
      return func (term core.Term) any {
        return func () any {
          var dflt any = recurse(term)
          forFunction := func (f core.Function) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.FunctionElimination:
                return func (e core.Elimination) any {
                  return dflt
                }(v.Value)
                case core.FunctionLambda:
                return func (l core.Lambda) any {
                  return forLambda(l)
                }(v.Value)
                default:
                return dflt
              }
              return nil
            }(f)
          }
          forLambda := func (l core.Lambda) any {
            return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
              return v.(core.Lambda).Parameter
            }(l).(core.Name), Domain: libmaybes.Map(func (v1 core.Type) any {
              return SubstInType(subst, v1)
            }).(func(any) any)(func (v any) any {
              return v.(core.Lambda).Domain
            }(l)), Body: SubstTypesInTerm(subst, func (v any) any {
              return v.(core.Lambda).Body
            }(l).(core.Term))}}}
          }
          forLet := func (l core.Let) any {
            return func () any {
              rewriteBinding := func (b core.Binding) any {
                return core.Binding{Name: func (v any) any {
                  return v.(core.Binding).Name
                }(b).(core.Name), Term: SubstTypesInTerm(subst, func (v any) any {
                  return v.(core.Binding).Term
                }(b).(core.Term)), Type_: libmaybes.Map(func (v1 core.TypeScheme) any {
                  return SubstInTypeScheme(subst, v1)
                }).(func(any) any)(func (v any) any {
                  return v.(core.Binding).Type_
                }(b))}
              }
              return core.TermLet{Value: core.Let{Bindings: liblists.Map(rewriteBinding).(func(any) any)(func (v any) any {
                return v.(core.Let).Bindings
              }(l)).([]any), Body: SubstTypesInTerm(subst, func (v any) any {
                return v.(core.Let).Body
              }(l).(core.Term))}}
            }()
          }
          forTypeApplication := func (tt core.TypeApplicationTerm) any {
            return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: SubstTypesInTerm(subst, func (v any) any {
              return v.(core.TypeApplicationTerm).Body
            }(tt).(core.Term)), Type_: SubstInType(subst, func (v any) any {
              return v.(core.TypeApplicationTerm).Type_
            }(tt).(core.Type))}}
          }
          forTypeLambda := func (ta core.TypeLambda) any {
            return func () any {
              var param any = func (v any) any {
                return v.(core.TypeLambda).Parameter
              }(ta)
              var subst2 any = typing.TypeSubst(libmaps.Delete(param).(func(any) any)(func (v any) any {
                return v
              }(subst)).([]any))
              return core.TermTypeLambda{Value: core.TypeLambda{Parameter: param.(core.Name), Body: SubstTypesInTerm(subst2.(typing.TypeSubst), func (v any) any {
                return v.(core.TypeLambda).Body
              }(ta).(core.Term))}}
            }()
          }
          return func (x any) any {
            switch v := x.(type) {
              case core.TermFunction:
              return func (f core.Function) any {
                return forFunction(f)
              }(v.Value)
              case core.TermLet:
              return func (l core.Let) any {
                return forLet(l)
              }(v.Value)
              case core.TermTypeApplication:
              return func (ta core.TypeApplicationTerm) any {
                return forTypeApplication(ta)
              }(v.Value)
              case core.TermTypeLambda:
              return func (tl core.TypeLambda) any {
                return forTypeLambda(tl)
              }(v.Value)
              default:
              return dflt
            }
            return nil
          }(term)
        }()
      }
    }
    return rewriting.RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return rewrite(_p).(func(core.Term) core.Term)
    }, term0)
  }().(core.Term)
}
