// Note: this is an automatically generated file. Do not edit.

package hoisting

import (
  "hydra.dev/hydra/accessors"
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
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
  "hydra.dev/hydra/sorting"
  "hydra.dev/hydra/substitution"
  "hydra.dev/hydra/typing"
)

func AugmentBindingsWithNewFreeVars (cx graph.Graph, boundVars []any, bindings []any) any {
  return func () any {
    var types any = libmaps.Map(rewriting.TypeSchemeToFType).(func(any) any)(func (v any) any {
      return v.(graph.Graph).BoundTypes
    }(cx))
    return func () any {
      var wrapAfterTypeLambdas func([]any) any
      wrapAfterTypeLambdas = func (vars []any) any {
        return func (term core.Term) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.TermTypeLambda:
              return func (tl core.TypeLambda) any {
                return core.TermTypeLambda{Value: core.TypeLambda{Parameter: func (v any) any {
                  return v.(core.TypeLambda).Parameter
                }(tl).(core.Name), Body: wrapAfterTypeLambdas(vars).(func(any) any)(func (v any) any {
                  return v.(core.TypeLambda).Body
                }(tl)).(core.Term)}}
              }(v.Value)
              default:
              return liblists.Foldl(func (t core.Term) any {
                return func (p any) any {
                  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: libpairs.First(p).(core.Name), Domain: libpairs.Second(p), Body: t}}}
                }
              }).(func(any) any)(term).(func(any) any)(liblists.Reverse(vars))
            }
            return nil
          }(term)
        }
      }
      return func () any {
        augment := func (b core.Binding) any {
          return func () any {
            var freeVars any = libsets.ToList(libsets.Intersection(boundVars).(func(any) any)(rewriting.FreeVariablesInTerm(func (v any) any {
              return v.(core.Binding).Term
            }(b).(core.Term))))
            return func () any {
              var varTypePairs any = liblists.Map(func (v core.Name) any {
                return [2]any{v, libmaps.Lookup(v).(func(any) any)(types)}
              }).(func(any) any)(freeVars)
              return func () any {
                var varTypes any = libmaybes.Cat(liblists.Map(libpairs.Second).(func(any) any)(varTypePairs))
                return liblogic.IfElse(liblogic.Or(liblists.Null(freeVars)).(func(any) any)(liblogic.Not(libequality.Equal(liblists.Length(varTypes)).(func(any) any)(liblists.Length(varTypePairs))))).(func(any) any)([2]any{b, nil}).(func(any) any)([2]any{core.Binding{Name: func (v any) any {
                  return v.(core.Binding).Name
                }(b).(core.Name), Term: wrapAfterTypeLambdas(varTypePairs.([]any)).(func(any) any)(func (v any) any {
                  return v.(core.Binding).Term
                }(b)).(core.Term), Type_: libmaybes.Map(func (ts core.TypeScheme) any {
                  return core.TypeScheme{Variables: func (v any) any {
                    return v.(core.TypeScheme).Variables
                  }(ts).([]any), Type_: liblists.Foldl(func (acc core.Type) any {
                    return func (t core.Type) any {
                      return core.TypeFunction{Value: core.FunctionType{Domain: t, Codomain: acc}}
                    }
                  }).(func(any) any)(func (v any) any {
                    return v.(core.TypeScheme).Type_
                  }(ts)).(func(any) any)(liblists.Reverse(varTypes)).(core.Type), Constraints: func (v any) any {
                    return v.(core.TypeScheme).Constraints
                  }(ts)}
                }).(func(any) any)(func (v any) any {
                  return v.(core.Binding).Type_
                }(b))}, func () any {
                  _v := [2]any{func (v any) any {
                    return v.(core.Binding).Name
                  }(b), liblists.Foldl(func (t core.Term) any {
                    return func (v core.Name) any {
                      return core.TermApplication{Value: core.Application{Function: t, Argument: core.TermVariable{Value: v}}}
                    }
                  }).(func(any) any)(core.TermVariable{Value: func (v any) any {
                    return v.(core.Binding).Name
                  }(b).(core.Name)}).(func(any) any)(freeVars)}
                  return &_v
                }()})
              }()
            }()
          }()
        }
        return func () any {
          var results any = liblists.Map(augment).(func(any) any)(bindings)
          return [2]any{liblists.Map(libpairs.First).(func(any) any)(results), typing.TermSubst(libmaps.FromList(libmaybes.Cat(liblists.Map(libpairs.Second).(func(any) any)(results))).([]any))}
        }()
      }()
    }()
  }()
}

func BindingIsPolymorphic (binding core.Binding) bool {
  return libmaybes.Maybe(false).(func(any) any)(func (ts core.TypeScheme) any {
    return liblogic.Not(liblists.Null(func (v any) any {
      return v.(core.TypeScheme).Variables
    }(ts)))
  }).(func(any) any)(func (v any) any {
    return v.(core.Binding).Type_
  }(binding)).(bool)
}

func BindingUsesContextTypeVars (cx graph.Graph, binding core.Binding) bool {
  return libmaybes.Maybe(false).(func(any) any)(func (ts core.TypeScheme) any {
    return func () any {
      var freeInType any = rewriting.FreeVariablesInType(func (v any) any {
        return v.(core.TypeScheme).Type_
      }(ts).(core.Type))
      return func () any {
        var contextTypeVars any = func (v any) any {
          return v.(graph.Graph).TypeVariables
        }(cx)
        return liblogic.Not(libsets.Null(libsets.Intersection(freeInType).(func(any) any)(contextTypeVars)))
      }()
    }()
  }).(func(any) any)(func (v any) any {
    return v.(core.Binding).Type_
  }(binding)).(bool)
}

func CountVarOccurrences (name core.Name, term core.Term) int32 {
  return func () any {
    var childCount any = liblists.Foldl(func (acc int32) any {
      return func (t core.Term) any {
        return libmath.Add(acc).(func(any) any)(CountVarOccurrences(name, t))
      }
    }).(func(any) any)(0).(func(any) any)(rewriting.Subterms(term))
    return func (x any) any {
      switch v := x.(type) {
        case core.TermVariable:
        return func (v core.Name) any {
          return liblogic.IfElse(libequality.Equal(v).(func(any) any)(name)).(func(any) any)(libmath.Add(1).(func(any) any)(childCount)).(func(any) any)(childCount)
        }(v.Value)
        default:
        return childCount
      }
      return nil
    }(term)
  }().(int32)
}

func HoistAllLetBindings (let0 core.Let) core.Let {
  return func () any {
    var emptyCx any = graph.Graph{BoundTerms: libmaps.Empty, BoundTypes: libmaps.Empty, ClassConstraints: libmaps.Empty, LambdaVariables: libsets.Empty, Metadata: libmaps.Empty, Primitives: libmaps.Empty, SchemaTypes: libmaps.Empty, TypeVariables: libsets.Empty}
    return HoistLetBindingsWithPredicate(func (_p core.Binding) bool {
      return func (_ core.Binding) bool {
        return true
      }(_p).(bool)
    }, func (_p graph.Graph) func(core.Binding) bool {
      return ShouldHoistAll(_p)
    }, emptyCx.(graph.Graph), let0)
  }().(core.Let)
}

func HoistCaseStatements (v1 graph.Graph, v2 core.Term) core.Term {
  return HoistSubterms(ShouldHoistCaseStatement, v1, v2)
}

func HoistCaseStatementsInGraph (bindings []any) []any {
  return func () any {
    var emptyTx any = graph.Graph{BoundTerms: libmaps.Empty, BoundTypes: libmaps.Empty, ClassConstraints: libmaps.Empty, LambdaVariables: libsets.Empty, Metadata: libmaps.Empty, Primitives: libmaps.Empty, SchemaTypes: libmaps.Empty, TypeVariables: libsets.Empty}
    return func () any {
      var term0 any = core.TermLet{Value: core.Let{Bindings: bindings, Body: core.TermUnit{}}}
      return func () any {
        var term1 any = HoistCaseStatements(emptyTx.(graph.Graph), term0.(core.Term))
        return schemas.TermAsBindings(term1.(core.Term))
      }()
    }()
  }().([]any)
}

func HoistLetBindingsWithContext (isParentBinding func(core.Binding) bool, cx graph.Graph, let0 core.Let) core.Let {
  return HoistLetBindingsWithPredicate(isParentBinding, ShouldHoistPolymorphic, cx, let0)
}

func HoistLetBindingsWithPredicate (isParentBinding func(core.Binding) bool, shouldHoistBinding func(graph.Graph) func(core.Binding) bool, cx0 graph.Graph, let0 core.Let) core.Let {
  return func () any {
    hoistOne := func (prefix string) any {
      return func (cx graph.Graph) any {
        return func (pair any) any {
          return func (bindingWithCapturedVars any) any {
            return func () any {
              var bindingAndReplacementPairs any = libpairs.First(pair)
              return func () any {
                var alreadyUsedNames any = libpairs.Second(pair)
                return func () any {
                  var b any = libpairs.First(bindingWithCapturedVars)
                  return func () any {
                    var capturedTermVars any = libpairs.Second(bindingWithCapturedVars)
                    return func () any {
                      var types any = libmaps.Map(rewriting.TypeSchemeToFType).(func(any) any)(func (v any) any {
                        return v.(graph.Graph).BoundTypes
                      }(cx))
                      return func () any {
                        var capturedTermVarTypePairs any = liblists.Map(func (v core.Name) any {
                          return [2]any{v, libmaps.Lookup(v).(func(any) any)(types)}
                        }).(func(any) any)(capturedTermVars)
                        return func () any {
                          var capturedTermVarTypes any = liblists.Map(func (typ core.Type) any {
                            return rewriting.DeannotateTypeParameters(typ)
                          }).(func(any) any)(libmaybes.Cat(liblists.Map(libpairs.Second).(func(any) any)(capturedTermVarTypePairs)))
                          return func () any {
                            var freeInBindingType any = libmaybes.Maybe(libsets.Empty).(func(any) any)(func (ts core.TypeScheme) any {
                              return rewriting.FreeVariablesInType(func (v any) any {
                                return v.(core.TypeScheme).Type_
                              }(ts).(core.Type))
                            }).(func(any) any)(b.(core.Binding).Type_)
                            return func () any {
                              var freeInCapturedVarTypes any = libsets.Unions(liblists.Map(func (t core.Type) any {
                                return rewriting.FreeVariablesInType(t)
                              }).(func(any) any)(capturedTermVarTypes))
                              return func () any {
                                var capturedTypeVars any = libsets.ToList(libsets.Intersection(func (v any) any {
                                  return v.(graph.Graph).TypeVariables
                                }(cx)).(func(any) any)(libsets.Union(freeInBindingType).(func(any) any)(freeInCapturedVarTypes)))
                                return func () any {
                                  var globalBindingName any = lexical.ChooseUniqueName(alreadyUsedNames.([]any), core.Name(libstrings.Cat2(prefix).(func(any) any)(func (v any) any {
                                    return v
                                  }(b.(core.Binding).Name)).(string)))
                                  return func () any {
                                    var newUsedNames any = libsets.Insert(globalBindingName).(func(any) any)(alreadyUsedNames)
                                    return func () any {
                                      var newTypeScheme any = liblogic.IfElse(libequality.Equal(liblists.Length(capturedTermVarTypes)).(func(any) any)(liblists.Length(capturedTermVarTypePairs))).(func(any) any)(libmaybes.Map(func (ts core.TypeScheme) any {
                                        return core.TypeScheme{Variables: liblists.Nub(liblists.Concat2(capturedTypeVars).(func(any) any)(func (v any) any {
                                          return v.(core.TypeScheme).Variables
                                        }(ts))).([]any), Type_: liblists.Foldl(func (t core.Type) any {
                                          return func (a core.Type) any {
                                            return core.TypeFunction{Value: core.FunctionType{Domain: a, Codomain: t}}
                                          }
                                        }).(func(any) any)(func (v any) any {
                                          return v.(core.TypeScheme).Type_
                                        }(ts)).(func(any) any)(liblists.Reverse(capturedTermVarTypes)).(core.Type), Constraints: func (v any) any {
                                          return v.(core.TypeScheme).Constraints
                                        }(ts)}
                                      }).(func(any) any)(b.(core.Binding).Type_)).(func(any) any)(nil)
                                      return func () any {
                                        var strippedTerm any = rewriting.StripTypeLambdas(b.(core.Binding).Term)
                                        return func () any {
                                          var termWithLambdas any = liblists.Foldl(func (t core.Term) any {
                                            return func (p any) any {
                                              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: libpairs.First(p).(core.Name), Domain: libmaybes.Map(func (dom core.Type) any {
                                                return rewriting.DeannotateTypeParameters(dom)
                                              }).(func(any) any)(libpairs.Second(p)), Body: t}}}
                                            }
                                          }).(func(any) any)(strippedTerm).(func(any) any)(liblists.Reverse(capturedTermVarTypePairs))
                                          return func () any {
                                            var termWithTypeLambdas any = liblists.Foldl(func (t core.Term) any {
                                              return func (v core.Name) any {
                                                return core.TermTypeLambda{Value: core.TypeLambda{Parameter: v, Body: t}}
                                              }
                                            }).(func(any) any)(termWithLambdas).(func(any) any)(liblists.Reverse(libmaybes.Maybe([]any{}).(func(any) any)(func (v any) any {
                                              return v.(core.TypeScheme).Variables
                                            }).(func(any) any)(newTypeScheme)))
                                            return func () any {
                                              var withTypeApps any = liblists.Foldl(func (t core.Term) any {
                                                return func (v core.Name) any {
                                                  return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: t, Type_: core.TypeVariable{Value: v}}}
                                                }
                                              }).(func(any) any)(core.TermVariable{Value: globalBindingName.(core.Name)}).(func(any) any)(capturedTypeVars)
                                              return func () any {
                                                var replacement any = liblists.Foldl(func (t core.Term) any {
                                                  return func (v core.Name) any {
                                                    return core.TermApplication{Value: core.Application{Function: t, Argument: core.TermVariable{Value: v}}}
                                                  }
                                                }).(func(any) any)(withTypeApps).(func(any) any)(capturedTermVars)
                                                return func () any {
                                                  var newBindingAndReplacement any = [2]any{core.Binding{Name: globalBindingName.(core.Name), Term: termWithTypeLambdas.(core.Term), Type_: newTypeScheme}, replacement}
                                                  return func () any {
                                                    var newPairs any = liblists.Cons(newBindingAndReplacement).(func(any) any)(bindingAndReplacementPairs)
                                                    return [2]any{newPairs, newUsedNames}
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
                    }()
                  }()
                }()
              }()
            }()
          }
        }
      }
    }
    return func () any {
      rewrite := func (prefix string) any {
        return func (recurse func(any) func(core.Term) any) any {
          return func (cx graph.Graph) any {
            return func (bindingsAndNames any) any {
              return func (term core.Term) any {
                return func () any {
                  var previouslyFinishedBindings any = libpairs.First(bindingsAndNames)
                  return func () any {
                    var emptyBindingsAndNames any = [2]any{[]any{}, libpairs.Second(bindingsAndNames)}
                    return func () any {
                      var result any = recurse(emptyBindingsAndNames)(term)
                      return func () any {
                        var newBindingsAndNames any = libpairs.First(result)
                        return func () any {
                          var bindingsSoFar any = libpairs.First(newBindingsAndNames)
                          return func () any {
                            var alreadyUsedNames any = libpairs.Second(newBindingsAndNames)
                            return func () any {
                              var newTerm any = libpairs.Second(result)
                              return func (x any) any {
                                switch v := x.(type) {
                                  case core.TermLet:
                                  return func (l core.Let) any {
                                    return func () any {
                                      var body any = func (v any) any {
                                        return v.(core.Let).Body
                                      }(l)
                                      return func () any {
                                        var partitionPair any = liblists.Partition(func (v1 core.Binding) any {
                                          return shouldHoistBinding(cx)(v1)
                                        }).(func(any) any)(func (v any) any {
                                          return v.(core.Let).Bindings
                                        }(l))
                                        return func () any {
                                          var hoistUs any = libpairs.First(partitionPair)
                                          return func () any {
                                            var keepUs any = libpairs.Second(partitionPair)
                                            return func () any {
                                              var hoistedBindingNames any = liblists.Map(func (v any) any {
                                                return v.(core.Binding).Name
                                              }).(func(any) any)(hoistUs)
                                              return func () any {
                                                var polyLetVariables any = libsets.FromList(liblists.Filter(func (v core.Name) any {
                                                  return libmaybes.Maybe(false).(func(any) any)(schemas.FTypeIsPolymorphic).(func(any) any)(libmaybes.Map(rewriting.TypeSchemeToFType).(func(any) any)(libmaps.Lookup(v).(func(any) any)(func (v any) any {
                                                    return v.(graph.Graph).BoundTypes
                                                  }(cx))))
                                                }).(func(any) any)(libsets.ToList(libsets.Difference(libsets.FromList(libmaps.Keys(func (v any) any {
                                                  return v.(graph.Graph).BoundTerms
                                                }(cx)))).(func(any) any)(func (v any) any {
                                                  return v.(graph.Graph).LambdaVariables
                                                }(cx)))))
                                                return func () any {
                                                  var boundTermVariables any = libsets.Union(func (v any) any {
                                                    return v.(graph.Graph).LambdaVariables
                                                  }(cx)).(func(any) any)(libsets.Difference(libsets.FromList(libmaps.Keys(func (v any) any {
                                                    return v.(graph.Graph).BoundTerms
                                                  }(cx)))).(func(any) any)(func (v any) any {
                                                    return v.(graph.Graph).LambdaVariables
                                                  }(cx)))
                                                  return func () any {
                                                    var freeVariablesInEachBinding any = liblists.Map(func (b core.Binding) any {
                                                      return libsets.ToList(libsets.Intersection(boundTermVariables).(func(any) any)(rewriting.FreeVariablesInTerm(func (v any) any {
                                                        return v.(core.Binding).Term
                                                      }(b).(core.Term))))
                                                    }).(func(any) any)(hoistUs)
                                                    return func () any {
                                                      var bindingDependencies any = liblists.Map(func (vars []any) any {
                                                        return liblists.Partition(func (v core.Name) any {
                                                          return libsets.Member(v).(func(any) any)(libsets.FromList(hoistedBindingNames))
                                                        }).(func(any) any)(vars)
                                                      }).(func(any) any)(freeVariablesInEachBinding)
                                                      return func () any {
                                                        var bindingEdges any = liblists.Zip(hoistedBindingNames).(func(any) any)(liblists.Map(libpairs.First).(func(any) any)(bindingDependencies))
                                                        return func () any {
                                                          var bindingImmediateCapturedVars any = liblists.Zip(hoistedBindingNames).(func(any) any)(liblists.Map(libpairs.Second).(func(any) any)(bindingDependencies))
                                                          return func () any {
                                                            var capturedVarsMap any = libmaps.FromList(sorting.PropagateTags(bindingEdges.([]any), bindingImmediateCapturedVars.([]any)))
                                                            return func () any {
                                                              var bindingsWithCapturedVars any = liblists.Map(func (b core.Binding) any {
                                                                return [2]any{b, libmaybes.Maybe([]any{}).(func(any) any)(func (vars []any) any {
                                                                  return libsets.ToList(libsets.Difference(vars).(func(any) any)(polyLetVariables))
                                                                }).(func(any) any)(libmaps.Lookup(func (v any) any {
                                                                  return v.(core.Binding).Name
                                                                }(b)).(func(any) any)(capturedVarsMap))}
                                                              }).(func(any) any)(hoistUs)
                                                              return func () any {
                                                                var hoistPairsAndNames any = liblists.Foldl(func (v1 any) any {
                                                                  return func (v2 any) any {
                                                                    return hoistOne(prefix).(func(any) any)(cx).(func(any) any)(v1).(func(any) any)(v2)
                                                                  }
                                                                }).(func(any) any)([2]any{[]any{}, alreadyUsedNames}).(func(any) any)(bindingsWithCapturedVars)
                                                                return func () any {
                                                                  var hoistPairs any = liblists.Reverse(libpairs.First(hoistPairsAndNames))
                                                                  return func () any {
                                                                    var hoistedBindings any = liblists.Map(libpairs.First).(func(any) any)(hoistPairs)
                                                                    return func () any {
                                                                      var replacements any = liblists.Map(libpairs.Second).(func(any) any)(hoistPairs)
                                                                      return func () any {
                                                                        var finalUsedNames any = libpairs.Second(hoistPairsAndNames)
                                                                        return func () any {
                                                                          var hoistNameReplacementPairs any = liblists.Zip(liblists.Map(func (v any) any {
                                                                            return v.(core.Binding).Name
                                                                          }).(func(any) any)(hoistUs)).(func(any) any)(replacements)
                                                                          return func () any {
                                                                            var hoistBindingMap any = libmaps.FromList(liblists.Map(func (b core.Binding) any {
                                                                              return [2]any{func (v any) any {
                                                                                return v.(core.Binding).Name
                                                                              }(b), b}
                                                                            }).(func(any) any)(hoistUs))
                                                                            return func () any {
                                                                              isCacheable := func (name core.Name) any {
                                                                                return func () any {
                                                                                  var multiRef any = libequality.Gte(CountVarOccurrences(name, body.(core.Term))).(func(any) any)(2)
                                                                                  return func () any {
                                                                                    var isPoly any = libmaybes.Maybe(false).(func(any) any)(func (b core.Binding) any {
                                                                                      return BindingIsPolymorphic(b)
                                                                                    }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(hoistBindingMap))
                                                                                    return liblogic.And(multiRef).(func(any) any)(liblogic.Not(isPoly))
                                                                                  }()
                                                                                }()
                                                                              }
                                                                              return func () any {
                                                                                var singleRefPairs any = liblists.Filter(func (p any) any {
                                                                                  return liblogic.Not(isCacheable(libpairs.First(p).(core.Name)))
                                                                                }).(func(any) any)(hoistNameReplacementPairs)
                                                                                return func () any {
                                                                                  var multiRefPairs any = liblists.Filter(func (p any) any {
                                                                                    return isCacheable(libpairs.First(p).(core.Name))
                                                                                  }).(func(any) any)(hoistNameReplacementPairs)
                                                                                  return func () any {
                                                                                    var fullSubst any = typing.TermSubst(libmaps.FromList(hoistNameReplacementPairs).([]any))
                                                                                    return func () any {
                                                                                      var bodyOnlySubst any = typing.TermSubst(libmaps.FromList(singleRefPairs).([]any))
                                                                                      return func () any {
                                                                                        var bodySubst any = substitution.SubstituteInTerm(bodyOnlySubst.(typing.TermSubst), body.(core.Term))
                                                                                        return func () any {
                                                                                          var cacheBindings any = liblists.Map(func (p any) any {
                                                                                            return func () any {
                                                                                              var origType any = libmaybes.Maybe(nil).(func(any) any)(func (b core.Binding) any {
                                                                                                return func (v any) any {
                                                                                                  return v.(core.Binding).Type_
                                                                                                }(b)
                                                                                              }).(func(any) any)(libmaps.Lookup(libpairs.First(p)).(func(any) any)(hoistBindingMap))
                                                                                              return core.Binding{Name: libpairs.First(p).(core.Name), Term: libpairs.Second(p).(core.Term), Type_: origType}
                                                                                            }()
                                                                                          }).(func(any) any)(multiRefPairs)
                                                                                          return func () any {
                                                                                            var bodyWithCache any = liblogic.IfElse(liblists.Null(cacheBindings)).(func(any) any)(bodySubst).(func(any) any)(core.TermLet{Value: core.Let{Bindings: cacheBindings.([]any), Body: bodySubst.(core.Term)}})
                                                                                            return func () any {
                                                                                              var keepUsSubst any = liblists.Map(func (v1 core.Binding) any {
                                                                                                return substitution.SubstituteInBinding(fullSubst.(typing.TermSubst), v1)
                                                                                              }).(func(any) any)(keepUs)
                                                                                              return func () any {
                                                                                                var hoistedBindingsSubst any = liblists.Map(func (v1 core.Binding) any {
                                                                                                  return substitution.SubstituteInBinding(fullSubst.(typing.TermSubst), v1)
                                                                                                }).(func(any) any)(hoistedBindings)
                                                                                                return func () any {
                                                                                                  var bindingsSoFarSubst any = liblists.Map(func (v1 core.Binding) any {
                                                                                                    return substitution.SubstituteInBinding(fullSubst.(typing.TermSubst), v1)
                                                                                                  }).(func(any) any)(bindingsSoFar)
                                                                                                  return func () any {
                                                                                                    var augmentResult any = AugmentBindingsWithNewFreeVars(cx, libsets.Difference(boundTermVariables).(func(any) any)(polyLetVariables).([]any), bindingsSoFarSubst.([]any))
                                                                                                    return func () any {
                                                                                                      var bindingsSoFarAugmented any = libpairs.First(augmentResult)
                                                                                                      return func () any {
                                                                                                        var augmentSubst any = libpairs.Second(augmentResult)
                                                                                                        return func () any {
                                                                                                          var hoistedBindingsFinal any = liblists.Map(func (v1 core.Binding) any {
                                                                                                            return substitution.SubstituteInBinding(augmentSubst.(typing.TermSubst), v1)
                                                                                                          }).(func(any) any)(hoistedBindingsSubst)
                                                                                                          return func () any {
                                                                                                            var bindingsSoFarFinal any = liblists.Map(func (v1 core.Binding) any {
                                                                                                              return substitution.SubstituteInBinding(augmentSubst.(typing.TermSubst), v1)
                                                                                                            }).(func(any) any)(bindingsSoFarAugmented)
                                                                                                            return func () any {
                                                                                                              var bodyFinal any = substitution.SubstituteInTerm(augmentSubst.(typing.TermSubst), bodyWithCache.(core.Term))
                                                                                                              return func () any {
                                                                                                                var keepUsFinal any = liblists.Map(func (v1 core.Binding) any {
                                                                                                                  return substitution.SubstituteInBinding(augmentSubst.(typing.TermSubst), v1)
                                                                                                                }).(func(any) any)(keepUsSubst)
                                                                                                                return func () any {
                                                                                                                  var finalTerm any = liblogic.IfElse(liblists.Null(keepUsFinal)).(func(any) any)(bodyFinal).(func(any) any)(core.TermLet{Value: core.Let{Bindings: keepUsFinal.([]any), Body: bodyFinal.(core.Term)}})
                                                                                                                  return [2]any{[2]any{liblists.Concat([]any{previouslyFinishedBindings, hoistedBindingsFinal, bindingsSoFarFinal}), finalUsedNames}, finalTerm}
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
                                  default:
                                  return [2]any{[2]any{liblists.Concat2(previouslyFinishedBindings).(func(any) any)(bindingsSoFar), alreadyUsedNames}, newTerm}
                                }
                                return nil
                              }(newTerm)
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
      return func () any {
        var cx1 any = schemas.ExtendGraphForLet(func (_p graph.Graph) func(core.Binding) any {
          return func (c graph.Graph) func(core.Binding) any {
            return func (b core.Binding) any {
              return nil
            }
          }(_p).(func(core.Binding) any)
        }, cx0, let0)
        return func () any {
          forActiveBinding := func (b core.Binding) any {
            return func () any {
              var prefix any = libstrings.Cat2(func (v any) any {
                return v.(core.Binding).Name
              }(b)).(func(any) any)("_")
              return func () any {
                var init any = [2]any{[]any{}, libsets.Singleton(func (v any) any {
                  return v.(core.Binding).Name
                }(b))}
                return func () any {
                  var resultPair any = RewriteAndFoldTermWithTypeContext[any](func (_p func(any) func(core.Term) any) func(graph.Graph) func(any) func(core.Term) any {
                    return func (v1 func(any) func(core.Term) any) func(graph.Graph) func(any) func(core.Term) any {
                      return func (v2 graph.Graph) func(any) func(core.Term) any {
                        return func (v3 any) func(core.Term) any {
                          return func (v4 core.Term) any {
                            return rewrite(prefix.(string)).(func(any) any)(v1).(func(any) any)(v2).(func(any) any)(v3).(func(any) any)(v4)
                          }
                        }
                      }
                    }(_p).(func(graph.Graph) func(any) func(core.Term) any)
                  }, cx1.(graph.Graph), init, func (v any) any {
                    return v.(core.Binding).Term
                  }(b).(core.Term))
                  return func () any {
                    var resultBindings any = libpairs.First(libpairs.First(resultPair))
                    return func () any {
                      var resultTerm any = libpairs.Second(resultPair)
                      return liblists.Cons(core.Binding{Name: func (v any) any {
                        return v.(core.Binding).Name
                      }(b).(core.Name), Term: resultTerm.(core.Term), Type_: func (v any) any {
                        return v.(core.Binding).Type_
                      }(b)}).(func(any) any)(resultBindings)
                    }()
                  }()
                }()
              }()
            }()
          }
          return func () any {
            forBinding := func (b core.Binding) any {
              return liblogic.IfElse(isParentBinding(b)).(func(any) any)(forActiveBinding(b)).(func(any) any)([]any{b})
            }
            return core.Let{Bindings: liblists.Concat(liblists.Map(forBinding).(func(any) any)(func (v any) any {
              return v.(core.Let).Bindings
            }(let0))).([]any), Body: func (v any) any {
              return v.(core.Let).Body
            }(let0).(core.Term)}
          }()
        }()
      }()
    }()
  }().(core.Let)
}

func HoistPolymorphicLetBindings (isParentBinding func(core.Binding) bool, let0 core.Let) core.Let {
  return func () any {
    var emptyCx any = graph.Graph{BoundTerms: libmaps.Empty, BoundTypes: libmaps.Empty, ClassConstraints: libmaps.Empty, LambdaVariables: libsets.Empty, Metadata: libmaps.Empty, Primitives: libmaps.Empty, SchemaTypes: libmaps.Empty, TypeVariables: libsets.Empty}
    return HoistLetBindingsWithPredicate(isParentBinding, ShouldHoistPolymorphic, emptyCx.(graph.Graph), let0)
  }().(core.Let)
}

func HoistSubterms (shouldHoist func(any) bool, cx0 graph.Graph, term0 core.Term) core.Term {
  return func () any {
    processImmediateSubterm := func (cx graph.Graph) any {
      return func (counter int32) any {
        return func (namePrefix string) any {
          return func (pathPrefix []any) any {
            return func (subterm core.Term) any {
              return func () any {
                var baselineLambdaVars any = func (v any) any {
                  return v.(graph.Graph).LambdaVariables
                }(cx)
                return func () any {
                  collectAndReplace := func (recurse func(any) func(core.Term) any) any {
                    return func (path []any) any {
                      return func (cxInner graph.Graph) any {
                        return func (acc any) any {
                          return func (term core.Term) any {
                            return func () any {
                              var currentCounter any = libpairs.First(acc)
                              return func () any {
                                var collectedBindings any = libpairs.Second(acc)
                                return func (x any) any {
                                  switch v := x.(type) {
                                    case core.TermLet:
                                    return func (_ core.Let) any {
                                      return [2]any{acc, term}
                                    }(v.Value)
                                    case core.TermTypeLambda:
                                    return func (_ core.TypeLambda) any {
                                      return [2]any{acc, term}
                                    }(v.Value)
                                    default:
                                    return func () any {
                                      var result any = recurse(acc)(term)
                                      return func () any {
                                        var newAcc any = libpairs.First(result)
                                        return func () any {
                                          var processedTerm any = libpairs.Second(result)
                                          return func () any {
                                            var newCounter any = libpairs.First(newAcc)
                                            return func () any {
                                              var newBindings any = libpairs.Second(newAcc)
                                              return func () any {
                                                var fullPath any = liblists.Concat2(pathPrefix).(func(any) any)(path)
                                                return liblogic.IfElse(shouldHoist([2]any{fullPath, processedTerm})).(func(any) any)(func () any {
                                                  var bindingName any = core.Name(libstrings.Cat([]any{"_hoist_", namePrefix, "_", libliterals.ShowInt32(newCounter)}).(string))
                                                  return func () any {
                                                    var allLambdaVars any = func (v any) any {
                                                      return v.(graph.Graph).LambdaVariables
                                                    }(cxInner)
                                                    return func () any {
                                                      var newLambdaVars any = libsets.Difference(allLambdaVars).(func(any) any)(baselineLambdaVars)
                                                      return func () any {
                                                        var freeVars any = rewriting.FreeVariablesInTerm(processedTerm.(core.Term))
                                                        return func () any {
                                                          var capturedVars any = libsets.ToList(libsets.Intersection(newLambdaVars).(func(any) any)(freeVars))
                                                          return func () any {
                                                            var typeMap any = libmaps.Map(rewriting.TypeSchemeToFType).(func(any) any)(func (v any) any {
                                                              return v.(graph.Graph).BoundTypes
                                                            }(cxInner))
                                                            return func () any {
                                                              var wrappedTerm any = liblists.Foldl(func (body core.Term) any {
                                                                return func (varName core.Name) any {
                                                                  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: varName, Domain: libmaps.Lookup(varName).(func(any) any)(typeMap), Body: body}}}
                                                                }
                                                              }).(func(any) any)(processedTerm).(func(any) any)(liblists.Reverse(capturedVars))
                                                              return func () any {
                                                                var reference any = liblists.Foldl(func (fn core.Term) any {
                                                                  return func (varName core.Name) any {
                                                                    return core.TermApplication{Value: core.Application{Function: fn, Argument: core.TermVariable{Value: varName}}}
                                                                  }
                                                                }).(func(any) any)(core.TermVariable{Value: bindingName.(core.Name)}).(func(any) any)(capturedVars)
                                                                return func () any {
                                                                  var newBinding any = core.Binding{Name: bindingName.(core.Name), Term: wrappedTerm.(core.Term), Type_: nil}
                                                                  return [2]any{[2]any{libmath.Add(newCounter).(func(any) any)(1), liblists.Cons(newBinding).(func(any) any)(newBindings)}, reference}
                                                                }()
                                                              }()
                                                            }()
                                                          }()
                                                        }()
                                                      }()
                                                    }()
                                                  }()
                                                }()).(func(any) any)([2]any{newAcc, processedTerm})
                                              }()
                                            }()
                                          }()
                                        }()
                                      }()
                                    }()
                                  }
                                  return nil
                                }(term)
                              }()
                            }()
                          }
                        }
                      }
                    }
                  }
                  return func () any {
                    var result any = RewriteAndFoldTermWithTypeContextAndPath[any](func (_p func(any) func(core.Term) any) func([]any) func(graph.Graph) func(any) func(core.Term) any {
                      return collectAndReplace(_p).(func([]any) func(graph.Graph) func(any) func(core.Term) any)
                    }, cx, [2]any{counter, []any{}}, subterm)
                    return func () any {
                      var finalAcc any = libpairs.First(result)
                      return func () any {
                        var transformedSubterm any = libpairs.Second(result)
                        return func () any {
                          var finalCounter any = libpairs.First(finalAcc)
                          return func () any {
                            var bindings any = libpairs.Second(finalAcc)
                            return liblogic.IfElse(liblists.Null(bindings)).(func(any) any)([2]any{finalCounter, transformedSubterm}).(func(any) any)(func () any {
                              var localLet any = core.TermLet{Value: core.Let{Bindings: liblists.Reverse(bindings).([]any), Body: transformedSubterm.(core.Term)}}
                              return [2]any{finalCounter, localLet}
                            }())
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
    return func () any {
      processLetTerm := func (cx graph.Graph) any {
        return func (counter any) any {
          return func (path []any) any {
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
                    processBinding := func (acc []any) any {
                      return func (binding core.Binding) any {
                        return func () any {
                          var namePrefix any = libstrings.Intercalate("_").(func(any) any)(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
                            return v.(core.Binding).Name
                          }(binding)))
                          return func () any {
                            var bindingPathPrefix any = liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorLetBinding{Value: func (v any) any {
                              return v.(core.Binding).Name
                            }(binding).(core.Name)}})
                            return func () any {
                              var result any = processImmediateSubterm(cx).(func(any) any)(1).(func(any) any)(namePrefix).(func(any) any)(bindingPathPrefix).(func(any) any)(func (v any) any {
                                return v.(core.Binding).Term
                              }(binding))
                              return func () any {
                                var newValue any = libpairs.Second(result)
                                return func () any {
                                  var newBinding any = core.Binding{Name: func (v any) any {
                                    return v.(core.Binding).Name
                                  }(binding).(core.Name), Term: newValue.(core.Term), Type_: func (v any) any {
                                    return v.(core.Binding).Type_
                                  }(binding)}
                                  return liblists.Cons(newBinding).(func(any) any)(acc)
                                }()
                              }()
                            }()
                          }()
                        }()
                      }
                    }
                    return func () any {
                      var newBindingsReversed any = liblists.Foldl(processBinding).(func(any) any)([]any{}).(func(any) any)(bindings)
                      return func () any {
                        var newBindings any = liblists.Reverse(newBindingsReversed)
                        return func () any {
                          var bodyPathPrefix any = liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorLetBody{}})
                          return func () any {
                            var bodyResult any = processImmediateSubterm(cx).(func(any) any)(1).(func(any) any)("_body").(func(any) any)(bodyPathPrefix).(func(any) any)(body)
                            return func () any {
                              var newBody any = libpairs.Second(bodyResult)
                              return [2]any{counter, core.TermLet{Value: core.Let{Bindings: newBindings.([]any), Body: newBody.(core.Term)}}}
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
      return func () any {
        rewrite := func (recurse func(int32) func(core.Term) any) any {
          return func (path []any) any {
            return func (cx graph.Graph) any {
              return func (counter int32) any {
                return func (term core.Term) any {
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.TermLet:
                      return func (lt core.Let) any {
                        return func () any {
                          var recursed any = recurse(counter)(term)
                          return func () any {
                            var newCounter any = libpairs.First(recursed)
                            return func () any {
                              var recursedTerm any = libpairs.Second(recursed)
                              return func (x any) any {
                                switch v := x.(type) {
                                  case core.TermLet:
                                  return func (lt2 core.Let) any {
                                    return processLetTerm(cx).(func(any) any)(newCounter).(func(any) any)(path).(func(any) any)(lt2)
                                  }(v.Value)
                                  default:
                                  return [2]any{newCounter, recursedTerm}
                                }
                                return nil
                              }(recursedTerm)
                            }()
                          }()
                        }()
                      }(v.Value)
                      default:
                      return recurse(counter)(term)
                    }
                    return nil
                  }(term)
                }
              }
            }
          }
        }
        return libpairs.Second(RewriteAndFoldTermWithTypeContextAndPath[int32](func (_p func(any) func(core.Term) any) func([]any) func(graph.Graph) func(any) func(core.Term) any {
          return rewrite(_p).(func([]any) func(graph.Graph) func(any) func(core.Term) any)
        }, cx0, 1, term0))
      }()
    }()
  }().(core.Term)
}

func IsApplicationFunction (acc accessors.TermAccessor) bool {
  return func (x any) any {
    switch v := x.(type) {
      case accessors.TermAccessorApplicationFunction:
      return func (_ struct{}) any {
        return true
      }(v)
      default:
      return false
    }
    return nil
  }(acc).(bool)
}

func IsEliminationUnion (f core.Function) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.FunctionElimination:
      return func (e core.Elimination) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.EliminationUnion:
            return func (_ core.CaseStatement) any {
              return true
            }(v.Value)
            default:
            return false
          }
          return nil
        }(e)
      }(v.Value)
      default:
      return false
    }
    return nil
  }(f).(bool)
}

func IsLambdaBody (acc accessors.TermAccessor) bool {
  return func (x any) any {
    switch v := x.(type) {
      case accessors.TermAccessorLambdaBody:
      return func (_ struct{}) any {
        return true
      }(v)
      default:
      return false
    }
    return nil
  }(acc).(bool)
}

func IsUnionElimination (term core.Term) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermFunction:
      return func (f core.Function) any {
        return IsEliminationUnion(f)
      }(v.Value)
      default:
      return false
    }
    return nil
  }(term).(bool)
}

func IsUnionEliminationApplication (term core.Term) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermApplication:
      return func (app core.Application) any {
        return IsUnionElimination(rewriting.DeannotateAndDetypeTerm(func (v any) any {
          return v.(core.Application).Function
        }(app).(core.Term)))
      }(v.Value)
      default:
      return false
    }
    return nil
  }(term).(bool)
}

func NormalizePathForHoisting (path []any) []any {
  return func () any {
    var go_ func([]any) any
    go_ = func (remaining []any) any {
      return liblogic.IfElse(liblogic.Or(liblists.Null(remaining)).(func(any) any)(liblists.Null(liblists.Tail(remaining)))).(func(any) any)(remaining).(func(any) any)(func () any {
        var first any = liblists.Head(remaining)
        return func () any {
          var second any = liblists.Head(liblists.Tail(remaining))
          return func () any {
            var rest any = liblists.Tail(liblists.Tail(remaining))
            return liblogic.IfElse(liblogic.And(IsApplicationFunction(first.(accessors.TermAccessor))).(func(any) any)(IsLambdaBody(second.(accessors.TermAccessor)))).(func(any) any)(liblists.Cons(accessors.TermAccessorLetBody{}).(func(any) any)(go_(rest.([]any)))).(func(any) any)(liblists.Cons(first).(func(any) any)(go_(liblists.Tail(remaining).([]any))))
          }()
        }()
      }())
    }
    return go_(path)
  }().([]any)
}

func RewriteAndFoldTermWithTypeContext[T0 any] (f func(func(T0) func(core.Term) any) func(graph.Graph) func(T0) func(core.Term) any, cx0 graph.Graph, val0 T0, term0 core.Term) any {
  return func () any {
    wrapper := func (lowLevelRecurse func(any) func(core.Term) any) any {
      return func (valAndCx any) any {
        return func (term core.Term) any {
          return func () any {
            var val any = libpairs.First(valAndCx)
            return func () any {
              var cx any = libpairs.Second(valAndCx)
              return func () any {
                var cx1 any = func (x any) any {
                  switch v := x.(type) {
                    case core.TermFunction:
                    return func (fun core.Function) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.FunctionLambda:
                          return func (l core.Lambda) any {
                            return schemas.ExtendGraphForLambda(cx.(graph.Graph), l)
                          }(v.Value)
                          default:
                          return cx
                        }
                        return nil
                      }(fun)
                    }(v.Value)
                    case core.TermLet:
                    return func (l core.Let) any {
                      return schemas.ExtendGraphForLet(func (_p graph.Graph) func(core.Binding) any {
                        return func (_ graph.Graph) func(core.Binding) any {
                          return func (_2 core.Binding) any {
                            return nil
                          }
                        }(_p).(func(core.Binding) any)
                      }, cx.(graph.Graph), l)
                    }(v.Value)
                    case core.TermTypeLambda:
                    return func (tl core.TypeLambda) any {
                      return schemas.ExtendGraphForTypeLambda(cx.(graph.Graph), tl)
                    }(v.Value)
                    default:
                    return cx
                  }
                  return nil
                }(term)
                return func () any {
                  recurseForUser := func (newVal T0) any {
                    return func (subterm core.Term) any {
                      return func () any {
                        var result any = lowLevelRecurse([2]any{newVal, cx1})(subterm)
                        return [2]any{libpairs.First(libpairs.First(result)), libpairs.Second(result)}
                      }()
                    }
                  }
                  return func () any {
                    var fResult any = f(recurseForUser)(cx1)(val)(term)
                    return [2]any{[2]any{libpairs.First(fResult), cx}, libpairs.Second(fResult)}
                  }()
                }()
              }()
            }()
          }()
        }
      }
    }
    return func () any {
      var result any = rewriting.RewriteAndFoldTerm[any](func (_p func(any) func(core.Term) any) func(any) func(core.Term) any {
        return wrapper(_p).(func(any) func(core.Term) any)
      }, [2]any{val0, cx0}, term0)
      return [2]any{libpairs.First(libpairs.First(result)), libpairs.Second(result)}
    }()
  }()
}

func RewriteAndFoldTermWithTypeContextAndPath[T0 any] (f func(func(T0) func(core.Term) any) func([]any) func(graph.Graph) func(T0) func(core.Term) any, cx0 graph.Graph, val0 T0, term0 core.Term) any {
  return func () any {
    wrapper := func (recurse func([]any) func(any) func(core.Term) any) any {
      return func (path []any) any {
        return func (cxAndVal any) any {
          return func (term core.Term) any {
            return func () any {
              var cx any = libpairs.First(cxAndVal)
              return func () any {
                var val any = libpairs.Second(cxAndVal)
                return func () any {
                  var cx1 any = func (x any) any {
                    switch v := x.(type) {
                      case core.TermFunction:
                      return func (fun core.Function) any {
                        return func (x any) any {
                          switch v := x.(type) {
                            case core.FunctionLambda:
                            return func (l core.Lambda) any {
                              return schemas.ExtendGraphForLambda(cx.(graph.Graph), l)
                            }(v.Value)
                            default:
                            return cx
                          }
                          return nil
                        }(fun)
                      }(v.Value)
                      case core.TermLet:
                      return func (l core.Let) any {
                        return schemas.ExtendGraphForLet(func (_p graph.Graph) func(core.Binding) any {
                          return func (_ graph.Graph) func(core.Binding) any {
                            return func (_2 core.Binding) any {
                              return nil
                            }
                          }(_p).(func(core.Binding) any)
                        }, cx.(graph.Graph), l)
                      }(v.Value)
                      case core.TermTypeLambda:
                      return func (tl core.TypeLambda) any {
                        return schemas.ExtendGraphForTypeLambda(cx.(graph.Graph), tl)
                      }(v.Value)
                      default:
                      return cx
                    }
                    return nil
                  }(term)
                  return func () any {
                    recurseForUser := func (valIn T0) any {
                      return func (termIn core.Term) any {
                        return func () any {
                          var result any = recurse(path)([2]any{cx1, valIn})(termIn)
                          return [2]any{libpairs.Second(libpairs.First(result)), libpairs.Second(result)}
                        }()
                      }
                    }
                    return func () any {
                      var fResult any = f(recurseForUser)(path)(cx1)(val)(term)
                      return [2]any{[2]any{cx, libpairs.First(fResult)}, libpairs.Second(fResult)}
                    }()
                  }()
                }()
              }()
            }()
          }
        }
      }
    }
    return func () any {
      var result any = rewriting.RewriteAndFoldTermWithPath[any](func (_p func([]any) func(any) func(core.Term) any) func([]any) func(any) func(core.Term) any {
        return wrapper(_p).(func([]any) func(any) func(core.Term) any)
      }, [2]any{cx0, val0}, term0)
      return [2]any{libpairs.Second(libpairs.First(result)), libpairs.Second(result)}
    }()
  }()
}

func RewriteTermWithTypeContext[T0 any] (f func(func(core.Term) T0) func(graph.Graph) func(core.Term) T0, cx0 graph.Graph, term0 core.Term) T0 {
  return func () any {
    f2 := func (recurse func(graph.Graph) func(core.Term) T0) any {
      return func (cx graph.Graph) any {
        return func (term core.Term) any {
          return func () any {
            recurse1 := func (term2 core.Term) any {
              return recurse(cx)(term2)
            }
            return func (x any) any {
              switch v := x.(type) {
                case core.TermFunction:
                return func (fun core.Function) any {
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.FunctionLambda:
                      return func (l core.Lambda) any {
                        return func () any {
                          var cx1 any = schemas.ExtendGraphForLambda(cx, l)
                          return func () any {
                            recurse2 := func (term2 core.Term) any {
                              return recurse(cx1.(graph.Graph))(term2)
                            }
                            return f(recurse2)(cx1)(term)
                          }()
                        }()
                      }(v.Value)
                      default:
                      return f(recurse1)(cx)(term)
                    }
                    return nil
                  }(fun)
                }(v.Value)
                case core.TermLet:
                return func (l core.Let) any {
                  return func () any {
                    var cx1 any = schemas.ExtendGraphForLet(func (_p graph.Graph) func(core.Binding) any {
                      return func (_ graph.Graph) func(core.Binding) any {
                        return func (_2 core.Binding) any {
                          return nil
                        }
                      }(_p).(func(core.Binding) any)
                    }, cx, l)
                    return func () any {
                      recurse2 := func (term2 core.Term) any {
                        return recurse(cx1.(graph.Graph))(term2)
                      }
                      return f(recurse2)(cx1)(term)
                    }()
                  }()
                }(v.Value)
                case core.TermTypeLambda:
                return func (tl core.TypeLambda) any {
                  return func () any {
                    var cx1 any = schemas.ExtendGraphForTypeLambda(cx, tl)
                    return func () any {
                      recurse2 := func (term2 core.Term) any {
                        return recurse(cx1.(graph.Graph))(term2)
                      }
                      return f(recurse2)(cx1)(term)
                    }()
                  }()
                }(v.Value)
                default:
                return f(recurse1)(cx)(term)
              }
              return nil
            }(term)
          }()
        }
      }
    }
    return func () any {
      var rewrite func(graph.Graph) any
      rewrite = func (cx graph.Graph) any {
        return func (term core.Term) any {
          return f2(rewrite).(func(any) any)(cx).(func(any) any)(term)
        }
      }
      return rewrite(cx0).(func(any) any)(term0)
    }()
  }().(T0)
}

func ShouldHoistAll[T0, T1 any] (_ T0, _2 T1) bool {
  return true
}

func ShouldHoistCaseStatement (pathAndTerm any) bool {
  return func () any {
    var path any = libpairs.First(pathAndTerm)
    return func () any {
      var term any = libpairs.Second(pathAndTerm)
      return liblogic.IfElse(liblogic.Not(liblogic.Or(IsUnionElimination(term.(core.Term))).(func(any) any)(IsUnionEliminationApplication(term.(core.Term))))).(func(any) any)(false).(func(any) any)(func () any {
        var finalState any = liblists.Foldl(func (st any) any {
          return func (acc accessors.TermAccessor) any {
            return UpdateHoistState(acc, st)
          }
        }).(func(any) any)([2]any{true, false}).(func(any) any)(path)
        return liblogic.Not(libpairs.First(finalState))
      }())
    }()
  }().(bool)
}

func ShouldHoistPolymorphic (cx graph.Graph, binding core.Binding) bool {
  return liblogic.Or(BindingIsPolymorphic(binding)).(func(any) any)(BindingUsesContextTypeVars(cx, binding)).(bool)
}

func UpdateHoistState (accessor accessors.TermAccessor, state any) any {
  return func () any {
    var atTop any = libpairs.First(state)
    return func () any {
      var usedApp any = libpairs.Second(state)
      return liblogic.IfElse(liblogic.Not(atTop)).(func(any) any)([2]any{false, usedApp}).(func(any) any)(func (x any) any {
        switch v := x.(type) {
          case accessors.TermAccessorAnnotatedBody:
          return func (_ struct{}) any {
            return [2]any{true, usedApp}
          }(v)
          case accessors.TermAccessorLetBody:
          return func (_ struct{}) any {
            return [2]any{true, usedApp}
          }(v)
          case accessors.TermAccessorLetBinding:
          return func (_ core.Name) any {
            return [2]any{true, usedApp}
          }(v.Value)
          case accessors.TermAccessorLambdaBody:
          return func (_ struct{}) any {
            return liblogic.IfElse(usedApp).(func(any) any)([2]any{false, true}).(func(any) any)([2]any{true, false})
          }(v)
          case accessors.TermAccessorUnionCasesBranch:
          return func (_ core.Name) any {
            return liblogic.IfElse(usedApp).(func(any) any)([2]any{false, true}).(func(any) any)([2]any{true, false})
          }(v.Value)
          case accessors.TermAccessorUnionCasesDefault:
          return func (_ struct{}) any {
            return liblogic.IfElse(usedApp).(func(any) any)([2]any{false, true}).(func(any) any)([2]any{true, false})
          }(v)
          case accessors.TermAccessorApplicationFunction:
          return func (_ struct{}) any {
            return liblogic.IfElse(usedApp).(func(any) any)([2]any{false, true}).(func(any) any)([2]any{true, true})
          }(v)
          case accessors.TermAccessorApplicationArgument:
          return func (_ struct{}) any {
            return [2]any{false, usedApp}
          }(v)
          default:
          return [2]any{false, usedApp}
        }
        return nil
      }(accessor))
    }()
  }()
}
