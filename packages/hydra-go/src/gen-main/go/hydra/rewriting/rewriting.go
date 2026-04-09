// Note: this is an automatically generated file. Do not edit.

package rewriting

import (
  "hydra.dev/hydra/accessors"
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/core"
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
  "hydra.dev/hydra/names"
  "hydra.dev/hydra/sorting"
)

func ApplyInsideTypeLambdasAndAnnotations (f func(core.Term) core.Term, term0 core.Term) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (at core.AnnotatedTerm) any {
        return core.TermAnnotated{Value: core.AnnotatedTerm{Body: ApplyInsideTypeLambdasAndAnnotations(f, func (v any) any {
          return v.(core.AnnotatedTerm).Body
        }(at).(core.Term)), Annotation: func (v any) any {
          return v.(core.AnnotatedTerm).Annotation
        }(at).([]any)}}
      }(v.Value)
      case core.TermTypeLambda:
      return func (tl core.TypeLambda) any {
        return core.TermTypeLambda{Value: core.TypeLambda{Parameter: func (v any) any {
          return v.(core.TypeLambda).Parameter
        }(tl).(core.Name), Body: ApplyInsideTypeLambdasAndAnnotations(f, func (v any) any {
          return v.(core.TypeLambda).Body
        }(tl).(core.Term))}}
      }(v.Value)
      default:
      return f(term0)
    }
    return nil
  }(term0).(core.Term)
}

func DeannotateAndDetypeTerm (t core.Term) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (at core.AnnotatedTerm) any {
        return DeannotateAndDetypeTerm(func (v any) any {
          return v.(core.AnnotatedTerm).Body
        }(at).(core.Term))
      }(v.Value)
      case core.TermTypeApplication:
      return func (tt core.TypeApplicationTerm) any {
        return DeannotateAndDetypeTerm(func (v any) any {
          return v.(core.TypeApplicationTerm).Body
        }(tt).(core.Term))
      }(v.Value)
      case core.TermTypeLambda:
      return func (ta core.TypeLambda) any {
        return DeannotateAndDetypeTerm(func (v any) any {
          return v.(core.TypeLambda).Body
        }(ta).(core.Term))
      }(v.Value)
      default:
      return t
    }
    return nil
  }(t).(core.Term)
}

func DeannotateTerm (t core.Term) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (at core.AnnotatedTerm) any {
        return DeannotateTerm(func (v any) any {
          return v.(core.AnnotatedTerm).Body
        }(at).(core.Term))
      }(v.Value)
      default:
      return t
    }
    return nil
  }(t).(core.Term)
}

func DeannotateType (t core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (arg_ core.AnnotatedType) any {
        return DeannotateType(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(arg_).(core.Type))
      }(v.Value)
      default:
      return t
    }
    return nil
  }(t).(core.Type)
}

func DeannotateTypeParameters (t core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeForall:
      return func (lt core.ForallType) any {
        return DeannotateTypeParameters(func (v any) any {
          return v.(core.ForallType).Body
        }(lt).(core.Type))
      }(v.Value)
      default:
      return t
    }
    return nil
  }(DeannotateType(t)).(core.Type)
}

func DeannotateTypeRecursive (typ core.Type) core.Type {
  return func () any {
    strip := func (recurse func(core.Type) core.Type) any {
      return func (typ2 core.Type) any {
        return func () any {
          var rewritten any = recurse(typ2)
          return func (x any) any {
            switch v := x.(type) {
              case core.TypeAnnotated:
              return func (at core.AnnotatedType) any {
                return func (v any) any {
                  return v.(core.AnnotatedType).Body
                }(at)
              }(v.Value)
              default:
              return rewritten
            }
            return nil
          }(rewritten)
        }()
      }
    }
    return RewriteType(func (_p func(core.Type) core.Type) func(core.Type) core.Type {
      return strip(_p).(func(core.Type) core.Type)
    }, typ)
  }().(core.Type)
}

func DeannotateTypeSchemeRecursive (ts core.TypeScheme) core.TypeScheme {
  return func () any {
    var vars any = func (v any) any {
      return v.(core.TypeScheme).Variables
    }(ts)
    return func () any {
      var typ any = func (v any) any {
        return v.(core.TypeScheme).Type_
      }(ts)
      return func () any {
        var constraints any = func (v any) any {
          return v.(core.TypeScheme).Constraints
        }(ts)
        return core.TypeScheme{Variables: vars.([]any), Type_: DeannotateTypeRecursive(typ.(core.Type)), Constraints: constraints}
      }()
    }()
  }().(core.TypeScheme)
}

func DetypeTerm (t core.Term) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (at core.AnnotatedTerm) any {
        return func () any {
          var subj any = func (v any) any {
            return v.(core.AnnotatedTerm).Body
          }(at)
          return func () any {
            var ann any = func (v any) any {
              return v.(core.AnnotatedTerm).Annotation
            }(at)
            return core.TermAnnotated{Value: core.AnnotatedTerm{Body: DetypeTerm(subj.(core.Term)), Annotation: ann.([]any)}}
          }()
        }()
      }(v.Value)
      case core.TermTypeApplication:
      return func (tt core.TypeApplicationTerm) any {
        return DeannotateAndDetypeTerm(func (v any) any {
          return v.(core.TypeApplicationTerm).Body
        }(tt).(core.Term))
      }(v.Value)
      case core.TermTypeLambda:
      return func (ta core.TypeLambda) any {
        return DeannotateAndDetypeTerm(func (v any) any {
          return v.(core.TypeLambda).Body
        }(ta).(core.Term))
      }(v.Value)
      default:
      return t
    }
    return nil
  }(t).(core.Term)
}

func FlattenLetTerms (term core.Term) core.Term {
  return func () any {
    var rewriteBinding func(core.Binding) any
    rewriteBinding = func (binding core.Binding) any {
      return func () any {
        var key0 any = func (v any) any {
          return v.(core.Binding).Name
        }(binding)
        return func () any {
          var val0 any = func (v any) any {
            return v.(core.Binding).Term
          }(binding)
          return func () any {
            var t any = func (v any) any {
              return v.(core.Binding).Type_
            }(binding)
            return func (x any) any {
              switch v := x.(type) {
                case core.TermAnnotated:
                return func (at core.AnnotatedTerm) any {
                  return func () any {
                    var val1 any = func (v any) any {
                      return v.(core.AnnotatedTerm).Body
                    }(at)
                    return func () any {
                      var ann any = func (v any) any {
                        return v.(core.AnnotatedTerm).Annotation
                      }(at)
                      return func () any {
                        var recursive any = rewriteBinding(core.Binding{Name: key0.(core.Name), Term: val1.(core.Term), Type_: t})
                        return func () any {
                          var innerBinding any = libpairs.First(recursive)
                          return func () any {
                            var deps any = libpairs.Second(recursive)
                            return func () any {
                              var val2 any = innerBinding.(core.Binding).Term
                              return [2]any{core.Binding{Name: key0.(core.Name), Term: core.TermAnnotated{Value: core.AnnotatedTerm{Body: val2.(core.Term), Annotation: ann.([]any)}}, Type_: t}, deps}
                            }()
                          }()
                        }()
                      }()
                    }()
                  }()
                }(v.Value)
                case core.TermLet:
                return func (innerLet core.Let) any {
                  return func () any {
                    var bindings1 any = func (v any) any {
                      return v.(core.Let).Bindings
                    }(innerLet)
                    return func () any {
                      var body1 any = func (v any) any {
                        return v.(core.Let).Body
                      }(innerLet)
                      return func () any {
                        var prefix any = libstrings.Cat2(key0).(func(any) any)("_")
                        return func () any {
                          qualify := func (n core.Name) any {
                            return core.Name(libstrings.Cat2(prefix).(func(any) any)(func (v any) any {
                              return v
                            }(n)).(string))
                          }
                          return func () any {
                            toSubstPair := func (b core.Binding) any {
                              return [2]any{func (v any) any {
                                return v.(core.Binding).Name
                              }(b), qualify(func (v any) any {
                                return v.(core.Binding).Name
                              }(b).(core.Name))}
                            }
                            return func () any {
                              var subst any = libmaps.FromList(liblists.Map(toSubstPair).(func(any) any)(bindings1))
                              return func () any {
                                replaceVars := func (v1 core.Term) any {
                                  return SubstituteVariables(subst.([]any), v1)
                                }
                                return func () any {
                                  var newBody any = replaceVars(body1.(core.Term))
                                  return func () any {
                                    newBinding := func (b core.Binding) any {
                                      return core.Binding{Name: qualify(func (v any) any {
                                        return v.(core.Binding).Name
                                      }(b).(core.Name)).(core.Name), Term: replaceVars(func (v any) any {
                                        return v.(core.Binding).Term
                                      }(b).(core.Term)).(core.Term), Type_: func (v any) any {
                                        return v.(core.Binding).Type_
                                      }(b)}
                                    }
                                    return [2]any{core.Binding{Name: key0.(core.Name), Term: newBody.(core.Term), Type_: t}, liblists.Map(newBinding).(func(any) any)(bindings1)}
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
                return [2]any{core.Binding{Name: key0.(core.Name), Term: val0.(core.Term), Type_: t}, []any{}}
              }
              return nil
            }(val0)
          }()
        }()
      }()
    }
    return func () any {
      var flattenBodyLet func([]any) any
      flattenBodyLet = func (bindings []any) any {
        return func (body core.Term) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.TermLet:
              return func (innerLt core.Let) any {
                return func () any {
                  var innerBindings any = func (v any) any {
                    return v.(core.Let).Bindings
                  }(innerLt)
                  return func () any {
                    var innerBody any = func (v any) any {
                      return v.(core.Let).Body
                    }(innerLt)
                    return flattenBodyLet(liblists.Concat2(bindings).(func(any) any)(innerBindings).([]any)).(func(any) any)(innerBody)
                  }()
                }()
              }(v.Value)
              default:
              return [2]any{liblists.Concat2([]any{}).(func(any) any)(bindings), body}
            }
            return nil
          }(body)
        }
      }
      return func () any {
        flatten := func (recurse func(core.Term) core.Term) any {
          return func (term2 core.Term) any {
            return func () any {
              var rewritten any = recurse(term2)
              return func (x any) any {
                switch v := x.(type) {
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
                          forResult := func (hr any) any {
                            return liblists.Concat2(libpairs.Second(hr)).(func(any) any)(liblists.Pure(libpairs.First(hr)))
                          }
                          return func () any {
                            var flattenedBindings any = liblists.Concat(liblists.Map(func (arg_ core.Binding) any {
                              return forResult(rewriteBinding(arg_))
                            }).(func(any) any)(bindings))
                            return func () any {
                              var merged any = flattenBodyLet(flattenedBindings.([]any)).(func(any) any)(body)
                              return func () any {
                                var newBindings any = libpairs.First(merged)
                                return func () any {
                                  var newBody any = libpairs.Second(merged)
                                  return core.TermLet{Value: core.Let{Bindings: newBindings.([]any), Body: newBody.(core.Term)}}
                                }()
                              }()
                            }()
                          }()
                        }()
                      }()
                    }()
                  }(v.Value)
                  default:
                  return rewritten
                }
                return nil
              }(rewritten)
            }()
          }
        }
        return RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
          return flatten(_p).(func(core.Term) core.Term)
        }, term)
      }()
    }()
  }().(core.Term)
}

func FoldOverTerm[T0 any] (order coders.TraversalOrder, fld func(T0) func(core.Term) T0, b0 T0, term core.Term) T0 {
  return func (x any) any {
    switch v := x.(type) {
      case coders.TraversalOrderPre:
      return func (_ struct{}) any {
        return liblists.Foldl(func (v1 T0) any {
          return func (v2 core.Term) any {
            return FoldOverTerm[T0](order, fld, v1, v2)
          }
        }).(func(any) any)(fld(b0)(term)).(func(any) any)(Subterms(term))
      }(v)
      case coders.TraversalOrderPost:
      return func (_ struct{}) any {
        return fld(liblists.Foldl(func (v1 T0) any {
          return func (v2 core.Term) any {
            return FoldOverTerm[T0](order, fld, v1, v2)
          }
        }).(func(any) any)(b0).(func(any) any)(Subterms(term)).(T0))(term)
      }(v)
    }
    return nil
  }(order).(T0)
}

func FoldOverType[T0 any] (order coders.TraversalOrder, fld func(T0) func(core.Type) T0, b0 T0, typ core.Type) T0 {
  return func (x any) any {
    switch v := x.(type) {
      case coders.TraversalOrderPre:
      return func (_ struct{}) any {
        return liblists.Foldl(func (v1 T0) any {
          return func (v2 core.Type) any {
            return FoldOverType[T0](order, fld, v1, v2)
          }
        }).(func(any) any)(fld(b0)(typ)).(func(any) any)(Subtypes(typ))
      }(v)
      case coders.TraversalOrderPost:
      return func (_ struct{}) any {
        return fld(liblists.Foldl(func (v1 T0) any {
          return func (v2 core.Type) any {
            return FoldOverType[T0](order, fld, v1, v2)
          }
        }).(func(any) any)(b0).(func(any) any)(Subtypes(typ)).(T0))(typ)
      }(v)
    }
    return nil
  }(order).(T0)
}

func FTypeToTypeScheme (typ core.Type) core.TypeScheme {
  return func () any {
    var gatherForall func([]any) any
    gatherForall = func (vars []any) any {
      return func (typ2 core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeForall:
            return func (ft core.ForallType) any {
              return gatherForall(liblists.Cons(func (v any) any {
                return v.(core.ForallType).Parameter
              }(ft)).(func(any) any)(vars).([]any)).(func(any) any)(func (v any) any {
                return v.(core.ForallType).Body
              }(ft))
            }(v.Value)
            default:
            return core.TypeScheme{Variables: liblists.Reverse(vars).([]any), Type_: typ2, Constraints: nil}
          }
          return nil
        }(DeannotateType(typ2))
      }
    }
    return gatherForall([]any{}).(func(any) any)(typ)
  }().(core.TypeScheme)
}

func FreeTypeVariablesInTerm (term0 core.Term) []any {
  return func () any {
    allOf := func (sets []any) any {
      return liblists.Foldl(libsets.Union).(func(any) any)(libsets.Empty).(func(any) any)(sets)
    }
    return func () any {
      tryType := func (tvars []any) any {
        return func (typ core.Type) any {
          return libsets.Difference(FreeVariablesInType(typ)).(func(any) any)(tvars)
        }
      }
      return func () any {
        var getAll func([]any) any
        getAll = func (vars []any) any {
          return func (term core.Term) any {
            return func () any {
              recurse := func (v1 core.Term) any {
                return getAll(vars).(func(any) any)(v1)
              }
              return func () any {
                var dflt any = allOf(liblists.Map(recurse).(func(any) any)(Subterms(term)).([]any))
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
                            return func () any {
                              var domt any = libmaybes.Maybe(libsets.Empty).(func(any) any)(func (v1 core.Type) any {
                                return tryType(vars).(func(any) any)(v1)
                              }).(func(any) any)(func (v any) any {
                                return v.(core.Lambda).Domain
                              }(l))
                              return libsets.Union(domt).(func(any) any)(recurse(func (v any) any {
                                return v.(core.Lambda).Body
                              }(l).(core.Term)))
                            }()
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
                            var newVars any = libmaybes.Maybe(vars).(func(any) any)(func (ts core.TypeScheme) any {
                              return libsets.Union(vars).(func(any) any)(libsets.FromList(func (v any) any {
                                return v.(core.TypeScheme).Variables
                              }(ts)))
                            }).(func(any) any)(func (v any) any {
                              return v.(core.Binding).Type_
                            }(b))
                            return libsets.Union(getAll(newVars.([]any)).(func(any) any)(func (v any) any {
                              return v.(core.Binding).Term
                            }(b))).(func(any) any)(libmaybes.Maybe(libsets.Empty).(func(any) any)(func (ts core.TypeScheme) any {
                              return tryType(newVars.([]any)).(func(any) any)(func (v any) any {
                                return v.(core.TypeScheme).Type_
                              }(ts))
                            }).(func(any) any)(func (v any) any {
                              return v.(core.Binding).Type_
                            }(b)))
                          }()
                        }
                        return libsets.Union(allOf(liblists.Map(forBinding).(func(any) any)(func (v any) any {
                          return v.(core.Let).Bindings
                        }(l)).([]any))).(func(any) any)(recurse(func (v any) any {
                          return v.(core.Let).Body
                        }(l).(core.Term)))
                      }()
                    }(v.Value)
                    case core.TermTypeApplication:
                    return func (tt core.TypeApplicationTerm) any {
                      return libsets.Union(tryType(vars).(func(any) any)(func (v any) any {
                        return v.(core.TypeApplicationTerm).Type_
                      }(tt))).(func(any) any)(recurse(func (v any) any {
                        return v.(core.TypeApplicationTerm).Body
                      }(tt).(core.Term)))
                    }(v.Value)
                    case core.TermTypeLambda:
                    return func (tl core.TypeLambda) any {
                      return libsets.Union(tryType(vars).(func(any) any)(core.TypeVariable{Value: func (v any) any {
                        return v.(core.TypeLambda).Parameter
                      }(tl).(core.Name)})).(func(any) any)(recurse(func (v any) any {
                        return v.(core.TypeLambda).Body
                      }(tl).(core.Term)))
                    }(v.Value)
                    default:
                    return dflt
                  }
                  return nil
                }(term)
              }()
            }()
          }
        }
        return getAll(libsets.Empty).(func(any) any)(term0)
      }()
    }()
  }().([]any)
}

func FreeVariablesInTerm (term core.Term) []any {
  return func () any {
    var dfltVars any = liblists.Foldl(func (s []any) any {
      return func (t core.Term) any {
        return libsets.Union(s).(func(any) any)(FreeVariablesInTerm(t))
      }
    }).(func(any) any)(libsets.Empty).(func(any) any)(Subterms(term))
    return func (x any) any {
      switch v := x.(type) {
        case core.TermFunction:
        return func (v1 core.Function) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.FunctionLambda:
              return func (l core.Lambda) any {
                return libsets.Delete(func (v any) any {
                  return v.(core.Lambda).Parameter
                }(l)).(func(any) any)(FreeVariablesInTerm(func (v any) any {
                  return v.(core.Lambda).Body
                }(l).(core.Term)))
              }(v.Value)
              default:
              return dfltVars
            }
            return nil
          }(v1)
        }(v.Value)
        case core.TermLet:
        return func (l core.Let) any {
          return libsets.Difference(dfltVars).(func(any) any)(libsets.FromList(liblists.Map(func (v any) any {
            return v.(core.Binding).Name
          }).(func(any) any)(func (v any) any {
            return v.(core.Let).Bindings
          }(l))))
        }(v.Value)
        case core.TermVariable:
        return func (v core.Name) any {
          return libsets.Singleton(v)
        }(v.Value)
        default:
        return dfltVars
      }
      return nil
    }(term)
  }().([]any)
}

func FreeVariablesInType (typ core.Type) []any {
  return func () any {
    var dfltVars any = liblists.Foldl(func (s []any) any {
      return func (t core.Type) any {
        return libsets.Union(s).(func(any) any)(FreeVariablesInType(t))
      }
    }).(func(any) any)(libsets.Empty).(func(any) any)(Subtypes(typ))
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeForall:
        return func (lt core.ForallType) any {
          return libsets.Delete(func (v any) any {
            return v.(core.ForallType).Parameter
          }(lt)).(func(any) any)(FreeVariablesInType(func (v any) any {
            return v.(core.ForallType).Body
          }(lt).(core.Type)))
        }(v.Value)
        case core.TypeVariable:
        return func (v core.Name) any {
          return libsets.Singleton(v)
        }(v.Value)
        default:
        return dfltVars
      }
      return nil
    }(typ)
  }().([]any)
}

func FreeVariablesInTypeOrdered (typ core.Type) []any {
  return func () any {
    var collectVars func([]any) any
    collectVars = func (boundVars []any) any {
      return func (t core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeVariable:
            return func (v core.Name) any {
              return liblogic.IfElse(libsets.Member(v).(func(any) any)(boundVars)).(func(any) any)([]any{}).(func(any) any)([]any{v})
            }(v.Value)
            case core.TypeForall:
            return func (ft core.ForallType) any {
              return collectVars(libsets.Insert(func (v any) any {
                return v.(core.ForallType).Parameter
              }(ft)).(func(any) any)(boundVars).([]any)).(func(any) any)(func (v any) any {
                return v.(core.ForallType).Body
              }(ft))
            }(v.Value)
            default:
            return liblists.Concat(liblists.Map(func (v1 core.Type) any {
              return collectVars(boundVars).(func(any) any)(v1)
            }).(func(any) any)(Subtypes(t)))
          }
          return nil
        }(t)
      }
    }
    return liblists.Nub(collectVars(libsets.Empty).(func(any) any)(typ))
  }().([]any)
}

func FreeVariablesInTypeSchemeSimple (ts core.TypeScheme) []any {
  return func () any {
    var vars any = func (v any) any {
      return v.(core.TypeScheme).Variables
    }(ts)
    return func () any {
      var t any = func (v any) any {
        return v.(core.TypeScheme).Type_
      }(ts)
      return libsets.Difference(FreeVariablesInTypeSimple(t.(core.Type))).(func(any) any)(libsets.FromList(vars))
    }()
  }().([]any)
}

func FreeVariablesInTypeScheme (ts core.TypeScheme) []any {
  return func () any {
    var vars any = func (v any) any {
      return v.(core.TypeScheme).Variables
    }(ts)
    return func () any {
      var t any = func (v any) any {
        return v.(core.TypeScheme).Type_
      }(ts)
      return libsets.Difference(FreeVariablesInType(t.(core.Type))).(func(any) any)(libsets.FromList(vars))
    }()
  }().([]any)
}

func FreeVariablesInTypeSimple (typ core.Type) []any {
  return func () any {
    helper := func (types []any) any {
      return func (typ2 core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeVariable:
            return func (v core.Name) any {
              return libsets.Insert(v).(func(any) any)(types)
            }(v.Value)
            default:
            return types
          }
          return nil
        }(typ2)
      }
    }
    return FoldOverType(coders.TraversalOrderPre{}, func (_p any) func(core.Type) any {
      return helper(_p.([]any)).(func(core.Type) any)
    }, libsets.Empty, typ)
  }().([]any)
}

func InlineType (schema []any, typ core.Type) any {
  return func () any {
    f := func (recurse func(core.Type) any) any {
      return func (typ2 core.Type) any {
        return func () any {
          afterRecurse := func (tr core.Type) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.TypeVariable:
                return func (v core.Name) any {
                  return libmaybes.Maybe([2]any{"left", libstrings.Cat2("No such type in schema: ").(func(any) any)(func (v any) any {
                    return v
                  }(v))}).(func(any) any)(func (v1 core.Type) any {
                    return InlineType(schema, v1)
                  }).(func(any) any)(libmaps.Lookup(v).(func(any) any)(schema))
                }(v.Value)
                default:
                return [2]any{"right", tr}
              }
              return nil
            }(tr)
          }
          return libeithers.Bind(recurse(typ2)).(func(any) any)(func (tr core.Type) any {
            return afterRecurse(tr)
          })
        }()
      }
    }
    return RewriteTypeM(f, typ)
  }()
}

func IsFreeVariableInTerm (v core.Name, term core.Term) bool {
  return liblogic.Not(libsets.Member(v).(func(any) any)(FreeVariablesInTerm(term))).(bool)
}

func IsLambda (term core.Term) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermFunction:
      return func (v1 core.Function) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.FunctionLambda:
            return func (_ core.Lambda) any {
              return true
            }(v.Value)
            default:
            return false
          }
          return nil
        }(v1)
      }(v.Value)
      case core.TermLet:
      return func (lt core.Let) any {
        return IsLambda(func (v any) any {
          return v.(core.Let).Body
        }(lt).(core.Term))
      }(v.Value)
      default:
      return false
    }
    return nil
  }(DeannotateTerm(term)).(bool)
}

func LiftLambdaAboveLet (term0 core.Term) core.Term {
  return func () any {
    var rewrite func(func(core.Term) core.Term) any
    rewrite = func (recurse func(core.Term) core.Term) any {
      return func (term core.Term) any {
        return func () any {
          rewriteBinding := func (b core.Binding) any {
            return core.Binding{Name: func (v any) any {
              return v.(core.Binding).Name
            }(b).(core.Name), Term: rewrite(recurse).(func(any) any)(func (v any) any {
              return v.(core.Binding).Term
            }(b)).(core.Term), Type_: func (v any) any {
              return v.(core.Binding).Type_
            }(b)}
          }
          return func () any {
            rewriteBindings := func (bs []any) any {
              return liblists.Map(rewriteBinding).(func(any) any)(bs)
            }
            return func () any {
              var digForLambdas func(core.Term) any
              digForLambdas = func (original core.Term) any {
                return func (cons func(core.Term) core.Term) any {
                  return func (term2 core.Term) any {
                    return func (x any) any {
                      switch v := x.(type) {
                        case core.TermAnnotated:
                        return func (at core.AnnotatedTerm) any {
                          return digForLambdas(original).(func(any) any)(func (t core.Term) any {
                            return core.TermAnnotated{Value: core.AnnotatedTerm{Body: cons(t), Annotation: func (v any) any {
                              return v.(core.AnnotatedTerm).Annotation
                            }(at).([]any)}}
                          }).(func(any) any)(func (v any) any {
                            return v.(core.AnnotatedTerm).Body
                          }(at))
                        }(v.Value)
                        case core.TermFunction:
                        return func (f core.Function) any {
                          return func (x any) any {
                            switch v := x.(type) {
                              case core.FunctionLambda:
                              return func (l core.Lambda) any {
                                return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
                                  return v.(core.Lambda).Parameter
                                }(l).(core.Name), Domain: func (v any) any {
                                  return v.(core.Lambda).Domain
                                }(l), Body: digForLambdas(cons(func (v any) any {
                                  return v.(core.Lambda).Body
                                }(l).(core.Term))).(func(any) any)(func (t core.Term) any {
                                  return cons(t)
                                }).(func(any) any)(func (v any) any {
                                  return v.(core.Lambda).Body
                                }(l)).(core.Term)}}}
                              }(v.Value)
                              default:
                              return recurse(original)
                            }
                            return nil
                          }(f)
                        }(v.Value)
                        case core.TermLet:
                        return func (l core.Let) any {
                          return digForLambdas(original).(func(any) any)(func (t core.Term) any {
                            return cons(core.TermLet{Value: core.Let{Bindings: rewriteBindings(func (v any) any {
                              return v.(core.Let).Bindings
                            }(l).([]any)).([]any), Body: t}})
                          }).(func(any) any)(func (v any) any {
                            return v.(core.Let).Body
                          }(l))
                        }(v.Value)
                        default:
                        return recurse(original)
                      }
                      return nil
                    }(term2)
                  }
                }
              }
              return func (x any) any {
                switch v := x.(type) {
                  case core.TermLet:
                  return func (l core.Let) any {
                    return digForLambdas(term).(func(any) any)(func (t core.Term) any {
                      return core.TermLet{Value: core.Let{Bindings: rewriteBindings(func (v any) any {
                        return v.(core.Let).Bindings
                      }(l).([]any)).([]any), Body: t}}
                    }).(func(any) any)(func (v any) any {
                      return v.(core.Let).Body
                    }(l))
                  }(v.Value)
                  default:
                  return recurse(term)
                }
                return nil
              }(term)
            }()
          }()
        }()
      }
    }
    return RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return rewrite(_p).(func(core.Term) core.Term)
    }, term0)
  }().(core.Term)
}

func MapBeneathTypeAnnotations (f func(core.Type) core.Type, t core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return core.TypeAnnotated{Value: core.AnnotatedType{Body: MapBeneathTypeAnnotations(f, func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type)), Annotation: func (v any) any {
          return v.(core.AnnotatedType).Annotation
        }(at).([]any)}}
      }(v.Value)
      default:
      return f(t)
    }
    return nil
  }(t).(core.Type)
}

func NormalizeTypeVariablesInTerm (term core.Term) core.Term {
  return func () any {
    replaceName := func (subst []any) any {
      return func (v any) any {
        return libmaybes.FromMaybe(v).(func(any) any)(libmaps.Lookup(v).(func(any) any)(subst))
      }
    }
    return func () any {
      substType := func (subst []any) any {
        return func (typ core.Type) any {
          return func () any {
            rewrite := func (recurse func(core.Type) core.Type) any {
              return func (typ2 core.Type) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TypeVariable:
                    return func (v core.Name) any {
                      return core.TypeVariable{Value: replaceName(subst).(func(any) any)(v).(core.Name)}
                    }(v.Value)
                    default:
                    return recurse(typ2)
                  }
                  return nil
                }(typ2)
              }
            }
            return RewriteType(func (_p func(core.Type) core.Type) func(core.Type) core.Type {
              return rewrite(_p).(func(core.Type) core.Type)
            }, typ)
          }()
        }
      }
      return func () any {
        var rewriteWithSubst func(any) any
        rewriteWithSubst = func (state any) any {
          return func (term0 core.Term) any {
            return func () any {
              var sb any = libpairs.First(state)
              return func () any {
                var next any = libpairs.Second(state)
                return func () any {
                  var subst any = libpairs.First(sb)
                  return func () any {
                    var boundVars any = libpairs.Second(sb)
                    return func () any {
                      rewrite := func (recurse func(core.Term) core.Term) any {
                        return func (term2 core.Term) any {
                          return func (x any) any {
                            switch v := x.(type) {
                              case core.TermFunction:
                              return func (v1 core.Function) any {
                                return func (x any) any {
                                  switch v := x.(type) {
                                    case core.FunctionElimination:
                                    return func (_ core.Elimination) any {
                                      return recurse(term2)
                                    }(v.Value)
                                    case core.FunctionLambda:
                                    return func (l core.Lambda) any {
                                      return func () any {
                                        var domain any = func (v any) any {
                                          return v.(core.Lambda).Domain
                                        }(l)
                                        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
                                          return v.(core.Lambda).Parameter
                                        }(l).(core.Name), Domain: libmaybes.Map(func (v12 core.Type) any {
                                          return substType(subst.([]any)).(func(any) any)(v12)
                                        }).(func(any) any)(domain), Body: rewriteWithSubst([2]any{[2]any{subst, boundVars}, next}).(func(any) any)(func (v any) any {
                                          return v.(core.Lambda).Body
                                        }(l)).(core.Term)}}}
                                      }()
                                    }(v.Value)
                                    default:
                                    return recurse(term2)
                                  }
                                  return nil
                                }(v1)
                              }(v.Value)
                              case core.TermLet:
                              return func (lt core.Let) any {
                                return func () any {
                                  var bindings0 any = func (v any) any {
                                    return v.(core.Let).Bindings
                                  }(lt)
                                  return func () any {
                                    var body0 any = func (v any) any {
                                      return v.(core.Let).Body
                                    }(lt)
                                    return func () any {
                                      var step func([]any) any
                                      step = func (acc []any) any {
                                        return func (bs []any) any {
                                          return func () any {
                                            var b any = liblists.Head(bs)
                                            return func () any {
                                              var tl any = liblists.Tail(bs)
                                              return func () any {
                                                var noType any = func () any {
                                                  var newVal any = rewriteWithSubst([2]any{[2]any{subst, boundVars}, next}).(func(any) any)(b.(core.Binding).Term)
                                                  return func () any {
                                                    var b1 any = core.Binding{Name: b.(core.Binding).Name, Term: newVal.(core.Term), Type_: nil}
                                                    return step(liblists.Cons(b1).(func(any) any)(acc).([]any)).(func(any) any)(tl)
                                                  }()
                                                }()
                                                return func () any {
                                                  withType := func (ts core.TypeScheme) any {
                                                    return func () any {
                                                      var vars any = func (v any) any {
                                                        return v.(core.TypeScheme).Variables
                                                      }(ts)
                                                      return func () any {
                                                        var typ any = func (v any) any {
                                                          return v.(core.TypeScheme).Type_
                                                        }(ts)
                                                        return func () any {
                                                          var k any = liblists.Length(vars)
                                                          return func () any {
                                                            var gen func(int32) any
                                                            gen = func (i int32) any {
                                                              return func (rem int32) any {
                                                                return func (acc2 []any) any {
                                                                  return func () any {
                                                                    var ti any = core.Name(libstrings.Cat2("t").(func(any) any)(libliterals.ShowInt32(libmath.Add(next).(func(any) any)(i))).(string))
                                                                    return liblogic.IfElse(libequality.Equal(rem).(func(any) any)(0)).(func(any) any)(liblists.Reverse(acc2)).(func(any) any)(gen(libmath.Add(i).(func(any) any)(1).(int32)).(func(any) any)(libmath.Sub(rem).(func(any) any)(1)).(func(any) any)(liblists.Cons(ti).(func(any) any)(acc2)))
                                                                  }()
                                                                }
                                                              }
                                                            }
                                                            return func () any {
                                                              var newVars any = gen(0).(func(any) any)(k).(func(any) any)([]any{})
                                                              return func () any {
                                                                var newSubst any = libmaps.Union(libmaps.FromList(liblists.Zip(vars).(func(any) any)(newVars))).(func(any) any)(subst)
                                                                return func () any {
                                                                  var newBound any = libsets.Union(boundVars).(func(any) any)(libsets.FromList(newVars))
                                                                  return func () any {
                                                                    var newVal any = rewriteWithSubst([2]any{[2]any{newSubst, newBound}, libmath.Add(next).(func(any) any)(k)}).(func(any) any)(b.(core.Binding).Term)
                                                                    return func () any {
                                                                      renameConstraintKeys := func (constraintMap []any) any {
                                                                        return libmaps.FromList(liblists.Map(func (p any) any {
                                                                          return func () any {
                                                                            var oldName any = libpairs.First(p)
                                                                            return func () any {
                                                                              var meta any = libpairs.Second(p)
                                                                              return func () any {
                                                                                var newName any = libmaybes.FromMaybe(oldName).(func(any) any)(libmaps.Lookup(oldName).(func(any) any)(newSubst))
                                                                                return [2]any{newName, meta}
                                                                              }()
                                                                            }()
                                                                          }()
                                                                        }).(func(any) any)(libmaps.ToList(constraintMap)))
                                                                      }
                                                                      return func () any {
                                                                        var oldConstraints any = func (v any) any {
                                                                          return v.(core.TypeScheme).Constraints
                                                                        }(ts)
                                                                        return func () any {
                                                                          var newConstraints any = libmaybes.Map(renameConstraintKeys).(func(any) any)(oldConstraints)
                                                                          return func () any {
                                                                            var b1 any = core.Binding{Name: b.(core.Binding).Name, Term: newVal.(core.Term), Type_: func () any {
                                                                              _v := core.TypeScheme{Variables: newVars.([]any), Type_: substType(newSubst.([]any)).(func(any) any)(typ).(core.Type), Constraints: newConstraints}
                                                                              return &_v
                                                                            }()}
                                                                            return step(liblists.Cons(b1).(func(any) any)(acc).([]any)).(func(any) any)(tl)
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
                                                  return liblogic.IfElse(liblists.Null(bs)).(func(any) any)(liblists.Reverse(acc)).(func(any) any)(libmaybes.Maybe(noType).(func(any) any)(func (ts core.TypeScheme) any {
                                                    return withType(ts)
                                                  }).(func(any) any)(b.(core.Binding).Type_))
                                                }()
                                              }()
                                            }()
                                          }()
                                        }
                                      }
                                      return func () any {
                                        var bindings1 any = step([]any{}).(func(any) any)(bindings0)
                                        return core.TermLet{Value: core.Let{Bindings: bindings1.([]any), Body: rewriteWithSubst([2]any{[2]any{subst, boundVars}, next}).(func(any) any)(body0).(core.Term)}}
                                      }()
                                    }()
                                  }()
                                }()
                              }(v.Value)
                              case core.TermTypeApplication:
                              return func (tt core.TypeApplicationTerm) any {
                                return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: rewriteWithSubst([2]any{[2]any{subst, boundVars}, next}).(func(any) any)(func (v any) any {
                                  return v.(core.TypeApplicationTerm).Body
                                }(tt)).(core.Term), Type_: substType(subst.([]any)).(func(any) any)(func (v any) any {
                                  return v.(core.TypeApplicationTerm).Type_
                                }(tt)).(core.Type)}}
                              }(v.Value)
                              case core.TermTypeLambda:
                              return func (ta core.TypeLambda) any {
                                return core.TermTypeLambda{Value: core.TypeLambda{Parameter: replaceName(subst.([]any)).(func(any) any)(func (v any) any {
                                  return v.(core.TypeLambda).Parameter
                                }(ta)).(core.Name), Body: rewriteWithSubst([2]any{[2]any{subst, boundVars}, next}).(func(any) any)(func (v any) any {
                                  return v.(core.TypeLambda).Body
                                }(ta)).(core.Term)}}
                              }(v.Value)
                              default:
                              return recurse(term2)
                            }
                            return nil
                          }(term2)
                        }
                      }
                      return RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
                        return rewrite(_p).(func(core.Term) core.Term)
                      }, term0)
                    }()
                  }()
                }()
              }()
            }()
          }
        }
        return rewriteWithSubst([2]any{[2]any{libmaps.Empty, libsets.Empty}, 0}).(func(any) any)(term)
      }()
    }()
  }().(core.Term)
}

func PruneLet (l core.Let) core.Let {
  return func () any {
    var bindingMap any = libmaps.FromList(liblists.Map(func (b core.Binding) any {
      return [2]any{func (v any) any {
        return v.(core.Binding).Name
      }(b), func (v any) any {
        return v.(core.Binding).Term
      }(b)}
    }).(func(any) any)(func (v any) any {
      return v.(core.Let).Bindings
    }(l)))
    return func () any {
      var rootName any = core.Name("[[[root]]]")
      return func () any {
        adj := func (n core.Name) any {
          return libsets.Intersection(libsets.FromList(libmaps.Keys(bindingMap))).(func(any) any)(FreeVariablesInTerm(liblogic.IfElse(libequality.Equal(n).(func(any) any)(rootName)).(func(any) any)(func (v any) any {
            return v.(core.Let).Body
          }(l)).(func(any) any)(libmaybes.FromJust(libmaps.Lookup(n).(func(any) any)(bindingMap))).(core.Term)))
        }
        return func () any {
          var reachable any = sorting.FindReachableNodes[core.Name](func (_p any) []any {
            return adj(_p.(core.Name)).([]any)
          }, rootName)
          return func () any {
            var prunedBindings any = liblists.Filter(func (b core.Binding) any {
              return libsets.Member(func (v any) any {
                return v.(core.Binding).Name
              }(b)).(func(any) any)(reachable)
            }).(func(any) any)(func (v any) any {
              return v.(core.Let).Bindings
            }(l))
            return core.Let{Bindings: prunedBindings.([]any), Body: func (v any) any {
              return v.(core.Let).Body
            }(l).(core.Term)}
          }()
        }()
      }()
    }()
  }().(core.Let)
}

func RemoveTermAnnotations (term core.Term) core.Term {
  return func () any {
    remove := func (recurse func(core.Term) core.Term) any {
      return func (term2 core.Term) any {
        return func () any {
          var rewritten any = recurse(term2)
          return func (x any) any {
            switch v := x.(type) {
              case core.TermAnnotated:
              return func (at core.AnnotatedTerm) any {
                return func (v any) any {
                  return v.(core.AnnotatedTerm).Body
                }(at)
              }(v.Value)
              default:
              return rewritten
            }
            return nil
          }(term2)
        }()
      }
    }
    return RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return remove(_p).(func(core.Term) core.Term)
    }, term)
  }().(core.Term)
}

func RemoveTypeAnnotations (typ core.Type) core.Type {
  return func () any {
    remove := func (recurse func(core.Type) core.Type) any {
      return func (typ2 core.Type) any {
        return func () any {
          var rewritten any = recurse(typ2)
          return func (x any) any {
            switch v := x.(type) {
              case core.TypeAnnotated:
              return func (at core.AnnotatedType) any {
                return func (v any) any {
                  return v.(core.AnnotatedType).Body
                }(at)
              }(v.Value)
              default:
              return rewritten
            }
            return nil
          }(rewritten)
        }()
      }
    }
    return RewriteType(func (_p func(core.Type) core.Type) func(core.Type) core.Type {
      return remove(_p).(func(core.Type) core.Type)
    }, typ)
  }().(core.Type)
}

func RemoveTypeAnnotationsFromTerm (term core.Term) core.Term {
  return func () any {
    strip := func (recurse func(core.Term) core.Term) any {
      return func (term2 core.Term) any {
        return func () any {
          var rewritten any = recurse(term2)
          return func () any {
            stripBinding := func (b core.Binding) any {
              return core.Binding{Name: func (v any) any {
                return v.(core.Binding).Name
              }(b).(core.Name), Term: func (v any) any {
                return v.(core.Binding).Term
              }(b).(core.Term), Type_: nil}
            }
            return func (x any) any {
              switch v := x.(type) {
                case core.TermLet:
                return func (lt core.Let) any {
                  return core.TermLet{Value: core.Let{Bindings: liblists.Map(stripBinding).(func(any) any)(func (v any) any {
                    return v.(core.Let).Bindings
                  }(lt)).([]any), Body: func (v any) any {
                    return v.(core.Let).Body
                  }(lt).(core.Term)}}
                }(v.Value)
                case core.TermTypeApplication:
                return func (tt core.TypeApplicationTerm) any {
                  return func (v any) any {
                    return v.(core.TypeApplicationTerm).Body
                  }(tt)
                }(v.Value)
                case core.TermTypeLambda:
                return func (ta core.TypeLambda) any {
                  return func (v any) any {
                    return v.(core.TypeLambda).Body
                  }(ta)
                }(v.Value)
                default:
                return rewritten
              }
              return nil
            }(rewritten)
          }()
        }()
      }
    }
    return RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return strip(_p).(func(core.Term) core.Term)
    }, term)
  }().(core.Term)
}

func RemoveTypesFromTerm (term core.Term) core.Term {
  return func () any {
    strip := func (recurse func(core.Term) core.Term) any {
      return func (term2 core.Term) any {
        return func () any {
          var rewritten any = recurse(term2)
          return func () any {
            stripBinding := func (b core.Binding) any {
              return core.Binding{Name: func (v any) any {
                return v.(core.Binding).Name
              }(b).(core.Name), Term: func (v any) any {
                return v.(core.Binding).Term
              }(b).(core.Term), Type_: nil}
            }
            return func (x any) any {
              switch v := x.(type) {
                case core.TermFunction:
                return func (f core.Function) any {
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.FunctionElimination:
                      return func (e core.Elimination) any {
                        return core.TermFunction{Value: core.FunctionElimination{Value: e}}
                      }(v.Value)
                      case core.FunctionLambda:
                      return func (l core.Lambda) any {
                        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
                          return v.(core.Lambda).Parameter
                        }(l).(core.Name), Domain: nil, Body: func (v any) any {
                          return v.(core.Lambda).Body
                        }(l).(core.Term)}}}
                      }(v.Value)
                      default:
                      return core.TermFunction{Value: f}
                    }
                    return nil
                  }(f)
                }(v.Value)
                case core.TermLet:
                return func (lt core.Let) any {
                  return core.TermLet{Value: core.Let{Bindings: liblists.Map(stripBinding).(func(any) any)(func (v any) any {
                    return v.(core.Let).Bindings
                  }(lt)).([]any), Body: func (v any) any {
                    return v.(core.Let).Body
                  }(lt).(core.Term)}}
                }(v.Value)
                case core.TermTypeApplication:
                return func (tt core.TypeApplicationTerm) any {
                  return func (v any) any {
                    return v.(core.TypeApplicationTerm).Body
                  }(tt)
                }(v.Value)
                case core.TermTypeLambda:
                return func (ta core.TypeLambda) any {
                  return func (v any) any {
                    return v.(core.TypeLambda).Body
                  }(ta)
                }(v.Value)
                default:
                return rewritten
              }
              return nil
            }(rewritten)
          }()
        }()
      }
    }
    return RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return strip(_p).(func(core.Term) core.Term)
    }, term)
  }().(core.Term)
}

func ReplaceFreeTermVariable (vold core.Name, tnew core.Term, term core.Term) core.Term {
  return func () any {
    rewrite := func (recurse func(core.Term) core.Term) any {
      return func (t core.Term) any {
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
                      return liblogic.IfElse(libequality.Equal(v).(func(any) any)(vold)).(func(any) any)(t).(func(any) any)(recurse(t))
                    }()
                  }(v.Value)
                  default:
                  return recurse(t)
                }
                return nil
              }(f)
            }(v.Value)
            case core.TermVariable:
            return func (v core.Name) any {
              return liblogic.IfElse(libequality.Equal(v).(func(any) any)(vold)).(func(any) any)(tnew).(func(any) any)(core.TermVariable{Value: v})
            }(v.Value)
            default:
            return recurse(t)
          }
          return nil
        }(t)
      }
    }
    return RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return rewrite(_p).(func(core.Term) core.Term)
    }, term)
  }().(core.Term)
}

func ReplaceFreeTypeVariable (v core.Name, rep core.Type, typ core.Type) core.Type {
  return func () any {
    mapExpr := func (recurse func(core.Type) core.Type) any {
      return func (t core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeForall:
            return func (ft core.ForallType) any {
              return liblogic.IfElse(libequality.Equal(v).(func(any) any)(func (v any) any {
                return v.(core.ForallType).Parameter
              }(ft))).(func(any) any)(t).(func(any) any)(core.TypeForall{Value: core.ForallType{Parameter: func (v any) any {
                return v.(core.ForallType).Parameter
              }(ft).(core.Name), Body: recurse(func (v any) any {
                return v.(core.ForallType).Body
              }(ft).(core.Type))}})
            }(v.Value)
            case core.TypeVariable:
            return func (v_ core.Name) any {
              return liblogic.IfElse(libequality.Equal(v).(func(any) any)(v_)).(func(any) any)(rep).(func(any) any)(t)
            }(v.Value)
            default:
            return recurse(t)
          }
          return nil
        }(t)
      }
    }
    return RewriteType(func (_p func(core.Type) core.Type) func(core.Type) core.Type {
      return mapExpr(_p).(func(core.Type) core.Type)
    }, typ)
  }().(core.Type)
}

func ReplaceTypedefs (types []any, typ0 core.Type) core.Type {
  return func () any {
    var rewrite func(func(core.Type) core.Type) any
    rewrite = func (recurse func(core.Type) core.Type) any {
      return func (typ core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeAnnotated:
            return func (at core.AnnotatedType) any {
              return core.TypeAnnotated{Value: core.AnnotatedType{Body: rewrite(recurse).(func(any) any)(func (v any) any {
                return v.(core.AnnotatedType).Body
              }(at)).(core.Type), Annotation: func (v any) any {
                return v.(core.AnnotatedType).Annotation
              }(at).([]any)}}
            }(v.Value)
            case core.TypeRecord:
            return func (_ []any) any {
              return typ
            }(v.Value)
            case core.TypeUnion:
            return func (_ []any) any {
              return typ
            }(v.Value)
            case core.TypeVariable:
            return func (v core.Name) any {
              return func () any {
                forMono := func (t core.Type) any {
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.TypeRecord:
                      return func (_ []any) any {
                        return typ
                      }(v.Value)
                      case core.TypeUnion:
                      return func (_ []any) any {
                        return typ
                      }(v.Value)
                      case core.TypeWrap:
                      return func (_ core.Type) any {
                        return typ
                      }(v.Value)
                      default:
                      return rewrite(recurse).(func(any) any)(t)
                    }
                    return nil
                  }(t)
                }
                return func () any {
                  forTypeScheme := func (ts core.TypeScheme) any {
                    return func () any {
                      var t any = func (v any) any {
                        return v.(core.TypeScheme).Type_
                      }(ts)
                      return liblogic.IfElse(liblists.Null(func (v any) any {
                        return v.(core.TypeScheme).Variables
                      }(ts))).(func(any) any)(forMono(t.(core.Type))).(func(any) any)(typ)
                    }()
                  }
                  return libmaybes.Maybe(typ).(func(any) any)(func (ts core.TypeScheme) any {
                    return forTypeScheme(ts)
                  }).(func(any) any)(libmaps.Lookup(v).(func(any) any)(types))
                }()
              }()
            }(v.Value)
            case core.TypeWrap:
            return func (_ core.Type) any {
              return typ
            }(v.Value)
            default:
            return recurse(typ)
          }
          return nil
        }(typ)
      }
    }
    return RewriteType(func (_p func(core.Type) core.Type) func(core.Type) core.Type {
      return rewrite(_p).(func(core.Type) core.Type)
    }, typ0)
  }().(core.Type)
}

func RewriteAndFoldTerm[T0 any] (f func(func(T0) func(core.Term) any) func(T0) func(core.Term) any, term0 T0, v1 core.Term) any {
  return func () any {
    fsub := func (recurse func(T0) func(core.Term) any) any {
      return func (val0 T0) any {
        return func (term02 core.Term) any {
          return func () any {
            forSingle := func (rec func(T0) func(core.Term) any) any {
              return func (cons func(core.Term) core.Term) any {
                return func (val T0) any {
                  return func (term core.Term) any {
                    return func () any {
                      var r any = rec(val)(term)
                      return [2]any{libpairs.First(r), cons(libpairs.Second(r).(core.Term))}
                    }()
                  }
                }
              }
            }
            return func () any {
              forMany := func (rec func(T0) func(core.Field) any) any {
                return func (cons func([]any) []any) any {
                  return func (val T0) any {
                    return func (els []any) any {
                      return func () any {
                        var rr any = liblists.Foldl(func (r any) any {
                          return func (el core.Field) any {
                            return func () any {
                              var r2 any = rec(libpairs.First(r).(T0))(el)
                              return [2]any{libpairs.First(r2), liblists.Cons(libpairs.Second(r2)).(func(any) any)(libpairs.Second(r))}
                            }()
                          }
                        }).(func(any) any)([2]any{val, []any{}}).(func(any) any)(els)
                        return [2]any{libpairs.First(rr), cons(liblists.Reverse(libpairs.Second(rr)).([]any))}
                      }()
                    }
                  }
                }
              }
              return func () any {
                forField := func (val T0) any {
                  return func (field core.Field) any {
                    return func () any {
                      var r any = recurse(val)(func (v any) any {
                        return v.(core.Field).Term
                      }(field))
                      return [2]any{libpairs.First(r), core.Field{Name: func (v any) any {
                        return v.(core.Field).Name
                      }(field).(core.Name), Term: libpairs.Second(r).(core.Term)}}
                    }()
                  }
                }
                return func () any {
                  forFields := func (v1 T0) any {
                    return func (v2 []any) any {
                      return forMany(forField).(func(any) any)(func (x []any) any {
                        return x
                      }).(func(any) any)(v1).(func(any) any)(v2)
                    }
                  }
                  return func () any {
                    forPair := func (val T0) any {
                      return func (kv any) any {
                        return func () any {
                          var rk any = recurse(val)(libpairs.First(kv))
                          return func () any {
                            var rv any = recurse(libpairs.First(rk).(T0))(libpairs.Second(kv))
                            return [2]any{libpairs.First(rv), [2]any{libpairs.Second(rk), libpairs.Second(rv)}}
                          }()
                        }()
                      }
                    }
                    return func () any {
                      forBinding := func (val T0) any {
                        return func (binding core.Binding) any {
                          return func () any {
                            var r any = recurse(val)(func (v any) any {
                              return v.(core.Binding).Term
                            }(binding))
                            return [2]any{libpairs.First(r), core.Binding{Name: func (v any) any {
                              return v.(core.Binding).Name
                            }(binding).(core.Name), Term: libpairs.Second(r).(core.Term), Type_: func (v any) any {
                              return v.(core.Binding).Type_
                            }(binding)}}
                          }()
                        }
                      }
                      return func () any {
                        forElimination := func (val T0) any {
                          return func (elm core.Elimination) any {
                            return func () any {
                              var r any = func (x any) any {
                                switch v := x.(type) {
                                  case core.EliminationUnion:
                                  return func (cs core.CaseStatement) any {
                                    return func () any {
                                      var rmd any = libmaybes.Map(func (v1 core.Term) any {
                                        return recurse(val)(v1)
                                      }).(func(any) any)(func (v any) any {
                                        return v.(core.CaseStatement).Default_
                                      }(cs))
                                      return func () any {
                                        var val1 any = libmaybes.Maybe(val).(func(any) any)(libpairs.First).(func(any) any)(rmd)
                                        return func () any {
                                          var rcases any = forFields(val1.(T0)).(func(any) any)(func (v any) any {
                                            return v.(core.CaseStatement).Cases
                                          }(cs))
                                          return [2]any{libpairs.First(rcases), core.EliminationUnion{Value: core.CaseStatement{TypeName: func (v any) any {
                                            return v.(core.CaseStatement).TypeName
                                          }(cs).(core.Name), Default_: libmaybes.Map(libpairs.Second).(func(any) any)(rmd), Cases: libpairs.Second(rcases).([]any)}}}
                                        }()
                                      }()
                                    }()
                                  }(v.Value)
                                  default:
                                  return [2]any{val, elm}
                                }
                                return nil
                              }(elm)
                              return [2]any{libpairs.First(r), libpairs.Second(r)}
                            }()
                          }
                        }
                        return func () any {
                          forFunction := func (val T0) any {
                            return func (fun core.Function) any {
                              return func (x any) any {
                                switch v := x.(type) {
                                  case core.FunctionElimination:
                                  return func (elm core.Elimination) any {
                                    return func () any {
                                      var re any = forElimination(val).(func(any) any)(elm)
                                      return [2]any{libpairs.First(re), core.FunctionElimination{Value: libpairs.Second(re).(core.Elimination)}}
                                    }()
                                  }(v.Value)
                                  case core.FunctionLambda:
                                  return func (l core.Lambda) any {
                                    return func () any {
                                      var rl any = recurse(val)(func (v any) any {
                                        return v.(core.Lambda).Body
                                      }(l))
                                      return [2]any{libpairs.First(rl), core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
                                        return v.(core.Lambda).Parameter
                                      }(l).(core.Name), Domain: func (v any) any {
                                        return v.(core.Lambda).Domain
                                      }(l), Body: libpairs.Second(rl).(core.Term)}}}
                                    }()
                                  }(v.Value)
                                  default:
                                  return [2]any{val, fun}
                                }
                                return nil
                              }(fun)
                            }
                          }
                          return func () any {
                            var dflt any = [2]any{val0, term02}
                            return func (x any) any {
                              switch v := x.(type) {
                                case core.TermAnnotated:
                                return func (at core.AnnotatedTerm) any {
                                  return forSingle(recurse).(func(any) any)(func (t core.Term) any {
                                    return core.TermAnnotated{Value: core.AnnotatedTerm{Body: t, Annotation: func (v any) any {
                                      return v.(core.AnnotatedTerm).Annotation
                                    }(at).([]any)}}
                                  }).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                    return v.(core.AnnotatedTerm).Body
                                  }(at))
                                }(v.Value)
                                case core.TermApplication:
                                return func (a core.Application) any {
                                  return func () any {
                                    var rlhs any = recurse(val0)(func (v any) any {
                                      return v.(core.Application).Function
                                    }(a))
                                    return func () any {
                                      var rrhs any = recurse(libpairs.First(rlhs).(T0))(func (v any) any {
                                        return v.(core.Application).Argument
                                      }(a))
                                      return [2]any{libpairs.First(rrhs), core.TermApplication{Value: core.Application{Function: libpairs.Second(rlhs).(core.Term), Argument: libpairs.Second(rrhs).(core.Term)}}}
                                    }()
                                  }()
                                }(v.Value)
                                case core.TermEither:
                                return func (e any) any {
                                  return libeithers.Either(func (l core.Term) any {
                                    return func () any {
                                      var rl any = recurse(val0)(l)
                                      return [2]any{libpairs.First(rl), core.TermEither{Value: [2]any{"left", libpairs.Second(rl)}}}
                                    }()
                                  }).(func(any) any)(func (r core.Term) any {
                                    return func () any {
                                      var rr any = recurse(val0)(r)
                                      return [2]any{libpairs.First(rr), core.TermEither{Value: [2]any{"right", libpairs.Second(rr)}}}
                                    }()
                                  }).(func(any) any)(e)
                                }(v.Value)
                                case core.TermFunction:
                                return func (f2 core.Function) any {
                                  return forSingle(forFunction).(func(any) any)(func (f3 core.Function) any {
                                    return core.TermFunction{Value: f3}
                                  }).(func(any) any)(val0).(func(any) any)(f2)
                                }(v.Value)
                                case core.TermLet:
                                return func (l core.Let) any {
                                  return func () any {
                                    var renv any = recurse(val0)(func (v any) any {
                                      return v.(core.Let).Body
                                    }(l))
                                    return forMany(forBinding).(func(any) any)(func (bins []any) any {
                                      return core.TermLet{Value: core.Let{Bindings: bins, Body: libpairs.Second(renv).(core.Term)}}
                                    }).(func(any) any)(libpairs.First(renv)).(func(any) any)(func (v any) any {
                                      return v.(core.Let).Bindings
                                    }(l))
                                  }()
                                }(v.Value)
                                case core.TermList:
                                return func (els []any) any {
                                  return forMany(recurse).(func(any) any)(func (x []any) any {
                                    return core.TermList{Value: x}
                                  }).(func(any) any)(val0).(func(any) any)(els)
                                }(v.Value)
                                case core.TermMap_:
                                return func (m []any) any {
                                  return forMany(forPair).(func(any) any)(func (pairs []any) any {
                                    return core.TermMap_{Value: libmaps.FromList(pairs).([]any)}
                                  }).(func(any) any)(val0).(func(any) any)(libmaps.ToList(m))
                                }(v.Value)
                                case core.TermMaybe:
                                return func (mt any) any {
                                  return libmaybes.Maybe(dflt).(func(any) any)(func (t core.Term) any {
                                    return forSingle(recurse).(func(any) any)(func (t1 core.Term) any {
                                      return core.TermMaybe{Value: func () any {
                                        _v := t1
                                        return &_v
                                      }()}
                                    }).(func(any) any)(val0).(func(any) any)(t)
                                  }).(func(any) any)(mt)
                                }(v.Value)
                                case core.TermPair:
                                return func (p any) any {
                                  return func () any {
                                    var rf any = recurse(val0)(libpairs.First(p))
                                    return func () any {
                                      var rs any = recurse(libpairs.First(rf).(T0))(libpairs.Second(p))
                                      return [2]any{libpairs.First(rs), core.TermPair{Value: [2]any{libpairs.Second(rf), libpairs.Second(rs)}}}
                                    }()
                                  }()
                                }(v.Value)
                                case core.TermRecord:
                                return func (r core.Record) any {
                                  return forMany(forField).(func(any) any)(func (fields []any) any {
                                    return core.TermRecord{Value: core.Record{TypeName: func (v any) any {
                                      return v.(core.Record).TypeName
                                    }(r).(core.Name), Fields: fields}}
                                  }).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                    return v.(core.Record).Fields
                                  }(r))
                                }(v.Value)
                                case core.TermSet:
                                return func (els []any) any {
                                  return forMany(recurse).(func(any) any)(func (e []any) any {
                                    return core.TermSet{Value: libsets.FromList(e).([]any)}
                                  }).(func(any) any)(val0).(func(any) any)(libsets.ToList(els))
                                }(v.Value)
                                case core.TermTypeApplication:
                                return func (ta core.TypeApplicationTerm) any {
                                  return forSingle(recurse).(func(any) any)(func (t core.Term) any {
                                    return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: t, Type_: func (v any) any {
                                      return v.(core.TypeApplicationTerm).Type_
                                    }(ta).(core.Type)}}
                                  }).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                    return v.(core.TypeApplicationTerm).Body
                                  }(ta))
                                }(v.Value)
                                case core.TermTypeLambda:
                                return func (tl core.TypeLambda) any {
                                  return forSingle(recurse).(func(any) any)(func (t core.Term) any {
                                    return core.TermTypeLambda{Value: core.TypeLambda{Parameter: func (v any) any {
                                      return v.(core.TypeLambda).Parameter
                                    }(tl).(core.Name), Body: t}}
                                  }).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                    return v.(core.TypeLambda).Body
                                  }(tl))
                                }(v.Value)
                                case core.TermUnion:
                                return func (inj core.Injection) any {
                                  return forSingle(recurse).(func(any) any)(func (t core.Term) any {
                                    return core.TermUnion{Value: core.Injection{TypeName: func (v any) any {
                                      return v.(core.Injection).TypeName
                                    }(inj).(core.Name), Field: core.Field{Name: func (v any) any {
                                      return v.(core.Injection).Field
                                    }(inj).(core.Field).Name, Term: t}}}
                                  }).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                    return v.(core.Injection).Field
                                  }(inj).(core.Field).Term)
                                }(v.Value)
                                case core.TermWrap:
                                return func (wt core.WrappedTerm) any {
                                  return forSingle(recurse).(func(any) any)(func (t core.Term) any {
                                    return core.TermWrap{Value: core.WrappedTerm{TypeName: func (v any) any {
                                      return v.(core.WrappedTerm).TypeName
                                    }(wt).(core.Name), Body: t}}
                                  }).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                    return v.(core.WrappedTerm).Body
                                  }(wt))
                                }(v.Value)
                                default:
                                return dflt
                              }
                              return nil
                            }(term02)
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
    return func () any {
      var recurse func(T0) any
      recurse = func (v1 T0) any {
        return func (v2 core.Term) any {
          return f(func (_p T0) func(core.Term) any {
            return func (v12 T0) any {
              return func (v22 core.Term) any {
                return fsub(recurse).(func(any) any)(v12).(func(any) any)(v22)
              }
            }(_p).(func(core.Term) any)
          })(v1)(v2)
        }
      }
      return recurse(term0).(func(any) any)(v1)
    }()
  }()
}

func RewriteAndFoldTermWithPath[T0 any] (f func(func([]any) func(T0) func(core.Term) any) func([]any) func(T0) func(core.Term) any, term0 T0, v1 core.Term) any {
  return func () any {
    fsub := func (recurse func([]any) func(T0) func(core.Term) any) any {
      return func (path []any) any {
        return func (val0 T0) any {
          return func (term02 core.Term) any {
            return func () any {
              forSingleWithAccessor := func (rec func([]any) func(T0) func(core.Term) any) any {
                return func (cons func(core.Term) core.Term) any {
                  return func (accessor accessors.TermAccessor) any {
                    return func (val T0) any {
                      return func (term core.Term) any {
                        return func () any {
                          var r any = rec(liblists.Concat2(path).(func(any) any)([]any{accessor}).([]any))(val)(term)
                          return [2]any{libpairs.First(r), cons(libpairs.Second(r).(core.Term))}
                        }()
                      }
                    }
                  }
                }
              }
              return func () any {
                forManyWithAccessors := func (rec func([]any) func(T0) func(core.Field) any) any {
                  return func (cons func([]any) []any) any {
                    return func (val T0) any {
                      return func (accessorTermPairs []any) any {
                        return func () any {
                          var rr any = liblists.Foldl(func (r any) any {
                            return func (atp any) any {
                              return func () any {
                                var r2 any = rec(liblists.Concat2(path).(func(any) any)([]any{libpairs.First(atp)}).([]any))(libpairs.First(r))(libpairs.Second(atp))
                                return [2]any{libpairs.First(r2), liblists.Cons(libpairs.Second(r2)).(func(any) any)(libpairs.Second(r))}
                              }()
                            }
                          }).(func(any) any)([2]any{val, []any{}}).(func(any) any)(accessorTermPairs)
                          return [2]any{libpairs.First(rr), cons(liblists.Reverse(libpairs.Second(rr)).([]any))}
                        }()
                      }
                    }
                  }
                }
                return func () any {
                  forFieldWithAccessor := func (mkAccessor func(core.Name) accessors.TermAccessor) any {
                    return func (val T0) any {
                      return func (field core.Field) any {
                        return func () any {
                          var r any = recurse(liblists.Concat2(path).(func(any) any)([]any{mkAccessor(func (v any) any {
                            return v.(core.Field).Name
                          }(field).(core.Name))}).([]any))(val)(func (v any) any {
                            return v.(core.Field).Term
                          }(field))
                          return [2]any{libpairs.First(r), core.Field{Name: func (v any) any {
                            return v.(core.Field).Name
                          }(field).(core.Name), Term: libpairs.Second(r).(core.Term)}}
                        }()
                      }
                    }
                  }
                  return func () any {
                    forFieldsWithAccessor := func (mkAccessor func(core.Name) accessors.TermAccessor) any {
                      return func (v1 T0) any {
                        return func (v2 []any) any {
                          return forManyWithAccessors(func (_p []any) func(T0) func(core.Field) any {
                            return func (path1 []any) any {
                              return func (val1 T0) any {
                                return func (field1 core.Field) any {
                                  return forFieldWithAccessor(mkAccessor).(func(any) any)(val1).(func(any) any)(field1)
                                }
                              }
                            }(_p).(func(T0) func(core.Field) any)
                          }).(func(any) any)(func (x []any) any {
                            return x
                          }).(func(any) any)(v1).(func(any) any)(v2)
                        }
                      }
                    }
                    return func () any {
                      forPairWithAccessors := func (keyAccessor accessors.TermAccessor) any {
                        return func (valAccessor accessors.TermAccessor) any {
                          return func (val T0) any {
                            return func (kv any) any {
                              return func () any {
                                var rk any = recurse(liblists.Concat2(path).(func(any) any)([]any{keyAccessor}).([]any))(val)(libpairs.First(kv))
                                return func () any {
                                  var rv any = recurse(liblists.Concat2(path).(func(any) any)([]any{valAccessor}).([]any))(libpairs.First(rk))(libpairs.Second(kv))
                                  return [2]any{libpairs.First(rv), [2]any{libpairs.Second(rk), libpairs.Second(rv)}}
                                }()
                              }()
                            }
                          }
                        }
                      }
                      return func () any {
                        forBindingWithAccessor := func (val T0) any {
                          return func (binding core.Binding) any {
                            return func () any {
                              var r any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorLetBinding{Value: func (v any) any {
                                return v.(core.Binding).Name
                              }(binding).(core.Name)}}).([]any))(val)(func (v any) any {
                                return v.(core.Binding).Term
                              }(binding))
                              return [2]any{libpairs.First(r), core.Binding{Name: func (v any) any {
                                return v.(core.Binding).Name
                              }(binding).(core.Name), Term: libpairs.Second(r).(core.Term), Type_: func (v any) any {
                                return v.(core.Binding).Type_
                              }(binding)}}
                            }()
                          }
                        }
                        return func () any {
                          forElimination := func (val T0) any {
                            return func (elm core.Elimination) any {
                              return func () any {
                                var r any = func (x any) any {
                                  switch v := x.(type) {
                                    case core.EliminationUnion:
                                    return func (cs core.CaseStatement) any {
                                      return func () any {
                                        var rmd any = libmaybes.Map(func (def core.Term) any {
                                          return recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorUnionCasesDefault{}}).([]any))(val)(def)
                                        }).(func(any) any)(func (v any) any {
                                          return v.(core.CaseStatement).Default_
                                        }(cs))
                                        return func () any {
                                          var val1 any = libmaybes.Maybe(val).(func(any) any)(libpairs.First).(func(any) any)(rmd)
                                          return func () any {
                                            var rcases any = forManyWithAccessors(recurse).(func(any) any)(func (x []any) any {
                                              return x
                                            }).(func(any) any)(val1).(func(any) any)(liblists.Map(func (f2 core.Field) any {
                                              return [2]any{accessors.TermAccessorUnionCasesBranch{Value: func (v any) any {
                                                return v.(core.Field).Name
                                              }(f2).(core.Name)}, func (v any) any {
                                                return v.(core.Field).Term
                                              }(f2)}
                                            }).(func(any) any)(func (v any) any {
                                              return v.(core.CaseStatement).Cases
                                            }(cs)))
                                            return [2]any{libpairs.First(rcases), core.EliminationUnion{Value: core.CaseStatement{TypeName: func (v any) any {
                                              return v.(core.CaseStatement).TypeName
                                            }(cs).(core.Name), Default_: libmaybes.Map(libpairs.Second).(func(any) any)(rmd), Cases: liblists.Map(func (ft any) any {
                                              return core.Field{Name: libpairs.First(ft).(core.Name), Term: libpairs.Second(ft).(core.Term)}
                                            }).(func(any) any)(liblists.Zip(liblists.Map(func (v any) any {
                                              return v.(core.Field).Name
                                            }).(func(any) any)(func (v any) any {
                                              return v.(core.CaseStatement).Cases
                                            }(cs))).(func(any) any)(libpairs.Second(rcases))).([]any)}}}
                                          }()
                                        }()
                                      }()
                                    }(v.Value)
                                    default:
                                    return [2]any{val, elm}
                                  }
                                  return nil
                                }(elm)
                                return [2]any{libpairs.First(r), libpairs.Second(r)}
                              }()
                            }
                          }
                          return func () any {
                            forFunction := func (val T0) any {
                              return func (fun core.Function) any {
                                return func (x any) any {
                                  switch v := x.(type) {
                                    case core.FunctionElimination:
                                    return func (elm core.Elimination) any {
                                      return func () any {
                                        var re any = forElimination(val).(func(any) any)(elm)
                                        return [2]any{libpairs.First(re), core.FunctionElimination{Value: libpairs.Second(re).(core.Elimination)}}
                                      }()
                                    }(v.Value)
                                    case core.FunctionLambda:
                                    return func (l core.Lambda) any {
                                      return func () any {
                                        var rl any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorLambdaBody{}}).([]any))(val)(func (v any) any {
                                          return v.(core.Lambda).Body
                                        }(l))
                                        return [2]any{libpairs.First(rl), core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
                                          return v.(core.Lambda).Parameter
                                        }(l).(core.Name), Domain: func (v any) any {
                                          return v.(core.Lambda).Domain
                                        }(l), Body: libpairs.Second(rl).(core.Term)}}}
                                      }()
                                    }(v.Value)
                                    default:
                                    return [2]any{val, fun}
                                  }
                                  return nil
                                }(fun)
                              }
                            }
                            return func () any {
                              var dflt any = [2]any{val0, term02}
                              return func (x any) any {
                                switch v := x.(type) {
                                  case core.TermAnnotated:
                                  return func (at core.AnnotatedTerm) any {
                                    return forSingleWithAccessor(recurse).(func(any) any)(func (t core.Term) any {
                                      return core.TermAnnotated{Value: core.AnnotatedTerm{Body: t, Annotation: func (v any) any {
                                        return v.(core.AnnotatedTerm).Annotation
                                      }(at).([]any)}}
                                    }).(func(any) any)(accessors.TermAccessorAnnotatedBody{}).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                      return v.(core.AnnotatedTerm).Body
                                    }(at))
                                  }(v.Value)
                                  case core.TermApplication:
                                  return func (a core.Application) any {
                                    return func () any {
                                      var rlhs any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorApplicationFunction{}}).([]any))(val0)(func (v any) any {
                                        return v.(core.Application).Function
                                      }(a))
                                      return func () any {
                                        var rrhs any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorApplicationArgument{}}).([]any))(libpairs.First(rlhs))(func (v any) any {
                                          return v.(core.Application).Argument
                                        }(a))
                                        return [2]any{libpairs.First(rrhs), core.TermApplication{Value: core.Application{Function: libpairs.Second(rlhs).(core.Term), Argument: libpairs.Second(rrhs).(core.Term)}}}
                                      }()
                                    }()
                                  }(v.Value)
                                  case core.TermEither:
                                  return func (e any) any {
                                    return libeithers.Either(func (l core.Term) any {
                                      return func () any {
                                        var rl any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorSumTerm{}}).([]any))(val0)(l)
                                        return [2]any{libpairs.First(rl), core.TermEither{Value: [2]any{"left", libpairs.Second(rl)}}}
                                      }()
                                    }).(func(any) any)(func (r core.Term) any {
                                      return func () any {
                                        var rr any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorSumTerm{}}).([]any))(val0)(r)
                                        return [2]any{libpairs.First(rr), core.TermEither{Value: [2]any{"right", libpairs.Second(rr)}}}
                                      }()
                                    }).(func(any) any)(e)
                                  }(v.Value)
                                  case core.TermFunction:
                                  return func (f2 core.Function) any {
                                    return func () any {
                                      var rf any = forFunction(val0).(func(any) any)(f2)
                                      return [2]any{libpairs.First(rf), core.TermFunction{Value: libpairs.Second(rf).(core.Function)}}
                                    }()
                                  }(v.Value)
                                  case core.TermLet:
                                  return func (l core.Let) any {
                                    return func () any {
                                      var renv any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorLetBody{}}).([]any))(val0)(func (v any) any {
                                        return v.(core.Let).Body
                                      }(l))
                                      return func () any {
                                        var rbindings any = liblists.Foldl(func (r any) any {
                                          return func (binding core.Binding) any {
                                            return func () any {
                                              var rb any = forBindingWithAccessor(libpairs.First(r).(T0)).(func(any) any)(binding)
                                              return [2]any{libpairs.First(rb), liblists.Cons(libpairs.Second(rb)).(func(any) any)(libpairs.Second(r))}
                                            }()
                                          }
                                        }).(func(any) any)([2]any{libpairs.First(renv), []any{}}).(func(any) any)(func (v any) any {
                                          return v.(core.Let).Bindings
                                        }(l))
                                        return [2]any{libpairs.First(rbindings), core.TermLet{Value: core.Let{Bindings: liblists.Reverse(libpairs.Second(rbindings)).([]any), Body: libpairs.Second(renv).(core.Term)}}}
                                      }()
                                    }()
                                  }(v.Value)
                                  case core.TermList:
                                  return func (els []any) any {
                                    return func () any {
                                      var idx any = 0
                                      return func () any {
                                        var rr any = liblists.Foldl(func (r any) any {
                                          return func (el core.Term) any {
                                            return func () any {
                                              var r2 any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorListElement{Value: libpairs.First(r).(int32)}}).([]any))(libpairs.First(libpairs.Second(r)))(el)
                                              return [2]any{libmath.Add(libpairs.First(r)).(func(any) any)(1), [2]any{libpairs.First(r2), liblists.Cons(libpairs.Second(r2)).(func(any) any)(libpairs.Second(libpairs.Second(r)))}}
                                            }()
                                          }
                                        }).(func(any) any)([2]any{idx, [2]any{val0, []any{}}}).(func(any) any)(els)
                                        return [2]any{libpairs.First(libpairs.Second(rr)), core.TermList{Value: liblists.Reverse(libpairs.Second(libpairs.Second(rr))).([]any)}}
                                      }()
                                    }()
                                  }(v.Value)
                                  case core.TermMap_:
                                  return func (m []any) any {
                                    return func () any {
                                      var idx any = 0
                                      return func () any {
                                        var rr any = liblists.Foldl(func (r any) any {
                                          return func (kv any) any {
                                            return func () any {
                                              var rk any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorMapKey{Value: libpairs.First(r).(int32)}}).([]any))(libpairs.First(libpairs.Second(r)))(libpairs.First(kv))
                                              return func () any {
                                                var rv any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorMapValue{Value: libpairs.First(r).(int32)}}).([]any))(libpairs.First(rk))(libpairs.Second(kv))
                                                return [2]any{libmath.Add(libpairs.First(r)).(func(any) any)(1), [2]any{libpairs.First(rv), liblists.Cons([2]any{libpairs.Second(rk), libpairs.Second(rv)}).(func(any) any)(libpairs.Second(libpairs.Second(r)))}}
                                              }()
                                            }()
                                          }
                                        }).(func(any) any)([2]any{idx, [2]any{val0, []any{}}}).(func(any) any)(libmaps.ToList(m))
                                        return [2]any{libpairs.First(libpairs.Second(rr)), core.TermMap_{Value: libmaps.FromList(liblists.Reverse(libpairs.Second(libpairs.Second(rr)))).([]any)}}
                                      }()
                                    }()
                                  }(v.Value)
                                  case core.TermMaybe:
                                  return func (mt any) any {
                                    return libmaybes.Maybe(dflt).(func(any) any)(func (t core.Term) any {
                                      return forSingleWithAccessor(recurse).(func(any) any)(func (t1 core.Term) any {
                                        return core.TermMaybe{Value: func () any {
                                          _v := t1
                                          return &_v
                                        }()}
                                      }).(func(any) any)(accessors.TermAccessorMaybeTerm{}).(func(any) any)(val0).(func(any) any)(t)
                                    }).(func(any) any)(mt)
                                  }(v.Value)
                                  case core.TermPair:
                                  return func (p any) any {
                                    return func () any {
                                      var rf any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorProductTerm{Value: 0}}).([]any))(val0)(libpairs.First(p))
                                      return func () any {
                                        var rs any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorProductTerm{Value: 1}}).([]any))(libpairs.First(rf))(libpairs.Second(p))
                                        return [2]any{libpairs.First(rs), core.TermPair{Value: [2]any{libpairs.Second(rf), libpairs.Second(rs)}}}
                                      }()
                                    }()
                                  }(v.Value)
                                  case core.TermRecord:
                                  return func (r core.Record) any {
                                    return func () any {
                                      var rfields any = forManyWithAccessors(recurse).(func(any) any)(func (x []any) any {
                                        return x
                                      }).(func(any) any)(val0).(func(any) any)(liblists.Map(func (f2 core.Field) any {
                                        return [2]any{accessors.TermAccessorRecordField{Value: func (v any) any {
                                          return v.(core.Field).Name
                                        }(f2).(core.Name)}, func (v any) any {
                                          return v.(core.Field).Term
                                        }(f2)}
                                      }).(func(any) any)(func (v any) any {
                                        return v.(core.Record).Fields
                                      }(r)))
                                      return [2]any{libpairs.First(rfields), core.TermRecord{Value: core.Record{TypeName: func (v any) any {
                                        return v.(core.Record).TypeName
                                      }(r).(core.Name), Fields: liblists.Map(func (ft any) any {
                                        return core.Field{Name: libpairs.First(ft).(core.Name), Term: libpairs.Second(ft).(core.Term)}
                                      }).(func(any) any)(liblists.Zip(liblists.Map(func (v any) any {
                                        return v.(core.Field).Name
                                      }).(func(any) any)(func (v any) any {
                                        return v.(core.Record).Fields
                                      }(r))).(func(any) any)(libpairs.Second(rfields))).([]any)}}}
                                    }()
                                  }(v.Value)
                                  case core.TermSet:
                                  return func (els []any) any {
                                    return func () any {
                                      var idx any = 0
                                      return func () any {
                                        var rr any = liblists.Foldl(func (r any) any {
                                          return func (el core.Term) any {
                                            return func () any {
                                              var r2 any = recurse(liblists.Concat2(path).(func(any) any)([]any{accessors.TermAccessorSetElement{Value: libpairs.First(r).(int32)}}).([]any))(libpairs.First(libpairs.Second(r)))(el)
                                              return [2]any{libmath.Add(libpairs.First(r)).(func(any) any)(1), [2]any{libpairs.First(r2), liblists.Cons(libpairs.Second(r2)).(func(any) any)(libpairs.Second(libpairs.Second(r)))}}
                                            }()
                                          }
                                        }).(func(any) any)([2]any{idx, [2]any{val0, []any{}}}).(func(any) any)(libsets.ToList(els))
                                        return [2]any{libpairs.First(libpairs.Second(rr)), core.TermSet{Value: libsets.FromList(liblists.Reverse(libpairs.Second(libpairs.Second(rr)))).([]any)}}
                                      }()
                                    }()
                                  }(v.Value)
                                  case core.TermTypeApplication:
                                  return func (ta core.TypeApplicationTerm) any {
                                    return forSingleWithAccessor(recurse).(func(any) any)(func (t core.Term) any {
                                      return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: t, Type_: func (v any) any {
                                        return v.(core.TypeApplicationTerm).Type_
                                      }(ta).(core.Type)}}
                                    }).(func(any) any)(accessors.TermAccessorTypeApplicationTerm{}).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                      return v.(core.TypeApplicationTerm).Body
                                    }(ta))
                                  }(v.Value)
                                  case core.TermTypeLambda:
                                  return func (tl core.TypeLambda) any {
                                    return forSingleWithAccessor(recurse).(func(any) any)(func (t core.Term) any {
                                      return core.TermTypeLambda{Value: core.TypeLambda{Parameter: func (v any) any {
                                        return v.(core.TypeLambda).Parameter
                                      }(tl).(core.Name), Body: t}}
                                    }).(func(any) any)(accessors.TermAccessorTypeLambdaBody{}).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                      return v.(core.TypeLambda).Body
                                    }(tl))
                                  }(v.Value)
                                  case core.TermUnion:
                                  return func (inj core.Injection) any {
                                    return forSingleWithAccessor(recurse).(func(any) any)(func (t core.Term) any {
                                      return core.TermUnion{Value: core.Injection{TypeName: func (v any) any {
                                        return v.(core.Injection).TypeName
                                      }(inj).(core.Name), Field: core.Field{Name: func (v any) any {
                                        return v.(core.Injection).Field
                                      }(inj).(core.Field).Name, Term: t}}}
                                    }).(func(any) any)(accessors.TermAccessorInjectionTerm{}).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                      return v.(core.Injection).Field
                                    }(inj).(core.Field).Term)
                                  }(v.Value)
                                  case core.TermWrap:
                                  return func (wt core.WrappedTerm) any {
                                    return forSingleWithAccessor(recurse).(func(any) any)(func (t core.Term) any {
                                      return core.TermWrap{Value: core.WrappedTerm{TypeName: func (v any) any {
                                        return v.(core.WrappedTerm).TypeName
                                      }(wt).(core.Name), Body: t}}
                                    }).(func(any) any)(accessors.TermAccessorWrappedTerm{}).(func(any) any)(val0).(func(any) any)(func (v any) any {
                                      return v.(core.WrappedTerm).Body
                                    }(wt))
                                  }(v.Value)
                                  default:
                                  return dflt
                                }
                                return nil
                              }(term02)
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
      var recurse func([]any) any
      recurse = func (v1 []any) any {
        return func (v2 T0) any {
          return func (v3 core.Term) any {
            return f(func (_p []any) func(T0) func(core.Term) any {
              return func (v12 []any) any {
                return func (v22 T0) any {
                  return func (v32 core.Term) any {
                    return fsub(recurse).(func(any) any)(v12).(func(any) any)(v22).(func(any) any)(v32)
                  }
                }
              }(_p).(func(T0) func(core.Term) any)
            })(v1)(v2)(v3)
          }
        }
      }
      return recurse([]any{}).(func(any) any)(term0).(func(any) any)(v1)
    }()
  }()
}

func RewriteTerm (f func(func(core.Term) core.Term) func(core.Term) core.Term, term0 core.Term) core.Term {
  return func () any {
    fsub := func (recurse func(core.Term) core.Term) any {
      return func (term core.Term) any {
        return func () any {
          forField := func (f2 core.Field) any {
            return core.Field{Name: func (v any) any {
              return v.(core.Field).Name
            }(f2).(core.Name), Term: recurse(func (v any) any {
              return v.(core.Field).Term
            }(f2).(core.Term))}
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
                    }(cs).(core.Name), Default_: libmaybes.Map(recurse).(func(any) any)(func (v any) any {
                      return v.(core.CaseStatement).Default_
                    }(cs)), Cases: liblists.Map(forField).(func(any) any)(func (v any) any {
                      return v.(core.CaseStatement).Cases
                    }(cs)).([]any)}}
                  }(v.Value)
                  case core.EliminationWrap:
                  return func (name core.Name) any {
                    return core.EliminationWrap{Value: name}
                  }(v.Value)
                }
                return nil
              }(elm)
            }
            return func () any {
              forFunction := func (fun core.Function) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.FunctionElimination:
                    return func (elm core.Elimination) any {
                      return core.FunctionElimination{Value: forElimination(elm).(core.Elimination)}
                    }(v.Value)
                    case core.FunctionLambda:
                    return func (l core.Lambda) any {
                      return core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
                        return v.(core.Lambda).Parameter
                      }(l).(core.Name), Domain: func (v any) any {
                        return v.(core.Lambda).Domain
                      }(l), Body: recurse(func (v any) any {
                        return v.(core.Lambda).Body
                      }(l).(core.Term))}}
                    }(v.Value)
                    case core.FunctionPrimitive:
                    return func (name core.Name) any {
                      return core.FunctionPrimitive{Value: name}
                    }(v.Value)
                  }
                  return nil
                }(fun)
              }
              return func () any {
                forLet := func (lt core.Let) any {
                  return func () any {
                    mapBinding := func (b core.Binding) any {
                      return core.Binding{Name: func (v any) any {
                        return v.(core.Binding).Name
                      }(b).(core.Name), Term: recurse(func (v any) any {
                        return v.(core.Binding).Term
                      }(b).(core.Term)), Type_: func (v any) any {
                        return v.(core.Binding).Type_
                      }(b)}
                    }
                    return core.Let{Bindings: liblists.Map(mapBinding).(func(any) any)(func (v any) any {
                      return v.(core.Let).Bindings
                    }(lt)).([]any), Body: recurse(func (v any) any {
                      return v.(core.Let).Body
                    }(lt).(core.Term))}
                  }()
                }
                return func () any {
                  forMap := func (m []any) any {
                    return func () any {
                      forPair := func (p any) any {
                        return [2]any{recurse(libpairs.First(p).(core.Term)), recurse(libpairs.Second(p).(core.Term))}
                      }
                      return libmaps.FromList(liblists.Map(forPair).(func(any) any)(libmaps.ToList(m)))
                    }()
                  }
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.TermAnnotated:
                      return func (at core.AnnotatedTerm) any {
                        return core.TermAnnotated{Value: core.AnnotatedTerm{Body: recurse(func (v any) any {
                          return v.(core.AnnotatedTerm).Body
                        }(at).(core.Term)), Annotation: func (v any) any {
                          return v.(core.AnnotatedTerm).Annotation
                        }(at).([]any)}}
                      }(v.Value)
                      case core.TermApplication:
                      return func (a core.Application) any {
                        return core.TermApplication{Value: core.Application{Function: recurse(func (v any) any {
                          return v.(core.Application).Function
                        }(a).(core.Term)), Argument: recurse(func (v any) any {
                          return v.(core.Application).Argument
                        }(a).(core.Term))}}
                      }(v.Value)
                      case core.TermEither:
                      return func (e any) any {
                        return core.TermEither{Value: libeithers.Either(func (l core.Term) any {
                          return [2]any{"left", recurse(l)}
                        }).(func(any) any)(func (r core.Term) any {
                          return [2]any{"right", recurse(r)}
                        }).(func(any) any)(e)}
                      }(v.Value)
                      case core.TermFunction:
                      return func (fun core.Function) any {
                        return core.TermFunction{Value: forFunction(fun).(core.Function)}
                      }(v.Value)
                      case core.TermLet:
                      return func (lt core.Let) any {
                        return core.TermLet{Value: forLet(lt).(core.Let)}
                      }(v.Value)
                      case core.TermList:
                      return func (els []any) any {
                        return core.TermList{Value: liblists.Map(recurse).(func(any) any)(els).([]any)}
                      }(v.Value)
                      case core.TermLiteral:
                      return func (v core.Literal) any {
                        return core.TermLiteral{Value: v}
                      }(v.Value)
                      case core.TermMap_:
                      return func (m []any) any {
                        return core.TermMap_{Value: forMap(m).([]any)}
                      }(v.Value)
                      case core.TermMaybe:
                      return func (m any) any {
                        return core.TermMaybe{Value: libmaybes.Map(recurse).(func(any) any)(m)}
                      }(v.Value)
                      case core.TermPair:
                      return func (p any) any {
                        return core.TermPair{Value: [2]any{recurse(libpairs.First(p).(core.Term)), recurse(libpairs.Second(p).(core.Term))}}
                      }(v.Value)
                      case core.TermRecord:
                      return func (r core.Record) any {
                        return core.TermRecord{Value: core.Record{TypeName: func (v any) any {
                          return v.(core.Record).TypeName
                        }(r).(core.Name), Fields: liblists.Map(forField).(func(any) any)(func (v any) any {
                          return v.(core.Record).Fields
                        }(r)).([]any)}}
                      }(v.Value)
                      case core.TermSet:
                      return func (s []any) any {
                        return core.TermSet{Value: libsets.FromList(liblists.Map(recurse).(func(any) any)(libsets.ToList(s))).([]any)}
                      }(v.Value)
                      case core.TermTypeApplication:
                      return func (tt core.TypeApplicationTerm) any {
                        return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: recurse(func (v any) any {
                          return v.(core.TypeApplicationTerm).Body
                        }(tt).(core.Term)), Type_: func (v any) any {
                          return v.(core.TypeApplicationTerm).Type_
                        }(tt).(core.Type)}}
                      }(v.Value)
                      case core.TermTypeLambda:
                      return func (ta core.TypeLambda) any {
                        return core.TermTypeLambda{Value: core.TypeLambda{Parameter: func (v any) any {
                          return v.(core.TypeLambda).Parameter
                        }(ta).(core.Name), Body: recurse(func (v any) any {
                          return v.(core.TypeLambda).Body
                        }(ta).(core.Term))}}
                      }(v.Value)
                      case core.TermUnion:
                      return func (i core.Injection) any {
                        return core.TermUnion{Value: core.Injection{TypeName: func (v any) any {
                          return v.(core.Injection).TypeName
                        }(i).(core.Name), Field: forField(func (v any) any {
                          return v.(core.Injection).Field
                        }(i).(core.Field)).(core.Field)}}
                      }(v.Value)
                      case core.TermUnit:
                      return func (_ struct{}) any {
                        return core.TermUnit{}
                      }(v)
                      case core.TermVariable:
                      return func (v core.Name) any {
                        return core.TermVariable{Value: v}
                      }(v.Value)
                      case core.TermWrap:
                      return func (wt core.WrappedTerm) any {
                        return core.TermWrap{Value: core.WrappedTerm{TypeName: func (v any) any {
                          return v.(core.WrappedTerm).TypeName
                        }(wt).(core.Name), Body: recurse(func (v any) any {
                          return v.(core.WrappedTerm).Body
                        }(wt).(core.Term))}}
                      }(v.Value)
                    }
                    return nil
                  }(term)
                }()
              }()
            }()
          }()
        }()
      }
    }
    return func () any {
      var recurse func(core.Term) any
      recurse = func (v1 core.Term) any {
        return f(func (_p core.Term) core.Term {
          return func (v12 core.Term) any {
            return fsub(recurse).(func(any) any)(v12)
          }(_p).(core.Term)
        })(v1)
      }
      return recurse(term0)
    }()
  }().(core.Term)
}

func RewriteTermM (f func(func(core.Term) any) func(core.Term) any, term0 core.Term) any {
  return func () any {
    fsub := func (recurse func(core.Term) any) any {
      return func (term core.Term) any {
        return func () any {
          forField := func (field core.Field) any {
            return libeithers.Bind(recurse(func (v any) any {
              return v.(core.Field).Term
            }(field).(core.Term))).(func(any) any)(func (t core.Term) any {
              return [2]any{"right", core.Field{Name: func (v any) any {
                return v.(core.Field).Name
              }(field).(core.Name), Term: t}}
            })
          }
          return func () any {
            forPair := func (kv any) any {
              return libeithers.Bind(recurse(libpairs.First(kv).(core.Term))).(func(any) any)(func (k core.Term) any {
                return libeithers.Bind(recurse(libpairs.Second(kv).(core.Term))).(func(any) any)(func (v core.Term) any {
                  return [2]any{"right", [2]any{k, v}}
                })
              })
            }
            return func () any {
              mapBinding := func (b core.Binding) any {
                return libeithers.Bind(recurse(func (v any) any {
                  return v.(core.Binding).Term
                }(b).(core.Term))).(func(any) any)(func (v core.Term) any {
                  return [2]any{"right", core.Binding{Name: func (v any) any {
                    return v.(core.Binding).Name
                  }(b).(core.Name), Term: v, Type_: func (v any) any {
                    return v.(core.Binding).Type_
                  }(b)}}
                })
              }
              return func (x any) any {
                switch v := x.(type) {
                  case core.TermAnnotated:
                  return func (at core.AnnotatedTerm) any {
                    return libeithers.Bind(recurse(func (v any) any {
                      return v.(core.AnnotatedTerm).Body
                    }(at).(core.Term))).(func(any) any)(func (ex core.Term) any {
                      return [2]any{"right", core.TermAnnotated{Value: core.AnnotatedTerm{Body: ex, Annotation: func (v any) any {
                        return v.(core.AnnotatedTerm).Annotation
                      }(at).([]any)}}}
                    })
                  }(v.Value)
                  case core.TermApplication:
                  return func (app core.Application) any {
                    return libeithers.Bind(recurse(func (v any) any {
                      return v.(core.Application).Function
                    }(app).(core.Term))).(func(any) any)(func (lhs core.Term) any {
                      return libeithers.Bind(recurse(func (v any) any {
                        return v.(core.Application).Argument
                      }(app).(core.Term))).(func(any) any)(func (rhs core.Term) any {
                        return [2]any{"right", core.TermApplication{Value: core.Application{Function: lhs, Argument: rhs}}}
                      })
                    })
                  }(v.Value)
                  case core.TermEither:
                  return func (e any) any {
                    return libeithers.Bind(libeithers.Either(func (l core.Term) any {
                      return libeithers.Map(func (x core.Term) any {
                        return [2]any{"left", x}
                      }).(func(any) any)(recurse(l))
                    }).(func(any) any)(func (r core.Term) any {
                      return libeithers.Map(func (x core.Term) any {
                        return [2]any{"right", x}
                      }).(func(any) any)(recurse(r))
                    }).(func(any) any)(e)).(func(any) any)(func (re any) any {
                      return [2]any{"right", core.TermEither{Value: re}}
                    })
                  }(v.Value)
                  case core.TermFunction:
                  return func (fun core.Function) any {
                    return func () any {
                      forElm := func (e core.Elimination) any {
                        return func (x any) any {
                          switch v := x.(type) {
                            case core.EliminationRecord:
                            return func (p core.Projection) any {
                              return [2]any{"right", core.FunctionElimination{Value: core.EliminationRecord{Value: p}}}
                            }(v.Value)
                            case core.EliminationUnion:
                            return func (cs core.CaseStatement) any {
                              return func () any {
                                var n any = func (v any) any {
                                  return v.(core.CaseStatement).TypeName
                                }(cs)
                                return func () any {
                                  var def any = func (v any) any {
                                    return v.(core.CaseStatement).Default_
                                  }(cs)
                                  return func () any {
                                    var cases any = func (v any) any {
                                      return v.(core.CaseStatement).Cases
                                    }(cs)
                                    return libeithers.Bind(libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (t core.Term) any {
                                      return libeithers.Map(libmaybes.Pure).(func(any) any)(recurse(t))
                                    }).(func(any) any)(def)).(func(any) any)(func (rdef any) any {
                                      return libeithers.Map(func (rcases []any) any {
                                        return core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: n.(core.Name), Default_: rdef, Cases: rcases}}}
                                      }).(func(any) any)(libeithers.MapList(forField).(func(any) any)(cases))
                                    })
                                  }()
                                }()
                              }()
                            }(v.Value)
                            case core.EliminationWrap:
                            return func (name core.Name) any {
                              return [2]any{"right", core.FunctionElimination{Value: core.EliminationWrap{Value: name}}}
                            }(v.Value)
                          }
                          return nil
                        }(e)
                      }
                      return func () any {
                        forFun := func (fun2 core.Function) any {
                          return func (x any) any {
                            switch v := x.(type) {
                              case core.FunctionElimination:
                              return func (e core.Elimination) any {
                                return forElm(e)
                              }(v.Value)
                              case core.FunctionLambda:
                              return func (l core.Lambda) any {
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
                                      return libeithers.Bind(recurse(body.(core.Term))).(func(any) any)(func (rbody core.Term) any {
                                        return [2]any{"right", core.FunctionLambda{Value: core.Lambda{Parameter: v.(core.Name), Domain: d, Body: rbody}}}
                                      })
                                    }()
                                  }()
                                }()
                              }(v.Value)
                              case core.FunctionPrimitive:
                              return func (name core.Name) any {
                                return [2]any{"right", core.FunctionPrimitive{Value: name}}
                              }(v.Value)
                            }
                            return nil
                          }(fun2)
                        }
                        return libeithers.Bind(forFun(fun)).(func(any) any)(func (rfun core.Function) any {
                          return [2]any{"right", core.TermFunction{Value: rfun}}
                        })
                      }()
                    }()
                  }(v.Value)
                  case core.TermLet:
                  return func (lt core.Let) any {
                    return func () any {
                      var bindings any = func (v any) any {
                        return v.(core.Let).Bindings
                      }(lt)
                      return func () any {
                        var env any = func (v any) any {
                          return v.(core.Let).Body
                        }(lt)
                        return libeithers.Bind(libeithers.MapList(mapBinding).(func(any) any)(bindings)).(func(any) any)(func (rbindings []any) any {
                          return libeithers.Bind(recurse(env.(core.Term))).(func(any) any)(func (renv core.Term) any {
                            return [2]any{"right", core.TermLet{Value: core.Let{Bindings: rbindings, Body: renv}}}
                          })
                        })
                      }()
                    }()
                  }(v.Value)
                  case core.TermList:
                  return func (els []any) any {
                    return libeithers.Bind(libeithers.MapList(recurse).(func(any) any)(els)).(func(any) any)(func (rels []any) any {
                      return [2]any{"right", core.TermList{Value: rels}}
                    })
                  }(v.Value)
                  case core.TermLiteral:
                  return func (v core.Literal) any {
                    return [2]any{"right", core.TermLiteral{Value: v}}
                  }(v.Value)
                  case core.TermMap_:
                  return func (m []any) any {
                    return libeithers.Bind(libeithers.MapList(forPair).(func(any) any)(libmaps.ToList(m))).(func(any) any)(func (pairs []any) any {
                      return [2]any{"right", core.TermMap_{Value: libmaps.FromList(pairs).([]any)}}
                    })
                  }(v.Value)
                  case core.TermMaybe:
                  return func (m any) any {
                    return libeithers.Bind(libeithers.MapMaybe(recurse).(func(any) any)(m)).(func(any) any)(func (rm any) any {
                      return [2]any{"right", core.TermMaybe{Value: rm}}
                    })
                  }(v.Value)
                  case core.TermPair:
                  return func (p any) any {
                    return libeithers.Bind(recurse(libpairs.First(p).(core.Term))).(func(any) any)(func (rf core.Term) any {
                      return libeithers.Bind(recurse(libpairs.Second(p).(core.Term))).(func(any) any)(func (rs core.Term) any {
                        return [2]any{"right", core.TermPair{Value: [2]any{rf, rs}}}
                      })
                    })
                  }(v.Value)
                  case core.TermRecord:
                  return func (r core.Record) any {
                    return func () any {
                      var n any = func (v any) any {
                        return v.(core.Record).TypeName
                      }(r)
                      return func () any {
                        var fields any = func (v any) any {
                          return v.(core.Record).Fields
                        }(r)
                        return libeithers.Map(func (rfields []any) any {
                          return core.TermRecord{Value: core.Record{TypeName: n.(core.Name), Fields: rfields}}
                        }).(func(any) any)(libeithers.MapList(forField).(func(any) any)(fields))
                      }()
                    }()
                  }(v.Value)
                  case core.TermSet:
                  return func (s []any) any {
                    return libeithers.Bind(libeithers.MapList(recurse).(func(any) any)(libsets.ToList(s))).(func(any) any)(func (rlist []any) any {
                      return [2]any{"right", core.TermSet{Value: libsets.FromList(rlist).([]any)}}
                    })
                  }(v.Value)
                  case core.TermTypeApplication:
                  return func (tt core.TypeApplicationTerm) any {
                    return libeithers.Bind(recurse(func (v any) any {
                      return v.(core.TypeApplicationTerm).Body
                    }(tt).(core.Term))).(func(any) any)(func (t core.Term) any {
                      return [2]any{"right", core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: t, Type_: func (v any) any {
                        return v.(core.TypeApplicationTerm).Type_
                      }(tt).(core.Type)}}}
                    })
                  }(v.Value)
                  case core.TermTypeLambda:
                  return func (tl core.TypeLambda) any {
                    return func () any {
                      var v any = func (v any) any {
                        return v.(core.TypeLambda).Parameter
                      }(tl)
                      return func () any {
                        var body any = func (v any) any {
                          return v.(core.TypeLambda).Body
                        }(tl)
                        return libeithers.Bind(recurse(body.(core.Term))).(func(any) any)(func (rbody core.Term) any {
                          return [2]any{"right", core.TermTypeLambda{Value: core.TypeLambda{Parameter: v.(core.Name), Body: rbody}}}
                        })
                      }()
                    }()
                  }(v.Value)
                  case core.TermUnion:
                  return func (i core.Injection) any {
                    return func () any {
                      var n any = func (v any) any {
                        return v.(core.Injection).TypeName
                      }(i)
                      return func () any {
                        var field any = func (v any) any {
                          return v.(core.Injection).Field
                        }(i)
                        return libeithers.Map(func (rfield core.Field) any {
                          return core.TermUnion{Value: core.Injection{TypeName: n.(core.Name), Field: rfield}}
                        }).(func(any) any)(forField(field.(core.Field)))
                      }()
                    }()
                  }(v.Value)
                  case core.TermUnit:
                  return func (_ struct{}) any {
                    return [2]any{"right", core.TermUnit{}}
                  }(v)
                  case core.TermVariable:
                  return func (v core.Name) any {
                    return [2]any{"right", core.TermVariable{Value: v}}
                  }(v.Value)
                  case core.TermWrap:
                  return func (wt core.WrappedTerm) any {
                    return func () any {
                      var name any = func (v any) any {
                        return v.(core.WrappedTerm).TypeName
                      }(wt)
                      return func () any {
                        var t any = func (v any) any {
                          return v.(core.WrappedTerm).Body
                        }(wt)
                        return libeithers.Bind(recurse(t.(core.Term))).(func(any) any)(func (rt core.Term) any {
                          return [2]any{"right", core.TermWrap{Value: core.WrappedTerm{TypeName: name.(core.Name), Body: rt}}}
                        })
                      }()
                    }()
                  }(v.Value)
                }
                return nil
              }(term)
            }()
          }()
        }()
      }
    }
    return func () any {
      var recurse func(core.Term) any
      recurse = func (v1 core.Term) any {
        return f(func (_p core.Term) any {
          return func (v12 core.Term) any {
            return fsub(recurse).(func(any) any)(v12)
          }(_p)
        })(v1)
      }
      return recurse(term0)
    }()
  }()
}

func RewriteTermWithContext[T0 any] (f func(func(T0) func(core.Term) core.Term) func(T0) func(core.Term) core.Term, cx0 T0, term0 core.Term) core.Term {
  return func () any {
    forSubterms := func (recurse0 func(T0) func(core.Term) core.Term) any {
      return func (cx T0) any {
        return func (term core.Term) any {
          return func () any {
            recurse := func (v1 core.Term) any {
              return recurse0(cx)(v1)
            }
            return func () any {
              forField := func (field core.Field) any {
                return core.Field{Name: func (v any) any {
                  return v.(core.Field).Name
                }(field).(core.Name), Term: recurse(func (v any) any {
                  return v.(core.Field).Term
                }(field).(core.Term)).(core.Term)}
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
                        }(cs).(core.Name), Default_: libmaybes.Map(recurse).(func(any) any)(func (v any) any {
                          return v.(core.CaseStatement).Default_
                        }(cs)), Cases: liblists.Map(forField).(func(any) any)(func (v any) any {
                          return v.(core.CaseStatement).Cases
                        }(cs)).([]any)}}
                      }(v.Value)
                      case core.EliminationWrap:
                      return func (name core.Name) any {
                        return core.EliminationWrap{Value: name}
                      }(v.Value)
                    }
                    return nil
                  }(elm)
                }
                return func () any {
                  forFunction := func (fun core.Function) any {
                    return func (x any) any {
                      switch v := x.(type) {
                        case core.FunctionElimination:
                        return func (elm core.Elimination) any {
                          return core.FunctionElimination{Value: forElimination(elm).(core.Elimination)}
                        }(v.Value)
                        case core.FunctionLambda:
                        return func (l core.Lambda) any {
                          return core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
                            return v.(core.Lambda).Parameter
                          }(l).(core.Name), Domain: func (v any) any {
                            return v.(core.Lambda).Domain
                          }(l), Body: recurse(func (v any) any {
                            return v.(core.Lambda).Body
                          }(l).(core.Term)).(core.Term)}}
                        }(v.Value)
                        case core.FunctionPrimitive:
                        return func (name core.Name) any {
                          return core.FunctionPrimitive{Value: name}
                        }(v.Value)
                      }
                      return nil
                    }(fun)
                  }
                  return func () any {
                    forLet := func (lt core.Let) any {
                      return func () any {
                        mapBinding := func (b core.Binding) any {
                          return core.Binding{Name: func (v any) any {
                            return v.(core.Binding).Name
                          }(b).(core.Name), Term: recurse(func (v any) any {
                            return v.(core.Binding).Term
                          }(b).(core.Term)).(core.Term), Type_: func (v any) any {
                            return v.(core.Binding).Type_
                          }(b)}
                        }
                        return core.Let{Bindings: liblists.Map(mapBinding).(func(any) any)(func (v any) any {
                          return v.(core.Let).Bindings
                        }(lt)).([]any), Body: recurse(func (v any) any {
                          return v.(core.Let).Body
                        }(lt).(core.Term)).(core.Term)}
                      }()
                    }
                    return func () any {
                      forMap := func (m []any) any {
                        return func () any {
                          forPair := func (p any) any {
                            return [2]any{recurse(libpairs.First(p).(core.Term)), recurse(libpairs.Second(p).(core.Term))}
                          }
                          return libmaps.FromList(liblists.Map(forPair).(func(any) any)(libmaps.ToList(m)))
                        }()
                      }
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TermAnnotated:
                          return func (at core.AnnotatedTerm) any {
                            return core.TermAnnotated{Value: core.AnnotatedTerm{Body: recurse(func (v any) any {
                              return v.(core.AnnotatedTerm).Body
                            }(at).(core.Term)).(core.Term), Annotation: func (v any) any {
                              return v.(core.AnnotatedTerm).Annotation
                            }(at).([]any)}}
                          }(v.Value)
                          case core.TermApplication:
                          return func (a core.Application) any {
                            return core.TermApplication{Value: core.Application{Function: recurse(func (v any) any {
                              return v.(core.Application).Function
                            }(a).(core.Term)).(core.Term), Argument: recurse(func (v any) any {
                              return v.(core.Application).Argument
                            }(a).(core.Term)).(core.Term)}}
                          }(v.Value)
                          case core.TermEither:
                          return func (e any) any {
                            return core.TermEither{Value: libeithers.Either(func (l core.Term) any {
                              return [2]any{"left", recurse(l)}
                            }).(func(any) any)(func (r core.Term) any {
                              return [2]any{"right", recurse(r)}
                            }).(func(any) any)(e)}
                          }(v.Value)
                          case core.TermFunction:
                          return func (fun core.Function) any {
                            return core.TermFunction{Value: forFunction(fun).(core.Function)}
                          }(v.Value)
                          case core.TermLet:
                          return func (lt core.Let) any {
                            return core.TermLet{Value: forLet(lt).(core.Let)}
                          }(v.Value)
                          case core.TermList:
                          return func (els []any) any {
                            return core.TermList{Value: liblists.Map(recurse).(func(any) any)(els).([]any)}
                          }(v.Value)
                          case core.TermLiteral:
                          return func (v core.Literal) any {
                            return core.TermLiteral{Value: v}
                          }(v.Value)
                          case core.TermMap_:
                          return func (m []any) any {
                            return core.TermMap_{Value: forMap(m).([]any)}
                          }(v.Value)
                          case core.TermMaybe:
                          return func (m any) any {
                            return core.TermMaybe{Value: libmaybes.Map(recurse).(func(any) any)(m)}
                          }(v.Value)
                          case core.TermPair:
                          return func (p any) any {
                            return core.TermPair{Value: [2]any{recurse(libpairs.First(p).(core.Term)), recurse(libpairs.Second(p).(core.Term))}}
                          }(v.Value)
                          case core.TermRecord:
                          return func (r core.Record) any {
                            return core.TermRecord{Value: core.Record{TypeName: func (v any) any {
                              return v.(core.Record).TypeName
                            }(r).(core.Name), Fields: liblists.Map(forField).(func(any) any)(func (v any) any {
                              return v.(core.Record).Fields
                            }(r)).([]any)}}
                          }(v.Value)
                          case core.TermSet:
                          return func (s []any) any {
                            return core.TermSet{Value: libsets.FromList(liblists.Map(recurse).(func(any) any)(libsets.ToList(s))).([]any)}
                          }(v.Value)
                          case core.TermTypeApplication:
                          return func (tt core.TypeApplicationTerm) any {
                            return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: recurse(func (v any) any {
                              return v.(core.TypeApplicationTerm).Body
                            }(tt).(core.Term)).(core.Term), Type_: func (v any) any {
                              return v.(core.TypeApplicationTerm).Type_
                            }(tt).(core.Type)}}
                          }(v.Value)
                          case core.TermTypeLambda:
                          return func (ta core.TypeLambda) any {
                            return core.TermTypeLambda{Value: core.TypeLambda{Parameter: func (v any) any {
                              return v.(core.TypeLambda).Parameter
                            }(ta).(core.Name), Body: recurse(func (v any) any {
                              return v.(core.TypeLambda).Body
                            }(ta).(core.Term)).(core.Term)}}
                          }(v.Value)
                          case core.TermUnion:
                          return func (i core.Injection) any {
                            return core.TermUnion{Value: core.Injection{TypeName: func (v any) any {
                              return v.(core.Injection).TypeName
                            }(i).(core.Name), Field: forField(func (v any) any {
                              return v.(core.Injection).Field
                            }(i).(core.Field)).(core.Field)}}
                          }(v.Value)
                          case core.TermUnit:
                          return func (_ struct{}) any {
                            return core.TermUnit{}
                          }(v)
                          case core.TermVariable:
                          return func (v core.Name) any {
                            return core.TermVariable{Value: v}
                          }(v.Value)
                          case core.TermWrap:
                          return func (wt core.WrappedTerm) any {
                            return core.TermWrap{Value: core.WrappedTerm{TypeName: func (v any) any {
                              return v.(core.WrappedTerm).TypeName
                            }(wt).(core.Name), Body: recurse(func (v any) any {
                              return v.(core.WrappedTerm).Body
                            }(wt).(core.Term)).(core.Term)}}
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
        }
      }
    }
    return func () any {
      var rewrite func(T0) any
      rewrite = func (cx T0) any {
        return func (term core.Term) any {
          return f(func (_p T0) func(core.Term) core.Term {
            return func (v1 T0) any {
              return func (v2 core.Term) any {
                return forSubterms(rewrite).(func(any) any)(v1).(func(any) any)(v2)
              }
            }(_p).(func(core.Term) core.Term)
          })(cx)(term)
        }
      }
      return rewrite(cx0).(func(any) any)(term0)
    }()
  }().(core.Term)
}

func RewriteTermWithContextM[T0 any] (f func(func(T0) func(core.Term) any) func(T0) func(core.Term) any, cx0 T0, term0 core.Term) any {
  return func () any {
    forSubterms := func (recurse0 func(T0) func(core.Term) any) any {
      return func (cx T0) any {
        return func (term core.Term) any {
          return func () any {
            recurse := func (v1 core.Term) any {
              return recurse0(cx)(v1)
            }
            return func () any {
              forField := func (field core.Field) any {
                return libeithers.Bind(recurse(func (v any) any {
                  return v.(core.Field).Term
                }(field).(core.Term))).(func(any) any)(func (t core.Term) any {
                  return [2]any{"right", core.Field{Name: func (v any) any {
                    return v.(core.Field).Name
                  }(field).(core.Name), Term: t}}
                })
              }
              return func () any {
                forPair := func (kv any) any {
                  return libeithers.Bind(recurse(libpairs.First(kv).(core.Term))).(func(any) any)(func (k core.Term) any {
                    return libeithers.Bind(recurse(libpairs.Second(kv).(core.Term))).(func(any) any)(func (v core.Term) any {
                      return [2]any{"right", [2]any{k, v}}
                    })
                  })
                }
                return func () any {
                  forElimination := func (e core.Elimination) any {
                    return func (x any) any {
                      switch v := x.(type) {
                        case core.EliminationRecord:
                        return func (p core.Projection) any {
                          return [2]any{"right", core.FunctionElimination{Value: core.EliminationRecord{Value: p}}}
                        }(v.Value)
                        case core.EliminationUnion:
                        return func (cs core.CaseStatement) any {
                          return func () any {
                            var n any = func (v any) any {
                              return v.(core.CaseStatement).TypeName
                            }(cs)
                            return func () any {
                              var def any = func (v any) any {
                                return v.(core.CaseStatement).Default_
                              }(cs)
                              return func () any {
                                var cases any = func (v any) any {
                                  return v.(core.CaseStatement).Cases
                                }(cs)
                                return libeithers.Bind(libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (t core.Term) any {
                                  return libeithers.Map(libmaybes.Pure).(func(any) any)(recurse(t))
                                }).(func(any) any)(def)).(func(any) any)(func (rdef any) any {
                                  return libeithers.Map(func (rcases []any) any {
                                    return core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: n.(core.Name), Default_: rdef, Cases: rcases}}}
                                  }).(func(any) any)(libeithers.MapList(forField).(func(any) any)(cases))
                                })
                              }()
                            }()
                          }()
                        }(v.Value)
                        case core.EliminationWrap:
                        return func (name core.Name) any {
                          return [2]any{"right", core.FunctionElimination{Value: core.EliminationWrap{Value: name}}}
                        }(v.Value)
                      }
                      return nil
                    }(e)
                  }
                  return func () any {
                    forFunction := func (fun core.Function) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.FunctionElimination:
                          return func (e core.Elimination) any {
                            return forElimination(e)
                          }(v.Value)
                          case core.FunctionLambda:
                          return func (l core.Lambda) any {
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
                                  return libeithers.Bind(recurse(body.(core.Term))).(func(any) any)(func (rbody core.Term) any {
                                    return [2]any{"right", core.FunctionLambda{Value: core.Lambda{Parameter: v.(core.Name), Domain: d, Body: rbody}}}
                                  })
                                }()
                              }()
                            }()
                          }(v.Value)
                          case core.FunctionPrimitive:
                          return func (name core.Name) any {
                            return [2]any{"right", core.FunctionPrimitive{Value: name}}
                          }(v.Value)
                        }
                        return nil
                      }(fun)
                    }
                    return func () any {
                      mapBinding := func (b core.Binding) any {
                        return libeithers.Bind(recurse(func (v any) any {
                          return v.(core.Binding).Term
                        }(b).(core.Term))).(func(any) any)(func (v core.Term) any {
                          return [2]any{"right", core.Binding{Name: func (v any) any {
                            return v.(core.Binding).Name
                          }(b).(core.Name), Term: v, Type_: func (v any) any {
                            return v.(core.Binding).Type_
                          }(b)}}
                        })
                      }
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TermAnnotated:
                          return func (at core.AnnotatedTerm) any {
                            return libeithers.Bind(recurse(func (v any) any {
                              return v.(core.AnnotatedTerm).Body
                            }(at).(core.Term))).(func(any) any)(func (ex core.Term) any {
                              return [2]any{"right", core.TermAnnotated{Value: core.AnnotatedTerm{Body: ex, Annotation: func (v any) any {
                                return v.(core.AnnotatedTerm).Annotation
                              }(at).([]any)}}}
                            })
                          }(v.Value)
                          case core.TermApplication:
                          return func (app core.Application) any {
                            return libeithers.Bind(recurse(func (v any) any {
                              return v.(core.Application).Function
                            }(app).(core.Term))).(func(any) any)(func (lhs core.Term) any {
                              return libeithers.Bind(recurse(func (v any) any {
                                return v.(core.Application).Argument
                              }(app).(core.Term))).(func(any) any)(func (rhs core.Term) any {
                                return [2]any{"right", core.TermApplication{Value: core.Application{Function: lhs, Argument: rhs}}}
                              })
                            })
                          }(v.Value)
                          case core.TermEither:
                          return func (e any) any {
                            return libeithers.Bind(libeithers.Either(func (l core.Term) any {
                              return libeithers.Map(func (x core.Term) any {
                                return [2]any{"left", x}
                              }).(func(any) any)(recurse(l))
                            }).(func(any) any)(func (r core.Term) any {
                              return libeithers.Map(func (x core.Term) any {
                                return [2]any{"right", x}
                              }).(func(any) any)(recurse(r))
                            }).(func(any) any)(e)).(func(any) any)(func (re any) any {
                              return [2]any{"right", core.TermEither{Value: re}}
                            })
                          }(v.Value)
                          case core.TermFunction:
                          return func (fun core.Function) any {
                            return libeithers.Bind(forFunction(fun)).(func(any) any)(func (rfun core.Function) any {
                              return [2]any{"right", core.TermFunction{Value: rfun}}
                            })
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
                                return libeithers.Bind(libeithers.MapList(mapBinding).(func(any) any)(bindings)).(func(any) any)(func (rbindings []any) any {
                                  return libeithers.Bind(recurse(body.(core.Term))).(func(any) any)(func (rbody core.Term) any {
                                    return [2]any{"right", core.TermLet{Value: core.Let{Bindings: rbindings, Body: rbody}}}
                                  })
                                })
                              }()
                            }()
                          }(v.Value)
                          case core.TermList:
                          return func (els []any) any {
                            return libeithers.Bind(libeithers.MapList(recurse).(func(any) any)(els)).(func(any) any)(func (rels []any) any {
                              return [2]any{"right", core.TermList{Value: rels}}
                            })
                          }(v.Value)
                          case core.TermLiteral:
                          return func (v core.Literal) any {
                            return [2]any{"right", core.TermLiteral{Value: v}}
                          }(v.Value)
                          case core.TermMap_:
                          return func (m []any) any {
                            return libeithers.Bind(libeithers.MapList(forPair).(func(any) any)(libmaps.ToList(m))).(func(any) any)(func (pairs []any) any {
                              return [2]any{"right", core.TermMap_{Value: libmaps.FromList(pairs).([]any)}}
                            })
                          }(v.Value)
                          case core.TermMaybe:
                          return func (m any) any {
                            return libeithers.Bind(libeithers.MapMaybe(recurse).(func(any) any)(m)).(func(any) any)(func (rm any) any {
                              return [2]any{"right", core.TermMaybe{Value: rm}}
                            })
                          }(v.Value)
                          case core.TermPair:
                          return func (p any) any {
                            return libeithers.Bind(recurse(libpairs.First(p).(core.Term))).(func(any) any)(func (rfirst core.Term) any {
                              return libeithers.Bind(recurse(libpairs.Second(p).(core.Term))).(func(any) any)(func (rsecond core.Term) any {
                                return [2]any{"right", core.TermPair{Value: [2]any{rfirst, rsecond}}}
                              })
                            })
                          }(v.Value)
                          case core.TermRecord:
                          return func (r core.Record) any {
                            return func () any {
                              var n any = func (v any) any {
                                return v.(core.Record).TypeName
                              }(r)
                              return func () any {
                                var fields any = func (v any) any {
                                  return v.(core.Record).Fields
                                }(r)
                                return libeithers.Map(func (rfields []any) any {
                                  return core.TermRecord{Value: core.Record{TypeName: n.(core.Name), Fields: rfields}}
                                }).(func(any) any)(libeithers.MapList(forField).(func(any) any)(fields))
                              }()
                            }()
                          }(v.Value)
                          case core.TermSet:
                          return func (s []any) any {
                            return libeithers.Bind(libeithers.MapList(recurse).(func(any) any)(libsets.ToList(s))).(func(any) any)(func (rlist []any) any {
                              return [2]any{"right", core.TermSet{Value: libsets.FromList(rlist).([]any)}}
                            })
                          }(v.Value)
                          case core.TermTypeApplication:
                          return func (tt core.TypeApplicationTerm) any {
                            return libeithers.Bind(recurse(func (v any) any {
                              return v.(core.TypeApplicationTerm).Body
                            }(tt).(core.Term))).(func(any) any)(func (t core.Term) any {
                              return [2]any{"right", core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: t, Type_: func (v any) any {
                                return v.(core.TypeApplicationTerm).Type_
                              }(tt).(core.Type)}}}
                            })
                          }(v.Value)
                          case core.TermTypeLambda:
                          return func (tl core.TypeLambda) any {
                            return func () any {
                              var v any = func (v any) any {
                                return v.(core.TypeLambda).Parameter
                              }(tl)
                              return func () any {
                                var body any = func (v any) any {
                                  return v.(core.TypeLambda).Body
                                }(tl)
                                return libeithers.Bind(recurse(body.(core.Term))).(func(any) any)(func (rbody core.Term) any {
                                  return [2]any{"right", core.TermTypeLambda{Value: core.TypeLambda{Parameter: v.(core.Name), Body: rbody}}}
                                })
                              }()
                            }()
                          }(v.Value)
                          case core.TermUnion:
                          return func (i core.Injection) any {
                            return func () any {
                              var n any = func (v any) any {
                                return v.(core.Injection).TypeName
                              }(i)
                              return func () any {
                                var field any = func (v any) any {
                                  return v.(core.Injection).Field
                                }(i)
                                return libeithers.Map(func (rfield core.Field) any {
                                  return core.TermUnion{Value: core.Injection{TypeName: n.(core.Name), Field: rfield}}
                                }).(func(any) any)(forField(field.(core.Field)))
                              }()
                            }()
                          }(v.Value)
                          case core.TermUnit:
                          return func (_ struct{}) any {
                            return [2]any{"right", core.TermUnit{}}
                          }(v)
                          case core.TermVariable:
                          return func (v core.Name) any {
                            return [2]any{"right", core.TermVariable{Value: v}}
                          }(v.Value)
                          case core.TermWrap:
                          return func (wt core.WrappedTerm) any {
                            return func () any {
                              var name any = func (v any) any {
                                return v.(core.WrappedTerm).TypeName
                              }(wt)
                              return func () any {
                                var t any = func (v any) any {
                                  return v.(core.WrappedTerm).Body
                                }(wt)
                                return libeithers.Bind(recurse(t.(core.Term))).(func(any) any)(func (rt core.Term) any {
                                  return [2]any{"right", core.TermWrap{Value: core.WrappedTerm{TypeName: name.(core.Name), Body: rt}}}
                                })
                              }()
                            }()
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
        }
      }
    }
    return func () any {
      var rewrite func(T0) any
      rewrite = func (cx T0) any {
        return func (term core.Term) any {
          return f(func (_p T0) func(core.Term) any {
            return func (v1 T0) any {
              return func (v2 core.Term) any {
                return forSubterms(rewrite).(func(any) any)(v1).(func(any) any)(v2)
              }
            }(_p).(func(core.Term) any)
          })(cx)(term)
        }
      }
      return rewrite(cx0).(func(any) any)(term0)
    }()
  }()
}

func RewriteType (f func(func(core.Type) core.Type) func(core.Type) core.Type, typ0 core.Type) core.Type {
  return func () any {
    fsub := func (recurse func(core.Type) core.Type) any {
      return func (typ core.Type) any {
        return func () any {
          forField := func (field core.FieldType) any {
            return core.FieldType{Name: func (v any) any {
              return v.(core.FieldType).Name
            }(field).(core.Name), Type_: recurse(func (v any) any {
              return v.(core.FieldType).Type_
            }(field).(core.Type))}
          }
          return func (x any) any {
            switch v := x.(type) {
              case core.TypeAnnotated:
              return func (at core.AnnotatedType) any {
                return core.TypeAnnotated{Value: core.AnnotatedType{Body: recurse(func (v any) any {
                  return v.(core.AnnotatedType).Body
                }(at).(core.Type)), Annotation: func (v any) any {
                  return v.(core.AnnotatedType).Annotation
                }(at).([]any)}}
              }(v.Value)
              case core.TypeApplication:
              return func (app core.ApplicationType) any {
                return core.TypeApplication{Value: core.ApplicationType{Function: recurse(func (v any) any {
                  return v.(core.ApplicationType).Function
                }(app).(core.Type)), Argument: recurse(func (v any) any {
                  return v.(core.ApplicationType).Argument
                }(app).(core.Type))}}
              }(v.Value)
              case core.TypeEither:
              return func (et core.EitherType) any {
                return core.TypeEither{Value: core.EitherType{Left: recurse(func (v any) any {
                  return v.(core.EitherType).Left
                }(et).(core.Type)), Right: recurse(func (v any) any {
                  return v.(core.EitherType).Right
                }(et).(core.Type))}}
              }(v.Value)
              case core.TypePair:
              return func (pt core.PairType) any {
                return core.TypePair{Value: core.PairType{First: recurse(func (v any) any {
                  return v.(core.PairType).First
                }(pt).(core.Type)), Second: recurse(func (v any) any {
                  return v.(core.PairType).Second
                }(pt).(core.Type))}}
              }(v.Value)
              case core.TypeFunction:
              return func (fun core.FunctionType) any {
                return core.TypeFunction{Value: core.FunctionType{Domain: recurse(func (v any) any {
                  return v.(core.FunctionType).Domain
                }(fun).(core.Type)), Codomain: recurse(func (v any) any {
                  return v.(core.FunctionType).Codomain
                }(fun).(core.Type))}}
              }(v.Value)
              case core.TypeForall:
              return func (lt core.ForallType) any {
                return core.TypeForall{Value: core.ForallType{Parameter: func (v any) any {
                  return v.(core.ForallType).Parameter
                }(lt).(core.Name), Body: recurse(func (v any) any {
                  return v.(core.ForallType).Body
                }(lt).(core.Type))}}
              }(v.Value)
              case core.TypeList:
              return func (t core.Type) any {
                return core.TypeList{Value: recurse(t)}
              }(v.Value)
              case core.TypeLiteral:
              return func (lt core.LiteralType) any {
                return core.TypeLiteral{Value: lt}
              }(v.Value)
              case core.TypeMap_:
              return func (mt core.MapType) any {
                return core.TypeMap_{Value: core.MapType{Keys: recurse(func (v any) any {
                  return v.(core.MapType).Keys
                }(mt).(core.Type)), Values: recurse(func (v any) any {
                  return v.(core.MapType).Values
                }(mt).(core.Type))}}
              }(v.Value)
              case core.TypeMaybe:
              return func (t core.Type) any {
                return core.TypeMaybe{Value: recurse(t)}
              }(v.Value)
              case core.TypeRecord:
              return func (rt []any) any {
                return core.TypeRecord{Value: liblists.Map(forField).(func(any) any)(rt).([]any)}
              }(v.Value)
              case core.TypeSet:
              return func (t core.Type) any {
                return core.TypeSet{Value: recurse(t)}
              }(v.Value)
              case core.TypeUnion:
              return func (rt []any) any {
                return core.TypeUnion{Value: liblists.Map(forField).(func(any) any)(rt).([]any)}
              }(v.Value)
              case core.TypeUnit:
              return func (_ struct{}) any {
                return core.TypeUnit{}
              }(v)
              case core.TypeVariable:
              return func (v core.Name) any {
                return core.TypeVariable{Value: v}
              }(v.Value)
              case core.TypeWrap:
              return func (wt core.Type) any {
                return core.TypeWrap{Value: recurse(wt)}
              }(v.Value)
            }
            return nil
          }(typ)
        }()
      }
    }
    return func () any {
      var recurse func(core.Type) any
      recurse = func (v1 core.Type) any {
        return f(func (_p core.Type) core.Type {
          return func (v12 core.Type) any {
            return fsub(recurse).(func(any) any)(v12)
          }(_p).(core.Type)
        })(v1)
      }
      return recurse(typ0)
    }()
  }().(core.Type)
}

func RewriteTypeM (f func(func(core.Type) any) func(core.Type) any, typ0 core.Type) any {
  return func () any {
    fsub := func (recurse func(core.Type) any) any {
      return func (typ core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeAnnotated:
            return func (at core.AnnotatedType) any {
              return libeithers.Bind(recurse(func (v any) any {
                return v.(core.AnnotatedType).Body
              }(at).(core.Type))).(func(any) any)(func (t core.Type) any {
                return [2]any{"right", core.TypeAnnotated{Value: core.AnnotatedType{Body: t, Annotation: func (v any) any {
                  return v.(core.AnnotatedType).Annotation
                }(at).([]any)}}}
              })
            }(v.Value)
            case core.TypeApplication:
            return func (at core.ApplicationType) any {
              return libeithers.Bind(recurse(func (v any) any {
                return v.(core.ApplicationType).Function
              }(at).(core.Type))).(func(any) any)(func (lhs core.Type) any {
                return libeithers.Bind(recurse(func (v any) any {
                  return v.(core.ApplicationType).Argument
                }(at).(core.Type))).(func(any) any)(func (rhs core.Type) any {
                  return [2]any{"right", core.TypeApplication{Value: core.ApplicationType{Function: lhs, Argument: rhs}}}
                })
              })
            }(v.Value)
            case core.TypeEither:
            return func (et core.EitherType) any {
              return libeithers.Bind(recurse(func (v any) any {
                return v.(core.EitherType).Left
              }(et).(core.Type))).(func(any) any)(func (left core.Type) any {
                return libeithers.Bind(recurse(func (v any) any {
                  return v.(core.EitherType).Right
                }(et).(core.Type))).(func(any) any)(func (right core.Type) any {
                  return [2]any{"right", core.TypeEither{Value: core.EitherType{Left: left, Right: right}}}
                })
              })
            }(v.Value)
            case core.TypePair:
            return func (pt core.PairType) any {
              return libeithers.Bind(recurse(func (v any) any {
                return v.(core.PairType).First
              }(pt).(core.Type))).(func(any) any)(func (pairFirst core.Type) any {
                return libeithers.Bind(recurse(func (v any) any {
                  return v.(core.PairType).Second
                }(pt).(core.Type))).(func(any) any)(func (pairSecond core.Type) any {
                  return [2]any{"right", core.TypePair{Value: core.PairType{First: pairFirst, Second: pairSecond}}}
                })
              })
            }(v.Value)
            case core.TypeFunction:
            return func (ft core.FunctionType) any {
              return libeithers.Bind(recurse(func (v any) any {
                return v.(core.FunctionType).Domain
              }(ft).(core.Type))).(func(any) any)(func (dom core.Type) any {
                return libeithers.Bind(recurse(func (v any) any {
                  return v.(core.FunctionType).Codomain
                }(ft).(core.Type))).(func(any) any)(func (cod core.Type) any {
                  return [2]any{"right", core.TypeFunction{Value: core.FunctionType{Domain: dom, Codomain: cod}}}
                })
              })
            }(v.Value)
            case core.TypeForall:
            return func (ft core.ForallType) any {
              return libeithers.Bind(recurse(func (v any) any {
                return v.(core.ForallType).Body
              }(ft).(core.Type))).(func(any) any)(func (b core.Type) any {
                return [2]any{"right", core.TypeForall{Value: core.ForallType{Parameter: func (v any) any {
                  return v.(core.ForallType).Parameter
                }(ft).(core.Name), Body: b}}}
              })
            }(v.Value)
            case core.TypeList:
            return func (t core.Type) any {
              return libeithers.Bind(recurse(t)).(func(any) any)(func (rt core.Type) any {
                return [2]any{"right", core.TypeList{Value: rt}}
              })
            }(v.Value)
            case core.TypeLiteral:
            return func (lt core.LiteralType) any {
              return [2]any{"right", core.TypeLiteral{Value: lt}}
            }(v.Value)
            case core.TypeMap_:
            return func (mt core.MapType) any {
              return libeithers.Bind(recurse(func (v any) any {
                return v.(core.MapType).Keys
              }(mt).(core.Type))).(func(any) any)(func (kt core.Type) any {
                return libeithers.Bind(recurse(func (v any) any {
                  return v.(core.MapType).Values
                }(mt).(core.Type))).(func(any) any)(func (vt core.Type) any {
                  return [2]any{"right", core.TypeMap_{Value: core.MapType{Keys: kt, Values: vt}}}
                })
              })
            }(v.Value)
            case core.TypeMaybe:
            return func (t core.Type) any {
              return libeithers.Bind(recurse(t)).(func(any) any)(func (rt core.Type) any {
                return [2]any{"right", core.TypeMaybe{Value: rt}}
              })
            }(v.Value)
            case core.TypeRecord:
            return func (rt []any) any {
              return func () any {
                forField := func (f2 core.FieldType) any {
                  return libeithers.Bind(recurse(func (v any) any {
                    return v.(core.FieldType).Type_
                  }(f2).(core.Type))).(func(any) any)(func (t core.Type) any {
                    return [2]any{"right", core.FieldType{Name: func (v any) any {
                      return v.(core.FieldType).Name
                    }(f2).(core.Name), Type_: t}}
                  })
                }
                return libeithers.Bind(libeithers.MapList(forField).(func(any) any)(rt)).(func(any) any)(func (rfields []any) any {
                  return [2]any{"right", core.TypeRecord{Value: rfields}}
                })
              }()
            }(v.Value)
            case core.TypeSet:
            return func (t core.Type) any {
              return libeithers.Bind(recurse(t)).(func(any) any)(func (rt core.Type) any {
                return [2]any{"right", core.TypeSet{Value: rt}}
              })
            }(v.Value)
            case core.TypeUnion:
            return func (rt []any) any {
              return func () any {
                forField := func (f2 core.FieldType) any {
                  return libeithers.Bind(recurse(func (v any) any {
                    return v.(core.FieldType).Type_
                  }(f2).(core.Type))).(func(any) any)(func (t core.Type) any {
                    return [2]any{"right", core.FieldType{Name: func (v any) any {
                      return v.(core.FieldType).Name
                    }(f2).(core.Name), Type_: t}}
                  })
                }
                return libeithers.Bind(libeithers.MapList(forField).(func(any) any)(rt)).(func(any) any)(func (rfields []any) any {
                  return [2]any{"right", core.TypeUnion{Value: rfields}}
                })
              }()
            }(v.Value)
            case core.TypeUnit:
            return func (_ struct{}) any {
              return [2]any{"right", core.TypeUnit{}}
            }(v)
            case core.TypeVariable:
            return func (v core.Name) any {
              return [2]any{"right", core.TypeVariable{Value: v}}
            }(v.Value)
            case core.TypeWrap:
            return func (wt core.Type) any {
              return libeithers.Bind(recurse(wt)).(func(any) any)(func (t core.Type) any {
                return [2]any{"right", core.TypeWrap{Value: t}}
              })
            }(v.Value)
          }
          return nil
        }(typ)
      }
    }
    return func () any {
      var recurse func(core.Type) any
      recurse = func (v1 core.Type) any {
        return f(func (_p core.Type) any {
          return func (v12 core.Type) any {
            return fsub(recurse).(func(any) any)(v12)
          }(_p)
        })(v1)
      }
      return recurse(typ0)
    }()
  }()
}

func SimplifyTerm (term core.Term) core.Term {
  return func () any {
    simplify := func (recurse func(core.Term) core.Term) any {
      return func (term2 core.Term) any {
        return func () any {
          forRhs := func (rhs core.Term) any {
            return func (var_ core.Name) any {
              return func (body core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermVariable:
                    return func (v core.Name) any {
                      return SimplifyTerm(SubstituteVariable(var_, v, body))
                    }(v.Value)
                    default:
                    return term2
                  }
                  return nil
                }(DeannotateTerm(rhs))
              }
            }
          }
          return func () any {
            forLhs := func (lhs core.Term) any {
              return func (rhs core.Term) any {
                return func () any {
                  forFun := func (fun core.Function) any {
                    return func (x any) any {
                      switch v := x.(type) {
                        case core.FunctionLambda:
                        return func (l core.Lambda) any {
                          return func () any {
                            var var_ any = func (v any) any {
                              return v.(core.Lambda).Parameter
                            }(l)
                            return func () any {
                              var body any = func (v any) any {
                                return v.(core.Lambda).Body
                              }(l)
                              return liblogic.IfElse(libsets.Member(var_).(func(any) any)(FreeVariablesInTerm(body.(core.Term)))).(func(any) any)(forRhs(rhs).(func(any) any)(var_).(func(any) any)(body)).(func(any) any)(SimplifyTerm(body.(core.Term)))
                            }()
                          }()
                        }(v.Value)
                        default:
                        return term2
                      }
                      return nil
                    }(fun)
                  }
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.TermFunction:
                      return func (fun core.Function) any {
                        return forFun(fun)
                      }(v.Value)
                      default:
                      return term2
                    }
                    return nil
                  }(DeannotateTerm(lhs))
                }()
              }
            }
            return func () any {
              forTerm := func (stripped core.Term) any {
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
                          return forLhs(lhs.(core.Term)).(func(any) any)(rhs)
                        }()
                      }()
                    }(v.Value)
                    default:
                    return term2
                  }
                  return nil
                }(stripped)
              }
              return func () any {
                var stripped any = DeannotateTerm(term2)
                return recurse(forTerm(stripped.(core.Term)).(core.Term))
              }()
            }()
          }()
        }()
      }
    }
    return RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return simplify(_p).(func(core.Term) core.Term)
    }, term)
  }().(core.Term)
}

func SubstituteTypeVariables (subst []any, typ core.Type) core.Type {
  return func () any {
    replace := func (recurse func(core.Type) core.Type) any {
      return func (typ2 core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeVariable:
            return func (n core.Name) any {
              return core.TypeVariable{Value: libmaybes.FromMaybe(n).(func(any) any)(libmaps.Lookup(n).(func(any) any)(subst)).(core.Name)}
            }(v.Value)
            default:
            return recurse(typ2)
          }
          return nil
        }(typ2)
      }
    }
    return RewriteType(func (_p func(core.Type) core.Type) func(core.Type) core.Type {
      return replace(_p).(func(core.Type) core.Type)
    }, typ)
  }().(core.Type)
}

func SubstituteVariable (from core.Name, to core.Name, term core.Term) core.Term {
  return func () any {
    replace := func (recurse func(core.Term) core.Term) any {
      return func (term2 core.Term) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TermVariable:
            return func (x core.Name) any {
              return core.TermVariable{Value: liblogic.IfElse(libequality.Equal(x).(func(any) any)(from)).(func(any) any)(to).(func(any) any)(x).(core.Name)}
            }(v.Value)
            case core.TermFunction:
            return func (v1 core.Function) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.FunctionLambda:
                  return func (l core.Lambda) any {
                    return liblogic.IfElse(libequality.Equal(func (v any) any {
                      return v.(core.Lambda).Parameter
                    }(l)).(func(any) any)(from)).(func(any) any)(term2).(func(any) any)(recurse(term2))
                  }(v.Value)
                  default:
                  return recurse(term2)
                }
                return nil
              }(v1)
            }(v.Value)
            default:
            return recurse(term2)
          }
          return nil
        }(term2)
      }
    }
    return RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return replace(_p).(func(core.Term) core.Term)
    }, term)
  }().(core.Term)
}

func SubstituteVariables (subst []any, term core.Term) core.Term {
  return func () any {
    replace := func (recurse func(core.Term) core.Term) any {
      return func (term2 core.Term) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TermVariable:
            return func (n core.Name) any {
              return core.TermVariable{Value: libmaybes.FromMaybe(n).(func(any) any)(libmaps.Lookup(n).(func(any) any)(subst)).(core.Name)}
            }(v.Value)
            case core.TermFunction:
            return func (v1 core.Function) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.FunctionLambda:
                  return func (l core.Lambda) any {
                    return libmaybes.Maybe(recurse(term2)).(func(any) any)(func (_ core.Name) any {
                      return term2
                    }).(func(any) any)(libmaps.Lookup(func (v any) any {
                      return v.(core.Lambda).Parameter
                    }(l)).(func(any) any)(subst))
                  }(v.Value)
                  default:
                  return recurse(term2)
                }
                return nil
              }(v1)
            }(v.Value)
            default:
            return recurse(term2)
          }
          return nil
        }(term2)
      }
    }
    return RewriteTerm(func (_p func(core.Term) core.Term) func(core.Term) core.Term {
      return replace(_p).(func(core.Term) core.Term)
    }, term)
  }().(core.Term)
}

func StripTypeLambdas (t core.Term) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (at core.AnnotatedTerm) any {
        return func () any {
          var subj any = func (v any) any {
            return v.(core.AnnotatedTerm).Body
          }(at)
          return func () any {
            var ann any = func (v any) any {
              return v.(core.AnnotatedTerm).Annotation
            }(at)
            return core.TermAnnotated{Value: core.AnnotatedTerm{Body: StripTypeLambdas(subj.(core.Term)), Annotation: ann.([]any)}}
          }()
        }()
      }(v.Value)
      case core.TermTypeLambda:
      return func (ta core.TypeLambda) any {
        return StripTypeLambdas(func (v any) any {
          return v.(core.TypeLambda).Body
        }(ta).(core.Term))
      }(v.Value)
      default:
      return t
    }
    return nil
  }(t).(core.Term)
}

func Subterms (v1 core.Term) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (at core.AnnotatedTerm) any {
        return []any{func (v any) any {
          return v.(core.AnnotatedTerm).Body
        }(at)}
      }(v.Value)
      case core.TermApplication:
      return func (p core.Application) any {
        return []any{func (v any) any {
          return v.(core.Application).Function
        }(p), func (v any) any {
          return v.(core.Application).Argument
        }(p)}
      }(v.Value)
      case core.TermEither:
      return func (e any) any {
        return libeithers.Either(func (l core.Term) any {
          return []any{l}
        }).(func(any) any)(func (r core.Term) any {
          return []any{r}
        }).(func(any) any)(e)
      }(v.Value)
      case core.TermFunction:
      return func (v12 core.Function) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.FunctionElimination:
            return func (v13 core.Elimination) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.EliminationUnion:
                  return func (cs core.CaseStatement) any {
                    return liblists.Concat2(libmaybes.Maybe([]any{}).(func(any) any)(func (t core.Term) any {
                      return []any{t}
                    }).(func(any) any)(func (v any) any {
                      return v.(core.CaseStatement).Default_
                    }(cs))).(func(any) any)(liblists.Map(func (v any) any {
                      return v.(core.Field).Term
                    }).(func(any) any)(func (v any) any {
                      return v.(core.CaseStatement).Cases
                    }(cs)))
                  }(v.Value)
                  default:
                  return []any{}
                }
                return nil
              }(v13)
            }(v.Value)
            case core.FunctionLambda:
            return func (l core.Lambda) any {
              return []any{func (v any) any {
                return v.(core.Lambda).Body
              }(l)}
            }(v.Value)
            default:
            return []any{}
          }
          return nil
        }(v12)
      }(v.Value)
      case core.TermLet:
      return func (lt core.Let) any {
        return liblists.Cons(func (v any) any {
          return v.(core.Let).Body
        }(lt)).(func(any) any)(liblists.Map(func (v any) any {
          return v.(core.Binding).Term
        }).(func(any) any)(func (v any) any {
          return v.(core.Let).Bindings
        }(lt)))
      }(v.Value)
      case core.TermList:
      return func (l []any) any {
        return l
      }(v.Value)
      case core.TermLiteral:
      return func (_ core.Literal) any {
        return []any{}
      }(v.Value)
      case core.TermMap_:
      return func (m []any) any {
        return liblists.Concat(liblists.Map(func (p any) any {
          return []any{libpairs.First(p), libpairs.Second(p)}
        }).(func(any) any)(libmaps.ToList(m)))
      }(v.Value)
      case core.TermMaybe:
      return func (m any) any {
        return libmaybes.Maybe([]any{}).(func(any) any)(func (t core.Term) any {
          return []any{t}
        }).(func(any) any)(m)
      }(v.Value)
      case core.TermPair:
      return func (p any) any {
        return []any{libpairs.First(p), libpairs.Second(p)}
      }(v.Value)
      case core.TermRecord:
      return func (rt core.Record) any {
        return liblists.Map(func (v any) any {
          return v.(core.Field).Term
        }).(func(any) any)(func (v any) any {
          return v.(core.Record).Fields
        }(rt))
      }(v.Value)
      case core.TermSet:
      return func (l []any) any {
        return libsets.ToList(l)
      }(v.Value)
      case core.TermTypeApplication:
      return func (ta core.TypeApplicationTerm) any {
        return []any{func (v any) any {
          return v.(core.TypeApplicationTerm).Body
        }(ta)}
      }(v.Value)
      case core.TermTypeLambda:
      return func (ta core.TypeLambda) any {
        return []any{func (v any) any {
          return v.(core.TypeLambda).Body
        }(ta)}
      }(v.Value)
      case core.TermUnion:
      return func (ut core.Injection) any {
        return []any{func (v any) any {
          return v.(core.Injection).Field
        }(ut).(core.Field).Term}
      }(v.Value)
      case core.TermUnit:
      return func (_ struct{}) any {
        return []any{}
      }(v)
      case core.TermVariable:
      return func (_ core.Name) any {
        return []any{}
      }(v.Value)
      case core.TermWrap:
      return func (n core.WrappedTerm) any {
        return []any{func (v any) any {
          return v.(core.WrappedTerm).Body
        }(n)}
      }(v.Value)
    }
    return nil
  }(v1).([]any)
}

func SubtermsWithAccessors (v1 core.Term) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (at core.AnnotatedTerm) any {
        return []any{[2]any{accessors.TermAccessorAnnotatedBody{}, func (v any) any {
          return v.(core.AnnotatedTerm).Body
        }(at)}}
      }(v.Value)
      case core.TermApplication:
      return func (p core.Application) any {
        return []any{[2]any{accessors.TermAccessorApplicationFunction{}, func (v any) any {
          return v.(core.Application).Function
        }(p)}, [2]any{accessors.TermAccessorApplicationArgument{}, func (v any) any {
          return v.(core.Application).Argument
        }(p)}}
      }(v.Value)
      case core.TermEither:
      return func (e any) any {
        return []any{}
      }(v.Value)
      case core.TermFunction:
      return func (v12 core.Function) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.FunctionElimination:
            return func (v13 core.Elimination) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.EliminationUnion:
                  return func (cs core.CaseStatement) any {
                    return liblists.Concat2(libmaybes.Maybe([]any{}).(func(any) any)(func (t core.Term) any {
                      return []any{[2]any{accessors.TermAccessorUnionCasesDefault{}, t}}
                    }).(func(any) any)(func (v any) any {
                      return v.(core.CaseStatement).Default_
                    }(cs))).(func(any) any)(liblists.Map(func (f core.Field) any {
                      return [2]any{accessors.TermAccessorUnionCasesBranch{Value: func (v any) any {
                        return v.(core.Field).Name
                      }(f).(core.Name)}, func (v any) any {
                        return v.(core.Field).Term
                      }(f)}
                    }).(func(any) any)(func (v any) any {
                      return v.(core.CaseStatement).Cases
                    }(cs)))
                  }(v.Value)
                  default:
                  return []any{}
                }
                return nil
              }(v13)
            }(v.Value)
            case core.FunctionLambda:
            return func (l core.Lambda) any {
              return []any{[2]any{accessors.TermAccessorLambdaBody{}, func (v any) any {
                return v.(core.Lambda).Body
              }(l)}}
            }(v.Value)
            default:
            return []any{}
          }
          return nil
        }(v12)
      }(v.Value)
      case core.TermLet:
      return func (lt core.Let) any {
        return liblists.Cons([2]any{accessors.TermAccessorLetBody{}, func (v any) any {
          return v.(core.Let).Body
        }(lt)}).(func(any) any)(liblists.Map(func (b core.Binding) any {
          return [2]any{accessors.TermAccessorLetBinding{Value: func (v any) any {
            return v.(core.Binding).Name
          }(b).(core.Name)}, func (v any) any {
            return v.(core.Binding).Term
          }(b)}
        }).(func(any) any)(func (v any) any {
          return v.(core.Let).Bindings
        }(lt)))
      }(v.Value)
      case core.TermList:
      return func (l []any) any {
        return liblists.Map(func (e core.Term) any {
          return [2]any{accessors.TermAccessorListElement{Value: 0}, e}
        }).(func(any) any)(l)
      }(v.Value)
      case core.TermLiteral:
      return func (_ core.Literal) any {
        return []any{}
      }(v.Value)
      case core.TermMap_:
      return func (m []any) any {
        return liblists.Concat(liblists.Map(func (p any) any {
          return []any{[2]any{accessors.TermAccessorMapKey{Value: 0}, libpairs.First(p)}, [2]any{accessors.TermAccessorMapValue{Value: 0}, libpairs.Second(p)}}
        }).(func(any) any)(libmaps.ToList(m)))
      }(v.Value)
      case core.TermMaybe:
      return func (m any) any {
        return libmaybes.Maybe([]any{}).(func(any) any)(func (t core.Term) any {
          return []any{[2]any{accessors.TermAccessorMaybeTerm{}, t}}
        }).(func(any) any)(m)
      }(v.Value)
      case core.TermPair:
      return func (p any) any {
        return []any{}
      }(v.Value)
      case core.TermRecord:
      return func (rt core.Record) any {
        return liblists.Map(func (f core.Field) any {
          return [2]any{accessors.TermAccessorRecordField{Value: func (v any) any {
            return v.(core.Field).Name
          }(f).(core.Name)}, func (v any) any {
            return v.(core.Field).Term
          }(f)}
        }).(func(any) any)(func (v any) any {
          return v.(core.Record).Fields
        }(rt))
      }(v.Value)
      case core.TermSet:
      return func (s []any) any {
        return liblists.Map(func (e core.Term) any {
          return [2]any{accessors.TermAccessorListElement{Value: 0}, e}
        }).(func(any) any)(libsets.ToList(s))
      }(v.Value)
      case core.TermTypeApplication:
      return func (ta core.TypeApplicationTerm) any {
        return []any{[2]any{accessors.TermAccessorTypeApplicationTerm{}, func (v any) any {
          return v.(core.TypeApplicationTerm).Body
        }(ta)}}
      }(v.Value)
      case core.TermTypeLambda:
      return func (ta core.TypeLambda) any {
        return []any{[2]any{accessors.TermAccessorTypeLambdaBody{}, func (v any) any {
          return v.(core.TypeLambda).Body
        }(ta)}}
      }(v.Value)
      case core.TermUnion:
      return func (ut core.Injection) any {
        return []any{[2]any{accessors.TermAccessorInjectionTerm{}, func (v any) any {
          return v.(core.Injection).Field
        }(ut).(core.Field).Term}}
      }(v.Value)
      case core.TermUnit:
      return func (_ struct{}) any {
        return []any{}
      }(v)
      case core.TermVariable:
      return func (_ core.Name) any {
        return []any{}
      }(v.Value)
      case core.TermWrap:
      return func (n core.WrappedTerm) any {
        return []any{[2]any{accessors.TermAccessorWrappedTerm{}, func (v any) any {
          return v.(core.WrappedTerm).Body
        }(n)}}
      }(v.Value)
    }
    return nil
  }(v1).([]any)
}

func Subtypes (v1 core.Type) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return []any{func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at)}
      }(v.Value)
      case core.TypeApplication:
      return func (at core.ApplicationType) any {
        return []any{func (v any) any {
          return v.(core.ApplicationType).Function
        }(at), func (v any) any {
          return v.(core.ApplicationType).Argument
        }(at)}
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return []any{func (v any) any {
          return v.(core.EitherType).Left
        }(et), func (v any) any {
          return v.(core.EitherType).Right
        }(et)}
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return []any{func (v any) any {
          return v.(core.PairType).First
        }(pt), func (v any) any {
          return v.(core.PairType).Second
        }(pt)}
      }(v.Value)
      case core.TypeFunction:
      return func (ft core.FunctionType) any {
        return []any{func (v any) any {
          return v.(core.FunctionType).Domain
        }(ft), func (v any) any {
          return v.(core.FunctionType).Codomain
        }(ft)}
      }(v.Value)
      case core.TypeForall:
      return func (lt core.ForallType) any {
        return []any{func (v any) any {
          return v.(core.ForallType).Body
        }(lt)}
      }(v.Value)
      case core.TypeList:
      return func (lt core.Type) any {
        return []any{lt}
      }(v.Value)
      case core.TypeLiteral:
      return func (_ core.LiteralType) any {
        return []any{}
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return []any{func (v any) any {
          return v.(core.MapType).Keys
        }(mt), func (v any) any {
          return v.(core.MapType).Values
        }(mt)}
      }(v.Value)
      case core.TypeMaybe:
      return func (ot core.Type) any {
        return []any{ot}
      }(v.Value)
      case core.TypeRecord:
      return func (rt []any) any {
        return liblists.Map(func (v any) any {
          return v.(core.FieldType).Type_
        }).(func(any) any)(rt)
      }(v.Value)
      case core.TypeSet:
      return func (st core.Type) any {
        return []any{st}
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return liblists.Map(func (v any) any {
          return v.(core.FieldType).Type_
        }).(func(any) any)(rt)
      }(v.Value)
      case core.TypeUnit:
      return func (_ struct{}) any {
        return []any{}
      }(v)
      case core.TypeVariable:
      return func (_ core.Name) any {
        return []any{}
      }(v.Value)
      case core.TypeWrap:
      return func (nt core.Type) any {
        return []any{nt}
      }(v.Value)
    }
    return nil
  }(v1).([]any)
}

func TermDependencyNames (binds bool, withPrims bool, withNoms bool, term0 core.Term) []any {
  return func () any {
    addNames := func (names []any) any {
      return func (term core.Term) any {
        return func () any {
          nominal := func (name core.Name) any {
            return liblogic.IfElse(withNoms).(func(any) any)(libsets.Insert(name).(func(any) any)(names)).(func(any) any)(names)
          }
          return func () any {
            prim := func (name core.Name) any {
              return liblogic.IfElse(withPrims).(func(any) any)(libsets.Insert(name).(func(any) any)(names)).(func(any) any)(names)
            }
            return func () any {
              var_ := func (name core.Name) any {
                return liblogic.IfElse(binds).(func(any) any)(libsets.Insert(name).(func(any) any)(names)).(func(any) any)(names)
              }
              return func (x any) any {
                switch v := x.(type) {
                  case core.TermFunction:
                  return func (f core.Function) any {
                    return func (x any) any {
                      switch v := x.(type) {
                        case core.FunctionPrimitive:
                        return func (name core.Name) any {
                          return prim(name)
                        }(v.Value)
                        case core.FunctionElimination:
                        return func (e core.Elimination) any {
                          return func (x any) any {
                            switch v := x.(type) {
                              case core.EliminationRecord:
                              return func (proj core.Projection) any {
                                return nominal(func (v any) any {
                                  return v.(core.Projection).TypeName
                                }(proj).(core.Name))
                              }(v.Value)
                              case core.EliminationUnion:
                              return func (caseStmt core.CaseStatement) any {
                                return nominal(func (v any) any {
                                  return v.(core.CaseStatement).TypeName
                                }(caseStmt).(core.Name))
                              }(v.Value)
                              case core.EliminationWrap:
                              return func (name core.Name) any {
                                return nominal(name)
                              }(v.Value)
                            }
                            return nil
                          }(e)
                        }(v.Value)
                        default:
                        return names
                      }
                      return nil
                    }(f)
                  }(v.Value)
                  case core.TermRecord:
                  return func (record core.Record) any {
                    return nominal(func (v any) any {
                      return v.(core.Record).TypeName
                    }(record).(core.Name))
                  }(v.Value)
                  case core.TermUnion:
                  return func (injection core.Injection) any {
                    return nominal(func (v any) any {
                      return v.(core.Injection).TypeName
                    }(injection).(core.Name))
                  }(v.Value)
                  case core.TermVariable:
                  return func (name core.Name) any {
                    return var_(name)
                  }(v.Value)
                  case core.TermWrap:
                  return func (wrappedTerm core.WrappedTerm) any {
                    return nominal(func (v any) any {
                      return v.(core.WrappedTerm).TypeName
                    }(wrappedTerm).(core.Name))
                  }(v.Value)
                  default:
                  return names
                }
                return nil
              }(term)
            }()
          }()
        }()
      }
    }
    return FoldOverTerm(coders.TraversalOrderPre{}, func (_p any) func(core.Term) any {
      return addNames(_p.([]any)).(func(core.Term) any)
    }, libsets.Empty, term0)
  }().([]any)
}

func ToShortNames (original []any) []any {
  return func () any {
    addName := func (acc []any) any {
      return func (name core.Name) any {
        return func () any {
          var local any = names.LocalNameOf(name)
          return func () any {
            var group any = libmaybes.FromMaybe(libsets.Empty).(func(any) any)(libmaps.Lookup(local).(func(any) any)(acc))
            return libmaps.Insert(local).(func(any) any)(libsets.Insert(name).(func(any) any)(group)).(func(any) any)(acc)
          }()
        }()
      }
    }
    return func () any {
      groupNamesByLocal := func (names []any) any {
        return liblists.Foldl(addName).(func(any) any)(libmaps.Empty).(func(any) any)(names)
      }
      return func () any {
        var groups any = groupNamesByLocal(original)
        return func () any {
          renameGroup := func (localNames any) any {
            return func () any {
              var local any = libpairs.First(localNames)
              return func () any {
                var names any = libpairs.Second(localNames)
                return func () any {
                  var rangeFrom func(int32) any
                  rangeFrom = func (start int32) any {
                    return liblists.Cons(start).(func(any) any)(rangeFrom(libmath.Add(start).(func(any) any)(1).(int32)))
                  }
                  return func () any {
                    rename := func (name core.Name) any {
                      return func (i int32) any {
                        return [2]any{name, core.Name(liblogic.IfElse(libequality.Gt(i).(func(any) any)(1)).(func(any) any)(libstrings.Cat2(local).(func(any) any)(libliterals.ShowInt32(i))).(func(any) any)(local).(string))}
                      }
                    }
                    return liblists.ZipWith(rename).(func(any) any)(libsets.ToList(names)).(func(any) any)(rangeFrom(1))
                  }()
                }()
              }()
            }()
          }
          return libmaps.FromList(liblists.Concat(liblists.Map(renameGroup).(func(any) any)(libmaps.ToList(groups))))
        }()
      }()
    }()
  }().([]any)
}

func TopologicalSortBindingMap (bindingMap []any) []any {
  return func () any {
    var bindings any = libmaps.ToList(bindingMap)
    return func () any {
      var keys any = libsets.FromList(liblists.Map(libpairs.First).(func(any) any)(bindings))
      return func () any {
        var hasTypeAnnotation func(core.Term) any
        hasTypeAnnotation = func (term core.Term) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.TermAnnotated:
              return func (at core.AnnotatedTerm) any {
                return hasTypeAnnotation(func (v any) any {
                  return v.(core.AnnotatedTerm).Body
                }(at).(core.Term))
              }(v.Value)
              default:
              return false
            }
            return nil
          }(term)
        }
        return func () any {
          depsOf := func (nameAndTerm any) any {
            return func () any {
              var name any = libpairs.First(nameAndTerm)
              return func () any {
                var term any = libpairs.Second(nameAndTerm)
                return [2]any{name, liblogic.IfElse(hasTypeAnnotation(term.(core.Term))).(func(any) any)([]any{}).(func(any) any)(libsets.ToList(libsets.Intersection(keys).(func(any) any)(FreeVariablesInTerm(term.(core.Term)))))}
              }()
            }()
          }
          return func () any {
            toPair := func (name core.Name) any {
              return [2]any{name, libmaybes.FromMaybe(core.TermLiteral{Value: core.LiteralString_{Value: "Impossible!"}}).(func(any) any)(libmaps.Lookup(name).(func(any) any)(bindingMap))}
            }
            return liblists.Map(func (v1 []any) any {
              return liblists.Map(toPair).(func(any) any)(v1)
            }).(func(any) any)(sorting.TopologicalSortComponents(liblists.Map(depsOf).(func(any) any)(bindings).([]any)))
          }()
        }()
      }()
    }()
  }().([]any)
}

func TopologicalSortBindings (els []any) any {
  return func () any {
    adjlist := func (e core.Binding) any {
      return [2]any{func (v any) any {
        return v.(core.Binding).Name
      }(e), libsets.ToList(TermDependencyNames(false, true, true, func (v any) any {
        return v.(core.Binding).Term
      }(e).(core.Term)))}
    }
    return sorting.TopologicalSort(liblists.Map(adjlist).(func(any) any)(els).([]any))
  }()
}

func TypeDependencyNames (withSchema bool, typ core.Type) []any {
  return liblogic.IfElse(withSchema).(func(any) any)(libsets.Union(FreeVariablesInType(typ)).(func(any) any)(TypeNamesInType(typ))).(func(any) any)(FreeVariablesInType(typ)).([]any)
}

func TypeNamesInType (typ0 core.Type) []any {
  return func () any {
    addNames := func (names []any) any {
      return func (typ core.Type) any {
        return names
      }
    }
    return FoldOverType(coders.TraversalOrderPre{}, func (_p any) func(core.Type) any {
      return addNames(_p).(func(core.Type) any)
    }, libsets.Empty, typ0)
  }().([]any)
}

func TypeSchemeToFType (ts core.TypeScheme) core.Type {
  return func () any {
    var vars any = func (v any) any {
      return v.(core.TypeScheme).Variables
    }(ts)
    return func () any {
      var body any = func (v any) any {
        return v.(core.TypeScheme).Type_
      }(ts)
      return liblists.Foldl(func (t core.Type) any {
        return func (v core.Name) any {
          return core.TypeForall{Value: core.ForallType{Parameter: v, Body: t}}
        }
      }).(func(any) any)(body).(func(any) any)(liblists.Reverse(vars))
    }()
  }().(core.Type)
}

func UnshadowVariables (term0 core.Term) core.Term {
  return func () any {
    var freshName func(core.Name) any
    freshName = func (base core.Name) any {
      return func (i int32) any {
        return func (m []any) any {
          return func () any {
            var candidate any = core.Name(libstrings.Cat2(func (v any) any {
              return v
            }(base)).(func(any) any)(libliterals.ShowInt32(i)).(string))
            return liblogic.IfElse(libmaps.Member(candidate).(func(any) any)(m)).(func(any) any)(freshName(base).(func(any) any)(libmath.Add(i).(func(any) any)(1)).(func(any) any)(m)).(func(any) any)(candidate)
          }()
        }
      }
    }
    return func () any {
      var f func(func([]any) func(core.Term) core.Term) any
      f = func (recurse func([]any) func(core.Term) core.Term) any {
        return func (m []any) any {
          return func (term core.Term) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.TermFunction:
                return func (fn core.Function) any {
                  return func (x any) any {
                    switch v := x.(type) {
                      case core.FunctionLambda:
                      return func (l core.Lambda) any {
                        return func () any {
                          var v any = func (v any) any {
                            return v.(core.Lambda).Parameter
                          }(l)
                          return func () any {
                            var domain any = func (v any) any {
                              return v.(core.Lambda).Domain
                            }(l)
                            return func () any {
                              var body any = func (v any) any {
                                return v.(core.Lambda).Body
                              }(l)
                              return liblogic.IfElse(libmaps.Member(v).(func(any) any)(m)).(func(any) any)(func () any {
                                var v2 any = freshName(v.(core.Name)).(func(any) any)(2).(func(any) any)(m)
                                return func () any {
                                  var m2 any = libmaps.Insert(v).(func(any) any)(v2).(func(any) any)(libmaps.Insert(v2).(func(any) any)(v2).(func(any) any)(m))
                                  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: v2.(core.Name), Domain: domain, Body: f(recurse)(m2)(body)}}}
                                }()
                              }()).(func(any) any)(core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: v.(core.Name), Domain: domain, Body: f(recurse)(libmaps.Insert(v).(func(any) any)(v).(func(any) any)(m))(body)}}})
                            }()
                          }()
                        }()
                      }(v.Value)
                      default:
                      return recurse(m)(term)
                    }
                    return nil
                  }(fn)
                }(v.Value)
                case core.TermLet:
                return func (lt core.Let) any {
                  return func () any {
                    var m2 any = liblists.Foldl(func (acc []any) any {
                      return func (b core.Binding) any {
                        return func () any {
                          var bname any = func (v any) any {
                            return v.(core.Binding).Name
                          }(b)
                          return liblogic.IfElse(libmaps.Member(bname).(func(any) any)(acc)).(func(any) any)(acc).(func(any) any)(libmaps.Insert(bname).(func(any) any)(bname).(func(any) any)(acc))
                        }()
                      }
                    }).(func(any) any)(m).(func(any) any)(func (v any) any {
                      return v.(core.Let).Bindings
                    }(lt))
                    return recurse(m2.([]any))(term)
                  }()
                }(v.Value)
                case core.TermVariable:
                return func (v core.Name) any {
                  return core.TermVariable{Value: libmaybes.Maybe(v).(func(any) any)(func (renamed core.Name) any {
                    return renamed
                  }).(func(any) any)(libmaps.Lookup(v).(func(any) any)(m)).(core.Name)}
                }(v.Value)
                default:
                return recurse(m)(term)
              }
              return nil
            }(term)
          }
        }
      }
      return RewriteTermWithContext(f, libmaps.Empty, term0)
    }()
  }().(core.Term)
}
