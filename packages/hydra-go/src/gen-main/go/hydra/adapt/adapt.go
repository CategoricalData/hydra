// Note: this is an automatically generated file. Do not edit.

package adapt

import (
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/hoisting"
  "hydra.dev/hydra/inference"
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
  "hydra.dev/hydra/literals"
  hmodule "hydra.dev/hydra/module"
  "hydra.dev/hydra/names"
  "hydra.dev/hydra/reduction"
  "hydra.dev/hydra/reflect"
  "hydra.dev/hydra/rewriting"
  "hydra.dev/hydra/schemas"
  showcore "hydra.dev/hydra/show/core"
  showerror "hydra.dev/hydra/show/error"
  "hydra.dev/hydra/util"
  "math/big"
)

func AdaptFloatType (constraints coders.LanguageConstraints, ft core.FloatType) any {
  return func () any {
    var supported any = libsets.Member(ft).(func(any) any)(func (v any) any {
      return v.(coders.LanguageConstraints).FloatTypes
    }(constraints))
    return func () any {
      alt := func (v1 core.FloatType) any {
        return AdaptFloatType(constraints, v1)
      }
      return func () any {
        forUnsupported := func (ft2 core.FloatType) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.FloatTypeBigfloat:
              return func (_ struct{}) any {
                return alt(core.FloatTypeFloat64_{})
              }(v)
              case core.FloatTypeFloat32_:
              return func (_ struct{}) any {
                return alt(core.FloatTypeFloat64_{})
              }(v)
              case core.FloatTypeFloat64_:
              return func (_ struct{}) any {
                return alt(core.FloatTypeBigfloat{})
              }(v)
            }
            return nil
          }(ft2)
        }
        return liblogic.IfElse(supported).(func(any) any)(func () any {
          _v := ft
          return &_v
        }()).(func(any) any)(forUnsupported(ft))
      }()
    }()
  }()
}

func AdaptDataGraph (constraints coders.LanguageConstraints, doExpand bool, els0 []any, cx context.Context, graph0 graph.Graph) any {
  return func () any {
    transform := func (g graph.Graph) any {
      return func (gterm core.Term) any {
        return func () any {
          var tx any = g
          return func () any {
            var gterm1 any = rewriting.UnshadowVariables(PushTypeAppsInward(gterm))
            return func () any {
              var gterm2 any = rewriting.UnshadowVariables(liblogic.IfElse(doExpand).(func(any) any)(PushTypeAppsInward(reduction.EtaExpandTermNew(tx.(graph.Graph), gterm1.(core.Term)))).(func(any) any)(gterm1).(core.Term))
              return rewriting.LiftLambdaAboveLet(gterm2.(core.Term))
            }()
          }()
        }()
      }
    }
    return func () any {
      var litmap any = AdaptLiteralTypesMap(constraints)
      return func () any {
        var prims0 any = func (v any) any {
          return v.(graph.Graph).Primitives
        }(graph0)
        return func () any {
          var schemaTypes0 any = func (v any) any {
            return v.(graph.Graph).SchemaTypes
          }(graph0)
          return func () any {
            var schemaBindings any = schemas.TypesToElements(libmaps.Map(func (ts core.TypeScheme) any {
              return rewriting.TypeSchemeToFType(ts)
            }).(func(any) any)(schemaTypes0).([]any))
            return libeithers.Bind(liblogic.IfElse(libmaps.Null(schemaTypes0)).(func(any) any)([2]any{"right", libmaps.Empty}).(func(any) any)(libeithers.Bind(libeithers.Bimap(func (ic context.InContext[error.DecodingError]) any {
              return func (v any) any {
                return v.(context.InContext[error.DecodingError]).Object
              }(ic)
            }).(func(any) any)(func (x []any) any {
              return x
            }).(func(any) any)(schemas.GraphAsTypes(cx, graph0, schemaBindings.([]any)))).(func(any) any)(func (tmap0 []any) any {
              return libeithers.Bind(AdaptGraphSchema(constraints, litmap.([]any), tmap0)).(func(any) any)(func (tmap1 []any) any {
                return [2]any{"right", libmaps.Map(func (t core.Type) any {
                  return schemas.TypeToTypeScheme(t)
                }).(func(any) any)(tmap1)}
              })
            }))).(func(any) any)(func (schemaResult []any) any {
              return func () any {
                var adaptedSchemaTypes any = schemaResult
                return func () any {
                  var gterm0 any = core.TermLet{Value: core.Let{Bindings: els0, Body: core.TermUnit{}}}
                  return func () any {
                    var gterm1 any = liblogic.IfElse(doExpand).(func(any) any)(transform(graph0).(func(any) any)(gterm0)).(func(any) any)(gterm0)
                    return libeithers.Bind(AdaptTerm(constraints, litmap.([]any), cx, graph0, gterm1.(core.Term))).(func(any) any)(func (gterm2 core.Term) any {
                      return libeithers.Bind(rewriting.RewriteTermM(func (_p func(core.Term) any) func(core.Term) any {
                        return func (v1 func(core.Term) any) func(core.Term) any {
                          return func (v2 core.Term) any {
                            return AdaptLambdaDomains[core.Term](constraints, litmap.([]any), v1, v2)
                          }
                        }(_p).(func(core.Term) any)
                      }, gterm2)).(func(any) any)(func (gterm3 core.Term) any {
                        return func () any {
                          var els1Raw any = schemas.TermAsBindings(gterm3)
                          return func () any {
                            processBinding := func (el core.Binding) any {
                              return libeithers.Bind(rewriting.RewriteTermM(func (_p func(core.Term) any) func(core.Term) any {
                                return func (v1 func(core.Term) any) func(core.Term) any {
                                  return func (v2 core.Term) any {
                                    return AdaptNestedTypes[core.Term](constraints, litmap.([]any), v1, v2)
                                  }
                                }(_p).(func(core.Term) any)
                              }, func (v any) any {
                                return v.(core.Binding).Term
                              }(el).(core.Term))).(func(any) any)(func (newTerm core.Term) any {
                                return libeithers.Bind(libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (ts core.TypeScheme) any {
                                  return libeithers.Bind(AdaptTypeScheme(constraints, litmap.([]any), ts)).(func(any) any)(func (ts1 core.TypeScheme) any {
                                    return [2]any{"right", func () any {
                                      _v := ts1
                                      return &_v
                                    }()}
                                  })
                                }).(func(any) any)(func (v any) any {
                                  return v.(core.Binding).Type_
                                }(el))).(func(any) any)(func (adaptedType any) any {
                                  return [2]any{"right", core.Binding{Name: func (v any) any {
                                    return v.(core.Binding).Name
                                  }(el).(core.Name), Term: newTerm, Type_: adaptedType}}
                                })
                              })
                            }
                            return libeithers.Bind(libeithers.MapList(processBinding).(func(any) any)(els1Raw)).(func(any) any)(func (els1 []any) any {
                              return libeithers.Bind(libeithers.MapList(func (kv any) any {
                                return libeithers.Bind(AdaptPrimitive(constraints, litmap.([]any), libpairs.Second(kv).(graph.Primitive))).(func(any) any)(func (prim1 graph.Primitive) any {
                                  return [2]any{"right", [2]any{libpairs.First(kv), prim1}}
                                })
                              }).(func(any) any)(libmaps.ToList(prims0))).(func(any) any)(func (primPairs []any) any {
                                return func () any {
                                  var prims1 any = libmaps.FromList(primPairs)
                                  return func () any {
                                    var adaptedGraph any = graph.Graph{BoundTerms: func (v any) any {
                                      return v.(graph.Graph).BoundTerms
                                    }(lexical.BuildGraph(els1, libmaps.Empty, prims1.([]any))).([]any), BoundTypes: func (v any) any {
                                      return v.(graph.Graph).BoundTypes
                                    }(lexical.BuildGraph(els1, libmaps.Empty, prims1.([]any))).([]any), ClassConstraints: func (v any) any {
                                      return v.(graph.Graph).ClassConstraints
                                    }(lexical.BuildGraph(els1, libmaps.Empty, prims1.([]any))).([]any), LambdaVariables: func (v any) any {
                                      return v.(graph.Graph).LambdaVariables
                                    }(lexical.BuildGraph(els1, libmaps.Empty, prims1.([]any))).([]any), Metadata: func (v any) any {
                                      return v.(graph.Graph).Metadata
                                    }(lexical.BuildGraph(els1, libmaps.Empty, prims1.([]any))).([]any), Primitives: func (v any) any {
                                      return v.(graph.Graph).Primitives
                                    }(lexical.BuildGraph(els1, libmaps.Empty, prims1.([]any))).([]any), SchemaTypes: adaptedSchemaTypes.([]any), TypeVariables: func (v any) any {
                                      return v.(graph.Graph).TypeVariables
                                    }(lexical.BuildGraph(els1, libmaps.Empty, prims1.([]any))).([]any)}
                                    return [2]any{"right", [2]any{adaptedGraph, els1}}
                                  }()
                                }()
                              })
                            })
                          }()
                        }()
                      })
                    })
                  }()
                }()
              }()
            })
          }()
        }()
      }()
    }()
  }()
}

func AdaptGraphSchema (constraints coders.LanguageConstraints, litmap []any, types0 []any) any {
  return func () any {
    mapPair := func (pair any) any {
      return func () any {
        var name any = libpairs.First(pair)
        return func () any {
          var typ any = libpairs.Second(pair)
          return libeithers.Bind(AdaptType(constraints, litmap, typ.(core.Type))).(func(any) any)(func (typ1 core.Type) any {
            return [2]any{"right", [2]any{name, typ1}}
          })
        }()
      }()
    }
    return libeithers.Bind(libeithers.MapList(mapPair).(func(any) any)(libmaps.ToList(types0))).(func(any) any)(func (pairs []any) any {
      return [2]any{"right", libmaps.FromList(pairs)}
    })
  }()
}

func AdaptIntegerType (constraints coders.LanguageConstraints, it core.IntegerType) any {
  return func () any {
    var supported any = libsets.Member(it).(func(any) any)(func (v any) any {
      return v.(coders.LanguageConstraints).IntegerTypes
    }(constraints))
    return func () any {
      alt := func (v1 core.IntegerType) any {
        return AdaptIntegerType(constraints, v1)
      }
      return func () any {
        forUnsupported := func (it2 core.IntegerType) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.IntegerTypeBigint:
              return func (_ struct{}) any {
                return nil
              }(v)
              case core.IntegerTypeInt8_:
              return func (_ struct{}) any {
                return alt(core.IntegerTypeUint16_{})
              }(v)
              case core.IntegerTypeInt16_:
              return func (_ struct{}) any {
                return alt(core.IntegerTypeUint32_{})
              }(v)
              case core.IntegerTypeInt32_:
              return func (_ struct{}) any {
                return alt(core.IntegerTypeUint64_{})
              }(v)
              case core.IntegerTypeInt64_:
              return func (_ struct{}) any {
                return alt(core.IntegerTypeBigint{})
              }(v)
              case core.IntegerTypeUint8_:
              return func (_ struct{}) any {
                return alt(core.IntegerTypeInt16_{})
              }(v)
              case core.IntegerTypeUint16_:
              return func (_ struct{}) any {
                return alt(core.IntegerTypeInt32_{})
              }(v)
              case core.IntegerTypeUint32_:
              return func (_ struct{}) any {
                return alt(core.IntegerTypeInt64_{})
              }(v)
              case core.IntegerTypeUint64_:
              return func (_ struct{}) any {
                return alt(core.IntegerTypeBigint{})
              }(v)
            }
            return nil
          }(it2)
        }
        return liblogic.IfElse(supported).(func(any) any)(func () any {
          _v := it
          return &_v
        }()).(func(any) any)(forUnsupported(it))
      }()
    }()
  }()
}

func AdaptLambdaDomains[T0 any] (constraints coders.LanguageConstraints, litmap []any, recurse func(T0) any, term T0) any {
  return libeithers.Bind(recurse(term)).(func(any) any)(func (rewritten core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermFunction:
        return func (f core.Function) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.FunctionLambda:
              return func (l core.Lambda) any {
                return libeithers.Bind(libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (dom core.Type) any {
                  return libeithers.Bind(AdaptType(constraints, litmap, dom)).(func(any) any)(func (dom1 core.Type) any {
                    return [2]any{"right", func () any {
                      _v := dom1
                      return &_v
                    }()}
                  })
                }).(func(any) any)(func (v any) any {
                  return v.(core.Lambda).Domain
                }(l))).(func(any) any)(func (adaptedDomain any) any {
                  return [2]any{"right", core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
                    return v.(core.Lambda).Parameter
                  }(l).(core.Name), Domain: adaptedDomain, Body: func (v any) any {
                    return v.(core.Lambda).Body
                  }(l).(core.Term)}}}}
                })
              }(v.Value)
              default:
              return [2]any{"right", core.TermFunction{Value: f}}
            }
            return nil
          }(f)
        }(v.Value)
        default:
        return [2]any{"right", rewritten}
      }
      return nil
    }(rewritten)
  })
}

func AdaptLiteral (lt core.LiteralType, l core.Literal) core.Literal {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralBinary:
      return func (b []byte) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.LiteralTypeString_:
            return func (_ struct{}) any {
              return core.LiteralString_{Value: libliterals.BinaryToString(b).(string)}
            }(v)
          }
          return nil
        }(lt)
      }(v.Value)
      case core.LiteralBoolean:
      return func (b bool) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.LiteralTypeInteger:
            return func (it core.IntegerType) any {
              return core.LiteralInteger{Value: literals.BigintToIntegerValue(it, liblogic.IfElse(b).(func(any) any)(big.NewInt(1)).(func(any) any)(big.NewInt(0)).(*big.Int))}
            }(v.Value)
          }
          return nil
        }(lt)
      }(v.Value)
      case core.LiteralFloat:
      return func (f core.FloatValue) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.LiteralTypeFloat:
            return func (ft core.FloatType) any {
              return core.LiteralFloat{Value: literals.BigfloatToFloatValue(ft, literals.FloatValueToBigfloat(f))}
            }(v.Value)
          }
          return nil
        }(lt)
      }(v.Value)
      case core.LiteralInteger:
      return func (i core.IntegerValue) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.LiteralTypeInteger:
            return func (it core.IntegerType) any {
              return core.LiteralInteger{Value: literals.BigintToIntegerValue(it, literals.IntegerValueToBigint(i))}
            }(v.Value)
          }
          return nil
        }(lt)
      }(v.Value)
    }
    return nil
  }(l).(core.Literal)
}

func AdaptLiteralType (constraints coders.LanguageConstraints, lt core.LiteralType) any {
  return func () any {
    forUnsupported := func (lt2 core.LiteralType) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.LiteralTypeBinary:
          return func (_ struct{}) any {
            return func () any {
              _v := core.LiteralTypeString_{}
              return &_v
            }()
          }(v)
          case core.LiteralTypeBoolean:
          return func (_ struct{}) any {
            return libmaybes.Map(func (x core.IntegerType) any {
              return core.LiteralTypeInteger{Value: x}
            }).(func(any) any)(AdaptIntegerType(constraints, core.IntegerTypeInt8_{}))
          }(v)
          case core.LiteralTypeFloat:
          return func (ft core.FloatType) any {
            return libmaybes.Map(func (x core.FloatType) any {
              return core.LiteralTypeFloat{Value: x}
            }).(func(any) any)(AdaptFloatType(constraints, ft))
          }(v.Value)
          case core.LiteralTypeInteger:
          return func (it core.IntegerType) any {
            return libmaybes.Map(func (x core.IntegerType) any {
              return core.LiteralTypeInteger{Value: x}
            }).(func(any) any)(AdaptIntegerType(constraints, it))
          }(v.Value)
          default:
          return nil
        }
        return nil
      }(lt2)
    }
    return liblogic.IfElse(LiteralTypeSupported(constraints, lt)).(func(any) any)(nil).(func(any) any)(forUnsupported(lt))
  }()
}

func AdaptLiteralTypesMap (constraints coders.LanguageConstraints) []any {
  return func () any {
    tryType := func (lt core.LiteralType) any {
      return libmaybes.Maybe(nil).(func(any) any)(func (lt2 core.LiteralType) any {
        return func () any {
          _v := [2]any{lt, lt2}
          return &_v
        }()
      }).(func(any) any)(AdaptLiteralType(constraints, lt))
    }
    return libmaps.FromList(libmaybes.Cat(liblists.Map(tryType).(func(any) any)(reflect.LiteralTypes)))
  }().([]any)
}

func AdaptLiteralValue[T0 any] (litmap []any, lt T0, l core.Literal) core.Literal {
  return libmaybes.Maybe(core.LiteralString_{Value: showcore.Literal(l)}).(func(any) any)(func (lt2 core.LiteralType) any {
    return AdaptLiteral(lt2, l)
  }).(func(any) any)(libmaps.Lookup(lt).(func(any) any)(litmap)).(core.Literal)
}

func AdaptNestedTypes[T0 any] (constraints coders.LanguageConstraints, litmap []any, recurse func(T0) any, term T0) any {
  return libeithers.Bind(recurse(term)).(func(any) any)(func (rewritten core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermLet:
        return func (lt core.Let) any {
          return func () any {
            adaptB := func (b core.Binding) any {
              return libeithers.Bind(libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (ts core.TypeScheme) any {
                return libeithers.Bind(AdaptTypeScheme(constraints, litmap, ts)).(func(any) any)(func (ts1 core.TypeScheme) any {
                  return [2]any{"right", func () any {
                    _v := ts1
                    return &_v
                  }()}
                })
              }).(func(any) any)(func (v any) any {
                return v.(core.Binding).Type_
              }(b))).(func(any) any)(func (adaptedBType any) any {
                return [2]any{"right", core.Binding{Name: func (v any) any {
                  return v.(core.Binding).Name
                }(b).(core.Name), Term: func (v any) any {
                  return v.(core.Binding).Term
                }(b).(core.Term), Type_: adaptedBType}}
              })
            }
            return libeithers.Bind(libeithers.MapList(adaptB).(func(any) any)(func (v any) any {
              return v.(core.Let).Bindings
            }(lt))).(func(any) any)(func (adaptedBindings []any) any {
              return [2]any{"right", core.TermLet{Value: core.Let{Bindings: adaptedBindings, Body: func (v any) any {
                return v.(core.Let).Body
              }(lt).(core.Term)}}}
            })
          }()
        }(v.Value)
        default:
        return [2]any{"right", rewritten}
      }
      return nil
    }(rewritten)
  })
}

func AdaptPrimitive (constraints coders.LanguageConstraints, litmap []any, prim0 graph.Primitive) any {
  return func () any {
    var ts0 any = func (v any) any {
      return v.(graph.Primitive).Type_
    }(prim0)
    return libeithers.Bind(AdaptTypeScheme(constraints, litmap, ts0.(core.TypeScheme))).(func(any) any)(func (ts1 core.TypeScheme) any {
      return [2]any{"right", graph.Primitive{Name: func (v any) any {
        return v.(graph.Primitive).Name
      }(prim0).(core.Name), Type_: ts1, Implementation: func (_p context.Context) func(graph.Graph) func([]any) any {
        return func (v any) any {
          return v.(graph.Primitive).Implementation
        }(prim0)(_p).(func(graph.Graph) func([]any) any)
      }}}
    })
  }()
}

func AdaptTerm (constraints coders.LanguageConstraints, litmap []any, cx context.Context, graph graph.Graph, term0 core.Term) any {
  return func () any {
    rewrite := func (recurse func(core.Term) any) any {
      return func (term02 core.Term) any {
        return func () any {
          forSupported := func (term core.Term) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.TermLiteral:
                return func (l core.Literal) any {
                  return func () any {
                    var lt any = reflect.LiteralType(l)
                    return [2]any{"right", func () any {
                      _v := liblogic.IfElse(LiteralTypeSupported(constraints, lt.(core.LiteralType))).(func(any) any)(term).(func(any) any)(core.TermLiteral{Value: AdaptLiteralValue[core.LiteralType](litmap, lt, l)})
                      return &_v
                    }()}
                  }()
                }(v.Value)
                default:
                return [2]any{"right", func () any {
                  _v := term
                  return &_v
                }()}
              }
              return nil
            }(term)
          }
          forUnsupported := func (term core.Term) any {
            return func () any {
              forNonNull := func (alts []any) any {
                return libeithers.Bind(tryTerm(liblists.Head(alts))).(func(any) any)(func (mterm any) any {
                  return libmaybes.Maybe(tryAlts(liblists.Tail(alts))).(func(any) any)(func (t core.Term) any {
                    return [2]any{"right", func () any {
                      _v := t
                      return &_v
                    }()}
                  }).(func(any) any)(mterm)
                })
              }
              tryAlts := func (alts []any) any {
                return liblogic.IfElse(liblists.Null(alts)).(func(any) any)([2]any{"right", nil}).(func(any) any)(forNonNull(alts))
              }
              return libeithers.Bind(TermAlternatives(cx, graph, term)).(func(any) any)(func (alts0 []any) any {
                return tryAlts(alts0)
              })
            }()
          }
          tryTerm := func (term core.Term) any {
            return func () any {
              var supportedVariant any = libsets.Member(reflect.TermVariant(term)).(func(any) any)(func (v any) any {
                return v.(coders.LanguageConstraints).TermVariants
              }(constraints))
              return liblogic.IfElse(supportedVariant).(func(any) any)(forSupported(term)).(func(any) any)(forUnsupported(term))
            }()
          }
          return libeithers.Bind(recurse(term02)).(func(any) any)(func (term1 core.Term) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.TermTypeApplication:
                return func (ta core.TypeApplicationTerm) any {
                  return libeithers.Bind(AdaptType(constraints, litmap, func (v any) any {
                    return v.(core.TypeApplicationTerm).Type_
                  }(ta).(core.Type))).(func(any) any)(func (atyp core.Type) any {
                    return [2]any{"right", core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: func (v any) any {
                      return v.(core.TypeApplicationTerm).Body
                    }(ta).(core.Term), Type_: atyp}}}
                  })
                }(v.Value)
                case core.TermTypeLambda:
                return func (_ core.TypeLambda) any {
                  return [2]any{"right", term1}
                }(v.Value)
                default:
                return libeithers.Bind(tryTerm(term1)).(func(any) any)(func (mterm any) any {
                  return libmaybes.Maybe([2]any{"left", libstrings.Cat2("no alternatives for term: ").(func(any) any)(showcore.Term(term1))}).(func(any) any)(func (term2 core.Term) any {
                    return [2]any{"right", term2}
                  }).(func(any) any)(mterm)
                })
              }
              return nil
            }(term1)
          })
        }()
      }
    }
    return rewriting.RewriteTermM(func (_p func(core.Term) any) func(core.Term) any {
      return rewrite(_p).(func(core.Term) any)
    }, term0)
  }()
}

func AdaptTermForLanguage (lang coders.Language, cx context.Context, g graph.Graph, term core.Term) any {
  return func () any {
    var constraints any = func (v any) any {
      return v.(coders.Language).Constraints
    }(lang)
    return func () any {
      var litmap any = AdaptLiteralTypesMap(constraints.(coders.LanguageConstraints))
      return AdaptTerm(constraints.(coders.LanguageConstraints), litmap.([]any), cx, g, term)
    }()
  }()
}

func AdaptType (constraints coders.LanguageConstraints, litmap []any, type0 core.Type) any {
  return func () any {
    forSupported := func (typ core.Type) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.TypeLiteral:
          return func (lt core.LiteralType) any {
            return liblogic.IfElse(LiteralTypeSupported(constraints, lt)).(func(any) any)(func () any {
              _v := typ
              return &_v
            }()).(func(any) any)(libmaybes.Maybe(func () any {
              _v := core.TypeLiteral{Value: core.LiteralTypeString_{}}
              return &_v
            }()).(func(any) any)(func (lt2 core.LiteralType) any {
              return func () any {
                _v := core.TypeLiteral{Value: lt2}
                return &_v
              }()
            }).(func(any) any)(libmaps.Lookup(lt).(func(any) any)(litmap)))
          }(v.Value)
          default:
          return func () any {
            _v := typ
            return &_v
          }()
        }
        return nil
      }(typ)
    }
    forUnsupported := func (typ core.Type) any {
      return func () any {
        var tryAlts func([]any) any
        tryAlts = func (alts []any) any {
          return liblogic.IfElse(liblists.Null(alts)).(func(any) any)(nil).(func(any) any)(libmaybes.Maybe(tryAlts(liblists.Tail(alts).([]any))).(func(any) any)(func (t core.Type) any {
            return func () any {
              _v := t
              return &_v
            }()
          }).(func(any) any)(tryType(liblists.Head(alts).(core.LiteralType))))
        }
        return func () any {
          var alts0 any = TypeAlternatives(typ)
          return tryAlts(alts0.([]any))
        }()
      }()
    }
    tryType := func (typ core.Type) any {
      return func () any {
        var supportedVariant any = libsets.Member(reflect.TypeVariant(typ)).(func(any) any)(func (v any) any {
          return v.(coders.LanguageConstraints).TypeVariants
        }(constraints))
        return liblogic.IfElse(supportedVariant).(func(any) any)(forSupported(typ)).(func(any) any)(forUnsupported(typ))
      }()
    }
    return func () any {
      rewrite := func (recurse func(core.Type) any) any {
        return func (typ core.Type) any {
          return libeithers.Bind(recurse(typ)).(func(any) any)(func (type1 core.Type) any {
            return libmaybes.Maybe([2]any{"left", libstrings.Cat2("no alternatives for type: ").(func(any) any)(showcore.Type_(typ))}).(func(any) any)(func (type2 core.Type) any {
              return [2]any{"right", type2}
            }).(func(any) any)(tryType(type1))
          })
        }
      }
      return rewriting.RewriteTypeM(func (_p func(core.Type) any) func(core.Type) any {
        return rewrite(_p).(func(core.Type) any)
      }, type0)
    }()
  }()
}

func AdaptTypeForLanguage (lang coders.Language, typ core.Type) any {
  return func () any {
    var constraints any = func (v any) any {
      return v.(coders.Language).Constraints
    }(lang)
    return func () any {
      var litmap any = AdaptLiteralTypesMap(constraints.(coders.LanguageConstraints))
      return AdaptType(constraints.(coders.LanguageConstraints), litmap.([]any), typ)
    }()
  }()
}

func AdaptTypeScheme (constraints coders.LanguageConstraints, litmap []any, ts0 core.TypeScheme) any {
  return func () any {
    var vars0 any = func (v any) any {
      return v.(core.TypeScheme).Variables
    }(ts0)
    return func () any {
      var t0 any = func (v any) any {
        return v.(core.TypeScheme).Type_
      }(ts0)
      return libeithers.Bind(AdaptType(constraints, litmap, t0.(core.Type))).(func(any) any)(func (t1 core.Type) any {
        return [2]any{"right", core.TypeScheme{Variables: vars0.([]any), Type_: t1, Constraints: func (v any) any {
          return v.(core.TypeScheme).Constraints
        }(ts0)}}
      })
    }()
  }()
}

func ComposeCoders[T0, T1, T2 any] (c1 util.Coder[T0, T1], c2 util.Coder[T1, T2]) util.Coder[T0, T2] {
  return util.Coder[T0, T2]{Encode: func (_p context.Context) func(T0) any {
    return func (cx context.Context) any {
      return func (a T0) any {
        return libeithers.Bind(func (v any) any {
          return v.(util.Coder[T0, T1]).Encode
        }(c1).(func(any) any)(cx).(func(any) any)(a)).(func(any) any)(func (b1 T1) any {
          return func (v any) any {
            return v.(util.Coder[T1, T2]).Encode
          }(c2).(func(any) any)(cx).(func(any) any)(b1)
        })
      }
    }(_p).(func(T0) any)
  }, Decode: func (_p context.Context) func(T2) any {
    return func (cx context.Context) any {
      return func (c T2) any {
        return libeithers.Bind(func (v any) any {
          return v.(util.Coder[T1, T2]).Decode
        }(c2).(func(any) any)(cx).(func(any) any)(c)).(func(any) any)(func (b2 T1) any {
          return func (v any) any {
            return v.(util.Coder[T0, T1]).Decode
          }(c1).(func(any) any)(cx).(func(any) any)(b2)
        })
      }
    }(_p).(func(T2) any)
  }}
}

func DataGraphToDefinitions (constraints coders.LanguageConstraints, doInfer bool, doExpand bool, doHoistCaseStatements bool, doHoistPolymorphicLetBindings bool, originalBindings []any, graph0 graph.Graph, namespaces []any, cx context.Context) any {
  return func () any {
    var namespacesSet any = libsets.FromList(namespaces)
    return func () any {
      isParentBinding := func (b core.Binding) any {
        return libmaybes.Maybe(false).(func(any) any)(func (ns hmodule.Namespace) any {
          return libsets.Member(ns).(func(any) any)(namespacesSet)
        }).(func(any) any)(names.NamespaceOf(func (v any) any {
          return v.(core.Binding).Name
        }(b).(core.Name)))
      }
      return func () any {
        hoistCases := func (bindings []any) any {
          return func () any {
            var stripped any = liblists.Map(func (b core.Binding) any {
              return core.Binding{Name: func (v any) any {
                return v.(core.Binding).Name
              }(b).(core.Name), Term: rewriting.StripTypeLambdas(func (v any) any {
                return v.(core.Binding).Term
              }(b).(core.Term)), Type_: func (v any) any {
                return v.(core.Binding).Type_
              }(b)}
            }).(func(any) any)(bindings)
            return func () any {
              var term0 any = core.TermLet{Value: core.Let{Bindings: stripped.([]any), Body: core.TermUnit{}}}
              return func () any {
                var unshadowed0 any = schemas.TermAsBindings(rewriting.UnshadowVariables(term0.(core.Term)))
                return func () any {
                  var hoisted any = hoisting.HoistCaseStatementsInGraph(unshadowed0.([]any))
                  return func () any {
                    var term1 any = core.TermLet{Value: core.Let{Bindings: hoisted.([]any), Body: core.TermUnit{}}}
                    return schemas.TermAsBindings(rewriting.UnshadowVariables(term1.(core.Term)))
                  }()
                }()
              }()
            }()
          }()
        }
        return func () any {
          hoistPoly := func (bindings []any) any {
            return func () any {
              var letBefore any = core.Let{Bindings: bindings, Body: core.TermUnit{}}
              return func () any {
                var letAfter any = hoisting.HoistPolymorphicLetBindings(func (_p core.Binding) bool {
                  return isParentBinding(_p).(bool)
                }, letBefore.(core.Let))
                return letAfter.(core.Let).Bindings
              }()
            }()
          }
          return func () any {
            checkBindingsTyped := func (debugLabel string) any {
              return func (bindings []any) any {
                return func () any {
                  var untypedBindings any = liblists.Map(func (b core.Binding) any {
                    return func (v any) any {
                      return v.(core.Binding).Name
                    }(b)
                  }).(func(any) any)(liblists.Filter(func (b core.Binding) any {
                    return liblogic.Not(libmaybes.IsJust(func (v any) any {
                      return v.(core.Binding).Type_
                    }(b)))
                  }).(func(any) any)(bindings))
                  return liblogic.IfElse(liblists.Null(untypedBindings)).(func(any) any)([2]any{"right", bindings}).(func(any) any)([2]any{"left", libstrings.Cat([]any{"Found untyped bindings (", debugLabel, "): ", libstrings.Intercalate(", ").(func(any) any)(untypedBindings)})})
                }()
              }
            }
            return func () any {
              normalizeBindings := func (bindings []any) any {
                return liblists.Map(func (b core.Binding) any {
                  return core.Binding{Name: func (v any) any {
                    return v.(core.Binding).Name
                  }(b).(core.Name), Term: PushTypeAppsInward(func (v any) any {
                    return v.(core.Binding).Term
                  }(b).(core.Term)), Type_: func (v any) any {
                    return v.(core.Binding).Type_
                  }(b)}
                }).(func(any) any)(bindings)
              }
              return func () any {
                rebuildGraph := func (bindings []any) any {
                  return func () any {
                    var g any = lexical.BuildGraph(bindings, libmaps.Empty, func (v any) any {
                      return v.(graph.Graph).Primitives
                    }(graph0).([]any))
                    return graph.Graph{BoundTerms: g.(graph.Graph).BoundTerms, BoundTypes: g.(graph.Graph).BoundTypes, ClassConstraints: g.(graph.Graph).ClassConstraints, LambdaVariables: g.(graph.Graph).LambdaVariables, Metadata: g.(graph.Graph).Metadata, Primitives: g.(graph.Graph).Primitives, SchemaTypes: func (v any) any {
                      return v.(graph.Graph).SchemaTypes
                    }(graph0).([]any), TypeVariables: g.(graph.Graph).TypeVariables}
                  }()
                }
                return func () any {
                  var bins0 any = originalBindings
                  return func () any {
                    var bins1 any = liblogic.IfElse(doHoistCaseStatements).(func(any) any)(hoistCases(bins0.([]any))).(func(any) any)(bins0)
                    return libeithers.Bind(liblogic.IfElse(doInfer).(func(any) any)(libeithers.Map(func (result any) any {
                      return libpairs.Second(libpairs.First(result))
                    }).(func(any) any)(libeithers.Bimap(func (ic context.InContext[error.Error]) any {
                      return showerror.Error_(func (v any) any {
                        return v.(context.InContext[error.Error]).Object
                      }(ic).(error.Error))
                    }).(func(any) any)(func (x any) any {
                      return x
                    }).(func(any) any)(inference.InferGraphTypes(cx, bins1.([]any), rebuildGraph(bins1.([]any)).(graph.Graph))))).(func(any) any)(checkBindingsTyped("after case hoisting").(func(any) any)(bins1))).(func(any) any)(func (bins2 []any) any {
                      return libeithers.Bind(liblogic.IfElse(doHoistPolymorphicLetBindings).(func(any) any)(checkBindingsTyped("after let hoisting").(func(any) any)(hoistPoly(bins2))).(func(any) any)([2]any{"right", bins2})).(func(any) any)(func (bins3 []any) any {
                        return libeithers.Bind(AdaptDataGraph(constraints, doExpand, bins3, cx, rebuildGraph(bins3).(graph.Graph))).(func(any) any)(func (adaptResult any) any {
                          return func () any {
                            var adapted any = libpairs.First(adaptResult)
                            return func () any {
                              var adaptedBindings any = libpairs.Second(adaptResult)
                              return libeithers.Bind(checkBindingsTyped("after adaptation").(func(any) any)(adaptedBindings)).(func(any) any)(func (bins4 []any) any {
                                return func () any {
                                  var bins5 any = normalizeBindings(bins4)
                                  return func () any {
                                    toDef := func (el core.Binding) any {
                                      return libmaybes.Map(func (ts core.TypeScheme) any {
                                        return hmodule.TermDefinition{Name: func (v any) any {
                                          return v.(core.Binding).Name
                                        }(el).(core.Name), Term: func (v any) any {
                                          return v.(core.Binding).Term
                                        }(el).(core.Term), Type_: ts}
                                      }).(func(any) any)(func (v any) any {
                                        return v.(core.Binding).Type_
                                      }(el))
                                    }
                                    return func () any {
                                      var selectedElements any = liblists.Filter(func (el core.Binding) any {
                                        return libmaybes.Maybe(false).(func(any) any)(func (ns hmodule.Namespace) any {
                                          return libsets.Member(ns).(func(any) any)(namespacesSet)
                                        }).(func(any) any)(names.NamespaceOf(func (v any) any {
                                          return v.(core.Binding).Name
                                        }(el).(core.Name)))
                                      }).(func(any) any)(bins5)
                                      return func () any {
                                        var elementsByNamespace any = liblists.Foldl(func (acc []any) any {
                                          return func (el core.Binding) any {
                                            return libmaybes.Maybe(acc).(func(any) any)(func (ns hmodule.Namespace) any {
                                              return func () any {
                                                var existing any = libmaybes.Maybe([]any{}).(func(any) any)(libequality.Identity).(func(any) any)(libmaps.Lookup(ns).(func(any) any)(acc))
                                                return libmaps.Insert(ns).(func(any) any)(liblists.Concat2(existing).(func(any) any)([]any{el})).(func(any) any)(acc)
                                              }()
                                            }).(func(any) any)(names.NamespaceOf(func (v any) any {
                                              return v.(core.Binding).Name
                                            }(el).(core.Name)))
                                          }
                                        }).(func(any) any)(libmaps.Empty).(func(any) any)(selectedElements)
                                        return func () any {
                                          var defsGrouped any = liblists.Map(func (ns hmodule.Namespace) any {
                                            return func () any {
                                              var elsForNs any = libmaybes.Maybe([]any{}).(func(any) any)(libequality.Identity).(func(any) any)(libmaps.Lookup(ns).(func(any) any)(elementsByNamespace))
                                              return libmaybes.Cat(liblists.Map(toDef).(func(any) any)(elsForNs))
                                            }()
                                          }).(func(any) any)(namespaces)
                                          return func () any {
                                            var g any = lexical.BuildGraph(bins5.([]any), libmaps.Empty, adapted.(graph.Graph).Primitives)
                                            return [2]any{"right", [2]any{graph.Graph{BoundTerms: g.(graph.Graph).BoundTerms, BoundTypes: g.(graph.Graph).BoundTypes, ClassConstraints: g.(graph.Graph).ClassConstraints, LambdaVariables: g.(graph.Graph).LambdaVariables, Metadata: g.(graph.Graph).Metadata, Primitives: g.(graph.Graph).Primitives, SchemaTypes: adapted.(graph.Graph).SchemaTypes, TypeVariables: g.(graph.Graph).TypeVariables}, defsGrouped}}
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

func LiteralTypeSupported (constraints coders.LanguageConstraints, lt core.LiteralType) bool {
  return func () any {
    forType := func (lt2 core.LiteralType) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.LiteralTypeFloat:
          return func (ft core.FloatType) any {
            return libsets.Member(ft).(func(any) any)(func (v any) any {
              return v.(coders.LanguageConstraints).FloatTypes
            }(constraints))
          }(v.Value)
          case core.LiteralTypeInteger:
          return func (it core.IntegerType) any {
            return libsets.Member(it).(func(any) any)(func (v any) any {
              return v.(coders.LanguageConstraints).IntegerTypes
            }(constraints))
          }(v.Value)
          default:
          return true
        }
        return nil
      }(lt2)
    }
    return liblogic.IfElse(libsets.Member(reflect.LiteralTypeVariant(lt)).(func(any) any)(func (v any) any {
      return v.(coders.LanguageConstraints).LiteralVariants
    }(constraints))).(func(any) any)(forType(lt)).(func(any) any)(false)
  }().(bool)
}

func PushTypeAppsInward (term core.Term) core.Term {
  return func () any {
    push := func (body core.Term) any {
      return func (typ core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TermApplication:
            return func (a core.Application) any {
              return go_(core.TermApplication{Value: core.Application{Function: core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: func (v any) any {
                return v.(core.Application).Function
              }(a).(core.Term), Type_: typ}}, Argument: func (v any) any {
                return v.(core.Application).Argument
              }(a).(core.Term)}})
            }(v.Value)
            case core.TermFunction:
            return func (f core.Function) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.FunctionLambda:
                  return func (l core.Lambda) any {
                    return go_(core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: func (v any) any {
                      return v.(core.Lambda).Parameter
                    }(l).(core.Name), Domain: func (v any) any {
                      return v.(core.Lambda).Domain
                    }(l), Body: core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: func (v any) any {
                      return v.(core.Lambda).Body
                    }(l).(core.Term), Type_: typ}}}}})
                  }(v.Value)
                  default:
                  return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: core.TermFunction{Value: f}, Type_: typ}}
                }
                return nil
              }(f)
            }(v.Value)
            case core.TermLet:
            return func (lt core.Let) any {
              return go_(core.TermLet{Value: core.Let{Bindings: func (v any) any {
                return v.(core.Let).Bindings
              }(lt).([]any), Body: core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: func (v any) any {
                return v.(core.Let).Body
              }(lt).(core.Term), Type_: typ}}}})
            }(v.Value)
            default:
            return core.TermTypeApplication{Value: core.TypeApplicationTerm{Body: body, Type_: typ}}
          }
          return nil
        }(body)
      }
    }
    var go_ func(core.Term) any
    go_ = func (t core.Term) any {
      return func () any {
        forField := func (fld core.Field) any {
          return core.Field{Name: func (v any) any {
            return v.(core.Field).Name
          }(fld).(core.Name), Term: go_(func (v any) any {
            return v.(core.Field).Term
          }(fld).(core.Term)).(core.Term)}
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
                  }(cs).(core.Name), Default_: libmaybes.Map(go_).(func(any) any)(func (v any) any {
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
                    }(l), Body: go_(func (v any) any {
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
                    }(b).(core.Name), Term: go_(func (v any) any {
                      return v.(core.Binding).Term
                    }(b).(core.Term)).(core.Term), Type_: func (v any) any {
                      return v.(core.Binding).Type_
                    }(b)}
                  }
                  return core.Let{Bindings: liblists.Map(mapBinding).(func(any) any)(func (v any) any {
                    return v.(core.Let).Bindings
                  }(lt)).([]any), Body: go_(func (v any) any {
                    return v.(core.Let).Body
                  }(lt).(core.Term)).(core.Term)}
                }()
              }
              return func () any {
                forMap := func (m []any) any {
                  return func () any {
                    forPair := func (p any) any {
                      return [2]any{go_(libpairs.First(p).(core.Term)), go_(libpairs.Second(p).(core.Term))}
                    }
                    return libmaps.FromList(liblists.Map(forPair).(func(any) any)(libmaps.ToList(m)))
                  }()
                }
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermAnnotated:
                    return func (at core.AnnotatedTerm) any {
                      return core.TermAnnotated{Value: core.AnnotatedTerm{Body: go_(func (v any) any {
                        return v.(core.AnnotatedTerm).Body
                      }(at).(core.Term)).(core.Term), Annotation: func (v any) any {
                        return v.(core.AnnotatedTerm).Annotation
                      }(at).([]any)}}
                    }(v.Value)
                    case core.TermApplication:
                    return func (a core.Application) any {
                      return core.TermApplication{Value: core.Application{Function: go_(func (v any) any {
                        return v.(core.Application).Function
                      }(a).(core.Term)).(core.Term), Argument: go_(func (v any) any {
                        return v.(core.Application).Argument
                      }(a).(core.Term)).(core.Term)}}
                    }(v.Value)
                    case core.TermEither:
                    return func (e any) any {
                      return core.TermEither{Value: libeithers.Either(func (l core.Term) any {
                        return [2]any{"left", go_(l)}
                      }).(func(any) any)(func (r core.Term) any {
                        return [2]any{"right", go_(r)}
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
                      return core.TermList{Value: liblists.Map(go_).(func(any) any)(els).([]any)}
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
                      return core.TermMaybe{Value: libmaybes.Map(go_).(func(any) any)(m)}
                    }(v.Value)
                    case core.TermPair:
                    return func (p any) any {
                      return core.TermPair{Value: [2]any{go_(libpairs.First(p).(core.Term)), go_(libpairs.Second(p).(core.Term))}}
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
                      return core.TermSet{Value: libsets.FromList(liblists.Map(go_).(func(any) any)(libsets.ToList(s))).([]any)}
                    }(v.Value)
                    case core.TermTypeApplication:
                    return func (tt core.TypeApplicationTerm) any {
                      return func () any {
                        var body1 any = go_(func (v any) any {
                          return v.(core.TypeApplicationTerm).Body
                        }(tt).(core.Term))
                        return push(body1).(func(any) any)(func (v any) any {
                          return v.(core.TypeApplicationTerm).Type_
                        }(tt))
                      }()
                    }(v.Value)
                    case core.TermTypeLambda:
                    return func (ta core.TypeLambda) any {
                      return core.TermTypeLambda{Value: core.TypeLambda{Parameter: func (v any) any {
                        return v.(core.TypeLambda).Parameter
                      }(ta).(core.Name), Body: go_(func (v any) any {
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
                      }(wt).(core.Name), Body: go_(func (v any) any {
                        return v.(core.WrappedTerm).Body
                      }(wt).(core.Term)).(core.Term)}}
                    }(v.Value)
                  }
                  return nil
                }(t)
              }()
            }()
          }()
        }()
      }()
    }
    return go_(term)
  }().(core.Term)
}

func SchemaGraphToDefinitions (constraints coders.LanguageConstraints, graph graph.Graph, nameLists []any, cx context.Context) any {
  return func () any {
    var litmap any = AdaptLiteralTypesMap(constraints)
    return libeithers.Bind(libeithers.Bimap(func (ic context.InContext[error.DecodingError]) any {
      return func (v any) any {
        return v.(context.InContext[error.DecodingError]).Object
      }(ic)
    }).(func(any) any)(func (x []any) any {
      return x
    }).(func(any) any)(schemas.GraphAsTypes(cx, graph, lexical.GraphToBindings(graph)))).(func(any) any)(func (tmap0 []any) any {
      return libeithers.Bind(AdaptGraphSchema(constraints, litmap.([]any), tmap0)).(func(any) any)(func (tmap1 []any) any {
        return func () any {
          toDef := func (pair any) any {
            return hmodule.TypeDefinition{Name: libpairs.First(pair).(core.Name), Type_: libpairs.Second(pair).(core.Type)}
          }
          return [2]any{"right", [2]any{tmap1, liblists.Map(func (names []any) any {
            return liblists.Map(toDef).(func(any) any)(liblists.Map(func (n core.Name) any {
              return [2]any{n, libmaybes.FromJust(libmaps.Lookup(n).(func(any) any)(tmap1))}
            }).(func(any) any)(names))
          }).(func(any) any)(nameLists)}}
        }()
      })
    })
  }()
}

func SimpleLanguageAdapter[T0 any] (lang coders.Language, cx T0, g graph.Graph, typ core.Type) any {
  return func () any {
    var constraints any = func (v any) any {
      return v.(coders.Language).Constraints
    }(lang)
    return func () any {
      var litmap any = AdaptLiteralTypesMap(constraints.(coders.LanguageConstraints))
      return libeithers.Bind(AdaptType(constraints.(coders.LanguageConstraints), litmap.([]any), typ)).(func(any) any)(func (adaptedType core.Type) any {
        return [2]any{"right", util.Adapter[core.Type, core.Type, core.Term, core.Term]{IsLossy: false, Source: typ, Target: adaptedType, Coder: util.Coder[core.Term, core.Term]{Encode: func (_p context.Context) func(core.Term) any {
          return func (cx2 context.Context) any {
            return func (term core.Term) any {
              return libeithers.Bimap(func (_s string) any {
                return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(_s)}, Context: cx2}
              }).(func(any) any)(func (_x core.Term) any {
                return _x
              }).(func(any) any)(AdaptTerm(constraints.(coders.LanguageConstraints), litmap.([]any), cx2, g, term))
            }
          }(_p).(func(core.Term) any)
        }, Decode: func (_p context.Context) func(core.Term) any {
          return func (cx2 context.Context) any {
            return func (term core.Term) any {
              return [2]any{"right", term}
            }
          }(_p).(func(core.Term) any)
        }}}}
      })
    }()
  }()
}

func TermAlternatives (cx context.Context, graph graph.Graph, term core.Term) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (at core.AnnotatedTerm) any {
        return func () any {
          var term2 any = func (v any) any {
            return v.(core.AnnotatedTerm).Body
          }(at)
          return [2]any{"right", []any{term2}}
        }()
      }(v.Value)
      case core.TermMaybe:
      return func (ot any) any {
        return [2]any{"right", []any{core.TermList{Value: libmaybes.Maybe([]any{}).(func(any) any)(func (term2 core.Term) any {
          return []any{term2}
        }).(func(any) any)(ot).([]any)}}}
      }(v.Value)
      case core.TermTypeLambda:
      return func (abs core.TypeLambda) any {
        return func () any {
          var term2 any = func (v any) any {
            return v.(core.TypeLambda).Body
          }(abs)
          return [2]any{"right", []any{term2}}
        }()
      }(v.Value)
      case core.TermTypeApplication:
      return func (ta core.TypeApplicationTerm) any {
        return func () any {
          var term2 any = func (v any) any {
            return v.(core.TypeApplicationTerm).Body
          }(ta)
          return [2]any{"right", []any{term2}}
        }()
      }(v.Value)
      case core.TermUnion:
      return func (inj core.Injection) any {
        return func () any {
          var tname any = func (v any) any {
            return v.(core.Injection).TypeName
          }(inj)
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            return func () any {
              var fname any = field.(core.Field).Name
              return func () any {
                var fterm any = field.(core.Field).Term
                return func () any {
                  forFieldType := func (ft core.FieldType) any {
                    return func () any {
                      var ftname any = func (v any) any {
                        return v.(core.FieldType).Name
                      }(ft)
                      return core.Field{Name: fname.(core.Name), Term: core.TermMaybe{Value: liblogic.IfElse(libequality.Equal(ftname).(func(any) any)(fname)).(func(any) any)(func () any {
                        _v := fterm
                        return &_v
                      }()).(func(any) any)(nil)}}
                    }()
                  }
                  return libeithers.Bind(libeithers.Bimap(func (ic context.InContext[error.Error]) any {
                    return showerror.Error_(func (v any) any {
                      return v.(context.InContext[error.Error]).Object
                    }(ic).(error.Error))
                  }).(func(any) any)(func (x []any) any {
                    return x
                  }).(func(any) any)(schemas.RequireUnionType(cx, graph, tname.(core.Name)))).(func(any) any)(func (rt []any) any {
                    return [2]any{"right", []any{core.TermRecord{Value: core.Record{TypeName: tname.(core.Name), Fields: liblists.Map(forFieldType).(func(any) any)(rt).([]any)}}}}
                  })
                }()
              }()
            }()
          }()
        }()
      }(v.Value)
      case core.TermUnit:
      return func (_ struct{}) any {
        return [2]any{"right", []any{core.TermLiteral{Value: core.LiteralBoolean{Value: true}}}}
      }(v)
      case core.TermWrap:
      return func (wt core.WrappedTerm) any {
        return func () any {
          var term2 any = func (v any) any {
            return v.(core.WrappedTerm).Body
          }(wt)
          return [2]any{"right", []any{term2}}
        }()
      }(v.Value)
      default:
      return [2]any{"right", []any{}}
    }
    return nil
  }(term)
}

func TypeAlternatives (type_ core.Type) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return func () any {
          var type2 any = func (v any) any {
            return v.(core.AnnotatedType).Body
          }(at)
          return []any{type2}
        }()
      }(v.Value)
      case core.TypeMaybe:
      return func (ot core.Type) any {
        return []any{core.TypeList{Value: ot}}
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return func () any {
          toOptField := func (f core.FieldType) any {
            return core.FieldType{Name: func (v any) any {
              return v.(core.FieldType).Name
            }(f).(core.Name), Type_: core.TypeMaybe{Value: func (v any) any {
              return v.(core.FieldType).Type_
            }(f).(core.Type)}}
          }
          return func () any {
            var optFields any = liblists.Map(toOptField).(func(any) any)(rt)
            return []any{core.TypeRecord{Value: optFields.([]any)}}
          }()
        }()
      }(v.Value)
      case core.TypeUnit:
      return func (_ struct{}) any {
        return []any{core.TypeLiteral{Value: core.LiteralTypeBoolean{}}}
      }(v)
      default:
      return []any{}
    }
    return nil
  }(type_).([]any)
}
