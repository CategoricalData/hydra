// Note: this is an automatically generated file. Do not edit.

package schemas

import (
  "hydra.dev/hydra/annotations"
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/constants"
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  decodecore "hydra.dev/hydra/decode/core"
  encodecore "hydra.dev/hydra/encode/core"
  "hydra.dev/hydra/error"
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
  hmodule "hydra.dev/hydra/module"
  "hydra.dev/hydra/names"
  "hydra.dev/hydra/reflect"
  "hydra.dev/hydra/rewriting"
  showcore "hydra.dev/hydra/show/core"
  "hydra.dev/hydra/sorting"
  "hydra.dev/hydra/substitution"
  "hydra.dev/hydra/typing"
  "hydra.dev/hydra/variants"
)

func AddNamesToNamespaces[T0 any] (encodeNamespace func(hmodule.Namespace) T0, names []any, ns0 hmodule.Namespaces[T0]) hmodule.Namespaces[T0] {
  return func () any {
    var nss any = libsets.FromList(libmaybes.Cat(liblists.Map(names.NamespaceOf).(func(any) any)(libsets.ToList(names))))
    return func () any {
      toPair := func (ns hmodule.Namespace) any {
        return [2]any{ns, encodeNamespace(ns)}
      }
      return hmodule.Namespaces[T0]{Focus: func (v any) any {
        return v.(hmodule.Namespaces[T0]).Focus
      }(ns0), Mapping: libmaps.Union(func (v any) any {
        return v.(hmodule.Namespaces[T0]).Mapping
      }(ns0)).(func(any) any)(libmaps.FromList(liblists.Map(toPair).(func(any) any)(libsets.ToList(nss)))).([]any)}
    }()
  }().(hmodule.Namespaces[T0])
}

func DefinitionDependencyNamespaces (defs []any) []any {
  return func () any {
    defNames := func (def hmodule.Definition) any {
      return func (x any) any {
        switch v := x.(type) {
          case hmodule.DefinitionType_:
          return func (typeDef hmodule.TypeDefinition) any {
            return rewriting.TypeDependencyNames(true, func (v any) any {
              return v.(hmodule.TypeDefinition).Type_
            }(typeDef).(core.Type))
          }(v.Value)
          case hmodule.DefinitionTerm:
          return func (termDef hmodule.TermDefinition) any {
            return rewriting.TermDependencyNames(true, true, true, func (v any) any {
              return v.(hmodule.TermDefinition).Term
            }(termDef).(core.Term))
          }(v.Value)
        }
        return nil
      }(def)
    }
    return func () any {
      var allNames any = libsets.Unions(liblists.Map(defNames).(func(any) any)(defs))
      return libsets.FromList(libmaybes.Cat(liblists.Map(names.NamespaceOf).(func(any) any)(libsets.ToList(allNames))))
    }()
  }().([]any)
}

func DependencyNamespaces (cx context.Context, graph graph.Graph, binds bool, withPrims bool, withNoms bool, withSchema bool, els []any) any {
  return func () any {
    depNames := func (el core.Binding) any {
      return func () any {
        var term any = func (v any) any {
          return v.(core.Binding).Term
        }(el)
        return func () any {
          var deannotatedTerm any = rewriting.DeannotateTerm(term.(core.Term))
          return func () any {
            var dataNames any = rewriting.TermDependencyNames(binds, withPrims, withNoms, term.(core.Term))
            return func () any {
              var schemaNames any = liblogic.IfElse(withSchema).(func(any) any)(libmaybes.Maybe(libsets.Empty).(func(any) any)(func (ts core.TypeScheme) any {
                return rewriting.TypeDependencyNames(true, func (v any) any {
                  return v.(core.TypeScheme).Type_
                }(ts).(core.Type))
              }).(func(any) any)(func (v any) any {
                return v.(core.Binding).Type_
              }(el))).(func(any) any)(libsets.Empty)
              return liblogic.IfElse(IsEncodedType(deannotatedTerm.(core.Term))).(func(any) any)(libeithers.Map(func (typ core.Type) any {
                return libsets.Unions([]any{dataNames, schemaNames, rewriting.TypeDependencyNames(true, typ)})
              }).(func(any) any)(libeithers.Bimap(func (_wc_e error.Error) any {
                return context.InContext[error.Error]{Object: _wc_e, Context: context.Context{Trace: liblists.Cons("dependency namespace (type)").(func(any) any)(func (v any) any {
                  return v.(context.Context).Trace
                }(cx)).([]any), Messages: func (v any) any {
                  return v.(context.Context).Messages
                }(cx).([]any), Other: func (v any) any {
                  return v.(context.Context).Other
                }(cx).([]any)}}
              }).(func(any) any)(func (_wc_a core.Type) any {
                return _wc_a
              }).(func(any) any)(libeithers.Bimap(func (_e error.DecodingError) any {
                return error.ErrorOther{Value: error.OtherError(func (v any) any {
                  return v
                }(_e).(string))}
              }).(func(any) any)(func (_a core.Type) any {
                return _a
              }).(func(any) any)(decodecore.Type_(graph, term.(core.Term)))))).(func(any) any)(liblogic.IfElse(IsEncodedTerm(deannotatedTerm.(core.Term))).(func(any) any)(libeithers.Map(func (decodedTerm core.Term) any {
                return libsets.Unions([]any{dataNames, schemaNames, rewriting.TermDependencyNames(binds, withPrims, withNoms, decodedTerm)})
              }).(func(any) any)(libeithers.Bimap(func (_wc_e error.Error) any {
                return context.InContext[error.Error]{Object: _wc_e, Context: context.Context{Trace: liblists.Cons("dependency namespace (term)").(func(any) any)(func (v any) any {
                  return v.(context.Context).Trace
                }(cx)).([]any), Messages: func (v any) any {
                  return v.(context.Context).Messages
                }(cx).([]any), Other: func (v any) any {
                  return v.(context.Context).Other
                }(cx).([]any)}}
              }).(func(any) any)(func (_wc_a core.Term) any {
                return _wc_a
              }).(func(any) any)(libeithers.Bimap(func (_e error.DecodingError) any {
                return error.ErrorOther{Value: error.OtherError(func (v any) any {
                  return v
                }(_e).(string))}
              }).(func(any) any)(func (_a core.Term) any {
                return _a
              }).(func(any) any)(decodecore.Term(graph, term.(core.Term)))))).(func(any) any)([2]any{"right", libsets.Unions([]any{dataNames, schemaNames})}))
            }()
          }()
        }()
      }()
    }
    return libeithers.Map(func (namesList []any) any {
      return libsets.FromList(libmaybes.Cat(liblists.Map(names.NamespaceOf).(func(any) any)(libsets.ToList(libsets.Unions(namesList)))))
    }).(func(any) any)(libeithers.MapList(depNames).(func(any) any)(els))
  }()
}

func DereferenceType (cx context.Context, graph graph.Graph, name core.Name) any {
  return func () any {
    var mel any = lexical.DereferenceElement(graph, name)
    return libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (el core.Binding) any {
      return libeithers.Map(libmaybes.Pure).(func(any) any)(libeithers.Bimap(func (_wc_e error.Error) any {
        return context.InContext[error.Error]{Object: _wc_e, Context: cx}
      }).(func(any) any)(func (_wc_a core.Type) any {
        return _wc_a
      }).(func(any) any)(libeithers.Bimap(func (_e error.DecodingError) any {
        return error.ErrorOther{Value: error.OtherError(func (v any) any {
          return v
        }(_e).(string))}
      }).(func(any) any)(func (_a core.Type) any {
        return _a
      }).(func(any) any)(decodecore.Type_(graph, func (v any) any {
        return v.(core.Binding).Term
      }(el).(core.Term)))))
    }).(func(any) any)(mel)
  }()
}

func ElementAsTypeApplicationTerm (cx context.Context, el core.Binding) any {
  return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError("missing element type")}, Context: cx}}).(func(any) any)(func (ts core.TypeScheme) any {
    return [2]any{"right", core.TypeApplicationTerm{Body: func (v any) any {
      return v.(core.Binding).Term
    }(el).(core.Term), Type_: func (v any) any {
      return v.(core.TypeScheme).Type_
    }(ts).(core.Type)}}
  }).(func(any) any)(func (v any) any {
    return v.(core.Binding).Type_
  }(el))
}

func ElementsWithDependencies (cx context.Context, graph graph.Graph, original []any) any {
  return func () any {
    depNames := func (el core.Binding) any {
      return libsets.ToList(rewriting.TermDependencyNames(true, false, false, func (v any) any {
        return v.(core.Binding).Term
      }(el).(core.Term)))
    }
    return func () any {
      var allDepNames any = liblists.Nub(liblists.Concat2(liblists.Map(func (v any) any {
        return v.(core.Binding).Name
      }).(func(any) any)(original)).(func(any) any)(liblists.Concat(liblists.Map(depNames).(func(any) any)(original))))
      return libeithers.MapList(func (name core.Name) any {
        return lexical.RequireElement(cx, graph, name)
      }).(func(any) any)(allDepNames)
    }()
  }()
}

func ExtendGraphForLambda (g graph.Graph, lam core.Lambda) graph.Graph {
  return func () any {
    var var_ any = func (v any) any {
      return v.(core.Lambda).Parameter
    }(lam)
    return graph.Graph{BoundTerms: func (v any) any {
      return v.(graph.Graph).BoundTerms
    }(g).([]any), BoundTypes: libmaybes.Maybe(func (v any) any {
      return v.(graph.Graph).BoundTypes
    }(g)).(func(any) any)(func (dom core.Type) any {
      return libmaps.Insert(var_).(func(any) any)(rewriting.FTypeToTypeScheme(dom)).(func(any) any)(func (v any) any {
        return v.(graph.Graph).BoundTypes
      }(g))
    }).(func(any) any)(func (v any) any {
      return v.(core.Lambda).Domain
    }(lam)).([]any), ClassConstraints: func (v any) any {
      return v.(graph.Graph).ClassConstraints
    }(g).([]any), LambdaVariables: libsets.Insert(var_).(func(any) any)(func (v any) any {
      return v.(graph.Graph).LambdaVariables
    }(g)).([]any), Metadata: libmaps.Delete(var_).(func(any) any)(func (v any) any {
      return v.(graph.Graph).Metadata
    }(g)).([]any), Primitives: func (v any) any {
      return v.(graph.Graph).Primitives
    }(g).([]any), SchemaTypes: func (v any) any {
      return v.(graph.Graph).SchemaTypes
    }(g).([]any), TypeVariables: func (v any) any {
      return v.(graph.Graph).TypeVariables
    }(g).([]any)}
  }().(graph.Graph)
}

func ExtendGraphForLet (forBinding func(graph.Graph) func(core.Binding) any, g graph.Graph, letrec core.Let) graph.Graph {
  return func () any {
    var bindings any = func (v any) any {
      return v.(core.Let).Bindings
    }(letrec)
    return func () any {
      var g2 any = lexical.ExtendGraphWithBindings(bindings.([]any), g)
      return graph.Graph{BoundTerms: libmaps.Union(libmaps.FromList(liblists.Map(func (b core.Binding) any {
        return [2]any{func (v any) any {
          return v.(core.Binding).Name
        }(b), func (v any) any {
          return v.(core.Binding).Term
        }(b)}
      }).(func(any) any)(bindings))).(func(any) any)(func (v any) any {
        return v.(graph.Graph).BoundTerms
      }(g)).([]any), BoundTypes: libmaps.Union(libmaps.FromList(libmaybes.Cat(liblists.Map(func (b core.Binding) any {
        return libmaybes.Map(func (ts core.TypeScheme) any {
          return [2]any{func (v any) any {
            return v.(core.Binding).Name
          }(b), ts}
        }).(func(any) any)(func (v any) any {
          return v.(core.Binding).Type_
        }(b))
      }).(func(any) any)(bindings)))).(func(any) any)(func (v any) any {
        return v.(graph.Graph).BoundTypes
      }(g)).([]any), ClassConstraints: func (v any) any {
        return v.(graph.Graph).ClassConstraints
      }(g).([]any), LambdaVariables: liblists.Foldl(func (s []any) any {
        return func (b core.Binding) any {
          return libsets.Delete(func (v any) any {
            return v.(core.Binding).Name
          }(b)).(func(any) any)(s)
        }
      }).(func(any) any)(func (v any) any {
        return v.(graph.Graph).LambdaVariables
      }(g)).(func(any) any)(bindings).([]any), Metadata: liblists.Foldl(func (gAcc graph.Graph) any {
        return func (b core.Binding) any {
          return func () any {
            var m any = func (v any) any {
              return v.(graph.Graph).Metadata
            }(gAcc)
            return func () any {
              var newMeta any = libmaybes.Maybe(libmaps.Delete(func (v any) any {
                return v.(core.Binding).Name
              }(b)).(func(any) any)(m)).(func(any) any)(func (t core.Term) any {
                return libmaps.Insert(func (v any) any {
                  return v.(core.Binding).Name
                }(b)).(func(any) any)(t).(func(any) any)(m)
              }).(func(any) any)(forBinding(gAcc)(b))
              return graph.Graph{BoundTerms: func (v any) any {
                return v.(graph.Graph).BoundTerms
              }(gAcc).([]any), BoundTypes: func (v any) any {
                return v.(graph.Graph).BoundTypes
              }(gAcc).([]any), ClassConstraints: func (v any) any {
                return v.(graph.Graph).ClassConstraints
              }(gAcc).([]any), LambdaVariables: func (v any) any {
                return v.(graph.Graph).LambdaVariables
              }(gAcc).([]any), Metadata: newMeta.([]any), Primitives: func (v any) any {
                return v.(graph.Graph).Primitives
              }(gAcc).([]any), SchemaTypes: func (v any) any {
                return v.(graph.Graph).SchemaTypes
              }(gAcc).([]any), TypeVariables: func (v any) any {
                return v.(graph.Graph).TypeVariables
              }(gAcc).([]any)}
            }()
          }()
        }
      }).(func(any) any)(g2).(func(any) any)(bindings).(graph.Graph).Metadata, Primitives: func (v any) any {
        return v.(graph.Graph).Primitives
      }(g).([]any), SchemaTypes: func (v any) any {
        return v.(graph.Graph).SchemaTypes
      }(g).([]any), TypeVariables: func (v any) any {
        return v.(graph.Graph).TypeVariables
      }(g).([]any)}
    }()
  }().(graph.Graph)
}

func ExtendGraphForTypeLambda (g graph.Graph, tlam core.TypeLambda) graph.Graph {
  return func () any {
    var name any = func (v any) any {
      return v.(core.TypeLambda).Parameter
    }(tlam)
    return graph.Graph{BoundTerms: func (v any) any {
      return v.(graph.Graph).BoundTerms
    }(g).([]any), BoundTypes: func (v any) any {
      return v.(graph.Graph).BoundTypes
    }(g).([]any), ClassConstraints: func (v any) any {
      return v.(graph.Graph).ClassConstraints
    }(g).([]any), LambdaVariables: func (v any) any {
      return v.(graph.Graph).LambdaVariables
    }(g).([]any), Metadata: func (v any) any {
      return v.(graph.Graph).Metadata
    }(g).([]any), Primitives: func (v any) any {
      return v.(graph.Graph).Primitives
    }(g).([]any), SchemaTypes: func (v any) any {
      return v.(graph.Graph).SchemaTypes
    }(g).([]any), TypeVariables: libsets.Insert(name).(func(any) any)(func (v any) any {
      return v.(graph.Graph).TypeVariables
    }(g)).([]any)}
  }().(graph.Graph)
}

func FieldMap (fields []any) []any {
  return func () any {
    toPair := func (f core.Field) any {
      return [2]any{func (v any) any {
        return v.(core.Field).Name
      }(f), func (v any) any {
        return v.(core.Field).Term
      }(f)}
    }
    return libmaps.FromList(liblists.Map(toPair).(func(any) any)(fields))
  }().([]any)
}

func FieldTypeMap (fields []any) []any {
  return func () any {
    toPair := func (f core.FieldType) any {
      return [2]any{func (v any) any {
        return v.(core.FieldType).Name
      }(f), func (v any) any {
        return v.(core.FieldType).Type_
      }(f)}
    }
    return libmaps.FromList(liblists.Map(toPair).(func(any) any)(fields))
  }().([]any)
}

func FieldTypes (cx context.Context, graph graph.Graph, t core.Type) any {
  return func () any {
    toMap := func (fields []any) any {
      return libmaps.FromList(liblists.Map(func (ft core.FieldType) any {
        return [2]any{func (v any) any {
          return v.(core.FieldType).Name
        }(ft), func (v any) any {
          return v.(core.FieldType).Type_
        }(ft)}
      }).(func(any) any)(fields))
    }
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeForall:
        return func (ft core.ForallType) any {
          return FieldTypes(cx, graph, func (v any) any {
            return v.(core.ForallType).Body
          }(ft).(core.Type))
        }(v.Value)
        case core.TypeRecord:
        return func (rt []any) any {
          return [2]any{"right", toMap(rt)}
        }(v.Value)
        case core.TypeUnion:
        return func (rt []any) any {
          return [2]any{"right", toMap(rt)}
        }(v.Value)
        case core.TypeVariable:
        return func (name core.Name) any {
          return libeithers.Bind(lexical.RequireElement(cx, graph, name)).(func(any) any)(func (el core.Binding) any {
            return libeithers.Bind(libeithers.Bimap(func (_wc_e error.Error) any {
              return context.InContext[error.Error]{Object: _wc_e, Context: cx}
            }).(func(any) any)(func (_wc_a core.Type) any {
              return _wc_a
            }).(func(any) any)(libeithers.Bimap(func (_e error.DecodingError) any {
              return error.ErrorOther{Value: error.OtherError(func (v any) any {
                return v
              }(_e).(string))}
            }).(func(any) any)(func (_a core.Type) any {
              return _a
            }).(func(any) any)(decodecore.Type_(graph, func (v any) any {
              return v.(core.Binding).Term
            }(el).(core.Term))))).(func(any) any)(func (decodedType core.Type) any {
              return FieldTypes(cx, graph, decodedType)
            })
          })
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat([]any{"expected record or union type but found ", showcore.Type_(t)}).(string))}, Context: cx}}
      }
      return nil
    }(rewriting.DeannotateType(t))
  }()
}

func FindFieldType (cx context.Context, fname core.Name, fields []any) any {
  return func () any {
    var matchingFields any = liblists.Filter(func (ft core.FieldType) any {
      return libequality.Equal(func (v any) any {
        return v.(core.FieldType).Name
      }(ft)).(func(any) any)(func (v any) any {
        return v
      }(fname))
    }).(func(any) any)(fields)
    return liblogic.IfElse(liblists.Null(matchingFields)).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("No such field: ").(func(any) any)(func (v any) any {
      return v
    }(fname)).(string))}, Context: cx}}).(func(any) any)(liblogic.IfElse(libequality.Equal(liblists.Length(matchingFields)).(func(any) any)(1)).(func(any) any)([2]any{"right", liblists.Head(matchingFields).(core.FieldType).Type_}).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("Multiple fields named ").(func(any) any)(func (v any) any {
      return v
    }(fname)).(string))}, Context: cx}}))
  }()
}

func FreshName (cx context.Context) any {
  return func () any {
    var count any = annotations.GetCount(constants.Key_freshTypeVariableCount, cx)
    return [2]any{NormalTypeVariable(count.(int32)), annotations.PutCount(constants.Key_freshTypeVariableCount, libmath.Add(count).(func(any) any)(1).(int32), cx)}
  }()
}

func FreshNames (n int32, cx context.Context) any {
  return func () any {
    go_ := func (acc any) any {
      return func (_ struct{}) any {
        return func () any {
          var names any = libpairs.First(acc)
          return func () any {
            var cx0 any = libpairs.Second(acc)
            return func () any {
              var result any = FreshName(cx0.(context.Context))
              return func () any {
                var name any = libpairs.First(result)
                return func () any {
                  var cx1 any = libpairs.Second(result)
                  return [2]any{liblists.Concat2(names).(func(any) any)(liblists.Pure(name)), cx1}
                }()
              }()
            }()
          }()
        }()
      }
    }
    return liblists.Foldl(go_).(func(any) any)([2]any{[]any{}, cx}).(func(any) any)(liblists.Replicate(n).(func(any) any)(struct{}{}))
  }()
}

func FTypeIsPolymorphic (typ core.Type) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return FTypeIsPolymorphic(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return true
      }(v.Value)
      default:
      return false
    }
    return nil
  }(typ).(bool)
}

func FullyStripAndNormalizeType (typ core.Type) core.Type {
  return func () any {
    var go_ func(int32) any
    go_ = func (depth int32) any {
      return func (subst []any) any {
        return func (t core.Type) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.TypeForall:
              return func (ft core.ForallType) any {
                return func () any {
                  var oldVar any = func (v any) any {
                    return v.(core.ForallType).Parameter
                  }(ft)
                  return func () any {
                    var newVar any = core.Name(libstrings.Cat2("_").(func(any) any)(libliterals.ShowInt32(depth)).(string))
                    return go_(libmath.Add(depth).(func(any) any)(1).(int32)).(func(any) any)(libmaps.Insert(oldVar).(func(any) any)(newVar).(func(any) any)(subst)).(func(any) any)(func (v any) any {
                      return v.(core.ForallType).Body
                    }(ft))
                  }()
                }()
              }(v.Value)
              default:
              return [2]any{subst, t}
            }
            return nil
          }(rewriting.DeannotateType(t))
        }
      }
    }
    return func () any {
      var result any = go_(0).(func(any) any)(libmaps.Empty).(func(any) any)(typ)
      return func () any {
        var subst any = libpairs.First(result)
        return func () any {
          var body any = libpairs.Second(result)
          return rewriting.SubstituteTypeVariables(subst.([]any), body.(core.Type))
        }()
      }()
    }()
  }().(core.Type)
}

func FullyStripType (typ core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return FullyStripType(func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type))
      }(v.Value)
      default:
      return typ
    }
    return nil
  }(rewriting.DeannotateType(typ)).(core.Type)
}

func GraphAsLet (bindings []any, body core.Term) core.Let {
  return core.Let{Bindings: bindings, Body: body}
}

func GraphAsTerm (bindings []any, body core.Term) core.Term {
  return core.TermLet{Value: GraphAsLet(bindings, body)}
}

func GraphAsTypes (cx context.Context, graph graph.Graph, els []any) any {
  return func () any {
    toPair := func (el core.Binding) any {
      return libeithers.Map(func (typ core.Type) any {
        return [2]any{func (v any) any {
          return v.(core.Binding).Name
        }(el), typ}
      }).(func(any) any)(libeithers.Bimap(func (_wc_e error.DecodingError) any {
        return context.InContext[error.DecodingError]{Object: _wc_e, Context: cx}
      }).(func(any) any)(func (_wc_a core.Type) any {
        return _wc_a
      }).(func(any) any)(decodecore.Type_(graph, func (v any) any {
        return v.(core.Binding).Term
      }(el).(core.Term))))
    }
    return libeithers.Map(libmaps.FromList).(func(any) any)(libeithers.MapList(toPair).(func(any) any)(els))
  }()
}

func InstantiateType (cx context.Context, typ core.Type) any {
  return func () any {
    var result any = InstantiateTypeScheme(cx, TypeToTypeScheme(typ))
    return [2]any{rewriting.TypeSchemeToFType(libpairs.First(result).(core.TypeScheme)), libpairs.Second(result)}
  }()
}

func InstantiateTypeScheme (cx context.Context, scheme core.TypeScheme) any {
  return func () any {
    var oldVars any = func (v any) any {
      return v.(core.TypeScheme).Variables
    }(scheme)
    return func () any {
      var result any = FreshNames(liblists.Length(oldVars).(int32), cx)
      return func () any {
        var newVars any = libpairs.First(result)
        return func () any {
          var cx2 any = libpairs.Second(result)
          return func () any {
            var subst any = typing.TypeSubst(libmaps.FromList(liblists.Zip(oldVars).(func(any) any)(liblists.Map(func (x core.Name) any {
              return core.TypeVariable{Value: x}
            }).(func(any) any)(newVars))).([]any))
            return func () any {
              var nameSubst any = libmaps.FromList(liblists.Zip(oldVars).(func(any) any)(newVars))
              return func () any {
                var renamedConstraints any = libmaybes.Map(func (oldConstraints []any) any {
                  return libmaps.FromList(liblists.Map(func (kv any) any {
                    return [2]any{libmaybes.FromMaybe(libpairs.First(kv)).(func(any) any)(libmaps.Lookup(libpairs.First(kv)).(func(any) any)(nameSubst)), libpairs.Second(kv)}
                  }).(func(any) any)(libmaps.ToList(oldConstraints)))
                }).(func(any) any)(func (v any) any {
                  return v.(core.TypeScheme).Constraints
                }(scheme))
                return [2]any{core.TypeScheme{Variables: newVars.([]any), Type_: substitution.SubstInType(subst.(typing.TypeSubst), func (v any) any {
                  return v.(core.TypeScheme).Type_
                }(scheme).(core.Type)), Constraints: renamedConstraints}, cx2}
              }()
            }()
          }()
        }()
      }()
    }()
  }()
}

func IsEncodedTerm (t core.Term) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermApplication:
      return func (a core.Application) any {
        return IsEncodedTerm(func (v any) any {
          return v.(core.Application).Function
        }(a).(core.Term))
      }(v.Value)
      case core.TermUnion:
      return func (i core.Injection) any {
        return libequality.Equal("hydra.core.Term").(func(any) any)(func (v any) any {
          return v.(core.Injection).TypeName
        }(i))
      }(v.Value)
      default:
      return false
    }
    return nil
  }(rewriting.DeannotateTerm(t)).(bool)
}

func IsEncodedType (t core.Term) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermApplication:
      return func (a core.Application) any {
        return IsEncodedType(func (v any) any {
          return v.(core.Application).Function
        }(a).(core.Term))
      }(v.Value)
      case core.TermUnion:
      return func (i core.Injection) any {
        return libequality.Equal("hydra.core.Type").(func(any) any)(func (v any) any {
          return v.(core.Injection).TypeName
        }(i))
      }(v.Value)
      default:
      return false
    }
    return nil
  }(rewriting.DeannotateTerm(t)).(bool)
}

func IsEnumRowType (rt []any) bool {
  return liblists.Foldl(liblogic.And).(func(any) any)(true).(func(any) any)(liblists.Map(func (f core.FieldType) any {
    return IsUnitType(rewriting.DeannotateType(func (v any) any {
      return v.(core.FieldType).Type_
    }(f).(core.Type)))
  }).(func(any) any)(rt)).(bool)
}

func IsEnumType (typ core.Type) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeUnion:
      return func (rt []any) any {
        return IsEnumRowType(rt)
      }(v.Value)
      default:
      return false
    }
    return nil
  }(rewriting.DeannotateType(typ)).(bool)
}

func IsSerializable (cx context.Context, graph graph.Graph, el core.Binding) any {
  return func () any {
    variants := func (typ core.Type) any {
      return liblists.Map(reflect.TypeVariant).(func(any) any)(rewriting.FoldOverType(coders.TraversalOrderPre{}, func (_p any) func(core.Type) any {
        return func (m any) func(core.Type) any {
          return func (t core.Type) any {
            return liblists.Cons(t).(func(any) any)(m)
          }
        }(_p).(func(core.Type) any)
      }, []any{}, typ))
    }
    return libeithers.Map(func (deps []any) any {
      return func () any {
        var allVariants any = libsets.FromList(liblists.Concat(liblists.Map(variants).(func(any) any)(libmaps.Elems(deps))))
        return liblogic.Not(libsets.Member(variants.TypeVariantFunction{}).(func(any) any)(allVariants))
      }()
    }).(func(any) any)(TypeDependencies(cx, graph, false, func (_p core.Type) core.Type {
      return libequality.Identity(_p).(core.Type)
    }, func (v any) any {
      return v.(core.Binding).Name
    }(el).(core.Name)))
  }()
}

func IsSerializableType (typ core.Type) bool {
  return func () any {
    var allVariants any = libsets.FromList(liblists.Map(reflect.TypeVariant).(func(any) any)(rewriting.FoldOverType(coders.TraversalOrderPre{}, func (_p any) func(core.Type) any {
      return func (m any) func(core.Type) any {
        return func (t core.Type) any {
          return liblists.Cons(t).(func(any) any)(m)
        }
      }(_p).(func(core.Type) any)
    }, []any{}, typ)))
    return liblogic.Not(libsets.Member(variants.TypeVariantFunction{}).(func(any) any)(allVariants))
  }().(bool)
}

func IsSerializableByName (cx context.Context, graph graph.Graph, name core.Name) any {
  return func () any {
    variants := func (typ core.Type) any {
      return liblists.Map(reflect.TypeVariant).(func(any) any)(rewriting.FoldOverType(coders.TraversalOrderPre{}, func (_p any) func(core.Type) any {
        return func (m any) func(core.Type) any {
          return func (t core.Type) any {
            return liblists.Cons(t).(func(any) any)(m)
          }
        }(_p).(func(core.Type) any)
      }, []any{}, typ))
    }
    return libeithers.Map(func (deps []any) any {
      return func () any {
        var allVariants any = libsets.FromList(liblists.Concat(liblists.Map(variants).(func(any) any)(libmaps.Elems(deps))))
        return liblogic.Not(libsets.Member(variants.TypeVariantFunction{}).(func(any) any)(allVariants))
      }()
    }).(func(any) any)(TypeDependencies(cx, graph, false, func (_p core.Type) core.Type {
      return libequality.Identity(_p).(core.Type)
    }, name))
  }()
}

func IsType (t core.Type) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeApplication:
      return func (a core.ApplicationType) any {
        return IsType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(a).(core.Type))
      }(v.Value)
      case core.TypeForall:
      return func (l core.ForallType) any {
        return IsType(func (v any) any {
          return v.(core.ForallType).Body
        }(l).(core.Type))
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return false
      }(v.Value)
      case core.TypeVariable:
      return func (v core.Name) any {
        return libequality.Equal(v).(func(any) any)(core.Name("hydra.core.Type"))
      }(v.Value)
      default:
      return false
    }
    return nil
  }(rewriting.DeannotateType(t)).(bool)
}

func IsUnitTerm (v1 core.Term) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermUnit:
      return func (_ struct{}) any {
        return true
      }(v)
      default:
      return false
    }
    return nil
  }(v1).(bool)
}

func IsUnitType (v1 core.Type) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeUnit:
      return func (_ struct{}) any {
        return true
      }(v)
      default:
      return false
    }
    return nil
  }(v1).(bool)
}

func ModuleContainsBinaryLiterals (mod hmodule.Module) bool {
  return func () any {
    checkTerm := func (found bool) any {
      return func (term core.Term) any {
        return liblogic.Or(found).(func(any) any)(func (x any) any {
          switch v := x.(type) {
            case core.TermLiteral:
            return func (lit core.Literal) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.LiteralBinary:
                  return func (_ []byte) any {
                    return true
                  }(v.Value)
                  default:
                  return false
                }
                return nil
              }(lit)
            }(v.Value)
            default:
            return false
          }
          return nil
        }(term))
      }
    }
    return func () any {
      termContainsBinary := func (term core.Term) any {
        return rewriting.FoldOverTerm[bool](coders.TraversalOrderPre{}, func (_p any) func(core.Term) any {
          return checkTerm(_p.(bool)).(func(core.Term) any)
        }, false, term)
      }
      return liblists.Foldl(func (acc bool) any {
        return func (el core.Binding) any {
          return liblogic.Or(acc).(func(any) any)(termContainsBinary(func (v any) any {
            return v.(core.Binding).Term
          }(el).(core.Term)))
        }
      }).(func(any) any)(false).(func(any) any)(func (v any) any {
        return v.(hmodule.Module).Elements
      }(mod))
    }()
  }().(bool)
}

func ModuleDependencyNamespaces (cx context.Context, graph graph.Graph, binds bool, withPrims bool, withNoms bool, withSchema bool, mod hmodule.Module) any {
  return libeithers.Map(func (deps []any) any {
    return libsets.Delete(func (v any) any {
      return v.(hmodule.Module).Namespace
    }(mod)).(func(any) any)(deps)
  }).(func(any) any)(DependencyNamespaces(cx, graph, binds, withPrims, withNoms, withSchema, func (v any) any {
    return v.(hmodule.Module).Elements
  }(mod).([]any)))
}

func NamespacesForDefinitions[T0 any] (encodeNamespace func(hmodule.Namespace) T0, focusNs hmodule.Namespace, defs []any) hmodule.Namespaces[T0] {
  return func () any {
    var nss any = libsets.Delete(focusNs).(func(any) any)(DefinitionDependencyNamespaces(defs))
    return func () any {
      toPair := func (ns hmodule.Namespace) any {
        return [2]any{ns, encodeNamespace(ns)}
      }
      return hmodule.Namespaces[T0]{Focus: toPair(focusNs), Mapping: libmaps.FromList(liblists.Map(toPair).(func(any) any)(libsets.ToList(nss))).([]any)}
    }()
  }().(hmodule.Namespaces[T0])
}

func NominalApplication (tname core.Name, args []any) core.Type {
  return liblists.Foldl(func (t core.Type) any {
    return func (a core.Type) any {
      return core.TypeApplication{Value: core.ApplicationType{Function: t, Argument: a}}
    }
  }).(func(any) any)(core.TypeVariable{Value: tname}).(func(any) any)(args).(core.Type)
}

func NormalTypeVariable (i int32) core.Name {
  return core.Name(libstrings.Cat2("t").(func(any) any)(libliterals.ShowInt32(i)).(string))
}

func PartitionDefinitions (defs []any) any {
  return func () any {
    getType := func (def hmodule.Definition) any {
      return func (x any) any {
        switch v := x.(type) {
          case hmodule.DefinitionType_:
          return func (td hmodule.TypeDefinition) any {
            return func () any {
              _v := td
              return &_v
            }()
          }(v.Value)
          case hmodule.DefinitionTerm:
          return func (_ hmodule.TermDefinition) any {
            return nil
          }(v.Value)
        }
        return nil
      }(def)
    }
    return func () any {
      getTerm := func (def hmodule.Definition) any {
        return func (x any) any {
          switch v := x.(type) {
            case hmodule.DefinitionType_:
            return func (_ hmodule.TypeDefinition) any {
              return nil
            }(v.Value)
            case hmodule.DefinitionTerm:
            return func (td hmodule.TermDefinition) any {
              return func () any {
                _v := td
                return &_v
              }()
            }(v.Value)
          }
          return nil
        }(def)
      }
      return [2]any{libmaybes.Cat(liblists.Map(getType).(func(any) any)(defs)), libmaybes.Cat(liblists.Map(getTerm).(func(any) any)(defs))}
    }()
  }()
}

func RequireRecordType (cx context.Context, graph graph.Graph, name core.Name) any {
  return func () any {
    toRecord := func (t core.Type) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.TypeRecord:
          return func (rt []any) any {
            return func () any {
              _v := rt
              return &_v
            }()
          }(v.Value)
          default:
          return nil
        }
        return nil
      }(t)
    }
    return RequireRowType(cx, "record type", toRecord, graph, name)
  }()
}

func RequireRowType (cx context.Context, label string, getter func(core.Type) any, graph graph.Graph, name core.Name) any {
  return func () any {
    var rawType func(core.Type) any
    rawType = func (t core.Type) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.TypeAnnotated:
          return func (at core.AnnotatedType) any {
            return rawType(func (v any) any {
              return v.(core.AnnotatedType).Body
            }(at).(core.Type))
          }(v.Value)
          case core.TypeForall:
          return func (ft core.ForallType) any {
            return rawType(func (v any) any {
              return v.(core.ForallType).Body
            }(ft).(core.Type))
          }(v.Value)
          default:
          return t
        }
        return nil
      }(t)
    }
    return libeithers.Bind(RequireType(cx, graph, name)).(func(any) any)(func (t core.Type) any {
      return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat([]any{func (v any) any {
        return v
      }(name), " does not resolve to a ", label, " type: ", showcore.Type_(t)}).(string))}, Context: cx}}).(func(any) any)(func (x any) any {
        return [2]any{"right", x}
      }).(func(any) any)(getter(rawType(t).(core.Type)))
    })
  }()
}

func RequireSchemaType (cx context.Context, types []any, tname core.Name) any {
  return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat([]any{"No such schema type: ", func (v any) any {
    return v
  }(tname), ". Available types are: ", libstrings.Intercalate(", ").(func(any) any)(liblists.Map(func (v any) any {
    return v
  }).(func(any) any)(libmaps.Keys(types)))}).(string))}, Context: cx}}).(func(any) any)(func (ts core.TypeScheme) any {
    return [2]any{"right", InstantiateTypeScheme(cx, rewriting.DeannotateTypeSchemeRecursive(ts))}
  }).(func(any) any)(libmaps.Lookup(tname).(func(any) any)(types))
}

func RequireType (cx context.Context, graph graph.Graph, name core.Name) any {
  return libmaybes.Maybe(libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("no such type: ").(func(any) any)(func (v any) any {
    return v
  }(name)).(string))}, Context: cx}}).(func(any) any)(func (ts core.TypeScheme) any {
    return [2]any{"right", rewriting.TypeSchemeToFType(ts)}
  }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(func (v any) any {
    return v.(graph.Graph).BoundTypes
  }(graph)))).(func(any) any)(func (ts core.TypeScheme) any {
    return [2]any{"right", rewriting.TypeSchemeToFType(ts)}
  }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(func (v any) any {
    return v.(graph.Graph).SchemaTypes
  }(graph)))
}

func RequireUnionField (cx context.Context, graph graph.Graph, tname core.Name, fname core.Name) any {
  return func () any {
    withRowType := func (rt []any) any {
      return func () any {
        var matches any = liblists.Filter(func (ft core.FieldType) any {
          return libequality.Equal(func (v any) any {
            return v.(core.FieldType).Name
          }(ft)).(func(any) any)(fname)
        }).(func(any) any)(rt)
        return liblogic.IfElse(liblists.Null(matches)).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat([]any{"no field \"", func (v any) any {
          return v
        }(fname), "\" in union type \"", func (v any) any {
          return v
        }(tname)}).(string))}, Context: cx}}).(func(any) any)([2]any{"right", liblists.Head(matches).(core.FieldType).Type_})
      }()
    }
    return libeithers.Bind(RequireUnionType(cx, graph, tname)).(func(any) any)(withRowType)
  }()
}

func RequireUnionType (cx context.Context, graph graph.Graph, name core.Name) any {
  return func () any {
    toUnion := func (t core.Type) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.TypeUnion:
          return func (rt []any) any {
            return func () any {
              _v := rt
              return &_v
            }()
          }(v.Value)
          default:
          return nil
        }
        return nil
      }(t)
    }
    return RequireRowType(cx, "union", toUnion, graph, name)
  }()
}

func ResolveType (graph graph.Graph, typ core.Type) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeVariable:
      return func (name core.Name) any {
        return libmaybes.Maybe(libmaybes.Map(func (ts core.TypeScheme) any {
          return rewriting.TypeSchemeToFType(ts)
        }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(func (v any) any {
          return v.(graph.Graph).BoundTypes
        }(graph)))).(func(any) any)(func (ts core.TypeScheme) any {
          return func () any {
            _v := rewriting.TypeSchemeToFType(ts)
            return &_v
          }()
        }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(func (v any) any {
          return v.(graph.Graph).SchemaTypes
        }(graph)))
      }(v.Value)
      default:
      return func () any {
        _v := typ
        return &_v
      }()
    }
    return nil
  }(rewriting.DeannotateType(typ))
}

func SchemaGraphToTypingEnvironment (cx context.Context, g graph.Graph) any {
  return func () any {
    var toTypeScheme func([]any) any
    toTypeScheme = func (vars []any) any {
      return func (typ core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeForall:
            return func (ft core.ForallType) any {
              return toTypeScheme(liblists.Cons(func (v any) any {
                return v.(core.ForallType).Parameter
              }(ft)).(func(any) any)(vars).([]any)).(func(any) any)(func (v any) any {
                return v.(core.ForallType).Body
              }(ft))
            }(v.Value)
            default:
            return core.TypeScheme{Variables: liblists.Reverse(vars).([]any), Type_: typ, Constraints: nil}
          }
          return nil
        }(rewriting.DeannotateType(typ))
      }
    }
    return func () any {
      decodeType := func (term core.Term) any {
        return libeithers.Bimap(func (_wc_e error.Error) any {
          return context.InContext[error.Error]{Object: _wc_e, Context: cx}
        }).(func(any) any)(func (_wc_a core.Type) any {
          return _wc_a
        }).(func(any) any)(libeithers.Bimap(func (_e error.DecodingError) any {
          return error.ErrorOther{Value: error.OtherError(func (v any) any {
            return v
          }(_e).(string))}
        }).(func(any) any)(func (_a core.Type) any {
          return _a
        }).(func(any) any)(decodecore.Type_(g, term)))
      }
      return func () any {
        decodeTypeScheme := func (term core.Term) any {
          return libeithers.Bimap(func (_wc_e error.Error) any {
            return context.InContext[error.Error]{Object: _wc_e, Context: cx}
          }).(func(any) any)(func (_wc_a core.TypeScheme) any {
            return _wc_a
          }).(func(any) any)(libeithers.Bimap(func (_e error.DecodingError) any {
            return error.ErrorOther{Value: error.OtherError(func (v any) any {
              return v
            }(_e).(string))}
          }).(func(any) any)(func (_a core.TypeScheme) any {
            return _a
          }).(func(any) any)(decodecore.TypeScheme(g, term)))
        }
        return func () any {
          toPair := func (el core.Binding) any {
            return func () any {
              forTerm := func (term core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermRecord:
                    return func (r core.Record) any {
                      return liblogic.IfElse(libequality.Equal(func (v any) any {
                        return v.(core.Record).TypeName
                      }(r)).(func(any) any)(core.Name("hydra.core.TypeScheme"))).(func(any) any)(libeithers.Map(libmaybes.Pure).(func(any) any)(decodeTypeScheme(func (v any) any {
                        return v.(core.Binding).Term
                      }(el).(core.Term)))).(func(any) any)([2]any{"right", nil})
                    }(v.Value)
                    case core.TermUnion:
                    return func (i core.Injection) any {
                      return liblogic.IfElse(libequality.Equal(func (v any) any {
                        return v.(core.Injection).TypeName
                      }(i)).(func(any) any)(core.Name("hydra.core.Type"))).(func(any) any)(libeithers.Map(func (decoded core.Type) any {
                        return func () any {
                          _v := toTypeScheme([]any{}).(func(any) any)(decoded)
                          return &_v
                        }()
                      }).(func(any) any)(decodeType(func (v any) any {
                        return v.(core.Binding).Term
                      }(el).(core.Term)))).(func(any) any)([2]any{"right", nil})
                    }(v.Value)
                    default:
                    return [2]any{"right", nil}
                  }
                  return nil
                }(term)
              }
              return libeithers.Bind(libmaybes.Maybe(libeithers.Map(func (typ core.Type) any {
                return func () any {
                  _v := rewriting.FTypeToTypeScheme(typ)
                  return &_v
                }()
              }).(func(any) any)(decodeType(func (v any) any {
                return v.(core.Binding).Term
              }(el).(core.Term)))).(func(any) any)(func (ts core.TypeScheme) any {
                return liblogic.IfElse(libequality.Equal(ts).(func(any) any)(core.TypeScheme{Variables: []any{}, Type_: core.TypeVariable{Value: core.Name("hydra.core.TypeScheme")}, Constraints: nil})).(func(any) any)(libeithers.Map(libmaybes.Pure).(func(any) any)(decodeTypeScheme(func (v any) any {
                  return v.(core.Binding).Term
                }(el).(core.Term)))).(func(any) any)(liblogic.IfElse(libequality.Equal(ts).(func(any) any)(core.TypeScheme{Variables: []any{}, Type_: core.TypeVariable{Value: core.Name("hydra.core.Type")}, Constraints: nil})).(func(any) any)(libeithers.Map(func (decoded core.Type) any {
                  return func () any {
                    _v := toTypeScheme([]any{}).(func(any) any)(decoded)
                    return &_v
                  }()
                }).(func(any) any)(decodeType(func (v any) any {
                  return v.(core.Binding).Term
                }(el).(core.Term)))).(func(any) any)(forTerm(rewriting.DeannotateTerm(func (v any) any {
                  return v.(core.Binding).Term
                }(el).(core.Term)))))
              }).(func(any) any)(func (v any) any {
                return v.(core.Binding).Type_
              }(el))).(func(any) any)(func (mts any) any {
                return [2]any{"right", libmaybes.Map(func (ts core.TypeScheme) any {
                  return [2]any{func (v any) any {
                    return v.(core.Binding).Name
                  }(el), ts}
                }).(func(any) any)(mts)}
              })
            }()
          }
          return libeithers.Map(func (mpairs []any) any {
            return libmaps.FromList(libmaybes.Cat(mpairs))
          }).(func(any) any)(libeithers.MapList(toPair).(func(any) any)(lexical.GraphToBindings(g)))
        }()
      }()
    }()
  }()
}

func TermAsBindings (term core.Term) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermLet:
      return func (lt core.Let) any {
        return func (v any) any {
          return v.(core.Let).Bindings
        }(lt)
      }(v.Value)
      default:
      return []any{}
    }
    return nil
  }(rewriting.DeannotateTerm(term)).([]any)
}

func TopologicalSortTypeDefinitions (defs []any) []any {
  return func () any {
    toPair := func (def hmodule.TypeDefinition) any {
      return [2]any{func (v any) any {
        return v.(hmodule.TypeDefinition).Name
      }(def), libsets.ToList(rewriting.TypeDependencyNames(false, func (v any) any {
        return v.(hmodule.TypeDefinition).Type_
      }(def).(core.Type)))}
    }
    return func () any {
      var nameToDef any = libmaps.FromList(liblists.Map(func (d hmodule.TypeDefinition) any {
        return [2]any{func (v any) any {
          return v.(hmodule.TypeDefinition).Name
        }(d), d}
      }).(func(any) any)(defs))
      return func () any {
        var sorted any = sorting.TopologicalSortComponents(liblists.Map(toPair).(func(any) any)(defs).([]any))
        return liblists.Map(func (names []any) any {
          return libmaybes.Cat(liblists.Map(func (n core.Name) any {
            return libmaps.Lookup(n).(func(any) any)(nameToDef)
          }).(func(any) any)(names))
        }).(func(any) any)(sorted)
      }()
    }()
  }().([]any)
}

func TypeDependencies (cx context.Context, graph graph.Graph, withSchema bool, transform func(core.Type) core.Type, name core.Name) any {
  return func () any {
    requireType := func (name2 core.Name) any {
      return func () any {
        var cx1 any = context.Context{Trace: liblists.Cons(libstrings.Cat2("type dependencies of ").(func(any) any)(func (v any) any {
          return v
        }(name2))).(func(any) any)(func (v any) any {
          return v.(context.Context).Trace
        }(cx)).([]any), Messages: func (v any) any {
          return v.(context.Context).Messages
        }(cx).([]any), Other: func (v any) any {
          return v.(context.Context).Other
        }(cx).([]any)}
        return libeithers.Bind(lexical.RequireElement(cx1.(context.Context), graph, name2)).(func(any) any)(func (el core.Binding) any {
          return libeithers.Bimap(func (_wc_e error.Error) any {
            return context.InContext[error.Error]{Object: _wc_e, Context: cx1.(context.Context)}
          }).(func(any) any)(func (_wc_a core.Type) any {
            return _wc_a
          }).(func(any) any)(libeithers.Bimap(func (_e error.DecodingError) any {
            return error.ErrorOther{Value: error.OtherError(func (v any) any {
              return v
            }(_e).(string))}
          }).(func(any) any)(func (_a core.Type) any {
            return _a
          }).(func(any) any)(decodecore.Type_(graph, func (v any) any {
            return v.(core.Binding).Term
          }(el).(core.Term))))
        })
      }()
    }
    return func () any {
      toPair := func (name2 core.Name) any {
        return libeithers.Map(func (typ core.Type) any {
          return [2]any{name2, transform(typ)}
        }).(func(any) any)(requireType(name2))
      }
      return func () any {
        var deps func([]any) any
        deps = func (seeds []any) any {
          return func (names []any) any {
            return liblogic.IfElse(libsets.Null(seeds)).(func(any) any)([2]any{"right", names}).(func(any) any)(libeithers.Bind(libeithers.MapList(toPair).(func(any) any)(libsets.ToList(seeds))).(func(any) any)(func (pairs []any) any {
              return func () any {
                var newNames any = libmaps.Union(names).(func(any) any)(libmaps.FromList(pairs))
                return func () any {
                  var refs any = liblists.Foldl(libsets.Union).(func(any) any)(libsets.Empty).(func(any) any)(liblists.Map(func (pair any) any {
                    return rewriting.TypeDependencyNames(withSchema, libpairs.Second(pair).(core.Type))
                  }).(func(any) any)(pairs))
                  return func () any {
                    var visited any = libsets.FromList(libmaps.Keys(names))
                    return func () any {
                      var newSeeds any = libsets.Difference(refs).(func(any) any)(visited)
                      return deps(newSeeds.([]any)).(func(any) any)(newNames)
                    }()
                  }()
                }()
              }()
            }))
          }
        }
        return deps(libsets.Singleton(name).([]any)).(func(any) any)(libmaps.Empty)
      }()
    }()
  }()
}

func TypeToTypeScheme (t0 core.Type) core.TypeScheme {
  return func () any {
    var helper func([]any) any
    helper = func (vars []any) any {
      return func (t core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeForall:
            return func (ft core.ForallType) any {
              return helper(liblists.Cons(func (v any) any {
                return v.(core.ForallType).Parameter
              }(ft)).(func(any) any)(vars).([]any)).(func(any) any)(func (v any) any {
                return v.(core.ForallType).Body
              }(ft))
            }(v.Value)
            default:
            return core.TypeScheme{Variables: liblists.Reverse(vars).([]any), Type_: t, Constraints: nil}
          }
          return nil
        }(rewriting.DeannotateType(t))
      }
    }
    return helper([]any{}).(func(any) any)(t0)
  }().(core.TypeScheme)
}

func TypesToElements (typeMap []any) []any {
  return func () any {
    toElement := func (pair any) any {
      return func () any {
        var name any = libpairs.First(pair)
        return core.Binding{Name: name.(core.Name), Term: encodecore.Type_(libpairs.Second(pair).(core.Type)), Type_: nil}
      }()
    }
    return liblists.Map(toElement).(func(any) any)(libmaps.ToList(typeMap))
  }().([]any)
}

func WithLambdaContext[T0, T1, T2 any] (getContext func(T0) graph.Graph, setContext func(graph.Graph) func(T0) T1, env T0, lam core.Lambda, body func(T1) T2) T2 {
  return func () any {
    var newContext any = ExtendGraphForLambda(getContext(env), lam)
    return body(setContext(newContext.(graph.Graph))(env))
  }().(T2)
}

func WithLetContext[T0, T1, T2 any] (getContext func(T0) graph.Graph, setContext func(graph.Graph) func(T0) T1, forBinding func(graph.Graph) func(core.Binding) any, env T0, letrec core.Let, body func(T1) T2) T2 {
  return func () any {
    var newContext any = ExtendGraphForLet(forBinding, getContext(env), letrec)
    return body(setContext(newContext.(graph.Graph))(env))
  }().(T2)
}

func WithTypeLambdaContext[T0, T1, T2 any] (getContext func(T0) graph.Graph, setContext func(graph.Graph) func(T0) T1, env T0, tlam core.TypeLambda, body func(T1) T2) T2 {
  return func () any {
    var newContext any = ExtendGraphForTypeLambda(getContext(env), tlam)
    return body(setContext(newContext.(graph.Graph))(env))
  }().(T2)
}
