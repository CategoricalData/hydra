// Note: this is an automatically generated file. Do not edit.

package lexical

import (
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  "hydra.dev/hydra/graph"
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
  showcore "hydra.dev/hydra/show/core"
)

func BuildGraph (elements []any, environment []any, primitives []any) graph.Graph {
  return func () any {
    var elementTerms any = libmaps.FromList(liblists.Map(func (b core.Binding) any {
      return [2]any{func (v any) any {
        return v.(core.Binding).Name
      }(b), func (v any) any {
        return v.(core.Binding).Term
      }(b)}
    }).(func(any) any)(elements))
    return func () any {
      var letTerms any = libmaps.Map(func (mt any) any {
        return libmaybes.FromJust(mt)
      }).(func(any) any)(libmaps.Filter(func (mt any) any {
        return libmaybes.IsJust(mt)
      }).(func(any) any)(environment))
      return func () any {
        var elementTypes any = libmaps.FromList(libmaybes.Cat(liblists.Map(func (b core.Binding) any {
          return libmaybes.Map(func (ts core.TypeScheme) any {
            return [2]any{func (v any) any {
              return v.(core.Binding).Name
            }(b), ts}
          }).(func(any) any)(func (v any) any {
            return v.(core.Binding).Type_
          }(b))
        }).(func(any) any)(elements)))
        return graph.Graph{BoundTerms: libmaps.Union(elementTerms).(func(any) any)(letTerms).([]any), BoundTypes: elementTypes.([]any), ClassConstraints: libmaps.Empty, LambdaVariables: libsets.FromList(libmaps.Keys(libmaps.Filter(func (mt any) any {
          return libmaybes.IsNothing(mt)
        }).(func(any) any)(environment))).([]any), Metadata: libmaps.Empty, Primitives: primitives, SchemaTypes: libmaps.Empty, TypeVariables: libsets.Empty}
      }()
    }()
  }().(graph.Graph)
}

func ChooseUniqueName (reserved []any, name core.Name) core.Name {
  return func () any {
    var tryName func(int32) any
    tryName = func (index int32) any {
      return func () any {
        var candidate any = liblogic.IfElse(libequality.Equal(index).(func(any) any)(1)).(func(any) any)(name).(func(any) any)(core.Name(libstrings.Cat2(func (v any) any {
          return v
        }(name)).(func(any) any)(libliterals.ShowInt32(index)).(string)))
        return liblogic.IfElse(libsets.Member(candidate).(func(any) any)(reserved)).(func(any) any)(tryName(libmath.Add(index).(func(any) any)(1).(int32))).(func(any) any)(candidate)
      }()
    }
    return tryName(1)
  }().(core.Name)
}

func DereferenceElement (graph graph.Graph, name core.Name) any {
  return LookupElement(graph, name)
}

func DereferenceSchemaType (name core.Name, types []any) any {
  return func () any {
    var forType func(core.Type) any
    forType = func (t core.Type) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.TypeAnnotated:
          return func (at core.AnnotatedType) any {
            return forType(func (v any) any {
              return v.(core.AnnotatedType).Body
            }(at).(core.Type))
          }(v.Value)
          case core.TypeForall:
          return func (ft core.ForallType) any {
            return libmaybes.Map(func (ts core.TypeScheme) any {
              return core.TypeScheme{Variables: liblists.Cons(func (v any) any {
                return v.(core.ForallType).Parameter
              }(ft)).(func(any) any)(func (v any) any {
                return v.(core.TypeScheme).Variables
              }(ts)).([]any), Type_: func (v any) any {
                return v.(core.TypeScheme).Type_
              }(ts).(core.Type), Constraints: func (v any) any {
                return v.(core.TypeScheme).Constraints
              }(ts)}
            }).(func(any) any)(forType(func (v any) any {
              return v.(core.ForallType).Body
            }(ft).(core.Type)))
          }(v.Value)
          case core.TypeVariable:
          return func (v core.Name) any {
            return DereferenceSchemaType(v, types)
          }(v.Value)
          default:
          return func () any {
            _v := core.TypeScheme{Variables: []any{}, Type_: t, Constraints: nil}
            return &_v
          }()
        }
        return nil
      }(t)
    }
    return libmaybes.Bind(libmaps.Lookup(name).(func(any) any)(types)).(func(any) any)(func (ts core.TypeScheme) any {
      return libmaybes.Map(func (ts2 core.TypeScheme) any {
        return core.TypeScheme{Variables: liblists.Concat2(func (v any) any {
          return v.(core.TypeScheme).Variables
        }(ts)).(func(any) any)(func (v any) any {
          return v.(core.TypeScheme).Variables
        }(ts2)).([]any), Type_: func (v any) any {
          return v.(core.TypeScheme).Type_
        }(ts2).(core.Type), Constraints: func (v any) any {
          return v.(core.TypeScheme).Constraints
        }(ts2)}
      }).(func(any) any)(forType(func (v any) any {
        return v.(core.TypeScheme).Type_
      }(ts).(core.Type)))
    })
  }()
}

func DereferenceVariable (graph graph.Graph, name core.Name) any {
  return libmaybes.Maybe([2]any{"left", libstrings.Cat2("no such element: ").(func(any) any)(func (v any) any {
    return v
  }(name))}).(func(any) any)(func (right_ core.Binding) any {
    return [2]any{"right", right_}
  }).(func(any) any)(LookupElement(graph, name))
}

func ElementsToGraph (parent graph.Graph, schemaTypes []any, elements []any) graph.Graph {
  return func () any {
    var prims any = func (v any) any {
      return v.(graph.Graph).Primitives
    }(parent)
    return func () any {
      var g any = BuildGraph(elements, libmaps.Empty, prims.([]any))
      return graph.Graph{BoundTerms: g.(graph.Graph).BoundTerms, BoundTypes: g.(graph.Graph).BoundTypes, ClassConstraints: g.(graph.Graph).ClassConstraints, LambdaVariables: g.(graph.Graph).LambdaVariables, Metadata: g.(graph.Graph).Metadata, Primitives: g.(graph.Graph).Primitives, SchemaTypes: schemaTypes, TypeVariables: g.(graph.Graph).TypeVariables}
    }()
  }().(graph.Graph)
}

var EmptyContext = context.Context{Trace: []any{}, Messages: []any{}, Other: libmaps.Empty}

var EmptyGraph = graph.Graph{BoundTerms: libmaps.Empty, BoundTypes: libmaps.Empty, ClassConstraints: libmaps.Empty, LambdaVariables: libsets.Empty, Metadata: libmaps.Empty, Primitives: libmaps.Empty, SchemaTypes: libmaps.Empty, TypeVariables: libsets.Empty}

func ExtendGraphWithBindings (bindings []any, g graph.Graph) graph.Graph {
  return func () any {
    var newTerms any = libmaps.FromList(liblists.Map(func (b core.Binding) any {
      return [2]any{func (v any) any {
        return v.(core.Binding).Name
      }(b), func (v any) any {
        return v.(core.Binding).Term
      }(b)}
    }).(func(any) any)(bindings))
    return func () any {
      var newTypes any = libmaps.FromList(libmaybes.Cat(liblists.Map(func (b core.Binding) any {
        return libmaybes.Map(func (ts core.TypeScheme) any {
          return [2]any{func (v any) any {
            return v.(core.Binding).Name
          }(b), ts}
        }).(func(any) any)(func (v any) any {
          return v.(core.Binding).Type_
        }(b))
      }).(func(any) any)(bindings)))
      return graph.Graph{BoundTerms: libmaps.Union(newTerms).(func(any) any)(func (v any) any {
        return v.(graph.Graph).BoundTerms
      }(g)).([]any), BoundTypes: libmaps.Union(newTypes).(func(any) any)(func (v any) any {
        return v.(graph.Graph).BoundTypes
      }(g)).([]any), ClassConstraints: func (v any) any {
        return v.(graph.Graph).ClassConstraints
      }(g).([]any), LambdaVariables: func (v any) any {
        return v.(graph.Graph).LambdaVariables
      }(g).([]any), Metadata: func (v any) any {
        return v.(graph.Graph).Metadata
      }(g).([]any), Primitives: func (v any) any {
        return v.(graph.Graph).Primitives
      }(g).([]any), SchemaTypes: func (v any) any {
        return v.(graph.Graph).SchemaTypes
      }(g).([]any), TypeVariables: func (v any) any {
        return v.(graph.Graph).TypeVariables
      }(g).([]any)}
    }()
  }().(graph.Graph)
}

func GraphToBindings (g graph.Graph) []any {
  return liblists.Map(func (p any) any {
    return func () any {
      var name any = libpairs.First(p)
      return func () any {
        var term any = libpairs.Second(p)
        return core.Binding{Name: name.(core.Name), Term: term.(core.Term), Type_: libmaps.Lookup(name).(func(any) any)(func (v any) any {
          return v.(graph.Graph).BoundTypes
        }(g))}
      }()
    }()
  }).(func(any) any)(libmaps.ToList(func (v any) any {
    return v.(graph.Graph).BoundTerms
  }(g))).([]any)
}

func FieldsOf (t core.Type) []any {
  return func () any {
    var stripped any = rewriting.DeannotateType(t)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeForall:
        return func (forallType core.ForallType) any {
          return FieldsOf(func (v any) any {
            return v.(core.ForallType).Body
          }(forallType).(core.Type))
        }(v.Value)
        case core.TypeRecord:
        return func (rt []any) any {
          return rt
        }(v.Value)
        case core.TypeUnion:
        return func (rt []any) any {
          return rt
        }(v.Value)
        default:
        return []any{}
      }
      return nil
    }(stripped)
  }().([]any)
}

func GetField[T0 any] (cx context.Context, m []any, fname core.Name, decode func(T0) any) any {
  return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2("expected field ").(func(any) any)(func (v any) any {
    return v
  }(fname))).(func(any) any)(" not found").(string))}, Context: cx}}).(func(any) any)(decode).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(m))
}

func LookupElement (graph graph.Graph, name core.Name) any {
  return libmaybes.Map(func (term core.Term) any {
    return core.Binding{Name: name, Term: term, Type_: libmaps.Lookup(name).(func(any) any)(func (v any) any {
      return v.(graph.Graph).BoundTypes
    }(graph))}
  }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(func (v any) any {
    return v.(graph.Graph).BoundTerms
  }(graph)))
}

func LookupPrimitive (graph graph.Graph, name core.Name) any {
  return libmaps.Lookup(name).(func(any) any)(func (v any) any {
    return v.(graph.Graph).Primitives
  }(graph))
}

func LookupTerm (graph graph.Graph, name core.Name) any {
  return libmaps.Lookup(name).(func(any) any)(func (v any) any {
    return v.(graph.Graph).BoundTerms
  }(graph))
}

func MatchEnum (cx context.Context, graph graph.Graph, tname core.Name, pairs []any, v1 core.Term) any {
  return MatchUnion(cx, graph, tname, liblists.Map(func (pair any) any {
    return MatchUnitField(libpairs.First(pair), libpairs.Second(pair))
  }).(func(any) any)(pairs).([]any), v1)
}

func MatchRecord[T0 any] (cx context.Context, graph T0, decode func([]any) any, term core.Term) any {
  return func () any {
    var stripped any = rewriting.DeannotateAndDetypeTerm(term)
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return decode(libmaps.FromList(liblists.Map(func (field core.Field) any {
            return [2]any{func (v any) any {
              return v.(core.Field).Name
            }(field), func (v any) any {
              return v.(core.Field).Term
            }(field)}
          }).(func(any) any)(func (v any) any {
            return v.(core.Record).Fields
          }(record))).([]any))
        }(v.Value)
        default:
        return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("expected a record, got ").(func(any) any)(showcore.Term(term)).(string))}, Context: cx}}
      }
      return nil
    }(stripped)
  }()
}

func MatchUnion (cx context.Context, graph graph.Graph, tname core.Name, pairs []any, term core.Term) any {
  return func () any {
    var stripped any = rewriting.DeannotateAndDetypeTerm(term)
    return func () any {
      var mapping any = libmaps.FromList(pairs)
      return func (x any) any {
        switch v := x.(type) {
          case core.TermVariable:
          return func (name core.Name) any {
            return libeithers.Bind(RequireElement(cx, graph, name)).(func(any) any)(func (el core.Binding) any {
              return MatchUnion(cx, graph, tname, pairs, func (v any) any {
                return v.(core.Binding).Term
              }(el).(core.Term))
            })
          }(v.Value)
          case core.TermUnion:
          return func (injection core.Injection) any {
            return func () any {
              var exp any = func () any {
                var fname any = func (v any) any {
                  return v.(core.Injection).Field
                }(injection).(core.Field).Name
                return func () any {
                  var val any = func (v any) any {
                    return v.(core.Injection).Field
                  }(injection).(core.Field).Term
                  return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("no matching case for field \"").(func(any) any)(fname)).(func(any) any)("\" in union type ")).(func(any) any)(func (v any) any {
                    return v
                  }(tname)).(string))}, Context: cx}}).(func(any) any)(func (f func(core.Term) any) any {
                    return f(val.(core.Term))
                  }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(mapping))
                }()
              }()
              return liblogic.IfElse(libequality.Equal(func (v any) any {
                return v.(core.Injection).TypeName
              }(injection)).(func(any) any)(func (v any) any {
                return v
              }(tname))).(func(any) any)(exp).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("expected injection for type ").(func(any) any)(func (v any) any {
                return v
              }(tname))).(func(any) any)(", got ")).(func(any) any)(showcore.Term(term)).(string))}, Context: cx}})
            }()
          }(v.Value)
          default:
          return [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat([]any{"expected inject(", func (v any) any {
            return v
          }(tname), ") with one of {", libstrings.Intercalate(", ").(func(any) any)(liblists.Map(func (pair any) any {
            return libpairs.First(pair)
          }).(func(any) any)(pairs)), "}, got ", showcore.Term(stripped.(core.Term))}).(string))}, Context: cx}}
        }
        return nil
      }(stripped)
    }()
  }()
}

func MatchUnitField[T0, T1 any] (fname T0, x T1) any {
  return [2]any{fname, func (ignored any) any {
    return [2]any{"right", x}
  }}
}

func RequireElement (cx context.Context, graph graph.Graph, name core.Name) any {
  return func () any {
    var showAll any = false
    return func () any {
      ellipsis := func (strings []any) any {
        return liblogic.IfElse(liblogic.And(libequality.Gt(liblists.Length(strings)).(func(any) any)(3)).(func(any) any)(liblogic.Not(showAll))).(func(any) any)(liblists.Concat2(liblists.Take(3).(func(any) any)(strings)).(func(any) any)([]any{"..."})).(func(any) any)(strings)
      }
      return func () any {
        var errMsg any = libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("no such element: ").(func(any) any)(func (v any) any {
          return v
        }(name))).(func(any) any)(". Available elements: {")).(func(any) any)(libstrings.Intercalate(", ").(func(any) any)(ellipsis(liblists.Map(func (v any) any {
          return v
        }).(func(any) any)(libmaps.Keys(func (v any) any {
          return v.(graph.Graph).BoundTerms
        }(graph))).([]any))))).(func(any) any)("}")
        return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(errMsg.(string))}, Context: cx}}).(func(any) any)(func (x core.Binding) any {
          return [2]any{"right", x}
        }).(func(any) any)(DereferenceElement(graph, name))
      }()
    }()
  }()
}

func RequirePrimitive (cx context.Context, graph graph.Graph, name core.Name) any {
  return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("no such primitive function: ").(func(any) any)(func (v any) any {
    return v
  }(name)).(string))}, Context: cx}}).(func(any) any)(func (x graph.Primitive) any {
    return [2]any{"right", x}
  }).(func(any) any)(LookupPrimitive(graph, name))
}

func RequirePrimitiveType (cx context.Context, tx graph.Graph, name core.Name) any {
  return func () any {
    var mts any = libmaps.Lookup(name).(func(any) any)(libmaps.FromList(liblists.Map(func (_gpt_p graph.Primitive) any {
      return [2]any{func (v any) any {
        return v.(graph.Primitive).Name
      }(_gpt_p), func (v any) any {
        return v.(graph.Primitive).Type_
      }(_gpt_p)}
    }).(func(any) any)(libmaps.Elems(func (v any) any {
      return v.(graph.Graph).Primitives
    }(tx)))))
    return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("no such primitive function: ").(func(any) any)(func (v any) any {
      return v
    }(name)).(string))}, Context: cx}}).(func(any) any)(func (ts core.TypeScheme) any {
      return [2]any{"right", ts}
    }).(func(any) any)(mts)
  }()
}

func RequireTerm (cx context.Context, graph graph.Graph, name core.Name) any {
  return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("no such element: ").(func(any) any)(func (v any) any {
    return v
  }(name)).(string))}, Context: cx}}).(func(any) any)(func (x core.Term) any {
    return [2]any{"right", x}
  }).(func(any) any)(ResolveTerm(graph, name))
}

func ResolveTerm (graph graph.Graph, name core.Name) any {
  return func () any {
    recurse := func (term core.Term) any {
      return func () any {
        var stripped any = rewriting.DeannotateTerm(term)
        return func (x any) any {
          switch v := x.(type) {
            case core.TermVariable:
            return func (name_ core.Name) any {
              return ResolveTerm(graph, name_)
            }(v.Value)
            default:
            return func () any {
              _v := term
              return &_v
            }()
          }
          return nil
        }(stripped)
      }()
    }
    return libmaybes.Maybe(nil).(func(any) any)(recurse).(func(any) any)(LookupTerm(graph, name))
  }()
}

func StripAndDereferenceTerm (cx context.Context, graph graph.Graph, term core.Term) any {
  return func () any {
    var stripped any = rewriting.DeannotateAndDetypeTerm(term)
    return func (x any) any {
      switch v := x.(type) {
        case core.TermVariable:
        return func (v core.Name) any {
          return libeithers.Bind(RequireTerm(cx, graph, v)).(func(any) any)(func (t core.Term) any {
            return StripAndDereferenceTerm(cx, graph, t)
          })
        }(v.Value)
        default:
        return [2]any{"right", stripped}
      }
      return nil
    }(stripped)
  }()
}

func StripAndDereferenceTermEither (graph graph.Graph, term core.Term) any {
  return func () any {
    var stripped any = rewriting.DeannotateAndDetypeTerm(term)
    return func (x any) any {
      switch v := x.(type) {
        case core.TermVariable:
        return func (v core.Name) any {
          return libeithers.Either(func (left_ string) any {
            return [2]any{"left", left_}
          }).(func(any) any)(func (binding core.Binding) any {
            return StripAndDereferenceTermEither(graph, func (v any) any {
              return v.(core.Binding).Term
            }(binding).(core.Term))
          }).(func(any) any)(DereferenceVariable(graph, v))
        }(v.Value)
        default:
        return [2]any{"right", stripped}
      }
      return nil
    }(stripped)
  }()
}
