// Note: this is an automatically generated file. Do not edit.

package annotations

import (
  "hydra.dev/hydra/classes"
  "hydra.dev/hydra/constants"
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  decodecore "hydra.dev/hydra/decode/core"
  encodecore "hydra.dev/hydra/encode/core"
  "hydra.dev/hydra/error"
  extractcore "hydra.dev/hydra/extract/core"
  "hydra.dev/hydra/graph"
  libeithers "hydra.dev/hydra/lib/eithers"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
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

func AggregateAnnotations[T0, T1 any] (getValue func(T0) any, getX func(T1) T0, getAnns func(T1) []any, t T0) []any {
  return func () any {
    var toPairs func([]any) any
    toPairs = func (rest []any) any {
      return func (t2 T0) any {
        return libmaybes.Maybe(rest).(func(any) any)(func (yy T1) any {
          return toPairs(liblists.Cons(libmaps.ToList(getAnns(yy))).(func(any) any)(rest).([]any)).(func(any) any)(getX(yy))
        }).(func(any) any)(getValue(t2))
      }
    }
    return libmaps.FromList(liblists.Concat(toPairs([]any{}).(func(any) any)(t)))
  }().([]any)
}

func DebugIf (cx context.Context, debugId string, message string) any {
  return libeithers.Bind(GetDebugId(cx)).(func(any) any)(func (mid any) any {
    return liblogic.IfElse(libequality.Equal(mid).(func(any) any)(func () any {
      _v := debugId
      return &_v
    }())).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(message)}, Context: cx}}).(func(any) any)([2]any{"right", struct{}{}})
  })
}

func FailOnFlag (cx context.Context, flag core.Name, msg string) any {
  return libeithers.Bind(HasFlag(cx, flag)).(func(any) any)(func (val bool) any {
    return liblogic.IfElse(val).(func(any) any)([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(msg)}, Context: cx}}).(func(any) any)([2]any{"right", struct{}{}})
  })
}

func GetDebugId (cx context.Context) any {
  return libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (term core.Term) any {
    return libeithers.Map(libmaybes.Pure).(func(any) any)(extractcore.String_(cx, graph.Graph{BoundTerms: libmaps.Empty, BoundTypes: libmaps.Empty, ClassConstraints: libmaps.Empty, LambdaVariables: libsets.Empty, Metadata: libmaps.Empty, Primitives: libmaps.Empty, SchemaTypes: libmaps.Empty, TypeVariables: libsets.Empty}, term))
  }).(func(any) any)(GetAttr(constants.Key_debugId, cx))
}

func GetAttr (key core.Name, cx context.Context) any {
  return libmaps.Lookup(key).(func(any) any)(func (v any) any {
    return v.(context.Context).Other
  }(cx))
}

func GetAttrWithDefault (key core.Name, def core.Term, cx context.Context) core.Term {
  return libmaybes.FromMaybe(def).(func(any) any)(GetAttr(key, cx)).(core.Term)
}

func GetCount (key core.Name, cx context.Context) int32 {
  return libmaybes.Maybe(0).(func(any) any)(func (term core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermLiteral:
        return func (lit core.Literal) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.LiteralInteger:
              return func (iv core.IntegerValue) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.IntegerValueInt32_:
                    return func (i int32) any {
                      return i
                    }(v.Value)
                    default:
                    return 0
                  }
                  return nil
                }(iv)
              }(v.Value)
              default:
              return 0
            }
            return nil
          }(lit)
        }(v.Value)
        default:
        return 0
      }
      return nil
    }(term)
  }).(func(any) any)(libmaps.Lookup(key).(func(any) any)(func (v any) any {
    return v.(context.Context).Other
  }(cx))).(int32)
}

func GetDescription (cx context.Context, graph graph.Graph, anns []any) any {
  return libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (term core.Term) any {
    return libeithers.Map(libmaybes.Pure).(func(any) any)(extractcore.String_(cx, graph, term))
  }).(func(any) any)(libmaps.Lookup(core.Name("description")).(func(any) any)(anns))
}

func GetTermAnnotation (key core.Name, term core.Term) any {
  return libmaps.Lookup(key).(func(any) any)(TermAnnotationInternal(term))
}

func GetTermDescription (cx context.Context, graph graph.Graph, term core.Term) any {
  return func () any {
    var peel func(core.Term) any
    peel = func (t core.Term) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.TermTypeLambda:
          return func (tl core.TypeLambda) any {
            return peel(func (v any) any {
              return v.(core.TypeLambda).Body
            }(tl).(core.Term))
          }(v.Value)
          case core.TermTypeApplication:
          return func (ta core.TypeApplicationTerm) any {
            return peel(func (v any) any {
              return v.(core.TypeApplicationTerm).Body
            }(ta).(core.Term))
          }(v.Value)
          default:
          return t
        }
        return nil
      }(t)
    }
    return GetDescription(cx, graph, TermAnnotationInternal(peel(term).(core.Term)))
  }()
}

func GetType (graph graph.Graph, anns []any) any {
  return libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (dat core.Term) any {
    return libeithers.Map(libmaybes.Pure).(func(any) any)(decodecore.Type_(graph, dat))
  }).(func(any) any)(libmaps.Lookup(constants.Key_type).(func(any) any)(anns))
}

func GetTypeAnnotation (key core.Name, typ core.Type) any {
  return libmaps.Lookup(key).(func(any) any)(TypeAnnotationInternal(typ))
}

func GetTypeClasses (cx context.Context, graph graph.Graph, term core.Term) any {
  return func () any {
    decodeClass := func (term2 core.Term) any {
      return func () any {
        var byName any = libmaps.FromList([]any{[2]any{core.Name("equality"), classes.TypeClassEquality{}}, [2]any{core.Name("ordering"), classes.TypeClassOrdering{}}})
        return libeithers.Bind(extractcore.UnitVariant(cx, core.Name("hydra.classes.TypeClass"), graph, term2)).(func(any) any)(func (fn core.Name) any {
          return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("unexpected: expected type class, got ").(func(any) any)(showcore.Term(term2)).(string))}, Context: cx}}).(func(any) any)(func (x classes.TypeClass) any {
            return [2]any{"right", x}
          }).(func(any) any)(libmaps.Lookup(fn).(func(any) any)(byName))
        })
      }()
    }
    return libmaybes.Maybe([2]any{"right", libmaps.Empty}).(func(any) any)(func (term2 core.Term) any {
      return extractcore.Map_(cx, func (_p core.Term) any {
        return func (t core.Term) any {
          return libeithers.Bimap(func (de error.DecodingError) any {
            return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(func (v any) any {
              return v
            }(de).(string))}, Context: cx}
          }).(func(any) any)(func (x core.Name) any {
            return x
          }).(func(any) any)(decodecore.Name(graph, t))
        }(_p)
      }, func (_p core.Term) any {
        return func (v1 core.Term) any {
          return extractcore.SetOf(cx, decodeClass, graph, v1)
        }(_p)
      }, graph, term2)
    }).(func(any) any)(GetTermAnnotation(constants.Key_classes, term))
  }()
}

func GetTypeDescription (cx context.Context, graph graph.Graph, typ core.Type) any {
  return GetDescription(cx, graph, TypeAnnotationInternal(typ))
}

func IsNativeType (el core.Binding) bool {
  return func () any {
    var isFlaggedAsFirstClassType any = libmaybes.FromMaybe(false).(func(any) any)(libmaybes.Map(func (_ core.Term) any {
      return true
    }).(func(any) any)(GetTermAnnotation(constants.Key_firstClassType, func (v any) any {
      return v.(core.Binding).Term
    }(el).(core.Term))))
    return libmaybes.Maybe(false).(func(any) any)(func (ts core.TypeScheme) any {
      return liblogic.And(libequality.Equal(ts).(func(any) any)(core.TypeScheme{Variables: []any{}, Type_: core.TypeVariable{Value: core.Name("hydra.core.Type")}, Constraints: nil})).(func(any) any)(liblogic.Not(isFlaggedAsFirstClassType))
    }).(func(any) any)(func (v any) any {
      return v.(core.Binding).Type_
    }(el))
  }().(bool)
}

func HasDescription (anns []any) bool {
  return libmaybes.IsJust(libmaps.Lookup(constants.Key_description).(func(any) any)(anns)).(bool)
}

func HasFlag (cx context.Context, flag core.Name) any {
  return func () any {
    var term any = GetAttrWithDefault(flag, core.TermLiteral{Value: core.LiteralBoolean{Value: false}}, cx)
    return extractcore.Boolean(cx, graph.Graph{BoundTerms: libmaps.Empty, BoundTypes: libmaps.Empty, ClassConstraints: libmaps.Empty, LambdaVariables: libsets.Empty, Metadata: libmaps.Empty, Primitives: libmaps.Empty, SchemaTypes: libmaps.Empty, TypeVariables: libsets.Empty}, term.(core.Term))
  }()
}

func HasTypeDescription (typ core.Type) bool {
  return HasDescription(TypeAnnotationInternal(typ))
}

func NextCount (key core.Name, cx context.Context) any {
  return func () any {
    var count any = GetCount(key, cx)
    return [2]any{count, PutCount(key, libmath.Add(count).(func(any) any)(1).(int32), cx)}
  }()
}

func NormalizeTermAnnotations (term core.Term) core.Term {
  return func () any {
    var anns any = TermAnnotationInternal(term)
    return func () any {
      var stripped any = rewriting.DeannotateTerm(term)
      return liblogic.IfElse(libmaps.Null(anns)).(func(any) any)(stripped).(func(any) any)(core.TermAnnotated{Value: core.AnnotatedTerm{Body: stripped.(core.Term), Annotation: anns.([]any)}})
    }()
  }().(core.Term)
}

func NormalizeTypeAnnotations (typ core.Type) core.Type {
  return func () any {
    var anns any = TypeAnnotationInternal(typ)
    return func () any {
      var stripped any = rewriting.DeannotateType(typ)
      return liblogic.IfElse(libmaps.Null(anns)).(func(any) any)(stripped).(func(any) any)(core.TypeAnnotated{Value: core.AnnotatedType{Body: stripped.(core.Type), Annotation: anns.([]any)}})
    }()
  }().(core.Type)
}

func PutAttr (key core.Name, val core.Term, cx context.Context) context.Context {
  return context.Context{Trace: func (v any) any {
    return v.(context.Context).Trace
  }(cx).([]any), Messages: func (v any) any {
    return v.(context.Context).Messages
  }(cx).([]any), Other: libmaps.Insert(key).(func(any) any)(val).(func(any) any)(func (v any) any {
    return v.(context.Context).Other
  }(cx)).([]any)}
}

func PutCount (key core.Name, count int32, cx context.Context) context.Context {
  return PutAttr(key, core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: count}}}, cx)
}

func ResetCount (key core.Name, cx context.Context) context.Context {
  return PutAttr(key, core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: 0}}}, cx)
}

func SetAnnotation[T0 any] (key T0, val any, m []any) []any {
  return libmaps.Alter(func (_ any) any {
    return val
  }).(func(any) any)(key).(func(any) any)(m).([]any)
}

func SetDescription (d any, v1 []any) []any {
  return SetAnnotation(constants.Key_description, libmaybes.Map(func (arg_ string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: arg_}}
  }).(func(any) any)(d), v1)
}

func SetTermAnnotation (key core.Name, val any, term core.Term) core.Term {
  return func () any {
    var term_ any = rewriting.DeannotateTerm(term)
    return func () any {
      var anns any = SetAnnotation(key, val, TermAnnotationInternal(term))
      return liblogic.IfElse(libmaps.Null(anns)).(func(any) any)(term_).(func(any) any)(core.TermAnnotated{Value: core.AnnotatedTerm{Body: term_.(core.Term), Annotation: anns.([]any)}})
    }()
  }().(core.Term)
}

func SetTermDescription (d any, v1 core.Term) core.Term {
  return SetTermAnnotation(constants.Key_description, libmaybes.Map(func (s string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: s}}
  }).(func(any) any)(d), v1)
}

func SetType (mt any, v1 []any) []any {
  return SetAnnotation(constants.Key_type, libmaybes.Map(encodecore.Type_).(func(any) any)(mt), v1)
}

func SetTypeAnnotation (key core.Name, val any, typ core.Type) core.Type {
  return func () any {
    var typ_ any = rewriting.DeannotateType(typ)
    return func () any {
      var anns any = SetAnnotation(key, val, TypeAnnotationInternal(typ))
      return liblogic.IfElse(libmaps.Null(anns)).(func(any) any)(typ_).(func(any) any)(core.TypeAnnotated{Value: core.AnnotatedType{Body: typ_.(core.Type), Annotation: anns.([]any)}})
    }()
  }().(core.Type)
}

func SetTypeClasses (m []any, term core.Term) core.Term {
  return func () any {
    encodeClass := func (tc classes.TypeClass) any {
      return func (x any) any {
        switch v := x.(type) {
          case classes.TypeClassEquality:
          return func (_ struct{}) any {
            return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.classes.TypeClass"), Field: core.Field{Name: core.Name("equality"), Term: core.TermUnit{}}}}
          }(v)
          case classes.TypeClassOrdering:
          return func (_ struct{}) any {
            return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.classes.TypeClass"), Field: core.Field{Name: core.Name("ordering"), Term: core.TermUnit{}}}}
          }(v)
        }
        return nil
      }(tc)
    }
    return func () any {
      encodePair := func (nameClasses any) any {
        return func () any {
          var name any = libpairs.First(nameClasses)
          return func () any {
            var classes any = libpairs.Second(nameClasses)
            return [2]any{encodecore.Name(name.(core.Name)), core.TermSet{Value: libsets.FromList(liblists.Map(encodeClass).(func(any) any)(libsets.ToList(classes))).([]any)}}
          }()
        }()
      }
      return func () any {
        var encoded any = liblogic.IfElse(libmaps.Null(m)).(func(any) any)(nil).(func(any) any)(func () any {
          _v := core.TermMap_{Value: libmaps.FromList(liblists.Map(encodePair).(func(any) any)(libmaps.ToList(m))).([]any)}
          return &_v
        }())
        return SetTermAnnotation(constants.Key_classes, encoded, term)
      }()
    }()
  }().(core.Term)
}

func SetTypeDescription (d any, v1 core.Type) core.Type {
  return SetTypeAnnotation(constants.Key_description, libmaybes.Map(func (arg_ string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: arg_}}
  }).(func(any) any)(d), v1)
}

func TermAnnotationInternal (term core.Term) []any {
  return func () any {
    getAnn := func (t core.Term) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.TermAnnotated:
          return func (a core.AnnotatedTerm) any {
            return func () any {
              _v := a
              return &_v
            }()
          }(v.Value)
          default:
          return nil
        }
        return nil
      }(t)
    }
    return AggregateAnnotations(getAnn, func (_p any) core.Term {
      return func (at any) core.Term {
        return at.(core.AnnotatedTerm).Body
      }(_p).(core.Term)
    }, func (_p any) []any {
      return func (at any) []any {
        return at.(core.AnnotatedTerm).Annotation
      }(_p).([]any)
    }, term)
  }().([]any)
}

func TypeAnnotationInternal (typ core.Type) []any {
  return func () any {
    getAnn := func (t core.Type) any {
      return func (x any) any {
        switch v := x.(type) {
          case core.TypeAnnotated:
          return func (a core.AnnotatedType) any {
            return func () any {
              _v := a
              return &_v
            }()
          }(v.Value)
          default:
          return nil
        }
        return nil
      }(t)
    }
    return AggregateAnnotations(getAnn, func (_p any) core.Type {
      return func (at any) core.Type {
        return at.(core.AnnotatedType).Body
      }(_p).(core.Type)
    }, func (_p any) []any {
      return func (at any) []any {
        return at.(core.AnnotatedType).Annotation
      }(_p).([]any)
    }, typ)
  }().([]any)
}

func TypeElement (name core.Name, typ core.Type) core.Binding {
  return func () any {
    var schemaTerm any = core.TermVariable{Value: core.Name("hydra.core.Type")}
    return func () any {
      var dataTerm any = NormalizeTermAnnotations(core.TermAnnotated{Value: core.AnnotatedTerm{Body: encodecore.Type_(typ), Annotation: libmaps.FromList([]any{[2]any{constants.Key_type, schemaTerm}}).([]any)}})
      return core.Binding{Name: name, Term: dataTerm.(core.Term), Type_: func () any {
        _v := core.TypeScheme{Variables: []any{}, Type_: core.TypeVariable{Value: core.Name("hydra.core.Type")}, Constraints: nil}
        return &_v
      }()}
    }()
  }().(core.Binding)
}

func WhenFlag (cx context.Context, flag core.Name, ethen any, eelse any) any {
  return libeithers.Bind(HasFlag(cx, flag)).(func(any) any)(func (b bool) any {
    return liblogic.IfElse(b).(func(any) any)(ethen).(func(any) any)(eelse)
  })
}
