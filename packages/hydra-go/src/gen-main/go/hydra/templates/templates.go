// Note: this is an automatically generated file. Do not edit.

package templates

import (
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  decodecore "hydra.dev/hydra/decode/core"
  "hydra.dev/hydra/error"
  "hydra.dev/hydra/graph"
  libeithers "hydra.dev/hydra/lib/eithers"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  showcore "hydra.dev/hydra/show/core"
  "math/big"
)

func GraphToSchema (cx context.Context, graph graph.Graph, els []any) any {
  return func () any {
    toPair := func (el core.Binding) any {
      return func () any {
        var name any = func (v any) any {
          return v.(core.Binding).Name
        }(el)
        return libeithers.Bind(libeithers.Bimap(func (_wc_e error.DecodingError) any {
          return context.InContext[error.DecodingError]{Object: _wc_e, Context: cx}
        }).(func(any) any)(func (_wc_a core.Type) any {
          return _wc_a
        }).(func(any) any)(decodecore.Type_(graph, func (v any) any {
          return v.(core.Binding).Term
        }(el).(core.Term)))).(func(any) any)(func (t core.Type) any {
          return [2]any{"right", [2]any{name, t}}
        })
      }()
    }
    return libeithers.Bind(libeithers.MapList(toPair).(func(any) any)(els)).(func(any) any)(func (pairs []any) any {
      return [2]any{"right", libmaps.FromList(pairs)}
    })
  }()
}

func InstantiateTemplate (cx context.Context, minimal bool, schema []any, tname core.Name, t core.Type) any {
  return func () any {
    inst := func (tn core.Name) any {
      return func (v1 core.Type) any {
        return InstantiateTemplate(cx, minimal, schema, tn, v1)
      }
    }
    return func () any {
      var noPoly any = [2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError("Polymorphic and function types are not currently supported")}, Context: cx}}
      return func () any {
        forFloat := func (ft core.FloatType) any {
          return func (x any) any {
            switch v := x.(type) {
              case core.FloatTypeBigfloat:
              return func (_ struct{}) any {
                return core.FloatValueBigfloat{Value: 0.0}
              }(v)
              case core.FloatTypeFloat32_:
              return func (_ struct{}) any {
                return core.FloatValueFloat32_{Value: 0.0}
              }(v)
              case core.FloatTypeFloat64_:
              return func (_ struct{}) any {
                return core.FloatValueFloat64_{Value: 0.0}
              }(v)
            }
            return nil
          }(ft)
        }
        return func () any {
          forInteger := func (it core.IntegerType) any {
            return func (x any) any {
              switch v := x.(type) {
                case core.IntegerTypeBigint:
                return func (_ struct{}) any {
                  return core.IntegerValueBigint{Value: big.NewInt(0)}
                }(v)
                case core.IntegerTypeInt8_:
                return func (_ struct{}) any {
                  return core.IntegerValueInt8_{Value: 0}
                }(v)
                case core.IntegerTypeInt16_:
                return func (_ struct{}) any {
                  return core.IntegerValueInt16_{Value: 0}
                }(v)
                case core.IntegerTypeInt32_:
                return func (_ struct{}) any {
                  return core.IntegerValueInt32_{Value: 0}
                }(v)
                case core.IntegerTypeInt64_:
                return func (_ struct{}) any {
                  return core.IntegerValueInt64_{Value: 0}
                }(v)
                case core.IntegerTypeUint8_:
                return func (_ struct{}) any {
                  return core.IntegerValueUint8_{Value: 0}
                }(v)
                case core.IntegerTypeUint16_:
                return func (_ struct{}) any {
                  return core.IntegerValueUint16_{Value: 0}
                }(v)
                case core.IntegerTypeUint32_:
                return func (_ struct{}) any {
                  return core.IntegerValueUint32_{Value: 0}
                }(v)
                case core.IntegerTypeUint64_:
                return func (_ struct{}) any {
                  return core.IntegerValueUint64_{Value: 0}
                }(v)
              }
              return nil
            }(it)
          }
          return func () any {
            forLiteral := func (lt core.LiteralType) any {
              return func (x any) any {
                switch v := x.(type) {
                  case core.LiteralTypeBinary:
                  return func (_ struct{}) any {
                    return core.LiteralString_{Value: ""}
                  }(v)
                  case core.LiteralTypeBoolean:
                  return func (_ struct{}) any {
                    return core.LiteralBoolean{Value: false}
                  }(v)
                  case core.LiteralTypeInteger:
                  return func (it core.IntegerType) any {
                    return core.LiteralInteger{Value: forInteger(it).(core.IntegerValue)}
                  }(v.Value)
                  case core.LiteralTypeFloat:
                  return func (ft core.FloatType) any {
                    return core.LiteralFloat{Value: forFloat(ft).(core.FloatValue)}
                  }(v.Value)
                  case core.LiteralTypeString_:
                  return func (_ struct{}) any {
                    return core.LiteralString_{Value: ""}
                  }(v)
                }
                return nil
              }(lt)
            }
            return func (x any) any {
              switch v := x.(type) {
                case core.TypeAnnotated:
                return func (at core.AnnotatedType) any {
                  return inst(tname).(func(any) any)(func (v any) any {
                    return v.(core.AnnotatedType).Body
                  }(at))
                }(v.Value)
                case core.TypeApplication:
                return func (_ core.ApplicationType) any {
                  return noPoly
                }(v.Value)
                case core.TypeFunction:
                return func (_ core.FunctionType) any {
                  return noPoly
                }(v.Value)
                case core.TypeForall:
                return func (_ core.ForallType) any {
                  return noPoly
                }(v.Value)
                case core.TypeList:
                return func (et core.Type) any {
                  return liblogic.IfElse(minimal).(func(any) any)([2]any{"right", core.TermList{Value: []any{}}}).(func(any) any)(libeithers.Bind(inst(tname).(func(any) any)(et)).(func(any) any)(func (e core.Term) any {
                    return [2]any{"right", core.TermList{Value: []any{e}}}
                  }))
                }(v.Value)
                case core.TypeLiteral:
                return func (lt core.LiteralType) any {
                  return [2]any{"right", core.TermLiteral{Value: forLiteral(lt).(core.Literal)}}
                }(v.Value)
                case core.TypeMap_:
                return func (mt core.MapType) any {
                  return func () any {
                    var kt any = func (v any) any {
                      return v.(core.MapType).Keys
                    }(mt)
                    return func () any {
                      var vt any = func (v any) any {
                        return v.(core.MapType).Values
                      }(mt)
                      return liblogic.IfElse(minimal).(func(any) any)([2]any{"right", core.TermMap_{Value: libmaps.Empty}}).(func(any) any)(libeithers.Bind(inst(tname).(func(any) any)(kt)).(func(any) any)(func (ke core.Term) any {
                        return libeithers.Bind(inst(tname).(func(any) any)(vt)).(func(any) any)(func (ve core.Term) any {
                          return [2]any{"right", core.TermMap_{Value: libmaps.Singleton(ke).(func(any) any)(ve).([]any)}}
                        })
                      }))
                    }()
                  }()
                }(v.Value)
                case core.TypeMaybe:
                return func (ot core.Type) any {
                  return liblogic.IfElse(minimal).(func(any) any)([2]any{"right", core.TermMaybe{Value: nil}}).(func(any) any)(libeithers.Bind(inst(tname).(func(any) any)(ot)).(func(any) any)(func (e core.Term) any {
                    return [2]any{"right", core.TermMaybe{Value: func () any {
                      _v := e
                      return &_v
                    }()}}
                  }))
                }(v.Value)
                case core.TypeRecord:
                return func (rt []any) any {
                  return func () any {
                    toField := func (ft core.FieldType) any {
                      return libeithers.Bind(inst(tname).(func(any) any)(func (v any) any {
                        return v.(core.FieldType).Type_
                      }(ft))).(func(any) any)(func (e core.Term) any {
                        return [2]any{"right", core.Field{Name: func (v any) any {
                          return v.(core.FieldType).Name
                        }(ft).(core.Name), Term: e}}
                      })
                    }
                    return libeithers.Bind(libeithers.MapList(toField).(func(any) any)(rt)).(func(any) any)(func (dfields []any) any {
                      return [2]any{"right", core.TermRecord{Value: core.Record{TypeName: tname, Fields: dfields}}}
                    })
                  }()
                }(v.Value)
                case core.TypeSet:
                return func (et core.Type) any {
                  return liblogic.IfElse(minimal).(func(any) any)([2]any{"right", core.TermSet{Value: libsets.Empty}}).(func(any) any)(libeithers.Bind(inst(tname).(func(any) any)(et)).(func(any) any)(func (e core.Term) any {
                    return [2]any{"right", core.TermSet{Value: libsets.FromList([]any{e}).([]any)}}
                  }))
                }(v.Value)
                case core.TypeVariable:
                return func (vname core.Name) any {
                  return libmaybes.Maybe([2]any{"left", context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(libstrings.Cat2("Type variable ").(func(any) any)(libstrings.Cat2(showcore.Term(core.TermVariable{Value: vname})).(func(any) any)(" not found in schema")).(string))}, Context: cx}}).(func(any) any)(func (v1 core.Type) any {
                    return inst(vname).(func(any) any)(v1)
                  }).(func(any) any)(libmaps.Lookup(vname).(func(any) any)(schema))
                }(v.Value)
                case core.TypeWrap:
                return func (wt core.Type) any {
                  return libeithers.Bind(inst(tname).(func(any) any)(wt)).(func(any) any)(func (e core.Term) any {
                    return [2]any{"right", core.TermWrap{Value: core.WrappedTerm{TypeName: tname, Body: e}}}
                  })
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
