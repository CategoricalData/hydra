// Note: this is an automatically generated file. Do not edit.

package decodejsonmodel

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  jsonmodel "hydra.dev/hydra/json/model"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
)

func Value (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            var fname any = field.(core.Field).Name
            var fterm any = field.(core.Field).Term
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("array"), func (input core.Term) any {
              return libeithers.Map(func (t []any) any {
                return jsonmodel.ValueArray{Value: t}
              }).(func(any) any)(extracthelpers.DecodeList(Value, cx, input))
            }}, [2]any{core.Name("boolean"), func (input core.Term) any {
              return libeithers.Map(func (t bool) any {
                return jsonmodel.ValueBoolean{Value: t}
              }).(func(any) any)(libeithers.Either(func (err string) any {
                return [2]any{"left", error.DecodingError(err)}
              }).(func(any) any)(func (stripped2 core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermLiteral:
                    return func (v core.Literal) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.LiteralBoolean:
                          return func (b bool) any {
                            return [2]any{"right", b}
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected boolean literal")}
                        }
                        return nil
                      }(v)
                    }(v.Value)
                    default:
                    return [2]any{"left", error.DecodingError("expected literal")}
                  }
                  return nil
                }(stripped2)
              }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, input)))
            }}, [2]any{core.Name("null"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return jsonmodel.ValueNull{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("number"), func (input core.Term) any {
              return libeithers.Map(func (t float64) any {
                return jsonmodel.ValueNumber{Value: t}
              }).(func(any) any)(libeithers.Either(func (err string) any {
                return [2]any{"left", error.DecodingError(err)}
              }).(func(any) any)(func (stripped2 core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermLiteral:
                    return func (v core.Literal) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.LiteralFloat:
                          return func (v1 core.FloatValue) any {
                            return func (x any) any {
                              switch v := x.(type) {
                                case core.FloatValueBigfloat:
                                return func (f float64) any {
                                  return [2]any{"right", f}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected bigfloat value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected bigfloat literal")}
                        }
                        return nil
                      }(v)
                    }(v.Value)
                    default:
                    return [2]any{"left", error.DecodingError("expected literal")}
                  }
                  return nil
                }(stripped2)
              }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, input)))
            }}, [2]any{core.Name("object"), func (input core.Term) any {
              return libeithers.Map(func (t []any) any {
                return jsonmodel.ValueObject{Value: t}
              }).(func(any) any)(extracthelpers.DecodeMap(func (_p graph.Graph) func(core.Term) any {
                return func (cx2 graph.Graph) func(core.Term) any {
                  return func (raw2 core.Term) any {
                    return libeithers.Either(func (err string) any {
                      return [2]any{"left", error.DecodingError(err)}
                    }).(func(any) any)(func (stripped2 core.Term) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TermLiteral:
                          return func (v core.Literal) any {
                            return func (x any) any {
                              switch v := x.(type) {
                                case core.LiteralString_:
                                return func (s string) any {
                                  return [2]any{"right", s}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected string literal")}
                              }
                              return nil
                            }(v)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected literal")}
                        }
                        return nil
                      }(stripped2)
                    }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2))
                  }
                }(_p).(func(core.Term) any)
              }, Value, cx, input))
            }}, [2]any{core.Name("string"), func (input core.Term) any {
              return libeithers.Map(func (t string) any {
                return jsonmodel.ValueString_{Value: t}
              }).(func(any) any)(libeithers.Either(func (err string) any {
                return [2]any{"left", error.DecodingError(err)}
              }).(func(any) any)(func (stripped2 core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermLiteral:
                    return func (v core.Literal) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.LiteralString_:
                          return func (s string) any {
                            return [2]any{"right", s}
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected string literal")}
                        }
                        return nil
                      }(v)
                    }(v.Value)
                    default:
                    return [2]any{"left", error.DecodingError("expected literal")}
                  }
                  return nil
                }(stripped2)
              }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, input)))
            }}})
            return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"no such field ", fname, " in union"}).(string))}).(func(any) any)(func (f func(core.Term) any) any {
              return f(fterm.(core.Term))
            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(variantMap))
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected union")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}
