// Note: this is an automatically generated file. Do not edit.

package decodeutil

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/util"
)

func CaseConvention (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("camel"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return util.CaseConventionCamel{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("pascal"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return util.CaseConventionPascal{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("lowerSnake"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return util.CaseConventionLowerSnake{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("upperSnake"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return util.CaseConventionUpperSnake{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
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

func Comparison (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("lessThan"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return util.ComparisonLessThan{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("equalTo"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return util.ComparisonEqualTo{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("greaterThan"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return util.ComparisonGreaterThan{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
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

func Precision (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("arbitrary"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return util.PrecisionArbitrary{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("bits"), func (input core.Term) any {
              return libeithers.Map(func (t int32) any {
                return util.PrecisionBits{Value: t}
              }).(func(any) any)(libeithers.Either(func (err string) any {
                return [2]any{"left", error.DecodingError(err)}
              }).(func(any) any)(func (stripped2 core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermLiteral:
                    return func (v core.Literal) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.LiteralInteger:
                          return func (v1 core.IntegerValue) any {
                            return func (x any) any {
                              switch v := x.(type) {
                                case core.IntegerValueInt32_:
                                return func (i int32) any {
                                  return [2]any{"right", i}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected int32 value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected int32 literal")}
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
