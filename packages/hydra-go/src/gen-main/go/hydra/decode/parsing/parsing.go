// Note: this is an automatically generated file. Do not edit.

package decodeparsing

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
  "hydra.dev/hydra/parsing"
)

func ParseError (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("message", func (_p graph.Graph) func(any) any {
              return func (cx2 graph.Graph) func(any) any {
                return func (raw2 any) any {
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
                  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2.(core.Term)))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_message string) any {
              return libeithers.Bind(extracthelpers.RequireField("remainder", func (_p graph.Graph) func(any) any {
                return func (cx2 graph.Graph) func(any) any {
                  return func (raw2 any) any {
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
                    }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2.(core.Term)))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_remainder string) any {
                return [2]any{"right", parsing.ParseError{Message: field_message, Remainder: field_remainder}}
              })
            })
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected record")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func ParseResult (a func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("success"), func (input core.Term) any {
              return libeithers.Map(func (t parsing.ParseSuccess[any]) any {
                return parsing.ParseResultSuccess[any]{Value: t}
              }).(func(any) any)(ParseSuccess(a, cx, input))
            }}, [2]any{core.Name("failure"), func (input core.Term) any {
              return libeithers.Map(func (t parsing.ParseError) any {
                return parsing.ParseResultFailure[any]{Value: t}
              }).(func(any) any)(ParseError(cx, input))
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

func ParseSuccess (a func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("value", a, fieldMap.([]any), cx)).(func(any) any)(func (field_value any) any {
              return libeithers.Bind(extracthelpers.RequireField("remainder", func (_p graph.Graph) func(any) any {
                return func (cx2 graph.Graph) func(any) any {
                  return func (raw2 any) any {
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
                    }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2.(core.Term)))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_remainder string) any {
                return [2]any{"right", parsing.ParseSuccess[any]{Value: field_value, Remainder: field_remainder}}
              })
            })
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected record")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}
