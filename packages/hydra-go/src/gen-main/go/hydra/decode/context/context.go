// Note: this is an automatically generated file. Do not edit.

package decodecontext

import (
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  decodecore "hydra.dev/hydra/decode/core"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
)

func Context (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("trace", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
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
                  }, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_trace []any) any {
              return libeithers.Bind(extracthelpers.RequireField("messages", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
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
                    }, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_messages []any) any {
                return libeithers.Bind(extracthelpers.RequireField("other", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeMap(decodecore.Name, decodecore.Term, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_other []any) any {
                  return [2]any{"right", context.Context{Trace: field_trace, Messages: field_messages, Other: field_other}}
                })
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

func InContext (e func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("object", e, fieldMap.([]any), cx)).(func(any) any)(func (field_object any) any {
              return libeithers.Bind(extracthelpers.RequireField("context", Context, fieldMap.([]any), cx)).(func(any) any)(func (field_context context.Context) any {
                return [2]any{"right", context.InContext[any]{Object: field_object, Context: field_context}}
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
