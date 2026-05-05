// Note: this is an automatically generated file. Do not edit.

package decodetopology

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  "hydra.dev/hydra/topology"
)

func Graph (v1 graph.Graph, v2 core.Term) any {
  return extracthelpers.DecodeMap(Vertex, func (_p graph.Graph) func(core.Term) any {
    return func (v12 graph.Graph) func(core.Term) any {
      return func (v22 core.Term) any {
        return extracthelpers.DecodeList(Vertex, v12, v22)
      }
    }(_p).(func(core.Term) any)
  }, v1, v2)
}

func TarjanState (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("counter", func (_p graph.Graph) func(any) any {
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
                  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2.(core.Term)))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_counter int32) any {
              return libeithers.Bind(extracthelpers.RequireField("indices", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeMap(Vertex, func (_p graph.Graph) func(core.Term) any {
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
                                      case core.LiteralInteger:
                                      return func (v12 core.IntegerValue) any {
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
                                        }(v12)
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
                          }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2))
                        }
                      }(_p).(func(core.Term) any)
                    }, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_indices []any) any {
                return libeithers.Bind(extracthelpers.RequireField("lowLinks", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeMap(Vertex, func (_p graph.Graph) func(core.Term) any {
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
                                        case core.LiteralInteger:
                                        return func (v12 core.IntegerValue) any {
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
                                          }(v12)
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
                            }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2))
                          }
                        }(_p).(func(core.Term) any)
                      }, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_lowLinks []any) any {
                  return libeithers.Bind(extracthelpers.RequireField("stack", func (_p graph.Graph) func(any) any {
                    return func (v1 graph.Graph) func(any) any {
                      return func (v2 any) any {
                        return extracthelpers.DecodeList(Vertex, v1, v2.(core.Term))
                      }
                    }(_p).(func(any) any)
                  }, fieldMap.([]any), cx)).(func(any) any)(func (field_stack []any) any {
                    return libeithers.Bind(extracthelpers.RequireField("onStack", func (_p graph.Graph) func(any) any {
                      return func (v1 graph.Graph) func(any) any {
                        return func (v2 any) any {
                          return extracthelpers.DecodeSet(Vertex, v1, v2.(core.Term))
                        }
                      }(_p).(func(any) any)
                    }, fieldMap.([]any), cx)).(func(any) any)(func (field_onStack []any) any {
                      return libeithers.Bind(extracthelpers.RequireField("sccs", func (_p graph.Graph) func(any) any {
                        return func (v1 graph.Graph) func(any) any {
                          return func (v2 any) any {
                            return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                              return func (v12 graph.Graph) func(core.Term) any {
                                return func (v22 core.Term) any {
                                  return extracthelpers.DecodeList(Vertex, v12, v22)
                                }
                              }(_p).(func(core.Term) any)
                            }, v1, v2.(core.Term))
                          }
                        }(_p).(func(any) any)
                      }, fieldMap.([]any), cx)).(func(any) any)(func (field_sccs []any) any {
                        return [2]any{"right", topology.TarjanState{Counter: field_counter, Indices: field_indices, LowLinks: field_lowLinks, Stack: field_stack, OnStack: field_onStack, Sccs: field_sccs}}
                      })
                    })
                  })
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

func Vertex (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
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
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}
