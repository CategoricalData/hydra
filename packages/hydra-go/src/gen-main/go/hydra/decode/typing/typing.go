// Note: this is an automatically generated file. Do not edit.

package decodetyping

import (
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  decodecontext "hydra.dev/hydra/decode/context"
  decodecore "hydra.dev/hydra/decode/core"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  "hydra.dev/hydra/typing"
)

func FunctionStructure (env func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("typeParams", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeList(decodecore.Name, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_typeParams []any) any {
              return libeithers.Bind(extracthelpers.RequireField("params", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(decodecore.Name, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_params []any) any {
                return libeithers.Bind(extracthelpers.RequireField("bindings", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeList(decodecore.Binding, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_bindings []any) any {
                  return libeithers.Bind(extracthelpers.RequireField("body", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_body core.Term) any {
                    return libeithers.Bind(extracthelpers.RequireField("domains", func (_p graph.Graph) func(any) any {
                      return func (v1 graph.Graph) func(any) any {
                        return func (v2 any) any {
                          return extracthelpers.DecodeList(decodecore.Type_, v1, v2.(core.Term))
                        }
                      }(_p).(func(any) any)
                    }, fieldMap.([]any), cx)).(func(any) any)(func (field_domains []any) any {
                      return libeithers.Bind(extracthelpers.RequireField("codomain", func (_p graph.Graph) func(any) any {
                        return func (v1 graph.Graph) func(any) any {
                          return func (v2 any) any {
                            return extracthelpers.DecodeMaybe(decodecore.Type_, v1, v2.(core.Term))
                          }
                        }(_p).(func(any) any)
                      }, fieldMap.([]any), cx)).(func(any) any)(func (field_codomain any) any {
                        return libeithers.Bind(extracthelpers.RequireField("environment", env, fieldMap.([]any), cx)).(func(any) any)(func (field_environment any) any {
                          return [2]any{"right", typing.FunctionStructure[any]{TypeParams: field_typeParams, Params: field_params, Bindings: field_bindings, Body: field_body, Domains: field_domains, Codomain: field_codomain, Environment: field_environment}}
                        })
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

func InferenceResult (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("term", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_term core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("type", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.Type) any {
                return libeithers.Bind(extracthelpers.RequireField("subst", TypeSubst, fieldMap.([]any), cx)).(func(any) any)(func (field_subst typing.TypeSubst) any {
                  return libeithers.Bind(extracthelpers.RequireField("classConstraints", func (_p graph.Graph) func(any) any {
                    return func (v1 graph.Graph) func(any) any {
                      return func (v2 any) any {
                        return extracthelpers.DecodeMap(decodecore.Name, decodecore.TypeVariableMetadata, v1, v2.(core.Term))
                      }
                    }(_p).(func(any) any)
                  }, fieldMap.([]any), cx)).(func(any) any)(func (field_classConstraints []any) any {
                    return libeithers.Bind(extracthelpers.RequireField("context", decodecontext.Context, fieldMap.([]any), cx)).(func(any) any)(func (field_context context.Context) any {
                      return [2]any{"right", typing.InferenceResult{Term: field_term, Type_: field_type, Subst: field_subst, ClassConstraints: field_classConstraints, Context: field_context}}
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

func TermSubst (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b []any) any {
            return typing.TermSubst(b)
          }).(func(any) any)(extracthelpers.DecodeMap(decodecore.Name, decodecore.Term, cx, func (v any) any {
            return v.(core.WrappedTerm).Body
          }(wrappedTerm).(core.Term)))
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected wrapped type")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func TypeConstraint (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("left", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_left core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("right", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_right core.Type) any {
                return libeithers.Bind(extracthelpers.RequireField("comment", func (_p graph.Graph) func(any) any {
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
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_comment string) any {
                  return [2]any{"right", typing.TypeConstraint{Left: field_left, Right: field_right, Comment: field_comment}}
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

func TypeSubst (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b []any) any {
            return typing.TypeSubst(b)
          }).(func(any) any)(extracthelpers.DecodeMap(decodecore.Name, decodecore.Type_, cx, func (v any) any {
            return v.(core.WrappedTerm).Body
          }(wrappedTerm).(core.Term)))
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected wrapped type")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}
