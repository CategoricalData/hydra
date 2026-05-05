// Note: this is an automatically generated file. Do not edit.

package decodeaccessors

import (
  "hydra.dev/hydra/accessors"
  "hydra.dev/hydra/core"
  decodecore "hydra.dev/hydra/decode/core"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
)

func AccessorEdge (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("source", AccessorNode, fieldMap.([]any), cx)).(func(any) any)(func (field_source accessors.AccessorNode) any {
              return libeithers.Bind(extracthelpers.RequireField("path", AccessorPath, fieldMap.([]any), cx)).(func(any) any)(func (field_path accessors.AccessorPath) any {
                return libeithers.Bind(extracthelpers.RequireField("target", AccessorNode, fieldMap.([]any), cx)).(func(any) any)(func (field_target accessors.AccessorNode) any {
                  return [2]any{"right", accessors.AccessorEdge{Source: field_source, Path: field_path, Target: field_target}}
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

func AccessorGraph (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("nodes", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeList(AccessorNode, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_nodes []any) any {
              return libeithers.Bind(extracthelpers.RequireField("edges", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(AccessorEdge, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_edges []any) any {
                return [2]any{"right", accessors.AccessorGraph{Nodes: field_nodes, Edges: field_edges}}
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

func AccessorNode (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("name", decodecore.Name, fieldMap.([]any), cx)).(func(any) any)(func (field_name core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("label", func (_p graph.Graph) func(any) any {
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
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_label string) any {
                return libeithers.Bind(extracthelpers.RequireField("id", func (_p graph.Graph) func(any) any {
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
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_id string) any {
                  return [2]any{"right", accessors.AccessorNode{Name: field_name, Label: field_label, Id: field_id}}
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

func AccessorPath (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b []any) any {
            return accessors.AccessorPath(b)
          }).(func(any) any)(extracthelpers.DecodeList(TermAccessor, cx, func (v any) any {
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

func TermAccessor (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("annotatedBody"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorAnnotatedBody{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("applicationFunction"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorApplicationFunction{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("applicationArgument"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorApplicationArgument{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("lambdaBody"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorLambdaBody{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("unionCasesDefault"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorUnionCasesDefault{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("unionCasesBranch"), func (input core.Term) any {
              return libeithers.Map(func (t core.Name) any {
                return accessors.TermAccessorUnionCasesBranch{Value: t}
              }).(func(any) any)(decodecore.Name(cx, input))
            }}, [2]any{core.Name("letBody"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorLetBody{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("letBinding"), func (input core.Term) any {
              return libeithers.Map(func (t core.Name) any {
                return accessors.TermAccessorLetBinding{Value: t}
              }).(func(any) any)(decodecore.Name(cx, input))
            }}, [2]any{core.Name("listElement"), func (input core.Term) any {
              return libeithers.Map(func (t int32) any {
                return accessors.TermAccessorListElement{Value: t}
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
            }}, [2]any{core.Name("mapKey"), func (input core.Term) any {
              return libeithers.Map(func (t int32) any {
                return accessors.TermAccessorMapKey{Value: t}
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
            }}, [2]any{core.Name("mapValue"), func (input core.Term) any {
              return libeithers.Map(func (t int32) any {
                return accessors.TermAccessorMapValue{Value: t}
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
            }}, [2]any{core.Name("maybeTerm"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorMaybeTerm{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("productTerm"), func (input core.Term) any {
              return libeithers.Map(func (t int32) any {
                return accessors.TermAccessorProductTerm{Value: t}
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
            }}, [2]any{core.Name("recordField"), func (input core.Term) any {
              return libeithers.Map(func (t core.Name) any {
                return accessors.TermAccessorRecordField{Value: t}
              }).(func(any) any)(decodecore.Name(cx, input))
            }}, [2]any{core.Name("setElement"), func (input core.Term) any {
              return libeithers.Map(func (t int32) any {
                return accessors.TermAccessorSetElement{Value: t}
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
            }}, [2]any{core.Name("sumTerm"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorSumTerm{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("typeLambdaBody"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorTypeLambdaBody{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("typeApplicationTerm"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorTypeApplicationTerm{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("injectionTerm"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorInjectionTerm{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("wrappedTerm"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return accessors.TermAccessorWrappedTerm{Value: t}
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
