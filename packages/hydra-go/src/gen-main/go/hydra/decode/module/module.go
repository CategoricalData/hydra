// Note: this is an automatically generated file. Do not edit.

package decodemodule

import (
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
  hmodule "hydra.dev/hydra/module"
)

func Definition (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("term"), func (input core.Term) any {
              return libeithers.Map(func (t hmodule.TermDefinition) any {
                return hmodule.DefinitionTerm{Value: t}
              }).(func(any) any)(TermDefinition(cx, input))
            }}, [2]any{core.Name("type"), func (input core.Term) any {
              return libeithers.Map(func (t hmodule.TypeDefinition) any {
                return hmodule.DefinitionType_{Value: t}
              }).(func(any) any)(TypeDefinition(cx, input))
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

func FileExtension (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b string) any {
            return hmodule.FileExtension(b)
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
          }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, func (v any) any {
            return v.(core.WrappedTerm).Body
          }(wrappedTerm).(core.Term))))
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected wrapped type")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func Module (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("namespace", Namespace, fieldMap.([]any), cx)).(func(any) any)(func (field_namespace hmodule.Namespace) any {
              return libeithers.Bind(extracthelpers.RequireField("elements", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(decodecore.Binding, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_elements []any) any {
                return libeithers.Bind(extracthelpers.RequireField("termDependencies", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeList(Namespace, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_termDependencies []any) any {
                  return libeithers.Bind(extracthelpers.RequireField("typeDependencies", func (_p graph.Graph) func(any) any {
                    return func (v1 graph.Graph) func(any) any {
                      return func (v2 any) any {
                        return extracthelpers.DecodeList(Namespace, v1, v2.(core.Term))
                      }
                    }(_p).(func(any) any)
                  }, fieldMap.([]any), cx)).(func(any) any)(func (field_typeDependencies []any) any {
                    return libeithers.Bind(extracthelpers.RequireField("description", func (_p graph.Graph) func(any) any {
                      return func (v1 graph.Graph) func(any) any {
                        return func (v2 any) any {
                          return extracthelpers.DecodeMaybe(func (_p graph.Graph) func(core.Term) any {
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
                    }, fieldMap.([]any), cx)).(func(any) any)(func (field_description any) any {
                      return [2]any{"right", hmodule.Module{Namespace: field_namespace, Elements: field_elements, TermDependencies: field_termDependencies, TypeDependencies: field_typeDependencies, Description: field_description}}
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

func Namespace (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b string) any {
            return hmodule.Namespace(b)
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
          }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, func (v any) any {
            return v.(core.WrappedTerm).Body
          }(wrappedTerm).(core.Term))))
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected wrapped type")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func Namespaces (n func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("focus", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodePair(Namespace, n, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_focus any) any {
              return libeithers.Bind(extracthelpers.RequireField("mapping", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeMap(Namespace, n, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_mapping []any) any {
                return [2]any{"right", hmodule.Namespaces[any]{Focus: field_focus, Mapping: field_mapping}}
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

func QualifiedName (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("namespace", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeMaybe(Namespace, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_namespace any) any {
              return libeithers.Bind(extracthelpers.RequireField("local", func (_p graph.Graph) func(any) any {
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
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_local string) any {
                return [2]any{"right", hmodule.QualifiedName{Namespace: field_namespace, Local: field_local}}
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

func TermDefinition (cx graph.Graph, raw core.Term) any {
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
              return libeithers.Bind(extracthelpers.RequireField("term", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_term core.Term) any {
                return libeithers.Bind(extracthelpers.RequireField("type", decodecore.TypeScheme, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.TypeScheme) any {
                  return [2]any{"right", hmodule.TermDefinition{Name: field_name, Term: field_term, Type_: field_type}}
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

func TypeDefinition (cx graph.Graph, raw core.Term) any {
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
              return libeithers.Bind(extracthelpers.RequireField("type", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.Type) any {
                return [2]any{"right", hmodule.TypeDefinition{Name: field_name, Type_: field_type}}
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
