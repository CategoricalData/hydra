// Note: this is an automatically generated file. Do not edit.

package decodecore

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
  "math/big"
)

func AnnotatedTerm (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("body", Term, fieldMap.([]any), cx)).(func(any) any)(func (field_body core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("annotation", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeMap(Name, Term, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_annotation []any) any {
                return [2]any{"right", core.AnnotatedTerm{Body: field_body, Annotation: field_annotation}}
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

func AnnotatedType (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("body", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_body core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("annotation", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeMap(Name, Term, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_annotation []any) any {
                return [2]any{"right", core.AnnotatedType{Body: field_body, Annotation: field_annotation}}
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

func Application (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("function", Term, fieldMap.([]any), cx)).(func(any) any)(func (field_function core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("argument", Term, fieldMap.([]any), cx)).(func(any) any)(func (field_argument core.Term) any {
                return [2]any{"right", core.Application{Function: field_function, Argument: field_argument}}
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

func ApplicationType (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("function", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_function core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("argument", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_argument core.Type) any {
                return [2]any{"right", core.ApplicationType{Function: field_function, Argument: field_argument}}
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

func Binding (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("name", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_name core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("term", Term, fieldMap.([]any), cx)).(func(any) any)(func (field_term core.Term) any {
                return libeithers.Bind(extracthelpers.RequireField("type", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeMaybe(TypeScheme, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_type any) any {
                  return [2]any{"right", core.Binding{Name: field_name, Term: field_term, Type_: field_type}}
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

func CaseStatement (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("typeName", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_typeName core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("default", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeMaybe(Term, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_default any) any {
                return libeithers.Bind(extracthelpers.RequireField("cases", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeList(Field, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_cases []any) any {
                  return [2]any{"right", core.CaseStatement{TypeName: field_typeName, Default_: field_default, Cases: field_cases}}
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

func EitherType (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("left", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_left core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("right", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_right core.Type) any {
                return [2]any{"right", core.EitherType{Left: field_left, Right: field_right}}
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

func PairType (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("first", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_first core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("second", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_second core.Type) any {
                return [2]any{"right", core.PairType{First: field_first, Second: field_second}}
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

func Elimination (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("record"), func (input core.Term) any {
              return libeithers.Map(func (t core.Projection) any {
                return core.EliminationRecord{Value: t}
              }).(func(any) any)(Projection(cx, input))
            }}, [2]any{core.Name("union"), func (input core.Term) any {
              return libeithers.Map(func (t core.CaseStatement) any {
                return core.EliminationUnion{Value: t}
              }).(func(any) any)(CaseStatement(cx, input))
            }}, [2]any{core.Name("wrap"), func (input core.Term) any {
              return libeithers.Map(func (t core.Name) any {
                return core.EliminationWrap{Value: t}
              }).(func(any) any)(Name(cx, input))
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

func Field (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("name", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_name core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("term", Term, fieldMap.([]any), cx)).(func(any) any)(func (field_term core.Term) any {
                return [2]any{"right", core.Field{Name: field_name, Term: field_term}}
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

func FieldType (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("name", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_name core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("type", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.Type) any {
                return [2]any{"right", core.FieldType{Name: field_name, Type_: field_type}}
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

func FloatType (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("bigfloat"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.FloatTypeBigfloat{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("float32"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.FloatTypeFloat32_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("float64"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.FloatTypeFloat64_{Value: t}
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

func FloatValue (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("bigfloat"), func (input core.Term) any {
              return libeithers.Map(func (t float64) any {
                return core.FloatValueBigfloat{Value: t}
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
            }}, [2]any{core.Name("float32"), func (input core.Term) any {
              return libeithers.Map(func (t float32) any {
                return core.FloatValueFloat32_{Value: t}
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
                                case core.FloatValueFloat32_:
                                return func (f float32) any {
                                  return [2]any{"right", f}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected float32 value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected float32 literal")}
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
            }}, [2]any{core.Name("float64"), func (input core.Term) any {
              return libeithers.Map(func (t float64) any {
                return core.FloatValueFloat64_{Value: t}
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
                                case core.FloatValueFloat64_:
                                return func (f float64) any {
                                  return [2]any{"right", f}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected float64 value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected float64 literal")}
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

func ForallType (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("parameter", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_parameter core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("body", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_body core.Type) any {
                return [2]any{"right", core.ForallType{Parameter: field_parameter, Body: field_body}}
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

func Function (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("elimination"), func (input core.Term) any {
              return libeithers.Map(func (t core.Elimination) any {
                return core.FunctionElimination{Value: t}
              }).(func(any) any)(Elimination(cx, input))
            }}, [2]any{core.Name("lambda"), func (input core.Term) any {
              return libeithers.Map(func (t core.Lambda) any {
                return core.FunctionLambda{Value: t}
              }).(func(any) any)(Lambda(cx, input))
            }}, [2]any{core.Name("primitive"), func (input core.Term) any {
              return libeithers.Map(func (t core.Name) any {
                return core.FunctionPrimitive{Value: t}
              }).(func(any) any)(Name(cx, input))
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

func FunctionType (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("domain", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_domain core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("codomain", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_codomain core.Type) any {
                return [2]any{"right", core.FunctionType{Domain: field_domain, Codomain: field_codomain}}
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

func Injection (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("typeName", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_typeName core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("field", Field, fieldMap.([]any), cx)).(func(any) any)(func (field_field core.Field) any {
                return [2]any{"right", core.Injection{TypeName: field_typeName, Field: field_field}}
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

func IntegerType (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("bigint"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.IntegerTypeBigint{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("int8"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.IntegerTypeInt8_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("int16"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.IntegerTypeInt16_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("int32"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.IntegerTypeInt32_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("int64"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.IntegerTypeInt64_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("uint8"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.IntegerTypeUint8_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("uint16"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.IntegerTypeUint16_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("uint32"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.IntegerTypeUint32_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("uint64"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.IntegerTypeUint64_{Value: t}
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

func IntegerValue (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("bigint"), func (input core.Term) any {
              return libeithers.Map(func (t *big.Int) any {
                return core.IntegerValueBigint{Value: t}
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
                                case core.IntegerValueBigint:
                                return func (i *big.Int) any {
                                  return [2]any{"right", i}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected bigint value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected bigint literal")}
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
            }}, [2]any{core.Name("int8"), func (input core.Term) any {
              return libeithers.Map(func (t int8) any {
                return core.IntegerValueInt8_{Value: t}
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
                                case core.IntegerValueInt8_:
                                return func (i int8) any {
                                  return [2]any{"right", i}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected int8 value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected int8 literal")}
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
            }}, [2]any{core.Name("int16"), func (input core.Term) any {
              return libeithers.Map(func (t int16) any {
                return core.IntegerValueInt16_{Value: t}
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
                                case core.IntegerValueInt16_:
                                return func (i int16) any {
                                  return [2]any{"right", i}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected int16 value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected int16 literal")}
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
            }}, [2]any{core.Name("int32"), func (input core.Term) any {
              return libeithers.Map(func (t int32) any {
                return core.IntegerValueInt32_{Value: t}
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
            }}, [2]any{core.Name("int64"), func (input core.Term) any {
              return libeithers.Map(func (t int64) any {
                return core.IntegerValueInt64_{Value: t}
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
                                case core.IntegerValueInt64_:
                                return func (i int64) any {
                                  return [2]any{"right", i}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected int64 value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected int64 literal")}
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
            }}, [2]any{core.Name("uint8"), func (input core.Term) any {
              return libeithers.Map(func (t uint8) any {
                return core.IntegerValueUint8_{Value: t}
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
                                case core.IntegerValueUint8_:
                                return func (i uint8) any {
                                  return [2]any{"right", i}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected uint8 value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected uint8 literal")}
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
            }}, [2]any{core.Name("uint16"), func (input core.Term) any {
              return libeithers.Map(func (t uint16) any {
                return core.IntegerValueUint16_{Value: t}
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
                                case core.IntegerValueUint16_:
                                return func (i uint16) any {
                                  return [2]any{"right", i}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected uint16 value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected uint16 literal")}
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
            }}, [2]any{core.Name("uint32"), func (input core.Term) any {
              return libeithers.Map(func (t uint32) any {
                return core.IntegerValueUint32_{Value: t}
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
                                case core.IntegerValueUint32_:
                                return func (i uint32) any {
                                  return [2]any{"right", i}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected uint32 value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected uint32 literal")}
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
            }}, [2]any{core.Name("uint64"), func (input core.Term) any {
              return libeithers.Map(func (t uint64) any {
                return core.IntegerValueUint64_{Value: t}
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
                                case core.IntegerValueUint64_:
                                return func (i uint64) any {
                                  return [2]any{"right", i}
                                }(v.Value)
                                default:
                                return [2]any{"left", error.DecodingError("expected uint64 value")}
                              }
                              return nil
                            }(v1)
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected uint64 literal")}
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

func Lambda (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("parameter", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_parameter core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("domain", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeMaybe(Type_, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_domain any) any {
                return libeithers.Bind(extracthelpers.RequireField("body", Term, fieldMap.([]any), cx)).(func(any) any)(func (field_body core.Term) any {
                  return [2]any{"right", core.Lambda{Parameter: field_parameter, Domain: field_domain, Body: field_body}}
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

func Let (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("bindings", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeList(Binding, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_bindings []any) any {
              return libeithers.Bind(extracthelpers.RequireField("body", Term, fieldMap.([]any), cx)).(func(any) any)(func (field_body core.Term) any {
                return [2]any{"right", core.Let{Bindings: field_bindings, Body: field_body}}
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

func Literal (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("binary"), func (input core.Term) any {
              return libeithers.Map(func (t []byte) any {
                return core.LiteralBinary{Value: t}
              }).(func(any) any)(libeithers.Either(func (err string) any {
                return [2]any{"left", error.DecodingError(err)}
              }).(func(any) any)(func (stripped2 core.Term) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermLiteral:
                    return func (v core.Literal) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.LiteralBinary:
                          return func (b []byte) any {
                            return [2]any{"right", b}
                          }(v.Value)
                          default:
                          return [2]any{"left", error.DecodingError("expected binary literal")}
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
            }}, [2]any{core.Name("boolean"), func (input core.Term) any {
              return libeithers.Map(func (t bool) any {
                return core.LiteralBoolean{Value: t}
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
            }}, [2]any{core.Name("float"), func (input core.Term) any {
              return libeithers.Map(func (t core.FloatValue) any {
                return core.LiteralFloat{Value: t}
              }).(func(any) any)(FloatValue(cx, input))
            }}, [2]any{core.Name("integer"), func (input core.Term) any {
              return libeithers.Map(func (t core.IntegerValue) any {
                return core.LiteralInteger{Value: t}
              }).(func(any) any)(IntegerValue(cx, input))
            }}, [2]any{core.Name("string"), func (input core.Term) any {
              return libeithers.Map(func (t string) any {
                return core.LiteralString_{Value: t}
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

func LiteralType (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("binary"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.LiteralTypeBinary{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("boolean"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.LiteralTypeBoolean{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("float"), func (input core.Term) any {
              return libeithers.Map(func (t core.FloatType) any {
                return core.LiteralTypeFloat{Value: t}
              }).(func(any) any)(FloatType(cx, input))
            }}, [2]any{core.Name("integer"), func (input core.Term) any {
              return libeithers.Map(func (t core.IntegerType) any {
                return core.LiteralTypeInteger{Value: t}
              }).(func(any) any)(IntegerType(cx, input))
            }}, [2]any{core.Name("string"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.LiteralTypeString_{Value: t}
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

func MapType (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("keys", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_keys core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("values", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_values core.Type) any {
                return [2]any{"right", core.MapType{Keys: field_keys, Values: field_values}}
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

func Name (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b string) any {
            return core.Name(b)
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

func Projection (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("typeName", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_typeName core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("field", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_field core.Name) any {
                return [2]any{"right", core.Projection{TypeName: field_typeName, Field: field_field}}
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

func Record (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("typeName", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_typeName core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("fields", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(Field, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_fields []any) any {
                return [2]any{"right", core.Record{TypeName: field_typeName, Fields: field_fields}}
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

func Term (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("annotated"), func (input core.Term) any {
              return libeithers.Map(func (t core.AnnotatedTerm) any {
                return core.TermAnnotated{Value: t}
              }).(func(any) any)(AnnotatedTerm(cx, input))
            }}, [2]any{core.Name("application"), func (input core.Term) any {
              return libeithers.Map(func (t core.Application) any {
                return core.TermApplication{Value: t}
              }).(func(any) any)(Application(cx, input))
            }}, [2]any{core.Name("either"), func (input core.Term) any {
              return libeithers.Map(func (t any) any {
                return core.TermEither{Value: t}
              }).(func(any) any)(extracthelpers.DecodeEither(Term, Term, cx, input))
            }}, [2]any{core.Name("function"), func (input core.Term) any {
              return libeithers.Map(func (t core.Function) any {
                return core.TermFunction{Value: t}
              }).(func(any) any)(Function(cx, input))
            }}, [2]any{core.Name("let"), func (input core.Term) any {
              return libeithers.Map(func (t core.Let) any {
                return core.TermLet{Value: t}
              }).(func(any) any)(Let(cx, input))
            }}, [2]any{core.Name("list"), func (input core.Term) any {
              return libeithers.Map(func (t []any) any {
                return core.TermList{Value: t}
              }).(func(any) any)(extracthelpers.DecodeList(Term, cx, input))
            }}, [2]any{core.Name("literal"), func (input core.Term) any {
              return libeithers.Map(func (t core.Literal) any {
                return core.TermLiteral{Value: t}
              }).(func(any) any)(Literal(cx, input))
            }}, [2]any{core.Name("map"), func (input core.Term) any {
              return libeithers.Map(func (t []any) any {
                return core.TermMap_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeMap(Term, Term, cx, input))
            }}, [2]any{core.Name("maybe"), func (input core.Term) any {
              return libeithers.Map(func (t any) any {
                return core.TermMaybe{Value: t}
              }).(func(any) any)(extracthelpers.DecodeMaybe(Term, cx, input))
            }}, [2]any{core.Name("pair"), func (input core.Term) any {
              return libeithers.Map(func (t any) any {
                return core.TermPair{Value: t}
              }).(func(any) any)(extracthelpers.DecodePair(Term, Term, cx, input))
            }}, [2]any{core.Name("record"), func (input core.Term) any {
              return libeithers.Map(func (t core.Record) any {
                return core.TermRecord{Value: t}
              }).(func(any) any)(Record(cx, input))
            }}, [2]any{core.Name("set"), func (input core.Term) any {
              return libeithers.Map(func (t []any) any {
                return core.TermSet{Value: t}
              }).(func(any) any)(extracthelpers.DecodeSet(Term, cx, input))
            }}, [2]any{core.Name("typeApplication"), func (input core.Term) any {
              return libeithers.Map(func (t core.TypeApplicationTerm) any {
                return core.TermTypeApplication{Value: t}
              }).(func(any) any)(TypeApplicationTerm(cx, input))
            }}, [2]any{core.Name("typeLambda"), func (input core.Term) any {
              return libeithers.Map(func (t core.TypeLambda) any {
                return core.TermTypeLambda{Value: t}
              }).(func(any) any)(TypeLambda(cx, input))
            }}, [2]any{core.Name("union"), func (input core.Term) any {
              return libeithers.Map(func (t core.Injection) any {
                return core.TermUnion{Value: t}
              }).(func(any) any)(Injection(cx, input))
            }}, [2]any{core.Name("unit"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.TermUnit{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("variable"), func (input core.Term) any {
              return libeithers.Map(func (t core.Name) any {
                return core.TermVariable{Value: t}
              }).(func(any) any)(Name(cx, input))
            }}, [2]any{core.Name("wrap"), func (input core.Term) any {
              return libeithers.Map(func (t core.WrappedTerm) any {
                return core.TermWrap{Value: t}
              }).(func(any) any)(WrappedTerm(cx, input))
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

func Type_ (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("annotated"), func (input core.Term) any {
              return libeithers.Map(func (t core.AnnotatedType) any {
                return core.TypeAnnotated{Value: t}
              }).(func(any) any)(AnnotatedType(cx, input))
            }}, [2]any{core.Name("application"), func (input core.Term) any {
              return libeithers.Map(func (t core.ApplicationType) any {
                return core.TypeApplication{Value: t}
              }).(func(any) any)(ApplicationType(cx, input))
            }}, [2]any{core.Name("either"), func (input core.Term) any {
              return libeithers.Map(func (t core.EitherType) any {
                return core.TypeEither{Value: t}
              }).(func(any) any)(EitherType(cx, input))
            }}, [2]any{core.Name("forall"), func (input core.Term) any {
              return libeithers.Map(func (t core.ForallType) any {
                return core.TypeForall{Value: t}
              }).(func(any) any)(ForallType(cx, input))
            }}, [2]any{core.Name("function"), func (input core.Term) any {
              return libeithers.Map(func (t core.FunctionType) any {
                return core.TypeFunction{Value: t}
              }).(func(any) any)(FunctionType(cx, input))
            }}, [2]any{core.Name("list"), func (input core.Term) any {
              return libeithers.Map(func (t core.Type) any {
                return core.TypeList{Value: t}
              }).(func(any) any)(Type_(cx, input))
            }}, [2]any{core.Name("literal"), func (input core.Term) any {
              return libeithers.Map(func (t core.LiteralType) any {
                return core.TypeLiteral{Value: t}
              }).(func(any) any)(LiteralType(cx, input))
            }}, [2]any{core.Name("map"), func (input core.Term) any {
              return libeithers.Map(func (t core.MapType) any {
                return core.TypeMap_{Value: t}
              }).(func(any) any)(MapType(cx, input))
            }}, [2]any{core.Name("maybe"), func (input core.Term) any {
              return libeithers.Map(func (t core.Type) any {
                return core.TypeMaybe{Value: t}
              }).(func(any) any)(Type_(cx, input))
            }}, [2]any{core.Name("pair"), func (input core.Term) any {
              return libeithers.Map(func (t core.PairType) any {
                return core.TypePair{Value: t}
              }).(func(any) any)(PairType(cx, input))
            }}, [2]any{core.Name("record"), func (input core.Term) any {
              return libeithers.Map(func (t []any) any {
                return core.TypeRecord{Value: t}
              }).(func(any) any)(extracthelpers.DecodeList(FieldType, cx, input))
            }}, [2]any{core.Name("set"), func (input core.Term) any {
              return libeithers.Map(func (t core.Type) any {
                return core.TypeSet{Value: t}
              }).(func(any) any)(Type_(cx, input))
            }}, [2]any{core.Name("union"), func (input core.Term) any {
              return libeithers.Map(func (t []any) any {
                return core.TypeUnion{Value: t}
              }).(func(any) any)(extracthelpers.DecodeList(FieldType, cx, input))
            }}, [2]any{core.Name("unit"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return core.TypeUnit{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("variable"), func (input core.Term) any {
              return libeithers.Map(func (t core.Name) any {
                return core.TypeVariable{Value: t}
              }).(func(any) any)(Name(cx, input))
            }}, [2]any{core.Name("wrap"), func (input core.Term) any {
              return libeithers.Map(func (t core.Type) any {
                return core.TypeWrap{Value: t}
              }).(func(any) any)(Type_(cx, input))
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

func TypeApplicationTerm (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("body", Term, fieldMap.([]any), cx)).(func(any) any)(func (field_body core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("type", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.Type) any {
                return [2]any{"right", core.TypeApplicationTerm{Body: field_body, Type_: field_type}}
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

func TypeLambda (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("parameter", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_parameter core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("body", Term, fieldMap.([]any), cx)).(func(any) any)(func (field_body core.Term) any {
                return [2]any{"right", core.TypeLambda{Parameter: field_parameter, Body: field_body}}
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

func TypeScheme (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("variables", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeList(Name, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_variables []any) any {
              return libeithers.Bind(extracthelpers.RequireField("type", Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.Type) any {
                return libeithers.Bind(extracthelpers.RequireField("constraints", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeMaybe(func (_p graph.Graph) func(core.Term) any {
                        return func (v12 graph.Graph) func(core.Term) any {
                          return func (v22 core.Term) any {
                            return extracthelpers.DecodeMap(Name, TypeVariableMetadata, v12, v22)
                          }
                        }(_p).(func(core.Term) any)
                      }, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_constraints any) any {
                  return [2]any{"right", core.TypeScheme{Variables: field_variables, Type_: field_type, Constraints: field_constraints}}
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

func TypeVariableMetadata (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("classes", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeSet(Name, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_classes []any) any {
              return [2]any{"right", core.TypeVariableMetadata{Classes: field_classes}}
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

func WrappedTerm (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("typeName", Name, fieldMap.([]any), cx)).(func(any) any)(func (field_typeName core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("body", Term, fieldMap.([]any), cx)).(func(any) any)(func (field_body core.Term) any {
                return [2]any{"right", core.WrappedTerm{TypeName: field_typeName, Body: field_body}}
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
