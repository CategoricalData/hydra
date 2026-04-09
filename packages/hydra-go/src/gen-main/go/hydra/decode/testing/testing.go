// Note: this is an automatically generated file. Do not edit.

package decodetesting

import (
  "hydra.dev/hydra/ast"
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/core"
  decodeast "hydra.dev/hydra/decode/ast"
  decodecoders "hydra.dev/hydra/decode/coders"
  decodecore "hydra.dev/hydra/decode/core"
  decodejsonmodel "hydra.dev/hydra/decode/json/model"
  decodeparsing "hydra.dev/hydra/decode/parsing"
  decodetyping "hydra.dev/hydra/decode/typing"
  decodeutil "hydra.dev/hydra/decode/util"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  jsonmodel "hydra.dev/hydra/json/model"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/parsing"
  "hydra.dev/hydra/testing"
  "hydra.dev/hydra/util"
)

func AlphaConversionTestCase (cx graph.Graph, raw core.Term) any {
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
              return libeithers.Bind(extracthelpers.RequireField("oldVariable", decodecore.Name, fieldMap.([]any), cx)).(func(any) any)(func (field_oldVariable core.Name) any {
                return libeithers.Bind(extracthelpers.RequireField("newVariable", decodecore.Name, fieldMap.([]any), cx)).(func(any) any)(func (field_newVariable core.Name) any {
                  return libeithers.Bind(extracthelpers.RequireField("result", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_result core.Term) any {
                    return [2]any{"right", testing.AlphaConversionTestCase{Term: field_term, OldVariable: field_oldVariable, NewVariable: field_newVariable, Result: field_result}}
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

func EvaluationStyle (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("eager"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.EvaluationStyleEager{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("lazy"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.EvaluationStyleLazy{Value: t}
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

func CaseConversionTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("fromConvention", decodeutil.CaseConvention, fieldMap.([]any), cx)).(func(any) any)(func (field_fromConvention util.CaseConvention) any {
              return libeithers.Bind(extracthelpers.RequireField("toConvention", decodeutil.CaseConvention, fieldMap.([]any), cx)).(func(any) any)(func (field_toConvention util.CaseConvention) any {
                return libeithers.Bind(extracthelpers.RequireField("fromString", func (_p graph.Graph) func(any) any {
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
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_fromString string) any {
                  return libeithers.Bind(extracthelpers.RequireField("toString", func (_p graph.Graph) func(any) any {
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
                  }, fieldMap.([]any), cx)).(func(any) any)(func (field_toString string) any {
                    return [2]any{"right", testing.CaseConversionTestCase{FromConvention: field_fromConvention, ToConvention: field_toConvention, FromString: field_fromString, ToString: field_toString}}
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

func DelegatedEvaluationTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                return [2]any{"right", testing.DelegatedEvaluationTestCase{Input: field_input, Output: field_output}}
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

func EtaExpansionTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                return [2]any{"right", testing.EtaExpansionTestCase{Input: field_input, Output: field_output}}
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

func DeannotateTermTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                return [2]any{"right", testing.DeannotateTermTestCase{Input: field_input, Output: field_output}}
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

func DeannotateTypeTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Type) any {
                return [2]any{"right", testing.DeannotateTypeTestCase{Input: field_input, Output: field_output}}
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

func FlattenLetTermsTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                return [2]any{"right", testing.FlattenLetTermsTestCase{Input: field_input, Output: field_output}}
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

func FoldOperation (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("sumInt32Literals"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.FoldOperationSumInt32Literals{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("collectListLengths"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.FoldOperationCollectListLengths{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("collectLabels"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.FoldOperationCollectLabels{Value: t}
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

func FoldOverTermTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("traversalOrder", decodecoders.TraversalOrder, fieldMap.([]any), cx)).(func(any) any)(func (field_traversalOrder coders.TraversalOrder) any {
                return libeithers.Bind(extracthelpers.RequireField("operation", FoldOperation, fieldMap.([]any), cx)).(func(any) any)(func (field_operation testing.FoldOperation) any {
                  return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                    return [2]any{"right", testing.FoldOverTermTestCase{Input: field_input, TraversalOrder: field_traversalOrder, Operation: field_operation, Output: field_output}}
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

func FreeVariablesTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeSet(decodecore.Name, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_output []any) any {
                return [2]any{"right", testing.FreeVariablesTestCase{Input: field_input, Output: field_output}}
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

func HoistPredicate (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("caseStatements"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.HoistPredicateCaseStatements{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("applications"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.HoistPredicateApplications{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("lists"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.HoistPredicateLists{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("nothing"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.HoistPredicateNothing{Value: t}
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

func HoistLetBindingsTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Let, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Let) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Let, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Let) any {
                return [2]any{"right", testing.HoistLetBindingsTestCase{Input: field_input, Output: field_output}}
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

func HoistPolymorphicLetBindingsTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Let, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Let) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Let, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Let) any {
                return [2]any{"right", testing.HoistPolymorphicLetBindingsTestCase{Input: field_input, Output: field_output}}
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

func HoistSubtermsTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("predicate", HoistPredicate, fieldMap.([]any), cx)).(func(any) any)(func (field_predicate testing.HoistPredicate) any {
              return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
                return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                  return [2]any{"right", testing.HoistSubtermsTestCase{Predicate: field_predicate, Input: field_input, Output: field_output}}
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

func HoistCaseStatementsTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                return [2]any{"right", testing.HoistCaseStatementsTestCase{Input: field_input, Output: field_output}}
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

func TermRewriter (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("replaceFooWithBar"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.TermRewriterReplaceFooWithBar{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("replaceInt32WithInt64"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.TermRewriterReplaceInt32WithInt64{Value: t}
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

func RewriteTermTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("rewriter", TermRewriter, fieldMap.([]any), cx)).(func(any) any)(func (field_rewriter testing.TermRewriter) any {
                return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                  return [2]any{"right", testing.RewriteTermTestCase{Input: field_input, Rewriter: field_rewriter, Output: field_output}}
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

func TypeRewriter (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("replaceStringWithInt32"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return testing.TypeRewriterReplaceStringWithInt32{Value: t}
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

func RewriteTypeTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("rewriter", TypeRewriter, fieldMap.([]any), cx)).(func(any) any)(func (field_rewriter testing.TypeRewriter) any {
                return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Type) any {
                  return [2]any{"right", testing.RewriteTypeTestCase{Input: field_input, Rewriter: field_rewriter, Output: field_output}}
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

func EvaluationTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("evaluationStyle", EvaluationStyle, fieldMap.([]any), cx)).(func(any) any)(func (field_evaluationStyle testing.EvaluationStyle) any {
              return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
                return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                  return [2]any{"right", testing.EvaluationTestCase{EvaluationStyle: field_evaluationStyle, Input: field_input, Output: field_output}}
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

func InferenceFailureTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return [2]any{"right", testing.InferenceFailureTestCase{Input: field_input}}
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

func InferenceTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.TypeScheme, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.TypeScheme) any {
                return [2]any{"right", testing.InferenceTestCase{Input: field_input, Output: field_output}}
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

func JsonDecodeTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("type", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("json", decodejsonmodel.Value, fieldMap.([]any), cx)).(func(any) any)(func (field_json jsonmodel.Value) any {
                return libeithers.Bind(extracthelpers.RequireField("expected", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeEither(func (_p graph.Graph) func(core.Term) any {
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
                      }, decodecore.Term, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_expected any) any {
                  return [2]any{"right", testing.JsonDecodeTestCase{Type_: field_type, Json: field_json, Expected: field_expected}}
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

func JsonEncodeTestCase (cx graph.Graph, raw core.Term) any {
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
              return libeithers.Bind(extracthelpers.RequireField("expected", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeEither(func (_p graph.Graph) func(core.Term) any {
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
                    }, decodejsonmodel.Value, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_expected any) any {
                return [2]any{"right", testing.JsonEncodeTestCase{Term: field_term, Expected: field_expected}}
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

func JsonParserTestCase (v1 graph.Graph, v2 core.Term) any {
  return ParserTestCase(decodejsonmodel.Value, v1, v2)
}

func JsonRoundtripTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("type", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("term", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_term core.Term) any {
                return [2]any{"right", testing.JsonRoundtripTestCase{Type_: field_type, Term: field_term}}
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

func LiftLambdaAboveLetTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                return [2]any{"right", testing.LiftLambdaAboveLetTestCase{Input: field_input, Output: field_output}}
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

func JsonWriterTestCase (v1 graph.Graph, v2 core.Term) any {
  return WriterTestCase(decodejsonmodel.Value, v1, v2)
}

func ParserTestCase (a func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", func (_p graph.Graph) func(any) any {
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
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_input string) any {
              return libeithers.Bind(extracthelpers.RequireField("output", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return decodeparsing.ParseResult(a, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_output parsing.ParseResult[any]) any {
                return [2]any{"right", testing.ParserTestCase[any]{Input: field_input, Output: field_output}}
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

func Tag (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b string) any {
            return testing.Tag(b)
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

func TestCase (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("alphaConversion"), func (input core.Term) any {
              return libeithers.Map(func (t testing.AlphaConversionTestCase) any {
                return testing.TestCaseAlphaConversion{Value: t}
              }).(func(any) any)(AlphaConversionTestCase(cx, input))
            }}, [2]any{core.Name("caseConversion"), func (input core.Term) any {
              return libeithers.Map(func (t testing.CaseConversionTestCase) any {
                return testing.TestCaseCaseConversion{Value: t}
              }).(func(any) any)(CaseConversionTestCase(cx, input))
            }}, [2]any{core.Name("deannotateTerm"), func (input core.Term) any {
              return libeithers.Map(func (t testing.DeannotateTermTestCase) any {
                return testing.TestCaseDeannotateTerm{Value: t}
              }).(func(any) any)(DeannotateTermTestCase(cx, input))
            }}, [2]any{core.Name("deannotateType"), func (input core.Term) any {
              return libeithers.Map(func (t testing.DeannotateTypeTestCase) any {
                return testing.TestCaseDeannotateType{Value: t}
              }).(func(any) any)(DeannotateTypeTestCase(cx, input))
            }}, [2]any{core.Name("delegatedEvaluation"), func (input core.Term) any {
              return libeithers.Map(func (t testing.DelegatedEvaluationTestCase) any {
                return testing.TestCaseDelegatedEvaluation{Value: t}
              }).(func(any) any)(DelegatedEvaluationTestCase(cx, input))
            }}, [2]any{core.Name("etaExpansion"), func (input core.Term) any {
              return libeithers.Map(func (t testing.EtaExpansionTestCase) any {
                return testing.TestCaseEtaExpansion{Value: t}
              }).(func(any) any)(EtaExpansionTestCase(cx, input))
            }}, [2]any{core.Name("flattenLetTerms"), func (input core.Term) any {
              return libeithers.Map(func (t testing.FlattenLetTermsTestCase) any {
                return testing.TestCaseFlattenLetTerms{Value: t}
              }).(func(any) any)(FlattenLetTermsTestCase(cx, input))
            }}, [2]any{core.Name("freeVariables"), func (input core.Term) any {
              return libeithers.Map(func (t testing.FreeVariablesTestCase) any {
                return testing.TestCaseFreeVariables{Value: t}
              }).(func(any) any)(FreeVariablesTestCase(cx, input))
            }}, [2]any{core.Name("evaluation"), func (input core.Term) any {
              return libeithers.Map(func (t testing.EvaluationTestCase) any {
                return testing.TestCaseEvaluation{Value: t}
              }).(func(any) any)(EvaluationTestCase(cx, input))
            }}, [2]any{core.Name("inference"), func (input core.Term) any {
              return libeithers.Map(func (t testing.InferenceTestCase) any {
                return testing.TestCaseInference{Value: t}
              }).(func(any) any)(InferenceTestCase(cx, input))
            }}, [2]any{core.Name("inferenceFailure"), func (input core.Term) any {
              return libeithers.Map(func (t testing.InferenceFailureTestCase) any {
                return testing.TestCaseInferenceFailure{Value: t}
              }).(func(any) any)(InferenceFailureTestCase(cx, input))
            }}, [2]any{core.Name("jsonDecode"), func (input core.Term) any {
              return libeithers.Map(func (t testing.JsonDecodeTestCase) any {
                return testing.TestCaseJsonDecode{Value: t}
              }).(func(any) any)(JsonDecodeTestCase(cx, input))
            }}, [2]any{core.Name("jsonEncode"), func (input core.Term) any {
              return libeithers.Map(func (t testing.JsonEncodeTestCase) any {
                return testing.TestCaseJsonEncode{Value: t}
              }).(func(any) any)(JsonEncodeTestCase(cx, input))
            }}, [2]any{core.Name("jsonParser"), func (input core.Term) any {
              return libeithers.Map(func (t testing.ParserTestCase[jsonmodel.Value]) any {
                return testing.TestCaseJsonParser{Value: t}
              }).(func(any) any)(JsonParserTestCase(cx, input))
            }}, [2]any{core.Name("jsonRoundtrip"), func (input core.Term) any {
              return libeithers.Map(func (t testing.JsonRoundtripTestCase) any {
                return testing.TestCaseJsonRoundtrip{Value: t}
              }).(func(any) any)(JsonRoundtripTestCase(cx, input))
            }}, [2]any{core.Name("jsonWriter"), func (input core.Term) any {
              return libeithers.Map(func (t testing.WriterTestCase[jsonmodel.Value]) any {
                return testing.TestCaseJsonWriter{Value: t}
              }).(func(any) any)(JsonWriterTestCase(cx, input))
            }}, [2]any{core.Name("liftLambdaAboveLet"), func (input core.Term) any {
              return libeithers.Map(func (t testing.LiftLambdaAboveLetTestCase) any {
                return testing.TestCaseLiftLambdaAboveLet{Value: t}
              }).(func(any) any)(LiftLambdaAboveLetTestCase(cx, input))
            }}, [2]any{core.Name("serialization"), func (input core.Term) any {
              return libeithers.Map(func (t testing.SerializationTestCase) any {
                return testing.TestCaseSerialization{Value: t}
              }).(func(any) any)(SerializationTestCase(cx, input))
            }}, [2]any{core.Name("simplifyTerm"), func (input core.Term) any {
              return libeithers.Map(func (t testing.SimplifyTermTestCase) any {
                return testing.TestCaseSimplifyTerm{Value: t}
              }).(func(any) any)(SimplifyTermTestCase(cx, input))
            }}, [2]any{core.Name("topologicalSort"), func (input core.Term) any {
              return libeithers.Map(func (t testing.TopologicalSortTestCase) any {
                return testing.TestCaseTopologicalSort{Value: t}
              }).(func(any) any)(TopologicalSortTestCase(cx, input))
            }}, [2]any{core.Name("topologicalSortBindings"), func (input core.Term) any {
              return libeithers.Map(func (t testing.TopologicalSortBindingsTestCase) any {
                return testing.TestCaseTopologicalSortBindings{Value: t}
              }).(func(any) any)(TopologicalSortBindingsTestCase(cx, input))
            }}, [2]any{core.Name("topologicalSortSCC"), func (input core.Term) any {
              return libeithers.Map(func (t testing.TopologicalSortSCCTestCase) any {
                return testing.TestCaseTopologicalSortSCC{Value: t}
              }).(func(any) any)(TopologicalSortSCCTestCase(cx, input))
            }}, [2]any{core.Name("typeChecking"), func (input core.Term) any {
              return libeithers.Map(func (t testing.TypeCheckingTestCase) any {
                return testing.TestCaseTypeChecking{Value: t}
              }).(func(any) any)(TypeCheckingTestCase(cx, input))
            }}, [2]any{core.Name("typeCheckingFailure"), func (input core.Term) any {
              return libeithers.Map(func (t testing.TypeCheckingFailureTestCase) any {
                return testing.TestCaseTypeCheckingFailure{Value: t}
              }).(func(any) any)(TypeCheckingFailureTestCase(cx, input))
            }}, [2]any{core.Name("typeReduction"), func (input core.Term) any {
              return libeithers.Map(func (t testing.TypeReductionTestCase) any {
                return testing.TestCaseTypeReduction{Value: t}
              }).(func(any) any)(TypeReductionTestCase(cx, input))
            }}, [2]any{core.Name("normalizeTypeVariables"), func (input core.Term) any {
              return libeithers.Map(func (t testing.NormalizeTypeVariablesTestCase) any {
                return testing.TestCaseNormalizeTypeVariables{Value: t}
              }).(func(any) any)(NormalizeTypeVariablesTestCase(cx, input))
            }}, [2]any{core.Name("foldOverTerm"), func (input core.Term) any {
              return libeithers.Map(func (t testing.FoldOverTermTestCase) any {
                return testing.TestCaseFoldOverTerm{Value: t}
              }).(func(any) any)(FoldOverTermTestCase(cx, input))
            }}, [2]any{core.Name("rewriteTerm"), func (input core.Term) any {
              return libeithers.Map(func (t testing.RewriteTermTestCase) any {
                return testing.TestCaseRewriteTerm{Value: t}
              }).(func(any) any)(RewriteTermTestCase(cx, input))
            }}, [2]any{core.Name("rewriteType"), func (input core.Term) any {
              return libeithers.Map(func (t testing.RewriteTypeTestCase) any {
                return testing.TestCaseRewriteType{Value: t}
              }).(func(any) any)(RewriteTypeTestCase(cx, input))
            }}, [2]any{core.Name("hoistSubterms"), func (input core.Term) any {
              return libeithers.Map(func (t testing.HoistSubtermsTestCase) any {
                return testing.TestCaseHoistSubterms{Value: t}
              }).(func(any) any)(HoistSubtermsTestCase(cx, input))
            }}, [2]any{core.Name("hoistCaseStatements"), func (input core.Term) any {
              return libeithers.Map(func (t testing.HoistCaseStatementsTestCase) any {
                return testing.TestCaseHoistCaseStatements{Value: t}
              }).(func(any) any)(HoistCaseStatementsTestCase(cx, input))
            }}, [2]any{core.Name("hoistLetBindings"), func (input core.Term) any {
              return libeithers.Map(func (t testing.HoistLetBindingsTestCase) any {
                return testing.TestCaseHoistLetBindings{Value: t}
              }).(func(any) any)(HoistLetBindingsTestCase(cx, input))
            }}, [2]any{core.Name("hoistPolymorphicLetBindings"), func (input core.Term) any {
              return libeithers.Map(func (t testing.HoistPolymorphicLetBindingsTestCase) any {
                return testing.TestCaseHoistPolymorphicLetBindings{Value: t}
              }).(func(any) any)(HoistPolymorphicLetBindingsTestCase(cx, input))
            }}, [2]any{core.Name("substInType"), func (input core.Term) any {
              return libeithers.Map(func (t testing.SubstInTypeTestCase) any {
                return testing.TestCaseSubstInType{Value: t}
              }).(func(any) any)(SubstInTypeTestCase(cx, input))
            }}, [2]any{core.Name("variableOccursInType"), func (input core.Term) any {
              return libeithers.Map(func (t testing.VariableOccursInTypeTestCase) any {
                return testing.TestCaseVariableOccursInType{Value: t}
              }).(func(any) any)(VariableOccursInTypeTestCase(cx, input))
            }}, [2]any{core.Name("unifyTypes"), func (input core.Term) any {
              return libeithers.Map(func (t testing.UnifyTypesTestCase) any {
                return testing.TestCaseUnifyTypes{Value: t}
              }).(func(any) any)(UnifyTypesTestCase(cx, input))
            }}, [2]any{core.Name("joinTypes"), func (input core.Term) any {
              return libeithers.Map(func (t testing.JoinTypesTestCase) any {
                return testing.TestCaseJoinTypes{Value: t}
              }).(func(any) any)(JoinTypesTestCase(cx, input))
            }}, [2]any{core.Name("unshadowVariables"), func (input core.Term) any {
              return libeithers.Map(func (t testing.UnshadowVariablesTestCase) any {
                return testing.TestCaseUnshadowVariables{Value: t}
              }).(func(any) any)(UnshadowVariablesTestCase(cx, input))
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

func TestCaseWithMetadata (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("name", func (_p graph.Graph) func(any) any {
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
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_name string) any {
              return libeithers.Bind(extracthelpers.RequireField("case", TestCase, fieldMap.([]any), cx)).(func(any) any)(func (field_case testing.TestCase) any {
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
                  return libeithers.Bind(extracthelpers.RequireField("tags", func (_p graph.Graph) func(any) any {
                    return func (v1 graph.Graph) func(any) any {
                      return func (v2 any) any {
                        return extracthelpers.DecodeList(Tag, v1, v2.(core.Term))
                      }
                    }(_p).(func(any) any)
                  }, fieldMap.([]any), cx)).(func(any) any)(func (field_tags []any) any {
                    return [2]any{"right", testing.TestCaseWithMetadata{Name: field_name, Case_: field_case, Description: field_description, Tags: field_tags}}
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

func TestGroup (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("name", func (_p graph.Graph) func(any) any {
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
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_name string) any {
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
                return libeithers.Bind(extracthelpers.RequireField("subgroups", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeList(TestGroup, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_subgroups []any) any {
                  return libeithers.Bind(extracthelpers.RequireField("cases", func (_p graph.Graph) func(any) any {
                    return func (v1 graph.Graph) func(any) any {
                      return func (v2 any) any {
                        return extracthelpers.DecodeList(TestCaseWithMetadata, v1, v2.(core.Term))
                      }
                    }(_p).(func(any) any)
                  }, fieldMap.([]any), cx)).(func(any) any)(func (field_cases []any) any {
                    return [2]any{"right", testing.TestGroup{Name: field_name, Description: field_description, Subgroups: field_subgroups, Cases: field_cases}}
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

func TypeCheckingTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("outputTerm", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_outputTerm core.Term) any {
                return libeithers.Bind(extracthelpers.RequireField("outputType", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_outputType core.Type) any {
                  return [2]any{"right", testing.TypeCheckingTestCase{Input: field_input, OutputTerm: field_outputTerm, OutputType: field_outputType}}
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

func TypeCheckingFailureTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return [2]any{"right", testing.TypeCheckingFailureTestCase{Input: field_input}}
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

func TopologicalSortBindingsTestCase (cx graph.Graph, raw core.Term) any {
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
                  return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                    return func (v12 graph.Graph) func(core.Term) any {
                      return func (v22 core.Term) any {
                        return extracthelpers.DecodePair(decodecore.Name, decodecore.Term, v12, v22)
                      }
                    }(_p).(func(core.Term) any)
                  }, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_bindings []any) any {
              return libeithers.Bind(extracthelpers.RequireField("expected", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                      return func (v12 graph.Graph) func(core.Term) any {
                        return func (v22 core.Term) any {
                          return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                            return func (v13 graph.Graph) func(core.Term) any {
                              return func (v23 core.Term) any {
                                return extracthelpers.DecodePair(decodecore.Name, decodecore.Term, v13, v23)
                              }
                            }(_p).(func(core.Term) any)
                          }, v12, v22)
                        }
                      }(_p).(func(core.Term) any)
                    }, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_expected []any) any {
                return [2]any{"right", testing.TopologicalSortBindingsTestCase{Bindings: field_bindings, Expected: field_expected}}
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

func TopologicalSortTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("adjacencyList", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                    return func (v12 graph.Graph) func(core.Term) any {
                      return func (v22 core.Term) any {
                        return extracthelpers.DecodePair(func (_p graph.Graph) func(core.Term) any {
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
                                          return func (v13 core.IntegerValue) any {
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
                                            }(v13)
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
                        }, func (_p graph.Graph) func(core.Term) any {
                          return func (v13 graph.Graph) func(core.Term) any {
                            return func (v23 core.Term) any {
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
                                                case core.LiteralInteger:
                                                return func (v14 core.IntegerValue) any {
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
                                                  }(v14)
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
                              }, v13, v23)
                            }
                          }(_p).(func(core.Term) any)
                        }, v12, v22)
                      }
                    }(_p).(func(core.Term) any)
                  }, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_adjacencyList []any) any {
              return libeithers.Bind(extracthelpers.RequireField("expected", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeEither(func (_p graph.Graph) func(core.Term) any {
                      return func (v12 graph.Graph) func(core.Term) any {
                        return func (v22 core.Term) any {
                          return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                            return func (v13 graph.Graph) func(core.Term) any {
                              return func (v23 core.Term) any {
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
                                                  case core.LiteralInteger:
                                                  return func (v14 core.IntegerValue) any {
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
                                                    }(v14)
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
                                }, v13, v23)
                              }
                            }(_p).(func(core.Term) any)
                          }, v12, v22)
                        }
                      }(_p).(func(core.Term) any)
                    }, func (_p graph.Graph) func(core.Term) any {
                      return func (v12 graph.Graph) func(core.Term) any {
                        return func (v22 core.Term) any {
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
                                            case core.LiteralInteger:
                                            return func (v13 core.IntegerValue) any {
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
                                              }(v13)
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
                          }, v12, v22)
                        }
                      }(_p).(func(core.Term) any)
                    }, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_expected any) any {
                return [2]any{"right", testing.TopologicalSortTestCase{AdjacencyList: field_adjacencyList, Expected: field_expected}}
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

func TopologicalSortSCCTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("adjacencyList", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                    return func (v12 graph.Graph) func(core.Term) any {
                      return func (v22 core.Term) any {
                        return extracthelpers.DecodePair(func (_p graph.Graph) func(core.Term) any {
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
                                          return func (v13 core.IntegerValue) any {
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
                                            }(v13)
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
                        }, func (_p graph.Graph) func(core.Term) any {
                          return func (v13 graph.Graph) func(core.Term) any {
                            return func (v23 core.Term) any {
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
                                                case core.LiteralInteger:
                                                return func (v14 core.IntegerValue) any {
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
                                                  }(v14)
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
                              }, v13, v23)
                            }
                          }(_p).(func(core.Term) any)
                        }, v12, v22)
                      }
                    }(_p).(func(core.Term) any)
                  }, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_adjacencyList []any) any {
              return libeithers.Bind(extracthelpers.RequireField("expected", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                      return func (v12 graph.Graph) func(core.Term) any {
                        return func (v22 core.Term) any {
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
                                            case core.LiteralInteger:
                                            return func (v13 core.IntegerValue) any {
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
                                              }(v13)
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
                          }, v12, v22)
                        }
                      }(_p).(func(core.Term) any)
                    }, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_expected []any) any {
                return [2]any{"right", testing.TopologicalSortSCCTestCase{AdjacencyList: field_adjacencyList, Expected: field_expected}}
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

func SerializationTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodeast.Expr, fieldMap.([]any), cx)).(func(any) any)(func (field_input ast.Expr) any {
              return libeithers.Bind(extracthelpers.RequireField("output", func (_p graph.Graph) func(any) any {
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
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_output string) any {
                return [2]any{"right", testing.SerializationTestCase{Input: field_input, Output: field_output}}
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

func SimplifyTermTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                return [2]any{"right", testing.SimplifyTermTestCase{Input: field_input, Output: field_output}}
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

func NormalizeTypeVariablesTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                return [2]any{"right", testing.NormalizeTypeVariablesTestCase{Input: field_input, Output: field_output}}
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

func TypeReductionTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Type) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Type) any {
                return [2]any{"right", testing.TypeReductionTestCase{Input: field_input, Output: field_output}}
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

func WriterTestCase (a func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", a, fieldMap.([]any), cx)).(func(any) any)(func (field_input any) any {
              return libeithers.Bind(extracthelpers.RequireField("output", func (_p graph.Graph) func(any) any {
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
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_output string) any {
                return [2]any{"right", testing.WriterTestCase[any]{Input: field_input, Output: field_output}}
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

func SubstInTypeTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("substitution", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                    return func (v12 graph.Graph) func(core.Term) any {
                      return func (v22 core.Term) any {
                        return extracthelpers.DecodePair(decodecore.Name, decodecore.Type_, v12, v22)
                      }
                    }(_p).(func(core.Term) any)
                  }, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_substitution []any) any {
              return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Type) any {
                return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Type) any {
                  return [2]any{"right", testing.SubstInTypeTestCase{Substitution: field_substitution, Input: field_input, Output: field_output}}
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

func VariableOccursInTypeTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("variable", decodecore.Name, fieldMap.([]any), cx)).(func(any) any)(func (field_variable core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("type", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.Type) any {
                return libeithers.Bind(extracthelpers.RequireField("expected", func (_p graph.Graph) func(any) any {
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
                      }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx2, raw2.(core.Term)))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_expected bool) any {
                  return [2]any{"right", testing.VariableOccursInTypeTestCase{Variable: field_variable, Type_: field_type, Expected: field_expected}}
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

func UnshadowVariablesTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("input", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_input core.Term) any {
              return libeithers.Bind(extracthelpers.RequireField("output", decodecore.Term, fieldMap.([]any), cx)).(func(any) any)(func (field_output core.Term) any {
                return [2]any{"right", testing.UnshadowVariablesTestCase{Input: field_input, Output: field_output}}
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

func UnifyTypesTestCase (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("schemaTypes", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeList(decodecore.Name, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_schemaTypes []any) any {
              return libeithers.Bind(extracthelpers.RequireField("left", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_left core.Type) any {
                return libeithers.Bind(extracthelpers.RequireField("right", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_right core.Type) any {
                  return libeithers.Bind(extracthelpers.RequireField("expected", func (_p graph.Graph) func(any) any {
                    return func (v1 graph.Graph) func(any) any {
                      return func (v2 any) any {
                        return extracthelpers.DecodeEither(func (_p graph.Graph) func(core.Term) any {
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
                        }, decodetyping.TypeSubst, v1, v2.(core.Term))
                      }
                    }(_p).(func(any) any)
                  }, fieldMap.([]any), cx)).(func(any) any)(func (field_expected any) any {
                    return [2]any{"right", testing.UnifyTypesTestCase{SchemaTypes: field_schemaTypes, Left: field_left, Right: field_right, Expected: field_expected}}
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

func JoinTypesTestCase (cx graph.Graph, raw core.Term) any {
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
                return libeithers.Bind(extracthelpers.RequireField("expected", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeEither(extracthelpers.DecodeUnit, func (_p graph.Graph) func(core.Term) any {
                        return func (v12 graph.Graph) func(core.Term) any {
                          return func (v22 core.Term) any {
                            return extracthelpers.DecodeList(decodetyping.TypeConstraint, v12, v22)
                          }
                        }(_p).(func(core.Term) any)
                      }, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_expected any) any {
                  return [2]any{"right", testing.JoinTypesTestCase{Left: field_left, Right: field_right, Expected: field_expected}}
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
