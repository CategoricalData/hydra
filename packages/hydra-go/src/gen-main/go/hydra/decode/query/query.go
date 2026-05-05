// Note: this is an automatically generated file. Do not edit.

package decodequery

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
  "hydra.dev/hydra/query"
)

func ComparisonConstraint (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("equal"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.ComparisonConstraintEqual{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("notEqual"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.ComparisonConstraintNotEqual{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("lessThan"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.ComparisonConstraintLessThan{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("greaterThan"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.ComparisonConstraintGreaterThan{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("lessThanOrEqual"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.ComparisonConstraintLessThanOrEqual{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("greaterThanOrEqual"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.ComparisonConstraintGreaterThanOrEqual{Value: t}
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

func Edge (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("type", decodecore.Name, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("out", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeMaybe(decodecore.Name, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_out any) any {
                return libeithers.Bind(extracthelpers.RequireField("in", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeMaybe(decodecore.Name, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_in any) any {
                  return [2]any{"right", query.Edge{Type_: field_type, Out: field_out, In: field_in}}
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

func GraphPattern (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("graph", decodecore.Name, fieldMap.([]any), cx)).(func(any) any)(func (field_graph core.Name) any {
              return libeithers.Bind(extracthelpers.RequireField("patterns", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(Pattern, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_patterns []any) any {
                return [2]any{"right", query.GraphPattern{Graph: field_graph, Patterns: field_patterns}}
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

func Node (cx graph.Graph, raw core.Term) any {
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
              return libeithers.Map(func (t core.Term) any {
                return query.Node_Term{Value: t}
              }).(func(any) any)(decodecore.Term(cx, input))
            }}, [2]any{core.Name("variable"), func (input core.Term) any {
              return libeithers.Map(func (t query.Variable) any {
                return query.Node_Variable{Value: t}
              }).(func(any) any)(Variable(cx, input))
            }}, [2]any{core.Name("wildcard"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.Node_Wildcard{Value: t}
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

func Path (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("step"), func (input core.Term) any {
              return libeithers.Map(func (t query.Step) any {
                return query.PathStep{Value: t}
              }).(func(any) any)(Step(cx, input))
            }}, [2]any{core.Name("regex"), func (input core.Term) any {
              return libeithers.Map(func (t query.RegexSequence) any {
                return query.PathRegex{Value: t}
              }).(func(any) any)(RegexSequence(cx, input))
            }}, [2]any{core.Name("inverse"), func (input core.Term) any {
              return libeithers.Map(func (t query.Path) any {
                return query.PathInverse{Value: t}
              }).(func(any) any)(Path(cx, input))
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

func PathEquation (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("left", Path, fieldMap.([]any), cx)).(func(any) any)(func (field_left query.Path) any {
              return libeithers.Bind(extracthelpers.RequireField("right", Path, fieldMap.([]any), cx)).(func(any) any)(func (field_right query.Path) any {
                return [2]any{"right", query.PathEquation{Left: field_left, Right: field_right}}
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

func Pattern (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("triple"), func (input core.Term) any {
              return libeithers.Map(func (t query.TriplePattern) any {
                return query.PatternTriple{Value: t}
              }).(func(any) any)(TriplePattern(cx, input))
            }}, [2]any{core.Name("negation"), func (input core.Term) any {
              return libeithers.Map(func (t query.Pattern) any {
                return query.PatternNegation{Value: t}
              }).(func(any) any)(Pattern(cx, input))
            }}, [2]any{core.Name("conjunction"), func (input core.Term) any {
              return libeithers.Map(func (t []any) any {
                return query.PatternConjunction{Value: t}
              }).(func(any) any)(extracthelpers.DecodeList(Pattern, cx, input))
            }}, [2]any{core.Name("disjunction"), func (input core.Term) any {
              return libeithers.Map(func (t []any) any {
                return query.PatternDisjunction{Value: t}
              }).(func(any) any)(extracthelpers.DecodeList(Pattern, cx, input))
            }}, [2]any{core.Name("graph"), func (input core.Term) any {
              return libeithers.Map(func (t query.GraphPattern) any {
                return query.PatternGraph{Value: t}
              }).(func(any) any)(GraphPattern(cx, input))
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

func PatternImplication (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("antecedent", Pattern, fieldMap.([]any), cx)).(func(any) any)(func (field_antecedent query.Pattern) any {
              return libeithers.Bind(extracthelpers.RequireField("consequent", Pattern, fieldMap.([]any), cx)).(func(any) any)(func (field_consequent query.Pattern) any {
                return [2]any{"right", query.PatternImplication{Antecedent: field_antecedent, Consequent: field_consequent}}
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

func Query (cx graph.Graph, raw core.Term) any {
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
                  return extracthelpers.DecodeList(Variable, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_variables []any) any {
              return libeithers.Bind(extracthelpers.RequireField("patterns", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(Pattern, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_patterns []any) any {
                return [2]any{"right", query.Query{Variables: field_variables, Patterns: field_patterns}}
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

func Range_ (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("min", func (_p graph.Graph) func(any) any {
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
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_min int32) any {
              return libeithers.Bind(extracthelpers.RequireField("max", func (_p graph.Graph) func(any) any {
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
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_max int32) any {
                return [2]any{"right", query.Range{Min_: field_min, Max_: field_max}}
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

func RegexQuantifier (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("one"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.RegexQuantifierOne{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("zeroOrOne"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.RegexQuantifierZeroOrOne{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("zeroOrMore"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.RegexQuantifierZeroOrMore{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("oneOrMore"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return query.RegexQuantifierOneOrMore{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("exactly"), func (input core.Term) any {
              return libeithers.Map(func (t int32) any {
                return query.RegexQuantifierExactly{Value: t}
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
            }}, [2]any{core.Name("atLeast"), func (input core.Term) any {
              return libeithers.Map(func (t int32) any {
                return query.RegexQuantifierAtLeast{Value: t}
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
            }}, [2]any{core.Name("range"), func (input core.Term) any {
              return libeithers.Map(func (t query.Range) any {
                return query.RegexQuantifierRange_{Value: t}
              }).(func(any) any)(Range_(cx, input))
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

func RegexSequence (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("path", Path, fieldMap.([]any), cx)).(func(any) any)(func (field_path query.Path) any {
              return libeithers.Bind(extracthelpers.RequireField("quantifier", RegexQuantifier, fieldMap.([]any), cx)).(func(any) any)(func (field_quantifier query.RegexQuantifier) any {
                return [2]any{"right", query.RegexSequence{Path: field_path, Quantifier: field_quantifier}}
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

func Step (cx graph.Graph, raw core.Term) any {
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
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("edge"), func (input core.Term) any {
              return libeithers.Map(func (t query.Edge) any {
                return query.StepEdge{Value: t}
              }).(func(any) any)(Edge(cx, input))
            }}, [2]any{core.Name("project"), func (input core.Term) any {
              return libeithers.Map(func (t core.Projection) any {
                return query.StepProject{Value: t}
              }).(func(any) any)(decodecore.Projection(cx, input))
            }}, [2]any{core.Name("compare"), func (input core.Term) any {
              return libeithers.Map(func (t query.ComparisonConstraint) any {
                return query.StepCompare{Value: t}
              }).(func(any) any)(ComparisonConstraint(cx, input))
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

func TriplePattern (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("subject", Node, fieldMap.([]any), cx)).(func(any) any)(func (field_subject query.Node_) any {
              return libeithers.Bind(extracthelpers.RequireField("predicate", Path, fieldMap.([]any), cx)).(func(any) any)(func (field_predicate query.Path) any {
                return libeithers.Bind(extracthelpers.RequireField("object", Node, fieldMap.([]any), cx)).(func(any) any)(func (field_object query.Node_) any {
                  return [2]any{"right", query.TriplePattern{Subject: field_subject, Predicate: field_predicate, Object: field_object}}
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

func Variable (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b string) any {
            return query.Variable(b)
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
