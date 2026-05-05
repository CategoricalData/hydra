// Note: this is an automatically generated file. Do not edit.

package decoderelational

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  "hydra.dev/hydra/relational"
)

func ColumnName (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b string) any {
            return relational.ColumnName(b)
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

func ColumnSchema (t func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("name", ColumnName, fieldMap.([]any), cx)).(func(any) any)(func (field_name relational.ColumnName) any {
              return libeithers.Bind(extracthelpers.RequireField("domain", t, fieldMap.([]any), cx)).(func(any) any)(func (field_domain any) any {
                return [2]any{"right", relational.ColumnSchema[any]{Name: field_name, Domain: field_domain}}
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

func ForeignKey (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("foreignRelation", RelationName, fieldMap.([]any), cx)).(func(any) any)(func (field_foreignRelation relational.RelationName) any {
              return libeithers.Bind(extracthelpers.RequireField("keys", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeMap(ColumnName, ColumnName, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_keys []any) any {
                return [2]any{"right", relational.ForeignKey{ForeignRelation: field_foreignRelation, Keys: field_keys}}
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

func PrimaryKey (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b []any) any {
            return relational.PrimaryKey(b)
          }).(func(any) any)(extracthelpers.DecodeList(ColumnName, cx, func (v any) any {
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

func Relation (v func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b []any) any {
            return relational.Relation[any](b)
          }).(func(any) any)(extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
            return func (v1 graph.Graph) func(core.Term) any {
              return func (v2 core.Term) any {
                return Row(v, v1, v2)
              }
            }(_p).(func(core.Term) any)
          }, cx, func (v any) any {
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

func RelationName (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b string) any {
            return relational.RelationName(b)
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

func RelationSchema (t func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("name", RelationName, fieldMap.([]any), cx)).(func(any) any)(func (field_name relational.RelationName) any {
              return libeithers.Bind(extracthelpers.RequireField("columns", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                      return func (v12 graph.Graph) func(core.Term) any {
                        return func (v22 core.Term) any {
                          return ColumnSchema(t, v12, v22)
                        }
                      }(_p).(func(core.Term) any)
                    }, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_columns []any) any {
                return libeithers.Bind(extracthelpers.RequireField("primaryKeys", func (_p graph.Graph) func(any) any {
                  return func (v1 graph.Graph) func(any) any {
                    return func (v2 any) any {
                      return extracthelpers.DecodeList(PrimaryKey, v1, v2.(core.Term))
                    }
                  }(_p).(func(any) any)
                }, fieldMap.([]any), cx)).(func(any) any)(func (field_primaryKeys []any) any {
                  return libeithers.Bind(extracthelpers.RequireField("foreignKeys", func (_p graph.Graph) func(any) any {
                    return func (v1 graph.Graph) func(any) any {
                      return func (v2 any) any {
                        return extracthelpers.DecodeList(ForeignKey, v1, v2.(core.Term))
                      }
                    }(_p).(func(any) any)
                  }, fieldMap.([]any), cx)).(func(any) any)(func (field_foreignKeys []any) any {
                    return [2]any{"right", relational.RelationSchema[any]{Name: field_name, Columns: field_columns, PrimaryKeys: field_primaryKeys, ForeignKeys: field_foreignKeys}}
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

func Relationship (v func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b []any) any {
            return relational.Relationship[any](b)
          }).(func(any) any)(extracthelpers.DecodeSet(func (_p graph.Graph) func(core.Term) any {
            return func (v1 graph.Graph) func(core.Term) any {
              return func (v2 core.Term) any {
                return extracthelpers.DecodeMap(ColumnName, v, v1, v2)
              }
            }(_p).(func(core.Term) any)
          }, cx, func (v any) any {
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

func Row (v func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b []any) any {
            return relational.Row[any](b)
          }).(func(any) any)(extracthelpers.DecodeList(v, cx, func (v any) any {
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
