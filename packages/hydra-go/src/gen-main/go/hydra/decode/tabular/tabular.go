// Note: this is an automatically generated file. Do not edit.

package decodetabular

import (
  "hydra.dev/hydra/core"
  decodecore "hydra.dev/hydra/decode/core"
  decoderelational "hydra.dev/hydra/decode/relational"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  "hydra.dev/hydra/relational"
  "hydra.dev/hydra/tabular"
)

func ColumnType (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("name", decoderelational.ColumnName, fieldMap.([]any), cx)).(func(any) any)(func (field_name relational.ColumnName) any {
              return libeithers.Bind(extracthelpers.RequireField("type", decodecore.Type_, fieldMap.([]any), cx)).(func(any) any)(func (field_type core.Type) any {
                return [2]any{"right", tabular.ColumnType{Name: field_name, Type_: field_type}}
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

func DataRow (v func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b []any) any {
            return tabular.DataRow[any](b)
          }).(func(any) any)(extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
            return func (v1 graph.Graph) func(core.Term) any {
              return func (v2 core.Term) any {
                return extracthelpers.DecodeMaybe(v, v1, v2)
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

func HeaderRow (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b []any) any {
            return tabular.HeaderRow(b)
          }).(func(any) any)(extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
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

func Table (v func(graph.Graph) func(core.Term) any, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("header", func (_p graph.Graph) func(any) any {
              return func (v1 graph.Graph) func(any) any {
                return func (v2 any) any {
                  return extracthelpers.DecodeMaybe(HeaderRow, v1, v2.(core.Term))
                }
              }(_p).(func(any) any)
            }, fieldMap.([]any), cx)).(func(any) any)(func (field_header any) any {
              return libeithers.Bind(extracthelpers.RequireField("data", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(func (_p graph.Graph) func(core.Term) any {
                      return func (v12 graph.Graph) func(core.Term) any {
                        return func (v22 core.Term) any {
                          return DataRow(v, v12, v22)
                        }
                      }(_p).(func(core.Term) any)
                    }, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_data []any) any {
                return [2]any{"right", tabular.Table[any]{Header: field_header, Data: field_data}}
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

func TableType (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermRecord:
        return func (record core.Record) any {
          return func () any {
            var fieldMap any = extracthelpers.ToFieldMap(record)
            return libeithers.Bind(extracthelpers.RequireField("name", decoderelational.RelationName, fieldMap.([]any), cx)).(func(any) any)(func (field_name relational.RelationName) any {
              return libeithers.Bind(extracthelpers.RequireField("columns", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return extracthelpers.DecodeList(ColumnType, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_columns []any) any {
                return [2]any{"right", tabular.TableType{Name: field_name, Columns: field_columns}}
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
