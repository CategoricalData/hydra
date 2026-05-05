// Note: this is an automatically generated file. Do not edit.

package extracthelpers

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  liblists "hydra.dev/hydra/lib/lists"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
)

func DecodeEither (leftDecoder func(graph.Graph) func(core.Term) any, rightDecoder func(graph.Graph) func(core.Term) any, g graph.Graph, term core.Term) any {
  return libeithers.Bind(libeithers.Bimap(func (x string) any {
    return error.DecodingError(x)
  }).(func(any) any)(func (x core.Term) any {
    return x
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(g, term))).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermEither:
        return func (e any) any {
          return libeithers.Either(func (lv core.Term) any {
            return libeithers.Map(func (x any) any {
              return [2]any{"left", x}
            }).(func(any) any)(leftDecoder(g)(lv))
          }).(func(any) any)(func (rv core.Term) any {
            return libeithers.Map(func (x any) any {
              return [2]any{"right", x}
            }).(func(any) any)(rightDecoder(g)(rv))
          }).(func(any) any)(e)
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected either value")}
      }
      return nil
    }(stripped)
  })
}

func DecodeList (elemDecoder func(graph.Graph) func(core.Term) any, g graph.Graph, term core.Term) any {
  return libeithers.Bind(libeithers.Bimap(func (x string) any {
    return error.DecodingError(x)
  }).(func(any) any)(func (x core.Term) any {
    return x
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(g, term))).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermList:
        return func (els []any) any {
          return libeithers.MapList(func (v1 core.Term) any {
            return elemDecoder(g)(v1)
          }).(func(any) any)(els)
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected list")}
      }
      return nil
    }(stripped)
  })
}

func DecodeMap (keyDecoder func(graph.Graph) func(core.Term) any, valDecoder func(graph.Graph) func(core.Term) any, g graph.Graph, term core.Term) any {
  return libeithers.Bind(libeithers.Bimap(func (x string) any {
    return error.DecodingError(x)
  }).(func(any) any)(func (x core.Term) any {
    return x
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(g, term))).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermMap_:
        return func (m []any) any {
          return libeithers.Map(libmaps.FromList).(func(any) any)(libeithers.MapList(func (kv any) any {
            return libeithers.Bind(keyDecoder(g)(libpairs.First(kv))).(func(any) any)(func (k any) any {
              return libeithers.Map(func (v any) any {
                return [2]any{k, v}
              }).(func(any) any)(valDecoder(g)(libpairs.Second(kv)))
            })
          }).(func(any) any)(libmaps.ToList(m)))
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected map")}
      }
      return nil
    }(stripped)
  })
}

func DecodeMaybe (elemDecoder func(graph.Graph) func(core.Term) any, g graph.Graph, term core.Term) any {
  return libeithers.Bind(libeithers.Bimap(func (x string) any {
    return error.DecodingError(x)
  }).(func(any) any)(func (x core.Term) any {
    return x
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(g, term))).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermMaybe:
        return func (opt any) any {
          return libeithers.MapMaybe(func (v1 core.Term) any {
            return elemDecoder(g)(v1)
          }).(func(any) any)(opt)
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected optional value")}
      }
      return nil
    }(stripped)
  })
}

func DecodePair (firstDecoder func(graph.Graph) func(core.Term) any, secondDecoder func(graph.Graph) func(core.Term) any, g graph.Graph, term core.Term) any {
  return libeithers.Bind(libeithers.Bimap(func (x string) any {
    return error.DecodingError(x)
  }).(func(any) any)(func (x core.Term) any {
    return x
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(g, term))).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermPair:
        return func (p any) any {
          return libeithers.Bind(firstDecoder(g)(libpairs.First(p))).(func(any) any)(func (f any) any {
            return libeithers.Map(func (s any) any {
              return [2]any{f, s}
            }).(func(any) any)(secondDecoder(g)(libpairs.Second(p)))
          })
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected pair")}
      }
      return nil
    }(stripped)
  })
}

func DecodeSet (elemDecoder func(graph.Graph) func(core.Term) any, g graph.Graph, term core.Term) any {
  return libeithers.Bind(libeithers.Bimap(func (x string) any {
    return error.DecodingError(x)
  }).(func(any) any)(func (x core.Term) any {
    return x
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(g, term))).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermSet:
        return func (s []any) any {
          return libeithers.Map(libsets.FromList).(func(any) any)(libeithers.MapList(func (v1 core.Term) any {
            return elemDecoder(g)(v1)
          }).(func(any) any)(libsets.ToList(s)))
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected set")}
      }
      return nil
    }(stripped)
  })
}

func DecodeUnit (g graph.Graph, term core.Term) any {
  return libeithers.Bind(libeithers.Bimap(func (x string) any {
    return error.DecodingError(x)
  }).(func(any) any)(func (x core.Term) any {
    return x
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(g, term))).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnit:
        return func (_ struct{}) any {
          return [2]any{"right", struct{}{}}
        }(v)
        default:
        return [2]any{"left", error.DecodingError("expected a unit value")}
      }
      return nil
    }(stripped)
  })
}

func DecodeWrapped (bodyDecoder func(graph.Graph) func(core.Term) any, g graph.Graph, term core.Term) any {
  return libeithers.Bind(libeithers.Bimap(func (x string) any {
    return error.DecodingError(x)
  }).(func(any) any)(func (x core.Term) any {
    return x
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(g, term))).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wt core.WrappedTerm) any {
          return bodyDecoder(g)(func (v any) any {
            return v.(core.WrappedTerm).Body
          }(wt))
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected wrapped value")}
      }
      return nil
    }(stripped)
  })
}

func RequireField[T0, T1 any] (fieldName string, decoder func(T0) func(T1) any, fieldMap []any, g T0) any {
  return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"missing field ", fieldName, " in record"}).(string))}).(func(any) any)(func (fieldTerm T1) any {
    return decoder(g)(fieldTerm)
  }).(func(any) any)(libmaps.Lookup(core.Name(fieldName)).(func(any) any)(fieldMap))
}

func ToFieldMap (record core.Record) []any {
  return libmaps.FromList(liblists.Map(func (f core.Field) any {
    return [2]any{func (v any) any {
      return v.(core.Field).Name
    }(f), func (v any) any {
      return v.(core.Field).Term
    }(f)}
  }).(func(any) any)(func (v any) any {
    return v.(core.Record).Fields
  }(record))).([]any)
}
