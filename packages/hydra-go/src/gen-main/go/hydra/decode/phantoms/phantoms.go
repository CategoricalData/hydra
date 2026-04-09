// Note: this is an automatically generated file. Do not edit.

package decodephantoms

import (
  "hydra.dev/hydra/core"
  decodecore "hydra.dev/hydra/decode/core"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  "hydra.dev/hydra/phantoms"
)

func TBinding[T0 any] (a T0, cx graph.Graph, raw core.Term) any {
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
              return libeithers.Bind(extracthelpers.RequireField("term", func (_p graph.Graph) func(any) any {
                return func (v1 graph.Graph) func(any) any {
                  return func (v2 any) any {
                    return TTerm(a, v1, v2.(core.Term))
                  }
                }(_p).(func(any) any)
              }, fieldMap.([]any), cx)).(func(any) any)(func (field_term phantoms.TTerm) any {
                return [2]any{"right", phantoms.TBinding[any]{Name: field_name, Term: field_term}}
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

func TTerm[T0 any] (a T0, cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermWrap:
        return func (wrappedTerm core.WrappedTerm) any {
          return libeithers.Map(func (b core.Term) any {
            return phantoms.TTerm(b)
          }).(func(any) any)(decodecore.Term(cx, func (v any) any {
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
