// Note: this is an automatically generated file. Do not edit.

package encodephantoms

import (
  "hydra.dev/hydra/core"
  encodecore "hydra.dev/hydra/encode/core"
  "hydra.dev/hydra/phantoms"
)

func TBinding[T0, T1 any] (a T0, x phantoms.TBinding[T1]) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.phantoms.TBinding"), Fields: []any{core.Field{Name: core.Name("name"), Term: encodecore.Name(func (v any) any {
    return v.(phantoms.TBinding[T1]).Name
  }(x).(core.Name))}, core.Field{Name: core.Name("term"), Term: TTerm(a, func (v any) any {
    return v.(phantoms.TBinding[T1]).Term
  }(x).(phantoms.TTerm))}}}}
}

func TTerm[T0 any] (a T0, x phantoms.TTerm) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.phantoms.TTerm"), Body: encodecore.Term(func (v any) any {
    return v
  }(x).(core.Term))}}
}
