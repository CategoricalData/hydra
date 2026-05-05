// Note: this is an automatically generated file. Do not edit.

package encodeclasses

import (
  "hydra.dev/hydra/classes"
  "hydra.dev/hydra/core"
)

func TypeClass (v1 classes.TypeClass) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case classes.TypeClassEquality:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.classes.TypeClass"), Field: core.Field{Name: core.Name("equality"), Term: core.TermUnit{}}}}
      }(v)
      case classes.TypeClassOrdering:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.classes.TypeClass"), Field: core.Field{Name: core.Name("ordering"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}
