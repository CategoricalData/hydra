// Note: this is an automatically generated file. Do not edit.

package arity

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/graph"
  liblists "hydra.dev/hydra/lib/lists"
  libmath "hydra.dev/hydra/lib/math"
)

func FunctionArity (v1 core.Function) int32 {
  return func (x any) any {
    switch v := x.(type) {
      case core.FunctionElimination:
      return func (_ core.Elimination) any {
        return 1
      }(v.Value)
      case core.FunctionLambda:
      return func (arg_ core.Lambda) any {
        return libmath.Add(1).(func(any) any)(TermArity(func (v any) any {
          return v.(core.Lambda).Body
        }(arg_).(core.Term)))
      }(v.Value)
      case core.FunctionPrimitive:
      return func (_ core.Name) any {
        return 42
      }(v.Value)
    }
    return nil
  }(v1).(int32)
}

func PrimitiveArity (arg_ graph.Primitive) int32 {
  return TypeArity(func (v any) any {
    return v.(graph.Primitive).Type_
  }(arg_).(core.TypeScheme).Type_)
}

func TermArity (v1 core.Term) int32 {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermApplication:
      return func (arg_ core.Application) any {
        return libmath.Sub(TermArity(func (v any) any {
          return v.(core.Application).Function
        }(arg_).(core.Term))).(func(any) any)(1)
      }(v.Value)
      case core.TermFunction:
      return func (v12 core.Function) any {
        return FunctionArity(v12)
      }(v.Value)
      default:
      return 0
    }
    return nil
  }(v1).(int32)
}

func TypeArity (v1 core.Type) int32 {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (arg_ core.AnnotatedType) any {
        return TypeArity(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(arg_).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (arg_ core.ApplicationType) any {
        return TypeArity(func (v any) any {
          return v.(core.ApplicationType).Function
        }(arg_).(core.Type))
      }(v.Value)
      case core.TypeForall:
      return func (arg_ core.ForallType) any {
        return TypeArity(func (v any) any {
          return v.(core.ForallType).Body
        }(arg_).(core.Type))
      }(v.Value)
      case core.TypeFunction:
      return func (f core.FunctionType) any {
        return libmath.Add(1).(func(any) any)(TypeArity(func (v any) any {
          return v.(core.FunctionType).Codomain
        }(f).(core.Type)))
      }(v.Value)
      default:
      return 0
    }
    return nil
  }(v1).(int32)
}

func TypeSchemeArity (arg_ core.TypeScheme) int32 {
  return TypeArity(func (v any) any {
    return v.(core.TypeScheme).Type_
  }(arg_).(core.Type))
}

func UncurryType (t core.Type) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (arg_ core.AnnotatedType) any {
        return UncurryType(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(arg_).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (arg_ core.ApplicationType) any {
        return UncurryType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(arg_).(core.Type))
      }(v.Value)
      case core.TypeForall:
      return func (arg_ core.ForallType) any {
        return UncurryType(func (v any) any {
          return v.(core.ForallType).Body
        }(arg_).(core.Type))
      }(v.Value)
      case core.TypeFunction:
      return func (ft core.FunctionType) any {
        return liblists.Cons(func (v any) any {
          return v.(core.FunctionType).Domain
        }(ft)).(func(any) any)(UncurryType(func (v any) any {
          return v.(core.FunctionType).Codomain
        }(ft).(core.Type)))
      }(v.Value)
      default:
      return []any{t}
    }
    return nil
  }(t).([]any)
}
