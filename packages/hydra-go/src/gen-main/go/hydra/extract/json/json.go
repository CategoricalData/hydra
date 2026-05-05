// Note: this is an automatically generated file. Do not edit.

package extractjson

import (
  jsonmodel "hydra.dev/hydra/json/model"
  libeithers "hydra.dev/hydra/lib/eithers"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
)

func ExpectArray (value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueArray:
      return func (els []any) any {
        return [2]any{"right", els}
      }(v.Value)
      default:
      return [2]any{"left", libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("JSON array")).(func(any) any)(libstrings.Cat2(" but found ").(func(any) any)(ShowValue[jsonmodel.Value](value)))}
    }
    return nil
  }(value)
}

func ExpectNumber (value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueNumber:
      return func (d float64) any {
        return [2]any{"right", d}
      }(v.Value)
      default:
      return [2]any{"left", libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("JSON number")).(func(any) any)(libstrings.Cat2(" but found ").(func(any) any)(ShowValue[jsonmodel.Value](value)))}
    }
    return nil
  }(value)
}

func ExpectObject (value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueObject:
      return func (m []any) any {
        return [2]any{"right", m}
      }(v.Value)
      default:
      return [2]any{"left", libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("JSON object")).(func(any) any)(libstrings.Cat2(" but found ").(func(any) any)(ShowValue[jsonmodel.Value](value)))}
    }
    return nil
  }(value)
}

func ExpectString (value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueString_:
      return func (s string) any {
        return [2]any{"right", s}
      }(v.Value)
      default:
      return [2]any{"left", libstrings.Cat2(libstrings.Cat2("expected ").(func(any) any)("JSON string")).(func(any) any)(libstrings.Cat2(" but found ").(func(any) any)(ShowValue[jsonmodel.Value](value)))}
    }
    return nil
  }(value)
}

func Opt[T0 any] (fname T0, m []any) any {
  return libmaps.Lookup(fname).(func(any) any)(m)
}

func OptArray[T0 any] (fname T0, m []any) any {
  return libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (a jsonmodel.Value) any {
    return libeithers.Map(func (x []any) any {
      return func () any {
        _v := x
        return &_v
      }()
    }).(func(any) any)(ExpectArray(a))
  }).(func(any) any)(Opt(fname, m))
}

func OptString[T0 any] (fname T0, m []any) any {
  return libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (s jsonmodel.Value) any {
    return libeithers.Map(func (x string) any {
      return func () any {
        _v := x
        return &_v
      }()
    }).(func(any) any)(ExpectString(s))
  }).(func(any) any)(Opt(fname, m))
}

func Require[T0 any] (fname T0, m []any) any {
  return libmaybes.Maybe([2]any{"left", libstrings.Cat([]any{"required attribute ", ShowValue[T0](fname), " not found"})}).(func(any) any)(func (value any) any {
    return [2]any{"right", value}
  }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(m))
}

func RequireArray[T0 any] (fname T0, m []any) any {
  return libeithers.Bind(Require(fname, m)).(func(any) any)(ExpectArray)
}

func RequireNumber[T0 any] (fname T0, m []any) any {
  return libeithers.Bind(Require(fname, m)).(func(any) any)(ExpectNumber)
}

func RequireString[T0 any] (fname T0, m []any) any {
  return libeithers.Bind(Require(fname, m)).(func(any) any)(ExpectString)
}

func ShowValue[T0 any] (value T0) string {
  return "TODO: implement showValue"
}
