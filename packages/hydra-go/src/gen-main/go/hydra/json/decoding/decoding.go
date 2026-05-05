// Note: this is an automatically generated file. Do not edit.

package extorgjsondecoding

import (
  jsonmodel "hydra.dev/hydra/json/model"
  libeithers "hydra.dev/hydra/lib/eithers"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
)

func DecodeArray (decodeElem func(jsonmodel.Value) any, v1 jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueArray:
      return func (a []any) any {
        return libeithers.MapList(decodeElem).(func(any) any)(a)
      }(v.Value)
      default:
      return [2]any{"left", "expected an array"}
    }
    return nil
  }(v1)
}

func DecodeBoolean (v1 jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueBoolean:
      return func (b bool) any {
        return [2]any{"right", b}
      }(v.Value)
      default:
      return [2]any{"left", "expected a boolean"}
    }
    return nil
  }(v1)
}

func DecodeField[T0 any] (decodeValue func(T0) any, name string, m []any) any {
  return libeithers.Bind(DecodeOptionalField(decodeValue, name, m)).(func(any) any)(func (v1 any) any {
    return libmaybes.Maybe([2]any{"left", libstrings.Cat2("missing field: ").(func(any) any)(name)}).(func(any) any)(func (f any) any {
      return [2]any{"right", f}
    }).(func(any) any)(v1)
  })
}

func DecodeObject (v1 jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueObject:
      return func (o []any) any {
        return [2]any{"right", o}
      }(v.Value)
      default:
      return [2]any{"left", "expected an object"}
    }
    return nil
  }(v1)
}

func DecodeOptionalField[T0, T3 any] (decodeValue func(T0) any, name T3, m []any) any {
  return libmaybes.Maybe([2]any{"right", nil}).(func(any) any)(func (v T0) any {
    return libeithers.Map(func (x any) any {
      return func () any {
        _v := x
        return &_v
      }()
    }).(func(any) any)(decodeValue(v))
  }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(m))
}

func DecodeString (v1 jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueString_:
      return func (s string) any {
        return [2]any{"right", s}
      }(v.Value)
      default:
      return [2]any{"left", "expected a string"}
    }
    return nil
  }(v1)
}
