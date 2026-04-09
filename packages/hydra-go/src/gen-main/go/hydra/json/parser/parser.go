// Note: this is an automatically generated file. Do not edit.

package jsonparser

import (
  jsonmodel "hydra.dev/hydra/json/model"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  libliterals "hydra.dev/hydra/lib/literals"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/parsers"
  "hydra.dev/hydra/parsing"
)

var Whitespace = parsers.Map_(func (_p []any) any {
  return func (_ []any) any {
    return struct{}{}
  }(_p)
}, parsers.Many[int32](parsers.Satisfy(func (_p int32) bool {
  return func (c int32) bool {
    return liblists.Foldl(liblogic.Or).(func(any) any)(false).(func(any) any)([]any{libequality.Equal(c).(func(any) any)(32), libequality.Equal(c).(func(any) any)(9), libequality.Equal(c).(func(any) any)(10), libequality.Equal(c).(func(any) any)(13)})
  }(_p).(bool)
})))

func Token[T0 any] (p parsing.Parser[T0]) parsing.Parser[T0] {
  return parsers.Bind(p, func (_p T0) parsing.Parser[T0] {
    return func (x T0) parsing.Parser[T0] {
      return parsers.Bind(Whitespace, func (_p struct{}) parsing.Parser[T0] {
        return func (_ struct{}) parsing.Parser[T0] {
          return parsers.Pure[T0](x)
        }(_p).(parsing.Parser[T0])
      })
    }(_p).(parsing.Parser[T0])
  })
}

var JsonNull = parsers.Map_(func (_ any) any {
  return jsonmodel.ValueNull{}
}, Token[string](parsers.String_("null")))

var JsonBool = parsers.Alt[jsonmodel.Value](parsers.Map_(func (_ any) any {
  return jsonmodel.ValueBoolean{Value: true}
}, Token[string](parsers.String_("true"))), parsers.Map_(func (_ any) any {
  return jsonmodel.ValueBoolean{Value: false}
}, Token[string](parsers.String_("false"))))

var Digit = parsers.Satisfy(func (_p int32) bool {
  return func (c int32) bool {
    return liblogic.And(libequality.Gte(c).(func(any) any)(48)).(func(any) any)(libequality.Lte(c).(func(any) any)(57))
  }(_p).(bool)
})

var Digits = parsers.Map_(func (_p []any) any {
  return libstrings.FromList(_p)
}, parsers.Some[int32](Digit))

var JsonIntegerPart = parsers.Bind(parsers.Optional[int32](parsers.Char(45)), func (_p any) parsing.Parser[any] {
  return func (sign any) parsing.Parser[any] {
    return parsers.Bind(Digits, func (_p string) parsing.Parser[any] {
      return func (digits string) parsing.Parser[any] {
        return parsers.Pure[string](libmaybes.Maybe(digits).(func(any) any)(func (_ int32) any {
          return libstrings.Cat2("-").(func(any) any)(digits)
        }).(func(any) any)(sign))
      }(_p).(parsing.Parser[any])
    })
  }(_p).(parsing.Parser[any])
})

var JsonFractionPart = parsers.Optional[string](parsers.Bind(parsers.Char(46), func (_p int32) parsing.Parser[any] {
  return func (_ int32) parsing.Parser[any] {
    return parsers.Map_(func (_p string) any {
      return func (d string) any {
        return libstrings.Cat2(".").(func(any) any)(d)
      }(_p)
    }, Digits)
  }(_p).(parsing.Parser[any])
}))

var JsonExponentPart = parsers.Optional[string](parsers.Bind(parsers.Satisfy(func (_p int32) bool {
  return func (c int32) bool {
    return liblogic.Or(libequality.Equal(c).(func(any) any)(101)).(func(any) any)(libequality.Equal(c).(func(any) any)(69))
  }(_p).(bool)
}), func (_p int32) parsing.Parser[any] {
  return func (_ int32) parsing.Parser[any] {
    return parsers.Bind(parsers.Optional[int32](parsers.Satisfy(func (_p int32) bool {
      return func (c int32) bool {
        return liblogic.Or(libequality.Equal(c).(func(any) any)(43)).(func(any) any)(libequality.Equal(c).(func(any) any)(45))
      }(_p).(bool)
    })), func (_p any) parsing.Parser[any] {
      return func (sign any) parsing.Parser[any] {
        return parsers.Map_(func (_p string) any {
          return func (digits string) any {
            return libstrings.Cat2(libstrings.Cat2("e").(func(any) any)(libmaybes.Maybe("").(func(any) any)(func (arg_ int32) any {
              return libstrings.FromList(liblists.Pure(arg_))
            }).(func(any) any)(sign))).(func(any) any)(digits)
          }(_p)
        }, Digits)
      }(_p).(parsing.Parser[any])
    })
  }(_p).(parsing.Parser[any])
}))

var JsonNumber = Token[jsonmodel.Value](parsers.Bind(JsonIntegerPart, func (_p string) parsing.Parser[any] {
  return func (intPart string) parsing.Parser[any] {
    return parsers.Bind(JsonFractionPart, func (_p any) parsing.Parser[any] {
      return func (fracPart any) parsing.Parser[any] {
        return parsers.Bind(JsonExponentPart, func (_p any) parsing.Parser[any] {
          return func (expPart any) parsing.Parser[any] {
            return func () any {
              var numStr any = libstrings.Cat2(libstrings.Cat2(intPart).(func(any) any)(libmaybes.Maybe("").(func(any) any)(libequality.Identity).(func(any) any)(fracPart))).(func(any) any)(libmaybes.Maybe("").(func(any) any)(libequality.Identity).(func(any) any)(expPart))
              return parsers.Pure[jsonmodel.Value](jsonmodel.ValueNumber{Value: libmaybes.Maybe(0.0).(func(any) any)(libequality.Identity).(func(any) any)(libliterals.ReadBigfloat(numStr)).(float64)})
            }()
          }(_p).(parsing.Parser[any])
        })
      }(_p).(parsing.Parser[any])
    })
  }(_p).(parsing.Parser[any])
}))

var JsonEscapeChar = parsers.Choice[int32]([]any{parsers.Map_(func (_p int32) any {
  return func (_ int32) any {
    return 34
  }(_p)
}, parsers.Char(34)), parsers.Map_(func (_p int32) any {
  return func (_ int32) any {
    return 92
  }(_p)
}, parsers.Char(92)), parsers.Map_(func (_p int32) any {
  return func (_ int32) any {
    return 47
  }(_p)
}, parsers.Char(47)), parsers.Map_(func (_p int32) any {
  return func (_ int32) any {
    return 8
  }(_p)
}, parsers.Char(98)), parsers.Map_(func (_p int32) any {
  return func (_ int32) any {
    return 12
  }(_p)
}, parsers.Char(102)), parsers.Map_(func (_p int32) any {
  return func (_ int32) any {
    return 10
  }(_p)
}, parsers.Char(110)), parsers.Map_(func (_p int32) any {
  return func (_ int32) any {
    return 13
  }(_p)
}, parsers.Char(114)), parsers.Map_(func (_p int32) any {
  return func (_ int32) any {
    return 9
  }(_p)
}, parsers.Char(116))})

var JsonStringChar = parsers.Alt[int32](parsers.Bind(parsers.Char(92), func (_p int32) parsing.Parser[int32] {
  return func (_ int32) parsing.Parser[int32] {
    return JsonEscapeChar
  }(_p).(parsing.Parser[int32])
}), parsers.Satisfy(func (_p int32) bool {
  return func (c int32) bool {
    return liblogic.And(liblogic.Not(libequality.Equal(c).(func(any) any)(34))).(func(any) any)(liblogic.Not(libequality.Equal(c).(func(any) any)(92)))
  }(_p).(bool)
}))

var JsonString = Token[jsonmodel.Value](parsers.Bind(parsers.Char(34), func (_p int32) parsing.Parser[any] {
  return func (_ int32) parsing.Parser[any] {
    return parsers.Bind(parsers.Many[int32](JsonStringChar), func (_p []any) parsing.Parser[any] {
      return func (chars []any) parsing.Parser[any] {
        return parsers.Bind(parsers.Char(34), func (_p int32) parsing.Parser[any] {
          return func (_2 int32) parsing.Parser[any] {
            return parsers.Pure[jsonmodel.Value](jsonmodel.ValueString_{Value: libstrings.FromList(chars).(string)})
          }(_p).(parsing.Parser[any])
        })
      }(_p).(parsing.Parser[any])
    })
  }(_p).(parsing.Parser[any])
}))

var JsonArray = parsers.Map_(func (x any) any {
  return jsonmodel.ValueArray{Value: x.([]any)}
}, parsers.Between(Token[int32](parsers.Char(91)), Token[int32](parsers.Char(93)), parsers.SepBy(parsers.Lazy[jsonmodel.Value](func (_p struct{}) parsing.Parser[any] {
  return func (_ struct{}) parsing.Parser[any] {
    return JsonValue
  }(_p).(parsing.Parser[any])
}), Token[int32](parsers.Char(44)))))

var JsonKeyValue = parsers.Bind(Token[string](parsers.Bind(parsers.Char(34), func (_p int32) parsing.Parser[any] {
  return func (_ int32) parsing.Parser[any] {
    return parsers.Bind(parsers.Many[int32](JsonStringChar), func (_p []any) parsing.Parser[any] {
      return func (chars []any) parsing.Parser[any] {
        return parsers.Bind(parsers.Char(34), func (_p int32) parsing.Parser[any] {
          return func (_2 int32) parsing.Parser[any] {
            return parsers.Pure[string](libstrings.FromList(chars))
          }(_p).(parsing.Parser[any])
        })
      }(_p).(parsing.Parser[any])
    })
  }(_p).(parsing.Parser[any])
})), func (_p any) parsing.Parser[any] {
  return func (key any) parsing.Parser[any] {
    return parsers.Bind(Token[int32](parsers.Char(58)), func (_p any) parsing.Parser[any] {
      return func (_ any) parsing.Parser[any] {
        return parsers.Map_(func (v any) any {
          return [2]any{key, v}
        }, parsers.Lazy[jsonmodel.Value](func (_p struct{}) parsing.Parser[any] {
          return func (_2 struct{}) parsing.Parser[any] {
            return JsonValue
          }(_p).(parsing.Parser[any])
        }))
      }(_p).(parsing.Parser[any])
    })
  }(_p).(parsing.Parser[any])
})

var JsonObject = parsers.Map_(func (arg_ any) any {
  return jsonmodel.ValueObject{Value: libmaps.FromList(arg_).([]any)}
}, parsers.Between(Token[int32](parsers.Char(123)), Token[int32](parsers.Char(125)), parsers.SepBy(JsonKeyValue, Token[int32](parsers.Char(44)))))

var JsonValue = parsers.Choice[jsonmodel.Value]([]any{JsonNull, JsonBool, JsonNumber, JsonString, JsonArray, JsonObject})

func ParseJson (input string) parsing.ParseResult[jsonmodel.Value] {
  return func(string) parsing.ParseResult[jsonmodel.Value](parsers.Bind(Whitespace, func (_p struct{}) parsing.Parser[any] {
    return func (_ struct{}) parsing.Parser[any] {
      return parsers.Bind(JsonValue, func (_p jsonmodel.Value) parsing.Parser[any] {
        return func (v jsonmodel.Value) parsing.Parser[any] {
          return parsers.Bind(Whitespace, func (_p struct{}) parsing.Parser[any] {
            return func (_2 struct{}) parsing.Parser[any] {
              return parsers.Bind(parsers.Eof, func (_p struct{}) parsing.Parser[any] {
                return func (_3 struct{}) parsing.Parser[any] {
                  return parsers.Pure[jsonmodel.Value](v)
                }(_p).(parsing.Parser[any])
              })
            }(_p).(parsing.Parser[any])
          })
        }(_p).(parsing.Parser[any])
      })
    }(_p).(parsing.Parser[any])
  }))(input).(parsing.ParseResult[jsonmodel.Value])
}
