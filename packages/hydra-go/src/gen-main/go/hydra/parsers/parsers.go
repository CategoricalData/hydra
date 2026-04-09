// Note: this is an automatically generated file. Do not edit.

package parsers

import (
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/parsing"
)

func Alt[T0 any] (p1 parsing.Parser[T0], p2 parsing.Parser[T0]) parsing.Parser[T0] {
  return func () any {
    parse := func (input string) any {
      return func (v1 parsing.ParseResult[T0]) any {
        return func (x any) any {
          switch v := x.(type) {
            case parsing.ParseResultSuccess[T0]:
            return func (s parsing.ParseSuccess[T0]) any {
              return parsing.ParseResultSuccess[T0]{Value: s}
            }(v.Value)
            case parsing.ParseResultFailure[T0]:
            return func (e parsing.ParseError) any {
              return liblogic.IfElse(libequality.Equal(func (v any) any {
                return v.(parsing.ParseError).Remainder
              }(e)).(func(any) any)(input)).(func(any) any)(func(string) parsing.ParseResult[T0](p2)(input)).(func(any) any)(parsing.ParseResultFailure[T0]{Value: e})
            }(v.Value)
          }
          return nil
        }(v1)
      }(func(string) parsing.ParseResult[T0](p1)(input))
    }
    return parsing.Parser[T0](func (_w string) parsing.ParseResult[T0] {
      return parse(_w).(parsing.ParseResult[T0])
    })
  }().(parsing.Parser[T0])
}

var AnyChar = Satisfy(func (_p int32) bool {
  return func (_ int32) bool {
    return true
  }(_p).(bool)
})

func Apply[T0, T1 any] (pf parsing.Parser[func(T0) T1], pa parsing.Parser[T0]) parsing.Parser[T1] {
  return func () any {
    parse := func (input string) any {
      return func (v1 parsing.ParseResult[func(T0) T1]) any {
        return func (x any) any {
          switch v := x.(type) {
            case parsing.ParseResultSuccess[func(T0) T1]:
            return func (sf parsing.ParseSuccess[func(T0) T1]) any {
              return func (v12 parsing.ParseResult[T0]) any {
                return func (x any) any {
                  switch v := x.(type) {
                    case parsing.ParseResultSuccess[T0]:
                    return func (sa parsing.ParseSuccess[T0]) any {
                      return parsing.ParseResultSuccess[T1]{Value: parsing.ParseSuccess[T1]{Value: func (v any) any {
                        return v.(parsing.ParseSuccess[T1]).Value
                      }(sf).(func(any) any)(func (v any) any {
                        return v.(parsing.ParseSuccess[T1]).Value
                      }(sa)).(T1), Remainder: func (v any) any {
                        return v.(parsing.ParseSuccess[T1]).Remainder
                      }(sa).(string)}}
                    }(v.Value)
                    case parsing.ParseResultFailure[T0]:
                    return func (e parsing.ParseError) any {
                      return parsing.ParseResultFailure[T1]{Value: e}
                    }(v.Value)
                  }
                  return nil
                }(v12)
              }(func(string) parsing.ParseResult[T0](pa)(func (v any) any {
                return v.(parsing.ParseSuccess[T0]).Remainder
              }(sf).(string)))
            }(v.Value)
            case parsing.ParseResultFailure[func(T0) T1]:
            return func (e parsing.ParseError) any {
              return parsing.ParseResultFailure[T1]{Value: e}
            }(v.Value)
          }
          return nil
        }(v1)
      }(func(string) parsing.ParseResult[func(T0) T1](pf)(input))
    }
    return parsing.Parser[T1](func (_w string) parsing.ParseResult[T1] {
      return parse(_w).(parsing.ParseResult[T1])
    })
  }().(parsing.Parser[T1])
}

func Between[T0, T1, T2 any] (open parsing.Parser[T0], close_ parsing.Parser[T1], p parsing.Parser[T2]) parsing.Parser[T2] {
  return Bind(open, func (_p T0) parsing.Parser[T2] {
    return func (_ T0) parsing.Parser[T2] {
      return Bind(p, func (_p T2) parsing.Parser[T2] {
        return func (x T2) parsing.Parser[T2] {
          return Bind(close_, func (_p T1) parsing.Parser[T2] {
            return func (_2 T1) parsing.Parser[T2] {
              return Pure[T2](x)
            }(_p).(parsing.Parser[T2])
          })
        }(_p).(parsing.Parser[T2])
      })
    }(_p).(parsing.Parser[T2])
  })
}

func Bind[T0, T1 any] (pa parsing.Parser[T0], f func(T0) parsing.Parser[T1]) parsing.Parser[T1] {
  return func () any {
    parse := func (input string) any {
      return func (v1 parsing.ParseResult[T0]) any {
        return func (x any) any {
          switch v := x.(type) {
            case parsing.ParseResultSuccess[T0]:
            return func (s parsing.ParseSuccess[T0]) any {
              return func(string) parsing.ParseResult[T1](f(func (v any) any {
                return v.(parsing.ParseSuccess[T0]).Value
              }(s).(T0)))(func (v any) any {
                return v.(parsing.ParseSuccess[T0]).Remainder
              }(s).(string))
            }(v.Value)
            case parsing.ParseResultFailure[T0]:
            return func (e parsing.ParseError) any {
              return parsing.ParseResultFailure[T1]{Value: e}
            }(v.Value)
          }
          return nil
        }(v1)
      }(func(string) parsing.ParseResult[T0](pa)(input))
    }
    return parsing.Parser[T1](func (_w string) parsing.ParseResult[T1] {
      return parse(_w).(parsing.ParseResult[T1])
    })
  }().(parsing.Parser[T1])
}

func Char (c int32) parsing.Parser[int32] {
  return Satisfy(func (_p int32) bool {
    return func (x int32) bool {
      return libequality.Equal(x).(func(any) any)(c)
    }(_p).(bool)
  })
}

func Choice[T0 any] (ps []any) parsing.Parser[T0] {
  return liblists.Foldl(Alt[T0]).(func(any) any)(Fail[T0]("no choice matched")).(func(any) any)(ps).(parsing.Parser[T0])
}

var Eof = parsing.Parser[struct{}](func (_w string) parsing.ParseResult[struct{}] {
  return func (input string) any {
    return liblogic.IfElse(libequality.Equal(input).(func(any) any)("")).(func(any) any)(parsing.ParseResultSuccess[struct{}]{Value: parsing.ParseSuccess[struct{}]{Value: struct{}{}, Remainder: ""}}).(func(any) any)(parsing.ParseResultFailure[struct{}]{Value: parsing.ParseError{Message: "expected end of input", Remainder: input}})
  }(_w).(parsing.ParseResult[struct{}])
})

func Fail[T0 any] (msg string) parsing.Parser[T0] {
  return parsing.Parser[T0](func (_w string) parsing.ParseResult[T0] {
    return func (input string) any {
      return parsing.ParseResultFailure[T0]{Value: parsing.ParseError{Message: msg, Remainder: input}}
    }(_w).(parsing.ParseResult[T0])
  })
}

func Lazy[T0 any] (f func(struct{}) parsing.Parser[T0]) parsing.Parser[T0] {
  return parsing.Parser[T0](func (_w string) parsing.ParseResult[T0] {
    return func (input string) any {
      return func(string) parsing.ParseResult[T0](f(struct{}{}))(input)
    }(_w).(parsing.ParseResult[T0])
  })
}

func Many[T0 any] (p parsing.Parser[T0]) parsing.Parser[[]any] {
  return Alt(Some[T0](p), Pure([]any{}))
}

func Map_[T0, T1 any] (f func(T0) T1, pa parsing.Parser[T0]) parsing.Parser[T1] {
  return func () any {
    parse := func (input string) any {
      return func (v1 parsing.ParseResult[T0]) any {
        return func (x any) any {
          switch v := x.(type) {
            case parsing.ParseResultSuccess[T0]:
            return func (s parsing.ParseSuccess[T0]) any {
              return parsing.ParseResultSuccess[T1]{Value: parsing.ParseSuccess[T1]{Value: f(func (v any) any {
                return v.(parsing.ParseSuccess[T1]).Value
              }(s).(T0)), Remainder: func (v any) any {
                return v.(parsing.ParseSuccess[T1]).Remainder
              }(s).(string)}}
            }(v.Value)
            case parsing.ParseResultFailure[T0]:
            return func (e parsing.ParseError) any {
              return parsing.ParseResultFailure[T1]{Value: e}
            }(v.Value)
          }
          return nil
        }(v1)
      }(func(string) parsing.ParseResult[T0](pa)(input))
    }
    return parsing.Parser[T1](func (_w string) parsing.ParseResult[T1] {
      return parse(_w).(parsing.ParseResult[T1])
    })
  }().(parsing.Parser[T1])
}

func Optional[T0 any] (p parsing.Parser[T0]) parsing.Parser[any] {
  return Alt[any](Map_(func (_p T0) T0 {
    return libmaybes.Pure(_p).(T0)
  }, p), Pure[any](nil))
}

func Pure[T0 any] (a T0) parsing.Parser[T0] {
  return parsing.Parser[T0](func (_w string) parsing.ParseResult[T0] {
    return func (input string) any {
      return parsing.ParseResultSuccess[T0]{Value: parsing.ParseSuccess[T0]{Value: a, Remainder: input}}
    }(_w).(parsing.ParseResult[T0])
  })
}

func RunParser[T0 any] (p parsing.Parser[T0], input string) parsing.ParseResult[T0] {
  return func(string) parsing.ParseResult[T0](p)(input).(parsing.ParseResult[T0])
}

func Satisfy (pred func(int32) bool) parsing.Parser[int32] {
  return func () any {
    parse := func (input string) any {
      return func () any {
        var codes any = libstrings.ToList(input)
        return libmaybes.Maybe(parsing.ParseResultFailure[int32]{Value: parsing.ParseError{Message: "unexpected end of input", Remainder: input}}).(func(any) any)(func (c int32) any {
          return func () any {
            var rest any = libstrings.FromList(liblists.Drop(1).(func(any) any)(codes))
            return liblogic.IfElse(pred(c)).(func(any) any)(parsing.ParseResultSuccess[int32]{Value: parsing.ParseSuccess[int32]{Value: c, Remainder: rest.(string)}}).(func(any) any)(parsing.ParseResultFailure[int32]{Value: parsing.ParseError{Message: "character did not satisfy predicate", Remainder: input}})
          }()
        }).(func(any) any)(liblists.SafeHead(codes))
      }()
    }
    return parsing.Parser[int32](func (_w string) parsing.ParseResult[int32] {
      return parse(_w).(parsing.ParseResult[int32])
    })
  }().(parsing.Parser[int32])
}

func SepBy[T0, T1 any] (p parsing.Parser[T0], sep parsing.Parser[T1]) parsing.Parser[[]any] {
  return Alt(SepBy1(p, sep), Pure([]any{}))
}

func SepBy1[T0, T1 any] (p parsing.Parser[T0], sep parsing.Parser[T1]) parsing.Parser[[]any] {
  return Bind(p, func (_p T0) parsing.Parser[[]any] {
    return func (x T0) parsing.Parser[[]any] {
      return Bind(Many[T0](Bind(sep, func (_p T1) parsing.Parser[any] {
        return func (_ T1) parsing.Parser[any] {
          return p
        }(_p).(parsing.Parser[any])
      })), func (_p []any) parsing.Parser[[]any] {
        return func (xs []any) parsing.Parser[[]any] {
          return Pure(liblists.Cons(x).(func(any) any)(xs))
        }(_p).(parsing.Parser[[]any])
      })
    }(_p).(parsing.Parser[[]any])
  })
}

func Some[T0 any] (p parsing.Parser[T0]) parsing.Parser[[]any] {
  return Bind(p, func (_p T0) parsing.Parser[[]any] {
    return func (x T0) parsing.Parser[[]any] {
      return Bind(Many[T0](p), func (_p []any) parsing.Parser[[]any] {
        return func (xs []any) parsing.Parser[[]any] {
          return Pure(liblists.Cons(x).(func(any) any)(xs))
        }(_p).(parsing.Parser[[]any])
      })
    }(_p).(parsing.Parser[[]any])
  })
}

func String_ (str string) parsing.Parser[string] {
  return parsing.Parser[string](func (_w string) parsing.ParseResult[string] {
    return func (input string) any {
      return func () any {
        var strCodes any = libstrings.ToList(str)
        return func () any {
          var inputCodes any = libstrings.ToList(input)
          return func () any {
            var strLen any = liblists.Length(strCodes)
            return func () any {
              var inputPrefix any = liblists.Take(strLen).(func(any) any)(inputCodes)
              return liblogic.IfElse(libequality.Equal(strCodes).(func(any) any)(inputPrefix)).(func(any) any)(parsing.ParseResultSuccess[string]{Value: parsing.ParseSuccess[string]{Value: str, Remainder: libstrings.FromList(liblists.Drop(strLen).(func(any) any)(inputCodes)).(string)}}).(func(any) any)(parsing.ParseResultFailure[string]{Value: parsing.ParseError{Message: libstrings.Cat2("expected: ").(func(any) any)(str).(string), Remainder: input}})
            }()
          }()
        }()
      }()
    }(_w).(parsing.ParseResult[string])
  })
}
