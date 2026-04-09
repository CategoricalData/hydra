// Note: this is an automatically generated file. Do not edit.

package formatting

import (
  libchars "hydra.dev/hydra/lib/chars"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/util"
)

func Capitalize (v1 string) string {
  return MapFirstLetter(func (_p string) string {
    return libstrings.ToUpper(_p).(string)
  }, v1)
}

func ConvertCase (from util.CaseConvention, to util.CaseConvention, original string) string {
  return func () any {
    var parts any = func () any {
      var byCaps any = func () any {
        splitOnUppercase := func (acc []any) any {
          return func (c int32) any {
            return liblists.Concat2(liblogic.IfElse(libchars.IsUpper(c)).(func(any) any)([]any{[]any{}}).(func(any) any)([]any{})).(func(any) any)(liblists.Cons(liblists.Cons(c).(func(any) any)(liblists.Head(acc))).(func(any) any)(liblists.Tail(acc)))
          }
        }
        return liblists.Map(libstrings.FromList).(func(any) any)(liblists.Foldl(splitOnUppercase).(func(any) any)([]any{[]any{}}).(func(any) any)(liblists.Reverse(libstrings.ToList(Decapitalize(original)))))
      }()
      var byUnderscores any = libstrings.SplitOn("_").(func(any) any)(original)
      return func (x any) any {
        switch v := x.(type) {
          case util.CaseConventionCamel:
          return func (_ struct{}) any {
            return byCaps
          }(v)
          case util.CaseConventionPascal:
          return func (_ struct{}) any {
            return byCaps
          }(v)
          case util.CaseConventionLowerSnake:
          return func (_ struct{}) any {
            return byUnderscores
          }(v)
          case util.CaseConventionUpperSnake:
          return func (_ struct{}) any {
            return byUnderscores
          }(v)
        }
        return nil
      }(from)
    }()
    return func (x any) any {
      switch v := x.(type) {
        case util.CaseConventionCamel:
        return func (_ struct{}) any {
          return Decapitalize(libstrings.Cat(liblists.Map(func (arg_ string) any {
            return Capitalize(libstrings.ToLower(arg_).(string))
          }).(func(any) any)(parts)).(string))
        }(v)
        case util.CaseConventionPascal:
        return func (_ struct{}) any {
          return libstrings.Cat(liblists.Map(func (arg_ string) any {
            return Capitalize(libstrings.ToLower(arg_).(string))
          }).(func(any) any)(parts))
        }(v)
        case util.CaseConventionLowerSnake:
        return func (_ struct{}) any {
          return libstrings.Intercalate("_").(func(any) any)(liblists.Map(libstrings.ToLower).(func(any) any)(parts))
        }(v)
        case util.CaseConventionUpperSnake:
        return func (_ struct{}) any {
          return libstrings.Intercalate("_").(func(any) any)(liblists.Map(libstrings.ToUpper).(func(any) any)(parts))
        }(v)
      }
      return nil
    }(to)
  }().(string)
}

func ConvertCaseCamelToLowerSnake (v1 string) string {
  return ConvertCase(util.CaseConventionCamel{}, util.CaseConventionLowerSnake{}, v1)
}

func ConvertCaseCamelToUpperSnake (v1 string) string {
  return ConvertCase(util.CaseConventionCamel{}, util.CaseConventionUpperSnake{}, v1)
}

func ConvertCasePascalToUpperSnake (v1 string) string {
  return ConvertCase(util.CaseConventionPascal{}, util.CaseConventionUpperSnake{}, v1)
}

func Decapitalize (v1 string) string {
  return MapFirstLetter(func (_p string) string {
    return libstrings.ToLower(_p).(string)
  }, v1)
}

func EscapeWithUnderscore (reserved []any, s string) string {
  return liblogic.IfElse(libsets.Member(s).(func(any) any)(reserved)).(func(any) any)(libstrings.Cat2(s).(func(any) any)("_")).(func(any) any)(s).(string)
}

func IndentLines (s string) string {
  return func () any {
    indent := func (l string) any {
      return libstrings.Cat2("    ").(func(any) any)(l)
    }
    return libstrings.Unlines(liblists.Map(indent).(func(any) any)(libstrings.Lines(s)))
  }().(string)
}

func JavaStyleComment (s string) string {
  return libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("/**\n").(func(any) any)(" * ")).(func(any) any)(s)).(func(any) any)("\n */").(string)
}

func MapFirstLetter (mapping func(string) string, s string) string {
  return func () any {
    var list any = libstrings.ToList(s)
    return func () any {
      var firstLetter any = mapping(libstrings.FromList(liblists.Pure(liblists.Head(list))).(string))
      return liblogic.IfElse(libstrings.Null(s)).(func(any) any)(s).(func(any) any)(libstrings.Cat2(firstLetter).(func(any) any)(libstrings.FromList(liblists.Tail(list))))
    }()
  }().(string)
}

func NonAlnumToUnderscores (input string) string {
  return func () any {
    isAlnum := func (c int32) any {
      return liblogic.Or(liblogic.And(libequality.Gte(c).(func(any) any)(65)).(func(any) any)(libequality.Lte(c).(func(any) any)(90))).(func(any) any)(liblogic.Or(liblogic.And(libequality.Gte(c).(func(any) any)(97)).(func(any) any)(libequality.Lte(c).(func(any) any)(122))).(func(any) any)(liblogic.And(libequality.Gte(c).(func(any) any)(48)).(func(any) any)(libequality.Lte(c).(func(any) any)(57))))
    }
    return func () any {
      replace := func (p any) any {
        return func (c int32) any {
          return func () any {
            var s any = libpairs.First(p)
            return func () any {
              var b any = libpairs.Second(p)
              return liblogic.IfElse(isAlnum(c)).(func(any) any)([2]any{liblists.Cons(c).(func(any) any)(s), false}).(func(any) any)(liblogic.IfElse(b).(func(any) any)([2]any{s, true}).(func(any) any)([2]any{liblists.Cons(95).(func(any) any)(s), true}))
            }()
          }()
        }
      }
      return func () any {
        var result any = liblists.Foldl(replace).(func(any) any)([2]any{[]any{}, false}).(func(any) any)(libstrings.ToList(input))
        return libstrings.FromList(liblists.Reverse(libpairs.First(result)))
      }()
    }()
  }().(string)
}

func SanitizeWithUnderscores (reserved []any, s string) string {
  return EscapeWithUnderscore(reserved, NonAlnumToUnderscores(s))
}

func ShowList[T0 any] (f func(T0) string, els []any) string {
  return libstrings.Cat([]any{"[", libstrings.Intercalate(", ").(func(any) any)(liblists.Map(f).(func(any) any)(els)), "]"}).(string)
}

func StripLeadingAndTrailingWhitespace (s string) string {
  return libstrings.FromList(liblists.DropWhile(libchars.IsSpace).(func(any) any)(liblists.Reverse(liblists.DropWhile(libchars.IsSpace).(func(any) any)(liblists.Reverse(libstrings.ToList(s)))))).(string)
}

func WithCharacterAliases (original string) string {
  return func () any {
    var aliases any = libmaps.FromList([]any{[2]any{32, "sp"}, [2]any{33, "excl"}, [2]any{34, "quot"}, [2]any{35, "num"}, [2]any{36, "dollar"}, [2]any{37, "percnt"}, [2]any{38, "amp"}, [2]any{39, "apos"}, [2]any{40, "lpar"}, [2]any{41, "rpar"}, [2]any{42, "ast"}, [2]any{43, "plus"}, [2]any{44, "comma"}, [2]any{45, "minus"}, [2]any{46, "period"}, [2]any{47, "sol"}, [2]any{58, "colon"}, [2]any{59, "semi"}, [2]any{60, "lt"}, [2]any{61, "equals"}, [2]any{62, "gt"}, [2]any{63, "quest"}, [2]any{64, "commat"}, [2]any{91, "lsqb"}, [2]any{92, "bsol"}, [2]any{93, "rsqb"}, [2]any{94, "circ"}, [2]any{95, "lowbar"}, [2]any{96, "grave"}, [2]any{123, "lcub"}, [2]any{124, "verbar"}, [2]any{125, "rcub"}, [2]any{126, "tilde"}})
    alias := func (c int32) any {
      return libmaybes.FromMaybe(liblists.Pure(c)).(func(any) any)(libmaybes.Map(libstrings.ToList).(func(any) any)(libmaps.Lookup(c).(func(any) any)(aliases)))
    }
    return libstrings.FromList(liblists.Filter(libchars.IsAlphaNum).(func(any) any)(liblists.Concat(liblists.Map(alias).(func(any) any)(libstrings.ToList(original)))))
  }().(string)
}

func WrapLine (maxlen int32, input string) string {
  return func () any {
    var helper func([]any) any
    helper = func (prev []any) any {
      return func (rem []any) any {
        return func () any {
          var trunc any = liblists.Take(maxlen).(func(any) any)(rem)
          var spanResult any = liblists.Span(func (c int32) any {
            return liblogic.And(liblogic.Not(libequality.Equal(c).(func(any) any)(32))).(func(any) any)(liblogic.Not(libequality.Equal(c).(func(any) any)(9)))
          }).(func(any) any)(liblists.Reverse(trunc))
          var prefix any = liblists.Reverse(libpairs.Second(spanResult))
          var suffix any = liblists.Reverse(libpairs.First(spanResult))
          return liblogic.IfElse(libequality.Lte(liblists.Length(rem)).(func(any) any)(maxlen)).(func(any) any)(liblists.Reverse(liblists.Cons(rem).(func(any) any)(prev))).(func(any) any)(liblogic.IfElse(liblists.Null(prefix)).(func(any) any)(helper(liblists.Cons(trunc).(func(any) any)(prev).([]any)).(func(any) any)(liblists.Drop(maxlen).(func(any) any)(rem))).(func(any) any)(helper(liblists.Cons(liblists.Init(prefix)).(func(any) any)(prev).([]any)).(func(any) any)(liblists.Concat2(suffix).(func(any) any)(liblists.Drop(maxlen).(func(any) any)(rem)))))
        }()
      }
    }
    return libstrings.FromList(liblists.Intercalate([]any{10}).(func(any) any)(helper([]any{}).(func(any) any)(libstrings.ToList(input))))
  }().(string)
}
