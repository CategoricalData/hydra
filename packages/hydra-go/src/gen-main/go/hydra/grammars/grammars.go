// Note: this is an automatically generated file. Do not edit.

package grammars

import (
  "hydra.dev/hydra/annotations"
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/formatting"
  "hydra.dev/hydra/grammar"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  libliterals "hydra.dev/hydra/lib/literals"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmath "hydra.dev/hydra/lib/math"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libstrings "hydra.dev/hydra/lib/strings"
  hmodule "hydra.dev/hydra/module"
  "hydra.dev/hydra/names"
)

func ChildName (lname string, n string) string {
  return libstrings.Cat([]any{lname, "_", formatting.Capitalize(n)}).(string)
}

func FindNames (pats []any) []any {
  return func () any {
    nextName := func (acc any) any {
      return func (pat grammar.Pattern) any {
        return func () any {
          var names any = libpairs.First(acc)
          return func () any {
            var nameMap any = libpairs.Second(acc)
            return func () any {
              var rn any = RawName(pat)
              return func () any {
                var nameAndIndex any = libmaybes.Maybe([2]any{rn, 1}).(func(any) any)(func (i int32) any {
                  return [2]any{libstrings.Cat2(rn).(func(any) any)(libliterals.ShowInt32(libmath.Add(i).(func(any) any)(1))), libmath.Add(i).(func(any) any)(1)}
                }).(func(any) any)(libmaps.Lookup(rn).(func(any) any)(nameMap))
                return func () any {
                  var nn any = libpairs.First(nameAndIndex)
                  return func () any {
                    var ni any = libpairs.Second(nameAndIndex)
                    return [2]any{liblists.Cons(nn).(func(any) any)(names), libmaps.Insert(rn).(func(any) any)(ni).(func(any) any)(nameMap)}
                  }()
                }()
              }()
            }()
          }()
        }()
      }
    }
    return liblists.Reverse(libpairs.First(liblists.Foldl(nextName).(func(any) any)([2]any{[]any{}, libmaps.Empty}).(func(any) any)(pats)))
  }().([]any)
}

func GrammarToModule (ns hmodule.Namespace, grammar grammar.Grammar, desc any) hmodule.Module {
  return func () any {
    var prodPairs any = liblists.Map(func (prod grammar.Production) any {
      return [2]any{func (v any) any {
        return v.(grammar.Production).Symbol
      }(prod), func (v any) any {
        return v.(grammar.Production).Pattern
      }(prod)}
    }).(func(any) any)(func (v any) any {
      return v
    }(grammar))
    return func () any {
      var capitalizedNames any = liblists.Map(func (pair any) any {
        return formatting.Capitalize(libpairs.First(pair).(string))
      }).(func(any) any)(prodPairs)
      return func () any {
        var patterns any = liblists.Map(func (pair any) any {
          return libpairs.Second(pair)
        }).(func(any) any)(prodPairs)
        return func () any {
          var elementPairs any = liblists.Concat(liblists.ZipWith(func (v1 string) any {
            return func (v2 grammar.Pattern) any {
              return MakeElements(false, ns, v1, v2)
            }
          }).(func(any) any)(capitalizedNames).(func(any) any)(patterns))
          return func () any {
            var elements any = liblists.Map(func (pair any) any {
              return func () any {
                var lname any = libpairs.First(pair)
                return func () any {
                  var elName any = ToName(ns, lname.(string))
                  return func () any {
                    var typ any = ReplacePlaceholders(elName, WrapType(libpairs.Second(pair).(core.Type)))
                    return annotations.TypeElement(elName.(core.Name), typ.(core.Type))
                  }()
                }()
              }()
            }).(func(any) any)(elementPairs)
            return hmodule.Module{Namespace: ns, Elements: elements.([]any), TermDependencies: []any{}, TypeDependencies: []any{}, Description: desc}
          }()
        }()
      }()
    }()
  }().(hmodule.Module)
}

func IsComplex (pat grammar.Pattern) bool {
  return func (x any) any {
    switch v := x.(type) {
      case grammar.PatternLabeled:
      return func (lp grammar.LabeledPattern) any {
        return IsComplex(func (v any) any {
          return v.(grammar.LabeledPattern).Pattern
        }(lp).(grammar.Pattern))
      }(v.Value)
      case grammar.PatternSequence:
      return func (pats []any) any {
        return IsNontrivial(true, pats)
      }(v.Value)
      case grammar.PatternAlternatives:
      return func (pats []any) any {
        return IsNontrivial(false, pats)
      }(v.Value)
      default:
      return false
    }
    return nil
  }(pat).(bool)
}

func IsNontrivial (isRecord bool, pats []any) bool {
  return func () any {
    var minPats any = Simplify(isRecord, pats)
    return func () any {
      isLabeled := func (p grammar.Pattern) any {
        return func (x any) any {
          switch v := x.(type) {
            case grammar.PatternLabeled:
            return func (_ grammar.LabeledPattern) any {
              return true
            }(v.Value)
            default:
            return false
          }
          return nil
        }(p)
      }
      return liblogic.IfElse(libequality.Equal(liblists.Length(minPats)).(func(any) any)(1)).(func(any) any)(isLabeled(liblists.Head(minPats).(grammar.Pattern))).(func(any) any)(true)
    }()
  }().(bool)
}

func MakeElements (omitTrivial bool, ns hmodule.Namespace, lname string, pat grammar.Pattern) []any {
  return func () any {
    var trivial any = liblogic.IfElse(omitTrivial).(func(any) any)([]any{}).(func(any) any)([]any{[2]any{lname, core.TypeUnit{}}})
    return func () any {
      descend := func (n string) any {
        return func (f func([]any) []any) any {
          return func (p grammar.Pattern) any {
            return func () any {
              var cpairs any = MakeElements(false, ns, ChildName(lname, n), p)
              return f(liblogic.IfElse(IsComplex(p)).(func(any) any)(liblists.Cons([2]any{lname, core.TypeVariable{Value: ToName(ns, libpairs.First(liblists.Head(cpairs)).(string))}}).(func(any) any)(cpairs)).(func(any) any)(liblogic.IfElse(liblists.Null(cpairs)).(func(any) any)([]any{[2]any{lname, core.TypeUnit{}}}).(func(any) any)(liblists.Cons([2]any{lname, libpairs.Second(liblists.Head(cpairs))}).(func(any) any)(liblists.Tail(cpairs)))).([]any))
            }()
          }
        }
      }
      return func () any {
        mod := func (n string) any {
          return func (f func(core.Type) core.Type) any {
            return func (p grammar.Pattern) any {
              return descend(n).(func(any) any)(func (pairs []any) any {
                return liblists.Cons([2]any{lname, f(libpairs.Second(liblists.Head(pairs)).(core.Type))}).(func(any) any)(liblists.Tail(pairs))
              }).(func(any) any)(p)
            }
          }
        }
        return func () any {
          var forPat func(grammar.Pattern) any
          forPat = func (pat2 grammar.Pattern) any {
            return func (x any) any {
              switch v := x.(type) {
                case grammar.PatternAlternatives:
                return func (pats []any) any {
                  return forRecordOrUnion(false).(func(any) any)(func (fields []any) any {
                    return core.TypeUnion{Value: fields}
                  }).(func(any) any)(pats)
                }(v.Value)
                case grammar.PatternConstant:
                return func (_ grammar.Constant) any {
                  return trivial
                }(v.Value)
                case grammar.PatternIgnored:
                return func (_ grammar.Pattern) any {
                  return []any{}
                }(v.Value)
                case grammar.PatternLabeled:
                return func (lp grammar.LabeledPattern) any {
                  return forPat(func (v any) any {
                    return v.(grammar.LabeledPattern).Pattern
                  }(lp).(grammar.Pattern))
                }(v.Value)
                case grammar.PatternNil_:
                return func (_ struct{}) any {
                  return trivial
                }(v)
                case grammar.PatternNonterminal:
                return func (s grammar.Symbol) any {
                  return []any{[2]any{lname, core.TypeVariable{Value: ToName(ns, func (v any) any {
                    return v
                  }(s).(string))}}}
                }(v.Value)
                case grammar.PatternOption:
                return func (p grammar.Pattern) any {
                  return mod("Option").(func(any) any)(func (x core.Type) any {
                    return core.TypeMaybe{Value: x}
                  }).(func(any) any)(p)
                }(v.Value)
                case grammar.PatternPlus:
                return func (p grammar.Pattern) any {
                  return mod("Elmt").(func(any) any)(func (x core.Type) any {
                    return core.TypeList{Value: x}
                  }).(func(any) any)(p)
                }(v.Value)
                case grammar.PatternRegex:
                return func (_ grammar.Regex) any {
                  return []any{[2]any{lname, core.TypeLiteral{Value: core.LiteralTypeString_{}}}}
                }(v.Value)
                case grammar.PatternSequence:
                return func (pats []any) any {
                  return forRecordOrUnion(true).(func(any) any)(func (fields []any) any {
                    return core.TypeRecord{Value: fields}
                  }).(func(any) any)(pats)
                }(v.Value)
                case grammar.PatternStar:
                return func (p grammar.Pattern) any {
                  return mod("Elmt").(func(any) any)(func (x core.Type) any {
                    return core.TypeList{Value: x}
                  }).(func(any) any)(p)
                }(v.Value)
              }
              return nil
            }(pat2)
          }
          forRecordOrUnion := func (isRecord bool) any {
            return func (construct func([]any) core.Type) any {
              return func (pats []any) any {
                return func () any {
                  var minPats any = Simplify(isRecord, pats)
                  return func () any {
                    var fieldNames any = FindNames(minPats.([]any))
                    return func () any {
                      toField := func (n string) any {
                        return func (p grammar.Pattern) any {
                          return descend(n).(func(any) any)(func (pairs []any) any {
                            return [2]any{core.FieldType{Name: core.Name(n), Type_: libpairs.Second(liblists.Head(pairs)).(core.Type)}, liblists.Tail(pairs)}
                          }).(func(any) any)(p)
                        }
                      }
                      return func () any {
                        var fieldPairs any = liblists.ZipWith(toField).(func(any) any)(fieldNames).(func(any) any)(minPats)
                        return func () any {
                          var fields any = liblists.Map(libpairs.First).(func(any) any)(fieldPairs)
                          return func () any {
                            var els any = liblists.Concat(liblists.Map(libpairs.Second).(func(any) any)(fieldPairs))
                            return liblogic.IfElse(IsNontrivial(isRecord, pats)).(func(any) any)(liblists.Cons([2]any{lname, construct(fields.([]any))}).(func(any) any)(els)).(func(any) any)(forPat(liblists.Head(minPats).(grammar.Pattern)))
                          }()
                        }()
                      }()
                    }()
                  }()
                }()
              }
            }
          }
          return forPat(pat)
        }()
      }()
    }()
  }().([]any)
}

func RawName (pat grammar.Pattern) string {
  return func (x any) any {
    switch v := x.(type) {
      case grammar.PatternAlternatives:
      return func (_ []any) any {
        return "alts"
      }(v.Value)
      case grammar.PatternConstant:
      return func (c grammar.Constant) any {
        return formatting.Capitalize(formatting.WithCharacterAliases(func (v any) any {
          return v
        }(c).(string)))
      }(v.Value)
      case grammar.PatternIgnored:
      return func (_ grammar.Pattern) any {
        return "ignored"
      }(v.Value)
      case grammar.PatternLabeled:
      return func (lp grammar.LabeledPattern) any {
        return func (v any) any {
          return v.(grammar.LabeledPattern).Label
        }(lp)
      }(v.Value)
      case grammar.PatternNil_:
      return func (_ struct{}) any {
        return "none"
      }(v)
      case grammar.PatternNonterminal:
      return func (s grammar.Symbol) any {
        return formatting.Capitalize(func (v any) any {
          return v
        }(s).(string))
      }(v.Value)
      case grammar.PatternOption:
      return func (p grammar.Pattern) any {
        return formatting.Capitalize(RawName(p))
      }(v.Value)
      case grammar.PatternPlus:
      return func (p grammar.Pattern) any {
        return libstrings.Cat2("listOf").(func(any) any)(formatting.Capitalize(RawName(p)))
      }(v.Value)
      case grammar.PatternRegex:
      return func (_ grammar.Regex) any {
        return "regex"
      }(v.Value)
      case grammar.PatternSequence:
      return func (_ []any) any {
        return "sequence"
      }(v.Value)
      case grammar.PatternStar:
      return func (p grammar.Pattern) any {
        return libstrings.Cat2("listOf").(func(any) any)(formatting.Capitalize(RawName(p)))
      }(v.Value)
    }
    return nil
  }(pat).(string)
}

func ReplacePlaceholders[T0, T1 any] (elName T0, typ T1) T1 {
  return typ
}

func Simplify (isRecord bool, pats []any) []any {
  return func () any {
    isConstant := func (p grammar.Pattern) any {
      return func (x any) any {
        switch v := x.(type) {
          case grammar.PatternConstant:
          return func (_ grammar.Constant) any {
            return true
          }(v.Value)
          default:
          return false
        }
        return nil
      }(p)
    }
    return liblogic.IfElse(isRecord).(func(any) any)(liblists.Filter(func (p grammar.Pattern) any {
      return liblogic.Not(isConstant(p))
    }).(func(any) any)(pats)).(func(any) any)(pats)
  }().([]any)
}

func ToName (ns hmodule.Namespace, local string) core.Name {
  return names.UnqualifyName(hmodule.QualifiedName{Namespace: func () any {
    _v := ns
    return &_v
  }(), Local: local})
}

func WrapType (t core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeRecord:
      return func (_ []any) any {
        return t
      }(v.Value)
      case core.TypeUnion:
      return func (_ []any) any {
        return t
      }(v.Value)
      case core.TypeWrap:
      return func (_ core.Type) any {
        return t
      }(v.Value)
      default:
      return core.TypeWrap{Value: t}
    }
    return nil
  }(t).(core.Type)
}
