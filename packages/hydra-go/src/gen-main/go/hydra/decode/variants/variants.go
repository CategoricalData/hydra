// Note: this is an automatically generated file. Do not edit.

package decodevariants

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  extracthelpers "hydra.dev/hydra/extract/helpers"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/variants"
)

func EliminationVariant (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            var fname any = field.(core.Field).Name
            var fterm any = field.(core.Field).Term
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("record"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.EliminationVariantRecord{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("union"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.EliminationVariantUnion{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("wrap"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.EliminationVariantWrap{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}})
            return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"no such field ", fname, " in union"}).(string))}).(func(any) any)(func (f func(core.Term) any) any {
              return f(fterm.(core.Term))
            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(variantMap))
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected union")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func FunctionVariant (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            var fname any = field.(core.Field).Name
            var fterm any = field.(core.Field).Term
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("elimination"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.FunctionVariantElimination{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("lambda"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.FunctionVariantLambda{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("primitive"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.FunctionVariantPrimitive{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}})
            return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"no such field ", fname, " in union"}).(string))}).(func(any) any)(func (f func(core.Term) any) any {
              return f(fterm.(core.Term))
            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(variantMap))
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected union")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func LiteralVariant (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            var fname any = field.(core.Field).Name
            var fterm any = field.(core.Field).Term
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("binary"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.LiteralVariantBinary{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("boolean"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.LiteralVariantBoolean{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("float"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.LiteralVariantFloat{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("integer"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.LiteralVariantInteger{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("string"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.LiteralVariantString_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}})
            return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"no such field ", fname, " in union"}).(string))}).(func(any) any)(func (f func(core.Term) any) any {
              return f(fterm.(core.Term))
            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(variantMap))
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected union")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func TermVariant (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            var fname any = field.(core.Field).Name
            var fterm any = field.(core.Field).Term
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("annotated"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantAnnotated{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("application"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantApplication{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("either"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantEither{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("function"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantFunction{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("let"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantLet{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("list"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantList{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("literal"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantLiteral{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("map"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantMap_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("maybe"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantMaybe{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("pair"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantPair{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("record"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantRecord{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("set"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantSet{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("typeApplication"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantTypeApplication{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("typeLambda"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantTypeLambda{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("union"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantUnion{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("unit"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantUnit{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("variable"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantVariable{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("wrap"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TermVariantWrap{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}})
            return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"no such field ", fname, " in union"}).(string))}).(func(any) any)(func (f func(core.Term) any) any {
              return f(fterm.(core.Term))
            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(variantMap))
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected union")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}

func TypeVariant (cx graph.Graph, raw core.Term) any {
  return libeithers.Either(func (err string) any {
    return [2]any{"left", error.DecodingError(err)}
  }).(func(any) any)(func (stripped core.Term) any {
    return func (x any) any {
      switch v := x.(type) {
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            var fname any = field.(core.Field).Name
            var fterm any = field.(core.Field).Term
            var variantMap any = libmaps.FromList([]any{[2]any{core.Name("annotated"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantAnnotated{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("application"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantApplication{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("either"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantEither{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("forall"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantForall{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("function"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantFunction{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("list"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantList{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("literal"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantLiteral{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("map"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantMap_{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("maybe"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantMaybe{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("pair"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantPair{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("record"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantRecord{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("set"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantSet{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("union"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantUnion{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("unit"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantUnit{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("variable"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantVariable{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}, [2]any{core.Name("wrap"), func (input core.Term) any {
              return libeithers.Map(func (t struct{}) any {
                return variants.TypeVariantWrap{Value: t}
              }).(func(any) any)(extracthelpers.DecodeUnit(cx, input))
            }}})
            return libmaybes.Maybe([2]any{"left", error.DecodingError(libstrings.Cat([]any{"no such field ", fname, " in union"}).(string))}).(func(any) any)(func (f func(core.Term) any) any {
              return f(fterm.(core.Term))
            }).(func(any) any)(libmaps.Lookup(fname).(func(any) any)(variantMap))
          }()
        }(v.Value)
        default:
        return [2]any{"left", error.DecodingError("expected union")}
      }
      return nil
    }(stripped)
  }).(func(any) any)(lexical.StripAndDereferenceTermEither(cx, raw))
}
