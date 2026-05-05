// Note: this is an automatically generated file. Do not edit.

package decoding

import (
  "hydra.dev/hydra/annotations"
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  decodecore "hydra.dev/hydra/decode/core"
  "hydra.dev/hydra/error"
  "hydra.dev/hydra/formatting"
  "hydra.dev/hydra/graph"
  libeithers "hydra.dev/hydra/lib/eithers"
  liblists "hydra.dev/hydra/lib/lists"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  hmodule "hydra.dev/hydra/module"
  "hydra.dev/hydra/names"
  "hydra.dev/hydra/schemas"
)

func CollectForallVariables (typ core.Type) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return CollectForallVariables(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return liblists.Cons(func (v any) any {
          return v.(core.ForallType).Parameter
        }(ft)).(func(any) any)(CollectForallVariables(func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type)))
      }(v.Value)
      default:
      return []any{}
    }
    return nil
  }(typ).([]any)
}

func CollectOrdConstrainedVariables (typ core.Type) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return CollectOrdConstrainedVariables(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return liblists.Concat2(CollectOrdConstrainedVariables(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type))).(func(any) any)(CollectOrdConstrainedVariables(func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type)))
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return liblists.Concat2(CollectOrdConstrainedVariables(func (v any) any {
          return v.(core.EitherType).Left
        }(et).(core.Type))).(func(any) any)(CollectOrdConstrainedVariables(func (v any) any {
          return v.(core.EitherType).Right
        }(et).(core.Type)))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return CollectOrdConstrainedVariables(func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type))
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return CollectOrdConstrainedVariables(elemType)
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return liblists.Concat([]any{CollectTypeVariablesFromType(func (v any) any {
          return v.(core.MapType).Keys
        }(mt).(core.Type)), CollectOrdConstrainedVariables(func (v any) any {
          return v.(core.MapType).Keys
        }(mt).(core.Type)), CollectOrdConstrainedVariables(func (v any) any {
          return v.(core.MapType).Values
        }(mt).(core.Type))})
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return CollectOrdConstrainedVariables(elemType)
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return liblists.Concat2(CollectOrdConstrainedVariables(func (v any) any {
          return v.(core.PairType).First
        }(pt).(core.Type))).(func(any) any)(CollectOrdConstrainedVariables(func (v any) any {
          return v.(core.PairType).Second
        }(pt).(core.Type)))
      }(v.Value)
      case core.TypeRecord:
      return func (rt []any) any {
        return liblists.Concat(liblists.Map(func (ft core.FieldType) any {
          return CollectOrdConstrainedVariables(func (v any) any {
            return v.(core.FieldType).Type_
          }(ft).(core.Type))
        }).(func(any) any)(rt))
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return liblists.Concat2(CollectTypeVariablesFromType(elemType)).(func(any) any)(CollectOrdConstrainedVariables(elemType))
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return liblists.Concat(liblists.Map(func (ft core.FieldType) any {
          return CollectOrdConstrainedVariables(func (v any) any {
            return v.(core.FieldType).Type_
          }(ft).(core.Type))
        }).(func(any) any)(rt))
      }(v.Value)
      case core.TypeWrap:
      return func (wt core.Type) any {
        return CollectOrdConstrainedVariables(wt)
      }(v.Value)
      default:
      return []any{}
    }
    return nil
  }(typ).([]any)
}

func CollectTypeVariables (typ core.Type) []any {
  return CollectForallVariables(typ)
}

func CollectTypeVariablesFromType (typ core.Type) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return CollectTypeVariablesFromType(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return liblists.Concat2(CollectTypeVariablesFromType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type))).(func(any) any)(CollectTypeVariablesFromType(func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type)))
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return liblists.Concat2(CollectTypeVariablesFromType(func (v any) any {
          return v.(core.EitherType).Left
        }(et).(core.Type))).(func(any) any)(CollectTypeVariablesFromType(func (v any) any {
          return v.(core.EitherType).Right
        }(et).(core.Type)))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return CollectTypeVariablesFromType(func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type))
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return CollectTypeVariablesFromType(elemType)
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return liblists.Concat2(CollectTypeVariablesFromType(func (v any) any {
          return v.(core.MapType).Keys
        }(mt).(core.Type))).(func(any) any)(CollectTypeVariablesFromType(func (v any) any {
          return v.(core.MapType).Values
        }(mt).(core.Type)))
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return CollectTypeVariablesFromType(elemType)
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return liblists.Concat2(CollectTypeVariablesFromType(func (v any) any {
          return v.(core.PairType).First
        }(pt).(core.Type))).(func(any) any)(CollectTypeVariablesFromType(func (v any) any {
          return v.(core.PairType).Second
        }(pt).(core.Type)))
      }(v.Value)
      case core.TypeRecord:
      return func (rt []any) any {
        return liblists.Concat(liblists.Map(func (ft core.FieldType) any {
          return CollectTypeVariablesFromType(func (v any) any {
            return v.(core.FieldType).Type_
          }(ft).(core.Type))
        }).(func(any) any)(rt))
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return CollectTypeVariablesFromType(elemType)
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return liblists.Concat(liblists.Map(func (ft core.FieldType) any {
          return CollectTypeVariablesFromType(func (v any) any {
            return v.(core.FieldType).Type_
          }(ft).(core.Type))
        }).(func(any) any)(rt))
      }(v.Value)
      case core.TypeVariable:
      return func (name core.Name) any {
        return []any{name}
      }(v.Value)
      case core.TypeWrap:
      return func (wt core.Type) any {
        return CollectTypeVariablesFromType(wt)
      }(v.Value)
      default:
      return []any{}
    }
    return nil
  }(typ).([]any)
}

func DecodeBinding (cx context.Context, graph graph.Graph, b core.Binding) any {
  return libeithers.Bind(libeithers.Bimap(func (_wc_e error.DecodingError) any {
    return context.InContext[error.DecodingError]{Object: _wc_e, Context: cx}
  }).(func(any) any)(func (_wc_a core.Type) any {
    return _wc_a
  }).(func(any) any)(decodecore.Type_(graph, func (v any) any {
    return v.(core.Binding).Term
  }(b).(core.Term)))).(func(any) any)(func (typ core.Type) any {
    return [2]any{"right", core.Binding{Name: DecodeBindingName(func (v any) any {
      return v.(core.Binding).Name
    }(b).(core.Name)), Term: DecodeTypeNamed(func (v any) any {
      return v.(core.Binding).Name
    }(b).(core.Name), typ), Type_: func () any {
      _v := DecoderTypeSchemeNamed(func (v any) any {
        return v.(core.Binding).Name
      }(b).(core.Name), typ)
      return &_v
    }()}}
  })
}

func DecodeBindingName (n core.Name) core.Name {
  return liblogic.IfElse(liblogic.Not(liblists.Null(liblists.Tail(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
    return v
  }(n)))))).(func(any) any)(core.Name(libstrings.Intercalate(".").(func(any) any)(liblists.Concat2([]any{"hydra", "decode"}).(func(any) any)(liblists.Concat2(liblists.Tail(liblists.Init(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
    return v
  }(n))))).(func(any) any)([]any{formatting.Decapitalize(names.LocalNameOf(n))}))).(string))).(func(any) any)(core.Name(formatting.Decapitalize(names.LocalNameOf(n)))).(core.Name)
}

func DecodeEitherType (et core.EitherType) core.Term {
  return func () any {
    var leftDecoder any = DecodeType(func (v any) any {
      return v.(core.EitherType).Left
    }(et).(core.Type))
    return func () any {
      var rightDecoder any = DecodeType(func (v any) any {
        return v.(core.EitherType).Right
      }(et).(core.Type))
      return core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.extract.helpers.decodeEither")}, Argument: leftDecoder.(core.Term)}}, Argument: rightDecoder.(core.Term)}}
    }()
  }().(core.Term)
}

func DecodeForallType (ft core.ForallType) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: DecodeBindingName(func (v any) any {
    return v.(core.ForallType).Parameter
  }(ft).(core.Name)), Domain: nil, Body: DecodeType(func (v any) any {
    return v.(core.ForallType).Body
  }(ft).(core.Type))}}}
}

func DecodeListType (elemType core.Type) core.Term {
  return func () any {
    var elemDecoder any = DecodeType(elemType)
    return core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.extract.helpers.decodeList")}, Argument: elemDecoder.(core.Term)}}
  }().(core.Term)
}

func DecodeLiteralType (lt core.LiteralType) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralTypeBinary:
      return func (_ struct{}) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
          _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
          return &_v
        }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
          _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected binary literal"}}}}}}
          return &_v
        }(), Cases: []any{core.Field{Name: core.Name("binary"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("b"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("b")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
      }(v)
      case core.LiteralTypeBoolean:
      return func (_ struct{}) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
          _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
          return &_v
        }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
          _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected boolean literal"}}}}}}
          return &_v
        }(), Cases: []any{core.Field{Name: core.Name("boolean"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("b"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("b")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
      }(v)
      case core.LiteralTypeFloat:
      return func (ft core.FloatType) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.FloatTypeBigfloat:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "bigfloat", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("float"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.FloatValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "bigfloat", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("bigfloat"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("f"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("f")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
            case core.FloatTypeFloat32_:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "float32", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("float"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.FloatValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "float32", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("float32"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("f"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("f")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
            case core.FloatTypeFloat64_:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "float64", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("float"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.FloatValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "float64", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("float64"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("f"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("f")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
          }
          return nil
        }(ft)
      }(v.Value)
      case core.LiteralTypeInteger:
      return func (it core.IntegerType) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.IntegerTypeBigint:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "bigint", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("integer"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.IntegerValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "bigint", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("bigint"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("i"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("i")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
            case core.IntegerTypeInt8_:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "int8", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("integer"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.IntegerValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "int8", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("int8"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("i"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("i")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
            case core.IntegerTypeInt16_:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "int16", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("integer"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.IntegerValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "int16", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("int16"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("i"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("i")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
            case core.IntegerTypeInt32_:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "int32", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("integer"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.IntegerValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "int32", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("int32"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("i"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("i")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
            case core.IntegerTypeInt64_:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "int64", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("integer"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.IntegerValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "int64", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("int64"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("i"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("i")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
            case core.IntegerTypeUint8_:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "uint8", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("integer"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.IntegerValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "uint8", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("uint8"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("i"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("i")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
            case core.IntegerTypeUint16_:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "uint16", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("integer"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.IntegerValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "uint16", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("uint16"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("i"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("i")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
            case core.IntegerTypeUint32_:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "uint32", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("integer"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.IntegerValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "uint32", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("uint32"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("i"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("i")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
            case core.IntegerTypeUint64_:
            return func (_ struct{}) any {
              return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "uint64", " literal"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("integer"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.IntegerValue"), Default_: func () any {
                _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: libstrings.Cat([]any{"expected ", "uint64", " value"}).(string)}}}}}}
                return &_v
              }(), Cases: []any{core.Field{Name: core.Name("uint64"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("i"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("i")}}}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
            }(v)
          }
          return nil
        }(it)
      }(v.Value)
      case core.LiteralTypeString_:
      return func (_ struct{}) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
          _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected literal"}}}}}}
          return &_v
        }(), Cases: []any{core.Field{Name: core.Name("literal"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("v"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Literal"), Default_: func () any {
          _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected string literal"}}}}}}
          return &_v
        }(), Cases: []any{core.Field{Name: core.Name("string"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("s"), Domain: nil, Body: core.TermEither{Value: [2]any{"right", core.TermVariable{Value: core.Name("s")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("v")}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
      }(v)
    }
    return nil
  }(lt).(core.Term)
}

func DecodeMapType (mt core.MapType) core.Term {
  return func () any {
    var keyDecoder any = DecodeType(func (v any) any {
      return v.(core.MapType).Keys
    }(mt).(core.Type))
    return func () any {
      var valDecoder any = DecodeType(func (v any) any {
        return v.(core.MapType).Values
      }(mt).(core.Type))
      return core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.extract.helpers.decodeMap")}, Argument: keyDecoder.(core.Term)}}, Argument: valDecoder.(core.Term)}}
    }()
  }().(core.Term)
}

func DecodeMaybeType (elemType core.Type) core.Term {
  return func () any {
    var elemDecoder any = DecodeType(elemType)
    return core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.extract.helpers.decodeMaybe")}, Argument: elemDecoder.(core.Term)}}
  }().(core.Term)
}

func DecodeModule (cx context.Context, graph graph.Graph, mod hmodule.Module) any {
  return libeithers.Bind(FilterTypeBindings(cx, graph, func (v any) any {
    return v.(hmodule.Module).Elements
  }(mod).([]any))).(func(any) any)(func (typeBindings []any) any {
    return liblogic.IfElse(liblists.Null(typeBindings)).(func(any) any)([2]any{"right", nil}).(func(any) any)(libeithers.Bind(libeithers.MapList(func (b core.Binding) any {
      return libeithers.Bimap(func (ic context.InContext[error.DecodingError]) any {
        return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(func (v any) any {
          return v.(context.InContext[error.Error]).Object
        }(ic).(string))}, Context: func (v any) any {
          return v.(context.InContext[error.Error]).Context
        }(ic).(context.Context)}
      }).(func(any) any)(func (x core.Binding) any {
        return x
      }).(func(any) any)(DecodeBinding(cx, graph, b))
    }).(func(any) any)(typeBindings)).(func(any) any)(func (decodedBindings []any) any {
      return func () any {
        var decodedTypeDeps any = liblists.Map(DecodeNamespace).(func(any) any)(func (v any) any {
          return v.(hmodule.Module).TypeDependencies
        }(mod))
        return func () any {
          var decodedTermDeps any = liblists.Map(DecodeNamespace).(func(any) any)(func (v any) any {
            return v.(hmodule.Module).TermDependencies
          }(mod))
          return func () any {
            var allDecodedDeps any = liblists.Nub(liblists.Concat2(decodedTypeDeps).(func(any) any)(decodedTermDeps))
            return [2]any{"right", func () any {
              _v := hmodule.Module{Namespace: DecodeNamespace(func (v any) any {
                return v.(hmodule.Module).Namespace
              }(mod).(hmodule.Namespace)), Elements: decodedBindings, TermDependencies: liblists.Concat2([]any{hmodule.Namespace("hydra.extract.helpers"), hmodule.Namespace("hydra.lexical"), hmodule.Namespace("hydra.rewriting")}).(func(any) any)(allDecodedDeps).([]any), TypeDependencies: []any{func (v any) any {
                return v.(hmodule.Module).Namespace
              }(mod), hmodule.Namespace("hydra.util")}, Description: func () any {
                _v := libstrings.Cat([]any{"Term decoders for ", func (v any) any {
                  return v.(hmodule.Module).Namespace
                }(mod)})
                return &_v
              }()}
              return &_v
            }()}
          }()
        }()
      }()
    }))
  })
}

func DecodeNamespace (ns hmodule.Namespace) hmodule.Namespace {
  return hmodule.Namespace(libstrings.Cat([]any{"hydra.decode.", libstrings.Intercalate(".").(func(any) any)(liblists.Tail(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
    return v
  }(ns))))}).(string))
}

func DecodePairType (pt core.PairType) core.Term {
  return func () any {
    var firstDecoder any = DecodeType(func (v any) any {
      return v.(core.PairType).First
    }(pt).(core.Type))
    return func () any {
      var secondDecoder any = DecodeType(func (v any) any {
        return v.(core.PairType).Second
      }(pt).(core.Type))
      return core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.extract.helpers.decodePair")}, Argument: firstDecoder.(core.Term)}}, Argument: secondDecoder.(core.Term)}}
    }()
  }().(core.Term)
}

func DecodeRecordType (rt []any) core.Term {
  return DecodeRecordTypeImpl(core.Name("unknown"), rt)
}

func DecodeRecordTypeImpl (tname core.Name, rt []any) core.Term {
  return func () any {
    decodeFieldTerm := func (ft core.FieldType) any {
      return core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.extract.helpers.requireField")}, Argument: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
        return v.(core.FieldType).Name
      }(ft).(string)}}}}, Argument: DecodeType(func (v any) any {
        return v.(core.FieldType).Type_
      }(ft).(core.Type))}}, Argument: core.TermVariable{Value: core.Name("fieldMap")}}}, Argument: core.TermVariable{Value: core.Name("cx")}}}
    }
    return func () any {
      localVarName := func (ft core.FieldType) any {
        return core.Name(libstrings.Cat([]any{"field_", func (v any) any {
          return v.(core.FieldType).Name
        }(ft)}).(string))
      }
      return func () any {
        toFieldLambda := func (ft core.FieldType) any {
          return func (body core.Term) any {
            return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: localVarName(ft).(core.Name), Domain: nil, Body: body}}}
          }
        }
        return func () any {
          var decodeBody any = liblists.Foldl(func (acc core.Term) any {
            return func (ft core.FieldType) any {
              return core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.bind")}}, Argument: decodeFieldTerm(ft).(core.Term)}}, Argument: toFieldLambda(ft).(func(any) any)(acc).(core.Term)}}
            }
          }).(func(any) any)(core.TermEither{Value: [2]any{"right", core.TermRecord{Value: core.Record{TypeName: tname, Fields: liblists.Map(func (ft core.FieldType) any {
            return core.Field{Name: func (v any) any {
              return v.(core.FieldType).Name
            }(ft).(core.Name), Term: core.TermVariable{Value: localVarName(ft).(core.Name)}}
          }).(func(any) any)(rt).([]any)}}}}).(func(any) any)(liblists.Reverse(rt))
          return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
            _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected record"}}}}}}
            return &_v
          }(), Cases: []any{core.Field{Name: core.Name("record"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("record"), Domain: nil, Body: core.TermLet{Value: core.Let{Bindings: []any{core.Binding{Name: core.Name("fieldMap"), Term: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.extract.helpers.toFieldMap")}, Argument: core.TermVariable{Value: core.Name("record")}}}, Type_: nil}}, Body: decodeBody.(core.Term)}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
        }()
      }()
    }()
  }().(core.Term)
}

func DecodeRecordTypeNamed (ename core.Name, rt []any) core.Term {
  return DecodeRecordTypeImpl(ename, rt)
}

func DecodeSetType (elemType core.Type) core.Term {
  return func () any {
    var elemDecoder any = DecodeType(elemType)
    return core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.extract.helpers.decodeSet")}, Argument: elemDecoder.(core.Term)}}
  }().(core.Term)
}

func DecodeType (typ core.Type) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return DecodeType(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return core.TermApplication{Value: core.Application{Function: DecodeType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type)), Argument: DecodeType(func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type))}}
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return DecodeEitherType(et)
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return DecodeForallType(ft)
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return DecodeListType(elemType)
      }(v.Value)
      case core.TypeLiteral:
      return func (lt core.LiteralType) any {
        return DecodeLiteralType(lt)
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return DecodeMapType(mt)
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return DecodeMaybeType(elemType)
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return DecodePairType(pt)
      }(v.Value)
      case core.TypeRecord:
      return func (rt []any) any {
        return DecodeRecordType(rt)
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return DecodeSetType(elemType)
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return DecodeUnionType(rt)
      }(v.Value)
      case core.TypeUnit:
      return func (_ struct{}) any {
        return DecodeUnitType
      }(v)
      case core.TypeWrap:
      return func (wt core.Type) any {
        return DecodeWrappedType(wt)
      }(v.Value)
      case core.TypeVariable:
      return func (typeName core.Name) any {
        return core.TermVariable{Value: DecodeBindingName(typeName)}
      }(v.Value)
      default:
      return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("t"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "unsupported type variant"}}}}}}}}}}}}
    }
    return nil
  }(typ).(core.Term)
}

func DecodeTypeNamed (ename core.Name, typ core.Type) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return DecodeTypeNamed(ename, func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return core.TermApplication{Value: core.Application{Function: DecodeType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type)), Argument: DecodeType(func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type))}}
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return DecodeEitherType(et)
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: DecodeBindingName(func (v any) any {
          return v.(core.ForallType).Parameter
        }(ft).(core.Name)), Domain: nil, Body: DecodeTypeNamed(ename, func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type))}}}
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return DecodeListType(elemType)
      }(v.Value)
      case core.TypeLiteral:
      return func (lt core.LiteralType) any {
        return DecodeLiteralType(lt)
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return DecodeMapType(mt)
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return DecodeMaybeType(elemType)
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return DecodePairType(pt)
      }(v.Value)
      case core.TypeRecord:
      return func (rt []any) any {
        return DecodeRecordTypeNamed(ename, rt)
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return DecodeSetType(elemType)
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return DecodeUnionTypeNamed(ename, rt)
      }(v.Value)
      case core.TypeUnit:
      return func (_ struct{}) any {
        return DecodeUnitType
      }(v)
      case core.TypeWrap:
      return func (wt core.Type) any {
        return DecodeWrappedTypeNamed(ename, wt)
      }(v.Value)
      case core.TypeVariable:
      return func (typeName core.Name) any {
        return core.TermVariable{Value: DecodeBindingName(typeName)}
      }(v.Value)
      default:
      return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("t"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "unsupported type variant"}}}}}}}}}}}}
    }
    return nil
  }(typ).(core.Term)
}

var DecodeUnitType = core.TermVariable{Value: core.Name("hydra.extract.helpers.decodeUnit")}

func DecodeUnionType (rt []any) core.Term {
  return DecodeUnionTypeNamed(core.Name("unknown"), rt)
}

func DecodeUnionTypeNamed (ename core.Name, rt []any) core.Term {
  return func () any {
    toVariantPair := func (ft core.FieldType) any {
      return core.TermPair{Value: [2]any{core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.core.Name"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
        return v.(core.FieldType).Name
      }(ft).(string)}}}}, core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("input"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.map")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("t"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: ename, Field: core.Field{Name: func (v any) any {
        return v.(core.FieldType).Name
      }(ft).(core.Name), Term: core.TermVariable{Value: core.Name("t")}}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: DecodeType(func (v any) any {
        return v.(core.FieldType).Type_
      }(ft).(core.Type)), Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("input")}}}}}}}}}}
    }
    return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
      _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected union"}}}}}}
      return &_v
    }(), Cases: []any{core.Field{Name: core.Name("union"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("inj"), Domain: nil, Body: core.TermLet{Value: core.Let{Bindings: []any{core.Binding{Name: core.Name("field"), Term: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationRecord{Value: core.Projection{TypeName: core.Name("hydra.core.Injection"), Field: core.Name("field")}}}}, Argument: core.TermVariable{Value: core.Name("inj")}}}, Type_: nil}, core.Binding{Name: core.Name("fname"), Term: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationRecord{Value: core.Projection{TypeName: core.Name("hydra.core.Field"), Field: core.Name("name")}}}}, Argument: core.TermVariable{Value: core.Name("field")}}}, Type_: nil}, core.Binding{Name: core.Name("fterm"), Term: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationRecord{Value: core.Projection{TypeName: core.Name("hydra.core.Field"), Field: core.Name("term")}}}}, Argument: core.TermVariable{Value: core.Name("field")}}}, Type_: nil}, core.Binding{Name: core.Name("variantMap"), Term: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.maps.fromList")}}, Argument: core.TermList{Value: liblists.Map(toVariantPair).(func(any) any)(rt).([]any)}}}, Type_: nil}}, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.maybes.maybe")}}, Argument: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.strings.cat")}}, Argument: core.TermList{Value: []any{core.TermLiteral{Value: core.LiteralString_{Value: "no such field "}}, core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationWrap{Value: core.Name("hydra.core.Name")}}}, Argument: core.TermVariable{Value: core.Name("fname")}}}, core.TermLiteral{Value: core.LiteralString_{Value: " in union"}}}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("f"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("f")}, Argument: core.TermVariable{Value: core.Name("fterm")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.maps.lookup")}}, Argument: core.TermVariable{Value: core.Name("fname")}}}, Argument: core.TermVariable{Value: core.Name("variantMap")}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
  }().(core.Term)
}

func DecodeWrappedType (wt core.Type) core.Term {
  return DecodeWrappedTypeNamed(core.Name("unknown"), wt)
}

func DecodeWrappedTypeNamed (ename core.Name, wt core.Type) core.Term {
  return func () any {
    var bodyDecoder any = DecodeType(wt)
    return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("cx"), Domain: nil, Body: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("raw"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.either")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("err"), Domain: nil, Body: core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermVariable{Value: core.Name("err")}}}}}}}}}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("stripped"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: core.Name("hydra.core.Term"), Default_: func () any {
      _v := core.TermEither{Value: [2]any{"left", core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.error.DecodingError"), Body: core.TermLiteral{Value: core.LiteralString_{Value: "expected wrapped type"}}}}}}
      return &_v
    }(), Cases: []any{core.Field{Name: core.Name("wrap"), Term: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("wrappedTerm"), Domain: nil, Body: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.map")}}, Argument: core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("b"), Domain: nil, Body: core.TermWrap{Value: core.WrappedTerm{TypeName: ename, Body: core.TermVariable{Value: core.Name("b")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: bodyDecoder.(core.Term), Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationRecord{Value: core.Projection{TypeName: core.Name("hydra.core.WrappedTerm"), Field: core.Name("body")}}}}, Argument: core.TermVariable{Value: core.Name("wrappedTerm")}}}}}}}}}}}}}}}}, Argument: core.TermVariable{Value: core.Name("stripped")}}}}}}}}, Argument: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermVariable{Value: core.Name("hydra.lexical.stripAndDereferenceTermEither")}, Argument: core.TermVariable{Value: core.Name("cx")}}}, Argument: core.TermVariable{Value: core.Name("raw")}}}}}}}}}}}
  }().(core.Term)
}

func DecoderFullResultType (typ core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return DecoderFullResultType(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return core.TypeApplication{Value: core.ApplicationType{Function: DecoderFullResultType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type)), Argument: func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type)}}
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return core.TypeEither{Value: core.EitherType{Left: DecoderFullResultType(func (v any) any {
          return v.(core.EitherType).Left
        }(et).(core.Type)), Right: DecoderFullResultType(func (v any) any {
          return v.(core.EitherType).Right
        }(et).(core.Type))}}
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return core.TypeApplication{Value: core.ApplicationType{Function: DecoderFullResultType(func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type)), Argument: core.TypeVariable{Value: func (v any) any {
          return v.(core.ForallType).Parameter
        }(ft).(core.Name)}}}
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return core.TypeList{Value: DecoderFullResultType(elemType)}
      }(v.Value)
      case core.TypeLiteral:
      return func (_ core.LiteralType) any {
        return core.TypeVariable{Value: core.Name("hydra.core.Literal")}
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return core.TypeMap_{Value: core.MapType{Keys: DecoderFullResultType(func (v any) any {
          return v.(core.MapType).Keys
        }(mt).(core.Type)), Values: DecoderFullResultType(func (v any) any {
          return v.(core.MapType).Values
        }(mt).(core.Type))}}
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return core.TypeMaybe{Value: DecoderFullResultType(elemType)}
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return core.TypePair{Value: core.PairType{First: DecoderFullResultType(func (v any) any {
          return v.(core.PairType).First
        }(pt).(core.Type)), Second: DecoderFullResultType(func (v any) any {
          return v.(core.PairType).Second
        }(pt).(core.Type))}}
      }(v.Value)
      case core.TypeRecord:
      return func (_ []any) any {
        return core.TypeVariable{Value: core.Name("hydra.core.Term")}
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return core.TypeSet{Value: DecoderFullResultType(elemType)}
      }(v.Value)
      case core.TypeUnion:
      return func (_ []any) any {
        return core.TypeVariable{Value: core.Name("hydra.core.Term")}
      }(v.Value)
      case core.TypeUnit:
      return func (_ struct{}) any {
        return core.TypeUnit{}
      }(v)
      case core.TypeVariable:
      return func (name core.Name) any {
        return core.TypeVariable{Value: name}
      }(v.Value)
      case core.TypeWrap:
      return func (_ core.Type) any {
        return core.TypeVariable{Value: core.Name("hydra.core.Term")}
      }(v.Value)
      default:
      return core.TypeVariable{Value: core.Name("hydra.core.Term")}
    }
    return nil
  }(typ).(core.Type)
}

func DecoderFullResultTypeNamed (ename core.Name, typ core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return DecoderFullResultTypeNamed(ename, func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return core.TypeApplication{Value: core.ApplicationType{Function: DecoderFullResultTypeNamed(ename, func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type)), Argument: core.TypeVariable{Value: func (v any) any {
          return v.(core.ForallType).Parameter
        }(ft).(core.Name)}}}
      }(v.Value)
      case core.TypeRecord:
      return func (_ []any) any {
        return core.TypeVariable{Value: ename}
      }(v.Value)
      case core.TypeUnion:
      return func (_ []any) any {
        return core.TypeVariable{Value: ename}
      }(v.Value)
      case core.TypeWrap:
      return func (_ core.Type) any {
        return core.TypeVariable{Value: ename}
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return core.TypeApplication{Value: core.ApplicationType{Function: DecoderFullResultType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type)), Argument: func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type)}}
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return core.TypeEither{Value: core.EitherType{Left: DecoderFullResultType(func (v any) any {
          return v.(core.EitherType).Left
        }(et).(core.Type)), Right: DecoderFullResultType(func (v any) any {
          return v.(core.EitherType).Right
        }(et).(core.Type))}}
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return core.TypeList{Value: DecoderFullResultType(elemType)}
      }(v.Value)
      case core.TypeLiteral:
      return func (_ core.LiteralType) any {
        return core.TypeVariable{Value: core.Name("hydra.core.Literal")}
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return core.TypeMap_{Value: core.MapType{Keys: DecoderFullResultType(func (v any) any {
          return v.(core.MapType).Keys
        }(mt).(core.Type)), Values: DecoderFullResultType(func (v any) any {
          return v.(core.MapType).Values
        }(mt).(core.Type))}}
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return core.TypeMaybe{Value: DecoderFullResultType(elemType)}
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return core.TypePair{Value: core.PairType{First: DecoderFullResultType(func (v any) any {
          return v.(core.PairType).First
        }(pt).(core.Type)), Second: DecoderFullResultType(func (v any) any {
          return v.(core.PairType).Second
        }(pt).(core.Type))}}
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return core.TypeSet{Value: DecoderFullResultType(elemType)}
      }(v.Value)
      case core.TypeUnit:
      return func (_ struct{}) any {
        return core.TypeUnit{}
      }(v)
      case core.TypeVariable:
      return func (name core.Name) any {
        return core.TypeVariable{Value: name}
      }(v.Value)
      default:
      return core.TypeVariable{Value: core.Name("hydra.core.Term")}
    }
    return nil
  }(typ).(core.Type)
}

func DecoderResultType (typ core.Type) core.Name {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return DecoderResultType(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return DecoderResultType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return DecoderResultType(func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type))
      }(v.Value)
      case core.TypeLiteral:
      return func (_ core.LiteralType) any {
        return core.Name("hydra.core.Literal")
      }(v.Value)
      case core.TypeRecord:
      return func (_ []any) any {
        return core.Name("hydra.core.Term")
      }(v.Value)
      case core.TypeUnion:
      return func (_ []any) any {
        return core.Name("hydra.core.Term")
      }(v.Value)
      case core.TypeWrap:
      return func (_ core.Type) any {
        return core.Name("hydra.core.Term")
      }(v.Value)
      default:
      return core.Name("hydra.core.Term")
    }
    return nil
  }(typ).(core.Name)
}

func DecoderType (typ core.Type) core.Type {
  return func () any {
    var resultType any = DecoderFullResultType(typ)
    return func () any {
      var baseType any = core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: core.Name("hydra.graph.Graph")}, Codomain: core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: core.Name("hydra.core.Term")}, Codomain: core.TypeEither{Value: core.EitherType{Left: core.TypeVariable{Value: core.Name("hydra.error.DecodingError")}, Right: resultType.(core.Type)}}}}}}
      return PrependForallDecoders(baseType.(core.Type), typ)
    }()
  }().(core.Type)
}

func DecoderTypeNamed (ename core.Name, typ core.Type) core.Type {
  return func () any {
    var resultType any = DecoderFullResultTypeNamed(ename, typ)
    return func () any {
      var baseType any = core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: core.Name("hydra.graph.Graph")}, Codomain: core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: core.Name("hydra.core.Term")}, Codomain: core.TypeEither{Value: core.EitherType{Left: core.TypeVariable{Value: core.Name("hydra.error.DecodingError")}, Right: resultType.(core.Type)}}}}}}
      return PrependForallDecoders(baseType.(core.Type), typ)
    }()
  }().(core.Type)
}

func DecoderTypeScheme (typ core.Type) core.TypeScheme {
  return func () any {
    var typeVars any = CollectTypeVariables(typ)
    return func () any {
      var allOrdVars any = CollectOrdConstrainedVariables(typ)
      return func () any {
        var ordVars any = liblists.Filter(func (v core.Name) any {
          return liblists.Elem(v).(func(any) any)(typeVars)
        }).(func(any) any)(allOrdVars)
        return func () any {
          var constraints any = liblogic.IfElse(liblists.Null(ordVars)).(func(any) any)(nil).(func(any) any)(func () any {
            _v := libmaps.FromList(liblists.Map(func (v core.Name) any {
              return [2]any{v, core.TypeVariableMetadata{Classes: libsets.Singleton(core.Name("ordering")).([]any)}}
            }).(func(any) any)(ordVars))
            return &_v
          }())
          return core.TypeScheme{Variables: typeVars.([]any), Type_: DecoderType(typ), Constraints: constraints}
        }()
      }()
    }()
  }().(core.TypeScheme)
}

func DecoderTypeSchemeNamed (ename core.Name, typ core.Type) core.TypeScheme {
  return func () any {
    var typeVars any = CollectTypeVariables(typ)
    return func () any {
      var allOrdVars any = CollectOrdConstrainedVariables(typ)
      return func () any {
        var ordVars any = liblists.Filter(func (v core.Name) any {
          return liblists.Elem(v).(func(any) any)(typeVars)
        }).(func(any) any)(allOrdVars)
        return func () any {
          var constraints any = liblogic.IfElse(liblists.Null(ordVars)).(func(any) any)(nil).(func(any) any)(func () any {
            _v := libmaps.FromList(liblists.Map(func (v core.Name) any {
              return [2]any{v, core.TypeVariableMetadata{Classes: libsets.Singleton(core.Name("ordering")).([]any)}}
            }).(func(any) any)(ordVars))
            return &_v
          }())
          return core.TypeScheme{Variables: typeVars.([]any), Type_: DecoderTypeNamed(ename, typ), Constraints: constraints}
        }()
      }()
    }()
  }().(core.TypeScheme)
}

func FilterTypeBindings (cx context.Context, graph graph.Graph, bindings []any) any {
  return libeithers.Map(libmaybes.Cat).(func(any) any)(libeithers.MapList(func (v1 core.Binding) any {
    return IsDecodableBinding(cx, graph, v1)
  }).(func(any) any)(liblists.Filter(annotations.IsNativeType).(func(any) any)(bindings)))
}

func IsDecodableBinding (cx context.Context, graph graph.Graph, b core.Binding) any {
  return libeithers.Bind(schemas.IsSerializableByName(cx, graph, func (v any) any {
    return v.(core.Binding).Name
  }(b).(core.Name))).(func(any) any)(func (serializable bool) any {
    return [2]any{"right", liblogic.IfElse(serializable).(func(any) any)(func () any {
      _v := b
      return &_v
    }()).(func(any) any)(nil)}
  })
}

func PrependForallDecoders (baseType core.Type, typ core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return PrependForallDecoders(baseType, func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return core.TypeFunction{Value: core.FunctionType{Domain: core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: core.Name("hydra.graph.Graph")}, Codomain: core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: core.Name("hydra.core.Term")}, Codomain: core.TypeEither{Value: core.EitherType{Left: core.TypeVariable{Value: core.Name("hydra.error.DecodingError")}, Right: core.TypeVariable{Value: func (v any) any {
          return v.(core.ForallType).Parameter
        }(ft).(core.Name)}}}}}}}, Codomain: PrependForallDecoders(baseType, func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type))}}
      }(v.Value)
      default:
      return baseType
    }
    return nil
  }(typ).(core.Type)
}
