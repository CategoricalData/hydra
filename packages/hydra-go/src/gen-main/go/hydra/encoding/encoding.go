// Note: this is an automatically generated file. Do not edit.

package encoding

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

func EncodeBinding (cx context.Context, graph graph.Graph, b core.Binding) any {
  return libeithers.Bind(libeithers.Bimap(func (_wc_e error.DecodingError) any {
    return context.InContext[error.DecodingError]{Object: _wc_e, Context: cx}
  }).(func(any) any)(func (_wc_a core.Type) any {
    return _wc_a
  }).(func(any) any)(decodecore.Type_(graph, func (v any) any {
    return v.(core.Binding).Term
  }(b).(core.Term)))).(func(any) any)(func (typ core.Type) any {
    return [2]any{"right", core.Binding{Name: EncodeBindingName(func (v any) any {
      return v.(core.Binding).Name
    }(b).(core.Name)), Term: EncodeTypeNamed(func (v any) any {
      return v.(core.Binding).Name
    }(b).(core.Name), typ), Type_: func () any {
      _v := EncoderTypeSchemeNamed(func (v any) any {
        return v.(core.Binding).Name
      }(b).(core.Name), typ)
      return &_v
    }()}}
  })
}

func EncodeBindingName (n core.Name) core.Name {
  return liblogic.IfElse(liblogic.Not(liblists.Null(liblists.Tail(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
    return v
  }(n)))))).(func(any) any)(core.Name(libstrings.Intercalate(".").(func(any) any)(liblists.Concat2([]any{"hydra", "encode"}).(func(any) any)(liblists.Concat2(liblists.Tail(liblists.Init(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
    return v
  }(n))))).(func(any) any)([]any{formatting.Decapitalize(names.LocalNameOf(n))}))).(string))).(func(any) any)(core.Name(formatting.Decapitalize(names.LocalNameOf(n)))).(core.Name)
}

func EncoderCollectForallVariables (typ core.Type) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return EncoderCollectForallVariables(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return liblists.Cons(func (v any) any {
          return v.(core.ForallType).Parameter
        }(ft)).(func(any) any)(EncoderCollectForallVariables(func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type)))
      }(v.Value)
      default:
      return []any{}
    }
    return nil
  }(typ).([]any)
}

func EncoderCollectOrdVars (typ core.Type) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return EncoderCollectOrdVars(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return liblists.Concat2(EncoderCollectOrdVars(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type))).(func(any) any)(EncoderCollectOrdVars(func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type)))
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return liblists.Concat2(EncoderCollectOrdVars(func (v any) any {
          return v.(core.EitherType).Left
        }(et).(core.Type))).(func(any) any)(EncoderCollectOrdVars(func (v any) any {
          return v.(core.EitherType).Right
        }(et).(core.Type)))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return EncoderCollectOrdVars(func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type))
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return EncoderCollectOrdVars(elemType)
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return liblists.Concat([]any{EncoderCollectTypeVarsFromType(func (v any) any {
          return v.(core.MapType).Keys
        }(mt).(core.Type)), EncoderCollectOrdVars(func (v any) any {
          return v.(core.MapType).Keys
        }(mt).(core.Type)), EncoderCollectOrdVars(func (v any) any {
          return v.(core.MapType).Values
        }(mt).(core.Type))})
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return EncoderCollectOrdVars(elemType)
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return liblists.Concat2(EncoderCollectOrdVars(func (v any) any {
          return v.(core.PairType).First
        }(pt).(core.Type))).(func(any) any)(EncoderCollectOrdVars(func (v any) any {
          return v.(core.PairType).Second
        }(pt).(core.Type)))
      }(v.Value)
      case core.TypeRecord:
      return func (rt []any) any {
        return liblists.Concat(liblists.Map(func (ft core.FieldType) any {
          return EncoderCollectOrdVars(func (v any) any {
            return v.(core.FieldType).Type_
          }(ft).(core.Type))
        }).(func(any) any)(rt))
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return liblists.Concat2(EncoderCollectTypeVarsFromType(elemType)).(func(any) any)(EncoderCollectOrdVars(elemType))
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return liblists.Concat(liblists.Map(func (ft core.FieldType) any {
          return EncoderCollectOrdVars(func (v any) any {
            return v.(core.FieldType).Type_
          }(ft).(core.Type))
        }).(func(any) any)(rt))
      }(v.Value)
      case core.TypeWrap:
      return func (wt core.Type) any {
        return EncoderCollectOrdVars(wt)
      }(v.Value)
      default:
      return []any{}
    }
    return nil
  }(typ).([]any)
}

func EncoderCollectTypeVarsFromType (typ core.Type) []any {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return EncoderCollectTypeVarsFromType(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return liblists.Concat2(EncoderCollectTypeVarsFromType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type))).(func(any) any)(EncoderCollectTypeVarsFromType(func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type)))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return EncoderCollectTypeVarsFromType(func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type))
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return EncoderCollectTypeVarsFromType(elemType)
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return liblists.Concat2(EncoderCollectTypeVarsFromType(func (v any) any {
          return v.(core.MapType).Keys
        }(mt).(core.Type))).(func(any) any)(EncoderCollectTypeVarsFromType(func (v any) any {
          return v.(core.MapType).Values
        }(mt).(core.Type)))
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return EncoderCollectTypeVarsFromType(elemType)
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return liblists.Concat2(EncoderCollectTypeVarsFromType(func (v any) any {
          return v.(core.PairType).First
        }(pt).(core.Type))).(func(any) any)(EncoderCollectTypeVarsFromType(func (v any) any {
          return v.(core.PairType).Second
        }(pt).(core.Type)))
      }(v.Value)
      case core.TypeRecord:
      return func (rt []any) any {
        return liblists.Concat(liblists.Map(func (ft core.FieldType) any {
          return EncoderCollectTypeVarsFromType(func (v any) any {
            return v.(core.FieldType).Type_
          }(ft).(core.Type))
        }).(func(any) any)(rt))
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return EncoderCollectTypeVarsFromType(elemType)
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return liblists.Concat(liblists.Map(func (ft core.FieldType) any {
          return EncoderCollectTypeVarsFromType(func (v any) any {
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
        return EncoderCollectTypeVarsFromType(wt)
      }(v.Value)
      default:
      return []any{}
    }
    return nil
  }(typ).([]any)
}

func EncoderFullResultType (typ core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return EncoderFullResultType(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return core.TypeApplication{Value: core.ApplicationType{Function: EncoderFullResultType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type)), Argument: func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type)}}
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return core.TypeEither{Value: core.EitherType{Left: EncoderFullResultType(func (v any) any {
          return v.(core.EitherType).Left
        }(et).(core.Type)), Right: EncoderFullResultType(func (v any) any {
          return v.(core.EitherType).Right
        }(et).(core.Type))}}
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return core.TypeApplication{Value: core.ApplicationType{Function: EncoderFullResultType(func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type)), Argument: core.TypeVariable{Value: func (v any) any {
          return v.(core.ForallType).Parameter
        }(ft).(core.Name)}}}
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return core.TypeList{Value: EncoderFullResultType(elemType)}
      }(v.Value)
      case core.TypeLiteral:
      return func (_ core.LiteralType) any {
        return core.TypeVariable{Value: core.Name("hydra.core.Literal")}
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return core.TypeMap_{Value: core.MapType{Keys: EncoderFullResultType(func (v any) any {
          return v.(core.MapType).Keys
        }(mt).(core.Type)), Values: EncoderFullResultType(func (v any) any {
          return v.(core.MapType).Values
        }(mt).(core.Type))}}
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return core.TypeMaybe{Value: EncoderFullResultType(elemType)}
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return core.TypePair{Value: core.PairType{First: EncoderFullResultType(func (v any) any {
          return v.(core.PairType).First
        }(pt).(core.Type)), Second: EncoderFullResultType(func (v any) any {
          return v.(core.PairType).Second
        }(pt).(core.Type))}}
      }(v.Value)
      case core.TypeRecord:
      return func (_ []any) any {
        return core.TypeVariable{Value: core.Name("hydra.core.Term")}
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return core.TypeSet{Value: EncoderFullResultType(elemType)}
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

func EncoderFullResultTypeNamed (ename core.Name, typ core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return EncoderFullResultTypeNamed(ename, func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return core.TypeApplication{Value: core.ApplicationType{Function: EncoderFullResultType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type)), Argument: func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type)}}
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return core.TypeEither{Value: core.EitherType{Left: EncoderFullResultType(func (v any) any {
          return v.(core.EitherType).Left
        }(et).(core.Type)), Right: EncoderFullResultType(func (v any) any {
          return v.(core.EitherType).Right
        }(et).(core.Type))}}
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return core.TypeApplication{Value: core.ApplicationType{Function: EncoderFullResultTypeNamed(ename, func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type)), Argument: core.TypeVariable{Value: func (v any) any {
          return v.(core.ForallType).Parameter
        }(ft).(core.Name)}}}
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return core.TypeList{Value: EncoderFullResultType(elemType)}
      }(v.Value)
      case core.TypeLiteral:
      return func (_ core.LiteralType) any {
        return core.TypeVariable{Value: core.Name("hydra.core.Literal")}
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return core.TypeMap_{Value: core.MapType{Keys: EncoderFullResultType(func (v any) any {
          return v.(core.MapType).Keys
        }(mt).(core.Type)), Values: EncoderFullResultType(func (v any) any {
          return v.(core.MapType).Values
        }(mt).(core.Type))}}
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return core.TypeMaybe{Value: EncoderFullResultType(elemType)}
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return core.TypePair{Value: core.PairType{First: EncoderFullResultType(func (v any) any {
          return v.(core.PairType).First
        }(pt).(core.Type)), Second: EncoderFullResultType(func (v any) any {
          return v.(core.PairType).Second
        }(pt).(core.Type))}}
      }(v.Value)
      case core.TypeRecord:
      return func (_ []any) any {
        return core.TypeVariable{Value: ename}
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return core.TypeSet{Value: EncoderFullResultType(elemType)}
      }(v.Value)
      case core.TypeUnion:
      return func (_ []any) any {
        return core.TypeVariable{Value: ename}
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
        return core.TypeVariable{Value: ename}
      }(v.Value)
      default:
      return core.TypeVariable{Value: core.Name("hydra.core.Term")}
    }
    return nil
  }(typ).(core.Type)
}

func EncoderType (typ core.Type) core.Type {
  return func () any {
    var resultType any = EncoderFullResultType(typ)
    return func () any {
      var baseType any = core.TypeFunction{Value: core.FunctionType{Domain: resultType.(core.Type), Codomain: core.TypeVariable{Value: core.Name("hydra.core.Term")}}}
      return PrependForallEncoders(baseType.(core.Type), typ)
    }()
  }().(core.Type)
}

func EncoderTypeNamed (ename core.Name, typ core.Type) core.Type {
  return func () any {
    var resultType any = EncoderFullResultTypeNamed(ename, typ)
    return func () any {
      var baseType any = core.TypeFunction{Value: core.FunctionType{Domain: resultType.(core.Type), Codomain: core.TypeVariable{Value: core.Name("hydra.core.Term")}}}
      return PrependForallEncoders(baseType.(core.Type), typ)
    }()
  }().(core.Type)
}

func EncoderTypeScheme (typ core.Type) core.TypeScheme {
  return func () any {
    var typeVars any = EncoderCollectForallVariables(typ)
    var encoderFunType any = EncoderType(typ)
    var allOrdVars any = EncoderCollectOrdVars(typ)
    var ordVars any = liblists.Filter(func (v core.Name) any {
      return liblists.Elem(v).(func(any) any)(typeVars)
    }).(func(any) any)(allOrdVars)
    var constraints any = liblogic.IfElse(liblists.Null(ordVars)).(func(any) any)(nil).(func(any) any)(func () any {
      _v := libmaps.FromList(liblists.Map(func (v core.Name) any {
        return [2]any{v, core.TypeVariableMetadata{Classes: libsets.Singleton(core.Name("ordering")).([]any)}}
      }).(func(any) any)(ordVars))
      return &_v
    }())
    return core.TypeScheme{Variables: typeVars.([]any), Type_: encoderFunType.(core.Type), Constraints: constraints}
  }().(core.TypeScheme)
}

func EncoderTypeSchemeNamed (ename core.Name, typ core.Type) core.TypeScheme {
  return func () any {
    var typeVars any = EncoderCollectForallVariables(typ)
    var encoderFunType any = EncoderTypeNamed(ename, typ)
    var allOrdVars any = EncoderCollectOrdVars(typ)
    var ordVars any = liblists.Filter(func (v core.Name) any {
      return liblists.Elem(v).(func(any) any)(typeVars)
    }).(func(any) any)(allOrdVars)
    var constraints any = liblogic.IfElse(liblists.Null(ordVars)).(func(any) any)(nil).(func(any) any)(func () any {
      _v := libmaps.FromList(liblists.Map(func (v core.Name) any {
        return [2]any{v, core.TypeVariableMetadata{Classes: libsets.Singleton(core.Name("ordering")).([]any)}}
      }).(func(any) any)(ordVars))
      return &_v
    }())
    return core.TypeScheme{Variables: typeVars.([]any), Type_: encoderFunType.(core.Type), Constraints: constraints}
  }().(core.TypeScheme)
}

func PrependForallEncoders (baseType core.Type, typ core.Type) core.Type {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return PrependForallEncoders(baseType, func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return core.TypeFunction{Value: core.FunctionType{Domain: core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: func (v any) any {
          return v.(core.ForallType).Parameter
        }(ft).(core.Name)}, Codomain: core.TypeVariable{Value: core.Name("hydra.core.Term")}}}, Codomain: PrependForallEncoders(baseType, func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type))}}
      }(v.Value)
      default:
      return baseType
    }
    return nil
  }(typ).(core.Type)
}

func EncodeFieldValue (typeName core.Name, fieldName core.Name, fieldType core.Type) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("y"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("union"), Term: EncodeInjection(typeName, fieldName, core.TermApplication{Value: core.Application{Function: EncodeType(fieldType), Argument: core.TermVariable{Value: core.Name("y")}}})}}}}}}
}

func EncodeFloatValue (floatType core.FloatType, valTerm core.Term) core.Term {
  return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.FloatValue"), Field: core.Field{Name: func (x any) any {
    switch v := x.(type) {
      case core.FloatTypeBigfloat:
      return func (_ struct{}) any {
        return core.Name("bigfloat")
      }(v)
      case core.FloatTypeFloat32_:
      return func (_ struct{}) any {
        return core.Name("float32")
      }(v)
      case core.FloatTypeFloat64_:
      return func (_ struct{}) any {
        return core.Name("float64")
      }(v)
    }
    return nil
  }(floatType).(core.Name), Term: valTerm}}}
}

func EncodeInjection (typeName core.Name, fieldName core.Name, fieldTerm core.Term) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Injection"), Fields: []any{core.Field{Name: core.Name("typeName"), Term: EncodeName(typeName)}, core.Field{Name: core.Name("field"), Term: core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Field"), Fields: []any{core.Field{Name: core.Name("name"), Term: EncodeName(fieldName)}, core.Field{Name: core.Name("term"), Term: fieldTerm}}}}}}}}
}

func EncodeIntegerValue (intType core.IntegerType, valTerm core.Term) core.Term {
  return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerValue"), Field: core.Field{Name: func (x any) any {
    switch v := x.(type) {
      case core.IntegerTypeBigint:
      return func (_ struct{}) any {
        return core.Name("bigint")
      }(v)
      case core.IntegerTypeInt8_:
      return func (_ struct{}) any {
        return core.Name("int8")
      }(v)
      case core.IntegerTypeInt16_:
      return func (_ struct{}) any {
        return core.Name("int16")
      }(v)
      case core.IntegerTypeInt32_:
      return func (_ struct{}) any {
        return core.Name("int32")
      }(v)
      case core.IntegerTypeInt64_:
      return func (_ struct{}) any {
        return core.Name("int64")
      }(v)
      case core.IntegerTypeUint8_:
      return func (_ struct{}) any {
        return core.Name("uint8")
      }(v)
      case core.IntegerTypeUint16_:
      return func (_ struct{}) any {
        return core.Name("uint16")
      }(v)
      case core.IntegerTypeUint32_:
      return func (_ struct{}) any {
        return core.Name("uint32")
      }(v)
      case core.IntegerTypeUint64_:
      return func (_ struct{}) any {
        return core.Name("uint64")
      }(v)
    }
    return nil
  }(intType).(core.Name), Term: valTerm}}}
}

func EncodeListType (elemType core.Type) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("xs"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("list"), Term: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.lists.map")}}, Argument: EncodeType(elemType)}}, Argument: core.TermVariable{Value: core.Name("xs")}}}}}}}}}
}

func EncodeLiteralType (v1 core.LiteralType) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralTypeBinary:
      return func (_ struct{}) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("literal"), Term: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Literal"), Field: core.Field{Name: core.Name("binary"), Term: core.TermVariable{Value: core.Name("x")}}}}}}}}}}
      }(v)
      case core.LiteralTypeBoolean:
      return func (_ struct{}) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("literal"), Term: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Literal"), Field: core.Field{Name: core.Name("boolean"), Term: core.TermVariable{Value: core.Name("x")}}}}}}}}}}
      }(v)
      case core.LiteralTypeString_:
      return func (_ struct{}) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("literal"), Term: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Literal"), Field: core.Field{Name: core.Name("string"), Term: core.TermVariable{Value: core.Name("x")}}}}}}}}}}
      }(v)
      case core.LiteralTypeInteger:
      return func (intType core.IntegerType) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("literal"), Term: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Literal"), Field: core.Field{Name: core.Name("integer"), Term: EncodeIntegerValue(intType, core.TermVariable{Value: core.Name("x")})}}}}}}}}}
      }(v.Value)
      case core.LiteralTypeFloat:
      return func (floatType core.FloatType) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("literal"), Term: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Literal"), Field: core.Field{Name: core.Name("float"), Term: EncodeFloatValue(floatType, core.TermVariable{Value: core.Name("x")})}}}}}}}}}
      }(v.Value)
      default:
      return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermVariable{Value: core.Name("x")}}}}
    }
    return nil
  }(v1).(core.Term)
}

func EncodeEitherType (et core.EitherType) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("e"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("either"), Term: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.eithers.bimap")}}, Argument: EncodeType(func (v any) any {
    return v.(core.EitherType).Left
  }(et).(core.Type))}}, Argument: EncodeType(func (v any) any {
    return v.(core.EitherType).Right
  }(et).(core.Type))}}, Argument: core.TermVariable{Value: core.Name("e")}}}}}}}}}
}

func EncodeForallType (ft core.ForallType) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: EncodeBindingName(func (v any) any {
    return v.(core.ForallType).Parameter
  }(ft).(core.Name)), Domain: nil, Body: EncodeType(func (v any) any {
    return v.(core.ForallType).Body
  }(ft).(core.Type))}}}
}

func EncodeMapType (mt core.MapType) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("m"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("map"), Term: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.maps.bimap")}}, Argument: EncodeType(func (v any) any {
    return v.(core.MapType).Keys
  }(mt).(core.Type))}}, Argument: EncodeType(func (v any) any {
    return v.(core.MapType).Values
  }(mt).(core.Type))}}, Argument: core.TermVariable{Value: core.Name("m")}}}}}}}}}
}

func EncodeOptionalType (elemType core.Type) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("opt"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("maybe"), Term: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.maybes.map")}}, Argument: EncodeType(elemType)}}, Argument: core.TermVariable{Value: core.Name("opt")}}}}}}}}}
}

func EncodePairType (pt core.PairType) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("p"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("pair"), Term: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.pairs.bimap")}}, Argument: EncodeType(func (v any) any {
    return v.(core.PairType).First
  }(pt).(core.Type))}}, Argument: EncodeType(func (v any) any {
    return v.(core.PairType).Second
  }(pt).(core.Type))}}, Argument: core.TermVariable{Value: core.Name("p")}}}}}}}}}
}

func EncodeModule (cx context.Context, graph graph.Graph, mod hmodule.Module) any {
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
      }).(func(any) any)(EncodeBinding(cx, graph, b))
    }).(func(any) any)(typeBindings)).(func(any) any)(func (encodedBindings []any) any {
      return [2]any{"right", func () any {
        _v := hmodule.Module{Namespace: EncodeNamespace(func (v any) any {
          return v.(hmodule.Module).Namespace
        }(mod).(hmodule.Namespace)), Elements: encodedBindings, TermDependencies: liblists.Nub(liblists.Concat2(liblists.Map(EncodeNamespace).(func(any) any)(func (v any) any {
          return v.(hmodule.Module).TypeDependencies
        }(mod))).(func(any) any)(liblists.Map(EncodeNamespace).(func(any) any)(func (v any) any {
          return v.(hmodule.Module).TermDependencies
        }(mod)))).([]any), TypeDependencies: []any{func (v any) any {
          return v.(hmodule.Module).Namespace
        }(mod)}, Description: func () any {
          _v := libstrings.Cat([]any{"Term encoders for ", func (v any) any {
            return v.(hmodule.Module).Namespace
          }(mod)})
          return &_v
        }()}
        return &_v
      }()}
    }))
  })
}

func EncodeName (n core.Name) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.core.Name"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(n).(string)}}}}
}

func EncodeNamespace (ns hmodule.Namespace) hmodule.Namespace {
  return hmodule.Namespace(libstrings.Cat([]any{"hydra.encode.", libstrings.Intercalate(".").(func(any) any)(liblists.Tail(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
    return v
  }(ns))))}).(string))
}

func EncodeRecordType (rt []any) core.Term {
  return EncodeRecordTypeNamed(core.Name("unknown"), rt)
}

func EncodeRecordTypeNamed (ename core.Name, rt []any) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("record"), Term: core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Record"), Fields: []any{core.Field{Name: core.Name("typeName"), Term: EncodeName(ename)}, core.Field{Name: core.Name("fields"), Term: core.TermList{Value: liblists.Map(func (ft core.FieldType) any {
    return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Field"), Fields: []any{core.Field{Name: core.Name("name"), Term: EncodeName(func (v any) any {
      return v.(core.FieldType).Name
    }(ft).(core.Name))}, core.Field{Name: core.Name("term"), Term: core.TermApplication{Value: core.Application{Function: EncodeType(func (v any) any {
      return v.(core.FieldType).Type_
    }(ft).(core.Type)), Argument: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationRecord{Value: core.Projection{TypeName: ename, Field: func (v any) any {
      return v.(core.FieldType).Name
    }(ft).(core.Name)}}}}, Argument: core.TermVariable{Value: core.Name("x")}}}}}}}}}
  }).(func(any) any)(rt).([]any)}}}}}}}}}}}
}

func EncodeSetType (elemType core.Type) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("s"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("set"), Term: core.TermApplication{Value: core.Application{Function: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionPrimitive{Value: core.Name("hydra.lib.sets.map")}}, Argument: EncodeType(elemType)}}, Argument: core.TermVariable{Value: core.Name("s")}}}}}}}}}
}

func EncodeType (v1 core.Type) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return EncodeType(func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return core.TermApplication{Value: core.Application{Function: EncodeType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type)), Argument: EncodeType(func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type))}}
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return EncodeEitherType(et)
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return EncodeForallType(ft)
      }(v.Value)
      case core.TypeFunction:
      return func (_ core.FunctionType) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermVariable{Value: core.Name("x")}}}}
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return EncodeListType(elemType)
      }(v.Value)
      case core.TypeLiteral:
      return func (lt core.LiteralType) any {
        return EncodeLiteralType(lt)
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return EncodeMapType(mt)
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return EncodeOptionalType(elemType)
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return EncodePairType(pt)
      }(v.Value)
      case core.TypeRecord:
      return func (rt []any) any {
        return EncodeRecordType(rt)
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return EncodeSetType(elemType)
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return EncodeUnionType(rt)
      }(v.Value)
      case core.TypeWrap:
      return func (wt core.Type) any {
        return EncodeWrappedType(wt)
      }(v.Value)
      case core.TypeUnit:
      return func (_ struct{}) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("_"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("unit"), Term: core.TermUnit{}}}}}}}
      }(v)
      case core.TypeVariable:
      return func (typeName core.Name) any {
        return core.TermVariable{Value: EncodeBindingName(typeName)}
      }(v.Value)
      default:
      return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermVariable{Value: core.Name("x")}}}}
    }
    return nil
  }(v1).(core.Term)
}

func EncodeTypeNamed (ename core.Name, typ core.Type) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (at core.AnnotatedType) any {
        return EncodeTypeNamed(ename, func (v any) any {
          return v.(core.AnnotatedType).Body
        }(at).(core.Type))
      }(v.Value)
      case core.TypeApplication:
      return func (appType core.ApplicationType) any {
        return core.TermApplication{Value: core.Application{Function: EncodeType(func (v any) any {
          return v.(core.ApplicationType).Function
        }(appType).(core.Type)), Argument: EncodeType(func (v any) any {
          return v.(core.ApplicationType).Argument
        }(appType).(core.Type))}}
      }(v.Value)
      case core.TypeEither:
      return func (et core.EitherType) any {
        return EncodeEitherType(et)
      }(v.Value)
      case core.TypeForall:
      return func (ft core.ForallType) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: EncodeBindingName(func (v any) any {
          return v.(core.ForallType).Parameter
        }(ft).(core.Name)), Domain: nil, Body: EncodeTypeNamed(ename, func (v any) any {
          return v.(core.ForallType).Body
        }(ft).(core.Type))}}}
      }(v.Value)
      case core.TypeFunction:
      return func (_ core.FunctionType) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermVariable{Value: core.Name("x")}}}}
      }(v.Value)
      case core.TypeList:
      return func (elemType core.Type) any {
        return EncodeListType(elemType)
      }(v.Value)
      case core.TypeLiteral:
      return func (lt core.LiteralType) any {
        return EncodeLiteralType(lt)
      }(v.Value)
      case core.TypeMap_:
      return func (mt core.MapType) any {
        return EncodeMapType(mt)
      }(v.Value)
      case core.TypeMaybe:
      return func (elemType core.Type) any {
        return EncodeOptionalType(elemType)
      }(v.Value)
      case core.TypePair:
      return func (pt core.PairType) any {
        return EncodePairType(pt)
      }(v.Value)
      case core.TypeRecord:
      return func (rt []any) any {
        return EncodeRecordTypeNamed(ename, rt)
      }(v.Value)
      case core.TypeSet:
      return func (elemType core.Type) any {
        return EncodeSetType(elemType)
      }(v.Value)
      case core.TypeUnion:
      return func (rt []any) any {
        return EncodeUnionTypeNamed(ename, rt)
      }(v.Value)
      case core.TypeWrap:
      return func (wt core.Type) any {
        return EncodeWrappedTypeNamed(ename, wt)
      }(v.Value)
      case core.TypeUnit:
      return func (_ struct{}) any {
        return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("_"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("unit"), Term: core.TermUnit{}}}}}}}
      }(v)
      case core.TypeVariable:
      return func (typeName core.Name) any {
        return core.TermVariable{Value: EncodeBindingName(typeName)}
      }(v.Value)
      default:
      return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermVariable{Value: core.Name("x")}}}}
    }
    return nil
  }(typ).(core.Term)
}

func EncodeUnionType (rt []any) core.Term {
  return EncodeUnionTypeNamed(core.Name("unknown"), rt)
}

func EncodeUnionTypeNamed (ename core.Name, rt []any) core.Term {
  return core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationUnion{Value: core.CaseStatement{TypeName: ename, Default_: nil, Cases: liblists.Map(func (ft core.FieldType) any {
    return core.Field{Name: func (v any) any {
      return v.(core.FieldType).Name
    }(ft).(core.Name), Term: EncodeFieldValue(ename, func (v any) any {
      return v.(core.FieldType).Name
    }(ft).(core.Name), func (v any) any {
      return v.(core.FieldType).Type_
    }(ft).(core.Type))}
  }).(func(any) any)(rt).([]any)}}}}
}

func EncodeWrappedType (wt core.Type) core.Term {
  return EncodeWrappedTypeNamed(core.Name("unknown"), wt)
}

func EncodeWrappedTypeNamed (ename core.Name, wt core.Type) core.Term {
  return core.TermFunction{Value: core.FunctionLambda{Value: core.Lambda{Parameter: core.Name("x"), Domain: nil, Body: core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("wrap"), Term: core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.WrappedTerm"), Fields: []any{core.Field{Name: core.Name("typeName"), Term: EncodeName(ename)}, core.Field{Name: core.Name("body"), Term: core.TermApplication{Value: core.Application{Function: EncodeType(wt), Argument: core.TermApplication{Value: core.Application{Function: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationWrap{Value: ename}}}, Argument: core.TermVariable{Value: core.Name("x")}}}}}}}}}}}}}}}
}

func FilterTypeBindings (cx context.Context, graph graph.Graph, bindings []any) any {
  return libeithers.Map(libmaybes.Cat).(func(any) any)(libeithers.MapList(func (v1 core.Binding) any {
    return IsEncodableBinding(cx, graph, v1)
  }).(func(any) any)(liblists.Filter(annotations.IsNativeType).(func(any) any)(bindings)))
}

func IsEncodableBinding (cx context.Context, graph graph.Graph, b core.Binding) any {
  return libeithers.Bind(schemas.IsSerializableByName(cx, graph, func (v any) any {
    return v.(core.Binding).Name
  }(b).(core.Name))).(func(any) any)(func (serializable bool) any {
    return [2]any{"right", liblogic.IfElse(serializable).(func(any) any)(func () any {
      _v := b
      return &_v
    }()).(func(any) any)(nil)}
  })
}

func IsUnitType (v1 core.Type) bool {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeUnit:
      return func (_ struct{}) any {
        return true
      }(v)
      default:
      return false
    }
    return nil
  }(v1).(bool)
}
