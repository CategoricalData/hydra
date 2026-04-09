// Note: this is an automatically generated file. Do not edit.

package encodecore

import (
  "hydra.dev/hydra/core"
  libeithers "hydra.dev/hydra/lib/eithers"
  liblists "hydra.dev/hydra/lib/lists"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  "math/big"
)

func AnnotatedTerm (x core.AnnotatedTerm) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.AnnotatedTerm"), Fields: []any{core.Field{Name: core.Name("body"), Term: Term(func (v any) any {
    return v.(core.AnnotatedTerm).Body
  }(x).(core.Term))}, core.Field{Name: core.Name("annotation"), Term: core.TermMap_{Value: libmaps.Bimap(Name).(func(any) any)(Term).(func(any) any)(func (v any) any {
    return v.(core.AnnotatedTerm).Annotation
  }(x)).([]any)}}}}}
}

func AnnotatedType (x core.AnnotatedType) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.AnnotatedType"), Fields: []any{core.Field{Name: core.Name("body"), Term: Type_(func (v any) any {
    return v.(core.AnnotatedType).Body
  }(x).(core.Type))}, core.Field{Name: core.Name("annotation"), Term: core.TermMap_{Value: libmaps.Bimap(Name).(func(any) any)(Term).(func(any) any)(func (v any) any {
    return v.(core.AnnotatedType).Annotation
  }(x)).([]any)}}}}}
}

func Application (x core.Application) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Application"), Fields: []any{core.Field{Name: core.Name("function"), Term: Term(func (v any) any {
    return v.(core.Application).Function
  }(x).(core.Term))}, core.Field{Name: core.Name("argument"), Term: Term(func (v any) any {
    return v.(core.Application).Argument
  }(x).(core.Term))}}}}
}

func ApplicationType (x core.ApplicationType) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.ApplicationType"), Fields: []any{core.Field{Name: core.Name("function"), Term: Type_(func (v any) any {
    return v.(core.ApplicationType).Function
  }(x).(core.Type))}, core.Field{Name: core.Name("argument"), Term: Type_(func (v any) any {
    return v.(core.ApplicationType).Argument
  }(x).(core.Type))}}}}
}

func Binding (x core.Binding) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Binding"), Fields: []any{core.Field{Name: core.Name("name"), Term: Name(func (v any) any {
    return v.(core.Binding).Name
  }(x).(core.Name))}, core.Field{Name: core.Name("term"), Term: Term(func (v any) any {
    return v.(core.Binding).Term
  }(x).(core.Term))}, core.Field{Name: core.Name("type"), Term: core.TermMaybe{Value: libmaybes.Map(TypeScheme).(func(any) any)(func (v any) any {
    return v.(core.Binding).Type_
  }(x))}}}}}
}

func CaseStatement (x core.CaseStatement) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.CaseStatement"), Fields: []any{core.Field{Name: core.Name("typeName"), Term: Name(func (v any) any {
    return v.(core.CaseStatement).TypeName
  }(x).(core.Name))}, core.Field{Name: core.Name("default"), Term: core.TermMaybe{Value: libmaybes.Map(Term).(func(any) any)(func (v any) any {
    return v.(core.CaseStatement).Default_
  }(x))}}, core.Field{Name: core.Name("cases"), Term: core.TermList{Value: liblists.Map(Field).(func(any) any)(func (v any) any {
    return v.(core.CaseStatement).Cases
  }(x)).([]any)}}}}}
}

func EitherType (x core.EitherType) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.EitherType"), Fields: []any{core.Field{Name: core.Name("left"), Term: Type_(func (v any) any {
    return v.(core.EitherType).Left
  }(x).(core.Type))}, core.Field{Name: core.Name("right"), Term: Type_(func (v any) any {
    return v.(core.EitherType).Right
  }(x).(core.Type))}}}}
}

func PairType (x core.PairType) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.PairType"), Fields: []any{core.Field{Name: core.Name("first"), Term: Type_(func (v any) any {
    return v.(core.PairType).First
  }(x).(core.Type))}, core.Field{Name: core.Name("second"), Term: Type_(func (v any) any {
    return v.(core.PairType).Second
  }(x).(core.Type))}}}}
}

func Elimination (v1 core.Elimination) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.EliminationRecord:
      return func (y core.Projection) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Elimination"), Field: core.Field{Name: core.Name("record"), Term: Projection(y)}}}
      }(v.Value)
      case core.EliminationUnion:
      return func (y core.CaseStatement) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Elimination"), Field: core.Field{Name: core.Name("union"), Term: CaseStatement(y)}}}
      }(v.Value)
      case core.EliminationWrap:
      return func (y core.Name) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Elimination"), Field: core.Field{Name: core.Name("wrap"), Term: Name(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func Field (x core.Field) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Field"), Fields: []any{core.Field{Name: core.Name("name"), Term: Name(func (v any) any {
    return v.(core.Field).Name
  }(x).(core.Name))}, core.Field{Name: core.Name("term"), Term: Term(func (v any) any {
    return v.(core.Field).Term
  }(x).(core.Term))}}}}
}

func FieldType (x core.FieldType) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.FieldType"), Fields: []any{core.Field{Name: core.Name("name"), Term: Name(func (v any) any {
    return v.(core.FieldType).Name
  }(x).(core.Name))}, core.Field{Name: core.Name("type"), Term: Type_(func (v any) any {
    return v.(core.FieldType).Type_
  }(x).(core.Type))}}}}
}

func FloatType (v1 core.FloatType) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatTypeBigfloat:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.FloatType"), Field: core.Field{Name: core.Name("bigfloat"), Term: core.TermUnit{}}}}
      }(v)
      case core.FloatTypeFloat32_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.FloatType"), Field: core.Field{Name: core.Name("float32"), Term: core.TermUnit{}}}}
      }(v)
      case core.FloatTypeFloat64_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.FloatType"), Field: core.Field{Name: core.Name("float64"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func FloatValue (v1 core.FloatValue) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatValueBigfloat:
      return func (y float64) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.FloatValue"), Field: core.Field{Name: core.Name("bigfloat"), Term: core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueBigfloat{Value: y}}}}}}
      }(v.Value)
      case core.FloatValueFloat32_:
      return func (y float32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.FloatValue"), Field: core.Field{Name: core.Name("float32"), Term: core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueFloat32_{Value: y}}}}}}
      }(v.Value)
      case core.FloatValueFloat64_:
      return func (y float64) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.FloatValue"), Field: core.Field{Name: core.Name("float64"), Term: core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueFloat64_{Value: y}}}}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func ForallType (x core.ForallType) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.ForallType"), Fields: []any{core.Field{Name: core.Name("parameter"), Term: Name(func (v any) any {
    return v.(core.ForallType).Parameter
  }(x).(core.Name))}, core.Field{Name: core.Name("body"), Term: Type_(func (v any) any {
    return v.(core.ForallType).Body
  }(x).(core.Type))}}}}
}

func Function (v1 core.Function) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.FunctionElimination:
      return func (y core.Elimination) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Function"), Field: core.Field{Name: core.Name("elimination"), Term: Elimination(y)}}}
      }(v.Value)
      case core.FunctionLambda:
      return func (y core.Lambda) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Function"), Field: core.Field{Name: core.Name("lambda"), Term: Lambda(y)}}}
      }(v.Value)
      case core.FunctionPrimitive:
      return func (y core.Name) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Function"), Field: core.Field{Name: core.Name("primitive"), Term: Name(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func FunctionType (x core.FunctionType) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.FunctionType"), Fields: []any{core.Field{Name: core.Name("domain"), Term: Type_(func (v any) any {
    return v.(core.FunctionType).Domain
  }(x).(core.Type))}, core.Field{Name: core.Name("codomain"), Term: Type_(func (v any) any {
    return v.(core.FunctionType).Codomain
  }(x).(core.Type))}}}}
}

func Injection (x core.Injection) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Injection"), Fields: []any{core.Field{Name: core.Name("typeName"), Term: Name(func (v any) any {
    return v.(core.Injection).TypeName
  }(x).(core.Name))}, core.Field{Name: core.Name("field"), Term: Field(func (v any) any {
    return v.(core.Injection).Field
  }(x).(core.Field))}}}}
}

func IntegerType (v1 core.IntegerType) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerTypeBigint:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerType"), Field: core.Field{Name: core.Name("bigint"), Term: core.TermUnit{}}}}
      }(v)
      case core.IntegerTypeInt8_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerType"), Field: core.Field{Name: core.Name("int8"), Term: core.TermUnit{}}}}
      }(v)
      case core.IntegerTypeInt16_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerType"), Field: core.Field{Name: core.Name("int16"), Term: core.TermUnit{}}}}
      }(v)
      case core.IntegerTypeInt32_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerType"), Field: core.Field{Name: core.Name("int32"), Term: core.TermUnit{}}}}
      }(v)
      case core.IntegerTypeInt64_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerType"), Field: core.Field{Name: core.Name("int64"), Term: core.TermUnit{}}}}
      }(v)
      case core.IntegerTypeUint8_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerType"), Field: core.Field{Name: core.Name("uint8"), Term: core.TermUnit{}}}}
      }(v)
      case core.IntegerTypeUint16_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerType"), Field: core.Field{Name: core.Name("uint16"), Term: core.TermUnit{}}}}
      }(v)
      case core.IntegerTypeUint32_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerType"), Field: core.Field{Name: core.Name("uint32"), Term: core.TermUnit{}}}}
      }(v)
      case core.IntegerTypeUint64_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerType"), Field: core.Field{Name: core.Name("uint64"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func IntegerValue (v1 core.IntegerValue) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueBigint:
      return func (y *big.Int) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerValue"), Field: core.Field{Name: core.Name("bigint"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueBigint{Value: y}}}}}}
      }(v.Value)
      case core.IntegerValueInt8_:
      return func (y int8) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerValue"), Field: core.Field{Name: core.Name("int8"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt8_{Value: y}}}}}}
      }(v.Value)
      case core.IntegerValueInt16_:
      return func (y int16) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerValue"), Field: core.Field{Name: core.Name("int16"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt16_{Value: y}}}}}}
      }(v.Value)
      case core.IntegerValueInt32_:
      return func (y int32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerValue"), Field: core.Field{Name: core.Name("int32"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: y}}}}}}
      }(v.Value)
      case core.IntegerValueInt64_:
      return func (y int64) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerValue"), Field: core.Field{Name: core.Name("int64"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt64_{Value: y}}}}}}
      }(v.Value)
      case core.IntegerValueUint8_:
      return func (y uint8) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerValue"), Field: core.Field{Name: core.Name("uint8"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueUint8_{Value: y}}}}}}
      }(v.Value)
      case core.IntegerValueUint16_:
      return func (y uint16) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerValue"), Field: core.Field{Name: core.Name("uint16"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueUint16_{Value: y}}}}}}
      }(v.Value)
      case core.IntegerValueUint32_:
      return func (y uint32) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerValue"), Field: core.Field{Name: core.Name("uint32"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueUint32_{Value: y}}}}}}
      }(v.Value)
      case core.IntegerValueUint64_:
      return func (y uint64) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.IntegerValue"), Field: core.Field{Name: core.Name("uint64"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueUint64_{Value: y}}}}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func Lambda (x core.Lambda) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Lambda"), Fields: []any{core.Field{Name: core.Name("parameter"), Term: Name(func (v any) any {
    return v.(core.Lambda).Parameter
  }(x).(core.Name))}, core.Field{Name: core.Name("domain"), Term: core.TermMaybe{Value: libmaybes.Map(Type_).(func(any) any)(func (v any) any {
    return v.(core.Lambda).Domain
  }(x))}}, core.Field{Name: core.Name("body"), Term: Term(func (v any) any {
    return v.(core.Lambda).Body
  }(x).(core.Term))}}}}
}

func Let (x core.Let) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Let"), Fields: []any{core.Field{Name: core.Name("bindings"), Term: core.TermList{Value: liblists.Map(Binding).(func(any) any)(func (v any) any {
    return v.(core.Let).Bindings
  }(x)).([]any)}}, core.Field{Name: core.Name("body"), Term: Term(func (v any) any {
    return v.(core.Let).Body
  }(x).(core.Term))}}}}
}

func Literal (v1 core.Literal) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralBinary:
      return func (y []byte) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Literal"), Field: core.Field{Name: core.Name("binary"), Term: core.TermLiteral{Value: core.LiteralBinary{Value: y}}}}}
      }(v.Value)
      case core.LiteralBoolean:
      return func (y bool) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Literal"), Field: core.Field{Name: core.Name("boolean"), Term: core.TermLiteral{Value: core.LiteralBoolean{Value: y}}}}}
      }(v.Value)
      case core.LiteralFloat:
      return func (y core.FloatValue) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Literal"), Field: core.Field{Name: core.Name("float"), Term: FloatValue(y)}}}
      }(v.Value)
      case core.LiteralInteger:
      return func (y core.IntegerValue) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Literal"), Field: core.Field{Name: core.Name("integer"), Term: IntegerValue(y)}}}
      }(v.Value)
      case core.LiteralString_:
      return func (y string) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Literal"), Field: core.Field{Name: core.Name("string"), Term: core.TermLiteral{Value: core.LiteralString_{Value: y}}}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func LiteralType (v1 core.LiteralType) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralTypeBinary:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.LiteralType"), Field: core.Field{Name: core.Name("binary"), Term: core.TermUnit{}}}}
      }(v)
      case core.LiteralTypeBoolean:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.LiteralType"), Field: core.Field{Name: core.Name("boolean"), Term: core.TermUnit{}}}}
      }(v)
      case core.LiteralTypeFloat:
      return func (y core.FloatType) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.LiteralType"), Field: core.Field{Name: core.Name("float"), Term: FloatType(y)}}}
      }(v.Value)
      case core.LiteralTypeInteger:
      return func (y core.IntegerType) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.LiteralType"), Field: core.Field{Name: core.Name("integer"), Term: IntegerType(y)}}}
      }(v.Value)
      case core.LiteralTypeString_:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.LiteralType"), Field: core.Field{Name: core.Name("string"), Term: core.TermUnit{}}}}
      }(v)
    }
    return nil
  }(v1).(core.Term)
}

func MapType (x core.MapType) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.MapType"), Fields: []any{core.Field{Name: core.Name("keys"), Term: Type_(func (v any) any {
    return v.(core.MapType).Keys
  }(x).(core.Type))}, core.Field{Name: core.Name("values"), Term: Type_(func (v any) any {
    return v.(core.MapType).Values
  }(x).(core.Type))}}}}
}

func Name (x core.Name) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.core.Name"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func Projection (x core.Projection) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Projection"), Fields: []any{core.Field{Name: core.Name("typeName"), Term: Name(func (v any) any {
    return v.(core.Projection).TypeName
  }(x).(core.Name))}, core.Field{Name: core.Name("field"), Term: Name(func (v any) any {
    return v.(core.Projection).Field
  }(x).(core.Name))}}}}
}

func Record (x core.Record) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.Record"), Fields: []any{core.Field{Name: core.Name("typeName"), Term: Name(func (v any) any {
    return v.(core.Record).TypeName
  }(x).(core.Name))}, core.Field{Name: core.Name("fields"), Term: core.TermList{Value: liblists.Map(Field).(func(any) any)(func (v any) any {
    return v.(core.Record).Fields
  }(x)).([]any)}}}}}
}

func Term (v1 core.Term) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TermAnnotated:
      return func (y core.AnnotatedTerm) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("annotated"), Term: AnnotatedTerm(y)}}}
      }(v.Value)
      case core.TermApplication:
      return func (y core.Application) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("application"), Term: Application(y)}}}
      }(v.Value)
      case core.TermEither:
      return func (y any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("either"), Term: core.TermEither{Value: libeithers.Bimap(Term).(func(any) any)(Term).(func(any) any)(y)}}}}
      }(v.Value)
      case core.TermFunction:
      return func (y core.Function) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("function"), Term: Function(y)}}}
      }(v.Value)
      case core.TermLet:
      return func (y core.Let) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("let"), Term: Let(y)}}}
      }(v.Value)
      case core.TermList:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("list"), Term: core.TermList{Value: liblists.Map(Term).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case core.TermLiteral:
      return func (y core.Literal) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("literal"), Term: Literal(y)}}}
      }(v.Value)
      case core.TermMap_:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("map"), Term: core.TermMap_{Value: libmaps.Bimap(Term).(func(any) any)(Term).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case core.TermMaybe:
      return func (y any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("maybe"), Term: core.TermMaybe{Value: libmaybes.Map(Term).(func(any) any)(y)}}}}
      }(v.Value)
      case core.TermPair:
      return func (y any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("pair"), Term: core.TermPair{Value: libpairs.Bimap(Term).(func(any) any)(Term).(func(any) any)(y)}}}}
      }(v.Value)
      case core.TermRecord:
      return func (y core.Record) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("record"), Term: Record(y)}}}
      }(v.Value)
      case core.TermSet:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("set"), Term: core.TermSet{Value: libsets.Map(Term).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case core.TermTypeApplication:
      return func (y core.TypeApplicationTerm) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("typeApplication"), Term: TypeApplicationTerm(y)}}}
      }(v.Value)
      case core.TermTypeLambda:
      return func (y core.TypeLambda) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("typeLambda"), Term: TypeLambda(y)}}}
      }(v.Value)
      case core.TermUnion:
      return func (y core.Injection) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("union"), Term: Injection(y)}}}
      }(v.Value)
      case core.TermUnit:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("unit"), Term: core.TermUnit{}}}}
      }(v)
      case core.TermVariable:
      return func (y core.Name) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("variable"), Term: Name(y)}}}
      }(v.Value)
      case core.TermWrap:
      return func (y core.WrappedTerm) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Term"), Field: core.Field{Name: core.Name("wrap"), Term: WrappedTerm(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func Type_ (v1 core.Type) core.Term {
  return func (x any) any {
    switch v := x.(type) {
      case core.TypeAnnotated:
      return func (y core.AnnotatedType) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("annotated"), Term: AnnotatedType(y)}}}
      }(v.Value)
      case core.TypeApplication:
      return func (y core.ApplicationType) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("application"), Term: ApplicationType(y)}}}
      }(v.Value)
      case core.TypeEither:
      return func (y core.EitherType) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("either"), Term: EitherType(y)}}}
      }(v.Value)
      case core.TypeForall:
      return func (y core.ForallType) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("forall"), Term: ForallType(y)}}}
      }(v.Value)
      case core.TypeFunction:
      return func (y core.FunctionType) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("function"), Term: FunctionType(y)}}}
      }(v.Value)
      case core.TypeList:
      return func (y core.Type) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("list"), Term: Type_(y)}}}
      }(v.Value)
      case core.TypeLiteral:
      return func (y core.LiteralType) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("literal"), Term: LiteralType(y)}}}
      }(v.Value)
      case core.TypeMap_:
      return func (y core.MapType) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("map"), Term: MapType(y)}}}
      }(v.Value)
      case core.TypeMaybe:
      return func (y core.Type) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("maybe"), Term: Type_(y)}}}
      }(v.Value)
      case core.TypePair:
      return func (y core.PairType) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("pair"), Term: PairType(y)}}}
      }(v.Value)
      case core.TypeRecord:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("record"), Term: core.TermList{Value: liblists.Map(FieldType).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case core.TypeSet:
      return func (y core.Type) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("set"), Term: Type_(y)}}}
      }(v.Value)
      case core.TypeUnion:
      return func (y []any) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("union"), Term: core.TermList{Value: liblists.Map(FieldType).(func(any) any)(y).([]any)}}}}
      }(v.Value)
      case core.TypeUnit:
      return func (y struct{}) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("unit"), Term: core.TermUnit{}}}}
      }(v)
      case core.TypeVariable:
      return func (y core.Name) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("variable"), Term: Name(y)}}}
      }(v.Value)
      case core.TypeWrap:
      return func (y core.Type) any {
        return core.TermUnion{Value: core.Injection{TypeName: core.Name("hydra.core.Type"), Field: core.Field{Name: core.Name("wrap"), Term: Type_(y)}}}
      }(v.Value)
    }
    return nil
  }(v1).(core.Term)
}

func TypeApplicationTerm (x core.TypeApplicationTerm) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.TypeApplicationTerm"), Fields: []any{core.Field{Name: core.Name("body"), Term: Term(func (v any) any {
    return v.(core.TypeApplicationTerm).Body
  }(x).(core.Term))}, core.Field{Name: core.Name("type"), Term: Type_(func (v any) any {
    return v.(core.TypeApplicationTerm).Type_
  }(x).(core.Type))}}}}
}

func TypeLambda (x core.TypeLambda) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.TypeLambda"), Fields: []any{core.Field{Name: core.Name("parameter"), Term: Name(func (v any) any {
    return v.(core.TypeLambda).Parameter
  }(x).(core.Name))}, core.Field{Name: core.Name("body"), Term: Term(func (v any) any {
    return v.(core.TypeLambda).Body
  }(x).(core.Term))}}}}
}

func TypeScheme (x core.TypeScheme) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.TypeScheme"), Fields: []any{core.Field{Name: core.Name("variables"), Term: core.TermList{Value: liblists.Map(Name).(func(any) any)(func (v any) any {
    return v.(core.TypeScheme).Variables
  }(x)).([]any)}}, core.Field{Name: core.Name("type"), Term: Type_(func (v any) any {
    return v.(core.TypeScheme).Type_
  }(x).(core.Type))}, core.Field{Name: core.Name("constraints"), Term: core.TermMaybe{Value: libmaybes.Map(func (m []any) any {
    return core.TermMap_{Value: libmaps.Bimap(Name).(func(any) any)(TypeVariableMetadata).(func(any) any)(m).([]any)}
  }).(func(any) any)(func (v any) any {
    return v.(core.TypeScheme).Constraints
  }(x))}}}}}
}

func TypeVariableMetadata (x core.TypeVariableMetadata) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.TypeVariableMetadata"), Fields: []any{core.Field{Name: core.Name("classes"), Term: core.TermSet{Value: libsets.Map(Name).(func(any) any)(func (v any) any {
    return v.(core.TypeVariableMetadata).Classes
  }(x)).([]any)}}}}}
}

func WrappedTerm (x core.WrappedTerm) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.core.WrappedTerm"), Fields: []any{core.Field{Name: core.Name("typeName"), Term: Name(func (v any) any {
    return v.(core.WrappedTerm).TypeName
  }(x).(core.Name))}, core.Field{Name: core.Name("body"), Term: Term(func (v any) any {
    return v.(core.WrappedTerm).Body
  }(x).(core.Term))}}}}
}
