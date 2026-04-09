// Note: this is an automatically generated file. Do not edit.

package testtestTerms

import (
  "hydra.dev/hydra/core"
  testtestTypes "hydra.dev/hydra/test/testTypes"
)

func LatlonRecord (lat float32, lon float32) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: testtestTypes.TestTypeLatLonName, Fields: []any{core.Field{Name: core.Name("lat"), Term: core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueFloat32_{Value: lat}}}}, core.Field{Name: core.Name("lon"), Term: core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueFloat32_{Value: lon}}}}}}}
}

var TestDataArthur = core.TermRecord{Value: core.Record{TypeName: testtestTypes.TestTypePersonName, Fields: []any{core.Field{Name: core.Name("firstName"), Term: core.TermLiteral{Value: core.LiteralString_{Value: "Arthur"}}}, core.Field{Name: core.Name("lastName"), Term: core.TermLiteral{Value: core.LiteralString_{Value: "Dent"}}}, core.Field{Name: core.Name("age"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: 42}}}}}}}

var TestElementArthur = core.Binding{Name: core.Name("firstName"), Term: TestDataArthur, Type_: func () any {
  _v := core.TypeScheme{Variables: []any{}, Type_: core.TypeVariable{Value: testtestTypes.TestTypePersonName}, Constraints: nil}
  return &_v
}()}

var TestElementFirstName = core.Binding{Name: core.Name("firstName"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationRecord{Value: core.Projection{TypeName: testtestTypes.TestTypePersonName, Field: core.Name("firstName")}}}}, Type_: func () any {
  _v := core.TypeScheme{Variables: []any{}, Type_: core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: testtestTypes.TestTypePersonName}, Codomain: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}, Constraints: nil}
  return &_v
}()}
