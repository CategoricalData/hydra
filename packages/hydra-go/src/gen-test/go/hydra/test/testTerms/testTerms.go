// Note: this is an automatically generated file. Do not edit.

package testTerms

import (
  "hydra/core",
  "hydra/test/testTypes")

var latlonRecord = func (lat any) any {
  return func (lon any) any {
    return core.TermRecord{Value: core.Record{TypeName: testTypes.TestTypeLatLonName, Fields: []any{core.Field{Name: core.Name("lat"), Term: core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueFloat32_{Value: lat}}}}, core.Field{Name: core.Name("lon"), Term: core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueFloat32_{Value: lon}}}}}}}
  }
}

var testDataArthur = core.TermRecord{Value: core.Record{TypeName: testTypes.TestTypePersonName, Fields: []any{core.Field{Name: core.Name("firstName"), Term: core.TermLiteral{Value: core.LiteralString_{Value: "Arthur"}}}, core.Field{Name: core.Name("lastName"), Term: core.TermLiteral{Value: core.LiteralString_{Value: "Dent"}}}, core.Field{Name: core.Name("age"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: 42}}}}}}}

var testElementArthur = core.Binding{Name: core.Name("firstName"), Term: testDataArthur, Type_: &core.TypeScheme{Variables: []any{}, Type_: core.TypeVariable{Value: testTypes.TestTypePersonName}, Constraints: nil}}

var testElementFirstName = core.Binding{Name: core.Name("firstName"), Term: core.TermFunction{Value: core.FunctionElimination{Value: core.EliminationRecord{Value: core.Projection{TypeName: testTypes.TestTypePersonName, Field: core.Name("firstName")}}}}, Type_: &core.TypeScheme{Variables: []any{}, Type_: core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: testTypes.TestTypePersonName}, Codomain: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}, Constraints: nil}}
