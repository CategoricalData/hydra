// Note: this is an automatically generated file. Do not edit.

package testtestTypes

import "hydra.dev/hydra/core"

var TestTypeBuddyListA = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("head"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("tail"), Type_: core.TypeMaybe{Value: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeVariable{Value: TestTypeBuddyListBName}, Argument: core.TypeVariable{Value: core.Name("a")}}}}}}}}}

var TestTypeBuddyListAName = core.Name("BuddyListA")

var TestTypeBuddyListB = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("head"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("tail"), Type_: core.TypeMaybe{Value: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeVariable{Value: TestTypeBuddyListAName}, Argument: core.TypeVariable{Value: core.Name("a")}}}}}}}}}

var TestTypeBuddyListBName = core.Name("BuddyListB")

var TestTypeComparison = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("lessThan"), Type_: core.TypeUnit{}}, core.FieldType{Name: core.Name("equalTo"), Type_: core.TypeUnit{}}, core.FieldType{Name: core.Name("greaterThan"), Type_: core.TypeUnit{}}}}

var TestTypeComparisonName = core.Name("Comparison")

var TestTypeEither = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeForall{Value: core.ForallType{Parameter: core.Name("b"), Body: core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("left"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("right"), Type_: core.TypeVariable{Value: core.Name("b")}}}}}}}}

var TestTypeEitherName = core.Name("Either")

var TestTypeHydraLiteralType = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("boolean"), Type_: core.TypeLiteral{Value: core.LiteralTypeBoolean{}}}, core.FieldType{Name: core.Name("string"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}}

var TestTypeHydraLiteralTypeName = core.Name("HydraLiteralType")

var TestTypeHydraType = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("literal"), Type_: core.TypeVariable{Value: TestTypeHydraLiteralTypeName}}, core.FieldType{Name: core.Name("list"), Type_: core.TypeVariable{Value: TestTypeHydraTypeName}}}}

var TestTypeHydraTypeName = core.Name("HydraType")

var TestTypeIntList = core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("head"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}, core.FieldType{Name: core.Name("tail"), Type_: core.TypeMaybe{Value: core.TypeVariable{Value: TestTypeIntListName}}}}}

var TestTypeIntListName = core.Name("IntList")

var TestTypeLatLon = core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("lat"), Type_: core.TypeLiteral{Value: core.LiteralTypeFloat{Value: core.FloatTypeFloat32_{}}}}, core.FieldType{Name: core.Name("lon"), Type_: core.TypeLiteral{Value: core.LiteralTypeFloat{Value: core.FloatTypeFloat32_{}}}}}}

var TestTypeLatLonName = core.Name("LatLon")

var TestTypeLatLonPoly = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("lat"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("lon"), Type_: core.TypeVariable{Value: core.Name("a")}}}}}}

var TestTypeLatLonPolyName = core.Name("LatLonPoly")

var TestTypeList = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("head"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("tail"), Type_: core.TypeMaybe{Value: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeVariable{Value: TestTypeListName}, Argument: core.TypeVariable{Value: core.Name("a")}}}}}}}}}

var TestTypeListName = core.Name("List")

var TestTypeNumber = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("int"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}, core.FieldType{Name: core.Name("float"), Type_: core.TypeLiteral{Value: core.LiteralTypeFloat{Value: core.FloatTypeFloat32_{}}}}}}

var TestTypeNumberName = core.Name("Number")

var TestTypePerson = core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("firstName"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}, core.FieldType{Name: core.Name("lastName"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}, core.FieldType{Name: core.Name("age"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}}}

var TestTypePersonName = core.Name("Person")

var TestTypePersonOrSomething = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("person"), Type_: core.TypeVariable{Value: TestTypePersonName}}, core.FieldType{Name: core.Name("other"), Type_: core.TypeVariable{Value: core.Name("a")}}}}}}

var TestTypePersonOrSomethingName = core.Name("PersonOrSomething")

var TestTypePolymorphicWrapper = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeWrap{Value: core.TypeList{Value: core.TypeVariable{Value: core.Name("a")}}}}}

var TestTypePolymorphicWrapperName = core.Name("PolymorphicWrapper")

var TestTypeSimpleNumber = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("int"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}, core.FieldType{Name: core.Name("float"), Type_: core.TypeLiteral{Value: core.LiteralTypeFloat{Value: core.FloatTypeFloat32_{}}}}}}

var TestTypeSimpleNumberName = core.Name("SimpleNumber")

var TestTypeStringAlias = core.TypeWrap{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}

var TestTypeStringAliasName = core.Name("StringAlias")

var TestTypeSymmetricTriple = core.TypeForall{Value: core.ForallType{Parameter: core.Name("v"), Body: core.TypeForall{Value: core.ForallType{Parameter: core.Name("e"), Body: core.TypeWrap{Value: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeVariable{Value: TestTypeTripleName}, Argument: core.TypeVariable{Value: core.Name("v")}}}, Argument: core.TypeVariable{Value: core.Name("e")}}}, Argument: core.TypeVariable{Value: core.Name("v")}}}}}}}}

var TestTypeSymmetricTripleName = core.Name("SymmetricTriple")

var TestTypeTimestamp = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("unixTimeMillis"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeUint64_{}}}}, core.FieldType{Name: core.Name("date"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}}

var TestTypeTimestampName = core.Name("Timestamp")

var TestTypeTriple = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeForall{Value: core.ForallType{Parameter: core.Name("b"), Body: core.TypeForall{Value: core.ForallType{Parameter: core.Name("c"), Body: core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("first"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("second"), Type_: core.TypeVariable{Value: core.Name("b")}}, core.FieldType{Name: core.Name("third"), Type_: core.TypeVariable{Value: core.Name("c")}}}}}}}}}}

var TestTypeTripleName = core.Name("Triple")

var TestTypeUnionMonomorphic = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("bool"), Type_: core.TypeLiteral{Value: core.LiteralTypeBoolean{}}}, core.FieldType{Name: core.Name("string"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}, core.FieldType{Name: core.Name("unit"), Type_: core.TypeUnit{}}}}

var TestTypeUnionMonomorphicName = core.Name("UnionMonomorphic")

var TestTypeUnionPolymorphicRecursive = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("bool"), Type_: core.TypeLiteral{Value: core.LiteralTypeBoolean{}}}, core.FieldType{Name: core.Name("value"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("other"), Type_: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeVariable{Value: TestTypeUnionPolymorphicRecursiveName}, Argument: core.TypeVariable{Value: core.Name("a")}}}}}}}}

var TestTypeUnionPolymorphicRecursiveName = core.Name("UnionPolymorphicRecursive")

var TestTypeUnit = core.TypeRecord{Value: []any{}}

var TestTypeUnitName = core.Name("Unit")

var ConcatType = core.TypeFunction{Value: core.FunctionType{Domain: core.TypeLiteral{Value: core.LiteralTypeString_{}}, Codomain: core.TypeFunction{Value: core.FunctionType{Domain: core.TypeLiteral{Value: core.LiteralTypeString_{}}, Codomain: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}}}

var CompareStringsType = core.TypeFunction{Value: core.FunctionType{Domain: core.TypeLiteral{Value: core.LiteralTypeString_{}}, Codomain: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}

var EitherStringOrInt8TypeName = core.Name("EitherStringOrInt8")

var EitherStringOrInt8Type = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("left"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}, core.FieldType{Name: core.Name("right"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt8_{}}}}}}

var ExampleProjectionType = core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: TestTypePersonName}, Codomain: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}

var ListOfInt8sType = core.TypeList{Value: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt8_{}}}}

var ListOfInt16sType = core.TypeList{Value: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt16_{}}}}

var ListOfListsOfStringsType = core.TypeList{Value: core.TypeList{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}

var ListOfSetOfStringsType = core.TypeList{Value: core.TypeSet{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}

var ListOfStringsType = core.TypeList{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}

var MapOfStringsToIntsType = core.TypeMap_{Value: core.MapType{Keys: core.TypeLiteral{Value: core.LiteralTypeString_{}}, Values: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}}

var OptionalInt8Type = core.TypeMaybe{Value: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt8_{}}}}

var OptionalInt16Type = core.TypeMaybe{Value: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt16_{}}}}

var OptionalStringType = core.TypeMaybe{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}

var SetOfStringsType = core.TypeSet{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}

var StringOrIntName = core.Name("StringOrInt")

var StringOrIntType = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("left"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}, core.FieldType{Name: core.Name("right"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}}}

var TestTypeName = core.Name("Test")
