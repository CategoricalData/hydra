// Note: this is an automatically generated file. Do not edit.

package testTypes

import "hydra/core"

var testTypeBuddyListA = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("head"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("tail"), Type_: core.TypeMaybe{Value: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeVariable{Value: testTypeBuddyListBName}, Argument: core.TypeVariable{Value: core.Name("a")}}}}}}}}}

var testTypeBuddyListAName = core.Name("BuddyListA")

var testTypeBuddyListB = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("head"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("tail"), Type_: core.TypeMaybe{Value: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeVariable{Value: testTypeBuddyListAName}, Argument: core.TypeVariable{Value: core.Name("a")}}}}}}}}}

var testTypeBuddyListBName = core.Name("BuddyListB")

var testTypeComparison = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("lessThan"), Type_: core.TypeUnit{}}, core.FieldType{Name: core.Name("equalTo"), Type_: core.TypeUnit{}}, core.FieldType{Name: core.Name("greaterThan"), Type_: core.TypeUnit{}}}}

var testTypeComparisonName = core.Name("Comparison")

var testTypeEither = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeForall{Value: core.ForallType{Parameter: core.Name("b"), Body: core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("left"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("right"), Type_: core.TypeVariable{Value: core.Name("b")}}}}}}}}

var testTypeEitherName = core.Name("Either")

var testTypeHydraLiteralType = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("boolean"), Type_: core.TypeLiteral{Value: core.LiteralTypeBoolean{}}}, core.FieldType{Name: core.Name("string"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}}

var testTypeHydraLiteralTypeName = core.Name("HydraLiteralType")

var testTypeHydraType = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("literal"), Type_: core.TypeVariable{Value: testTypeHydraLiteralTypeName}}, core.FieldType{Name: core.Name("list"), Type_: core.TypeVariable{Value: testTypeHydraTypeName}}}}

var testTypeHydraTypeName = core.Name("HydraType")

var testTypeIntList = core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("head"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}, core.FieldType{Name: core.Name("tail"), Type_: core.TypeMaybe{Value: core.TypeVariable{Value: testTypeIntListName}}}}}

var testTypeIntListName = core.Name("IntList")

var testTypeLatLon = core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("lat"), Type_: core.TypeLiteral{Value: core.LiteralTypeFloat{Value: core.FloatTypeFloat32_{}}}}, core.FieldType{Name: core.Name("lon"), Type_: core.TypeLiteral{Value: core.LiteralTypeFloat{Value: core.FloatTypeFloat32_{}}}}}}

var testTypeLatLonName = core.Name("LatLon")

var testTypeLatLonPoly = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("lat"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("lon"), Type_: core.TypeVariable{Value: core.Name("a")}}}}}}

var testTypeLatLonPolyName = core.Name("LatLonPoly")

var testTypeList = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("head"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("tail"), Type_: core.TypeMaybe{Value: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeVariable{Value: testTypeListName}, Argument: core.TypeVariable{Value: core.Name("a")}}}}}}}}}

var testTypeListName = core.Name("List")

var testTypeNumber = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("int"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}, core.FieldType{Name: core.Name("float"), Type_: core.TypeLiteral{Value: core.LiteralTypeFloat{Value: core.FloatTypeFloat32_{}}}}}}

var testTypeNumberName = core.Name("Number")

var testTypePerson = core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("firstName"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}, core.FieldType{Name: core.Name("lastName"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}, core.FieldType{Name: core.Name("age"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}}}

var testTypePersonName = core.Name("Person")

var testTypePersonOrSomething = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("person"), Type_: core.TypeVariable{Value: testTypePersonName}}, core.FieldType{Name: core.Name("other"), Type_: core.TypeVariable{Value: core.Name("a")}}}}}}

var testTypePersonOrSomethingName = core.Name("PersonOrSomething")

var testTypePolymorphicWrapper = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeWrap{Value: core.TypeList{Value: core.TypeVariable{Value: core.Name("a")}}}}}

var testTypePolymorphicWrapperName = core.Name("PolymorphicWrapper")

var testTypeSimpleNumber = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("int"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}, core.FieldType{Name: core.Name("float"), Type_: core.TypeLiteral{Value: core.LiteralTypeFloat{Value: core.FloatTypeFloat32_{}}}}}}

var testTypeSimpleNumberName = core.Name("SimpleNumber")

var testTypeStringAlias = core.TypeWrap{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}

var testTypeStringAliasName = core.Name("StringAlias")

var testTypeSymmetricTriple = core.TypeForall{Value: core.ForallType{Parameter: core.Name("v"), Body: core.TypeForall{Value: core.ForallType{Parameter: core.Name("e"), Body: core.TypeWrap{Value: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeVariable{Value: testTypeTripleName}, Argument: core.TypeVariable{Value: core.Name("v")}}}, Argument: core.TypeVariable{Value: core.Name("e")}}}, Argument: core.TypeVariable{Value: core.Name("v")}}}}}}}}

var testTypeSymmetricTripleName = core.Name("SymmetricTriple")

var testTypeTimestamp = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("unixTimeMillis"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeUint64_{}}}}, core.FieldType{Name: core.Name("date"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}}

var testTypeTimestampName = core.Name("Timestamp")

var testTypeTriple = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeForall{Value: core.ForallType{Parameter: core.Name("b"), Body: core.TypeForall{Value: core.ForallType{Parameter: core.Name("c"), Body: core.TypeRecord{Value: []any{core.FieldType{Name: core.Name("first"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("second"), Type_: core.TypeVariable{Value: core.Name("b")}}, core.FieldType{Name: core.Name("third"), Type_: core.TypeVariable{Value: core.Name("c")}}}}}}}}}}

var testTypeTripleName = core.Name("Triple")

var testTypeUnionMonomorphic = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("bool"), Type_: core.TypeLiteral{Value: core.LiteralTypeBoolean{}}}, core.FieldType{Name: core.Name("string"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}, core.FieldType{Name: core.Name("unit"), Type_: core.TypeUnit{}}}}

var testTypeUnionMonomorphicName = core.Name("UnionMonomorphic")

var testTypeUnionPolymorphicRecursive = core.TypeForall{Value: core.ForallType{Parameter: core.Name("a"), Body: core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("bool"), Type_: core.TypeLiteral{Value: core.LiteralTypeBoolean{}}}, core.FieldType{Name: core.Name("value"), Type_: core.TypeVariable{Value: core.Name("a")}}, core.FieldType{Name: core.Name("other"), Type_: core.TypeApplication{Value: core.ApplicationType{Function: core.TypeVariable{Value: testTypeUnionPolymorphicRecursiveName}, Argument: core.TypeVariable{Value: core.Name("a")}}}}}}}}

var testTypeUnionPolymorphicRecursiveName = core.Name("UnionPolymorphicRecursive")

var testTypeUnit = core.TypeRecord{Value: []any{}}

var testTypeUnitName = core.Name("Unit")

var concatType = core.TypeFunction{Value: core.FunctionType{Domain: core.TypeLiteral{Value: core.LiteralTypeString_{}}, Codomain: core.TypeFunction{Value: core.FunctionType{Domain: core.TypeLiteral{Value: core.LiteralTypeString_{}}, Codomain: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}}}

var compareStringsType = core.TypeFunction{Value: core.FunctionType{Domain: core.TypeLiteral{Value: core.LiteralTypeString_{}}, Codomain: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}

var eitherStringOrInt8TypeName = core.Name("EitherStringOrInt8")

var eitherStringOrInt8Type = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("left"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}, core.FieldType{Name: core.Name("right"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt8_{}}}}}}

var exampleProjectionType = core.TypeFunction{Value: core.FunctionType{Domain: core.TypeVariable{Value: testTypePersonName}, Codomain: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}

var listOfInt8sType = core.TypeList{Value: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt8_{}}}}

var listOfInt16sType = core.TypeList{Value: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt16_{}}}}

var listOfListsOfStringsType = core.TypeList{Value: core.TypeList{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}

var listOfSetOfStringsType = core.TypeList{Value: core.TypeSet{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}}

var listOfStringsType = core.TypeList{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}

var mapOfStringsToIntsType = core.TypeMap_{Value: core.MapType{Keys: core.TypeLiteral{Value: core.LiteralTypeString_{}}, Values: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}}

var optionalInt8Type = core.TypeMaybe{Value: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt8_{}}}}

var optionalInt16Type = core.TypeMaybe{Value: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt16_{}}}}

var optionalStringType = core.TypeMaybe{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}

var setOfStringsType = core.TypeSet{Value: core.TypeLiteral{Value: core.LiteralTypeString_{}}}

var stringOrIntName = core.Name("StringOrInt")

var stringOrIntType = core.TypeUnion{Value: []any{core.FieldType{Name: core.Name("left"), Type_: core.TypeLiteral{Value: core.LiteralTypeString_{}}}, core.FieldType{Name: core.Name("right"), Type_: core.TypeLiteral{Value: core.LiteralTypeInteger{Value: core.IntegerTypeInt32_{}}}}}}

var testTypeName = core.Name("Test")
