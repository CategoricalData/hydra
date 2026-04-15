package hydra.test.testTypes

import hydra.core.*

lazy val compareStringsType: hydra.core.Type = hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.string),
   hydra.core.Type.literal(hydra.core.LiteralType.string)))

lazy val concatType: hydra.core.Type = hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.string),
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.string),
   hydra.core.Type.literal(hydra.core.LiteralType.string)))))

lazy val eitherStringOrInt8Type: hydra.core.Type = hydra.core.Type.union(Seq(hydra.core.FieldType("left",
   hydra.core.Type.literal(hydra.core.LiteralType.string)), hydra.core.FieldType("right",
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int8)))))

lazy val eitherStringOrInt8TypeName: hydra.core.Name = "EitherStringOrInt8"

lazy val exampleProjectionType: hydra.core.Type = hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable(hydra.test.testTypes.testTypePersonName),
   hydra.core.Type.literal(hydra.core.LiteralType.string)))

lazy val listOfInt16sType: hydra.core.Type = hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int16)))

lazy val listOfInt8sType: hydra.core.Type = hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int8)))

lazy val listOfListsOfStringsType: hydra.core.Type = hydra.core.Type.list(hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.string)))

lazy val listOfSetOfStringsType: hydra.core.Type = hydra.core.Type.list(hydra.core.Type.set(hydra.core.Type.literal(hydra.core.LiteralType.string)))

lazy val listOfStringsType: hydra.core.Type = hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.string))

lazy val mapOfStringsToIntsType: hydra.core.Type = hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.literal(hydra.core.LiteralType.string),
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))

lazy val optionalInt16Type: hydra.core.Type = hydra.core.Type.maybe(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int16)))

lazy val optionalInt8Type: hydra.core.Type = hydra.core.Type.maybe(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int8)))

lazy val optionalStringType: hydra.core.Type = hydra.core.Type.maybe(hydra.core.Type.literal(hydra.core.LiteralType.string))

lazy val setOfStringsType: hydra.core.Type = hydra.core.Type.set(hydra.core.Type.literal(hydra.core.LiteralType.string))

lazy val stringOrIntName: hydra.core.Name = "StringOrInt"

lazy val stringOrIntType: hydra.core.Type = hydra.core.Type.union(Seq(hydra.core.FieldType("left",
   hydra.core.Type.literal(hydra.core.LiteralType.string)), hydra.core.FieldType("right",
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))))

lazy val testTypeBuddyListA: hydra.core.Type = hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.record(Seq(hydra.core.FieldType("head", hydra.core.Type.variable("a")),
   hydra.core.FieldType("tail", hydra.core.Type.maybe(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable(hydra.test.testTypes.testTypeBuddyListBName),
   hydra.core.Type.variable("a")))))))))

lazy val testTypeBuddyListAName: hydra.core.Name = "BuddyListA"

lazy val testTypeBuddyListB: hydra.core.Type = hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.record(Seq(hydra.core.FieldType("head", hydra.core.Type.variable("a")),
   hydra.core.FieldType("tail", hydra.core.Type.maybe(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable(hydra.test.testTypes.testTypeBuddyListAName),
   hydra.core.Type.variable("a")))))))))

lazy val testTypeBuddyListBName: hydra.core.Name = "BuddyListB"

lazy val testTypeComparison: hydra.core.Type = hydra.core.Type.union(Seq(hydra.core.FieldType("lessThan",
   hydra.core.Type.unit), hydra.core.FieldType("equalTo", hydra.core.Type.unit), hydra.core.FieldType("greaterThan",
   hydra.core.Type.unit)))

lazy val testTypeComparisonName: hydra.core.Name = "Comparison"

lazy val testTypeEither: hydra.core.Type = hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.forall(hydra.core.ForallType("b", hydra.core.Type.union(Seq(hydra.core.FieldType("left",
   hydra.core.Type.variable("a")), hydra.core.FieldType("right", hydra.core.Type.variable("b"))))))))

lazy val testTypeEitherName: hydra.core.Name = "Either"

lazy val testTypeHydraLiteralType: hydra.core.Type = hydra.core.Type.union(Seq(hydra.core.FieldType("boolean",
   hydra.core.Type.literal(hydra.core.LiteralType.boolean)), hydra.core.FieldType("string",
   hydra.core.Type.literal(hydra.core.LiteralType.string))))

lazy val testTypeHydraLiteralTypeName: hydra.core.Name = "HydraLiteralType"

lazy val testTypeHydraType: hydra.core.Type = hydra.core.Type.union(Seq(hydra.core.FieldType("literal",
   hydra.core.Type.variable(hydra.test.testTypes.testTypeHydraLiteralTypeName)), hydra.core.FieldType("list",
   hydra.core.Type.variable(hydra.test.testTypes.testTypeHydraTypeName))))

lazy val testTypeHydraTypeName: hydra.core.Name = "HydraType"

lazy val testTypeIntList: hydra.core.Type = hydra.core.Type.record(Seq(hydra.core.FieldType("head",
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))),
   hydra.core.FieldType("tail", hydra.core.Type.maybe(hydra.core.Type.variable(hydra.test.testTypes.testTypeIntListName)))))

lazy val testTypeIntListName: hydra.core.Name = "IntList"

lazy val testTypeLatLon: hydra.core.Type = hydra.core.Type.record(Seq(hydra.core.FieldType("lat",
   hydra.core.Type.literal(hydra.core.LiteralType.float(hydra.core.FloatType.float32))),
   hydra.core.FieldType("lon", hydra.core.Type.literal(hydra.core.LiteralType.float(hydra.core.FloatType.float32)))))

lazy val testTypeLatLonName: hydra.core.Name = "LatLon"

lazy val testTypeLatLonPoly: hydra.core.Type = hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.record(Seq(hydra.core.FieldType("lat", hydra.core.Type.variable("a")),
   hydra.core.FieldType("lon", hydra.core.Type.variable("a"))))))

lazy val testTypeLatLonPolyName: hydra.core.Name = "LatLonPoly"

lazy val testTypeList: hydra.core.Type = hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.record(Seq(hydra.core.FieldType("head", hydra.core.Type.variable("a")),
   hydra.core.FieldType("tail", hydra.core.Type.maybe(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable(hydra.test.testTypes.testTypeListName),
   hydra.core.Type.variable("a")))))))))

lazy val testTypeListName: hydra.core.Name = "List"

lazy val testTypeName: hydra.core.Name = "Test"

lazy val testTypeNumber: hydra.core.Type = hydra.core.Type.union(Seq(hydra.core.FieldType("int",
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))),
   hydra.core.FieldType("float", hydra.core.Type.literal(hydra.core.LiteralType.float(hydra.core.FloatType.float32)))))

lazy val testTypeNumberName: hydra.core.Name = "Number"

lazy val testTypePerson: hydra.core.Type = hydra.core.Type.record(Seq(hydra.core.FieldType("firstName",
   hydra.core.Type.literal(hydra.core.LiteralType.string)), hydra.core.FieldType("lastName",
   hydra.core.Type.literal(hydra.core.LiteralType.string)), hydra.core.FieldType("age",
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))))

lazy val testTypePersonName: hydra.core.Name = "Person"

lazy val testTypePersonOrSomething: hydra.core.Type = hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.union(Seq(hydra.core.FieldType("person", hydra.core.Type.variable(hydra.test.testTypes.testTypePersonName)),
   hydra.core.FieldType("other", hydra.core.Type.variable("a"))))))

lazy val testTypePersonOrSomethingName: hydra.core.Name = "PersonOrSomething"

lazy val testTypePolymorphicWrapper: hydra.core.Type = hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.wrap(hydra.core.Type.list(hydra.core.Type.variable("a")))))

lazy val testTypePolymorphicWrapperName: hydra.core.Name = "PolymorphicWrapper"

lazy val testTypeSimpleNumber: hydra.core.Type = hydra.core.Type.union(Seq(hydra.core.FieldType("int",
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))),
   hydra.core.FieldType("float", hydra.core.Type.literal(hydra.core.LiteralType.float(hydra.core.FloatType.float32)))))

lazy val testTypeSimpleNumberName: hydra.core.Name = "SimpleNumber"

lazy val testTypeStringAlias: hydra.core.Type = hydra.core.Type.wrap(hydra.core.Type.literal(hydra.core.LiteralType.string))

lazy val testTypeStringAliasName: hydra.core.Name = "StringAlias"

lazy val testTypeSymmetricTriple: hydra.core.Type = hydra.core.Type.forall(hydra.core.ForallType("v",
   hydra.core.Type.forall(hydra.core.ForallType("e", hydra.core.Type.wrap(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable(hydra.test.testTypes.testTypeTripleName),
   hydra.core.Type.variable("v"))), hydra.core.Type.variable("e"))), hydra.core.Type.variable("v"))))))))

lazy val testTypeSymmetricTripleName: hydra.core.Name = "SymmetricTriple"

lazy val testTypeTimestamp: hydra.core.Type = hydra.core.Type.union(Seq(hydra.core.FieldType("unixTimeMillis",
   hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.uint64))),
   hydra.core.FieldType("date", hydra.core.Type.literal(hydra.core.LiteralType.string))))

lazy val testTypeTimestampName: hydra.core.Name = "Timestamp"

lazy val testTypeTriple: hydra.core.Type = hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.forall(hydra.core.ForallType("b", hydra.core.Type.forall(hydra.core.ForallType("c",
   hydra.core.Type.record(Seq(hydra.core.FieldType("first", hydra.core.Type.variable("a")),
   hydra.core.FieldType("second", hydra.core.Type.variable("b")), hydra.core.FieldType("third",
   hydra.core.Type.variable("c"))))))))))

lazy val testTypeTripleName: hydra.core.Name = "Triple"

lazy val testTypeUnionMonomorphic: hydra.core.Type = hydra.core.Type.union(Seq(hydra.core.FieldType("bool",
   hydra.core.Type.literal(hydra.core.LiteralType.boolean)), hydra.core.FieldType("string",
   hydra.core.Type.literal(hydra.core.LiteralType.string)), hydra.core.FieldType("unit",
   hydra.core.Type.unit)))

lazy val testTypeUnionMonomorphicName: hydra.core.Name = "UnionMonomorphic"

lazy val testTypeUnionPolymorphicRecursive: hydra.core.Type = hydra.core.Type.forall(hydra.core.ForallType("a",
   hydra.core.Type.union(Seq(hydra.core.FieldType("bool", hydra.core.Type.literal(hydra.core.LiteralType.boolean)),
   hydra.core.FieldType("value", hydra.core.Type.variable("a")), hydra.core.FieldType("other",
   hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable(hydra.test.testTypes.testTypeUnionPolymorphicRecursiveName),
   hydra.core.Type.variable("a"))))))))

lazy val testTypeUnionPolymorphicRecursiveName: hydra.core.Name = "UnionPolymorphicRecursive"

lazy val testTypeUnit: hydra.core.Type = hydra.core.Type.record(Seq())

lazy val testTypeUnitName: hydra.core.Name = "Unit"
