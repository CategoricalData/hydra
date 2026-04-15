package hydra.test.testGraph

import hydra.core.*

import hydra.packaging.*

lazy val testContext: hydra.context.Context = hydra.lexical.emptyContext

lazy val testGraph: hydra.graph.Graph = hydra.TestSuiteRunner.buildTestGraph()

lazy val testNamespace: hydra.packaging.Namespace = "testGraph"

lazy val testSchemaNamespace: hydra.packaging.Namespace = "testSchemaGraph"

lazy val testTerms: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maps.fromList[hydra.core.Name,
   hydra.core.Term](Seq(Tuple2("testDataArthur", hydra.test.testTerms.testDataArthur)))

lazy val testTypes: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.fromList[hydra.core.Name,
   hydra.core.Type](Seq(Tuple2(hydra.test.testTypes.testTypeBuddyListAName, hydra.test.testTypes.testTypeBuddyListA),
   Tuple2(hydra.test.testTypes.testTypeBuddyListBName, hydra.test.testTypes.testTypeBuddyListB),
   Tuple2(hydra.test.testTypes.testTypeComparisonName, hydra.test.testTypes.testTypeComparison),
   Tuple2(hydra.test.testTypes.testTypeEitherName, hydra.test.testTypes.testTypeEither),
   Tuple2(hydra.test.testTypes.testTypeHydraLiteralTypeName, hydra.test.testTypes.testTypeHydraLiteralType),
   Tuple2(hydra.test.testTypes.testTypeHydraTypeName, hydra.test.testTypes.testTypeHydraType),
   Tuple2(hydra.test.testTypes.testTypeIntListName, hydra.test.testTypes.testTypeIntList),
   Tuple2(hydra.test.testTypes.testTypeLatLonName, hydra.test.testTypes.testTypeLatLon),
   Tuple2(hydra.test.testTypes.testTypeLatLonPolyName, hydra.test.testTypes.testTypeLatLonPoly),
   Tuple2(hydra.test.testTypes.testTypeListName, hydra.test.testTypes.testTypeList),
   Tuple2(hydra.test.testTypes.testTypeNumberName, hydra.test.testTypes.testTypeNumber),
   Tuple2(hydra.test.testTypes.testTypePersonName, hydra.test.testTypes.testTypePerson),
   Tuple2(hydra.test.testTypes.testTypePersonOrSomethingName, hydra.test.testTypes.testTypePersonOrSomething),
   Tuple2(hydra.test.testTypes.testTypePolymorphicWrapperName, hydra.test.testTypes.testTypePolymorphicWrapper),
   Tuple2(hydra.test.testTypes.testTypeSimpleNumberName, hydra.test.testTypes.testTypeSimpleNumber),
   Tuple2(hydra.test.testTypes.testTypeStringAliasName, hydra.test.testTypes.testTypeStringAlias),
   Tuple2(hydra.test.testTypes.testTypeSymmetricTripleName, hydra.test.testTypes.testTypeSymmetricTriple),
   Tuple2(hydra.test.testTypes.testTypeTimestampName, hydra.test.testTypes.testTypeTimestamp),
   Tuple2(hydra.test.testTypes.testTypeTripleName, hydra.test.testTypes.testTypeTriple),
   Tuple2(hydra.test.testTypes.testTypeUnionMonomorphicName, hydra.test.testTypes.testTypeUnionMonomorphic),
   Tuple2(hydra.test.testTypes.testTypeUnionPolymorphicRecursiveName, hydra.test.testTypes.testTypeUnionPolymorphicRecursive),
   Tuple2(hydra.test.testTypes.testTypeUnitName, hydra.test.testTypes.testTypeUnit)))
