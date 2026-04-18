// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * A module defining the graph used in the test suite.
 */
public interface TestGraph {
  static hydra.context.Context testContext() {
    return hydra.Lexical.emptyContext();
  }

  static hydra.graph.Graph testGraph() {
    return hydra.Lexical.emptyGraph();
  }

  static hydra.packaging.Namespace testNamespace() {
    return new hydra.packaging.Namespace("testGraph");
  }

  static hydra.packaging.Namespace testSchemaNamespace() {
    return new hydra.packaging.Namespace("testSchemaGraph");
  }

  static java.util.Map<hydra.core.Name, hydra.core.Term> testTerms() {
    return hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>(new hydra.core.Name("testDataArthur"), hydra.test.TestTerms.testDataArthur())))));
  }

  static java.util.Map<hydra.core.Name, hydra.core.Type> testTypes() {
    return hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeBuddyListAName(), hydra.test.TestTypes.testTypeBuddyListA()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeBuddyListBName(), hydra.test.TestTypes.testTypeBuddyListB()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeComparisonName(), hydra.test.TestTypes.testTypeComparison()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeEitherName(), hydra.test.TestTypes.testTypeEither()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeHydraLiteralTypeName(), hydra.test.TestTypes.testTypeHydraLiteralType()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeHydraTypeName(), hydra.test.TestTypes.testTypeHydraType()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeIntListName(), hydra.test.TestTypes.testTypeIntList()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeLatLonName(), hydra.test.TestTypes.testTypeLatLon()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeLatLonPolyName(), hydra.test.TestTypes.testTypeLatLonPoly()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeListName(), hydra.test.TestTypes.testTypeList()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeNumberName(), hydra.test.TestTypes.testTypeNumber()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypePersonName(), hydra.test.TestTypes.testTypePerson()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypePersonOrSomethingName(), hydra.test.TestTypes.testTypePersonOrSomething()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypePolymorphicWrapperName(), hydra.test.TestTypes.testTypePolymorphicWrapper()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeSimpleNumberName(), hydra.test.TestTypes.testTypeSimpleNumber()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeStringAliasName(), hydra.test.TestTypes.testTypeStringAlias()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeSymmetricTripleName(), hydra.test.TestTypes.testTypeSymmetricTriple()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeTimestampName(), hydra.test.TestTypes.testTypeTimestamp()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeTripleName(), hydra.test.TestTypes.testTypeTriple()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeUnionMonomorphicName(), hydra.test.TestTypes.testTypeUnionMonomorphic()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName(), hydra.test.TestTypes.testTypeUnionPolymorphicRecursive()))),
      (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(hydra.test.TestTypes.testTypeUnitName(), hydra.test.TestTypes.testTypeUnit())))));
  }
}
