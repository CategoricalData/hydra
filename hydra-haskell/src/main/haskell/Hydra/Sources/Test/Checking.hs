{-# LANGUAGE OverloadedStrings #-}

-- | Type checking test cases for the common test suite
module Hydra.Sources.Test.Checking where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Testing
import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T
import qualified Hydra.Dsl.Phantoms as Phantoms

import Prelude hiding (map, product, sum)
import qualified Data.List as L
import qualified Data.Map as M


checkingTests :: TTerm TestGroup
checkingTests = supergroup "Type checking tests" [
  annotatedTermsTests,
  applicationsTests,
  eithersTests,
  flowsTests,
  functionsTests,
  letTermsTests,
  listsTests,
  literalsTests,
  mapsTests,
  optionalsTests,
  pairsTests,
  productsTests,
  recordsTests,
  setsTests,
  sumsTests,
  unionsTests,
  unitTests,
  variablesTests,
  wrappedTermsTests,
  failOnUntypedTests]

------ Annotated terms ------

annotatedTermsTests :: TTerm TestGroup
annotatedTermsTests = supergroup "Annotated terms" [
  topLevelAnnotationsTests,
  nestedAnnotationsTests,
  annotationsInComplexContextsTests]

topLevelAnnotationsTests :: TTerm TestGroup
topLevelAnnotationsTests = subgroup "Top-level annotations" []

nestedAnnotationsTests :: TTerm TestGroup
nestedAnnotationsTests = subgroup "Nested annotations" []

annotationsInComplexContextsTests :: TTerm TestGroup
annotationsInComplexContextsTests = subgroup "Annotations in complex contexts" []

------ Applications ------

applicationsTests :: TTerm TestGroup
applicationsTests = supergroup "Applications" [
  simpleFunctionApplicationsTests,
  partialApplicationsTests,
  higherOrderApplicationsTests,
  polymorphicApplicationsTests,
  applicationsInComplexContextsTests,
  applicationsWithComplexArgumentsTests]

simpleFunctionApplicationsTests :: TTerm TestGroup
simpleFunctionApplicationsTests = subgroup "Simple function applications" []

partialApplicationsTests :: TTerm TestGroup
partialApplicationsTests = subgroup "Partial applications" []

higherOrderApplicationsTests :: TTerm TestGroup
higherOrderApplicationsTests = subgroup "Higher-order applications" []

polymorphicApplicationsTests :: TTerm TestGroup
polymorphicApplicationsTests = subgroup "Polymorphic applications" []

applicationsInComplexContextsTests :: TTerm TestGroup
applicationsInComplexContextsTests = subgroup "Applications in complex contexts" []

applicationsWithComplexArgumentsTests :: TTerm TestGroup
applicationsWithComplexArgumentsTests = subgroup "Applications with complex arguments" []

------ Eithers ------

eithersTests :: TTerm TestGroup
eithersTests = supergroup "Eithers" [
  leftValuesTests,
  rightValuesTests,
  polymorphicEithersTests,
  eithersInComplexContextsTests,
  nestedEithersTests,
  eithersWithComplexTypesTests]

leftValuesTests :: TTerm TestGroup
leftValuesTests = subgroup "Left values" []

rightValuesTests :: TTerm TestGroup
rightValuesTests = subgroup "Right values" []

polymorphicEithersTests :: TTerm TestGroup
polymorphicEithersTests = subgroup "Polymorphic eithers" []

eithersInComplexContextsTests :: TTerm TestGroup
eithersInComplexContextsTests = subgroup "Eithers in complex contexts" []

nestedEithersTests :: TTerm TestGroup
nestedEithersTests = subgroup "Nested eithers" []

eithersWithComplexTypesTests :: TTerm TestGroup
eithersWithComplexTypesTests = subgroup "Eithers with complex types" []

------ Eliminations ------

eliminationsTests :: TTerm TestGroup
eliminationsTests = supergroup "Eliminations" [
  productEliminationsTests,
  recordEliminationsTests,
  unionEliminationsTests,
  wrapEliminationsTests]

------ Flows ------

flowsTests :: TTerm TestGroup
flowsTests = supergroup "Flows" [
  flowsWithFailureAcrossLetBindingsTests]

flowsWithFailureAcrossLetBindingsTests :: TTerm TestGroup
flowsWithFailureAcrossLetBindingsTests = subgroup "Flows with failure across let bindings" []

------ Functions ------

functionsTests :: TTerm TestGroup
functionsTests = supergroup "Functions" [
  eliminationsTests,
  lambdasTests,
  primitivesTests]

------ Lambdas ------

lambdasTests :: TTerm TestGroup
lambdasTests = supergroup "Lambdas" [
  simpleLambdasTests,
  multiParameterLambdasTests,
  lambdasWithOperationsTests,
  nestedLambdasTests,
  lambdasInComplexContextsTests,
  higherOrderLambdasTests]

simpleLambdasTests :: TTerm TestGroup
simpleLambdasTests = subgroup "Simple lambdas" []

multiParameterLambdasTests :: TTerm TestGroup
multiParameterLambdasTests = subgroup "Multi-parameter lambdas" []

lambdasWithOperationsTests :: TTerm TestGroup
lambdasWithOperationsTests = subgroup "Lambdas with operations" []

nestedLambdasTests :: TTerm TestGroup
nestedLambdasTests = subgroup "Nested lambdas" []

lambdasInComplexContextsTests :: TTerm TestGroup
lambdasInComplexContextsTests = subgroup "Lambdas in complex contexts" []

higherOrderLambdasTests :: TTerm TestGroup
higherOrderLambdasTests = subgroup "Higher-order lambdas" []

------ Let terms ------

letTermsTests :: TTerm TestGroup
letTermsTests = supergroup "Let terms" [
  simpleLetBindingsTests,
  letTermsWithShadowingTests,
  recursiveBindingsTests,
  mutualRecursionTests,
  nestedLetTermsTests,
  letWithComplexExpressionsTests]

simpleLetBindingsTests :: TTerm TestGroup
simpleLetBindingsTests = subgroup "Simple let bindings" []

letTermsWithShadowingTests :: TTerm TestGroup
letTermsWithShadowingTests = subgroup "Let terms with shadowing" []

recursiveBindingsTests :: TTerm TestGroup
recursiveBindingsTests = subgroup "Recursive bindings" []

mutualRecursionTests :: TTerm TestGroup
mutualRecursionTests = subgroup "Mutual recursion" []

nestedLetTermsTests :: TTerm TestGroup
nestedLetTermsTests = subgroup "Nested let terms" []

letWithComplexExpressionsTests :: TTerm TestGroup
letWithComplexExpressionsTests = subgroup "Let with complex expressions" []

------ Lists ------

listsTests :: TTerm TestGroup
listsTests = supergroup "Lists" [
  listsOfLiteralsTests,
  emptyListsTests,
  polymorphicListsTests,
  nestedListsTests,
  listsInComplexContextsTests]

listsOfLiteralsTests :: TTerm TestGroup
listsOfLiteralsTests = subgroup "Lists of literals" []

emptyListsTests :: TTerm TestGroup
emptyListsTests = subgroup "Empty lists" []

polymorphicListsTests :: TTerm TestGroup
polymorphicListsTests = subgroup "Polymorphic lists" []

nestedListsTests :: TTerm TestGroup
nestedListsTests = subgroup "Nested lists" []

listsInComplexContextsTests :: TTerm TestGroup
listsInComplexContextsTests = subgroup "Lists in complex contexts" []

------ Literals ------

literalsTests :: TTerm TestGroup
literalsTests = supergroup "Literals" [
  booleanLiteralsTests,
  stringLiteralsTests,
  integerLiteralsTests,
  floatLiteralsTests,
  binaryLiteralsTests,
  literalsInComplexContextsTests]

booleanLiteralsTests :: TTerm TestGroup
booleanLiteralsTests = subgroup "Boolean literals" []

stringLiteralsTests :: TTerm TestGroup
stringLiteralsTests = subgroup "String literals" [
  checkTest "int32 literal" []
    (int32 42)
    (int32 42)
    T.int32,
  checkTest "string literal" []
    (string "hello")
    (string "hello")
    T.string]

integerLiteralsTests :: TTerm TestGroup
integerLiteralsTests = subgroup "Integer literals" []

floatLiteralsTests :: TTerm TestGroup
floatLiteralsTests = subgroup "Float literals" []

binaryLiteralsTests :: TTerm TestGroup
binaryLiteralsTests = subgroup "Binary literals" []

literalsInComplexContextsTests :: TTerm TestGroup
literalsInComplexContextsTests = subgroup "Literals in complex contexts" []

------ Maps ------

mapsTests :: TTerm TestGroup
mapsTests = supergroup "Maps" [
  monomorphicMapsTests,
  polymorphicMapsTests,
  mapsInComplexContextsTests,
  mapsWithComplexTypesTests]

monomorphicMapsTests :: TTerm TestGroup
monomorphicMapsTests = subgroup "Monomorphic maps" []

polymorphicMapsTests :: TTerm TestGroup
polymorphicMapsTests = subgroup "Polymorphic maps" []

mapsInComplexContextsTests :: TTerm TestGroup
mapsInComplexContextsTests = subgroup "Maps in complex contexts" []

mapsWithComplexTypesTests :: TTerm TestGroup
mapsWithComplexTypesTests = subgroup "Maps with complex types" []

------ Optionals ------

optionalsTests :: TTerm TestGroup
optionalsTests = supergroup "Optionals" [
  monomorphicOptionalsTests,
  polymorphicOptionalsTests,
  optionalsInComplexContextsTests,
  nestedOptionalsTests,
  optionalsWithComplexTypesTests]

monomorphicOptionalsTests :: TTerm TestGroup
monomorphicOptionalsTests = subgroup "Monomorphic optionals" []

polymorphicOptionalsTests :: TTerm TestGroup
polymorphicOptionalsTests = subgroup "Polymorphic optionals" []

optionalsInComplexContextsTests :: TTerm TestGroup
optionalsInComplexContextsTests = subgroup "Optionals in complex contexts" []

nestedOptionalsTests :: TTerm TestGroup
nestedOptionalsTests = subgroup "Nested optionals" []

optionalsWithComplexTypesTests :: TTerm TestGroup
optionalsWithComplexTypesTests = subgroup "Optionals with complex types" []

------ Pairs ------

pairsTests :: TTerm TestGroup
pairsTests = supergroup "Pairs" [
  basicPairsTests,
  polymorphicPairsTests,
  pairsInComplexContextsTests,
  nestedPairsTests,
  pairsWithComplexTypesTests]

basicPairsTests :: TTerm TestGroup
basicPairsTests = subgroup "Basic pairs" []

polymorphicPairsTests :: TTerm TestGroup
polymorphicPairsTests = subgroup "Polymorphic pairs" []

pairsInComplexContextsTests :: TTerm TestGroup
pairsInComplexContextsTests = subgroup "Pairs in complex contexts" []

nestedPairsTests :: TTerm TestGroup
nestedPairsTests = subgroup "Nested pairs" []

pairsWithComplexTypesTests :: TTerm TestGroup
pairsWithComplexTypesTests = subgroup "Pairs with complex types" []

------ Primitives ------

primitivesTests :: TTerm TestGroup
primitivesTests = supergroup "Primitives" [
  nullaryPrimitivesTests,
  unaryPrimitivesTests,
  binaryPrimitivesTests,
  ternaryPrimitivesTests,
  monomorphicVsPolymorphicTests,
  higherOrderPrimitivesTests,
  primitivesInComplexContextsTests]

nullaryPrimitivesTests :: TTerm TestGroup
nullaryPrimitivesTests = subgroup "Nullary primitives" []

unaryPrimitivesTests :: TTerm TestGroup
unaryPrimitivesTests = subgroup "Unary primitives" []

binaryPrimitivesTests :: TTerm TestGroup
binaryPrimitivesTests = subgroup "Binary primitives" []

ternaryPrimitivesTests :: TTerm TestGroup
ternaryPrimitivesTests = subgroup "Ternary primitives" []

monomorphicVsPolymorphicTests :: TTerm TestGroup
monomorphicVsPolymorphicTests = subgroup "Monomorphic vs polymorphic" []

higherOrderPrimitivesTests :: TTerm TestGroup
higherOrderPrimitivesTests = subgroup "Higher-order primitives" []

primitivesInComplexContextsTests :: TTerm TestGroup
primitivesInComplexContextsTests = subgroup "Primitives in complex contexts" []

------ Products ------

productsTests :: TTerm TestGroup
productsTests = supergroup "Products" [
  monomorphicProductsTests,
  polymorphicProductsTests,
  nestedProductsTests]

monomorphicProductsTests :: TTerm TestGroup
monomorphicProductsTests = subgroup "Monomorphic products" []

polymorphicProductsTests :: TTerm TestGroup
polymorphicProductsTests = subgroup "Polymorphic products" []

nestedProductsTests :: TTerm TestGroup
nestedProductsTests = subgroup "Nested products" []

------ Product eliminations ------

productEliminationsTests :: TTerm TestGroup
productEliminationsTests = supergroup "Product eliminations" [
  simpleTupleProjectionsTests,
  polymorphicTupleProjectionsTests,
  projectionsWithVariablesTests,
  projectionsInComplexContextsTests,
  projectionsWithMixedTypesTests,
  projectionsWithPrimitiveFunctionsTests]

simpleTupleProjectionsTests :: TTerm TestGroup
simpleTupleProjectionsTests = subgroup "Simple tuple projections" []

polymorphicTupleProjectionsTests :: TTerm TestGroup
polymorphicTupleProjectionsTests = subgroup "Polymorphic tuple projections" []

projectionsWithVariablesTests :: TTerm TestGroup
projectionsWithVariablesTests = subgroup "Projections with variables" []

projectionsInComplexContextsTests :: TTerm TestGroup
projectionsInComplexContextsTests = subgroup "Projections in complex contexts" []

projectionsWithMixedTypesTests :: TTerm TestGroup
projectionsWithMixedTypesTests = subgroup "Projections with mixed types" []

projectionsWithPrimitiveFunctionsTests :: TTerm TestGroup
projectionsWithPrimitiveFunctionsTests = subgroup "Projections with primitive functions" []

------ Records ------

recordsTests :: TTerm TestGroup
recordsTests = supergroup "Records" [
  monomorphicRecordsTests,
  polymorphicRecordsTests,
  recordsInComplexContextsTests,
  multiParameterPolymorphicRecordsTests]

monomorphicRecordsTests :: TTerm TestGroup
monomorphicRecordsTests = subgroup "Monomorphic records" []

polymorphicRecordsTests :: TTerm TestGroup
polymorphicRecordsTests = subgroup "Polymorphic records" []

recordsInComplexContextsTests :: TTerm TestGroup
recordsInComplexContextsTests = subgroup "Records in complex contexts" []

multiParameterPolymorphicRecordsTests :: TTerm TestGroup
multiParameterPolymorphicRecordsTests = subgroup "Multi-parameter polymorphic records" []

------ Record eliminations ------

recordEliminationsTests :: TTerm TestGroup
recordEliminationsTests = supergroup "Record eliminations" [
  simpleRecordProjectionsTests,
  recordProjectionsAppliedToRecordsTests,
  polymorphicRecordProjectionsTests,
  polymorphicRecordProjectionsAppliedTests,
  recordProjectionsWithVariablesTests,
  recordProjectionsInComplexContextsTests,
  multiParameterPolymorphicProjectionsTests,
  higherOrderRecordProjectionsTests,
  recursiveRecordProjectionsTests,
  recordProjectionsWithMutualRecursionTests]

simpleRecordProjectionsTests :: TTerm TestGroup
simpleRecordProjectionsTests = subgroup "Simple record projections" []

recordProjectionsAppliedToRecordsTests :: TTerm TestGroup
recordProjectionsAppliedToRecordsTests = subgroup "Record projections applied to records" []

polymorphicRecordProjectionsTests :: TTerm TestGroup
polymorphicRecordProjectionsTests = subgroup "Polymorphic record projections" []

polymorphicRecordProjectionsAppliedTests :: TTerm TestGroup
polymorphicRecordProjectionsAppliedTests = subgroup "Polymorphic record projections applied" []

recordProjectionsWithVariablesTests :: TTerm TestGroup
recordProjectionsWithVariablesTests = subgroup "Record projections with variables" []

recordProjectionsInComplexContextsTests :: TTerm TestGroup
recordProjectionsInComplexContextsTests = subgroup "Record projections in complex contexts" []

multiParameterPolymorphicProjectionsTests :: TTerm TestGroup
multiParameterPolymorphicProjectionsTests = subgroup "Multi-parameter polymorphic projections" []

higherOrderRecordProjectionsTests :: TTerm TestGroup
higherOrderRecordProjectionsTests = subgroup "Higher-order record projections" []

recursiveRecordProjectionsTests :: TTerm TestGroup
recursiveRecordProjectionsTests = subgroup "Recursive record projections" []

recordProjectionsWithMutualRecursionTests :: TTerm TestGroup
recordProjectionsWithMutualRecursionTests = subgroup "Record projections with mutual recursion" []

------ Sets ------

setsTests :: TTerm TestGroup
setsTests = supergroup "Sets" [
  monomorphicSetsTests,
  polymorphicSetsTests,
  setsInComplexContextsTests,
  nestedSetsTests,
  setsWithComplexTypesTests]

monomorphicSetsTests :: TTerm TestGroup
monomorphicSetsTests = subgroup "Monomorphic sets" []

polymorphicSetsTests :: TTerm TestGroup
polymorphicSetsTests = subgroup "Polymorphic sets" []

setsInComplexContextsTests :: TTerm TestGroup
setsInComplexContextsTests = subgroup "Sets in complex contexts" []

nestedSetsTests :: TTerm TestGroup
nestedSetsTests = subgroup "Nested sets" []

setsWithComplexTypesTests :: TTerm TestGroup
setsWithComplexTypesTests = subgroup "Sets with complex types" []

------ Sums ------

sumsTests :: TTerm TestGroup
sumsTests = subgroup "Sums" []

------ Unions ------

unionsTests :: TTerm TestGroup
unionsTests = supergroup "Unions" [
  simpleUnionInjectionsTests,
  unionInjectionsWithDataTests,
  polymorphicUnionInjectionsTests,
  polymorphicRecursiveUnionInjectionsTests,
  polymorphicUnionsFromLambdaTests,
  unionsInComplexContextsTests,
  multiParameterPolymorphicInjectionsTests]

simpleUnionInjectionsTests :: TTerm TestGroup
simpleUnionInjectionsTests = subgroup "Simple union injections" []

unionInjectionsWithDataTests :: TTerm TestGroup
unionInjectionsWithDataTests = subgroup "Union injections with data" []

polymorphicUnionInjectionsTests :: TTerm TestGroup
polymorphicUnionInjectionsTests = subgroup "Polymorphic union injections" []

polymorphicRecursiveUnionInjectionsTests :: TTerm TestGroup
polymorphicRecursiveUnionInjectionsTests = subgroup "Polymorphic recursive union injections" []

polymorphicUnionsFromLambdaTests :: TTerm TestGroup
polymorphicUnionsFromLambdaTests = subgroup "Polymorphic unions from lambda" []

unionsInComplexContextsTests :: TTerm TestGroup
unionsInComplexContextsTests = subgroup "Unions in complex contexts" []

multiParameterPolymorphicInjectionsTests :: TTerm TestGroup
multiParameterPolymorphicInjectionsTests = subgroup "Multi-parameter polymorphic injections" []

------ Union eliminations ------

unionEliminationsTests :: TTerm TestGroup
unionEliminationsTests = supergroup "Union eliminations" [
  simpleUnitVariantEliminationsTests,
  unionEliminationsWithDataTests,
  polymorphicUnionEliminationsTests,
  unionEliminationsWithDefaultsTests,
  nestedUnionEliminationsTests,
  unionEliminationsInComplexContextsTests,
  multiParameterPolymorphicCaseStatementsTests,
  higherOrderUnionEliminationsTests,
  recursiveUnionEliminationsTests]

simpleUnitVariantEliminationsTests :: TTerm TestGroup
simpleUnitVariantEliminationsTests = subgroup "Simple unit variant eliminations" []

unionEliminationsWithDataTests :: TTerm TestGroup
unionEliminationsWithDataTests = subgroup "Union eliminations with data" []

polymorphicUnionEliminationsTests :: TTerm TestGroup
polymorphicUnionEliminationsTests = subgroup "Polymorphic union eliminations" []

unionEliminationsWithDefaultsTests :: TTerm TestGroup
unionEliminationsWithDefaultsTests = subgroup "Union eliminations with defaults" []

nestedUnionEliminationsTests :: TTerm TestGroup
nestedUnionEliminationsTests = subgroup "Nested union eliminations" []

unionEliminationsInComplexContextsTests :: TTerm TestGroup
unionEliminationsInComplexContextsTests = subgroup "Union eliminations in complex contexts" []

multiParameterPolymorphicCaseStatementsTests :: TTerm TestGroup
multiParameterPolymorphicCaseStatementsTests = subgroup "Multi-parameter polymorphic case statements" []

higherOrderUnionEliminationsTests :: TTerm TestGroup
higherOrderUnionEliminationsTests = subgroup "Higher-order union eliminations" []

recursiveUnionEliminationsTests :: TTerm TestGroup
recursiveUnionEliminationsTests = subgroup "Recursive union eliminations" []

------ Unit ------

unitTests :: TTerm TestGroup
unitTests = supergroup "Unit" [
  unitTermTests,
  unitTermInPolymorphicContextTests]

unitTermTests :: TTerm TestGroup
unitTermTests = subgroup "Unit term" []

unitTermInPolymorphicContextTests :: TTerm TestGroup
unitTermInPolymorphicContextTests = subgroup "Unit term in polymorphic context" []

------ Variables ------

variablesTests :: TTerm TestGroup
variablesTests = supergroup "Variables" [
  simpleVariableLookupTests,
  variableScopingTests,
  polymorphicVariablesTests,
  variablesInComplexContextsTests,
  recursiveVariablesTests]

simpleVariableLookupTests :: TTerm TestGroup
simpleVariableLookupTests = subgroup "Simple variable lookup" []

variableScopingTests :: TTerm TestGroup
variableScopingTests = subgroup "Variable scoping" []

polymorphicVariablesTests :: TTerm TestGroup
polymorphicVariablesTests = subgroup "Polymorphic variables" []

variablesInComplexContextsTests :: TTerm TestGroup
variablesInComplexContextsTests = subgroup "Variables in complex contexts" []

recursiveVariablesTests :: TTerm TestGroup
recursiveVariablesTests = subgroup "Recursive variables" []

------ Wrapped terms ------

wrappedTermsTests :: TTerm TestGroup
wrappedTermsTests = supergroup "Wrapped terms" [
  monomorphicWrappedTermsTests,
  polymorphicWrappedTermsTests,
  wrappedTermsInComplexContextsTests,
  nestedWrappedTermsTests,
  multipleWrappingLevelsTests,
  multiParameterPolymorphicWrappersTests]

monomorphicWrappedTermsTests :: TTerm TestGroup
monomorphicWrappedTermsTests = subgroup "Monomorphic wrapped terms" []

polymorphicWrappedTermsTests :: TTerm TestGroup
polymorphicWrappedTermsTests = subgroup "Polymorphic wrapped terms" []

wrappedTermsInComplexContextsTests :: TTerm TestGroup
wrappedTermsInComplexContextsTests = subgroup "Wrapped terms in complex contexts" []

nestedWrappedTermsTests :: TTerm TestGroup
nestedWrappedTermsTests = subgroup "Nested wrapped terms" []

multipleWrappingLevelsTests :: TTerm TestGroup
multipleWrappingLevelsTests = subgroup "Multiple wrapping levels" []

multiParameterPolymorphicWrappersTests :: TTerm TestGroup
multiParameterPolymorphicWrappersTests = subgroup "Multi-parameter polymorphic wrappers" []

------ Wrap eliminations ------

wrapEliminationsTests :: TTerm TestGroup
wrapEliminationsTests = supergroup "Wrap eliminations" [
  monomorphicUnwrappingTests,
  polymorphicUnwrappingTests,
  unwrapEliminationsInApplicationsTests,
  unwrapInComplexContextsTests,
  multiParameterPolymorphicUnwrappersTests,
  chainedUnwrappingTests,
  multipleUnwrapOperationsTests]

monomorphicUnwrappingTests :: TTerm TestGroup
monomorphicUnwrappingTests = subgroup "Monomorphic unwrapping" []

polymorphicUnwrappingTests :: TTerm TestGroup
polymorphicUnwrappingTests = subgroup "Polymorphic unwrapping" []

unwrapEliminationsInApplicationsTests :: TTerm TestGroup
unwrapEliminationsInApplicationsTests = subgroup "Unwrap eliminations in applications" []

unwrapInComplexContextsTests :: TTerm TestGroup
unwrapInComplexContextsTests = subgroup "Unwrap in complex contexts" []

multiParameterPolymorphicUnwrappersTests :: TTerm TestGroup
multiParameterPolymorphicUnwrappersTests = subgroup "Multi-parameter polymorphic unwrappers" []

chainedUnwrappingTests :: TTerm TestGroup
chainedUnwrappingTests = subgroup "Chained unwrapping" []

multipleUnwrapOperationsTests :: TTerm TestGroup
multipleUnwrapOperationsTests = subgroup "Multiple unwrap operations" []

------ Fail on untyped (pre-inference) terms ------

failOnUntypedTests :: TTerm TestGroup
failOnUntypedTests = supergroup "Fail on untyped (pre-inference) terms" [
  untypedLambdasTests]

untypedLambdasTests :: TTerm TestGroup
untypedLambdasTests = subgroup "Untyped lambdas" []

------ Helper functions ------

-- Helper function to create a type checking test case
checkTest :: String -> [Tag] -> TTerm Term -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
checkTest name tags input outputTerm outputType = testCaseWithMetadata (Phantoms.string name)
  (testCaseTypeChecking $ typeCheckingTestCase input outputTerm outputType) Phantoms.nothing (Phantoms.list $ tag . unTag <$> tags)

-- Create a TestCase variant for type checking
testCaseTypeChecking :: TTerm TypeCheckingTestCase -> TTerm TestCase
testCaseTypeChecking = variant _TestCase _TestCase_typeChecking

-- Create a TypeCheckingTestCase record
typeCheckingTestCase :: TTerm Term -> TTerm Term -> TTerm Type -> TTerm TypeCheckingTestCase
typeCheckingTestCase input outputTerm outputType = Phantoms.record _TypeCheckingTestCase [
  Phantoms.field _TypeCheckingTestCase_input input,
  Phantoms.field _TypeCheckingTestCase_outputTerm outputTerm,
  Phantoms.field _TypeCheckingTestCase_outputType outputType]
