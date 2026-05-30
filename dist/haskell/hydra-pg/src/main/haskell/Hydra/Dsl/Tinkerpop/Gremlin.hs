-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.tinkerpop.gremlin

module Hydra.Dsl.Tinkerpop.Gremlin where

import qualified Hydra.Core as Core
import qualified Hydra.Typed as Phantoms
import qualified Hydra.Tinkerpop.Gremlin as Gremlin
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

booleanArgumentValue :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Gremlin.BooleanArgument
booleanArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.BooleanArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

booleanArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.BooleanArgument
booleanArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.BooleanArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

byArgsOrder :: Phantoms.TypedTerm Gremlin.TraversalOrderArgument -> Phantoms.TypedTerm Gremlin.ByArgs
byArgsOrder x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ByArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "order"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

byArgsOther :: Phantoms.TypedTerm Gremlin.ByOtherArgs -> Phantoms.TypedTerm Gremlin.ByArgs
byArgsOther x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ByArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

byArgsToken :: Phantoms.TypedTerm Gremlin.TraversalTokenArgument -> Phantoms.TypedTerm Gremlin.ByArgs
byArgsToken x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ByArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "token"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

byOtherArgsComparator :: Phantoms.TypedTerm (Maybe Gremlin.TraversalComparatorArgument) -> Phantoms.TypedTerm Gremlin.ByOtherArgs
byOtherArgsComparator x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ByOtherArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "comparator"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

byOtherArgsOther :: Phantoms.TypedTerm (Maybe Gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal) -> Phantoms.TypedTerm Gremlin.ByOtherArgs
byOtherArgsOther x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ByOtherArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

chainedTraversal :: Phantoms.TypedTerm Gremlin.TraversalMethod -> Phantoms.TypedTerm Gremlin.ChainedTraversalElement -> Phantoms.TypedTerm Gremlin.ChainedTraversal
chainedTraversal first rest =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTypedTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTypedTerm rest)}]}))

chainedTraversalElementMethod :: Phantoms.TypedTerm Gremlin.TraversalMethod -> Phantoms.TypedTerm Gremlin.ChainedTraversalElement
chainedTraversalElementMethod x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversalElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

chainedTraversalElementSelf :: Phantoms.TypedTerm Gremlin.TraversalSelfMethod -> Phantoms.TypedTerm Gremlin.ChainedTraversalElement
chainedTraversalElementSelf x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversalElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "self"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

chainedTraversalFirst :: Phantoms.TypedTerm Gremlin.ChainedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
chainedTraversalFirst x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
        Core.projectionFieldName = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

chainedTraversalRest :: Phantoms.TypedTerm Gremlin.ChainedTraversal -> Phantoms.TypedTerm Gremlin.ChainedTraversalElement
chainedTraversalRest x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
        Core.projectionFieldName = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

chainedTraversalWithFirst :: Phantoms.TypedTerm Gremlin.ChainedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod -> Phantoms.TypedTerm Gremlin.ChainedTraversal
chainedTraversalWithFirst original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
              Core.projectionFieldName = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

chainedTraversalWithRest :: Phantoms.TypedTerm Gremlin.ChainedTraversal -> Phantoms.TypedTerm Gremlin.ChainedTraversalElement -> Phantoms.TypedTerm Gremlin.ChainedTraversal
chainedTraversalWithRest original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
              Core.projectionFieldName = (Core.Name "first")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

chooseArgsFunction :: Phantoms.TypedTerm Gremlin.TraversalFunctionArgument -> Phantoms.TypedTerm Gremlin.ChooseArgs
chooseArgsFunction x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChooseArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

chooseArgsPredicateTraversal :: Phantoms.TypedTerm Gremlin.PredicateTraversalArgument -> Phantoms.TypedTerm Gremlin.ChooseArgs
chooseArgsPredicateTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChooseArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicateTraversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

chooseArgsTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversalArgument -> Phantoms.TypedTerm Gremlin.ChooseArgs
chooseArgsTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChooseArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

concatArgsString :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.ConcatArgs
concatArgsString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ConcatArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

concatArgsTraversal :: Phantoms.TypedTerm [Gremlin.NestedTraversal] -> Phantoms.TypedTerm Gremlin.ConcatArgs
concatArgsTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ConcatArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

configuration :: Phantoms.TypedTerm Gremlin.KeywordOrIdentifier -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.Configuration
configuration key value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

configurationKey :: Phantoms.TypedTerm Gremlin.Configuration -> Phantoms.TypedTerm Gremlin.KeywordOrIdentifier
configurationKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

configurationValue :: Phantoms.TypedTerm Gremlin.Configuration -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
configurationValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

configurationWithKey :: Phantoms.TypedTerm Gremlin.Configuration -> Phantoms.TypedTerm Gremlin.KeywordOrIdentifier -> Phantoms.TypedTerm Gremlin.Configuration
configurationWithKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

configurationWithValue :: Phantoms.TypedTerm Gremlin.Configuration -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.Configuration
configurationWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

connectedComponentConstantsComponent :: Phantoms.TypedTerm Gremlin.ConnectedComponentConstants
connectedComponentConstantsComponent =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ConnectedComponentConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "component"),
        Core.fieldTerm = Core.TermUnit}}))

connectedComponentConstantsEdges :: Phantoms.TypedTerm Gremlin.ConnectedComponentConstants
connectedComponentConstantsEdges =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ConnectedComponentConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edges"),
        Core.fieldTerm = Core.TermUnit}}))

connectedComponentConstantsPropertyName :: Phantoms.TypedTerm Gremlin.ConnectedComponentConstants
connectedComponentConstantsPropertyName =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ConnectedComponentConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyName"),
        Core.fieldTerm = Core.TermUnit}}))

dateAddArgs :: Phantoms.TypedTerm Gremlin.TraversalDTArgument -> Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.DateAddArgs
dateAddArgs unit duration =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Phantoms.unTypedTerm unit)},
        Core.Field {
          Core.fieldName = (Core.Name "duration"),
          Core.fieldTerm = (Phantoms.unTypedTerm duration)}]}))

dateAddArgsDuration :: Phantoms.TypedTerm Gremlin.DateAddArgs -> Phantoms.TypedTerm Gremlin.IntegerArgument
dateAddArgsDuration x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
        Core.projectionFieldName = (Core.Name "duration")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dateAddArgsUnit :: Phantoms.TypedTerm Gremlin.DateAddArgs -> Phantoms.TypedTerm Gremlin.TraversalDTArgument
dateAddArgsUnit x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
        Core.projectionFieldName = (Core.Name "unit")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dateAddArgsWithDuration :: Phantoms.TypedTerm Gremlin.DateAddArgs -> Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.DateAddArgs
dateAddArgsWithDuration original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
              Core.projectionFieldName = (Core.Name "unit")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "duration"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

dateAddArgsWithUnit :: Phantoms.TypedTerm Gremlin.DateAddArgs -> Phantoms.TypedTerm Gremlin.TraversalDTArgument -> Phantoms.TypedTerm Gremlin.DateAddArgs
dateAddArgsWithUnit original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "duration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
              Core.projectionFieldName = (Core.Name "duration")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dateArgumentValue :: Phantoms.TypedTerm Gremlin.DateLiteral -> Phantoms.TypedTerm Gremlin.DateArgument
dateArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

dateArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.DateArgument
dateArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

dateDiffArgsDate :: Phantoms.TypedTerm Gremlin.DateArgument -> Phantoms.TypedTerm Gremlin.DateDiffArgs
dateDiffArgsDate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateDiffArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "date"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

dateDiffArgsTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.DateDiffArgs
dateDiffArgsTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateDiffArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

dateLiteral :: Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.DateLiteral
dateLiteral x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateLiteral"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

dedupArgsScopeString :: Phantoms.TypedTerm Gremlin.ScopeStringArgument -> Phantoms.TypedTerm Gremlin.DedupArgs
dedupArgsScopeString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DedupArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scopeString"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

dedupArgsString :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.DedupArgs
dedupArgsString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DedupArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

directionAndVarargs :: Phantoms.TypedTerm Gremlin.TraversalDirectionArgument -> Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.DirectionAndVarargs
directionAndVarargs direction varargs =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTypedTerm direction)},
        Core.Field {
          Core.fieldName = (Core.Name "varargs"),
          Core.fieldTerm = (Phantoms.unTypedTerm varargs)}]}))

directionAndVarargsDirection :: Phantoms.TypedTerm Gremlin.DirectionAndVarargs -> Phantoms.TypedTerm Gremlin.TraversalDirectionArgument
directionAndVarargsDirection x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
        Core.projectionFieldName = (Core.Name "direction")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

directionAndVarargsVarargs :: Phantoms.TypedTerm Gremlin.DirectionAndVarargs -> Phantoms.TypedTerm [Gremlin.StringNullableArgument]
directionAndVarargsVarargs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
        Core.projectionFieldName = (Core.Name "varargs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

directionAndVarargsWithDirection :: Phantoms.TypedTerm Gremlin.DirectionAndVarargs -> Phantoms.TypedTerm Gremlin.TraversalDirectionArgument -> Phantoms.TypedTerm Gremlin.DirectionAndVarargs
directionAndVarargsWithDirection original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "varargs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
              Core.projectionFieldName = (Core.Name "varargs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

directionAndVarargsWithVarargs :: Phantoms.TypedTerm Gremlin.DirectionAndVarargs -> Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.DirectionAndVarargs
directionAndVarargsWithVarargs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
              Core.projectionFieldName = (Core.Name "direction")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varargs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

floatArgumentValue :: Phantoms.TypedTerm Gremlin.FloatLiteral -> Phantoms.TypedTerm Gremlin.FloatArgument
floatArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.FloatArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

floatArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.FloatArgument
floatArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.FloatArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

floatLiteral :: Phantoms.TypedTerm Double -> Phantoms.TypedTerm Gremlin.FloatLiteral
floatLiteral x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.FloatLiteral"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

fromArgsString :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.FromArgs
fromArgsString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.FromArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

fromArgsTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.FromArgs
fromArgsTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.FromArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

fromArgsVertex :: Phantoms.TypedTerm Gremlin.StructureVertexArgument -> Phantoms.TypedTerm Gremlin.FromArgs
fromArgsVertex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.FromArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralArgumentAndNestedTraversal :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndNestedTraversal
genericLiteralArgumentAndNestedTraversal object traversal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTypedTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversal)}]}))

genericLiteralArgumentAndNestedTraversalObject :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
genericLiteralArgumentAndNestedTraversalObject x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
        Core.projectionFieldName = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

genericLiteralArgumentAndNestedTraversalTraversal :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal
genericLiteralArgumentAndNestedTraversalTraversal x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
        Core.projectionFieldName = (Core.Name "traversal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

genericLiteralArgumentAndNestedTraversalWithObject :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndNestedTraversal
genericLiteralArgumentAndNestedTraversalWithObject original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
              Core.projectionFieldName = (Core.Name "traversal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

genericLiteralArgumentAndNestedTraversalWithTraversal :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndNestedTraversal
genericLiteralArgumentAndNestedTraversalWithTraversal original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

genericLiteralArgumentAndOptionalTraversalBiFunctionArgument :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm (Maybe Gremlin.TraversalBiFunctionArgument) -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument
genericLiteralArgumentAndOptionalTraversalBiFunctionArgument literal biFunction =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTypedTerm literal)},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Phantoms.unTypedTerm biFunction)}]}))

genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentBiFunction :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument -> Phantoms.TypedTerm (Maybe Gremlin.TraversalBiFunctionArgument)
genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentBiFunction x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
        Core.projectionFieldName = (Core.Name "biFunction")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentLiteral :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
        Core.projectionFieldName = (Core.Name "literal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentWithBiFunction :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument -> Phantoms.TypedTerm (Maybe Gremlin.TraversalBiFunctionArgument) -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument
genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentWithBiFunction original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
              Core.projectionFieldName = (Core.Name "literal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentWithLiteral :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument
genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentWithLiteral original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
              Core.projectionFieldName = (Core.Name "biFunction")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

genericLiteralArgumentAndTraversalBiFunctionArgument :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalBiFunctionArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument
genericLiteralArgumentAndTraversalBiFunctionArgument literal biFunction =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTypedTerm literal)},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Phantoms.unTypedTerm biFunction)}]}))

genericLiteralArgumentAndTraversalBiFunctionArgumentBiFunction :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument -> Phantoms.TypedTerm Gremlin.TraversalBiFunctionArgument
genericLiteralArgumentAndTraversalBiFunctionArgumentBiFunction x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
        Core.projectionFieldName = (Core.Name "biFunction")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

genericLiteralArgumentAndTraversalBiFunctionArgumentLiteral :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
genericLiteralArgumentAndTraversalBiFunctionArgumentLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
        Core.projectionFieldName = (Core.Name "literal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

genericLiteralArgumentAndTraversalBiFunctionArgumentWithBiFunction :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument -> Phantoms.TypedTerm Gremlin.TraversalBiFunctionArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument
genericLiteralArgumentAndTraversalBiFunctionArgumentWithBiFunction original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
              Core.projectionFieldName = (Core.Name "literal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

genericLiteralArgumentAndTraversalBiFunctionArgumentWithLiteral :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument
genericLiteralArgumentAndTraversalBiFunctionArgumentWithLiteral original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
              Core.projectionFieldName = (Core.Name "biFunction")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

genericLiteralArgumentAndTraversalPredicateLiteral :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndTraversalPredicate
genericLiteralArgumentAndTraversalPredicateLiteral x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralArgumentAndTraversalPredicatePredicate :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndTraversalPredicate
genericLiteralArgumentAndTraversalPredicatePredicate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralArgumentValue :: Phantoms.TypedTerm Gremlin.GenericLiteral -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
genericLiteralArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
genericLiteralArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralBoolean :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralBoolean x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralCollection :: Phantoms.TypedTerm [Gremlin.GenericLiteral] -> Phantoms.TypedTerm Gremlin.GenericLiteralCollection
genericLiteralCollection x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralCollection"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

genericLiteralDate :: Phantoms.TypedTerm Gremlin.DateLiteral -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralDate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "date"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralGenericLiteralCollection :: Phantoms.TypedTerm Gremlin.GenericLiteralCollection -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralGenericLiteralCollection x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genericLiteralCollection"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralGenericLiteralMap :: Phantoms.TypedTerm Gremlin.GenericLiteralMap -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralGenericLiteralMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genericLiteralMap"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralGenericLiteralRange :: Phantoms.TypedTerm Gremlin.GenericLiteralRange -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralGenericLiteralRange x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genericLiteralRange"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralGenericLiteralSet :: Phantoms.TypedTerm Gremlin.GenericLiteralSet -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralGenericLiteralSet x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genericLiteralSet"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralInf :: Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralInf =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inf"),
        Core.fieldTerm = Core.TermUnit}}))

genericLiteralList :: Phantoms.TypedTerm [Gremlin.GenericLiteral] -> Phantoms.TypedTerm Gremlin.GenericLiteralList
genericLiteralList x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralList"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

genericLiteralListArgumentValue :: Phantoms.TypedTerm Gremlin.GenericLiteralList -> Phantoms.TypedTerm Gremlin.GenericLiteralListArgument
genericLiteralListArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralListArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralListArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.GenericLiteralListArgument
genericLiteralListArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralListArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralMap :: Phantoms.TypedTerm [Gremlin.MapEntry] -> Phantoms.TypedTerm Gremlin.GenericLiteralMap
genericLiteralMap x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMap"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

genericLiteralMapArgumentValue :: Phantoms.TypedTerm Gremlin.GenericLiteralMap -> Phantoms.TypedTerm Gremlin.GenericLiteralMapArgument
genericLiteralMapArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralMapArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.GenericLiteralMapArgument
genericLiteralMapArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralMapNullableArgumentAndTraversalCardinalityArgument :: Phantoms.TypedTerm Gremlin.TraversalCardinalityArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument
genericLiteralMapNullableArgumentAndTraversalCardinalityArgument cardinality object =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTypedTerm cardinality)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTypedTerm object)}]}))

genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentCardinality :: Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument -> Phantoms.TypedTerm Gremlin.TraversalCardinalityArgument
genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentCardinality x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
        Core.projectionFieldName = (Core.Name "cardinality")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentObject :: Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgument
genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentObject x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
        Core.projectionFieldName = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentWithCardinality :: Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument -> Phantoms.TypedTerm Gremlin.TraversalCardinalityArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument
genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentWithCardinality original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentWithObject :: Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument
genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentWithObject original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
              Core.projectionFieldName = (Core.Name "cardinality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

genericLiteralMapNullableArgumentOrNestedTraversalMap :: Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal
genericLiteralMapNullableArgumentOrNestedTraversalMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralMapNullableArgumentOrNestedTraversalTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal
genericLiteralMapNullableArgumentOrNestedTraversalTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralMapNullableArgumentValue :: Phantoms.TypedTerm (Maybe Gremlin.GenericLiteralMap) -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgument
genericLiteralMapNullableArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralMapNullableArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgument
genericLiteralMapNullableArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralNan :: Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralNan =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nan"),
        Core.fieldTerm = Core.TermUnit}}))

genericLiteralNestedTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralNestedTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedTraversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralNull :: Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralNull =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

genericLiteralNumeric :: Phantoms.TypedTerm Gremlin.NumericLiteral -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralNumeric x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralRangeInteger :: Phantoms.TypedTerm Gremlin.IntegerRange -> Phantoms.TypedTerm Gremlin.GenericLiteralRange
genericLiteralRangeInteger x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralRangeString :: Phantoms.TypedTerm Gremlin.StringRange -> Phantoms.TypedTerm Gremlin.GenericLiteralRange
genericLiteralRangeString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralSet :: Phantoms.TypedTerm [Gremlin.GenericLiteral] -> Phantoms.TypedTerm Gremlin.GenericLiteralSet
genericLiteralSet x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralSet"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

genericLiteralString :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralStructureVertex :: Phantoms.TypedTerm Gremlin.StructureVertex -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralStructureVertex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "structureVertex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralTerminatedTraversal :: Phantoms.TypedTerm Gremlin.TerminatedTraversal -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralTerminatedTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "terminatedTraversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralTraversalCardinality :: Phantoms.TypedTerm Gremlin.TraversalCardinality -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralTraversalCardinality x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalCardinality"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralTraversalDT :: Phantoms.TypedTerm Gremlin.TraversalDT -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralTraversalDT x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalDT"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralTraversalDirection :: Phantoms.TypedTerm Gremlin.TraversalDirection -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralTraversalDirection x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalDirection"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralTraversalMerge :: Phantoms.TypedTerm Gremlin.TraversalMerge -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralTraversalMerge x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalMerge"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralTraversalPick :: Phantoms.TypedTerm Gremlin.TraversalPick -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralTraversalPick x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalPick"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

genericLiteralTraversalToken :: Phantoms.TypedTerm Gremlin.TraversalToken -> Phantoms.TypedTerm Gremlin.GenericLiteral
genericLiteralTraversalToken x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalToken"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasArgsString :: Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm Gremlin.HasArgs
hasArgsString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasArgsTraversalToken :: Phantoms.TypedTerm Gremlin.HasTraversalTokenArgs -> Phantoms.TypedTerm Gremlin.HasArgs
hasArgsTraversalToken x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalToken"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargs :: Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm (Maybe Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest) -> Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs
hasStringArgumentAndOptionalStringLiteralVarargs string rest =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTypedTerm rest)}]}))

hasStringArgumentAndOptionalStringLiteralVarargsRest :: Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm (Maybe Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest)
hasStringArgumentAndOptionalStringLiteralVarargsRest x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
        Core.projectionFieldName = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

hasStringArgumentAndOptionalStringLiteralVarargsRestObject :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest
hasStringArgumentAndOptionalStringLiteralVarargsRestObject x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargsRestPredicate :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest
hasStringArgumentAndOptionalStringLiteralVarargsRestPredicate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargsRestStringObject :: Phantoms.TypedTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest
hasStringArgumentAndOptionalStringLiteralVarargsRestStringObject x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "stringObject"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargsRestStringPredicate :: Phantoms.TypedTerm Gremlin.StringNullableArgumentAndTraversalPredicate -> Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest
hasStringArgumentAndOptionalStringLiteralVarargsRestStringPredicate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "stringPredicate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargsRestTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest
hasStringArgumentAndOptionalStringLiteralVarargsRestTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargsString :: Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm Gremlin.StringNullableArgument
hasStringArgumentAndOptionalStringLiteralVarargsString x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
        Core.projectionFieldName = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

hasStringArgumentAndOptionalStringLiteralVarargsWithRest :: Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm (Maybe Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest) -> Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs
hasStringArgumentAndOptionalStringLiteralVarargsWithRest original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
              Core.projectionFieldName = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

hasStringArgumentAndOptionalStringLiteralVarargsWithString :: Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs
hasStringArgumentAndOptionalStringLiteralVarargsWithString original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
              Core.projectionFieldName = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

hasTraversalTokenArgs :: Phantoms.TypedTerm Gremlin.TraversalTokenArgument -> Phantoms.TypedTerm Gremlin.HasTraversalTokenArgsRest -> Phantoms.TypedTerm Gremlin.HasTraversalTokenArgs
hasTraversalTokenArgs traversalToken rest =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversalToken"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversalToken)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTypedTerm rest)}]}))

hasTraversalTokenArgsRest :: Phantoms.TypedTerm Gremlin.HasTraversalTokenArgs -> Phantoms.TypedTerm Gremlin.HasTraversalTokenArgsRest
hasTraversalTokenArgsRest x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
        Core.projectionFieldName = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

hasTraversalTokenArgsRestLiteral :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.HasTraversalTokenArgsRest
hasTraversalTokenArgsRestLiteral x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasTraversalTokenArgsRestPredicate :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.HasTraversalTokenArgsRest
hasTraversalTokenArgsRestPredicate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasTraversalTokenArgsRestTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.HasTraversalTokenArgsRest
hasTraversalTokenArgsRestTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

hasTraversalTokenArgsTraversalToken :: Phantoms.TypedTerm Gremlin.HasTraversalTokenArgs -> Phantoms.TypedTerm Gremlin.TraversalTokenArgument
hasTraversalTokenArgsTraversalToken x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
        Core.projectionFieldName = (Core.Name "traversalToken")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

hasTraversalTokenArgsWithRest :: Phantoms.TypedTerm Gremlin.HasTraversalTokenArgs -> Phantoms.TypedTerm Gremlin.HasTraversalTokenArgsRest -> Phantoms.TypedTerm Gremlin.HasTraversalTokenArgs
hasTraversalTokenArgsWithRest original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversalToken"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
              Core.projectionFieldName = (Core.Name "traversalToken")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

hasTraversalTokenArgsWithTraversalToken :: Phantoms.TypedTerm Gremlin.HasTraversalTokenArgs -> Phantoms.TypedTerm Gremlin.TraversalTokenArgument -> Phantoms.TypedTerm Gremlin.HasTraversalTokenArgs
hasTraversalTokenArgsWithTraversalToken original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversalToken"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
              Core.projectionFieldName = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

identifier :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Gremlin.Identifier
identifier x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.Identifier"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

integerArgumentValue :: Phantoms.TypedTerm Gremlin.IntegerLiteral -> Phantoms.TypedTerm Gremlin.IntegerArgument
integerArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

integerArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.IntegerArgument
integerArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

integerLiteral :: Phantoms.TypedTerm Integer -> Phantoms.TypedTerm Gremlin.IntegerLiteral
integerLiteral x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerLiteral"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

integerRange :: Phantoms.TypedTerm Gremlin.IntegerLiteral -> Phantoms.TypedTerm Gremlin.IntegerLiteral -> Phantoms.TypedTerm Gremlin.IntegerRange
integerRange left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

integerRangeLeft :: Phantoms.TypedTerm Gremlin.IntegerRange -> Phantoms.TypedTerm Gremlin.IntegerLiteral
integerRangeLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

integerRangeRight :: Phantoms.TypedTerm Gremlin.IntegerRange -> Phantoms.TypedTerm Gremlin.IntegerLiteral
integerRangeRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

integerRangeWithLeft :: Phantoms.TypedTerm Gremlin.IntegerRange -> Phantoms.TypedTerm Gremlin.IntegerLiteral -> Phantoms.TypedTerm Gremlin.IntegerRange
integerRangeWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

integerRangeWithRight :: Phantoms.TypedTerm Gremlin.IntegerRange -> Phantoms.TypedTerm Gremlin.IntegerLiteral -> Phantoms.TypedTerm Gremlin.IntegerRange
integerRangeWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

ioOptionsKeysReader :: Phantoms.TypedTerm Gremlin.IoOptionsKeys
ioOptionsKeysReader =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IoOptionsKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reader"),
        Core.fieldTerm = Core.TermUnit}}))

ioOptionsKeysWriter :: Phantoms.TypedTerm Gremlin.IoOptionsKeys
ioOptionsKeysWriter =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IoOptionsKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "writer"),
        Core.fieldTerm = Core.TermUnit}}))

ioOptionsValuesGraphml :: Phantoms.TypedTerm Gremlin.IoOptionsValues
ioOptionsValuesGraphml =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IoOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "graphml"),
        Core.fieldTerm = Core.TermUnit}}))

ioOptionsValuesGraphson :: Phantoms.TypedTerm Gremlin.IoOptionsValues
ioOptionsValuesGraphson =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IoOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "graphson"),
        Core.fieldTerm = Core.TermUnit}}))

ioOptionsValuesGryo :: Phantoms.TypedTerm Gremlin.IoOptionsValues
ioOptionsValuesGryo =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IoOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gryo"),
        Core.fieldTerm = Core.TermUnit}}))

keywordEdges :: Phantoms.TypedTerm Gremlin.Keyword
keywordEdges =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Keyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edges"),
        Core.fieldTerm = Core.TermUnit}}))

keywordKeys :: Phantoms.TypedTerm Gremlin.Keyword
keywordKeys =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Keyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keys"),
        Core.fieldTerm = Core.TermUnit}}))

keywordNew :: Phantoms.TypedTerm Gremlin.Keyword
keywordNew =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Keyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = Core.TermUnit}}))

keywordOrIdentifierIdentifier :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.KeywordOrIdentifier
keywordOrIdentifierIdentifier x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.KeywordOrIdentifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

keywordOrIdentifierKeyword :: Phantoms.TypedTerm Gremlin.Keyword -> Phantoms.TypedTerm Gremlin.KeywordOrIdentifier
keywordOrIdentifierKeyword x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.KeywordOrIdentifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keyword"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

keywordValues :: Phantoms.TypedTerm Gremlin.Keyword
keywordValues =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Keyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "values"),
        Core.fieldTerm = Core.TermUnit}}))

mapEntryKey :: Phantoms.TypedTerm Gremlin.MapKey -> Phantoms.TypedTerm Gremlin.MapEntry
mapEntryKey x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapEntry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "key"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapEntryValue :: Phantoms.TypedTerm Gremlin.GenericLiteral -> Phantoms.TypedTerm Gremlin.MapEntry
mapEntryValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapEntry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapKeyCollection :: Phantoms.TypedTerm Gremlin.GenericLiteralCollection -> Phantoms.TypedTerm Gremlin.MapKey
mapKeyCollection x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "collection"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapKeyIdentifier :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.MapKey
mapKeyIdentifier x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapKeyKeyword :: Phantoms.TypedTerm Gremlin.Keyword -> Phantoms.TypedTerm Gremlin.MapKey
mapKeyKeyword x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keyword"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapKeyMap :: Phantoms.TypedTerm Gremlin.GenericLiteralMap -> Phantoms.TypedTerm Gremlin.MapKey
mapKeyMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapKeyNumeric :: Phantoms.TypedTerm Gremlin.NumericLiteral -> Phantoms.TypedTerm Gremlin.MapKey
mapKeyNumeric x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapKeySet :: Phantoms.TypedTerm Gremlin.GenericLiteralSet -> Phantoms.TypedTerm Gremlin.MapKey
mapKeySet x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapKeyString :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Gremlin.MapKey
mapKeyString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapKeyTraversalDirection :: Phantoms.TypedTerm Gremlin.TraversalDirection -> Phantoms.TypedTerm Gremlin.MapKey
mapKeyTraversalDirection x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalDirection"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapKeyTraversalToken :: Phantoms.TypedTerm Gremlin.TraversalToken -> Phantoms.TypedTerm Gremlin.MapKey
mapKeyTraversalToken x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalToken"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

nestedTraversalAnonymous :: Phantoms.TypedTerm Gremlin.ChainedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal
nestedTraversalAnonymous x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

nestedTraversalArgument :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TypedTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TypedTerm Gremlin.NestedTraversalArgument
nestedTraversalArgument traversal1 traversal2 traversal3 =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversal1)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversal2)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal3"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversal3)}]}))

nestedTraversalArgumentTraversal1 :: Phantoms.TypedTerm Gremlin.NestedTraversalArgument -> Phantoms.TypedTerm Gremlin.NestedTraversal
nestedTraversalArgumentTraversal1 x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
        Core.projectionFieldName = (Core.Name "traversal1")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nestedTraversalArgumentTraversal2 :: Phantoms.TypedTerm Gremlin.NestedTraversalArgument -> Phantoms.TypedTerm (Maybe Gremlin.NestedTraversal)
nestedTraversalArgumentTraversal2 x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
        Core.projectionFieldName = (Core.Name "traversal2")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nestedTraversalArgumentTraversal3 :: Phantoms.TypedTerm Gremlin.NestedTraversalArgument -> Phantoms.TypedTerm (Maybe Gremlin.NestedTraversal)
nestedTraversalArgumentTraversal3 x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
        Core.projectionFieldName = (Core.Name "traversal3")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nestedTraversalArgumentWithTraversal1 :: Phantoms.TypedTerm Gremlin.NestedTraversalArgument -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversalArgument
nestedTraversalArgumentWithTraversal1 original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionFieldName = (Core.Name "traversal2")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionFieldName = (Core.Name "traversal3")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nestedTraversalArgumentWithTraversal2 :: Phantoms.TypedTerm Gremlin.NestedTraversalArgument -> Phantoms.TypedTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TypedTerm Gremlin.NestedTraversalArgument
nestedTraversalArgumentWithTraversal2 original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionFieldName = (Core.Name "traversal1")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionFieldName = (Core.Name "traversal3")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nestedTraversalArgumentWithTraversal3 :: Phantoms.TypedTerm Gremlin.NestedTraversalArgument -> Phantoms.TypedTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TypedTerm Gremlin.NestedTraversalArgument
nestedTraversalArgumentWithTraversal3 original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionFieldName = (Core.Name "traversal1")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionFieldName = (Core.Name "traversal2")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal3"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

nestedTraversalChained :: Phantoms.TypedTerm Gremlin.ChainedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal
nestedTraversalChained x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "chained"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

nestedTraversalRoot :: Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal
nestedTraversalRoot x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "root"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

numericLiteralFloat :: Phantoms.TypedTerm Gremlin.FloatLiteral -> Phantoms.TypedTerm Gremlin.NumericLiteral
numericLiteralFloat x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

numericLiteralInteger :: Phantoms.TypedTerm Gremlin.IntegerLiteral -> Phantoms.TypedTerm Gremlin.NumericLiteral
numericLiteralInteger x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

optionArgsMergeMap :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TypedTerm Gremlin.OptionArgs
optionArgsMergeMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeMap"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

optionArgsMergeTraversal :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.OptionArgs
optionArgsMergeTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeTraversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

optionArgsObjectTraversal :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.OptionArgs
optionArgsObjectTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectTraversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

optionArgsPredicateTraversal :: Phantoms.TypedTerm Gremlin.TraversalPredicateAndNestedTraversal -> Phantoms.TypedTerm Gremlin.OptionArgs
optionArgsPredicateTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicateTraversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

optionArgsTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.OptionArgs
optionArgsTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

optionalStringArgumentAndNestedTraversal :: Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.OptionalStringArgumentAndNestedTraversal
optionalStringArgumentAndNestedTraversal string traversal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversal)}]}))

optionalStringArgumentAndNestedTraversalString :: Phantoms.TypedTerm Gremlin.OptionalStringArgumentAndNestedTraversal -> Phantoms.TypedTerm (Maybe Gremlin.StringArgument)
optionalStringArgumentAndNestedTraversalString x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
        Core.projectionFieldName = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

optionalStringArgumentAndNestedTraversalTraversal :: Phantoms.TypedTerm Gremlin.OptionalStringArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal
optionalStringArgumentAndNestedTraversalTraversal x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
        Core.projectionFieldName = (Core.Name "traversal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

optionalStringArgumentAndNestedTraversalWithString :: Phantoms.TypedTerm Gremlin.OptionalStringArgumentAndNestedTraversal -> Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.OptionalStringArgumentAndNestedTraversal
optionalStringArgumentAndNestedTraversalWithString original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
              Core.projectionFieldName = (Core.Name "traversal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

optionalStringArgumentAndNestedTraversalWithTraversal :: Phantoms.TypedTerm Gremlin.OptionalStringArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.OptionalStringArgumentAndNestedTraversal
optionalStringArgumentAndNestedTraversalWithTraversal original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
              Core.projectionFieldName = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

optionalTraversalScopeArgumentAndIntegerArgument :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument
optionalTraversalScopeArgumentAndIntegerArgument scope long =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "long"),
          Core.fieldTerm = (Phantoms.unTypedTerm long)}]}))

optionalTraversalScopeArgumentAndIntegerArgumentLong :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TypedTerm Gremlin.IntegerArgument
optionalTraversalScopeArgumentAndIntegerArgumentLong x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
        Core.projectionFieldName = (Core.Name "long")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

optionalTraversalScopeArgumentAndIntegerArgumentScope :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument)
optionalTraversalScopeArgumentAndIntegerArgumentScope x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
        Core.projectionFieldName = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

optionalTraversalScopeArgumentAndIntegerArgumentWithLong :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument
optionalTraversalScopeArgumentAndIntegerArgumentWithLong original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "long"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

optionalTraversalScopeArgumentAndIntegerArgumentWithScope :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument
optionalTraversalScopeArgumentAndIntegerArgumentWithScope original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "long"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
              Core.projectionFieldName = (Core.Name "long")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

optionalTraversalScopeArgumentAndStringArgument :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument
optionalTraversalScopeArgumentAndStringArgument scope string =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm string)}]}))

optionalTraversalScopeArgumentAndStringArgumentScope :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument)
optionalTraversalScopeArgumentAndStringArgumentScope x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
        Core.projectionFieldName = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

optionalTraversalScopeArgumentAndStringArgumentString :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument -> Phantoms.TypedTerm Gremlin.StringArgument
optionalTraversalScopeArgumentAndStringArgumentString x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
        Core.projectionFieldName = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

optionalTraversalScopeArgumentAndStringArgumentWithScope :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument
optionalTraversalScopeArgumentAndStringArgumentWithScope original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
              Core.projectionFieldName = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

optionalTraversalScopeArgumentAndStringArgumentWithString :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument -> Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument
optionalTraversalScopeArgumentAndStringArgumentWithString original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

pageRankConstantsEdges :: Phantoms.TypedTerm Gremlin.PageRankConstants
pageRankConstantsEdges =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PageRankConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edges"),
        Core.fieldTerm = Core.TermUnit}}))

pageRankConstantsPropertyName :: Phantoms.TypedTerm Gremlin.PageRankConstants
pageRankConstantsPropertyName =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PageRankConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyName"),
        Core.fieldTerm = Core.TermUnit}}))

pageRankConstantsTimes :: Phantoms.TypedTerm Gremlin.PageRankConstants
pageRankConstantsTimes =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PageRankConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = Core.TermUnit}}))

peerPressureConstantsEdges :: Phantoms.TypedTerm Gremlin.PeerPressureConstants
peerPressureConstantsEdges =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PeerPressureConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edges"),
        Core.fieldTerm = Core.TermUnit}}))

peerPressureConstantsPropertyName :: Phantoms.TypedTerm Gremlin.PeerPressureConstants
peerPressureConstantsPropertyName =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PeerPressureConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyName"),
        Core.fieldTerm = Core.TermUnit}}))

peerPressureConstantsTimes :: Phantoms.TypedTerm Gremlin.PeerPressureConstants
peerPressureConstantsTimes =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PeerPressureConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = Core.TermUnit}}))

popStringsArgument :: Phantoms.TypedTerm Gremlin.TraversalPopArgument -> Phantoms.TypedTerm [Gremlin.StringArgument] -> Phantoms.TypedTerm Gremlin.PopStringsArgument
popStringsArgument pop string =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Phantoms.unTypedTerm pop)},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm string)}]}))

popStringsArgumentPop :: Phantoms.TypedTerm Gremlin.PopStringsArgument -> Phantoms.TypedTerm Gremlin.TraversalPopArgument
popStringsArgumentPop x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
        Core.projectionFieldName = (Core.Name "pop")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

popStringsArgumentString :: Phantoms.TypedTerm Gremlin.PopStringsArgument -> Phantoms.TypedTerm [Gremlin.StringArgument]
popStringsArgumentString x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
        Core.projectionFieldName = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

popStringsArgumentWithPop :: Phantoms.TypedTerm Gremlin.PopStringsArgument -> Phantoms.TypedTerm Gremlin.TraversalPopArgument -> Phantoms.TypedTerm Gremlin.PopStringsArgument
popStringsArgumentWithPop original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
              Core.projectionFieldName = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

popStringsArgumentWithString :: Phantoms.TypedTerm Gremlin.PopStringsArgument -> Phantoms.TypedTerm [Gremlin.StringArgument] -> Phantoms.TypedTerm Gremlin.PopStringsArgument
popStringsArgumentWithString original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
              Core.projectionFieldName = (Core.Name "pop")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

predicateOrTraversalPredicate :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.PredicateOrTraversal
predicateOrTraversalPredicate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateOrTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

predicateOrTraversalTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.PredicateOrTraversal
predicateOrTraversalTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateOrTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

predicateTraversalArgument :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TypedTerm Gremlin.PredicateTraversalArgument
predicateTraversalArgument predicate traversal1 traversal2 =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversal1)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversal2)}]}))

predicateTraversalArgumentPredicate :: Phantoms.TypedTerm Gremlin.PredicateTraversalArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
predicateTraversalArgumentPredicate x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
        Core.projectionFieldName = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

predicateTraversalArgumentTraversal1 :: Phantoms.TypedTerm Gremlin.PredicateTraversalArgument -> Phantoms.TypedTerm Gremlin.NestedTraversal
predicateTraversalArgumentTraversal1 x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
        Core.projectionFieldName = (Core.Name "traversal1")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

predicateTraversalArgumentTraversal2 :: Phantoms.TypedTerm Gremlin.PredicateTraversalArgument -> Phantoms.TypedTerm (Maybe Gremlin.NestedTraversal)
predicateTraversalArgumentTraversal2 x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
        Core.projectionFieldName = (Core.Name "traversal2")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

predicateTraversalArgumentWithPredicate :: Phantoms.TypedTerm Gremlin.PredicateTraversalArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.PredicateTraversalArgument
predicateTraversalArgumentWithPredicate original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionFieldName = (Core.Name "traversal1")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionFieldName = (Core.Name "traversal2")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

predicateTraversalArgumentWithTraversal1 :: Phantoms.TypedTerm Gremlin.PredicateTraversalArgument -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.PredicateTraversalArgument
predicateTraversalArgumentWithTraversal1 original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionFieldName = (Core.Name "traversal2")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

predicateTraversalArgumentWithTraversal2 :: Phantoms.TypedTerm Gremlin.PredicateTraversalArgument -> Phantoms.TypedTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TypedTerm Gremlin.PredicateTraversalArgument
predicateTraversalArgumentWithTraversal2 original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionFieldName = (Core.Name "traversal1")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertyArgsCardinalityObject :: Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument -> Phantoms.TypedTerm Gremlin.PropertyArgs
propertyArgsCardinalityObject x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PropertyArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cardinalityObject"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

propertyArgsCardinalityObjects :: Phantoms.TypedTerm Gremlin.TraversalCardinalityArgumentAndObjects -> Phantoms.TypedTerm Gremlin.PropertyArgs
propertyArgsCardinalityObjects x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PropertyArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cardinalityObjects"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

propertyArgsObject :: Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TypedTerm Gremlin.PropertyArgs
propertyArgsObject x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PropertyArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

propertyArgsObjects :: Phantoms.TypedTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TypedTerm Gremlin.PropertyArgs
propertyArgsObjects x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PropertyArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objects"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

queryEmpty :: Phantoms.TypedTerm Gremlin.Query
queryEmpty =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))

queryList :: Phantoms.TypedTerm [Gremlin.Query] -> Phantoms.TypedTerm Gremlin.QueryList
queryList x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.QueryList"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

queryRootTraversal :: Phantoms.TypedTerm Gremlin.RootTraversalQuery -> Phantoms.TypedTerm Gremlin.Query
queryRootTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rootTraversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

queryToString :: Phantoms.TypedTerm Gremlin.Query
queryToString =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toString"),
        Core.fieldTerm = Core.TermUnit}}))

queryTraversalSource :: Phantoms.TypedTerm Gremlin.TraversalSourceQuery -> Phantoms.TypedTerm Gremlin.Query
queryTraversalSource x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalSource"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

rangeArgs :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.RangeArgs
rangeArgs scope min max =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTypedTerm min)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTypedTerm max)}]}))

rangeArgsMax :: Phantoms.TypedTerm Gremlin.RangeArgs -> Phantoms.TypedTerm Gremlin.IntegerArgument
rangeArgsMax x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
        Core.projectionFieldName = (Core.Name "max")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rangeArgsMin :: Phantoms.TypedTerm Gremlin.RangeArgs -> Phantoms.TypedTerm Gremlin.IntegerArgument
rangeArgsMin x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
        Core.projectionFieldName = (Core.Name "min")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rangeArgsScope :: Phantoms.TypedTerm Gremlin.RangeArgs -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument)
rangeArgsScope x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
        Core.projectionFieldName = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rangeArgsWithMax :: Phantoms.TypedTerm Gremlin.RangeArgs -> Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.RangeArgs
rangeArgsWithMax original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionFieldName = (Core.Name "min")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

rangeArgsWithMin :: Phantoms.TypedTerm Gremlin.RangeArgs -> Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.RangeArgs
rangeArgsWithMin original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionFieldName = (Core.Name "max")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

rangeArgsWithScope :: Phantoms.TypedTerm Gremlin.RangeArgs -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.RangeArgs
rangeArgsWithScope original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionFieldName = (Core.Name "min")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionFieldName = (Core.Name "max")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

rangeArgument :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.RangeArgument
rangeArgument min max =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTypedTerm min)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTypedTerm max)}]}))

rangeArgumentMax :: Phantoms.TypedTerm Gremlin.RangeArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
rangeArgumentMax x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
        Core.projectionFieldName = (Core.Name "max")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rangeArgumentMin :: Phantoms.TypedTerm Gremlin.RangeArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
rangeArgumentMin x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
        Core.projectionFieldName = (Core.Name "min")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rangeArgumentWithMax :: Phantoms.TypedTerm Gremlin.RangeArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.RangeArgument
rangeArgumentWithMax original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
              Core.projectionFieldName = (Core.Name "min")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

rangeArgumentWithMin :: Phantoms.TypedTerm Gremlin.RangeArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.RangeArgument
rangeArgumentWithMin original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
              Core.projectionFieldName = (Core.Name "max")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

replaceArgs :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.ReplaceArgs
replaceArgs scope from to =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTypedTerm from)},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTypedTerm to)}]}))

replaceArgsFrom :: Phantoms.TypedTerm Gremlin.ReplaceArgs -> Phantoms.TypedTerm Gremlin.StringNullableArgument
replaceArgsFrom x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
        Core.projectionFieldName = (Core.Name "from")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

replaceArgsScope :: Phantoms.TypedTerm Gremlin.ReplaceArgs -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument)
replaceArgsScope x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
        Core.projectionFieldName = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

replaceArgsTo :: Phantoms.TypedTerm Gremlin.ReplaceArgs -> Phantoms.TypedTerm Gremlin.StringNullableArgument
replaceArgsTo x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
        Core.projectionFieldName = (Core.Name "to")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

replaceArgsWithFrom :: Phantoms.TypedTerm Gremlin.ReplaceArgs -> Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.ReplaceArgs
replaceArgsWithFrom original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionFieldName = (Core.Name "to")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

replaceArgsWithScope :: Phantoms.TypedTerm Gremlin.ReplaceArgs -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.ReplaceArgs
replaceArgsWithScope original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionFieldName = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionFieldName = (Core.Name "to")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

replaceArgsWithTo :: Phantoms.TypedTerm Gremlin.ReplaceArgs -> Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.ReplaceArgs
replaceArgsWithTo original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionFieldName = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

rootTraversal :: Phantoms.TypedTerm Gremlin.TraversalSource -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod -> Phantoms.TypedTerm [Gremlin.ChainedTraversalElement] -> Phantoms.TypedTerm Gremlin.RootTraversal
rootTraversal source spawnMethod chained =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTypedTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "spawnMethod"),
          Core.fieldTerm = (Phantoms.unTypedTerm spawnMethod)},
        Core.Field {
          Core.fieldName = (Core.Name "chained"),
          Core.fieldTerm = (Phantoms.unTypedTerm chained)}]}))

rootTraversalChained :: Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm [Gremlin.ChainedTraversalElement]
rootTraversalChained x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
        Core.projectionFieldName = (Core.Name "chained")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rootTraversalQuery :: Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm (Maybe Gremlin.TraversalTerminalMethod) -> Phantoms.TypedTerm Gremlin.RootTraversalQuery
rootTraversalQuery root terminalMethod =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Phantoms.unTypedTerm root)},
        Core.Field {
          Core.fieldName = (Core.Name "terminalMethod"),
          Core.fieldTerm = (Phantoms.unTypedTerm terminalMethod)}]}))

rootTraversalQueryRoot :: Phantoms.TypedTerm Gremlin.RootTraversalQuery -> Phantoms.TypedTerm Gremlin.RootTraversal
rootTraversalQueryRoot x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
        Core.projectionFieldName = (Core.Name "root")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rootTraversalQueryTerminalMethod :: Phantoms.TypedTerm Gremlin.RootTraversalQuery -> Phantoms.TypedTerm (Maybe Gremlin.TraversalTerminalMethod)
rootTraversalQueryTerminalMethod x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
        Core.projectionFieldName = (Core.Name "terminalMethod")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rootTraversalQueryWithRoot :: Phantoms.TypedTerm Gremlin.RootTraversalQuery -> Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm Gremlin.RootTraversalQuery
rootTraversalQueryWithRoot original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "terminalMethod"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
              Core.projectionFieldName = (Core.Name "terminalMethod")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

rootTraversalQueryWithTerminalMethod :: Phantoms.TypedTerm Gremlin.RootTraversalQuery -> Phantoms.TypedTerm (Maybe Gremlin.TraversalTerminalMethod) -> Phantoms.TypedTerm Gremlin.RootTraversalQuery
rootTraversalQueryWithTerminalMethod original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
              Core.projectionFieldName = (Core.Name "root")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "terminalMethod"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

rootTraversalSource :: Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm Gremlin.TraversalSource
rootTraversalSource x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rootTraversalSpawnMethod :: Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
rootTraversalSpawnMethod x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
        Core.projectionFieldName = (Core.Name "spawnMethod")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rootTraversalWithChained :: Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm [Gremlin.ChainedTraversalElement] -> Phantoms.TypedTerm Gremlin.RootTraversal
rootTraversalWithChained original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spawnMethod"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionFieldName = (Core.Name "spawnMethod")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "chained"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

rootTraversalWithSource :: Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm Gremlin.TraversalSource -> Phantoms.TypedTerm Gremlin.RootTraversal
rootTraversalWithSource original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "spawnMethod"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionFieldName = (Core.Name "spawnMethod")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "chained"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionFieldName = (Core.Name "chained")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

rootTraversalWithSpawnMethod :: Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod -> Phantoms.TypedTerm Gremlin.RootTraversal
rootTraversalWithSpawnMethod original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spawnMethod"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "chained"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionFieldName = (Core.Name "chained")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

scopeStringArgument :: Phantoms.TypedTerm Gremlin.TraversalScopeArgument -> Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.ScopeStringArgument
scopeStringArgument scope strings =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "strings"),
          Core.fieldTerm = (Phantoms.unTypedTerm strings)}]}))

scopeStringArgumentScope :: Phantoms.TypedTerm Gremlin.ScopeStringArgument -> Phantoms.TypedTerm Gremlin.TraversalScopeArgument
scopeStringArgumentScope x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
        Core.projectionFieldName = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

scopeStringArgumentStrings :: Phantoms.TypedTerm Gremlin.ScopeStringArgument -> Phantoms.TypedTerm [Gremlin.StringNullableArgument]
scopeStringArgumentStrings x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
        Core.projectionFieldName = (Core.Name "strings")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

scopeStringArgumentWithScope :: Phantoms.TypedTerm Gremlin.ScopeStringArgument -> Phantoms.TypedTerm Gremlin.TraversalScopeArgument -> Phantoms.TypedTerm Gremlin.ScopeStringArgument
scopeStringArgumentWithScope original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "strings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
              Core.projectionFieldName = (Core.Name "strings")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

scopeStringArgumentWithStrings :: Phantoms.TypedTerm Gremlin.ScopeStringArgument -> Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.ScopeStringArgument
scopeStringArgumentWithStrings original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "strings"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

selectArgsColumn :: Phantoms.TypedTerm Gremlin.TraversalColumnArgument -> Phantoms.TypedTerm Gremlin.SelectArgs
selectArgsColumn x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SelectArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "column"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

selectArgsPopStrings :: Phantoms.TypedTerm Gremlin.PopStringsArgument -> Phantoms.TypedTerm Gremlin.SelectArgs
selectArgsPopStrings x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SelectArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "popStrings"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

selectArgsPopTraversal :: Phantoms.TypedTerm Gremlin.TraversalPopArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.SelectArgs
selectArgsPopTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SelectArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "popTraversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

selectArgsStrings :: Phantoms.TypedTerm [Gremlin.StringArgument] -> Phantoms.TypedTerm Gremlin.SelectArgs
selectArgsStrings x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SelectArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strings"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

selectArgsTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.SelectArgs
selectArgsTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SelectArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

serviceArgumentsMap :: Phantoms.TypedTerm (Maybe Gremlin.GenericLiteralMapArgument) -> Phantoms.TypedTerm Gremlin.ServiceArguments
serviceArgumentsMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceArguments"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

serviceArgumentsTraversal :: Phantoms.TypedTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TypedTerm Gremlin.ServiceArguments
serviceArgumentsTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceArguments"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

serviceCall :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.ServiceArguments -> Phantoms.TypedTerm Gremlin.ServiceCall
serviceCall service arguments =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "service"),
          Core.fieldTerm = (Phantoms.unTypedTerm service)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTypedTerm arguments)}]}))

serviceCallArguments :: Phantoms.TypedTerm Gremlin.ServiceCall -> Phantoms.TypedTerm Gremlin.ServiceArguments
serviceCallArguments x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

serviceCallService :: Phantoms.TypedTerm Gremlin.ServiceCall -> Phantoms.TypedTerm Gremlin.StringArgument
serviceCallService x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
        Core.projectionFieldName = (Core.Name "service")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

serviceCallWithArguments :: Phantoms.TypedTerm Gremlin.ServiceCall -> Phantoms.TypedTerm Gremlin.ServiceArguments -> Phantoms.TypedTerm Gremlin.ServiceCall
serviceCallWithArguments original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "service"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
              Core.projectionFieldName = (Core.Name "service")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

serviceCallWithService :: Phantoms.TypedTerm Gremlin.ServiceCall -> Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.ServiceCall
serviceCallWithService original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "service"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

shortestPathConstantsDistance :: Phantoms.TypedTerm Gremlin.ShortestPathConstants
shortestPathConstantsDistance =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "distance"),
        Core.fieldTerm = Core.TermUnit}}))

shortestPathConstantsEdges :: Phantoms.TypedTerm Gremlin.ShortestPathConstants
shortestPathConstantsEdges =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edges"),
        Core.fieldTerm = Core.TermUnit}}))

shortestPathConstantsIncludeEdges :: Phantoms.TypedTerm Gremlin.ShortestPathConstants
shortestPathConstantsIncludeEdges =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "includeEdges"),
        Core.fieldTerm = Core.TermUnit}}))

shortestPathConstantsMaxDistance :: Phantoms.TypedTerm Gremlin.ShortestPathConstants
shortestPathConstantsMaxDistance =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxDistance"),
        Core.fieldTerm = Core.TermUnit}}))

shortestPathConstantsTarget :: Phantoms.TypedTerm Gremlin.ShortestPathConstants
shortestPathConstantsTarget =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "target"),
        Core.fieldTerm = Core.TermUnit}}))

splitArgs :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.SplitArgs
splitArgs scope delimiter =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "delimiter"),
          Core.fieldTerm = (Phantoms.unTypedTerm delimiter)}]}))

splitArgsDelimiter :: Phantoms.TypedTerm Gremlin.SplitArgs -> Phantoms.TypedTerm Gremlin.StringNullableArgument
splitArgsDelimiter x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
        Core.projectionFieldName = (Core.Name "delimiter")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

splitArgsScope :: Phantoms.TypedTerm Gremlin.SplitArgs -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument)
splitArgsScope x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
        Core.projectionFieldName = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

splitArgsWithDelimiter :: Phantoms.TypedTerm Gremlin.SplitArgs -> Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.SplitArgs
splitArgsWithDelimiter original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "delimiter"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

splitArgsWithScope :: Phantoms.TypedTerm Gremlin.SplitArgs -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.SplitArgs
splitArgsWithScope original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "delimiter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
              Core.projectionFieldName = (Core.Name "delimiter")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

stringArgumentAndGenericLiteralArgument :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringArgumentAndGenericLiteralArgument
stringArgumentAndGenericLiteralArgument string literal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTypedTerm literal)}]}))

stringArgumentAndGenericLiteralArgumentLiteral :: Phantoms.TypedTerm Gremlin.StringArgumentAndGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
stringArgumentAndGenericLiteralArgumentLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
        Core.projectionFieldName = (Core.Name "literal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringArgumentAndGenericLiteralArgumentString :: Phantoms.TypedTerm Gremlin.StringArgumentAndGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringArgument
stringArgumentAndGenericLiteralArgumentString x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
        Core.projectionFieldName = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringArgumentAndGenericLiteralArgumentWithLiteral :: Phantoms.TypedTerm Gremlin.StringArgumentAndGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringArgumentAndGenericLiteralArgument
stringArgumentAndGenericLiteralArgumentWithLiteral original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
              Core.projectionFieldName = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

stringArgumentAndGenericLiteralArgumentWithString :: Phantoms.TypedTerm Gremlin.StringArgumentAndGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.StringArgumentAndGenericLiteralArgument
stringArgumentAndGenericLiteralArgumentWithString original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
              Core.projectionFieldName = (Core.Name "literal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

stringArgumentAndOptionalGenericLiteralArgument :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm (Maybe Gremlin.GenericLiteralArgument) -> Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument
stringArgumentAndOptionalGenericLiteralArgument string literal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTypedTerm literal)}]}))

stringArgumentAndOptionalGenericLiteralArgumentLiteral :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument -> Phantoms.TypedTerm (Maybe Gremlin.GenericLiteralArgument)
stringArgumentAndOptionalGenericLiteralArgumentLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
        Core.projectionFieldName = (Core.Name "literal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringArgumentAndOptionalGenericLiteralArgumentString :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringArgument
stringArgumentAndOptionalGenericLiteralArgumentString x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
        Core.projectionFieldName = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringArgumentAndOptionalGenericLiteralArgumentWithLiteral :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument -> Phantoms.TypedTerm (Maybe Gremlin.GenericLiteralArgument) -> Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument
stringArgumentAndOptionalGenericLiteralArgumentWithLiteral original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
              Core.projectionFieldName = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

stringArgumentAndOptionalGenericLiteralArgumentWithString :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument
stringArgumentAndOptionalGenericLiteralArgumentWithString original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
              Core.projectionFieldName = (Core.Name "literal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

stringArgumentAndOptionalStringLiteralVarargs :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs
stringArgumentAndOptionalStringLiteralVarargs first rest =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTypedTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTypedTerm rest)}]}))

stringArgumentAndOptionalStringLiteralVarargsFirst :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm Gremlin.StringArgument
stringArgumentAndOptionalStringLiteralVarargsFirst x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
        Core.projectionFieldName = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringArgumentAndOptionalStringLiteralVarargsRest :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm [Gremlin.StringNullableArgument]
stringArgumentAndOptionalStringLiteralVarargsRest x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
        Core.projectionFieldName = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringArgumentAndOptionalStringLiteralVarargsWithFirst :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs
stringArgumentAndOptionalStringLiteralVarargsWithFirst original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
              Core.projectionFieldName = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

stringArgumentAndOptionalStringLiteralVarargsWithRest :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs
stringArgumentAndOptionalStringLiteralVarargsWithRest original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
              Core.projectionFieldName = (Core.Name "first")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

stringArgumentOrNestedTraversalString :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.StringArgumentOrNestedTraversal
stringArgumentOrNestedTraversalString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stringArgumentOrNestedTraversalTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.StringArgumentOrNestedTraversal
stringArgumentOrNestedTraversalTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stringArgumentValue :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Gremlin.StringArgument
stringArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stringArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.StringArgument
stringArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stringNullableArgumentAndGenericLiteralArgument :: Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument
stringNullableArgumentAndGenericLiteralArgument string literal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTypedTerm literal)}]}))

stringNullableArgumentAndGenericLiteralArgumentLiteral :: Phantoms.TypedTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
stringNullableArgumentAndGenericLiteralArgumentLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
        Core.projectionFieldName = (Core.Name "literal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringNullableArgumentAndGenericLiteralArgumentString :: Phantoms.TypedTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringNullableArgument
stringNullableArgumentAndGenericLiteralArgumentString x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
        Core.projectionFieldName = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringNullableArgumentAndGenericLiteralArgumentWithLiteral :: Phantoms.TypedTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument
stringNullableArgumentAndGenericLiteralArgumentWithLiteral original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
              Core.projectionFieldName = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

stringNullableArgumentAndGenericLiteralArgumentWithString :: Phantoms.TypedTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument
stringNullableArgumentAndGenericLiteralArgumentWithString original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
              Core.projectionFieldName = (Core.Name "literal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

stringNullableArgumentAndTraversalPredicate :: Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.StringNullableArgumentAndTraversalPredicate
stringNullableArgumentAndTraversalPredicate string predicate =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm predicate)}]}))

stringNullableArgumentAndTraversalPredicatePredicate :: Phantoms.TypedTerm Gremlin.StringNullableArgumentAndTraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalPredicate
stringNullableArgumentAndTraversalPredicatePredicate x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
        Core.projectionFieldName = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringNullableArgumentAndTraversalPredicateString :: Phantoms.TypedTerm Gremlin.StringNullableArgumentAndTraversalPredicate -> Phantoms.TypedTerm Gremlin.StringNullableArgument
stringNullableArgumentAndTraversalPredicateString x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
        Core.projectionFieldName = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringNullableArgumentAndTraversalPredicateWithPredicate :: Phantoms.TypedTerm Gremlin.StringNullableArgumentAndTraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.StringNullableArgumentAndTraversalPredicate
stringNullableArgumentAndTraversalPredicateWithPredicate original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
              Core.projectionFieldName = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

stringNullableArgumentAndTraversalPredicateWithString :: Phantoms.TypedTerm Gremlin.StringNullableArgumentAndTraversalPredicate -> Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.StringNullableArgumentAndTraversalPredicate
stringNullableArgumentAndTraversalPredicateWithString original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

stringNullableArgumentValue :: Phantoms.TypedTerm (Maybe String) -> Phantoms.TypedTerm Gremlin.StringNullableArgument
stringNullableArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stringNullableArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.StringNullableArgument
stringNullableArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stringRange :: Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Gremlin.StringRange
stringRange left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

stringRangeLeft :: Phantoms.TypedTerm Gremlin.StringRange -> Phantoms.TypedTerm String
stringRangeLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringRangeRight :: Phantoms.TypedTerm Gremlin.StringRange -> Phantoms.TypedTerm String
stringRangeRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringRangeWithLeft :: Phantoms.TypedTerm Gremlin.StringRange -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Gremlin.StringRange
stringRangeWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

stringRangeWithRight :: Phantoms.TypedTerm Gremlin.StringRange -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Gremlin.StringRange
stringRangeWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

structureVertex :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.StructureVertex
structureVertex new id label =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Phantoms.unTypedTerm new)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)}]}))

structureVertexArgumentValue :: Phantoms.TypedTerm Gremlin.StructureVertex -> Phantoms.TypedTerm Gremlin.StructureVertexArgument
structureVertexArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertexArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

structureVertexArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.StructureVertexArgument
structureVertexArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertexArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

structureVertexId :: Phantoms.TypedTerm Gremlin.StructureVertex -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument
structureVertexId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

structureVertexLabel :: Phantoms.TypedTerm Gremlin.StructureVertex -> Phantoms.TypedTerm Gremlin.StringArgument
structureVertexLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

structureVertexNew :: Phantoms.TypedTerm Gremlin.StructureVertex -> Phantoms.TypedTerm Bool
structureVertexNew x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
        Core.projectionFieldName = (Core.Name "new")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

structureVertexWithId :: Phantoms.TypedTerm Gremlin.StructureVertex -> Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.StructureVertex
structureVertexWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionFieldName = (Core.Name "new")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

structureVertexWithLabel :: Phantoms.TypedTerm Gremlin.StructureVertex -> Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.StructureVertex
structureVertexWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionFieldName = (Core.Name "new")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

structureVertexWithNew :: Phantoms.TypedTerm Gremlin.StructureVertex -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Gremlin.StructureVertex
structureVertexWithNew original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

substringArgs :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm (Maybe Gremlin.IntegerArgument) -> Phantoms.TypedTerm Gremlin.SubstringArgs
substringArgs scope start end =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTypedTerm start)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTypedTerm end)}]}))

substringArgsEnd :: Phantoms.TypedTerm Gremlin.SubstringArgs -> Phantoms.TypedTerm (Maybe Gremlin.IntegerArgument)
substringArgsEnd x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
        Core.projectionFieldName = (Core.Name "end")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

substringArgsScope :: Phantoms.TypedTerm Gremlin.SubstringArgs -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument)
substringArgsScope x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
        Core.projectionFieldName = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

substringArgsStart :: Phantoms.TypedTerm Gremlin.SubstringArgs -> Phantoms.TypedTerm Gremlin.IntegerArgument
substringArgsStart x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
        Core.projectionFieldName = (Core.Name "start")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

substringArgsWithEnd :: Phantoms.TypedTerm Gremlin.SubstringArgs -> Phantoms.TypedTerm (Maybe Gremlin.IntegerArgument) -> Phantoms.TypedTerm Gremlin.SubstringArgs
substringArgsWithEnd original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionFieldName = (Core.Name "start")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

substringArgsWithScope :: Phantoms.TypedTerm Gremlin.SubstringArgs -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.SubstringArgs
substringArgsWithScope original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionFieldName = (Core.Name "start")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionFieldName = (Core.Name "end")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

substringArgsWithStart :: Phantoms.TypedTerm Gremlin.SubstringArgs -> Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.SubstringArgs
substringArgsWithStart original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionFieldName = (Core.Name "end")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

tailArgs :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm (Maybe Gremlin.IntegerArgument) -> Phantoms.TypedTerm Gremlin.TailArgs
tailArgs scope integer =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "integer"),
          Core.fieldTerm = (Phantoms.unTypedTerm integer)}]}))

tailArgsInteger :: Phantoms.TypedTerm Gremlin.TailArgs -> Phantoms.TypedTerm (Maybe Gremlin.IntegerArgument)
tailArgsInteger x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
        Core.projectionFieldName = (Core.Name "integer")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

tailArgsScope :: Phantoms.TypedTerm Gremlin.TailArgs -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument)
tailArgsScope x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
        Core.projectionFieldName = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

tailArgsWithInteger :: Phantoms.TypedTerm Gremlin.TailArgs -> Phantoms.TypedTerm (Maybe Gremlin.IntegerArgument) -> Phantoms.TypedTerm Gremlin.TailArgs
tailArgsWithInteger original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integer"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

tailArgsWithScope :: Phantoms.TypedTerm Gremlin.TailArgs -> Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TailArgs
tailArgsWithScope original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "integer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
              Core.projectionFieldName = (Core.Name "integer")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

terminatedTraversal :: Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm Gremlin.TraversalTerminalMethod -> Phantoms.TypedTerm Gremlin.TerminatedTraversal
terminatedTraversal root terminal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Phantoms.unTypedTerm root)},
        Core.Field {
          Core.fieldName = (Core.Name "terminal"),
          Core.fieldTerm = (Phantoms.unTypedTerm terminal)}]}))

terminatedTraversalRoot :: Phantoms.TypedTerm Gremlin.TerminatedTraversal -> Phantoms.TypedTerm Gremlin.RootTraversal
terminatedTraversalRoot x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
        Core.projectionFieldName = (Core.Name "root")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

terminatedTraversalTerminal :: Phantoms.TypedTerm Gremlin.TerminatedTraversal -> Phantoms.TypedTerm Gremlin.TraversalTerminalMethod
terminatedTraversalTerminal x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
        Core.projectionFieldName = (Core.Name "terminal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

terminatedTraversalWithRoot :: Phantoms.TypedTerm Gremlin.TerminatedTraversal -> Phantoms.TypedTerm Gremlin.RootTraversal -> Phantoms.TypedTerm Gremlin.TerminatedTraversal
terminatedTraversalWithRoot original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "terminal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
              Core.projectionFieldName = (Core.Name "terminal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

terminatedTraversalWithTerminal :: Phantoms.TypedTerm Gremlin.TerminatedTraversal -> Phantoms.TypedTerm Gremlin.TraversalTerminalMethod -> Phantoms.TypedTerm Gremlin.TerminatedTraversal
terminatedTraversalWithTerminal original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
              Core.projectionFieldName = (Core.Name "root")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "terminal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

toArgsDirection :: Phantoms.TypedTerm Gremlin.DirectionAndVarargs -> Phantoms.TypedTerm Gremlin.ToArgs
toArgsDirection x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ToArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "direction"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

toArgsString :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.ToArgs
toArgsString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ToArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

toArgsTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.ToArgs
toArgsTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ToArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

toArgsVertex :: Phantoms.TypedTerm Gremlin.StructureVertexArgument -> Phantoms.TypedTerm Gremlin.ToArgs
toArgsVertex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ToArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

transactionPartBegin :: Phantoms.TypedTerm Gremlin.TransactionPart
transactionPartBegin =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TransactionPart"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "begin"),
        Core.fieldTerm = Core.TermUnit}}))

transactionPartCommit :: Phantoms.TypedTerm Gremlin.TransactionPart
transactionPartCommit =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TransactionPart"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "commit"),
        Core.fieldTerm = Core.TermUnit}}))

transactionPartRollback :: Phantoms.TypedTerm Gremlin.TransactionPart
transactionPartRollback =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TransactionPart"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rollback"),
        Core.fieldTerm = Core.TermUnit}}))

traversalBiFunctionArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalOperator -> Phantoms.TypedTerm Gremlin.TraversalBiFunctionArgument
traversalBiFunctionArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalBiFunctionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalBiFunctionArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalBiFunctionArgument
traversalBiFunctionArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalBiFunctionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalCardinalityArgumentAndObjects :: Phantoms.TypedTerm Gremlin.TraversalCardinalityArgument -> Phantoms.TypedTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TypedTerm Gremlin.TraversalCardinalityArgumentAndObjects
traversalCardinalityArgumentAndObjects cardinality objects =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTypedTerm cardinality)},
        Core.Field {
          Core.fieldName = (Core.Name "objects"),
          Core.fieldTerm = (Phantoms.unTypedTerm objects)}]}))

traversalCardinalityArgumentAndObjectsCardinality :: Phantoms.TypedTerm Gremlin.TraversalCardinalityArgumentAndObjects -> Phantoms.TypedTerm Gremlin.TraversalCardinalityArgument
traversalCardinalityArgumentAndObjectsCardinality x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
        Core.projectionFieldName = (Core.Name "cardinality")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalCardinalityArgumentAndObjectsObjects :: Phantoms.TypedTerm Gremlin.TraversalCardinalityArgumentAndObjects -> Phantoms.TypedTerm [Gremlin.GenericLiteralArgument]
traversalCardinalityArgumentAndObjectsObjects x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
        Core.projectionFieldName = (Core.Name "objects")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalCardinalityArgumentAndObjectsWithCardinality :: Phantoms.TypedTerm Gremlin.TraversalCardinalityArgumentAndObjects -> Phantoms.TypedTerm Gremlin.TraversalCardinalityArgument -> Phantoms.TypedTerm Gremlin.TraversalCardinalityArgumentAndObjects
traversalCardinalityArgumentAndObjectsWithCardinality original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "objects"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
              Core.projectionFieldName = (Core.Name "objects")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

traversalCardinalityArgumentAndObjectsWithObjects :: Phantoms.TypedTerm Gremlin.TraversalCardinalityArgumentAndObjects -> Phantoms.TypedTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TypedTerm Gremlin.TraversalCardinalityArgumentAndObjects
traversalCardinalityArgumentAndObjectsWithObjects original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
              Core.projectionFieldName = (Core.Name "cardinality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "objects"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

traversalCardinalityArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalCardinality -> Phantoms.TypedTerm Gremlin.TraversalCardinalityArgument
traversalCardinalityArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalCardinalityArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalCardinalityArgument
traversalCardinalityArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalCardinalityList :: Phantoms.TypedTerm Gremlin.GenericLiteral -> Phantoms.TypedTerm Gremlin.TraversalCardinality
traversalCardinalityList x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalCardinalitySet :: Phantoms.TypedTerm Gremlin.GenericLiteral -> Phantoms.TypedTerm Gremlin.TraversalCardinality
traversalCardinalitySet x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalCardinalitySingle :: Phantoms.TypedTerm Gremlin.GenericLiteral -> Phantoms.TypedTerm Gremlin.TraversalCardinality
traversalCardinalitySingle x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalColumnArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalColumn -> Phantoms.TypedTerm Gremlin.TraversalColumnArgument
traversalColumnArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalColumnArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalColumnArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalColumnArgument
traversalColumnArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalColumnArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalColumnKeys :: Phantoms.TypedTerm Gremlin.TraversalColumn
traversalColumnKeys =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalColumn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keys"),
        Core.fieldTerm = Core.TermUnit}}))

traversalColumnValues :: Phantoms.TypedTerm Gremlin.TraversalColumn
traversalColumnValues =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalColumn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "values"),
        Core.fieldTerm = Core.TermUnit}}))

traversalComparatorArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalOrder -> Phantoms.TypedTerm Gremlin.TraversalComparatorArgument
traversalComparatorArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalComparatorArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalComparatorArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalComparatorArgument
traversalComparatorArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalComparatorArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalDTArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalDT -> Phantoms.TypedTerm Gremlin.TraversalDTArgument
traversalDTArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDTArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalDTArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalDTArgument
traversalDTArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDTArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalDTDay :: Phantoms.TypedTerm Gremlin.TraversalDT
traversalDTDay =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDT"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "day"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDTHour :: Phantoms.TypedTerm Gremlin.TraversalDT
traversalDTHour =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDT"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hour"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDTMinute :: Phantoms.TypedTerm Gremlin.TraversalDT
traversalDTMinute =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDT"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minute"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDTSecond :: Phantoms.TypedTerm Gremlin.TraversalDT
traversalDTSecond =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDT"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "second"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDirectionArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalDirection -> Phantoms.TypedTerm Gremlin.TraversalDirectionArgument
traversalDirectionArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDirectionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalDirectionArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalDirectionArgument
traversalDirectionArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDirectionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalDirectionBoth :: Phantoms.TypedTerm Gremlin.TraversalDirection
traversalDirectionBoth =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDirectionIn :: Phantoms.TypedTerm Gremlin.TraversalDirection
traversalDirectionIn =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDirectionOut :: Phantoms.TypedTerm Gremlin.TraversalDirection
traversalDirectionOut =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "out"),
        Core.fieldTerm = Core.TermUnit}}))

traversalFunctionArgumentOrStringArgumentOrNestedTraversalFunction :: Phantoms.TypedTerm Gremlin.TraversalFunctionArgument -> Phantoms.TypedTerm Gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal
traversalFunctionArgumentOrStringArgumentOrNestedTraversalFunction x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalFunctionArgumentOrStringArgumentOrNestedTraversalString :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal
traversalFunctionArgumentOrStringArgumentOrNestedTraversalString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalFunctionArgumentOrStringArgumentOrNestedTraversalTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal
traversalFunctionArgumentOrStringArgumentOrNestedTraversalTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalFunctionArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalFunction -> Phantoms.TypedTerm Gremlin.TraversalFunctionArgument
traversalFunctionArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalFunctionArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalFunctionArgument
traversalFunctionArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalFunctionColumn :: Phantoms.TypedTerm Gremlin.TraversalColumn -> Phantoms.TypedTerm Gremlin.TraversalFunction
traversalFunctionColumn x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "column"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalFunctionToken :: Phantoms.TypedTerm Gremlin.TraversalToken -> Phantoms.TypedTerm Gremlin.TraversalFunction
traversalFunctionToken x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "token"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMergeArgumentAndGenericLiteralMapNullableArgument :: Phantoms.TypedTerm Gremlin.TraversalMergeArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TypedTerm (Maybe Gremlin.TraversalCardinality) -> Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgument merge map cardinality =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Phantoms.unTypedTerm merge)},
        Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = (Phantoms.unTypedTerm map)},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTypedTerm cardinality)}]}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentCardinality :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TypedTerm (Maybe Gremlin.TraversalCardinality)
traversalMergeArgumentAndGenericLiteralMapNullableArgumentCardinality x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
        Core.projectionFieldName = (Core.Name "cardinality")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentMap :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgumentMap x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
        Core.projectionFieldName = (Core.Name "map")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentMerge :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TypedTerm Gremlin.TraversalMergeArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgumentMerge x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
        Core.projectionFieldName = (Core.Name "merge")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithCardinality :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TypedTerm (Maybe Gremlin.TraversalCardinality) -> Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithCardinality original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionFieldName = (Core.Name "merge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionFieldName = (Core.Name "map")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithMap :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithMap original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionFieldName = (Core.Name "merge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionFieldName = (Core.Name "cardinality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithMerge :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TypedTerm Gremlin.TraversalMergeArgument -> Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithMerge original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionFieldName = (Core.Name "map")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionFieldName = (Core.Name "cardinality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

traversalMergeArgumentAndNestedTraversal :: Phantoms.TypedTerm Gremlin.TraversalMergeArgument -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndNestedTraversal
traversalMergeArgumentAndNestedTraversal merge traversal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Phantoms.unTypedTerm merge)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversal)}]}))

traversalMergeArgumentAndNestedTraversalMerge :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMergeArgument
traversalMergeArgumentAndNestedTraversalMerge x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
        Core.projectionFieldName = (Core.Name "merge")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalMergeArgumentAndNestedTraversalTraversal :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal
traversalMergeArgumentAndNestedTraversalTraversal x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
        Core.projectionFieldName = (Core.Name "traversal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalMergeArgumentAndNestedTraversalWithMerge :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMergeArgument -> Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndNestedTraversal
traversalMergeArgumentAndNestedTraversalWithMerge original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
              Core.projectionFieldName = (Core.Name "traversal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

traversalMergeArgumentAndNestedTraversalWithTraversal :: Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMergeArgumentAndNestedTraversal
traversalMergeArgumentAndNestedTraversalWithTraversal original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
              Core.projectionFieldName = (Core.Name "merge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

traversalMergeArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalMerge -> Phantoms.TypedTerm Gremlin.TraversalMergeArgument
traversalMergeArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMergeArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalMergeArgument
traversalMergeArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMergeInV :: Phantoms.TypedTerm Gremlin.TraversalMerge
traversalMergeInV =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMerge"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMergeOnCreate :: Phantoms.TypedTerm Gremlin.TraversalMerge
traversalMergeOnCreate =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMerge"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "onCreate"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMergeOnMatch :: Phantoms.TypedTerm Gremlin.TraversalMerge
traversalMergeOnMatch =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMerge"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "onMatch"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMergeOutV :: Phantoms.TypedTerm Gremlin.TraversalMerge
traversalMergeOutV =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMerge"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodAddE :: Phantoms.TypedTerm Gremlin.StringArgumentOrNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodAddE x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addE"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodAddV :: Phantoms.TypedTerm (Maybe Gremlin.StringArgumentOrNestedTraversal) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodAddV x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addV"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodAggregate :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodAggregate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "aggregate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodAll :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodAll x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodAnd :: Phantoms.TypedTerm [Gremlin.NestedTraversal] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodAnd x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodAny :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodAny x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodAs :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodAs x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodAsDate :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodAsDate =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "asDate"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodAsString :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodAsString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "asString"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodBarrier :: Phantoms.TypedTerm (Maybe Gremlin.TraversalSackMethodArgumentOrIntegerArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodBarrier x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "barrier"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodBoth :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodBoth x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodBothE :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodBothE x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bothE"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodBothV :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodBothV =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bothV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodBranch :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodBranch x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "branch"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodBy :: Phantoms.TypedTerm Gremlin.ByArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodBy x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "by"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodCall :: Phantoms.TypedTerm Gremlin.ServiceCall -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodCall x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodCap :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodCap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cap"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodChoose :: Phantoms.TypedTerm Gremlin.ChooseArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodChoose x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "choose"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodCoalesce :: Phantoms.TypedTerm [Gremlin.NestedTraversal] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodCoalesce x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "coalesce"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodCoin :: Phantoms.TypedTerm Gremlin.FloatArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodCoin x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "coin"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodCombine :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodCombine x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "combine"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodConcat :: Phantoms.TypedTerm Gremlin.ConcatArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodConcat x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "concat"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodConjoin :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodConjoin x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conjoin"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodConnectedComponent :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodConnectedComponent =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "connectedComponent"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodConstant :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodConstant x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodCount :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodCount x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "count"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodCyclicPath :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodCyclicPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cyclicPath"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodDateAdd :: Phantoms.TypedTerm Gremlin.DateAddArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodDateAdd x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dateAdd"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodDateDiff :: Phantoms.TypedTerm Gremlin.DateDiffArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodDateDiff x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dateDiff"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodDedup :: Phantoms.TypedTerm Gremlin.DedupArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodDedup x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dedup"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodDifference :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodDifference x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "difference"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodDisjunct :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodDisjunct x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjunct"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodDrop :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodDrop =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "drop"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodE :: Phantoms.TypedTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodE x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "e"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodElement :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodElement x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "element"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodElementMap :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodElementMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elementMap"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodEmit :: Phantoms.TypedTerm (Maybe Gremlin.PredicateOrTraversal) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodEmit x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emit"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodFail :: Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodFail x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fail"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodFilter :: Phantoms.TypedTerm Gremlin.PredicateOrTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodFilter x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "filter"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodFlatMap :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodFlatMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "flatMap"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodFold :: Phantoms.TypedTerm (Maybe Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodFold x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fold"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodFormat :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodFormat x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "format"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodFrom :: Phantoms.TypedTerm Gremlin.FromArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodFrom x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "from"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodGroup :: Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodGroup x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodGroupCount :: Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodGroupCount x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "groupCount"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodHas :: Phantoms.TypedTerm Gremlin.HasArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodHas x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "has"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodHasId :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndTraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodHasId x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasId"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodHasKey :: Phantoms.TypedTerm Gremlin.TraversalPredicateOrStringLiteralVarargs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodHasKey x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasKey"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodHasLabel :: Phantoms.TypedTerm Gremlin.TraversalPredicateOrStringLiteralVarargs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodHasLabel x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasLabel"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodHasNot :: Phantoms.TypedTerm Gremlin.StringNullableArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodHasNot x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasNot"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodHasValue :: Phantoms.TypedTerm Gremlin.TraversalPredicateOrGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodHasValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasValue"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodId :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodId =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "id"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodIdentity :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodIdentity =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identity"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodIn :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodIn x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodInE :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodInE x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inE"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodInV :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodInV =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodIndex :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodIndex =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "index"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodInject :: Phantoms.TypedTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodInject x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inject"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodIntersect :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodIntersect x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "intersect"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodIs :: Phantoms.TypedTerm Gremlin.TraversalPredicateOrGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodIs x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "is"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodKey :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodKey =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "key"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodLTrim :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodLTrim x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lTrim"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodLabel :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodLabel =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "label"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodLength :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodLength x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "length"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodLimit :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodLimit x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limit"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodLocal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodLocal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodLoops :: Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodLoops x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "loops"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodMap :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodMatch :: Phantoms.TypedTerm [Gremlin.NestedTraversal] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodMatch x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodMath :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodMath x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "math"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodMax :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodMax x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "max"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodMean :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodMean x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mean"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodMerge :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodMerge x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "merge"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodMergeE :: Phantoms.TypedTerm (Maybe Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodMergeE x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeE"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodMergeV :: Phantoms.TypedTerm (Maybe Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodMergeV x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeV"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodMin :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodMin x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "min"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodNone :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodNone x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodNot :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodNot x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodOption :: Phantoms.TypedTerm Gremlin.OptionArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodOption x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "option"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodOptional :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodOptional x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodOr :: Phantoms.TypedTerm [Gremlin.NestedTraversal] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodOr x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodOrder :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodOrder x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "order"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodOtherV :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodOtherV =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "otherV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodOut :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodOut x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "out"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodOutE :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodOutE x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outE"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodOutV :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodOutV =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodPageRank :: Phantoms.TypedTerm (Maybe Gremlin.FloatArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodPageRank x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pageRank"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodPath :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "path"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodPeerPressure :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodPeerPressure =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "peerPressure"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodProduct :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodProduct x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "product"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodProfile :: Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodProfile x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "profile"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodProject :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodProject x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodProperties :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodProperties x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "properties"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodProperty :: Phantoms.TypedTerm Gremlin.PropertyArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodPropertyMap :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodPropertyMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyMap"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodRTrim :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodRTrim x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rTrim"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodRange :: Phantoms.TypedTerm Gremlin.RangeArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodRange x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodRead :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodRead =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "read"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodRepeat :: Phantoms.TypedTerm Gremlin.OptionalStringArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodRepeat x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeat"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodReplace :: Phantoms.TypedTerm Gremlin.ReplaceArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodReplace x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "replace"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodReverse :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodReverse =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reverse"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodSack :: Phantoms.TypedTerm (Maybe Gremlin.TraversalBiFunctionArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodSack x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sack"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodSample :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodSample x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sample"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodSelect :: Phantoms.TypedTerm Gremlin.SelectArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodSelect x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodShortestPath :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodShortestPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shortestPath"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodSideEffect :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodSideEffect x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sideEffect"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodSimplePath :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodSimplePath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simplePath"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodSkip :: Phantoms.TypedTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodSkip x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "skip"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodSplit :: Phantoms.TypedTerm Gremlin.SplitArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodSplit x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "split"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodStore :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodStore x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "store"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodSubgraph :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodSubgraph x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subgraph"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodSubstring :: Phantoms.TypedTerm Gremlin.SubstringArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodSubstring x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "substring"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodSum :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodSum x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sum"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodTail :: Phantoms.TypedTerm (Maybe Gremlin.TailArgs) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodTail x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tail"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodTimes :: Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodTimes x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodTo :: Phantoms.TypedTerm Gremlin.ToArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodTo x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "to"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodToE :: Phantoms.TypedTerm Gremlin.DirectionAndVarargs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodToE x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toE"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodToLower :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodToLower x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toLower"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodToUpper :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodToUpper x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toUpper"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodToV :: Phantoms.TypedTerm Gremlin.TraversalDirectionArgument -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodToV x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toV"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodTree :: Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodTree x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tree"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodTrim :: Phantoms.TypedTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodTrim x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trim"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodUnfold :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodUnfold =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unfold"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodUnion :: Phantoms.TypedTerm [Gremlin.NestedTraversal] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodUnion x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodUntil :: Phantoms.TypedTerm Gremlin.PredicateOrTraversal -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodUntil x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "until"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodV :: Phantoms.TypedTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodV x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "v"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodValue :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodValue =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodValueMap :: Phantoms.TypedTerm Gremlin.ValueMapArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodValueMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "valueMap"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodValues :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodValues x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "values"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodWhere :: Phantoms.TypedTerm Gremlin.WhereArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodWhere x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "where"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodWith :: Phantoms.TypedTerm Gremlin.WithArgs -> Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodWith x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalMethodWrite :: Phantoms.TypedTerm Gremlin.TraversalMethod
traversalMethodWrite =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "write"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorAddAll :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorAddAll =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addAll"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorAnd :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorAnd =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorAssign :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorAssign =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorDiv :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorDiv =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "div"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorMax :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorMax =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "max"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorMin :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorMin =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "min"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorMinus :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorMinus =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorMult :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorMult =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mult"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorOr :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorOr =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorSum :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorSum =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sum"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorSumLong :: Phantoms.TypedTerm Gremlin.TraversalOperator
traversalOperatorSumLong =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sumLong"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalOrder -> Phantoms.TypedTerm Gremlin.TraversalOrderArgument
traversalOrderArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrderArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalOrderArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalOrderArgument
traversalOrderArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrderArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalOrderAsc :: Phantoms.TypedTerm Gremlin.TraversalOrder
traversalOrderAsc =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "asc"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderDecr :: Phantoms.TypedTerm Gremlin.TraversalOrder
traversalOrderDecr =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decr"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderDesc :: Phantoms.TypedTerm Gremlin.TraversalOrder
traversalOrderDesc =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "desc"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderIncr :: Phantoms.TypedTerm Gremlin.TraversalOrder
traversalOrderIncr =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "incr"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderShuffle :: Phantoms.TypedTerm Gremlin.TraversalOrder
traversalOrderShuffle =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shuffle"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPickAny :: Phantoms.TypedTerm Gremlin.TraversalPick
traversalPickAny =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPick"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPickNone :: Phantoms.TypedTerm Gremlin.TraversalPick
traversalPickNone =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPick"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPopAll :: Phantoms.TypedTerm Gremlin.TraversalPop
traversalPopAll =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPop"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPopArgumentAndNestedTraversal :: Phantoms.TypedTerm Gremlin.TraversalPopArgument -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalPopArgumentAndNestedTraversal
traversalPopArgumentAndNestedTraversal pop traversal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Phantoms.unTypedTerm pop)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversal)}]}))

traversalPopArgumentAndNestedTraversalPop :: Phantoms.TypedTerm Gremlin.TraversalPopArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalPopArgument
traversalPopArgumentAndNestedTraversalPop x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
        Core.projectionFieldName = (Core.Name "pop")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalPopArgumentAndNestedTraversalTraversal :: Phantoms.TypedTerm Gremlin.TraversalPopArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal
traversalPopArgumentAndNestedTraversalTraversal x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
        Core.projectionFieldName = (Core.Name "traversal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalPopArgumentAndNestedTraversalWithPop :: Phantoms.TypedTerm Gremlin.TraversalPopArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalPopArgument -> Phantoms.TypedTerm Gremlin.TraversalPopArgumentAndNestedTraversal
traversalPopArgumentAndNestedTraversalWithPop original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
              Core.projectionFieldName = (Core.Name "traversal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

traversalPopArgumentAndNestedTraversalWithTraversal :: Phantoms.TypedTerm Gremlin.TraversalPopArgumentAndNestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalPopArgumentAndNestedTraversal
traversalPopArgumentAndNestedTraversalWithTraversal original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
              Core.projectionFieldName = (Core.Name "pop")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

traversalPopArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalPop -> Phantoms.TypedTerm Gremlin.TraversalPopArgument
traversalPopArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPopArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalPopArgument
traversalPopArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPopFirst :: Phantoms.TypedTerm Gremlin.TraversalPop
traversalPopFirst =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPop"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "first"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPopLast :: Phantoms.TypedTerm Gremlin.TraversalPop
traversalPopLast =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPop"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "last"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPopMixed :: Phantoms.TypedTerm Gremlin.TraversalPop
traversalPopMixed =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPop"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mixed"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPredicateAnd :: Phantoms.TypedTerm Gremlin.TwoTraversalPredicates -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateAnd x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateAndNestedTraversal :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalPredicateAndNestedTraversal
traversalPredicateAndNestedTraversal predicate traversal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTypedTerm traversal)}]}))

traversalPredicateAndNestedTraversalPredicate :: Phantoms.TypedTerm Gremlin.TraversalPredicateAndNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateAndNestedTraversalPredicate x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
        Core.projectionFieldName = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalPredicateAndNestedTraversalTraversal :: Phantoms.TypedTerm Gremlin.TraversalPredicateAndNestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal
traversalPredicateAndNestedTraversalTraversal x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
        Core.projectionFieldName = (Core.Name "traversal")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalPredicateAndNestedTraversalWithPredicate :: Phantoms.TypedTerm Gremlin.TraversalPredicateAndNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalPredicateAndNestedTraversal
traversalPredicateAndNestedTraversalWithPredicate original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
              Core.projectionFieldName = (Core.Name "traversal")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

traversalPredicateAndNestedTraversalWithTraversal :: Phantoms.TypedTerm Gremlin.TraversalPredicateAndNestedTraversal -> Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalPredicateAndNestedTraversal
traversalPredicateAndNestedTraversalWithTraversal original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

traversalPredicateBetween :: Phantoms.TypedTerm Gremlin.RangeArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateBetween x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "between"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateContaining :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateContaining x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "containing"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateEndingWith :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateEndingWith x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endingWith"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateEq :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateEq x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eq"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateGt :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateGt x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gt"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateGte :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateGte x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gte"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateInside :: Phantoms.TypedTerm Gremlin.RangeArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateInside x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inside"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateLt :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateLt x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lt"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateLte :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateLte x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lte"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateNegate :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateNegate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateNeq :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateNeq x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "neq"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateNot :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateNot x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateNotContaining :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateNotContaining x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notContaining"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateNotEndingWith :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateNotEndingWith x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEndingWith"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateNotRegex :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateNotRegex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notRegex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateNotStartingWith :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateNotStartingWith x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notStartingWith"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateOr :: Phantoms.TypedTerm Gremlin.TwoTraversalPredicates -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateOr x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateOrGenericLiteralArgumentLiteral :: Phantoms.TypedTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TypedTerm Gremlin.TraversalPredicateOrGenericLiteralArgument
traversalPredicateOrGenericLiteralArgumentLiteral x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateOrGenericLiteralArgumentPredicate :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalPredicateOrGenericLiteralArgument
traversalPredicateOrGenericLiteralArgumentPredicate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateOrStringLiteralVarargsPredicate :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalPredicateOrStringLiteralVarargs
traversalPredicateOrStringLiteralVarargsPredicate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateOrStringLiteralVarargsString :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.TraversalPredicateOrStringLiteralVarargs
traversalPredicateOrStringLiteralVarargsString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateOutside :: Phantoms.TypedTerm Gremlin.RangeArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateOutside x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outside"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateRegex :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateRegex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateStartingWith :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateStartingWith x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "startingWith"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateWithin :: Phantoms.TypedTerm (Maybe Gremlin.GenericLiteralArgument) -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateWithin x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "within"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalPredicateWithout :: Phantoms.TypedTerm (Maybe Gremlin.GenericLiteralArgument) -> Phantoms.TypedTerm Gremlin.TraversalPredicate
traversalPredicateWithout x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "without"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSackMethodArgumentOrIntegerArgumentConsumer :: Phantoms.TypedTerm Gremlin.TraversalSackMethodArgument -> Phantoms.TypedTerm Gremlin.TraversalSackMethodArgumentOrIntegerArgument
traversalSackMethodArgumentOrIntegerArgumentConsumer x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "consumer"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSackMethodArgumentOrIntegerArgumentInt :: Phantoms.TypedTerm Gremlin.IntegerArgument -> Phantoms.TypedTerm Gremlin.TraversalSackMethodArgumentOrIntegerArgument
traversalSackMethodArgumentOrIntegerArgumentInt x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSackMethodArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalSackMethodArgument
traversalSackMethodArgumentValue =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSackMethodArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = Core.TermUnit}}))

traversalSackMethodArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalSackMethodArgument
traversalSackMethodArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSackMethodArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalScopeArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalScope -> Phantoms.TypedTerm Gremlin.TraversalScopeArgument
traversalScopeArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalScopeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalScopeArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalScopeArgument
traversalScopeArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalScopeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalScopeGlobal :: Phantoms.TypedTerm Gremlin.TraversalScope
traversalScopeGlobal =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalScope"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = Core.TermUnit}}))

traversalScopeLocal :: Phantoms.TypedTerm Gremlin.TraversalScope
traversalScopeLocal =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalScope"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = Core.TermUnit}}))

traversalSelfMethodDiscard :: Phantoms.TypedTerm Gremlin.TraversalSelfMethod
traversalSelfMethodDiscard =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "discard"),
        Core.fieldTerm = Core.TermUnit}}))

traversalSource :: Phantoms.TypedTerm [Gremlin.TraversalSourceSelfMethod] -> Phantoms.TypedTerm Gremlin.TraversalSource
traversalSource x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSource"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

traversalSourceQuery :: Phantoms.TypedTerm Gremlin.TraversalSource -> Phantoms.TypedTerm (Maybe Gremlin.TransactionPart) -> Phantoms.TypedTerm Gremlin.TraversalSourceQuery
traversalSourceQuery source transactionPart =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTypedTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "transactionPart"),
          Core.fieldTerm = (Phantoms.unTypedTerm transactionPart)}]}))

traversalSourceQuerySource :: Phantoms.TypedTerm Gremlin.TraversalSourceQuery -> Phantoms.TypedTerm Gremlin.TraversalSource
traversalSourceQuerySource x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalSourceQueryTransactionPart :: Phantoms.TypedTerm Gremlin.TraversalSourceQuery -> Phantoms.TypedTerm (Maybe Gremlin.TransactionPart)
traversalSourceQueryTransactionPart x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
        Core.projectionFieldName = (Core.Name "transactionPart")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalSourceQueryWithSource :: Phantoms.TypedTerm Gremlin.TraversalSourceQuery -> Phantoms.TypedTerm Gremlin.TraversalSource -> Phantoms.TypedTerm Gremlin.TraversalSourceQuery
traversalSourceQueryWithSource original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "transactionPart"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
              Core.projectionFieldName = (Core.Name "transactionPart")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

traversalSourceQueryWithTransactionPart :: Phantoms.TypedTerm Gremlin.TraversalSourceQuery -> Phantoms.TypedTerm (Maybe Gremlin.TransactionPart) -> Phantoms.TypedTerm Gremlin.TraversalSourceQuery
traversalSourceQueryWithTransactionPart original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "transactionPart"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

traversalSourceSelfMethodWith :: Phantoms.TypedTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWith x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSelfMethodWithBulk :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithBulk x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withBulk"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSelfMethodWithPath :: Phantoms.TypedTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withPath"),
        Core.fieldTerm = Core.TermUnit}}))

traversalSourceSelfMethodWithSack :: Phantoms.TypedTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument -> Phantoms.TypedTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithSack x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withSack"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSelfMethodWithSideEffect :: Phantoms.TypedTerm Gremlin.StringArgumentAndGenericLiteralArgument -> Phantoms.TypedTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithSideEffect x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withSideEffect"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSelfMethodWithStrategies :: Phantoms.TypedTerm [Gremlin.TraversalStrategy] -> Phantoms.TypedTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithStrategies x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withStrategies"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSelfMethodWithoutStrategies :: Phantoms.TypedTerm [Gremlin.Identifier] -> Phantoms.TypedTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithoutStrategies x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutStrategies"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSpawnMethodAddE :: Phantoms.TypedTerm Gremlin.StringArgumentOrNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodAddE x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addE"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSpawnMethodAddV :: Phantoms.TypedTerm (Maybe Gremlin.StringArgumentOrNestedTraversal) -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodAddV x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addV"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSpawnMethodCall :: Phantoms.TypedTerm (Maybe Gremlin.ServiceCall) -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodCall x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSpawnMethodE :: Phantoms.TypedTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodE x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "e"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSpawnMethodInject :: Phantoms.TypedTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodInject x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inject"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSpawnMethodIo :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodIo x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "io"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSpawnMethodMergeE :: Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodMergeE x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeE"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSpawnMethodMergeV :: Phantoms.TypedTerm Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodMergeV x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeV"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSpawnMethodUnion :: Phantoms.TypedTerm [Gremlin.NestedTraversal] -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodUnion x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalSourceSpawnMethodV :: Phantoms.TypedTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TypedTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodV x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "v"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalStrategy :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm [Gremlin.Configuration] -> Phantoms.TypedTerm Gremlin.TraversalStrategy
traversalStrategy new class_ configurations =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Phantoms.unTypedTerm new)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTypedTerm class_)},
        Core.Field {
          Core.fieldName = (Core.Name "configurations"),
          Core.fieldTerm = (Phantoms.unTypedTerm configurations)}]}))

traversalStrategyClass :: Phantoms.TypedTerm Gremlin.TraversalStrategy -> Phantoms.TypedTerm Gremlin.Identifier
traversalStrategyClass x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
        Core.projectionFieldName = (Core.Name "class")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalStrategyConfigurations :: Phantoms.TypedTerm Gremlin.TraversalStrategy -> Phantoms.TypedTerm [Gremlin.Configuration]
traversalStrategyConfigurations x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
        Core.projectionFieldName = (Core.Name "configurations")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalStrategyNew :: Phantoms.TypedTerm Gremlin.TraversalStrategy -> Phantoms.TypedTerm Bool
traversalStrategyNew x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
        Core.projectionFieldName = (Core.Name "new")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

traversalStrategyWithClass :: Phantoms.TypedTerm Gremlin.TraversalStrategy -> Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalStrategy
traversalStrategyWithClass original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionFieldName = (Core.Name "new")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "configurations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionFieldName = (Core.Name "configurations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

traversalStrategyWithConfigurations :: Phantoms.TypedTerm Gremlin.TraversalStrategy -> Phantoms.TypedTerm [Gremlin.Configuration] -> Phantoms.TypedTerm Gremlin.TraversalStrategy
traversalStrategyWithConfigurations original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionFieldName = (Core.Name "new")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionFieldName = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "configurations"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

traversalStrategyWithNew :: Phantoms.TypedTerm Gremlin.TraversalStrategy -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Gremlin.TraversalStrategy
traversalStrategyWithNew original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionFieldName = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "configurations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionFieldName = (Core.Name "configurations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

traversalTerminalMethodExplain :: Phantoms.TypedTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodExplain =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explain"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodHasNext :: Phantoms.TypedTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodHasNext =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasNext"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodIterate :: Phantoms.TypedTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodIterate =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iterate"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodNext :: Phantoms.TypedTerm (Maybe Gremlin.IntegerLiteral) -> Phantoms.TypedTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodNext x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "next"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalTerminalMethodToBulkSet :: Phantoms.TypedTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodToBulkSet =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toBulkSet"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodToList :: Phantoms.TypedTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodToList =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toList"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodToSet :: Phantoms.TypedTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodToSet =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toSet"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodTryNext :: Phantoms.TypedTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodTryNext =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tryNext"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTokenArgumentValue :: Phantoms.TypedTerm Gremlin.TraversalToken -> Phantoms.TypedTerm Gremlin.TraversalTokenArgument
traversalTokenArgumentValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTokenArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalTokenArgumentVariable :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm Gremlin.TraversalTokenArgument
traversalTokenArgumentVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTokenArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

traversalTokenId :: Phantoms.TypedTerm Gremlin.TraversalToken
traversalTokenId =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalToken"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "id"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTokenKey :: Phantoms.TypedTerm Gremlin.TraversalToken
traversalTokenKey =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalToken"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "key"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTokenLabel :: Phantoms.TypedTerm Gremlin.TraversalToken
traversalTokenLabel =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalToken"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "label"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTokenValue :: Phantoms.TypedTerm Gremlin.TraversalToken
traversalTokenValue =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalToken"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = Core.TermUnit}}))

twoTraversalPredicates :: Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TwoTraversalPredicates
twoTraversalPredicates left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

twoTraversalPredicatesLeft :: Phantoms.TypedTerm Gremlin.TwoTraversalPredicates -> Phantoms.TypedTerm Gremlin.TraversalPredicate
twoTraversalPredicatesLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

twoTraversalPredicatesRight :: Phantoms.TypedTerm Gremlin.TwoTraversalPredicates -> Phantoms.TypedTerm Gremlin.TraversalPredicate
twoTraversalPredicatesRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

twoTraversalPredicatesWithLeft :: Phantoms.TypedTerm Gremlin.TwoTraversalPredicates -> Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TwoTraversalPredicates
twoTraversalPredicatesWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

twoTraversalPredicatesWithRight :: Phantoms.TypedTerm Gremlin.TwoTraversalPredicates -> Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.TwoTraversalPredicates
twoTraversalPredicatesWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

unDateLiteral :: Phantoms.TypedTerm Gremlin.DateLiteral -> Phantoms.TypedTerm (Maybe Gremlin.StringArgument)
unDateLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.DateLiteral")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unFloatLiteral :: Phantoms.TypedTerm Gremlin.FloatLiteral -> Phantoms.TypedTerm Double
unFloatLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.FloatLiteral")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unGenericLiteralCollection :: Phantoms.TypedTerm Gremlin.GenericLiteralCollection -> Phantoms.TypedTerm [Gremlin.GenericLiteral]
unGenericLiteralCollection x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralCollection")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unGenericLiteralList :: Phantoms.TypedTerm Gremlin.GenericLiteralList -> Phantoms.TypedTerm [Gremlin.GenericLiteral]
unGenericLiteralList x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralList")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unGenericLiteralMap :: Phantoms.TypedTerm Gremlin.GenericLiteralMap -> Phantoms.TypedTerm [Gremlin.MapEntry]
unGenericLiteralMap x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMap")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unGenericLiteralSet :: Phantoms.TypedTerm Gremlin.GenericLiteralSet -> Phantoms.TypedTerm [Gremlin.GenericLiteral]
unGenericLiteralSet x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralSet")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unIdentifier :: Phantoms.TypedTerm Gremlin.Identifier -> Phantoms.TypedTerm String
unIdentifier x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.Identifier")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unIntegerLiteral :: Phantoms.TypedTerm Gremlin.IntegerLiteral -> Phantoms.TypedTerm Integer
unIntegerLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.IntegerLiteral")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unQueryList :: Phantoms.TypedTerm Gremlin.QueryList -> Phantoms.TypedTerm [Gremlin.Query]
unQueryList x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.QueryList")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unTraversalSource :: Phantoms.TypedTerm Gremlin.TraversalSource -> Phantoms.TypedTerm [Gremlin.TraversalSourceSelfMethod]
unTraversalSource x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.TraversalSource")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

valueMapArgsBoolean :: Phantoms.TypedTerm Gremlin.ValueMapBooleanArgs -> Phantoms.TypedTerm Gremlin.ValueMapArgs
valueMapArgsBoolean x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueMapArgsString :: Phantoms.TypedTerm [Gremlin.StringNullableArgument] -> Phantoms.TypedTerm Gremlin.ValueMapArgs
valueMapArgsString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueMapBooleanArgs :: Phantoms.TypedTerm Gremlin.BooleanArgument -> Phantoms.TypedTerm (Maybe [Gremlin.StringNullableArgument]) -> Phantoms.TypedTerm Gremlin.ValueMapBooleanArgs
valueMapBooleanArgs value keys =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTypedTerm keys)}]}))

valueMapBooleanArgsKeys :: Phantoms.TypedTerm Gremlin.ValueMapBooleanArgs -> Phantoms.TypedTerm (Maybe [Gremlin.StringNullableArgument])
valueMapBooleanArgsKeys x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
        Core.projectionFieldName = (Core.Name "keys")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

valueMapBooleanArgsValue :: Phantoms.TypedTerm Gremlin.ValueMapBooleanArgs -> Phantoms.TypedTerm Gremlin.BooleanArgument
valueMapBooleanArgsValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

valueMapBooleanArgsWithKeys :: Phantoms.TypedTerm Gremlin.ValueMapBooleanArgs -> Phantoms.TypedTerm (Maybe [Gremlin.StringNullableArgument]) -> Phantoms.TypedTerm Gremlin.ValueMapBooleanArgs
valueMapBooleanArgsWithKeys original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

valueMapBooleanArgsWithValue :: Phantoms.TypedTerm Gremlin.ValueMapBooleanArgs -> Phantoms.TypedTerm Gremlin.BooleanArgument -> Phantoms.TypedTerm Gremlin.ValueMapBooleanArgs
valueMapBooleanArgsWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
              Core.projectionFieldName = (Core.Name "keys")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

whereArgsPredicate :: Phantoms.TypedTerm Gremlin.WhereWithPredicateArgs -> Phantoms.TypedTerm Gremlin.WhereArgs
whereArgsPredicate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

whereArgsString :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.WhereArgs
whereArgsString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

whereArgsTraversal :: Phantoms.TypedTerm Gremlin.NestedTraversal -> Phantoms.TypedTerm Gremlin.WhereArgs
whereArgsTraversal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

whereWithPredicateArgs :: Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.WhereWithPredicateArgs
whereWithPredicateArgs leftArg predicate =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArg"),
          Core.fieldTerm = (Phantoms.unTypedTerm leftArg)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm predicate)}]}))

whereWithPredicateArgsLeftArg :: Phantoms.TypedTerm Gremlin.WhereWithPredicateArgs -> Phantoms.TypedTerm (Maybe Gremlin.StringArgument)
whereWithPredicateArgsLeftArg x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
        Core.projectionFieldName = (Core.Name "leftArg")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

whereWithPredicateArgsPredicate :: Phantoms.TypedTerm Gremlin.WhereWithPredicateArgs -> Phantoms.TypedTerm Gremlin.TraversalPredicate
whereWithPredicateArgsPredicate x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
        Core.projectionFieldName = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

whereWithPredicateArgsWithLeftArg :: Phantoms.TypedTerm Gremlin.WhereWithPredicateArgs -> Phantoms.TypedTerm (Maybe Gremlin.StringArgument) -> Phantoms.TypedTerm Gremlin.WhereWithPredicateArgs
whereWithPredicateArgsWithLeftArg original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArg"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

whereWithPredicateArgsWithPredicate :: Phantoms.TypedTerm Gremlin.WhereWithPredicateArgs -> Phantoms.TypedTerm Gremlin.TraversalPredicate -> Phantoms.TypedTerm Gremlin.WhereWithPredicateArgs
whereWithPredicateArgsWithPredicate original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
              Core.projectionFieldName = (Core.Name "leftArg")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

withArgs :: Phantoms.TypedTerm Gremlin.WithArgsKeys -> Phantoms.TypedTerm (Maybe Gremlin.WithArgsValues) -> Phantoms.TypedTerm Gremlin.WithArgs
withArgs keys values =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTypedTerm keys)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTypedTerm values)}]}))

withArgsKeys :: Phantoms.TypedTerm Gremlin.WithArgs -> Phantoms.TypedTerm Gremlin.WithArgsKeys
withArgsKeys x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
        Core.projectionFieldName = (Core.Name "keys")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

withArgsKeysString :: Phantoms.TypedTerm Gremlin.StringArgument -> Phantoms.TypedTerm Gremlin.WithArgsKeys
withArgsKeysString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgsKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

withArgsKeysWithOption :: Phantoms.TypedTerm Gremlin.WithOptionKeys -> Phantoms.TypedTerm Gremlin.WithArgsKeys
withArgsKeysWithOption x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgsKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withOption"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

withArgsValues :: Phantoms.TypedTerm Gremlin.WithArgs -> Phantoms.TypedTerm (Maybe Gremlin.WithArgsValues)
withArgsValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
        Core.projectionFieldName = (Core.Name "values")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

withArgsValuesIo :: Phantoms.TypedTerm Gremlin.IoOptionsValues -> Phantoms.TypedTerm Gremlin.WithArgsValues
withArgsValuesIo x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "io"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

withArgsValuesObject :: Phantoms.TypedTerm Gremlin.GenericLiteralArgument -> Phantoms.TypedTerm Gremlin.WithArgsValues
withArgsValuesObject x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

withArgsValuesWithOptions :: Phantoms.TypedTerm Gremlin.WithOptionsValues -> Phantoms.TypedTerm Gremlin.WithArgsValues
withArgsValuesWithOptions x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withOptions"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

withArgsWithKeys :: Phantoms.TypedTerm Gremlin.WithArgs -> Phantoms.TypedTerm Gremlin.WithArgsKeys -> Phantoms.TypedTerm Gremlin.WithArgs
withArgsWithKeys original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

withArgsWithValues :: Phantoms.TypedTerm Gremlin.WithArgs -> Phantoms.TypedTerm (Maybe Gremlin.WithArgsValues) -> Phantoms.TypedTerm Gremlin.WithArgs
withArgsWithValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
              Core.projectionFieldName = (Core.Name "keys")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

withOptionKeysConnectedComponent :: Phantoms.TypedTerm Gremlin.ConnectedComponentConstants -> Phantoms.TypedTerm Gremlin.WithOptionKeys
withOptionKeysConnectedComponent x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "connectedComponent"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

withOptionKeysIo :: Phantoms.TypedTerm Gremlin.IoOptionsKeys -> Phantoms.TypedTerm Gremlin.WithOptionKeys
withOptionKeysIo x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "io"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

withOptionKeysPageRank :: Phantoms.TypedTerm Gremlin.PageRankConstants -> Phantoms.TypedTerm Gremlin.WithOptionKeys
withOptionKeysPageRank x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pageRank"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

withOptionKeysPeerPressure :: Phantoms.TypedTerm Gremlin.PeerPressureConstants -> Phantoms.TypedTerm Gremlin.WithOptionKeys
withOptionKeysPeerPressure x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "peerPressure"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

withOptionKeysShortestPath :: Phantoms.TypedTerm Gremlin.ShortestPathConstants -> Phantoms.TypedTerm Gremlin.WithOptionKeys
withOptionKeysShortestPath x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shortestPath"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

withOptionKeysWithOptionsIndexer :: Phantoms.TypedTerm Gremlin.WithOptionKeys
withOptionKeysWithOptionsIndexer =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withOptionsIndexer"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionKeysWithOptionsTokens :: Phantoms.TypedTerm Gremlin.WithOptionKeys
withOptionKeysWithOptionsTokens =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withOptionsTokens"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesAll :: Phantoms.TypedTerm Gremlin.WithOptionsValues
withOptionsValuesAll =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesIds :: Phantoms.TypedTerm Gremlin.WithOptionsValues
withOptionsValuesIds =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ids"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesKeys :: Phantoms.TypedTerm Gremlin.WithOptionsValues
withOptionsValuesKeys =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keys"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesLabels :: Phantoms.TypedTerm Gremlin.WithOptionsValues
withOptionsValuesLabels =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labels"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesList :: Phantoms.TypedTerm Gremlin.WithOptionsValues
withOptionsValuesList =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesMap :: Phantoms.TypedTerm Gremlin.WithOptionsValues
withOptionsValuesMap =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesNone :: Phantoms.TypedTerm Gremlin.WithOptionsValues
withOptionsValuesNone =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesTokens :: Phantoms.TypedTerm Gremlin.WithOptionsValues
withOptionsValuesTokens =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tokens"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesValues :: Phantoms.TypedTerm Gremlin.WithOptionsValues
withOptionsValuesValues =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "values"),
        Core.fieldTerm = Core.TermUnit}}))
