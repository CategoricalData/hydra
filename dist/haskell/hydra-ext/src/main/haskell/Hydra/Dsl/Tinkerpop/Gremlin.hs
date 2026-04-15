-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.tinkerpop.gremlin

module Hydra.Dsl.Tinkerpop.Gremlin where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Tinkerpop.Gremlin as Gremlin
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

booleanArgumentValue :: Phantoms.TTerm Bool -> Phantoms.TTerm Gremlin.BooleanArgument
booleanArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.BooleanArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

booleanArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.BooleanArgument
booleanArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.BooleanArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

byArgsOrder :: Phantoms.TTerm Gremlin.TraversalOrderArgument -> Phantoms.TTerm Gremlin.ByArgs
byArgsOrder x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ByArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "order"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

byArgsOther :: Phantoms.TTerm Gremlin.ByOtherArgs -> Phantoms.TTerm Gremlin.ByArgs
byArgsOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ByArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

byArgsToken :: Phantoms.TTerm Gremlin.TraversalTokenArgument -> Phantoms.TTerm Gremlin.ByArgs
byArgsToken x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ByArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "token"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

byOtherArgsComparator :: Phantoms.TTerm (Maybe Gremlin.TraversalComparatorArgument) -> Phantoms.TTerm Gremlin.ByOtherArgs
byOtherArgsComparator x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ByOtherArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "comparator"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

byOtherArgsOther :: Phantoms.TTerm (Maybe Gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal) -> Phantoms.TTerm Gremlin.ByOtherArgs
byOtherArgsOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ByOtherArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

chainedTraversal :: Phantoms.TTerm Gremlin.TraversalMethod -> Phantoms.TTerm Gremlin.ChainedTraversalElement -> Phantoms.TTerm Gremlin.ChainedTraversal
chainedTraversal first rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

chainedTraversalElementMethod :: Phantoms.TTerm Gremlin.TraversalMethod -> Phantoms.TTerm Gremlin.ChainedTraversalElement
chainedTraversalElementMethod x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversalElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

chainedTraversalElementSelf :: Phantoms.TTerm Gremlin.TraversalSelfMethod -> Phantoms.TTerm Gremlin.ChainedTraversalElement
chainedTraversalElementSelf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversalElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "self"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

chainedTraversalFirst :: Phantoms.TTerm Gremlin.ChainedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
chainedTraversalFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
        Core.projectionField = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

chainedTraversalRest :: Phantoms.TTerm Gremlin.ChainedTraversal -> Phantoms.TTerm Gremlin.ChainedTraversalElement
chainedTraversalRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
        Core.projectionField = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

chainedTraversalWithFirst :: Phantoms.TTerm Gremlin.ChainedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod -> Phantoms.TTerm Gremlin.ChainedTraversal
chainedTraversalWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
              Core.projectionField = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

chainedTraversalWithRest :: Phantoms.TTerm Gremlin.ChainedTraversal -> Phantoms.TTerm Gremlin.ChainedTraversalElement -> Phantoms.TTerm Gremlin.ChainedTraversal
chainedTraversalWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"),
              Core.projectionField = (Core.Name "first")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

chooseArgsFunction :: Phantoms.TTerm Gremlin.TraversalFunctionArgument -> Phantoms.TTerm Gremlin.ChooseArgs
chooseArgsFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChooseArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

chooseArgsPredicateTraversal :: Phantoms.TTerm Gremlin.PredicateTraversalArgument -> Phantoms.TTerm Gremlin.ChooseArgs
chooseArgsPredicateTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChooseArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicateTraversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

chooseArgsTraversal :: Phantoms.TTerm Gremlin.NestedTraversalArgument -> Phantoms.TTerm Gremlin.ChooseArgs
chooseArgsTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ChooseArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

concatArgsString :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.ConcatArgs
concatArgsString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ConcatArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

concatArgsTraversal :: Phantoms.TTerm [Gremlin.NestedTraversal] -> Phantoms.TTerm Gremlin.ConcatArgs
concatArgsTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ConcatArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

configuration :: Phantoms.TTerm Gremlin.KeywordOrIdentifier -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.Configuration
configuration key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

configurationKey :: Phantoms.TTerm Gremlin.Configuration -> Phantoms.TTerm Gremlin.KeywordOrIdentifier
configurationKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
        Core.projectionField = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

configurationValue :: Phantoms.TTerm Gremlin.Configuration -> Phantoms.TTerm Gremlin.GenericLiteralArgument
configurationValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

configurationWithKey :: Phantoms.TTerm Gremlin.Configuration -> Phantoms.TTerm Gremlin.KeywordOrIdentifier -> Phantoms.TTerm Gremlin.Configuration
configurationWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

configurationWithValue :: Phantoms.TTerm Gremlin.Configuration -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.Configuration
configurationWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Configuration"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

connectedComponentConstantsComponent :: Phantoms.TTerm Gremlin.ConnectedComponentConstants
connectedComponentConstantsComponent =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ConnectedComponentConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "component"),
        Core.fieldTerm = Core.TermUnit}}))

connectedComponentConstantsEdges :: Phantoms.TTerm Gremlin.ConnectedComponentConstants
connectedComponentConstantsEdges =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ConnectedComponentConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edges"),
        Core.fieldTerm = Core.TermUnit}}))

connectedComponentConstantsPropertyName :: Phantoms.TTerm Gremlin.ConnectedComponentConstants
connectedComponentConstantsPropertyName =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ConnectedComponentConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyName"),
        Core.fieldTerm = Core.TermUnit}}))

dateAddArgs :: Phantoms.TTerm Gremlin.TraversalDTArgument -> Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.DateAddArgs
dateAddArgs unit duration =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Phantoms.unTTerm unit)},
        Core.Field {
          Core.fieldName = (Core.Name "duration"),
          Core.fieldTerm = (Phantoms.unTTerm duration)}]}))

dateAddArgsDuration :: Phantoms.TTerm Gremlin.DateAddArgs -> Phantoms.TTerm Gremlin.IntegerArgument
dateAddArgsDuration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
        Core.projectionField = (Core.Name "duration")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dateAddArgsUnit :: Phantoms.TTerm Gremlin.DateAddArgs -> Phantoms.TTerm Gremlin.TraversalDTArgument
dateAddArgsUnit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
        Core.projectionField = (Core.Name "unit")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dateAddArgsWithDuration :: Phantoms.TTerm Gremlin.DateAddArgs -> Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.DateAddArgs
dateAddArgsWithDuration original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
              Core.projectionField = (Core.Name "unit")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "duration"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dateAddArgsWithUnit :: Phantoms.TTerm Gremlin.DateAddArgs -> Phantoms.TTerm Gremlin.TraversalDTArgument -> Phantoms.TTerm Gremlin.DateAddArgs
dateAddArgsWithUnit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "duration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"),
              Core.projectionField = (Core.Name "duration")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dateArgumentValue :: Phantoms.TTerm Gremlin.DateLiteral -> Phantoms.TTerm Gremlin.DateArgument
dateArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dateArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.DateArgument
dateArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dateDiffArgsDate :: Phantoms.TTerm Gremlin.DateArgument -> Phantoms.TTerm Gremlin.DateDiffArgs
dateDiffArgsDate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateDiffArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "date"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dateDiffArgsTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.DateDiffArgs
dateDiffArgsTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateDiffArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dateLiteral :: Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.DateLiteral
dateLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.DateLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dedupArgsScopeString :: Phantoms.TTerm Gremlin.ScopeStringArgument -> Phantoms.TTerm Gremlin.DedupArgs
dedupArgsScopeString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DedupArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scopeString"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dedupArgsString :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.DedupArgs
dedupArgsString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DedupArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

directionAndVarargs :: Phantoms.TTerm Gremlin.TraversalDirectionArgument -> Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.DirectionAndVarargs
directionAndVarargs direction varargs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTTerm direction)},
        Core.Field {
          Core.fieldName = (Core.Name "varargs"),
          Core.fieldTerm = (Phantoms.unTTerm varargs)}]}))

directionAndVarargsDirection :: Phantoms.TTerm Gremlin.DirectionAndVarargs -> Phantoms.TTerm Gremlin.TraversalDirectionArgument
directionAndVarargsDirection x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
        Core.projectionField = (Core.Name "direction")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directionAndVarargsVarargs :: Phantoms.TTerm Gremlin.DirectionAndVarargs -> Phantoms.TTerm [Gremlin.StringNullableArgument]
directionAndVarargsVarargs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
        Core.projectionField = (Core.Name "varargs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directionAndVarargsWithDirection :: Phantoms.TTerm Gremlin.DirectionAndVarargs -> Phantoms.TTerm Gremlin.TraversalDirectionArgument -> Phantoms.TTerm Gremlin.DirectionAndVarargs
directionAndVarargsWithDirection original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "varargs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
              Core.projectionField = (Core.Name "varargs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

directionAndVarargsWithVarargs :: Phantoms.TTerm Gremlin.DirectionAndVarargs -> Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.DirectionAndVarargs
directionAndVarargsWithVarargs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"),
              Core.projectionField = (Core.Name "direction")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varargs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

floatArgumentValue :: Phantoms.TTerm Gremlin.FloatLiteral -> Phantoms.TTerm Gremlin.FloatArgument
floatArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.FloatArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

floatArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.FloatArgument
floatArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.FloatArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

floatLiteral :: Phantoms.TTerm Double -> Phantoms.TTerm Gremlin.FloatLiteral
floatLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.FloatLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

fromArgsString :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.FromArgs
fromArgsString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.FromArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fromArgsTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.FromArgs
fromArgsTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.FromArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fromArgsVertex :: Phantoms.TTerm Gremlin.StructureVertexArgument -> Phantoms.TTerm Gremlin.FromArgs
fromArgsVertex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.FromArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralArgumentAndNestedTraversal :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndNestedTraversal
genericLiteralArgumentAndNestedTraversal object traversal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTTerm traversal)}]}))

genericLiteralArgumentAndNestedTraversalObject :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.GenericLiteralArgument
genericLiteralArgumentAndNestedTraversalObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
        Core.projectionField = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

genericLiteralArgumentAndNestedTraversalTraversal :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal
genericLiteralArgumentAndNestedTraversalTraversal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
        Core.projectionField = (Core.Name "traversal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

genericLiteralArgumentAndNestedTraversalWithObject :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndNestedTraversal
genericLiteralArgumentAndNestedTraversalWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
              Core.projectionField = (Core.Name "traversal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

genericLiteralArgumentAndNestedTraversalWithTraversal :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndNestedTraversal
genericLiteralArgumentAndNestedTraversalWithTraversal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal"),
              Core.projectionField = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

genericLiteralArgumentAndOptionalTraversalBiFunctionArgument :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm (Maybe Gremlin.TraversalBiFunctionArgument) -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument
genericLiteralArgumentAndOptionalTraversalBiFunctionArgument literal biFunction =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTTerm literal)},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Phantoms.unTTerm biFunction)}]}))

genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentBiFunction :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument -> Phantoms.TTerm (Maybe Gremlin.TraversalBiFunctionArgument)
genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentBiFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
        Core.projectionField = (Core.Name "biFunction")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentLiteral :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument
genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
        Core.projectionField = (Core.Name "literal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentWithBiFunction :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument -> Phantoms.TTerm (Maybe Gremlin.TraversalBiFunctionArgument) -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument
genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentWithBiFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
              Core.projectionField = (Core.Name "literal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentWithLiteral :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument
genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentWithLiteral original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
              Core.projectionField = (Core.Name "biFunction")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

genericLiteralArgumentAndTraversalBiFunctionArgument :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalBiFunctionArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument
genericLiteralArgumentAndTraversalBiFunctionArgument literal biFunction =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTTerm literal)},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Phantoms.unTTerm biFunction)}]}))

genericLiteralArgumentAndTraversalBiFunctionArgumentBiFunction :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument -> Phantoms.TTerm Gremlin.TraversalBiFunctionArgument
genericLiteralArgumentAndTraversalBiFunctionArgumentBiFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
        Core.projectionField = (Core.Name "biFunction")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

genericLiteralArgumentAndTraversalBiFunctionArgumentLiteral :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument
genericLiteralArgumentAndTraversalBiFunctionArgumentLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
        Core.projectionField = (Core.Name "literal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

genericLiteralArgumentAndTraversalBiFunctionArgumentWithBiFunction :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument -> Phantoms.TTerm Gremlin.TraversalBiFunctionArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument
genericLiteralArgumentAndTraversalBiFunctionArgumentWithBiFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
              Core.projectionField = (Core.Name "literal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

genericLiteralArgumentAndTraversalBiFunctionArgumentWithLiteral :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument
genericLiteralArgumentAndTraversalBiFunctionArgumentWithLiteral original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "biFunction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
              Core.projectionField = (Core.Name "biFunction")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

genericLiteralArgumentAndTraversalPredicateLiteral :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndTraversalPredicate
genericLiteralArgumentAndTraversalPredicateLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralArgumentAndTraversalPredicatePredicate :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.GenericLiteralArgumentAndTraversalPredicate
genericLiteralArgumentAndTraversalPredicatePredicate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralArgumentValue :: Phantoms.TTerm Gremlin.GenericLiteral -> Phantoms.TTerm Gremlin.GenericLiteralArgument
genericLiteralArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.GenericLiteralArgument
genericLiteralArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralBoolean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralCollection :: Phantoms.TTerm [Gremlin.GenericLiteral] -> Phantoms.TTerm Gremlin.GenericLiteralCollection
genericLiteralCollection x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralCollection"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

genericLiteralDate :: Phantoms.TTerm Gremlin.DateLiteral -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralDate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "date"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralGenericLiteralCollection :: Phantoms.TTerm Gremlin.GenericLiteralCollection -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralGenericLiteralCollection x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genericLiteralCollection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralGenericLiteralMap :: Phantoms.TTerm Gremlin.GenericLiteralMap -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralGenericLiteralMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genericLiteralMap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralGenericLiteralRange :: Phantoms.TTerm Gremlin.GenericLiteralRange -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralGenericLiteralRange x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genericLiteralRange"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralGenericLiteralSet :: Phantoms.TTerm Gremlin.GenericLiteralSet -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralGenericLiteralSet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genericLiteralSet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralInf :: Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralInf =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inf"),
        Core.fieldTerm = Core.TermUnit}}))

genericLiteralList :: Phantoms.TTerm [Gremlin.GenericLiteral] -> Phantoms.TTerm Gremlin.GenericLiteralList
genericLiteralList x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralList"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

genericLiteralListArgumentValue :: Phantoms.TTerm Gremlin.GenericLiteralList -> Phantoms.TTerm Gremlin.GenericLiteralListArgument
genericLiteralListArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralListArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralListArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.GenericLiteralListArgument
genericLiteralListArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralListArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralMap :: Phantoms.TTerm [Gremlin.MapEntry] -> Phantoms.TTerm Gremlin.GenericLiteralMap
genericLiteralMap x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMap"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

genericLiteralMapArgumentValue :: Phantoms.TTerm Gremlin.GenericLiteralMap -> Phantoms.TTerm Gremlin.GenericLiteralMapArgument
genericLiteralMapArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralMapArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.GenericLiteralMapArgument
genericLiteralMapArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralMapNullableArgumentAndTraversalCardinalityArgument :: Phantoms.TTerm Gremlin.TraversalCardinalityArgument -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument
genericLiteralMapNullableArgumentAndTraversalCardinalityArgument cardinality object =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm cardinality)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm object)}]}))

genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentCardinality :: Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument -> Phantoms.TTerm Gremlin.TraversalCardinalityArgument
genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentCardinality x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
        Core.projectionField = (Core.Name "cardinality")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentObject :: Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgument
genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
        Core.projectionField = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentWithCardinality :: Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument -> Phantoms.TTerm Gremlin.TraversalCardinalityArgument -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument
genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentWithCardinality original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
              Core.projectionField = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentWithObject :: Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument
genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
              Core.projectionField = (Core.Name "cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

genericLiteralMapNullableArgumentOrNestedTraversalMap :: Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal
genericLiteralMapNullableArgumentOrNestedTraversalMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralMapNullableArgumentOrNestedTraversalTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal
genericLiteralMapNullableArgumentOrNestedTraversalTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralMapNullableArgumentValue :: Phantoms.TTerm (Maybe Gremlin.GenericLiteralMap) -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgument
genericLiteralMapNullableArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralMapNullableArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgument
genericLiteralMapNullableArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralNan :: Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralNan =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nan"),
        Core.fieldTerm = Core.TermUnit}}))

genericLiteralNestedTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralNestedTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedTraversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralNull :: Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralNull =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

genericLiteralNumeric :: Phantoms.TTerm Gremlin.NumericLiteral -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralNumeric x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralRangeInteger :: Phantoms.TTerm Gremlin.IntegerRange -> Phantoms.TTerm Gremlin.GenericLiteralRange
genericLiteralRangeInteger x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralRangeString :: Phantoms.TTerm Gremlin.StringRange -> Phantoms.TTerm Gremlin.GenericLiteralRange
genericLiteralRangeString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralSet :: Phantoms.TTerm [Gremlin.GenericLiteral] -> Phantoms.TTerm Gremlin.GenericLiteralSet
genericLiteralSet x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralSet"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

genericLiteralString :: Phantoms.TTerm String -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralStructureVertex :: Phantoms.TTerm Gremlin.StructureVertex -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralStructureVertex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "structureVertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralTerminatedTraversal :: Phantoms.TTerm Gremlin.TerminatedTraversal -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralTerminatedTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "terminatedTraversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralTraversalCardinality :: Phantoms.TTerm Gremlin.TraversalCardinality -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralTraversalCardinality x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralTraversalDT :: Phantoms.TTerm Gremlin.TraversalDT -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralTraversalDT x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalDT"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralTraversalDirection :: Phantoms.TTerm Gremlin.TraversalDirection -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralTraversalDirection x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalDirection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralTraversalMerge :: Phantoms.TTerm Gremlin.TraversalMerge -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralTraversalMerge x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalMerge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralTraversalPick :: Phantoms.TTerm Gremlin.TraversalPick -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralTraversalPick x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalPick"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericLiteralTraversalToken :: Phantoms.TTerm Gremlin.TraversalToken -> Phantoms.TTerm Gremlin.GenericLiteral
genericLiteralTraversalToken x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalToken"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasArgsString :: Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm Gremlin.HasArgs
hasArgsString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasArgsTraversalToken :: Phantoms.TTerm Gremlin.HasTraversalTokenArgs -> Phantoms.TTerm Gremlin.HasArgs
hasArgsTraversalToken x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalToken"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargs :: Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm (Maybe Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest) -> Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs
hasStringArgumentAndOptionalStringLiteralVarargs string rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

hasStringArgumentAndOptionalStringLiteralVarargsRest :: Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm (Maybe Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest)
hasStringArgumentAndOptionalStringLiteralVarargsRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
        Core.projectionField = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasStringArgumentAndOptionalStringLiteralVarargsRestObject :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest
hasStringArgumentAndOptionalStringLiteralVarargsRestObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargsRestPredicate :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest
hasStringArgumentAndOptionalStringLiteralVarargsRestPredicate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargsRestStringObject :: Phantoms.TTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument -> Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest
hasStringArgumentAndOptionalStringLiteralVarargsRestStringObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "stringObject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargsRestStringPredicate :: Phantoms.TTerm Gremlin.StringNullableArgumentAndTraversalPredicate -> Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest
hasStringArgumentAndOptionalStringLiteralVarargsRestStringPredicate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "stringPredicate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargsRestTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest
hasStringArgumentAndOptionalStringLiteralVarargsRestTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasStringArgumentAndOptionalStringLiteralVarargsString :: Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm Gremlin.StringNullableArgument
hasStringArgumentAndOptionalStringLiteralVarargsString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
        Core.projectionField = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasStringArgumentAndOptionalStringLiteralVarargsWithRest :: Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm (Maybe Gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest) -> Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs
hasStringArgumentAndOptionalStringLiteralVarargsWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
              Core.projectionField = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

hasStringArgumentAndOptionalStringLiteralVarargsWithString :: Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.HasStringArgumentAndOptionalStringLiteralVarargs
hasStringArgumentAndOptionalStringLiteralVarargsWithString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
              Core.projectionField = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hasTraversalTokenArgs :: Phantoms.TTerm Gremlin.TraversalTokenArgument -> Phantoms.TTerm Gremlin.HasTraversalTokenArgsRest -> Phantoms.TTerm Gremlin.HasTraversalTokenArgs
hasTraversalTokenArgs traversalToken rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversalToken"),
          Core.fieldTerm = (Phantoms.unTTerm traversalToken)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

hasTraversalTokenArgsRest :: Phantoms.TTerm Gremlin.HasTraversalTokenArgs -> Phantoms.TTerm Gremlin.HasTraversalTokenArgsRest
hasTraversalTokenArgsRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
        Core.projectionField = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasTraversalTokenArgsRestLiteral :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.HasTraversalTokenArgsRest
hasTraversalTokenArgsRestLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasTraversalTokenArgsRestPredicate :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.HasTraversalTokenArgsRest
hasTraversalTokenArgsRestPredicate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasTraversalTokenArgsRestTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.HasTraversalTokenArgsRest
hasTraversalTokenArgsRestTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgsRest"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hasTraversalTokenArgsTraversalToken :: Phantoms.TTerm Gremlin.HasTraversalTokenArgs -> Phantoms.TTerm Gremlin.TraversalTokenArgument
hasTraversalTokenArgsTraversalToken x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
        Core.projectionField = (Core.Name "traversalToken")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasTraversalTokenArgsWithRest :: Phantoms.TTerm Gremlin.HasTraversalTokenArgs -> Phantoms.TTerm Gremlin.HasTraversalTokenArgsRest -> Phantoms.TTerm Gremlin.HasTraversalTokenArgs
hasTraversalTokenArgsWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversalToken"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
              Core.projectionField = (Core.Name "traversalToken")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

hasTraversalTokenArgsWithTraversalToken :: Phantoms.TTerm Gremlin.HasTraversalTokenArgs -> Phantoms.TTerm Gremlin.TraversalTokenArgument -> Phantoms.TTerm Gremlin.HasTraversalTokenArgs
hasTraversalTokenArgsWithTraversalToken original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversalToken"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.HasTraversalTokenArgs"),
              Core.projectionField = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

identifier :: Phantoms.TTerm String -> Phantoms.TTerm Gremlin.Identifier
identifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.Identifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

integerArgumentValue :: Phantoms.TTerm Gremlin.IntegerLiteral -> Phantoms.TTerm Gremlin.IntegerArgument
integerArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.IntegerArgument
integerArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerLiteral :: Phantoms.TTerm Integer -> Phantoms.TTerm Gremlin.IntegerLiteral
integerLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

integerRange :: Phantoms.TTerm Gremlin.IntegerLiteral -> Phantoms.TTerm Gremlin.IntegerLiteral -> Phantoms.TTerm Gremlin.IntegerRange
integerRange left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

integerRangeLeft :: Phantoms.TTerm Gremlin.IntegerRange -> Phantoms.TTerm Gremlin.IntegerLiteral
integerRangeLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

integerRangeRight :: Phantoms.TTerm Gremlin.IntegerRange -> Phantoms.TTerm Gremlin.IntegerLiteral
integerRangeRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

integerRangeWithLeft :: Phantoms.TTerm Gremlin.IntegerRange -> Phantoms.TTerm Gremlin.IntegerLiteral -> Phantoms.TTerm Gremlin.IntegerRange
integerRangeWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

integerRangeWithRight :: Phantoms.TTerm Gremlin.IntegerRange -> Phantoms.TTerm Gremlin.IntegerLiteral -> Phantoms.TTerm Gremlin.IntegerRange
integerRangeWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IntegerRange"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ioOptionsKeysReader :: Phantoms.TTerm Gremlin.IoOptionsKeys
ioOptionsKeysReader =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IoOptionsKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reader"),
        Core.fieldTerm = Core.TermUnit}}))

ioOptionsKeysWriter :: Phantoms.TTerm Gremlin.IoOptionsKeys
ioOptionsKeysWriter =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IoOptionsKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "writer"),
        Core.fieldTerm = Core.TermUnit}}))

ioOptionsValuesGraphml :: Phantoms.TTerm Gremlin.IoOptionsValues
ioOptionsValuesGraphml =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IoOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "graphml"),
        Core.fieldTerm = Core.TermUnit}}))

ioOptionsValuesGraphson :: Phantoms.TTerm Gremlin.IoOptionsValues
ioOptionsValuesGraphson =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IoOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "graphson"),
        Core.fieldTerm = Core.TermUnit}}))

ioOptionsValuesGryo :: Phantoms.TTerm Gremlin.IoOptionsValues
ioOptionsValuesGryo =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.IoOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gryo"),
        Core.fieldTerm = Core.TermUnit}}))

keywordEdges :: Phantoms.TTerm Gremlin.Keyword
keywordEdges =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Keyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edges"),
        Core.fieldTerm = Core.TermUnit}}))

keywordKeys :: Phantoms.TTerm Gremlin.Keyword
keywordKeys =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Keyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keys"),
        Core.fieldTerm = Core.TermUnit}}))

keywordNew :: Phantoms.TTerm Gremlin.Keyword
keywordNew =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Keyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = Core.TermUnit}}))

keywordOrIdentifierIdentifier :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.KeywordOrIdentifier
keywordOrIdentifierIdentifier x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.KeywordOrIdentifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

keywordOrIdentifierKeyword :: Phantoms.TTerm Gremlin.Keyword -> Phantoms.TTerm Gremlin.KeywordOrIdentifier
keywordOrIdentifierKeyword x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.KeywordOrIdentifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keyword"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

keywordValues :: Phantoms.TTerm Gremlin.Keyword
keywordValues =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Keyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "values"),
        Core.fieldTerm = Core.TermUnit}}))

mapEntryKey :: Phantoms.TTerm Gremlin.MapKey -> Phantoms.TTerm Gremlin.MapEntry
mapEntryKey x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapEntry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "key"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapEntryValue :: Phantoms.TTerm Gremlin.GenericLiteral -> Phantoms.TTerm Gremlin.MapEntry
mapEntryValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapEntry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapKeyCollection :: Phantoms.TTerm Gremlin.GenericLiteralCollection -> Phantoms.TTerm Gremlin.MapKey
mapKeyCollection x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "collection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapKeyIdentifier :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.MapKey
mapKeyIdentifier x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapKeyKeyword :: Phantoms.TTerm Gremlin.Keyword -> Phantoms.TTerm Gremlin.MapKey
mapKeyKeyword x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keyword"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapKeyMap :: Phantoms.TTerm Gremlin.GenericLiteralMap -> Phantoms.TTerm Gremlin.MapKey
mapKeyMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapKeyNumeric :: Phantoms.TTerm Gremlin.NumericLiteral -> Phantoms.TTerm Gremlin.MapKey
mapKeyNumeric x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapKeySet :: Phantoms.TTerm Gremlin.GenericLiteralSet -> Phantoms.TTerm Gremlin.MapKey
mapKeySet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapKeyString :: Phantoms.TTerm String -> Phantoms.TTerm Gremlin.MapKey
mapKeyString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapKeyTraversalDirection :: Phantoms.TTerm Gremlin.TraversalDirection -> Phantoms.TTerm Gremlin.MapKey
mapKeyTraversalDirection x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalDirection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapKeyTraversalToken :: Phantoms.TTerm Gremlin.TraversalToken -> Phantoms.TTerm Gremlin.MapKey
mapKeyTraversalToken x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.MapKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalToken"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nestedTraversalAnonymous :: Phantoms.TTerm Gremlin.ChainedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal
nestedTraversalAnonymous x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nestedTraversalArgument :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TTerm Gremlin.NestedTraversalArgument
nestedTraversalArgument traversal1 traversal2 traversal3 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Phantoms.unTTerm traversal1)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Phantoms.unTTerm traversal2)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal3"),
          Core.fieldTerm = (Phantoms.unTTerm traversal3)}]}))

nestedTraversalArgumentTraversal1 :: Phantoms.TTerm Gremlin.NestedTraversalArgument -> Phantoms.TTerm Gremlin.NestedTraversal
nestedTraversalArgumentTraversal1 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
        Core.projectionField = (Core.Name "traversal1")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nestedTraversalArgumentTraversal2 :: Phantoms.TTerm Gremlin.NestedTraversalArgument -> Phantoms.TTerm (Maybe Gremlin.NestedTraversal)
nestedTraversalArgumentTraversal2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
        Core.projectionField = (Core.Name "traversal2")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nestedTraversalArgumentTraversal3 :: Phantoms.TTerm Gremlin.NestedTraversalArgument -> Phantoms.TTerm (Maybe Gremlin.NestedTraversal)
nestedTraversalArgumentTraversal3 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
        Core.projectionField = (Core.Name "traversal3")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nestedTraversalArgumentWithTraversal1 :: Phantoms.TTerm Gremlin.NestedTraversalArgument -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversalArgument
nestedTraversalArgumentWithTraversal1 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionField = (Core.Name "traversal2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionField = (Core.Name "traversal3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nestedTraversalArgumentWithTraversal2 :: Phantoms.TTerm Gremlin.NestedTraversalArgument -> Phantoms.TTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TTerm Gremlin.NestedTraversalArgument
nestedTraversalArgumentWithTraversal2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionField = (Core.Name "traversal1")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionField = (Core.Name "traversal3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nestedTraversalArgumentWithTraversal3 :: Phantoms.TTerm Gremlin.NestedTraversalArgument -> Phantoms.TTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TTerm Gremlin.NestedTraversalArgument
nestedTraversalArgumentWithTraversal3 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionField = (Core.Name "traversal1")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversalArgument"),
              Core.projectionField = (Core.Name "traversal2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal3"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nestedTraversalChained :: Phantoms.TTerm Gremlin.ChainedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal
nestedTraversalChained x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "chained"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nestedTraversalRoot :: Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm Gremlin.NestedTraversal
nestedTraversalRoot x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "root"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericLiteralFloat :: Phantoms.TTerm Gremlin.FloatLiteral -> Phantoms.TTerm Gremlin.NumericLiteral
numericLiteralFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericLiteralInteger :: Phantoms.TTerm Gremlin.IntegerLiteral -> Phantoms.TTerm Gremlin.NumericLiteral
numericLiteralInteger x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

optionArgsMergeMap :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TTerm Gremlin.OptionArgs
optionArgsMergeMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeMap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

optionArgsMergeTraversal :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.OptionArgs
optionArgsMergeTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeTraversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

optionArgsObjectTraversal :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.OptionArgs
optionArgsObjectTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectTraversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

optionArgsPredicateTraversal :: Phantoms.TTerm Gremlin.TraversalPredicateAndNestedTraversal -> Phantoms.TTerm Gremlin.OptionArgs
optionArgsPredicateTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicateTraversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

optionArgsTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.OptionArgs
optionArgsTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

optionalStringArgumentAndNestedTraversal :: Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.OptionalStringArgumentAndNestedTraversal
optionalStringArgumentAndNestedTraversal string traversal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTTerm traversal)}]}))

optionalStringArgumentAndNestedTraversalString :: Phantoms.TTerm Gremlin.OptionalStringArgumentAndNestedTraversal -> Phantoms.TTerm (Maybe Gremlin.StringArgument)
optionalStringArgumentAndNestedTraversalString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
        Core.projectionField = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionalStringArgumentAndNestedTraversalTraversal :: Phantoms.TTerm Gremlin.OptionalStringArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal
optionalStringArgumentAndNestedTraversalTraversal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
        Core.projectionField = (Core.Name "traversal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionalStringArgumentAndNestedTraversalWithString :: Phantoms.TTerm Gremlin.OptionalStringArgumentAndNestedTraversal -> Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.OptionalStringArgumentAndNestedTraversal
optionalStringArgumentAndNestedTraversalWithString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
              Core.projectionField = (Core.Name "traversal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

optionalStringArgumentAndNestedTraversalWithTraversal :: Phantoms.TTerm Gremlin.OptionalStringArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.OptionalStringArgumentAndNestedTraversal
optionalStringArgumentAndNestedTraversalWithTraversal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal"),
              Core.projectionField = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

optionalTraversalScopeArgumentAndIntegerArgument :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument
optionalTraversalScopeArgumentAndIntegerArgument scope long =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "long"),
          Core.fieldTerm = (Phantoms.unTTerm long)}]}))

optionalTraversalScopeArgumentAndIntegerArgumentLong :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TTerm Gremlin.IntegerArgument
optionalTraversalScopeArgumentAndIntegerArgumentLong x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
        Core.projectionField = (Core.Name "long")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionalTraversalScopeArgumentAndIntegerArgumentScope :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument)
optionalTraversalScopeArgumentAndIntegerArgumentScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
        Core.projectionField = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionalTraversalScopeArgumentAndIntegerArgumentWithLong :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument
optionalTraversalScopeArgumentAndIntegerArgumentWithLong original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "long"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

optionalTraversalScopeArgumentAndIntegerArgumentWithScope :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument
optionalTraversalScopeArgumentAndIntegerArgumentWithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "long"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
              Core.projectionField = (Core.Name "long")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

optionalTraversalScopeArgumentAndStringArgument :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument
optionalTraversalScopeArgumentAndStringArgument scope string =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm string)}]}))

optionalTraversalScopeArgumentAndStringArgumentScope :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument)
optionalTraversalScopeArgumentAndStringArgumentScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
        Core.projectionField = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionalTraversalScopeArgumentAndStringArgumentString :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument -> Phantoms.TTerm Gremlin.StringArgument
optionalTraversalScopeArgumentAndStringArgumentString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
        Core.projectionField = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionalTraversalScopeArgumentAndStringArgumentWithScope :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument
optionalTraversalScopeArgumentAndStringArgumentWithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
              Core.projectionField = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

optionalTraversalScopeArgumentAndStringArgumentWithString :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument -> Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument
optionalTraversalScopeArgumentAndStringArgumentWithString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pageRankConstantsEdges :: Phantoms.TTerm Gremlin.PageRankConstants
pageRankConstantsEdges =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PageRankConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edges"),
        Core.fieldTerm = Core.TermUnit}}))

pageRankConstantsPropertyName :: Phantoms.TTerm Gremlin.PageRankConstants
pageRankConstantsPropertyName =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PageRankConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyName"),
        Core.fieldTerm = Core.TermUnit}}))

pageRankConstantsTimes :: Phantoms.TTerm Gremlin.PageRankConstants
pageRankConstantsTimes =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PageRankConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = Core.TermUnit}}))

peerPressureConstantsEdges :: Phantoms.TTerm Gremlin.PeerPressureConstants
peerPressureConstantsEdges =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PeerPressureConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edges"),
        Core.fieldTerm = Core.TermUnit}}))

peerPressureConstantsPropertyName :: Phantoms.TTerm Gremlin.PeerPressureConstants
peerPressureConstantsPropertyName =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PeerPressureConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyName"),
        Core.fieldTerm = Core.TermUnit}}))

peerPressureConstantsTimes :: Phantoms.TTerm Gremlin.PeerPressureConstants
peerPressureConstantsTimes =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PeerPressureConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = Core.TermUnit}}))

popStringsArgument :: Phantoms.TTerm Gremlin.TraversalPopArgument -> Phantoms.TTerm [Gremlin.StringArgument] -> Phantoms.TTerm Gremlin.PopStringsArgument
popStringsArgument pop string =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Phantoms.unTTerm pop)},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm string)}]}))

popStringsArgumentPop :: Phantoms.TTerm Gremlin.PopStringsArgument -> Phantoms.TTerm Gremlin.TraversalPopArgument
popStringsArgumentPop x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
        Core.projectionField = (Core.Name "pop")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

popStringsArgumentString :: Phantoms.TTerm Gremlin.PopStringsArgument -> Phantoms.TTerm [Gremlin.StringArgument]
popStringsArgumentString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
        Core.projectionField = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

popStringsArgumentWithPop :: Phantoms.TTerm Gremlin.PopStringsArgument -> Phantoms.TTerm Gremlin.TraversalPopArgument -> Phantoms.TTerm Gremlin.PopStringsArgument
popStringsArgumentWithPop original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
              Core.projectionField = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

popStringsArgumentWithString :: Phantoms.TTerm Gremlin.PopStringsArgument -> Phantoms.TTerm [Gremlin.StringArgument] -> Phantoms.TTerm Gremlin.PopStringsArgument
popStringsArgumentWithString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PopStringsArgument"),
              Core.projectionField = (Core.Name "pop")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

predicateOrTraversalPredicate :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.PredicateOrTraversal
predicateOrTraversalPredicate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateOrTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

predicateOrTraversalTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.PredicateOrTraversal
predicateOrTraversalTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateOrTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

predicateTraversalArgument :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TTerm Gremlin.PredicateTraversalArgument
predicateTraversalArgument predicate traversal1 traversal2 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Phantoms.unTTerm traversal1)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Phantoms.unTTerm traversal2)}]}))

predicateTraversalArgumentPredicate :: Phantoms.TTerm Gremlin.PredicateTraversalArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
predicateTraversalArgumentPredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
        Core.projectionField = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

predicateTraversalArgumentTraversal1 :: Phantoms.TTerm Gremlin.PredicateTraversalArgument -> Phantoms.TTerm Gremlin.NestedTraversal
predicateTraversalArgumentTraversal1 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
        Core.projectionField = (Core.Name "traversal1")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

predicateTraversalArgumentTraversal2 :: Phantoms.TTerm Gremlin.PredicateTraversalArgument -> Phantoms.TTerm (Maybe Gremlin.NestedTraversal)
predicateTraversalArgumentTraversal2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
        Core.projectionField = (Core.Name "traversal2")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

predicateTraversalArgumentWithPredicate :: Phantoms.TTerm Gremlin.PredicateTraversalArgument -> Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.PredicateTraversalArgument
predicateTraversalArgumentWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionField = (Core.Name "traversal1")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionField = (Core.Name "traversal2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

predicateTraversalArgumentWithTraversal1 :: Phantoms.TTerm Gremlin.PredicateTraversalArgument -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.PredicateTraversalArgument
predicateTraversalArgumentWithTraversal1 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionField = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionField = (Core.Name "traversal2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

predicateTraversalArgumentWithTraversal2 :: Phantoms.TTerm Gremlin.PredicateTraversalArgument -> Phantoms.TTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TTerm Gremlin.PredicateTraversalArgument
predicateTraversalArgumentWithTraversal2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionField = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PredicateTraversalArgument"),
              Core.projectionField = (Core.Name "traversal1")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyArgsCardinalityObject :: Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument -> Phantoms.TTerm Gremlin.PropertyArgs
propertyArgsCardinalityObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PropertyArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cardinalityObject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyArgsCardinalityObjects :: Phantoms.TTerm Gremlin.TraversalCardinalityArgumentAndObjects -> Phantoms.TTerm Gremlin.PropertyArgs
propertyArgsCardinalityObjects x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PropertyArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cardinalityObjects"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyArgsObject :: Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TTerm Gremlin.PropertyArgs
propertyArgsObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PropertyArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyArgsObjects :: Phantoms.TTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TTerm Gremlin.PropertyArgs
propertyArgsObjects x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.PropertyArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objects"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

queryEmpty :: Phantoms.TTerm Gremlin.Query
queryEmpty =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))

queryList :: Phantoms.TTerm [Gremlin.Query] -> Phantoms.TTerm Gremlin.QueryList
queryList x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.QueryList"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

queryRootTraversal :: Phantoms.TTerm Gremlin.RootTraversalQuery -> Phantoms.TTerm Gremlin.Query
queryRootTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rootTraversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

queryToString :: Phantoms.TTerm Gremlin.Query
queryToString =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toString"),
        Core.fieldTerm = Core.TermUnit}}))

queryTraversalSource :: Phantoms.TTerm Gremlin.TraversalSourceQuery -> Phantoms.TTerm Gremlin.Query
queryTraversalSource x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversalSource"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rangeArgs :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.RangeArgs
rangeArgs scope min max =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm min)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm max)}]}))

rangeArgsMax :: Phantoms.TTerm Gremlin.RangeArgs -> Phantoms.TTerm Gremlin.IntegerArgument
rangeArgsMax x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
        Core.projectionField = (Core.Name "max")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeArgsMin :: Phantoms.TTerm Gremlin.RangeArgs -> Phantoms.TTerm Gremlin.IntegerArgument
rangeArgsMin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
        Core.projectionField = (Core.Name "min")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeArgsScope :: Phantoms.TTerm Gremlin.RangeArgs -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument)
rangeArgsScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
        Core.projectionField = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeArgsWithMax :: Phantoms.TTerm Gremlin.RangeArgs -> Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.RangeArgs
rangeArgsWithMax original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionField = (Core.Name "min")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rangeArgsWithMin :: Phantoms.TTerm Gremlin.RangeArgs -> Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.RangeArgs
rangeArgsWithMin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionField = (Core.Name "max")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rangeArgsWithScope :: Phantoms.TTerm Gremlin.RangeArgs -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.RangeArgs
rangeArgsWithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionField = (Core.Name "min")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgs"),
              Core.projectionField = (Core.Name "max")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rangeArgument :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.RangeArgument
rangeArgument min max =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm min)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm max)}]}))

rangeArgumentMax :: Phantoms.TTerm Gremlin.RangeArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument
rangeArgumentMax x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
        Core.projectionField = (Core.Name "max")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeArgumentMin :: Phantoms.TTerm Gremlin.RangeArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument
rangeArgumentMin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
        Core.projectionField = (Core.Name "min")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeArgumentWithMax :: Phantoms.TTerm Gremlin.RangeArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.RangeArgument
rangeArgumentWithMax original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
              Core.projectionField = (Core.Name "min")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rangeArgumentWithMin :: Phantoms.TTerm Gremlin.RangeArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.RangeArgument
rangeArgumentWithMin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RangeArgument"),
              Core.projectionField = (Core.Name "max")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

replaceArgs :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.ReplaceArgs
replaceArgs scope from to =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTTerm from)},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTTerm to)}]}))

replaceArgsFrom :: Phantoms.TTerm Gremlin.ReplaceArgs -> Phantoms.TTerm Gremlin.StringNullableArgument
replaceArgsFrom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
        Core.projectionField = (Core.Name "from")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

replaceArgsScope :: Phantoms.TTerm Gremlin.ReplaceArgs -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument)
replaceArgsScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
        Core.projectionField = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

replaceArgsTo :: Phantoms.TTerm Gremlin.ReplaceArgs -> Phantoms.TTerm Gremlin.StringNullableArgument
replaceArgsTo x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
        Core.projectionField = (Core.Name "to")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

replaceArgsWithFrom :: Phantoms.TTerm Gremlin.ReplaceArgs -> Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.ReplaceArgs
replaceArgsWithFrom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionField = (Core.Name "to")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

replaceArgsWithScope :: Phantoms.TTerm Gremlin.ReplaceArgs -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.ReplaceArgs
replaceArgsWithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionField = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionField = (Core.Name "to")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

replaceArgsWithTo :: Phantoms.TTerm Gremlin.ReplaceArgs -> Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.ReplaceArgs
replaceArgsWithTo original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"),
              Core.projectionField = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rootTraversal :: Phantoms.TTerm Gremlin.TraversalSource -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod -> Phantoms.TTerm [Gremlin.ChainedTraversalElement] -> Phantoms.TTerm Gremlin.RootTraversal
rootTraversal source spawnMethod chained =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "spawnMethod"),
          Core.fieldTerm = (Phantoms.unTTerm spawnMethod)},
        Core.Field {
          Core.fieldName = (Core.Name "chained"),
          Core.fieldTerm = (Phantoms.unTTerm chained)}]}))

rootTraversalChained :: Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm [Gremlin.ChainedTraversalElement]
rootTraversalChained x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
        Core.projectionField = (Core.Name "chained")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rootTraversalQuery :: Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm (Maybe Gremlin.TraversalTerminalMethod) -> Phantoms.TTerm Gremlin.RootTraversalQuery
rootTraversalQuery root terminalMethod =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Phantoms.unTTerm root)},
        Core.Field {
          Core.fieldName = (Core.Name "terminalMethod"),
          Core.fieldTerm = (Phantoms.unTTerm terminalMethod)}]}))

rootTraversalQueryRoot :: Phantoms.TTerm Gremlin.RootTraversalQuery -> Phantoms.TTerm Gremlin.RootTraversal
rootTraversalQueryRoot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
        Core.projectionField = (Core.Name "root")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rootTraversalQueryTerminalMethod :: Phantoms.TTerm Gremlin.RootTraversalQuery -> Phantoms.TTerm (Maybe Gremlin.TraversalTerminalMethod)
rootTraversalQueryTerminalMethod x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
        Core.projectionField = (Core.Name "terminalMethod")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rootTraversalQueryWithRoot :: Phantoms.TTerm Gremlin.RootTraversalQuery -> Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm Gremlin.RootTraversalQuery
rootTraversalQueryWithRoot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "terminalMethod"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
              Core.projectionField = (Core.Name "terminalMethod")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rootTraversalQueryWithTerminalMethod :: Phantoms.TTerm Gremlin.RootTraversalQuery -> Phantoms.TTerm (Maybe Gremlin.TraversalTerminalMethod) -> Phantoms.TTerm Gremlin.RootTraversalQuery
rootTraversalQueryWithTerminalMethod original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"),
              Core.projectionField = (Core.Name "root")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "terminalMethod"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rootTraversalSource :: Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm Gremlin.TraversalSource
rootTraversalSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
        Core.projectionField = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rootTraversalSpawnMethod :: Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
rootTraversalSpawnMethod x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
        Core.projectionField = (Core.Name "spawnMethod")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rootTraversalWithChained :: Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm [Gremlin.ChainedTraversalElement] -> Phantoms.TTerm Gremlin.RootTraversal
rootTraversalWithChained original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spawnMethod"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionField = (Core.Name "spawnMethod")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "chained"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rootTraversalWithSource :: Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm Gremlin.TraversalSource -> Phantoms.TTerm Gremlin.RootTraversal
rootTraversalWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "spawnMethod"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionField = (Core.Name "spawnMethod")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "chained"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionField = (Core.Name "chained")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rootTraversalWithSpawnMethod :: Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod -> Phantoms.TTerm Gremlin.RootTraversal
rootTraversalWithSpawnMethod original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spawnMethod"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "chained"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.RootTraversal"),
              Core.projectionField = (Core.Name "chained")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

scopeStringArgument :: Phantoms.TTerm Gremlin.TraversalScopeArgument -> Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.ScopeStringArgument
scopeStringArgument scope strings =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "strings"),
          Core.fieldTerm = (Phantoms.unTTerm strings)}]}))

scopeStringArgumentScope :: Phantoms.TTerm Gremlin.ScopeStringArgument -> Phantoms.TTerm Gremlin.TraversalScopeArgument
scopeStringArgumentScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
        Core.projectionField = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scopeStringArgumentStrings :: Phantoms.TTerm Gremlin.ScopeStringArgument -> Phantoms.TTerm [Gremlin.StringNullableArgument]
scopeStringArgumentStrings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
        Core.projectionField = (Core.Name "strings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scopeStringArgumentWithScope :: Phantoms.TTerm Gremlin.ScopeStringArgument -> Phantoms.TTerm Gremlin.TraversalScopeArgument -> Phantoms.TTerm Gremlin.ScopeStringArgument
scopeStringArgumentWithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "strings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
              Core.projectionField = (Core.Name "strings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

scopeStringArgumentWithStrings :: Phantoms.TTerm Gremlin.ScopeStringArgument -> Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.ScopeStringArgument
scopeStringArgumentWithStrings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgument"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "strings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selectArgsColumn :: Phantoms.TTerm Gremlin.TraversalColumnArgument -> Phantoms.TTerm Gremlin.SelectArgs
selectArgsColumn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SelectArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "column"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectArgsPopStrings :: Phantoms.TTerm Gremlin.PopStringsArgument -> Phantoms.TTerm Gremlin.SelectArgs
selectArgsPopStrings x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SelectArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "popStrings"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectArgsPopTraversal :: Phantoms.TTerm Gremlin.TraversalPopArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.SelectArgs
selectArgsPopTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SelectArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "popTraversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectArgsStrings :: Phantoms.TTerm [Gremlin.StringArgument] -> Phantoms.TTerm Gremlin.SelectArgs
selectArgsStrings x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SelectArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strings"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectArgsTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.SelectArgs
selectArgsTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SelectArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

serviceArgumentsMap :: Phantoms.TTerm (Maybe Gremlin.GenericLiteralMapArgument) -> Phantoms.TTerm Gremlin.ServiceArguments
serviceArgumentsMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceArguments"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

serviceArgumentsTraversal :: Phantoms.TTerm (Maybe Gremlin.NestedTraversal) -> Phantoms.TTerm Gremlin.ServiceArguments
serviceArgumentsTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceArguments"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

serviceCall :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.ServiceArguments -> Phantoms.TTerm Gremlin.ServiceCall
serviceCall service arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "service"),
          Core.fieldTerm = (Phantoms.unTTerm service)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

serviceCallArguments :: Phantoms.TTerm Gremlin.ServiceCall -> Phantoms.TTerm Gremlin.ServiceArguments
serviceCallArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

serviceCallService :: Phantoms.TTerm Gremlin.ServiceCall -> Phantoms.TTerm Gremlin.StringArgument
serviceCallService x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
        Core.projectionField = (Core.Name "service")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

serviceCallWithArguments :: Phantoms.TTerm Gremlin.ServiceCall -> Phantoms.TTerm Gremlin.ServiceArguments -> Phantoms.TTerm Gremlin.ServiceCall
serviceCallWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "service"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
              Core.projectionField = (Core.Name "service")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

serviceCallWithService :: Phantoms.TTerm Gremlin.ServiceCall -> Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.ServiceCall
serviceCallWithService original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "service"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ServiceCall"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shortestPathConstantsDistance :: Phantoms.TTerm Gremlin.ShortestPathConstants
shortestPathConstantsDistance =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "distance"),
        Core.fieldTerm = Core.TermUnit}}))

shortestPathConstantsEdges :: Phantoms.TTerm Gremlin.ShortestPathConstants
shortestPathConstantsEdges =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edges"),
        Core.fieldTerm = Core.TermUnit}}))

shortestPathConstantsIncludeEdges :: Phantoms.TTerm Gremlin.ShortestPathConstants
shortestPathConstantsIncludeEdges =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "includeEdges"),
        Core.fieldTerm = Core.TermUnit}}))

shortestPathConstantsMaxDistance :: Phantoms.TTerm Gremlin.ShortestPathConstants
shortestPathConstantsMaxDistance =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxDistance"),
        Core.fieldTerm = Core.TermUnit}}))

shortestPathConstantsTarget :: Phantoms.TTerm Gremlin.ShortestPathConstants
shortestPathConstantsTarget =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "target"),
        Core.fieldTerm = Core.TermUnit}}))

splitArgs :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.SplitArgs
splitArgs scope delimiter =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "delimiter"),
          Core.fieldTerm = (Phantoms.unTTerm delimiter)}]}))

splitArgsDelimiter :: Phantoms.TTerm Gremlin.SplitArgs -> Phantoms.TTerm Gremlin.StringNullableArgument
splitArgsDelimiter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
        Core.projectionField = (Core.Name "delimiter")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

splitArgsScope :: Phantoms.TTerm Gremlin.SplitArgs -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument)
splitArgsScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
        Core.projectionField = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

splitArgsWithDelimiter :: Phantoms.TTerm Gremlin.SplitArgs -> Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.SplitArgs
splitArgsWithDelimiter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "delimiter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

splitArgsWithScope :: Phantoms.TTerm Gremlin.SplitArgs -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.SplitArgs
splitArgsWithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "delimiter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SplitArgs"),
              Core.projectionField = (Core.Name "delimiter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringArgumentAndGenericLiteralArgument :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.StringArgumentAndGenericLiteralArgument
stringArgumentAndGenericLiteralArgument string literal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTTerm literal)}]}))

stringArgumentAndGenericLiteralArgumentLiteral :: Phantoms.TTerm Gremlin.StringArgumentAndGenericLiteralArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument
stringArgumentAndGenericLiteralArgumentLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
        Core.projectionField = (Core.Name "literal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringArgumentAndGenericLiteralArgumentString :: Phantoms.TTerm Gremlin.StringArgumentAndGenericLiteralArgument -> Phantoms.TTerm Gremlin.StringArgument
stringArgumentAndGenericLiteralArgumentString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
        Core.projectionField = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringArgumentAndGenericLiteralArgumentWithLiteral :: Phantoms.TTerm Gremlin.StringArgumentAndGenericLiteralArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.StringArgumentAndGenericLiteralArgument
stringArgumentAndGenericLiteralArgumentWithLiteral original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
              Core.projectionField = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringArgumentAndGenericLiteralArgumentWithString :: Phantoms.TTerm Gremlin.StringArgumentAndGenericLiteralArgument -> Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.StringArgumentAndGenericLiteralArgument
stringArgumentAndGenericLiteralArgumentWithString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument"),
              Core.projectionField = (Core.Name "literal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringArgumentAndOptionalGenericLiteralArgument :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm (Maybe Gremlin.GenericLiteralArgument) -> Phantoms.TTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument
stringArgumentAndOptionalGenericLiteralArgument string literal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTTerm literal)}]}))

stringArgumentAndOptionalGenericLiteralArgumentLiteral :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument -> Phantoms.TTerm (Maybe Gremlin.GenericLiteralArgument)
stringArgumentAndOptionalGenericLiteralArgumentLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
        Core.projectionField = (Core.Name "literal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringArgumentAndOptionalGenericLiteralArgumentString :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument -> Phantoms.TTerm Gremlin.StringArgument
stringArgumentAndOptionalGenericLiteralArgumentString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
        Core.projectionField = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringArgumentAndOptionalGenericLiteralArgumentWithLiteral :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument -> Phantoms.TTerm (Maybe Gremlin.GenericLiteralArgument) -> Phantoms.TTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument
stringArgumentAndOptionalGenericLiteralArgumentWithLiteral original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
              Core.projectionField = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringArgumentAndOptionalGenericLiteralArgumentWithString :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument -> Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument
stringArgumentAndOptionalGenericLiteralArgumentWithString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
              Core.projectionField = (Core.Name "literal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringArgumentAndOptionalStringLiteralVarargs :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs
stringArgumentAndOptionalStringLiteralVarargs first rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

stringArgumentAndOptionalStringLiteralVarargsFirst :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm Gremlin.StringArgument
stringArgumentAndOptionalStringLiteralVarargsFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
        Core.projectionField = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringArgumentAndOptionalStringLiteralVarargsRest :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm [Gremlin.StringNullableArgument]
stringArgumentAndOptionalStringLiteralVarargsRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
        Core.projectionField = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringArgumentAndOptionalStringLiteralVarargsWithFirst :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs
stringArgumentAndOptionalStringLiteralVarargsWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
              Core.projectionField = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringArgumentAndOptionalStringLiteralVarargsWithRest :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs
stringArgumentAndOptionalStringLiteralVarargsWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
              Core.projectionField = (Core.Name "first")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringArgumentOrNestedTraversalString :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.StringArgumentOrNestedTraversal
stringArgumentOrNestedTraversalString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringArgumentOrNestedTraversalTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.StringArgumentOrNestedTraversal
stringArgumentOrNestedTraversalTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringArgumentValue :: Phantoms.TTerm String -> Phantoms.TTerm Gremlin.StringArgument
stringArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.StringArgument
stringArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringNullableArgumentAndGenericLiteralArgument :: Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument
stringNullableArgumentAndGenericLiteralArgument string literal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTTerm literal)}]}))

stringNullableArgumentAndGenericLiteralArgumentLiteral :: Phantoms.TTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument
stringNullableArgumentAndGenericLiteralArgumentLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
        Core.projectionField = (Core.Name "literal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringNullableArgumentAndGenericLiteralArgumentString :: Phantoms.TTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument -> Phantoms.TTerm Gremlin.StringNullableArgument
stringNullableArgumentAndGenericLiteralArgumentString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
        Core.projectionField = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringNullableArgumentAndGenericLiteralArgumentWithLiteral :: Phantoms.TTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument
stringNullableArgumentAndGenericLiteralArgumentWithLiteral original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
              Core.projectionField = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringNullableArgumentAndGenericLiteralArgumentWithString :: Phantoms.TTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument -> Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.StringNullableArgumentAndGenericLiteralArgument
stringNullableArgumentAndGenericLiteralArgumentWithString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument"),
              Core.projectionField = (Core.Name "literal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringNullableArgumentAndTraversalPredicate :: Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.StringNullableArgumentAndTraversalPredicate
stringNullableArgumentAndTraversalPredicate string predicate =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm predicate)}]}))

stringNullableArgumentAndTraversalPredicatePredicate :: Phantoms.TTerm Gremlin.StringNullableArgumentAndTraversalPredicate -> Phantoms.TTerm Gremlin.TraversalPredicate
stringNullableArgumentAndTraversalPredicatePredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
        Core.projectionField = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringNullableArgumentAndTraversalPredicateString :: Phantoms.TTerm Gremlin.StringNullableArgumentAndTraversalPredicate -> Phantoms.TTerm Gremlin.StringNullableArgument
stringNullableArgumentAndTraversalPredicateString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
        Core.projectionField = (Core.Name "string")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringNullableArgumentAndTraversalPredicateWithPredicate :: Phantoms.TTerm Gremlin.StringNullableArgumentAndTraversalPredicate -> Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.StringNullableArgumentAndTraversalPredicate
stringNullableArgumentAndTraversalPredicateWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
              Core.projectionField = (Core.Name "string")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringNullableArgumentAndTraversalPredicateWithString :: Phantoms.TTerm Gremlin.StringNullableArgumentAndTraversalPredicate -> Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.StringNullableArgumentAndTraversalPredicate
stringNullableArgumentAndTraversalPredicateWithString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate"),
              Core.projectionField = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringNullableArgumentValue :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Gremlin.StringNullableArgument
stringNullableArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringNullableArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.StringNullableArgument
stringNullableArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringNullableArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringRange :: Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Gremlin.StringRange
stringRange left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

stringRangeLeft :: Phantoms.TTerm Gremlin.StringRange -> Phantoms.TTerm String
stringRangeLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringRangeRight :: Phantoms.TTerm Gremlin.StringRange -> Phantoms.TTerm String
stringRangeRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringRangeWithLeft :: Phantoms.TTerm Gremlin.StringRange -> Phantoms.TTerm String -> Phantoms.TTerm Gremlin.StringRange
stringRangeWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringRangeWithRight :: Phantoms.TTerm Gremlin.StringRange -> Phantoms.TTerm String -> Phantoms.TTerm Gremlin.StringRange
stringRangeWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StringRange"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

structureVertex :: Phantoms.TTerm Bool -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.StructureVertex
structureVertex new id label =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Phantoms.unTTerm new)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)}]}))

structureVertexArgumentValue :: Phantoms.TTerm Gremlin.StructureVertex -> Phantoms.TTerm Gremlin.StructureVertexArgument
structureVertexArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertexArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

structureVertexArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.StructureVertexArgument
structureVertexArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertexArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

structureVertexId :: Phantoms.TTerm Gremlin.StructureVertex -> Phantoms.TTerm Gremlin.GenericLiteralArgument
structureVertexId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structureVertexLabel :: Phantoms.TTerm Gremlin.StructureVertex -> Phantoms.TTerm Gremlin.StringArgument
structureVertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structureVertexNew :: Phantoms.TTerm Gremlin.StructureVertex -> Phantoms.TTerm Bool
structureVertexNew x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
        Core.projectionField = (Core.Name "new")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structureVertexWithId :: Phantoms.TTerm Gremlin.StructureVertex -> Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.StructureVertex
structureVertexWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionField = (Core.Name "new")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structureVertexWithLabel :: Phantoms.TTerm Gremlin.StructureVertex -> Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.StructureVertex
structureVertexWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionField = (Core.Name "new")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

structureVertexWithNew :: Phantoms.TTerm Gremlin.StructureVertex -> Phantoms.TTerm Bool -> Phantoms.TTerm Gremlin.StructureVertex
structureVertexWithNew original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.StructureVertex"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

substringArgs :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm (Maybe Gremlin.IntegerArgument) -> Phantoms.TTerm Gremlin.SubstringArgs
substringArgs scope start end =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTTerm start)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTTerm end)}]}))

substringArgsEnd :: Phantoms.TTerm Gremlin.SubstringArgs -> Phantoms.TTerm (Maybe Gremlin.IntegerArgument)
substringArgsEnd x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
        Core.projectionField = (Core.Name "end")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

substringArgsScope :: Phantoms.TTerm Gremlin.SubstringArgs -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument)
substringArgsScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
        Core.projectionField = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

substringArgsStart :: Phantoms.TTerm Gremlin.SubstringArgs -> Phantoms.TTerm Gremlin.IntegerArgument
substringArgsStart x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
        Core.projectionField = (Core.Name "start")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

substringArgsWithEnd :: Phantoms.TTerm Gremlin.SubstringArgs -> Phantoms.TTerm (Maybe Gremlin.IntegerArgument) -> Phantoms.TTerm Gremlin.SubstringArgs
substringArgsWithEnd original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionField = (Core.Name "start")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

substringArgsWithScope :: Phantoms.TTerm Gremlin.SubstringArgs -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.SubstringArgs
substringArgsWithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionField = (Core.Name "start")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionField = (Core.Name "end")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

substringArgsWithStart :: Phantoms.TTerm Gremlin.SubstringArgs -> Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.SubstringArgs
substringArgsWithStart original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"),
              Core.projectionField = (Core.Name "end")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tailArgs :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm (Maybe Gremlin.IntegerArgument) -> Phantoms.TTerm Gremlin.TailArgs
tailArgs scope integer =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "integer"),
          Core.fieldTerm = (Phantoms.unTTerm integer)}]}))

tailArgsInteger :: Phantoms.TTerm Gremlin.TailArgs -> Phantoms.TTerm (Maybe Gremlin.IntegerArgument)
tailArgsInteger x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
        Core.projectionField = (Core.Name "integer")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tailArgsScope :: Phantoms.TTerm Gremlin.TailArgs -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument)
tailArgsScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
        Core.projectionField = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tailArgsWithInteger :: Phantoms.TTerm Gremlin.TailArgs -> Phantoms.TTerm (Maybe Gremlin.IntegerArgument) -> Phantoms.TTerm Gremlin.TailArgs
tailArgsWithInteger original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tailArgsWithScope :: Phantoms.TTerm Gremlin.TailArgs -> Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TailArgs
tailArgsWithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "integer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TailArgs"),
              Core.projectionField = (Core.Name "integer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

terminatedTraversal :: Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm Gremlin.TraversalTerminalMethod -> Phantoms.TTerm Gremlin.TerminatedTraversal
terminatedTraversal root terminal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Phantoms.unTTerm root)},
        Core.Field {
          Core.fieldName = (Core.Name "terminal"),
          Core.fieldTerm = (Phantoms.unTTerm terminal)}]}))

terminatedTraversalRoot :: Phantoms.TTerm Gremlin.TerminatedTraversal -> Phantoms.TTerm Gremlin.RootTraversal
terminatedTraversalRoot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
        Core.projectionField = (Core.Name "root")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

terminatedTraversalTerminal :: Phantoms.TTerm Gremlin.TerminatedTraversal -> Phantoms.TTerm Gremlin.TraversalTerminalMethod
terminatedTraversalTerminal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
        Core.projectionField = (Core.Name "terminal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

terminatedTraversalWithRoot :: Phantoms.TTerm Gremlin.TerminatedTraversal -> Phantoms.TTerm Gremlin.RootTraversal -> Phantoms.TTerm Gremlin.TerminatedTraversal
terminatedTraversalWithRoot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "terminal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
              Core.projectionField = (Core.Name "terminal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

terminatedTraversalWithTerminal :: Phantoms.TTerm Gremlin.TerminatedTraversal -> Phantoms.TTerm Gremlin.TraversalTerminalMethod -> Phantoms.TTerm Gremlin.TerminatedTraversal
terminatedTraversalWithTerminal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"),
              Core.projectionField = (Core.Name "root")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "terminal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

toArgsDirection :: Phantoms.TTerm Gremlin.DirectionAndVarargs -> Phantoms.TTerm Gremlin.ToArgs
toArgsDirection x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ToArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "direction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

toArgsString :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.ToArgs
toArgsString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ToArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

toArgsTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.ToArgs
toArgsTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ToArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

toArgsVertex :: Phantoms.TTerm Gremlin.StructureVertexArgument -> Phantoms.TTerm Gremlin.ToArgs
toArgsVertex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ToArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

transactionPartBegin :: Phantoms.TTerm Gremlin.TransactionPart
transactionPartBegin =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TransactionPart"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "begin"),
        Core.fieldTerm = Core.TermUnit}}))

transactionPartCommit :: Phantoms.TTerm Gremlin.TransactionPart
transactionPartCommit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TransactionPart"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "commit"),
        Core.fieldTerm = Core.TermUnit}}))

transactionPartRollback :: Phantoms.TTerm Gremlin.TransactionPart
transactionPartRollback =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TransactionPart"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rollback"),
        Core.fieldTerm = Core.TermUnit}}))

traversalBiFunctionArgumentValue :: Phantoms.TTerm Gremlin.TraversalOperator -> Phantoms.TTerm Gremlin.TraversalBiFunctionArgument
traversalBiFunctionArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalBiFunctionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalBiFunctionArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalBiFunctionArgument
traversalBiFunctionArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalBiFunctionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalCardinalityArgumentAndObjects :: Phantoms.TTerm Gremlin.TraversalCardinalityArgument -> Phantoms.TTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TTerm Gremlin.TraversalCardinalityArgumentAndObjects
traversalCardinalityArgumentAndObjects cardinality objects =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm cardinality)},
        Core.Field {
          Core.fieldName = (Core.Name "objects"),
          Core.fieldTerm = (Phantoms.unTTerm objects)}]}))

traversalCardinalityArgumentAndObjectsCardinality :: Phantoms.TTerm Gremlin.TraversalCardinalityArgumentAndObjects -> Phantoms.TTerm Gremlin.TraversalCardinalityArgument
traversalCardinalityArgumentAndObjectsCardinality x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
        Core.projectionField = (Core.Name "cardinality")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalCardinalityArgumentAndObjectsObjects :: Phantoms.TTerm Gremlin.TraversalCardinalityArgumentAndObjects -> Phantoms.TTerm [Gremlin.GenericLiteralArgument]
traversalCardinalityArgumentAndObjectsObjects x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
        Core.projectionField = (Core.Name "objects")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalCardinalityArgumentAndObjectsWithCardinality :: Phantoms.TTerm Gremlin.TraversalCardinalityArgumentAndObjects -> Phantoms.TTerm Gremlin.TraversalCardinalityArgument -> Phantoms.TTerm Gremlin.TraversalCardinalityArgumentAndObjects
traversalCardinalityArgumentAndObjectsWithCardinality original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "objects"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
              Core.projectionField = (Core.Name "objects")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traversalCardinalityArgumentAndObjectsWithObjects :: Phantoms.TTerm Gremlin.TraversalCardinalityArgumentAndObjects -> Phantoms.TTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TTerm Gremlin.TraversalCardinalityArgumentAndObjects
traversalCardinalityArgumentAndObjectsWithObjects original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects"),
              Core.projectionField = (Core.Name "cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "objects"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traversalCardinalityArgumentValue :: Phantoms.TTerm Gremlin.TraversalCardinality -> Phantoms.TTerm Gremlin.TraversalCardinalityArgument
traversalCardinalityArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalCardinalityArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalCardinalityArgument
traversalCardinalityArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalCardinalityList :: Phantoms.TTerm Gremlin.GenericLiteral -> Phantoms.TTerm Gremlin.TraversalCardinality
traversalCardinalityList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalCardinalitySet :: Phantoms.TTerm Gremlin.GenericLiteral -> Phantoms.TTerm Gremlin.TraversalCardinality
traversalCardinalitySet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalCardinalitySingle :: Phantoms.TTerm Gremlin.GenericLiteral -> Phantoms.TTerm Gremlin.TraversalCardinality
traversalCardinalitySingle x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalCardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalColumnArgumentValue :: Phantoms.TTerm Gremlin.TraversalColumn -> Phantoms.TTerm Gremlin.TraversalColumnArgument
traversalColumnArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalColumnArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalColumnArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalColumnArgument
traversalColumnArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalColumnArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalColumnKeys :: Phantoms.TTerm Gremlin.TraversalColumn
traversalColumnKeys =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalColumn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keys"),
        Core.fieldTerm = Core.TermUnit}}))

traversalColumnValues :: Phantoms.TTerm Gremlin.TraversalColumn
traversalColumnValues =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalColumn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "values"),
        Core.fieldTerm = Core.TermUnit}}))

traversalComparatorArgumentValue :: Phantoms.TTerm Gremlin.TraversalOrder -> Phantoms.TTerm Gremlin.TraversalComparatorArgument
traversalComparatorArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalComparatorArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalComparatorArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalComparatorArgument
traversalComparatorArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalComparatorArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalDTArgumentValue :: Phantoms.TTerm Gremlin.TraversalDT -> Phantoms.TTerm Gremlin.TraversalDTArgument
traversalDTArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDTArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalDTArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalDTArgument
traversalDTArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDTArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalDTDay :: Phantoms.TTerm Gremlin.TraversalDT
traversalDTDay =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDT"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "day"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDTHour :: Phantoms.TTerm Gremlin.TraversalDT
traversalDTHour =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDT"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hour"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDTMinute :: Phantoms.TTerm Gremlin.TraversalDT
traversalDTMinute =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDT"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minute"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDTSecond :: Phantoms.TTerm Gremlin.TraversalDT
traversalDTSecond =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDT"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "second"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDirectionArgumentValue :: Phantoms.TTerm Gremlin.TraversalDirection -> Phantoms.TTerm Gremlin.TraversalDirectionArgument
traversalDirectionArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDirectionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalDirectionArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalDirectionArgument
traversalDirectionArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDirectionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalDirectionBoth :: Phantoms.TTerm Gremlin.TraversalDirection
traversalDirectionBoth =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDirectionIn :: Phantoms.TTerm Gremlin.TraversalDirection
traversalDirectionIn =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))

traversalDirectionOut :: Phantoms.TTerm Gremlin.TraversalDirection
traversalDirectionOut =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "out"),
        Core.fieldTerm = Core.TermUnit}}))

traversalFunctionArgumentOrStringArgumentOrNestedTraversalFunction :: Phantoms.TTerm Gremlin.TraversalFunctionArgument -> Phantoms.TTerm Gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal
traversalFunctionArgumentOrStringArgumentOrNestedTraversalFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalFunctionArgumentOrStringArgumentOrNestedTraversalString :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal
traversalFunctionArgumentOrStringArgumentOrNestedTraversalString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalFunctionArgumentOrStringArgumentOrNestedTraversalTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal
traversalFunctionArgumentOrStringArgumentOrNestedTraversalTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalFunctionArgumentValue :: Phantoms.TTerm Gremlin.TraversalFunction -> Phantoms.TTerm Gremlin.TraversalFunctionArgument
traversalFunctionArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalFunctionArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalFunctionArgument
traversalFunctionArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalFunctionColumn :: Phantoms.TTerm Gremlin.TraversalColumn -> Phantoms.TTerm Gremlin.TraversalFunction
traversalFunctionColumn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "column"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalFunctionToken :: Phantoms.TTerm Gremlin.TraversalToken -> Phantoms.TTerm Gremlin.TraversalFunction
traversalFunctionToken x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "token"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMergeArgumentAndGenericLiteralMapNullableArgument :: Phantoms.TTerm Gremlin.TraversalMergeArgument -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TTerm (Maybe Gremlin.TraversalCardinality) -> Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgument merge map cardinality =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Phantoms.unTTerm merge)},
        Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = (Phantoms.unTTerm map)},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm cardinality)}]}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentCardinality :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TTerm (Maybe Gremlin.TraversalCardinality)
traversalMergeArgumentAndGenericLiteralMapNullableArgumentCardinality x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
        Core.projectionField = (Core.Name "cardinality")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentMap :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgumentMap x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
        Core.projectionField = (Core.Name "map")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentMerge :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TTerm Gremlin.TraversalMergeArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgumentMerge x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
        Core.projectionField = (Core.Name "merge")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithCardinality :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TTerm (Maybe Gremlin.TraversalCardinality) -> Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithCardinality original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionField = (Core.Name "merge")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionField = (Core.Name "map")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithMap :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgument -> Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithMap original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionField = (Core.Name "merge")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionField = (Core.Name "cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithMerge :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument -> Phantoms.TTerm Gremlin.TraversalMergeArgument -> Phantoms.TTerm Gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument
traversalMergeArgumentAndGenericLiteralMapNullableArgumentWithMerge original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionField = (Core.Name "map")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
              Core.projectionField = (Core.Name "cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traversalMergeArgumentAndNestedTraversal :: Phantoms.TTerm Gremlin.TraversalMergeArgument -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalMergeArgumentAndNestedTraversal
traversalMergeArgumentAndNestedTraversal merge traversal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Phantoms.unTTerm merge)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTTerm traversal)}]}))

traversalMergeArgumentAndNestedTraversalMerge :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.TraversalMergeArgument
traversalMergeArgumentAndNestedTraversalMerge x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
        Core.projectionField = (Core.Name "merge")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalMergeArgumentAndNestedTraversalTraversal :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal
traversalMergeArgumentAndNestedTraversalTraversal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
        Core.projectionField = (Core.Name "traversal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalMergeArgumentAndNestedTraversalWithMerge :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.TraversalMergeArgument -> Phantoms.TTerm Gremlin.TraversalMergeArgumentAndNestedTraversal
traversalMergeArgumentAndNestedTraversalWithMerge original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
              Core.projectionField = (Core.Name "traversal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traversalMergeArgumentAndNestedTraversalWithTraversal :: Phantoms.TTerm Gremlin.TraversalMergeArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalMergeArgumentAndNestedTraversal
traversalMergeArgumentAndNestedTraversalWithTraversal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "merge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal"),
              Core.projectionField = (Core.Name "merge")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traversalMergeArgumentValue :: Phantoms.TTerm Gremlin.TraversalMerge -> Phantoms.TTerm Gremlin.TraversalMergeArgument
traversalMergeArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMergeArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalMergeArgument
traversalMergeArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMergeInV :: Phantoms.TTerm Gremlin.TraversalMerge
traversalMergeInV =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMerge"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMergeOnCreate :: Phantoms.TTerm Gremlin.TraversalMerge
traversalMergeOnCreate =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMerge"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "onCreate"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMergeOnMatch :: Phantoms.TTerm Gremlin.TraversalMerge
traversalMergeOnMatch =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMerge"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "onMatch"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMergeOutV :: Phantoms.TTerm Gremlin.TraversalMerge
traversalMergeOutV =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMerge"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodAddE :: Phantoms.TTerm Gremlin.StringArgumentOrNestedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodAddE x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addE"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodAddV :: Phantoms.TTerm (Maybe Gremlin.StringArgumentOrNestedTraversal) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodAddV x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addV"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodAggregate :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndStringArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodAggregate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "aggregate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodAll :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodAll x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodAnd :: Phantoms.TTerm [Gremlin.NestedTraversal] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodAnd x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodAny :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodAny x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodAs :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodAs x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodAsDate :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodAsDate =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "asDate"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodAsString :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodAsString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "asString"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodBarrier :: Phantoms.TTerm (Maybe Gremlin.TraversalSackMethodArgumentOrIntegerArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodBarrier x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "barrier"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodBoth :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodBoth x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodBothE :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodBothE x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bothE"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodBothV :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodBothV =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bothV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodBranch :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodBranch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "branch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodBy :: Phantoms.TTerm Gremlin.ByArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodBy x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "by"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodCall :: Phantoms.TTerm Gremlin.ServiceCall -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodCall x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodCap :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodCap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodChoose :: Phantoms.TTerm Gremlin.ChooseArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodChoose x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "choose"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodCoalesce :: Phantoms.TTerm [Gremlin.NestedTraversal] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodCoalesce x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "coalesce"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodCoin :: Phantoms.TTerm Gremlin.FloatArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodCoin x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "coin"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodCombine :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodCombine x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "combine"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodConcat :: Phantoms.TTerm Gremlin.ConcatArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodConcat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "concat"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodConjoin :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodConjoin x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conjoin"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodConnectedComponent :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodConnectedComponent =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "connectedComponent"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodConstant :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodConstant x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodCount :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodCount x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "count"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodCyclicPath :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodCyclicPath =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cyclicPath"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodDateAdd :: Phantoms.TTerm Gremlin.DateAddArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodDateAdd x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dateAdd"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodDateDiff :: Phantoms.TTerm Gremlin.DateDiffArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodDateDiff x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dateDiff"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodDedup :: Phantoms.TTerm Gremlin.DedupArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodDedup x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dedup"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodDifference :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodDifference x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "difference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodDisjunct :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodDisjunct x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjunct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodDrop :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodDrop =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "drop"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodE :: Phantoms.TTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodE x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "e"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodElement :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodElement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "element"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodElementMap :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodElementMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elementMap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodEmit :: Phantoms.TTerm (Maybe Gremlin.PredicateOrTraversal) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodEmit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodFail :: Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodFail x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fail"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodFilter :: Phantoms.TTerm Gremlin.PredicateOrTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodFilter x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "filter"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodFlatMap :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodFlatMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "flatMap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodFold :: Phantoms.TTerm (Maybe Gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodFold x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fold"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodFormat :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodFormat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "format"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodFrom :: Phantoms.TTerm Gremlin.FromArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodFrom x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "from"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodGroup :: Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodGroup x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodGroupCount :: Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodGroupCount x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "groupCount"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodHas :: Phantoms.TTerm Gremlin.HasArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodHas x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "has"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodHasId :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndTraversalPredicate -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodHasId x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasId"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodHasKey :: Phantoms.TTerm Gremlin.TraversalPredicateOrStringLiteralVarargs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodHasKey x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasKey"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodHasLabel :: Phantoms.TTerm Gremlin.TraversalPredicateOrStringLiteralVarargs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodHasLabel x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasLabel"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodHasNot :: Phantoms.TTerm Gremlin.StringNullableArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodHasNot x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasNot"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodHasValue :: Phantoms.TTerm Gremlin.TraversalPredicateOrGenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodHasValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodId :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodId =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "id"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodIdentity :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodIdentity =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identity"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodIn :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodIn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodInE :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodInE x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inE"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodInV :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodInV =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodIndex :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodIndex =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "index"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodInject :: Phantoms.TTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodInject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodIntersect :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodIntersect x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "intersect"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodIs :: Phantoms.TTerm Gremlin.TraversalPredicateOrGenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodIs x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "is"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodKey :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodKey =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "key"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodLTrim :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodLTrim x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lTrim"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodLabel :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodLabel =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "label"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodLength :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodLength x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "length"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodLimit :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodLimit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodLocal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodLocal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodLoops :: Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodLoops x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "loops"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodMap :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodMatch :: Phantoms.TTerm [Gremlin.NestedTraversal] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodMatch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodMath :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodMath x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "math"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodMax :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodMax x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "max"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodMean :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodMean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodMerge :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodMerge x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "merge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodMergeE :: Phantoms.TTerm (Maybe Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodMergeE x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeE"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodMergeV :: Phantoms.TTerm (Maybe Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodMergeV x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeV"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodMin :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodMin x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "min"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodNone :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodNone x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodNot :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodNot x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodOption :: Phantoms.TTerm Gremlin.OptionArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodOption x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "option"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodOptional :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodOptional x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodOr :: Phantoms.TTerm [Gremlin.NestedTraversal] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodOr x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodOrder :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodOrder x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "order"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodOtherV :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodOtherV =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "otherV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodOut :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodOut x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "out"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodOutE :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodOutE x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outE"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodOutV :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodOutV =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outV"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodPageRank :: Phantoms.TTerm (Maybe Gremlin.FloatArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodPageRank x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pageRank"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodPath :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodPath =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "path"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodPeerPressure :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodPeerPressure =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "peerPressure"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodProduct :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodProduct x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "product"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodProfile :: Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodProfile x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "profile"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodProject :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalStringLiteralVarargs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodProject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodProperties :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodProperties x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "properties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodProperty :: Phantoms.TTerm Gremlin.PropertyArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodProperty x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodPropertyMap :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodPropertyMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyMap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodRTrim :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodRTrim x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rTrim"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodRange :: Phantoms.TTerm Gremlin.RangeArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodRange x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodRead :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodRead =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "read"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodRepeat :: Phantoms.TTerm Gremlin.OptionalStringArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodRepeat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeat"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodReplace :: Phantoms.TTerm Gremlin.ReplaceArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodReplace x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "replace"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodReverse :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodReverse =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reverse"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodSack :: Phantoms.TTerm (Maybe Gremlin.TraversalBiFunctionArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodSack x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sack"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodSample :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodSample x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sample"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodSelect :: Phantoms.TTerm Gremlin.SelectArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodSelect x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodShortestPath :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodShortestPath =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shortestPath"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodSideEffect :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodSideEffect x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sideEffect"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodSimplePath :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodSimplePath =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simplePath"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodSkip :: Phantoms.TTerm Gremlin.OptionalTraversalScopeArgumentAndIntegerArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodSkip x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "skip"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodSplit :: Phantoms.TTerm Gremlin.SplitArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodSplit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "split"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodStore :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodStore x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "store"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodSubgraph :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodSubgraph x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subgraph"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodSubstring :: Phantoms.TTerm Gremlin.SubstringArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodSubstring x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "substring"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodSum :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodSum x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodTail :: Phantoms.TTerm (Maybe Gremlin.TailArgs) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodTail x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tail"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodTimes :: Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodTimes x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodTo :: Phantoms.TTerm Gremlin.ToArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodTo x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "to"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodToE :: Phantoms.TTerm Gremlin.DirectionAndVarargs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodToE x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toE"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodToLower :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodToLower x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toLower"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodToUpper :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodToUpper x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toUpper"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodToV :: Phantoms.TTerm Gremlin.TraversalDirectionArgument -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodToV x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toV"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodTree :: Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodTree x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tree"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodTrim :: Phantoms.TTerm (Maybe Gremlin.TraversalScopeArgument) -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodTrim x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trim"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodUnfold :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodUnfold =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unfold"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodUnion :: Phantoms.TTerm [Gremlin.NestedTraversal] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodUnion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodUntil :: Phantoms.TTerm Gremlin.PredicateOrTraversal -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodUntil x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "until"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodV :: Phantoms.TTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodV x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "v"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodValue :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodValue =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = Core.TermUnit}}))

traversalMethodValueMap :: Phantoms.TTerm Gremlin.ValueMapArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodValueMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "valueMap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodValues :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodValues x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "values"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodWhere :: Phantoms.TTerm Gremlin.WhereArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodWhere x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "where"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodWith :: Phantoms.TTerm Gremlin.WithArgs -> Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalMethodWrite :: Phantoms.TTerm Gremlin.TraversalMethod
traversalMethodWrite =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "write"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorAddAll :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorAddAll =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addAll"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorAnd :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorAnd =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorAssign :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorAssign =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorDiv :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorDiv =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "div"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorMax :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorMax =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "max"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorMin :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorMin =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "min"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorMinus :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorMinus =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorMult :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorMult =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mult"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorOr :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorOr =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorSum :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorSum =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sum"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOperatorSumLong :: Phantoms.TTerm Gremlin.TraversalOperator
traversalOperatorSumLong =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sumLong"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderArgumentValue :: Phantoms.TTerm Gremlin.TraversalOrder -> Phantoms.TTerm Gremlin.TraversalOrderArgument
traversalOrderArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrderArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalOrderArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalOrderArgument
traversalOrderArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrderArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalOrderAsc :: Phantoms.TTerm Gremlin.TraversalOrder
traversalOrderAsc =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "asc"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderDecr :: Phantoms.TTerm Gremlin.TraversalOrder
traversalOrderDecr =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decr"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderDesc :: Phantoms.TTerm Gremlin.TraversalOrder
traversalOrderDesc =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "desc"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderIncr :: Phantoms.TTerm Gremlin.TraversalOrder
traversalOrderIncr =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "incr"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderShuffle :: Phantoms.TTerm Gremlin.TraversalOrder
traversalOrderShuffle =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shuffle"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPickAny :: Phantoms.TTerm Gremlin.TraversalPick
traversalPickAny =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPick"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPickNone :: Phantoms.TTerm Gremlin.TraversalPick
traversalPickNone =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPick"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPopAll :: Phantoms.TTerm Gremlin.TraversalPop
traversalPopAll =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPop"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPopArgumentAndNestedTraversal :: Phantoms.TTerm Gremlin.TraversalPopArgument -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalPopArgumentAndNestedTraversal
traversalPopArgumentAndNestedTraversal pop traversal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Phantoms.unTTerm pop)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTTerm traversal)}]}))

traversalPopArgumentAndNestedTraversalPop :: Phantoms.TTerm Gremlin.TraversalPopArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.TraversalPopArgument
traversalPopArgumentAndNestedTraversalPop x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
        Core.projectionField = (Core.Name "pop")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalPopArgumentAndNestedTraversalTraversal :: Phantoms.TTerm Gremlin.TraversalPopArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal
traversalPopArgumentAndNestedTraversalTraversal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
        Core.projectionField = (Core.Name "traversal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalPopArgumentAndNestedTraversalWithPop :: Phantoms.TTerm Gremlin.TraversalPopArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.TraversalPopArgument -> Phantoms.TTerm Gremlin.TraversalPopArgumentAndNestedTraversal
traversalPopArgumentAndNestedTraversalWithPop original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
              Core.projectionField = (Core.Name "traversal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traversalPopArgumentAndNestedTraversalWithTraversal :: Phantoms.TTerm Gremlin.TraversalPopArgumentAndNestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalPopArgumentAndNestedTraversal
traversalPopArgumentAndNestedTraversalWithTraversal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pop"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal"),
              Core.projectionField = (Core.Name "pop")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traversalPopArgumentValue :: Phantoms.TTerm Gremlin.TraversalPop -> Phantoms.TTerm Gremlin.TraversalPopArgument
traversalPopArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPopArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalPopArgument
traversalPopArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPopFirst :: Phantoms.TTerm Gremlin.TraversalPop
traversalPopFirst =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPop"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "first"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPopLast :: Phantoms.TTerm Gremlin.TraversalPop
traversalPopLast =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPop"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "last"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPopMixed :: Phantoms.TTerm Gremlin.TraversalPop
traversalPopMixed =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPop"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mixed"),
        Core.fieldTerm = Core.TermUnit}}))

traversalPredicateAnd :: Phantoms.TTerm Gremlin.TwoTraversalPredicates -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateAnd x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateAndNestedTraversal :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalPredicateAndNestedTraversal
traversalPredicateAndNestedTraversal predicate traversal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTTerm traversal)}]}))

traversalPredicateAndNestedTraversalPredicate :: Phantoms.TTerm Gremlin.TraversalPredicateAndNestedTraversal -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateAndNestedTraversalPredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
        Core.projectionField = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalPredicateAndNestedTraversalTraversal :: Phantoms.TTerm Gremlin.TraversalPredicateAndNestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal
traversalPredicateAndNestedTraversalTraversal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
        Core.projectionField = (Core.Name "traversal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalPredicateAndNestedTraversalWithPredicate :: Phantoms.TTerm Gremlin.TraversalPredicateAndNestedTraversal -> Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TraversalPredicateAndNestedTraversal
traversalPredicateAndNestedTraversalWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
              Core.projectionField = (Core.Name "traversal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traversalPredicateAndNestedTraversalWithTraversal :: Phantoms.TTerm Gremlin.TraversalPredicateAndNestedTraversal -> Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.TraversalPredicateAndNestedTraversal
traversalPredicateAndNestedTraversalWithTraversal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal"),
              Core.projectionField = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traversalPredicateBetween :: Phantoms.TTerm Gremlin.RangeArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateBetween x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "between"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateContaining :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateContaining x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "containing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateEndingWith :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateEndingWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endingWith"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateEq :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateEq x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eq"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateGt :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateGt x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gt"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateGte :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateGte x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gte"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateInside :: Phantoms.TTerm Gremlin.RangeArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateInside x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inside"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateLt :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateLt x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lt"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateLte :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateLte x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lte"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateNegate :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateNegate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateNeq :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateNeq x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "neq"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateNot :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateNot x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateNotContaining :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateNotContaining x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notContaining"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateNotEndingWith :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateNotEndingWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEndingWith"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateNotRegex :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateNotRegex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notRegex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateNotStartingWith :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateNotStartingWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notStartingWith"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateOr :: Phantoms.TTerm Gremlin.TwoTraversalPredicates -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateOr x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateOrGenericLiteralArgumentLiteral :: Phantoms.TTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TTerm Gremlin.TraversalPredicateOrGenericLiteralArgument
traversalPredicateOrGenericLiteralArgumentLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateOrGenericLiteralArgumentPredicate :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TraversalPredicateOrGenericLiteralArgument
traversalPredicateOrGenericLiteralArgumentPredicate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateOrStringLiteralVarargsPredicate :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TraversalPredicateOrStringLiteralVarargs
traversalPredicateOrStringLiteralVarargsPredicate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateOrStringLiteralVarargsString :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.TraversalPredicateOrStringLiteralVarargs
traversalPredicateOrStringLiteralVarargsString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateOutside :: Phantoms.TTerm Gremlin.RangeArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateOutside x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outside"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateRegex :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateRegex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateStartingWith :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateStartingWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "startingWith"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateWithin :: Phantoms.TTerm (Maybe Gremlin.GenericLiteralArgument) -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateWithin x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "within"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalPredicateWithout :: Phantoms.TTerm (Maybe Gremlin.GenericLiteralArgument) -> Phantoms.TTerm Gremlin.TraversalPredicate
traversalPredicateWithout x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "without"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSackMethodArgumentOrIntegerArgumentConsumer :: Phantoms.TTerm Gremlin.TraversalSackMethodArgument -> Phantoms.TTerm Gremlin.TraversalSackMethodArgumentOrIntegerArgument
traversalSackMethodArgumentOrIntegerArgumentConsumer x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "consumer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSackMethodArgumentOrIntegerArgumentInt :: Phantoms.TTerm Gremlin.IntegerArgument -> Phantoms.TTerm Gremlin.TraversalSackMethodArgumentOrIntegerArgument
traversalSackMethodArgumentOrIntegerArgumentInt x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSackMethodArgumentValue :: Phantoms.TTerm Gremlin.TraversalSackMethodArgument
traversalSackMethodArgumentValue =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSackMethodArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = Core.TermUnit}}))

traversalSackMethodArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalSackMethodArgument
traversalSackMethodArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSackMethodArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalScopeArgumentValue :: Phantoms.TTerm Gremlin.TraversalScope -> Phantoms.TTerm Gremlin.TraversalScopeArgument
traversalScopeArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalScopeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalScopeArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalScopeArgument
traversalScopeArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalScopeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalScopeGlobal :: Phantoms.TTerm Gremlin.TraversalScope
traversalScopeGlobal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalScope"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = Core.TermUnit}}))

traversalScopeLocal :: Phantoms.TTerm Gremlin.TraversalScope
traversalScopeLocal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalScope"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = Core.TermUnit}}))

traversalSelfMethodDiscard :: Phantoms.TTerm Gremlin.TraversalSelfMethod
traversalSelfMethodDiscard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "discard"),
        Core.fieldTerm = Core.TermUnit}}))

traversalSource :: Phantoms.TTerm [Gremlin.TraversalSourceSelfMethod] -> Phantoms.TTerm Gremlin.TraversalSource
traversalSource x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSource"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

traversalSourceQuery :: Phantoms.TTerm Gremlin.TraversalSource -> Phantoms.TTerm (Maybe Gremlin.TransactionPart) -> Phantoms.TTerm Gremlin.TraversalSourceQuery
traversalSourceQuery source transactionPart =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "transactionPart"),
          Core.fieldTerm = (Phantoms.unTTerm transactionPart)}]}))

traversalSourceQuerySource :: Phantoms.TTerm Gremlin.TraversalSourceQuery -> Phantoms.TTerm Gremlin.TraversalSource
traversalSourceQuerySource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
        Core.projectionField = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalSourceQueryTransactionPart :: Phantoms.TTerm Gremlin.TraversalSourceQuery -> Phantoms.TTerm (Maybe Gremlin.TransactionPart)
traversalSourceQueryTransactionPart x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
        Core.projectionField = (Core.Name "transactionPart")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalSourceQueryWithSource :: Phantoms.TTerm Gremlin.TraversalSourceQuery -> Phantoms.TTerm Gremlin.TraversalSource -> Phantoms.TTerm Gremlin.TraversalSourceQuery
traversalSourceQueryWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "transactionPart"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
              Core.projectionField = (Core.Name "transactionPart")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traversalSourceQueryWithTransactionPart :: Phantoms.TTerm Gremlin.TraversalSourceQuery -> Phantoms.TTerm (Maybe Gremlin.TransactionPart) -> Phantoms.TTerm Gremlin.TraversalSourceQuery
traversalSourceQueryWithTransactionPart original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "transactionPart"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traversalSourceSelfMethodWith :: Phantoms.TTerm Gremlin.StringArgumentAndOptionalGenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSelfMethodWithBulk :: Phantoms.TTerm Bool -> Phantoms.TTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithBulk x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withBulk"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSelfMethodWithPath :: Phantoms.TTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithPath =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withPath"),
        Core.fieldTerm = Core.TermUnit}}))

traversalSourceSelfMethodWithSack :: Phantoms.TTerm Gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument -> Phantoms.TTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithSack x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withSack"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSelfMethodWithSideEffect :: Phantoms.TTerm Gremlin.StringArgumentAndGenericLiteralArgument -> Phantoms.TTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithSideEffect x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withSideEffect"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSelfMethodWithStrategies :: Phantoms.TTerm [Gremlin.TraversalStrategy] -> Phantoms.TTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithStrategies x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withStrategies"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSelfMethodWithoutStrategies :: Phantoms.TTerm [Gremlin.Identifier] -> Phantoms.TTerm Gremlin.TraversalSourceSelfMethod
traversalSourceSelfMethodWithoutStrategies x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutStrategies"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSpawnMethodAddE :: Phantoms.TTerm Gremlin.StringArgumentOrNestedTraversal -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodAddE x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addE"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSpawnMethodAddV :: Phantoms.TTerm (Maybe Gremlin.StringArgumentOrNestedTraversal) -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodAddV x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addV"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSpawnMethodCall :: Phantoms.TTerm (Maybe Gremlin.ServiceCall) -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodCall x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSpawnMethodE :: Phantoms.TTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodE x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "e"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSpawnMethodInject :: Phantoms.TTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodInject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSpawnMethodIo :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodIo x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "io"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSpawnMethodMergeE :: Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodMergeE x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeE"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSpawnMethodMergeV :: Phantoms.TTerm Gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodMergeV x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mergeV"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSpawnMethodUnion :: Phantoms.TTerm [Gremlin.NestedTraversal] -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodUnion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalSourceSpawnMethodV :: Phantoms.TTerm [Gremlin.GenericLiteralArgument] -> Phantoms.TTerm Gremlin.TraversalSourceSpawnMethod
traversalSourceSpawnMethodV x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "v"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalStrategy :: Phantoms.TTerm Bool -> Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm [Gremlin.Configuration] -> Phantoms.TTerm Gremlin.TraversalStrategy
traversalStrategy new class_ configurations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Phantoms.unTTerm new)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)},
        Core.Field {
          Core.fieldName = (Core.Name "configurations"),
          Core.fieldTerm = (Phantoms.unTTerm configurations)}]}))

traversalStrategyClass :: Phantoms.TTerm Gremlin.TraversalStrategy -> Phantoms.TTerm Gremlin.Identifier
traversalStrategyClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
        Core.projectionField = (Core.Name "class")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalStrategyConfigurations :: Phantoms.TTerm Gremlin.TraversalStrategy -> Phantoms.TTerm [Gremlin.Configuration]
traversalStrategyConfigurations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
        Core.projectionField = (Core.Name "configurations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalStrategyNew :: Phantoms.TTerm Gremlin.TraversalStrategy -> Phantoms.TTerm Bool
traversalStrategyNew x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
        Core.projectionField = (Core.Name "new")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traversalStrategyWithClass :: Phantoms.TTerm Gremlin.TraversalStrategy -> Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalStrategy
traversalStrategyWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionField = (Core.Name "new")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "configurations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionField = (Core.Name "configurations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traversalStrategyWithConfigurations :: Phantoms.TTerm Gremlin.TraversalStrategy -> Phantoms.TTerm [Gremlin.Configuration] -> Phantoms.TTerm Gremlin.TraversalStrategy
traversalStrategyWithConfigurations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionField = (Core.Name "new")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "configurations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traversalStrategyWithNew :: Phantoms.TTerm Gremlin.TraversalStrategy -> Phantoms.TTerm Bool -> Phantoms.TTerm Gremlin.TraversalStrategy
traversalStrategyWithNew original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "new"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "configurations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"),
              Core.projectionField = (Core.Name "configurations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traversalTerminalMethodExplain :: Phantoms.TTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodExplain =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explain"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodHasNext :: Phantoms.TTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodHasNext =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasNext"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodIterate :: Phantoms.TTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodIterate =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iterate"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodNext :: Phantoms.TTerm (Maybe Gremlin.IntegerLiteral) -> Phantoms.TTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodNext x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "next"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalTerminalMethodToBulkSet :: Phantoms.TTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodToBulkSet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toBulkSet"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodToList :: Phantoms.TTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodToList =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toList"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodToSet :: Phantoms.TTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodToSet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "toSet"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTerminalMethodTryNext :: Phantoms.TTerm Gremlin.TraversalTerminalMethod
traversalTerminalMethodTryNext =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tryNext"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTokenArgumentValue :: Phantoms.TTerm Gremlin.TraversalToken -> Phantoms.TTerm Gremlin.TraversalTokenArgument
traversalTokenArgumentValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTokenArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalTokenArgumentVariable :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm Gremlin.TraversalTokenArgument
traversalTokenArgumentVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalTokenArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traversalTokenId :: Phantoms.TTerm Gremlin.TraversalToken
traversalTokenId =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalToken"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "id"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTokenKey :: Phantoms.TTerm Gremlin.TraversalToken
traversalTokenKey =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalToken"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "key"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTokenLabel :: Phantoms.TTerm Gremlin.TraversalToken
traversalTokenLabel =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalToken"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "label"),
        Core.fieldTerm = Core.TermUnit}}))

traversalTokenValue :: Phantoms.TTerm Gremlin.TraversalToken
traversalTokenValue =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TraversalToken"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = Core.TermUnit}}))

twoTraversalPredicates :: Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TwoTraversalPredicates
twoTraversalPredicates left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

twoTraversalPredicatesLeft :: Phantoms.TTerm Gremlin.TwoTraversalPredicates -> Phantoms.TTerm Gremlin.TraversalPredicate
twoTraversalPredicatesLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

twoTraversalPredicatesRight :: Phantoms.TTerm Gremlin.TwoTraversalPredicates -> Phantoms.TTerm Gremlin.TraversalPredicate
twoTraversalPredicatesRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

twoTraversalPredicatesWithLeft :: Phantoms.TTerm Gremlin.TwoTraversalPredicates -> Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TwoTraversalPredicates
twoTraversalPredicatesWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

twoTraversalPredicatesWithRight :: Phantoms.TTerm Gremlin.TwoTraversalPredicates -> Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.TwoTraversalPredicates
twoTraversalPredicatesWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unDateLiteral :: Phantoms.TTerm Gremlin.DateLiteral -> Phantoms.TTerm (Maybe Gremlin.StringArgument)
unDateLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.DateLiteral")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unFloatLiteral :: Phantoms.TTerm Gremlin.FloatLiteral -> Phantoms.TTerm Double
unFloatLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.FloatLiteral")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unGenericLiteralCollection :: Phantoms.TTerm Gremlin.GenericLiteralCollection -> Phantoms.TTerm [Gremlin.GenericLiteral]
unGenericLiteralCollection x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralCollection")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unGenericLiteralList :: Phantoms.TTerm Gremlin.GenericLiteralList -> Phantoms.TTerm [Gremlin.GenericLiteral]
unGenericLiteralList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralList")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unGenericLiteralMap :: Phantoms.TTerm Gremlin.GenericLiteralMap -> Phantoms.TTerm [Gremlin.MapEntry]
unGenericLiteralMap x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMap")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unGenericLiteralSet :: Phantoms.TTerm Gremlin.GenericLiteralSet -> Phantoms.TTerm [Gremlin.GenericLiteral]
unGenericLiteralSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.GenericLiteralSet")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unIdentifier :: Phantoms.TTerm Gremlin.Identifier -> Phantoms.TTerm String
unIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.Identifier")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unIntegerLiteral :: Phantoms.TTerm Gremlin.IntegerLiteral -> Phantoms.TTerm Integer
unIntegerLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.IntegerLiteral")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unQueryList :: Phantoms.TTerm Gremlin.QueryList -> Phantoms.TTerm [Gremlin.Query]
unQueryList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.QueryList")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unTraversalSource :: Phantoms.TTerm Gremlin.TraversalSource -> Phantoms.TTerm [Gremlin.TraversalSourceSelfMethod]
unTraversalSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tinkerpop.gremlin.TraversalSource")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valueMapArgsBoolean :: Phantoms.TTerm Gremlin.ValueMapBooleanArgs -> Phantoms.TTerm Gremlin.ValueMapArgs
valueMapArgsBoolean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueMapArgsString :: Phantoms.TTerm [Gremlin.StringNullableArgument] -> Phantoms.TTerm Gremlin.ValueMapArgs
valueMapArgsString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueMapBooleanArgs :: Phantoms.TTerm Gremlin.BooleanArgument -> Phantoms.TTerm (Maybe [Gremlin.StringNullableArgument]) -> Phantoms.TTerm Gremlin.ValueMapBooleanArgs
valueMapBooleanArgs value keys =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm keys)}]}))

valueMapBooleanArgsKeys :: Phantoms.TTerm Gremlin.ValueMapBooleanArgs -> Phantoms.TTerm (Maybe [Gremlin.StringNullableArgument])
valueMapBooleanArgsKeys x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
        Core.projectionField = (Core.Name "keys")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valueMapBooleanArgsValue :: Phantoms.TTerm Gremlin.ValueMapBooleanArgs -> Phantoms.TTerm Gremlin.BooleanArgument
valueMapBooleanArgsValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valueMapBooleanArgsWithKeys :: Phantoms.TTerm Gremlin.ValueMapBooleanArgs -> Phantoms.TTerm (Maybe [Gremlin.StringNullableArgument]) -> Phantoms.TTerm Gremlin.ValueMapBooleanArgs
valueMapBooleanArgsWithKeys original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

valueMapBooleanArgsWithValue :: Phantoms.TTerm Gremlin.ValueMapBooleanArgs -> Phantoms.TTerm Gremlin.BooleanArgument -> Phantoms.TTerm Gremlin.ValueMapBooleanArgs
valueMapBooleanArgsWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"),
              Core.projectionField = (Core.Name "keys")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

whereArgsPredicate :: Phantoms.TTerm Gremlin.WhereWithPredicateArgs -> Phantoms.TTerm Gremlin.WhereArgs
whereArgsPredicate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

whereArgsString :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.WhereArgs
whereArgsString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

whereArgsTraversal :: Phantoms.TTerm Gremlin.NestedTraversal -> Phantoms.TTerm Gremlin.WhereArgs
whereArgsTraversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereArgs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "traversal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

whereWithPredicateArgs :: Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.WhereWithPredicateArgs
whereWithPredicateArgs leftArg predicate =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArg"),
          Core.fieldTerm = (Phantoms.unTTerm leftArg)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm predicate)}]}))

whereWithPredicateArgsLeftArg :: Phantoms.TTerm Gremlin.WhereWithPredicateArgs -> Phantoms.TTerm (Maybe Gremlin.StringArgument)
whereWithPredicateArgsLeftArg x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
        Core.projectionField = (Core.Name "leftArg")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whereWithPredicateArgsPredicate :: Phantoms.TTerm Gremlin.WhereWithPredicateArgs -> Phantoms.TTerm Gremlin.TraversalPredicate
whereWithPredicateArgsPredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
        Core.projectionField = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whereWithPredicateArgsWithLeftArg :: Phantoms.TTerm Gremlin.WhereWithPredicateArgs -> Phantoms.TTerm (Maybe Gremlin.StringArgument) -> Phantoms.TTerm Gremlin.WhereWithPredicateArgs
whereWithPredicateArgsWithLeftArg original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArg"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
              Core.projectionField = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

whereWithPredicateArgsWithPredicate :: Phantoms.TTerm Gremlin.WhereWithPredicateArgs -> Phantoms.TTerm Gremlin.TraversalPredicate -> Phantoms.TTerm Gremlin.WhereWithPredicateArgs
whereWithPredicateArgsWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"),
              Core.projectionField = (Core.Name "leftArg")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

withArgs :: Phantoms.TTerm Gremlin.WithArgsKeys -> Phantoms.TTerm (Maybe Gremlin.WithArgsValues) -> Phantoms.TTerm Gremlin.WithArgs
withArgs keys values =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm keys)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm values)}]}))

withArgsKeys :: Phantoms.TTerm Gremlin.WithArgs -> Phantoms.TTerm Gremlin.WithArgsKeys
withArgsKeys x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
        Core.projectionField = (Core.Name "keys")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

withArgsKeysString :: Phantoms.TTerm Gremlin.StringArgument -> Phantoms.TTerm Gremlin.WithArgsKeys
withArgsKeysString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgsKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

withArgsKeysWithOption :: Phantoms.TTerm Gremlin.WithOptionKeys -> Phantoms.TTerm Gremlin.WithArgsKeys
withArgsKeysWithOption x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgsKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withOption"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

withArgsValues :: Phantoms.TTerm Gremlin.WithArgs -> Phantoms.TTerm (Maybe Gremlin.WithArgsValues)
withArgsValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
        Core.projectionField = (Core.Name "values")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

withArgsValuesIo :: Phantoms.TTerm Gremlin.IoOptionsValues -> Phantoms.TTerm Gremlin.WithArgsValues
withArgsValuesIo x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "io"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

withArgsValuesObject :: Phantoms.TTerm Gremlin.GenericLiteralArgument -> Phantoms.TTerm Gremlin.WithArgsValues
withArgsValuesObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

withArgsValuesWithOptions :: Phantoms.TTerm Gremlin.WithOptionsValues -> Phantoms.TTerm Gremlin.WithArgsValues
withArgsValuesWithOptions x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withOptions"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

withArgsWithKeys :: Phantoms.TTerm Gremlin.WithArgs -> Phantoms.TTerm Gremlin.WithArgsKeys -> Phantoms.TTerm Gremlin.WithArgs
withArgsWithKeys original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
              Core.projectionField = (Core.Name "values")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

withArgsWithValues :: Phantoms.TTerm Gremlin.WithArgs -> Phantoms.TTerm (Maybe Gremlin.WithArgsValues) -> Phantoms.TTerm Gremlin.WithArgs
withArgsWithValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithArgs"),
              Core.projectionField = (Core.Name "keys")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

withOptionKeysConnectedComponent :: Phantoms.TTerm Gremlin.ConnectedComponentConstants -> Phantoms.TTerm Gremlin.WithOptionKeys
withOptionKeysConnectedComponent x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "connectedComponent"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

withOptionKeysIo :: Phantoms.TTerm Gremlin.IoOptionsKeys -> Phantoms.TTerm Gremlin.WithOptionKeys
withOptionKeysIo x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "io"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

withOptionKeysPageRank :: Phantoms.TTerm Gremlin.PageRankConstants -> Phantoms.TTerm Gremlin.WithOptionKeys
withOptionKeysPageRank x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pageRank"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

withOptionKeysPeerPressure :: Phantoms.TTerm Gremlin.PeerPressureConstants -> Phantoms.TTerm Gremlin.WithOptionKeys
withOptionKeysPeerPressure x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "peerPressure"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

withOptionKeysShortestPath :: Phantoms.TTerm Gremlin.ShortestPathConstants -> Phantoms.TTerm Gremlin.WithOptionKeys
withOptionKeysShortestPath x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shortestPath"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

withOptionKeysWithOptionsIndexer :: Phantoms.TTerm Gremlin.WithOptionKeys
withOptionKeysWithOptionsIndexer =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withOptionsIndexer"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionKeysWithOptionsTokens :: Phantoms.TTerm Gremlin.WithOptionKeys
withOptionKeysWithOptionsTokens =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withOptionsTokens"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesAll :: Phantoms.TTerm Gremlin.WithOptionsValues
withOptionsValuesAll =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesIds :: Phantoms.TTerm Gremlin.WithOptionsValues
withOptionsValuesIds =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ids"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesKeys :: Phantoms.TTerm Gremlin.WithOptionsValues
withOptionsValuesKeys =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keys"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesLabels :: Phantoms.TTerm Gremlin.WithOptionsValues
withOptionsValuesLabels =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labels"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesList :: Phantoms.TTerm Gremlin.WithOptionsValues
withOptionsValuesList =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesMap :: Phantoms.TTerm Gremlin.WithOptionsValues
withOptionsValuesMap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesNone :: Phantoms.TTerm Gremlin.WithOptionsValues
withOptionsValuesNone =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesTokens :: Phantoms.TTerm Gremlin.WithOptionsValues
withOptionsValuesTokens =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tokens"),
        Core.fieldTerm = Core.TermUnit}}))

withOptionsValuesValues :: Phantoms.TTerm Gremlin.WithOptionsValues
withOptionsValuesValues =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "values"),
        Core.fieldTerm = Core.TermUnit}}))
