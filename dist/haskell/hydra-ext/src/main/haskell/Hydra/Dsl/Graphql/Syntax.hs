-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.graphql.syntax

module Hydra.Dsl.Graphql.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Graphql.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

aliasColon :: Phantoms.TTerm Syntax.Alias
aliasColon =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Alias"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Colon"),
        Core.fieldTerm = Core.TermUnit}}))

aliasName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Alias
aliasName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Alias"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

argument :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.Argument
argument name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Argument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

argumentName :: Phantoms.TTerm Syntax.Argument -> Phantoms.TTerm Syntax.Name
argumentName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Argument"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

argumentValue :: Phantoms.TTerm Syntax.Argument -> Phantoms.TTerm Syntax.Value
argumentValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Argument"),
        Core.projectionField = (Core.Name "Value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

argumentWithName :: Phantoms.TTerm Syntax.Argument -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Argument
argumentWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Argument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Argument"),
              Core.projectionField = (Core.Name "Value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

argumentWithValue :: Phantoms.TTerm Syntax.Argument -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.Argument
argumentWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Argument"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Argument"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arguments :: Phantoms.TTerm [Syntax.Argument] -> Phantoms.TTerm Syntax.Arguments
arguments x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.Arguments"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

argumentsDefinition :: Phantoms.TTerm [Syntax.InputValueDefinition] -> Phantoms.TTerm Syntax.ArgumentsDefinition
argumentsDefinition x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.ArgumentsDefinition"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

booleanValueFalse :: Phantoms.TTerm Syntax.BooleanValue
booleanValueFalse =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.BooleanValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "False"),
        Core.fieldTerm = Core.TermUnit}}))

booleanValueTrue :: Phantoms.TTerm Syntax.BooleanValue
booleanValueTrue =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.BooleanValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "True"),
        Core.fieldTerm = Core.TermUnit}}))

defaultValue :: Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.DefaultValue
defaultValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.DefaultValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

definitionExecutable :: Phantoms.TTerm Syntax.ExecutableDefinition -> Phantoms.TTerm Syntax.Definition
definitionExecutable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "executable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

definitionTypeSystem :: Phantoms.TTerm Syntax.TypeSystemDefinitionOrExtension -> Phantoms.TTerm Syntax.Definition
definitionTypeSystem x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeSystem"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

description :: Phantoms.TTerm Syntax.StringValue -> Phantoms.TTerm Syntax.Description
description x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.Description"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

directive :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Arguments) -> Phantoms.TTerm Syntax.Directive
directive name arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Directive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

directiveArguments :: Phantoms.TTerm Syntax.Directive -> Phantoms.TTerm (Maybe Syntax.Arguments)
directiveArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Directive"),
        Core.projectionField = (Core.Name "Arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveDefinition :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.ArgumentsDefinition) -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.DirectiveLocations -> Phantoms.TTerm Syntax.DirectiveDefinition
directiveDefinition description name argumentsDefinition repeatable directiveLocations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm argumentsDefinition)},
        Core.Field {
          Core.fieldName = (Core.Name "Repeatable"),
          Core.fieldTerm = (Phantoms.unTTerm repeatable)},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocations"),
          Core.fieldTerm = (Phantoms.unTTerm directiveLocations)}]}))

directiveDefinitionArgumentsDefinition :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm (Maybe Syntax.ArgumentsDefinition)
directiveDefinitionArgumentsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
        Core.projectionField = (Core.Name "ArgumentsDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveDefinitionDescription :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm (Maybe Syntax.Description)
directiveDefinitionDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveDefinitionDirectiveLocations :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm Syntax.DirectiveLocations
directiveDefinitionDirectiveLocations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
        Core.projectionField = (Core.Name "DirectiveLocations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveDefinitionName :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm Syntax.Name
directiveDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveDefinitionRepeatable :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm (Maybe ())
directiveDefinitionRepeatable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
        Core.projectionField = (Core.Name "Repeatable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveDefinitionWithArgumentsDefinition :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm (Maybe Syntax.ArgumentsDefinition) -> Phantoms.TTerm Syntax.DirectiveDefinition
directiveDefinitionWithArgumentsDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Repeatable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Repeatable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "DirectiveLocations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

directiveDefinitionWithDescription :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.DirectiveDefinition
directiveDefinitionWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "ArgumentsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Repeatable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Repeatable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "DirectiveLocations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

directiveDefinitionWithDirectiveLocations :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm Syntax.DirectiveLocations -> Phantoms.TTerm Syntax.DirectiveDefinition
directiveDefinitionWithDirectiveLocations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "ArgumentsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Repeatable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Repeatable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

directiveDefinitionWithName :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.DirectiveDefinition
directiveDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "ArgumentsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Repeatable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Repeatable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "DirectiveLocations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

directiveDefinitionWithRepeatable :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.DirectiveDefinition
directiveDefinitionWithRepeatable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "ArgumentsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Repeatable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveDefinition"),
              Core.projectionField = (Core.Name "DirectiveLocations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

directiveLocationExecutable :: Phantoms.TTerm Syntax.ExecutableDirectiveLocation -> Phantoms.TTerm Syntax.DirectiveLocation
directiveLocationExecutable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "executable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

directiveLocationTypeSystem :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation -> Phantoms.TTerm Syntax.DirectiveLocation
directiveLocationTypeSystem x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeSystem"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

directiveLocationsSequence :: Phantoms.TTerm Syntax.DirectiveLocations_Sequence -> Phantoms.TTerm Syntax.DirectiveLocations
directiveLocationsSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

directiveLocationsSequence2 :: Phantoms.TTerm Syntax.DirectiveLocations_Sequence2 -> Phantoms.TTerm Syntax.DirectiveLocations
directiveLocationsSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

directiveLocations_Sequence :: Phantoms.TTerm Syntax.DirectiveLocations -> Phantoms.TTerm Syntax.DirectiveLocation -> Phantoms.TTerm Syntax.DirectiveLocations_Sequence
directiveLocations_Sequence directiveLocations directiveLocation =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocations"),
          Core.fieldTerm = (Phantoms.unTTerm directiveLocations)},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocation"),
          Core.fieldTerm = (Phantoms.unTTerm directiveLocation)}]}))

directiveLocations_Sequence2 :: Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.DirectiveLocation -> Phantoms.TTerm Syntax.DirectiveLocations_Sequence2
directiveLocations_Sequence2 or directiveLocation =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Or"),
          Core.fieldTerm = (Phantoms.unTTerm or)},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocation"),
          Core.fieldTerm = (Phantoms.unTTerm directiveLocation)}]}))

directiveLocations_Sequence2DirectiveLocation :: Phantoms.TTerm Syntax.DirectiveLocations_Sequence2 -> Phantoms.TTerm Syntax.DirectiveLocation
directiveLocations_Sequence2DirectiveLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence2"),
        Core.projectionField = (Core.Name "DirectiveLocation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveLocations_Sequence2Or :: Phantoms.TTerm Syntax.DirectiveLocations_Sequence2 -> Phantoms.TTerm (Maybe ())
directiveLocations_Sequence2Or x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence2"),
        Core.projectionField = (Core.Name "Or")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveLocations_Sequence2WithDirectiveLocation :: Phantoms.TTerm Syntax.DirectiveLocations_Sequence2 -> Phantoms.TTerm Syntax.DirectiveLocation -> Phantoms.TTerm Syntax.DirectiveLocations_Sequence2
directiveLocations_Sequence2WithDirectiveLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Or"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence2"),
              Core.projectionField = (Core.Name "Or")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

directiveLocations_Sequence2WithOr :: Phantoms.TTerm Syntax.DirectiveLocations_Sequence2 -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.DirectiveLocations_Sequence2
directiveLocations_Sequence2WithOr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Or"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence2"),
              Core.projectionField = (Core.Name "DirectiveLocation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

directiveLocations_SequenceDirectiveLocation :: Phantoms.TTerm Syntax.DirectiveLocations_Sequence -> Phantoms.TTerm Syntax.DirectiveLocation
directiveLocations_SequenceDirectiveLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence"),
        Core.projectionField = (Core.Name "DirectiveLocation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveLocations_SequenceDirectiveLocations :: Phantoms.TTerm Syntax.DirectiveLocations_Sequence -> Phantoms.TTerm Syntax.DirectiveLocations
directiveLocations_SequenceDirectiveLocations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence"),
        Core.projectionField = (Core.Name "DirectiveLocations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveLocations_SequenceWithDirectiveLocation :: Phantoms.TTerm Syntax.DirectiveLocations_Sequence -> Phantoms.TTerm Syntax.DirectiveLocation -> Phantoms.TTerm Syntax.DirectiveLocations_Sequence
directiveLocations_SequenceWithDirectiveLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence"),
              Core.projectionField = (Core.Name "DirectiveLocations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

directiveLocations_SequenceWithDirectiveLocations :: Phantoms.TTerm Syntax.DirectiveLocations_Sequence -> Phantoms.TTerm Syntax.DirectiveLocations -> Phantoms.TTerm Syntax.DirectiveLocations_Sequence
directiveLocations_SequenceWithDirectiveLocations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "DirectiveLocation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.DirectiveLocations_Sequence"),
              Core.projectionField = (Core.Name "DirectiveLocation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

directiveName :: Phantoms.TTerm Syntax.Directive -> Phantoms.TTerm Syntax.Name
directiveName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Directive"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

directiveWithArguments :: Phantoms.TTerm Syntax.Directive -> Phantoms.TTerm (Maybe Syntax.Arguments) -> Phantoms.TTerm Syntax.Directive
directiveWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Directive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Directive"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

directiveWithName :: Phantoms.TTerm Syntax.Directive -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Directive
directiveWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Directive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Directive"),
              Core.projectionField = (Core.Name "Arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

directives :: Phantoms.TTerm [Syntax.Directive] -> Phantoms.TTerm Syntax.Directives
directives x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.Directives"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

document :: Phantoms.TTerm [Syntax.Definition] -> Phantoms.TTerm Syntax.Document
document x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.Document"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

enumTypeDefinition :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm (Maybe Syntax.EnumValuesDefinition) -> Phantoms.TTerm Syntax.EnumTypeDefinition
enumTypeDefinition description name directives enumValuesDefinition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValuesDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm enumValuesDefinition)}]}))

enumTypeDefinitionDescription :: Phantoms.TTerm Syntax.EnumTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Description)
enumTypeDefinitionDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumTypeDefinitionDirectives :: Phantoms.TTerm Syntax.EnumTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Directives)
enumTypeDefinitionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumTypeDefinitionEnumValuesDefinition :: Phantoms.TTerm Syntax.EnumTypeDefinition -> Phantoms.TTerm (Maybe Syntax.EnumValuesDefinition)
enumTypeDefinitionEnumValuesDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
        Core.projectionField = (Core.Name "EnumValuesDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumTypeDefinitionName :: Phantoms.TTerm Syntax.EnumTypeDefinition -> Phantoms.TTerm Syntax.Name
enumTypeDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumTypeDefinitionWithDescription :: Phantoms.TTerm Syntax.EnumTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.EnumTypeDefinition
enumTypeDefinitionWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValuesDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "EnumValuesDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumTypeDefinitionWithDirectives :: Phantoms.TTerm Syntax.EnumTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.EnumTypeDefinition
enumTypeDefinitionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValuesDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "EnumValuesDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumTypeDefinitionWithEnumValuesDefinition :: Phantoms.TTerm Syntax.EnumTypeDefinition -> Phantoms.TTerm (Maybe Syntax.EnumValuesDefinition) -> Phantoms.TTerm Syntax.EnumTypeDefinition
enumTypeDefinitionWithEnumValuesDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValuesDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumTypeDefinitionWithName :: Phantoms.TTerm Syntax.EnumTypeDefinition -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.EnumTypeDefinition
enumTypeDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValuesDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeDefinition"),
              Core.projectionField = (Core.Name "EnumValuesDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumTypeExtensionSequence :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence -> Phantoms.TTerm Syntax.EnumTypeExtension
enumTypeExtensionSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumTypeExtensionSequence2 :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.EnumTypeExtension
enumTypeExtensionSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumTypeExtension_Sequence :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.EnumValuesDefinition -> Phantoms.TTerm Syntax.EnumTypeExtension_Sequence
enumTypeExtension_Sequence name directives enumValuesDefinition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValuesDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm enumValuesDefinition)}]}))

enumTypeExtension_Sequence2 :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.EnumTypeExtension_Sequence2
enumTypeExtension_Sequence2 name directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

enumTypeExtension_Sequence2Directives :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Directives
enumTypeExtension_Sequence2Directives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumTypeExtension_Sequence2Name :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Name
enumTypeExtension_Sequence2Name x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumTypeExtension_Sequence2WithDirectives :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.EnumTypeExtension_Sequence2
enumTypeExtension_Sequence2WithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumTypeExtension_Sequence2WithName :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.EnumTypeExtension_Sequence2
enumTypeExtension_Sequence2WithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumTypeExtension_SequenceDirectives :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives)
enumTypeExtension_SequenceDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumTypeExtension_SequenceEnumValuesDefinition :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence -> Phantoms.TTerm Syntax.EnumValuesDefinition
enumTypeExtension_SequenceEnumValuesDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "EnumValuesDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumTypeExtension_SequenceName :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence -> Phantoms.TTerm Syntax.Name
enumTypeExtension_SequenceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumTypeExtension_SequenceWithDirectives :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.EnumTypeExtension_Sequence
enumTypeExtension_SequenceWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValuesDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "EnumValuesDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumTypeExtension_SequenceWithEnumValuesDefinition :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence -> Phantoms.TTerm Syntax.EnumValuesDefinition -> Phantoms.TTerm Syntax.EnumTypeExtension_Sequence
enumTypeExtension_SequenceWithEnumValuesDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValuesDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumTypeExtension_SequenceWithName :: Phantoms.TTerm Syntax.EnumTypeExtension_Sequence -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.EnumTypeExtension_Sequence
enumTypeExtension_SequenceWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValuesDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "EnumValuesDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumValue :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.EnumValue
enumValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.EnumValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

enumValueDefinition :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.EnumValue -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.EnumValueDefinition
enumValueDefinition description enumValue directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValue"),
          Core.fieldTerm = (Phantoms.unTTerm enumValue)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

enumValueDefinitionDescription :: Phantoms.TTerm Syntax.EnumValueDefinition -> Phantoms.TTerm (Maybe Syntax.Description)
enumValueDefinitionDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumValueDefinitionDirectives :: Phantoms.TTerm Syntax.EnumValueDefinition -> Phantoms.TTerm (Maybe Syntax.Directives)
enumValueDefinitionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumValueDefinitionEnumValue :: Phantoms.TTerm Syntax.EnumValueDefinition -> Phantoms.TTerm Syntax.EnumValue
enumValueDefinitionEnumValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
        Core.projectionField = (Core.Name "EnumValue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumValueDefinitionWithDescription :: Phantoms.TTerm Syntax.EnumValueDefinition -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.EnumValueDefinition
enumValueDefinitionWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
              Core.projectionField = (Core.Name "EnumValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumValueDefinitionWithDirectives :: Phantoms.TTerm Syntax.EnumValueDefinition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.EnumValueDefinition
enumValueDefinitionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
              Core.projectionField = (Core.Name "EnumValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumValueDefinitionWithEnumValue :: Phantoms.TTerm Syntax.EnumValueDefinition -> Phantoms.TTerm Syntax.EnumValue -> Phantoms.TTerm Syntax.EnumValueDefinition
enumValueDefinitionWithEnumValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "EnumValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.EnumValueDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumValuesDefinition :: Phantoms.TTerm [Syntax.EnumValueDefinition] -> Phantoms.TTerm Syntax.EnumValuesDefinition
enumValuesDefinition x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.EnumValuesDefinition"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

executableDefinitionFragment :: Phantoms.TTerm Syntax.FragmentDefinition -> Phantoms.TTerm Syntax.ExecutableDefinition
executableDefinitionFragment x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fragment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

executableDefinitionOperation :: Phantoms.TTerm Syntax.OperationDefinition -> Phantoms.TTerm Syntax.ExecutableDefinition
executableDefinitionOperation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "operation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

executableDirectiveLocationFIELD :: Phantoms.TTerm Syntax.ExecutableDirectiveLocation
executableDirectiveLocationFIELD =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "FIELD"),
        Core.fieldTerm = Core.TermUnit}}))

executableDirectiveLocationFRAGMENT_DEFINITION :: Phantoms.TTerm Syntax.ExecutableDirectiveLocation
executableDirectiveLocationFRAGMENT_DEFINITION =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "FRAGMENT_DEFINITION"),
        Core.fieldTerm = Core.TermUnit}}))

executableDirectiveLocationFRAGMENT_SPREAD :: Phantoms.TTerm Syntax.ExecutableDirectiveLocation
executableDirectiveLocationFRAGMENT_SPREAD =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "FRAGMENT_SPREAD"),
        Core.fieldTerm = Core.TermUnit}}))

executableDirectiveLocationINLINE_FRAGMENT :: Phantoms.TTerm Syntax.ExecutableDirectiveLocation
executableDirectiveLocationINLINE_FRAGMENT =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "INLINE_FRAGMENT"),
        Core.fieldTerm = Core.TermUnit}}))

executableDirectiveLocationMUTATION :: Phantoms.TTerm Syntax.ExecutableDirectiveLocation
executableDirectiveLocationMUTATION =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MUTATION"),
        Core.fieldTerm = Core.TermUnit}}))

executableDirectiveLocationQUERY :: Phantoms.TTerm Syntax.ExecutableDirectiveLocation
executableDirectiveLocationQUERY =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "QUERY"),
        Core.fieldTerm = Core.TermUnit}}))

executableDirectiveLocationSUBSCRIPTION :: Phantoms.TTerm Syntax.ExecutableDirectiveLocation
executableDirectiveLocationSUBSCRIPTION =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "SUBSCRIPTION"),
        Core.fieldTerm = Core.TermUnit}}))

executableDirectiveLocationVARIABLE_DEFINITION :: Phantoms.TTerm Syntax.ExecutableDirectiveLocation
executableDirectiveLocationVARIABLE_DEFINITION =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "VARIABLE_DEFINITION"),
        Core.fieldTerm = Core.TermUnit}}))

executableDocument :: Phantoms.TTerm [Syntax.ExecutableDefinition] -> Phantoms.TTerm Syntax.ExecutableDocument
executableDocument x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.ExecutableDocument"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

field :: Phantoms.TTerm (Maybe Syntax.Alias) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Arguments) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm (Maybe Syntax.SelectionSet) -> Phantoms.TTerm Syntax.Field
field alias name arguments directives selectionSet =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alias"),
          Core.fieldTerm = (Phantoms.unTTerm alias)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Phantoms.unTTerm selectionSet)}]}))

fieldAlias :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe Syntax.Alias)
fieldAlias x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
        Core.projectionField = (Core.Name "Alias")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldArguments :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe Syntax.Arguments)
fieldArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
        Core.projectionField = (Core.Name "Arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDefinition :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.ArgumentsDefinition) -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinition description name argumentsDefinition type_ directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm argumentsDefinition)},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

fieldDefinitionArgumentsDefinition :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm (Maybe Syntax.ArgumentsDefinition)
fieldDefinitionArgumentsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
        Core.projectionField = (Core.Name "ArgumentsDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDefinitionDescription :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm (Maybe Syntax.Description)
fieldDefinitionDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDefinitionDirectives :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm (Maybe Syntax.Directives)
fieldDefinitionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDefinitionName :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm Syntax.Name
fieldDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDefinitionType :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm Syntax.Type
fieldDefinitionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
        Core.projectionField = (Core.Name "Type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDefinitionWithArgumentsDefinition :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm (Maybe Syntax.ArgumentsDefinition) -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinitionWithArgumentsDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldDefinitionWithDescription :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinitionWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "ArgumentsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldDefinitionWithDirectives :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinitionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "ArgumentsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldDefinitionWithName :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "ArgumentsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldDefinitionWithType :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinitionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ArgumentsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "ArgumentsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldDirectives :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe Syntax.Directives)
fieldDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldName :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Name
fieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldSelectionSet :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe Syntax.SelectionSet)
fieldSelectionSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
        Core.projectionField = (Core.Name "SelectionSet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldWithAlias :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe Syntax.Alias) -> Phantoms.TTerm Syntax.Field
fieldWithAlias original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alias"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithArguments :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe Syntax.Arguments) -> Phantoms.TTerm Syntax.Field
fieldWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Alias")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithDirectives :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.Field
fieldWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Alias")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithName :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Field
fieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Alias")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithSelectionSet :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe Syntax.SelectionSet) -> Phantoms.TTerm Syntax.Field
fieldWithSelectionSet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Alias")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.Field"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldsDefinition :: Phantoms.TTerm [Syntax.FieldDefinition] -> Phantoms.TTerm Syntax.FieldsDefinition
fieldsDefinition x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.FieldsDefinition"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

floatValue :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.FloatValue
floatValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.FloatValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

fragmentDefinition :: Phantoms.TTerm Syntax.FragmentName -> Phantoms.TTerm Syntax.TypeCondition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.SelectionSet -> Phantoms.TTerm Syntax.FragmentDefinition
fragmentDefinition fragmentName typeCondition directives selectionSet =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "FragmentName"),
          Core.fieldTerm = (Phantoms.unTTerm fragmentName)},
        Core.Field {
          Core.fieldName = (Core.Name "TypeCondition"),
          Core.fieldTerm = (Phantoms.unTTerm typeCondition)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Phantoms.unTTerm selectionSet)}]}))

fragmentDefinitionDirectives :: Phantoms.TTerm Syntax.FragmentDefinition -> Phantoms.TTerm (Maybe Syntax.Directives)
fragmentDefinitionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fragmentDefinitionFragmentName :: Phantoms.TTerm Syntax.FragmentDefinition -> Phantoms.TTerm Syntax.FragmentName
fragmentDefinitionFragmentName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
        Core.projectionField = (Core.Name "FragmentName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fragmentDefinitionSelectionSet :: Phantoms.TTerm Syntax.FragmentDefinition -> Phantoms.TTerm Syntax.SelectionSet
fragmentDefinitionSelectionSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
        Core.projectionField = (Core.Name "SelectionSet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fragmentDefinitionTypeCondition :: Phantoms.TTerm Syntax.FragmentDefinition -> Phantoms.TTerm Syntax.TypeCondition
fragmentDefinitionTypeCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
        Core.projectionField = (Core.Name "TypeCondition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fragmentDefinitionWithDirectives :: Phantoms.TTerm Syntax.FragmentDefinition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.FragmentDefinition
fragmentDefinitionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "FragmentName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "FragmentName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "TypeCondition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "TypeCondition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fragmentDefinitionWithFragmentName :: Phantoms.TTerm Syntax.FragmentDefinition -> Phantoms.TTerm Syntax.FragmentName -> Phantoms.TTerm Syntax.FragmentDefinition
fragmentDefinitionWithFragmentName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "FragmentName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "TypeCondition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "TypeCondition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fragmentDefinitionWithSelectionSet :: Phantoms.TTerm Syntax.FragmentDefinition -> Phantoms.TTerm Syntax.SelectionSet -> Phantoms.TTerm Syntax.FragmentDefinition
fragmentDefinitionWithSelectionSet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "FragmentName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "FragmentName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "TypeCondition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "TypeCondition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fragmentDefinitionWithTypeCondition :: Phantoms.TTerm Syntax.FragmentDefinition -> Phantoms.TTerm Syntax.TypeCondition -> Phantoms.TTerm Syntax.FragmentDefinition
fragmentDefinitionWithTypeCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "FragmentName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "FragmentName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "TypeCondition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentDefinition"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fragmentName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.FragmentName
fragmentName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.FragmentName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

fragmentSpread :: Phantoms.TTerm Syntax.FragmentName -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.FragmentSpread
fragmentSpread fragmentName directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FragmentSpread"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "FragmentName"),
          Core.fieldTerm = (Phantoms.unTTerm fragmentName)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

fragmentSpreadDirectives :: Phantoms.TTerm Syntax.FragmentSpread -> Phantoms.TTerm (Maybe Syntax.Directives)
fragmentSpreadDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentSpread"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fragmentSpreadFragmentName :: Phantoms.TTerm Syntax.FragmentSpread -> Phantoms.TTerm Syntax.FragmentName
fragmentSpreadFragmentName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentSpread"),
        Core.projectionField = (Core.Name "FragmentName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fragmentSpreadWithDirectives :: Phantoms.TTerm Syntax.FragmentSpread -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.FragmentSpread
fragmentSpreadWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FragmentSpread"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "FragmentName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentSpread"),
              Core.projectionField = (Core.Name "FragmentName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fragmentSpreadWithFragmentName :: Phantoms.TTerm Syntax.FragmentSpread -> Phantoms.TTerm Syntax.FragmentName -> Phantoms.TTerm Syntax.FragmentSpread
fragmentSpreadWithFragmentName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.FragmentSpread"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "FragmentName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.FragmentSpread"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implementsInterfacesSequence :: Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence -> Phantoms.TTerm Syntax.ImplementsInterfaces
implementsInterfacesSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

implementsInterfacesSequence2 :: Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence2 -> Phantoms.TTerm Syntax.ImplementsInterfaces
implementsInterfacesSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

implementsInterfaces_Sequence :: Phantoms.TTerm Syntax.ImplementsInterfaces -> Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence
implementsInterfaces_Sequence implementsInterfaces namedType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm implementsInterfaces)},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Phantoms.unTTerm namedType)}]}))

implementsInterfaces_Sequence2 :: Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence2
implementsInterfaces_Sequence2 amp namedType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Amp"),
          Core.fieldTerm = (Phantoms.unTTerm amp)},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Phantoms.unTTerm namedType)}]}))

implementsInterfaces_Sequence2Amp :: Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence2 -> Phantoms.TTerm (Maybe ())
implementsInterfaces_Sequence2Amp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence2"),
        Core.projectionField = (Core.Name "Amp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implementsInterfaces_Sequence2NamedType :: Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence2 -> Phantoms.TTerm Syntax.NamedType
implementsInterfaces_Sequence2NamedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence2"),
        Core.projectionField = (Core.Name "NamedType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implementsInterfaces_Sequence2WithAmp :: Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence2 -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence2
implementsInterfaces_Sequence2WithAmp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Amp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence2"),
              Core.projectionField = (Core.Name "NamedType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implementsInterfaces_Sequence2WithNamedType :: Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence2 -> Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence2
implementsInterfaces_Sequence2WithNamedType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Amp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence2"),
              Core.projectionField = (Core.Name "Amp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

implementsInterfaces_SequenceImplementsInterfaces :: Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence -> Phantoms.TTerm Syntax.ImplementsInterfaces
implementsInterfaces_SequenceImplementsInterfaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence"),
        Core.projectionField = (Core.Name "ImplementsInterfaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implementsInterfaces_SequenceNamedType :: Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence -> Phantoms.TTerm Syntax.NamedType
implementsInterfaces_SequenceNamedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence"),
        Core.projectionField = (Core.Name "NamedType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implementsInterfaces_SequenceWithImplementsInterfaces :: Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence -> Phantoms.TTerm Syntax.ImplementsInterfaces -> Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence
implementsInterfaces_SequenceWithImplementsInterfaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence"),
              Core.projectionField = (Core.Name "NamedType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implementsInterfaces_SequenceWithNamedType :: Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence -> Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.ImplementsInterfaces_Sequence
implementsInterfaces_SequenceWithNamedType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ImplementsInterfaces_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inlineFragment :: Phantoms.TTerm (Maybe Syntax.TypeCondition) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.SelectionSet -> Phantoms.TTerm Syntax.InlineFragment
inlineFragment typeCondition directives selectionSet =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "TypeCondition"),
          Core.fieldTerm = (Phantoms.unTTerm typeCondition)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Phantoms.unTTerm selectionSet)}]}))

inlineFragmentDirectives :: Phantoms.TTerm Syntax.InlineFragment -> Phantoms.TTerm (Maybe Syntax.Directives)
inlineFragmentDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineFragmentSelectionSet :: Phantoms.TTerm Syntax.InlineFragment -> Phantoms.TTerm Syntax.SelectionSet
inlineFragmentSelectionSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
        Core.projectionField = (Core.Name "SelectionSet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineFragmentTypeCondition :: Phantoms.TTerm Syntax.InlineFragment -> Phantoms.TTerm (Maybe Syntax.TypeCondition)
inlineFragmentTypeCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
        Core.projectionField = (Core.Name "TypeCondition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineFragmentWithDirectives :: Phantoms.TTerm Syntax.InlineFragment -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InlineFragment
inlineFragmentWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "TypeCondition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
              Core.projectionField = (Core.Name "TypeCondition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inlineFragmentWithSelectionSet :: Phantoms.TTerm Syntax.InlineFragment -> Phantoms.TTerm Syntax.SelectionSet -> Phantoms.TTerm Syntax.InlineFragment
inlineFragmentWithSelectionSet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "TypeCondition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
              Core.projectionField = (Core.Name "TypeCondition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inlineFragmentWithTypeCondition :: Phantoms.TTerm Syntax.InlineFragment -> Phantoms.TTerm (Maybe Syntax.TypeCondition) -> Phantoms.TTerm Syntax.InlineFragment
inlineFragmentWithTypeCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "TypeCondition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InlineFragment"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputFieldsDefinition :: Phantoms.TTerm [Syntax.InputValueDefinition] -> Phantoms.TTerm Syntax.InputFieldsDefinition
inputFieldsDefinition x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.InputFieldsDefinition"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

inputObjectTypeDefinitionSequence :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence -> Phantoms.TTerm Syntax.InputObjectTypeDefinition
inputObjectTypeDefinitionSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inputObjectTypeDefinitionSequence2 :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2 -> Phantoms.TTerm Syntax.InputObjectTypeDefinition
inputObjectTypeDefinitionSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inputObjectTypeDefinition_Sequence :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InputFieldsDefinition -> Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence
inputObjectTypeDefinition_Sequence description name directives inputFieldsDefinition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "InputFieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm inputFieldsDefinition)}]}))

inputObjectTypeDefinition_Sequence2 :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2
inputObjectTypeDefinition_Sequence2 description name directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

inputObjectTypeDefinition_Sequence2Description :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2 -> Phantoms.TTerm (Maybe Syntax.Description)
inputObjectTypeDefinition_Sequence2Description x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeDefinition_Sequence2Directives :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2 -> Phantoms.TTerm (Maybe Syntax.Directives)
inputObjectTypeDefinition_Sequence2Directives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeDefinition_Sequence2Name :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2 -> Phantoms.TTerm Syntax.Name
inputObjectTypeDefinition_Sequence2Name x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeDefinition_Sequence2WithDescription :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2 -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2
inputObjectTypeDefinition_Sequence2WithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputObjectTypeDefinition_Sequence2WithDirectives :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2 -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2
inputObjectTypeDefinition_Sequence2WithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inputObjectTypeDefinition_Sequence2WithName :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence2
inputObjectTypeDefinition_Sequence2WithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputObjectTypeDefinition_SequenceDescription :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Description)
inputObjectTypeDefinition_SequenceDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeDefinition_SequenceDirectives :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives)
inputObjectTypeDefinition_SequenceDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeDefinition_SequenceInputFieldsDefinition :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence -> Phantoms.TTerm Syntax.InputFieldsDefinition
inputObjectTypeDefinition_SequenceInputFieldsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
        Core.projectionField = (Core.Name "InputFieldsDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeDefinition_SequenceName :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence -> Phantoms.TTerm Syntax.Name
inputObjectTypeDefinition_SequenceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeDefinition_SequenceWithDescription :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence
inputObjectTypeDefinition_SequenceWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InputFieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "InputFieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputObjectTypeDefinition_SequenceWithDirectives :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence
inputObjectTypeDefinition_SequenceWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "InputFieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "InputFieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputObjectTypeDefinition_SequenceWithInputFieldsDefinition :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence -> Phantoms.TTerm Syntax.InputFieldsDefinition -> Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence
inputObjectTypeDefinition_SequenceWithInputFieldsDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InputFieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inputObjectTypeDefinition_SequenceWithName :: Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.InputObjectTypeDefinition_Sequence
inputObjectTypeDefinition_SequenceWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InputFieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "InputFieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputObjectTypeExtensionSequence :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence -> Phantoms.TTerm Syntax.InputObjectTypeExtension
inputObjectTypeExtensionSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inputObjectTypeExtensionSequence2 :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.InputObjectTypeExtension
inputObjectTypeExtensionSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inputObjectTypeExtension_Sequence :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InputFieldsDefinition -> Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence
inputObjectTypeExtension_Sequence name directives inputFieldsDefinition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "InputFieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm inputFieldsDefinition)}]}))

inputObjectTypeExtension_Sequence2 :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence2
inputObjectTypeExtension_Sequence2 name directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

inputObjectTypeExtension_Sequence2Directives :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Directives
inputObjectTypeExtension_Sequence2Directives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeExtension_Sequence2Name :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Name
inputObjectTypeExtension_Sequence2Name x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeExtension_Sequence2WithDirectives :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence2
inputObjectTypeExtension_Sequence2WithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inputObjectTypeExtension_Sequence2WithName :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence2
inputObjectTypeExtension_Sequence2WithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputObjectTypeExtension_SequenceDirectives :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives)
inputObjectTypeExtension_SequenceDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeExtension_SequenceInputFieldsDefinition :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence -> Phantoms.TTerm Syntax.InputFieldsDefinition
inputObjectTypeExtension_SequenceInputFieldsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "InputFieldsDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeExtension_SequenceName :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence -> Phantoms.TTerm Syntax.Name
inputObjectTypeExtension_SequenceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputObjectTypeExtension_SequenceWithDirectives :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence
inputObjectTypeExtension_SequenceWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "InputFieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "InputFieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputObjectTypeExtension_SequenceWithInputFieldsDefinition :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence -> Phantoms.TTerm Syntax.InputFieldsDefinition -> Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence
inputObjectTypeExtension_SequenceWithInputFieldsDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InputFieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inputObjectTypeExtension_SequenceWithName :: Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.InputObjectTypeExtension_Sequence
inputObjectTypeExtension_SequenceWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InputFieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "InputFieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputValueDefinition :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm (Maybe Syntax.DefaultValue) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InputValueDefinition
inputValueDefinition description name type_ defaultValue directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm defaultValue)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

inputValueDefinitionDefaultValue :: Phantoms.TTerm Syntax.InputValueDefinition -> Phantoms.TTerm (Maybe Syntax.DefaultValue)
inputValueDefinitionDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
        Core.projectionField = (Core.Name "DefaultValue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputValueDefinitionDescription :: Phantoms.TTerm Syntax.InputValueDefinition -> Phantoms.TTerm (Maybe Syntax.Description)
inputValueDefinitionDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputValueDefinitionDirectives :: Phantoms.TTerm Syntax.InputValueDefinition -> Phantoms.TTerm (Maybe Syntax.Directives)
inputValueDefinitionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputValueDefinitionName :: Phantoms.TTerm Syntax.InputValueDefinition -> Phantoms.TTerm Syntax.Name
inputValueDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputValueDefinitionType :: Phantoms.TTerm Syntax.InputValueDefinition -> Phantoms.TTerm Syntax.Type
inputValueDefinitionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
        Core.projectionField = (Core.Name "Type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inputValueDefinitionWithDefaultValue :: Phantoms.TTerm Syntax.InputValueDefinition -> Phantoms.TTerm (Maybe Syntax.DefaultValue) -> Phantoms.TTerm Syntax.InputValueDefinition
inputValueDefinitionWithDefaultValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputValueDefinitionWithDescription :: Phantoms.TTerm Syntax.InputValueDefinition -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.InputValueDefinition
inputValueDefinitionWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "DefaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputValueDefinitionWithDirectives :: Phantoms.TTerm Syntax.InputValueDefinition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InputValueDefinition
inputValueDefinitionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "DefaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inputValueDefinitionWithName :: Phantoms.TTerm Syntax.InputValueDefinition -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.InputValueDefinition
inputValueDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "DefaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inputValueDefinitionWithType :: Phantoms.TTerm Syntax.InputValueDefinition -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.InputValueDefinition
inputValueDefinitionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "DefaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InputValueDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

intValue :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.IntValue
intValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.IntValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

interfaceTypeDefinitionSequence :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm Syntax.InterfaceTypeDefinition
interfaceTypeDefinitionSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceTypeDefinitionSequence2 :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2 -> Phantoms.TTerm Syntax.InterfaceTypeDefinition
interfaceTypeDefinitionSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceTypeDefinition_Sequence :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.FieldsDefinition -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence
interfaceTypeDefinition_Sequence description name implementsInterfaces directives fieldsDefinition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm implementsInterfaces)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm fieldsDefinition)}]}))

interfaceTypeDefinition_Sequence2 :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ImplementsInterfaces -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2
interfaceTypeDefinition_Sequence2 description name implementsInterfaces directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm implementsInterfaces)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

interfaceTypeDefinition_Sequence2Description :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2 -> Phantoms.TTerm (Maybe Syntax.Description)
interfaceTypeDefinition_Sequence2Description x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeDefinition_Sequence2Directives :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2 -> Phantoms.TTerm (Maybe Syntax.Directives)
interfaceTypeDefinition_Sequence2Directives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeDefinition_Sequence2ImplementsInterfaces :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2 -> Phantoms.TTerm Syntax.ImplementsInterfaces
interfaceTypeDefinition_Sequence2ImplementsInterfaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
        Core.projectionField = (Core.Name "ImplementsInterfaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeDefinition_Sequence2Name :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2 -> Phantoms.TTerm Syntax.Name
interfaceTypeDefinition_Sequence2Name x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeDefinition_Sequence2WithDescription :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2 -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2
interfaceTypeDefinition_Sequence2WithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeDefinition_Sequence2WithDirectives :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2 -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2
interfaceTypeDefinition_Sequence2WithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

interfaceTypeDefinition_Sequence2WithImplementsInterfaces :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2 -> Phantoms.TTerm Syntax.ImplementsInterfaces -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2
interfaceTypeDefinition_Sequence2WithImplementsInterfaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeDefinition_Sequence2WithName :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence2
interfaceTypeDefinition_Sequence2WithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeDefinition_SequenceDescription :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Description)
interfaceTypeDefinition_SequenceDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeDefinition_SequenceDirectives :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives)
interfaceTypeDefinition_SequenceDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeDefinition_SequenceFieldsDefinition :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm Syntax.FieldsDefinition
interfaceTypeDefinition_SequenceFieldsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
        Core.projectionField = (Core.Name "FieldsDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeDefinition_SequenceImplementsInterfaces :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces)
interfaceTypeDefinition_SequenceImplementsInterfaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
        Core.projectionField = (Core.Name "ImplementsInterfaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeDefinition_SequenceName :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm Syntax.Name
interfaceTypeDefinition_SequenceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeDefinition_SequenceWithDescription :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence
interfaceTypeDefinition_SequenceWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeDefinition_SequenceWithDirectives :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence
interfaceTypeDefinition_SequenceWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeDefinition_SequenceWithFieldsDefinition :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm Syntax.FieldsDefinition -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence
interfaceTypeDefinition_SequenceWithFieldsDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

interfaceTypeDefinition_SequenceWithImplementsInterfaces :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence
interfaceTypeDefinition_SequenceWithImplementsInterfaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeDefinition_SequenceWithName :: Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.InterfaceTypeDefinition_Sequence
interfaceTypeDefinition_SequenceWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeDefinition_Sequence"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeExtensionSequence :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence -> Phantoms.TTerm Syntax.InterfaceTypeExtension
interfaceTypeExtensionSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceTypeExtensionSequence2 :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.InterfaceTypeExtension
interfaceTypeExtensionSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceTypeExtensionSequence3 :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence3 -> Phantoms.TTerm Syntax.InterfaceTypeExtension
interfaceTypeExtensionSequence3 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence3"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceTypeExtension_Sequence :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.FieldsDefinition -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence
interfaceTypeExtension_Sequence name implementsInterfaces directives fieldsDefinition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm implementsInterfaces)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm fieldsDefinition)}]}))

interfaceTypeExtension_Sequence2 :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2
interfaceTypeExtension_Sequence2 name implementsInterfaces directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm implementsInterfaces)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

interfaceTypeExtension_Sequence2Directives :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Directives
interfaceTypeExtension_Sequence2Directives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeExtension_Sequence2ImplementsInterfaces :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2 -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces)
interfaceTypeExtension_Sequence2ImplementsInterfaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "ImplementsInterfaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeExtension_Sequence2Name :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Name
interfaceTypeExtension_Sequence2Name x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeExtension_Sequence2WithDirectives :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2
interfaceTypeExtension_Sequence2WithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

interfaceTypeExtension_Sequence2WithImplementsInterfaces :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2 -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2
interfaceTypeExtension_Sequence2WithImplementsInterfaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeExtension_Sequence2WithName :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence2
interfaceTypeExtension_Sequence2WithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeExtension_Sequence3 :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ImplementsInterfaces -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence3
interfaceTypeExtension_Sequence3 name implementsInterfaces =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm implementsInterfaces)}]}))

interfaceTypeExtension_Sequence3ImplementsInterfaces :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence3 -> Phantoms.TTerm Syntax.ImplementsInterfaces
interfaceTypeExtension_Sequence3ImplementsInterfaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence3"),
        Core.projectionField = (Core.Name "ImplementsInterfaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeExtension_Sequence3Name :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence3 -> Phantoms.TTerm Syntax.Name
interfaceTypeExtension_Sequence3Name x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence3"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeExtension_Sequence3WithImplementsInterfaces :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence3 -> Phantoms.TTerm Syntax.ImplementsInterfaces -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence3
interfaceTypeExtension_Sequence3WithImplementsInterfaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence3"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

interfaceTypeExtension_Sequence3WithName :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence3 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence3
interfaceTypeExtension_Sequence3WithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence3"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeExtension_SequenceDirectives :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives)
interfaceTypeExtension_SequenceDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeExtension_SequenceFieldsDefinition :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence -> Phantoms.TTerm Syntax.FieldsDefinition
interfaceTypeExtension_SequenceFieldsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "FieldsDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeExtension_SequenceImplementsInterfaces :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces)
interfaceTypeExtension_SequenceImplementsInterfaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "ImplementsInterfaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeExtension_SequenceName :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence -> Phantoms.TTerm Syntax.Name
interfaceTypeExtension_SequenceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceTypeExtension_SequenceWithDirectives :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence
interfaceTypeExtension_SequenceWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeExtension_SequenceWithFieldsDefinition :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence -> Phantoms.TTerm Syntax.FieldsDefinition -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence
interfaceTypeExtension_SequenceWithFieldsDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

interfaceTypeExtension_SequenceWithImplementsInterfaces :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence
interfaceTypeExtension_SequenceWithImplementsInterfaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceTypeExtension_SequenceWithName :: Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.InterfaceTypeExtension_Sequence
interfaceTypeExtension_SequenceWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.InterfaceTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

listType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ListType
listType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.ListType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

listValueSequence :: Phantoms.TTerm Syntax.ListValue_Sequence -> Phantoms.TTerm Syntax.ListValue
listValueSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ListValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

listValueSequence2 :: Phantoms.TTerm [Syntax.Value] -> Phantoms.TTerm Syntax.ListValue
listValueSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ListValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

listValue_Sequence :: Phantoms.TTerm Syntax.ListValue_Sequence
listValue_Sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ListValue_Sequence"),
      Core.recordFields = []}))

name :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Name
name x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.Name"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

namedType :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.NamedType
namedType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.NamedType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

nonNullTypeList :: Phantoms.TTerm Syntax.ListType -> Phantoms.TTerm Syntax.NonNullType
nonNullTypeList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.NonNullType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonNullTypeNamed :: Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.NonNullType
nonNullTypeNamed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.NonNullType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nullValue :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.NullValue
nullValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.NullValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectField :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.ObjectField
objectField name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

objectFieldName :: Phantoms.TTerm Syntax.ObjectField -> Phantoms.TTerm Syntax.Name
objectFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectField"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectFieldValue :: Phantoms.TTerm Syntax.ObjectField -> Phantoms.TTerm Syntax.Value
objectFieldValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectField"),
        Core.projectionField = (Core.Name "Value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectFieldWithName :: Phantoms.TTerm Syntax.ObjectField -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ObjectField
objectFieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectField"),
              Core.projectionField = (Core.Name "Value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectFieldWithValue :: Phantoms.TTerm Syntax.ObjectField -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.ObjectField
objectFieldWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectField"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectTypeDefinition :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm (Maybe Syntax.FieldsDefinition) -> Phantoms.TTerm Syntax.ObjectTypeDefinition
objectTypeDefinition description name implementsInterfaces directives fieldsDefinition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm implementsInterfaces)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm fieldsDefinition)}]}))

objectTypeDefinitionDescription :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Description)
objectTypeDefinitionDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeDefinitionDirectives :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Directives)
objectTypeDefinitionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeDefinitionFieldsDefinition :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm (Maybe Syntax.FieldsDefinition)
objectTypeDefinitionFieldsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
        Core.projectionField = (Core.Name "FieldsDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeDefinitionImplementsInterfaces :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces)
objectTypeDefinitionImplementsInterfaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
        Core.projectionField = (Core.Name "ImplementsInterfaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeDefinitionName :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm Syntax.Name
objectTypeDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeDefinitionWithDescription :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.ObjectTypeDefinition
objectTypeDefinitionWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectTypeDefinitionWithDirectives :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.ObjectTypeDefinition
objectTypeDefinitionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectTypeDefinitionWithFieldsDefinition :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm (Maybe Syntax.FieldsDefinition) -> Phantoms.TTerm Syntax.ObjectTypeDefinition
objectTypeDefinitionWithFieldsDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectTypeDefinitionWithImplementsInterfaces :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm Syntax.ObjectTypeDefinition
objectTypeDefinitionWithImplementsInterfaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectTypeDefinitionWithName :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ObjectTypeDefinition
objectTypeDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeDefinition"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectTypeExtensionSequence :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence -> Phantoms.TTerm Syntax.ObjectTypeExtension
objectTypeExtensionSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectTypeExtensionSequence2 :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.ObjectTypeExtension
objectTypeExtensionSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectTypeExtensionSequence3 :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence3 -> Phantoms.TTerm Syntax.ObjectTypeExtension
objectTypeExtensionSequence3 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence3"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectTypeExtension_Sequence :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.FieldsDefinition -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence
objectTypeExtension_Sequence name implementsInterfaces directives fieldsDefinition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm implementsInterfaces)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm fieldsDefinition)}]}))

objectTypeExtension_Sequence2 :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2
objectTypeExtension_Sequence2 name implementsInterfaces directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm implementsInterfaces)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

objectTypeExtension_Sequence2Directives :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2 -> Phantoms.TTerm (Maybe Syntax.Directives)
objectTypeExtension_Sequence2Directives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeExtension_Sequence2ImplementsInterfaces :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2 -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces)
objectTypeExtension_Sequence2ImplementsInterfaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "ImplementsInterfaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeExtension_Sequence2Name :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Name
objectTypeExtension_Sequence2Name x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeExtension_Sequence2WithDirectives :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2 -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2
objectTypeExtension_Sequence2WithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectTypeExtension_Sequence2WithImplementsInterfaces :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2 -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2
objectTypeExtension_Sequence2WithImplementsInterfaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectTypeExtension_Sequence2WithName :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence2
objectTypeExtension_Sequence2WithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectTypeExtension_Sequence3 :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ImplementsInterfaces -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence3
objectTypeExtension_Sequence3 name implementsInterfaces =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm implementsInterfaces)}]}))

objectTypeExtension_Sequence3ImplementsInterfaces :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence3 -> Phantoms.TTerm Syntax.ImplementsInterfaces
objectTypeExtension_Sequence3ImplementsInterfaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence3"),
        Core.projectionField = (Core.Name "ImplementsInterfaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeExtension_Sequence3Name :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence3 -> Phantoms.TTerm Syntax.Name
objectTypeExtension_Sequence3Name x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence3"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeExtension_Sequence3WithImplementsInterfaces :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence3 -> Phantoms.TTerm Syntax.ImplementsInterfaces -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence3
objectTypeExtension_Sequence3WithImplementsInterfaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence3"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectTypeExtension_Sequence3WithName :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence3 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence3
objectTypeExtension_Sequence3WithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence3"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectTypeExtension_SequenceDirectives :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives)
objectTypeExtension_SequenceDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeExtension_SequenceFieldsDefinition :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence -> Phantoms.TTerm Syntax.FieldsDefinition
objectTypeExtension_SequenceFieldsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "FieldsDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeExtension_SequenceImplementsInterfaces :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces)
objectTypeExtension_SequenceImplementsInterfaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "ImplementsInterfaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeExtension_SequenceName :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence -> Phantoms.TTerm Syntax.Name
objectTypeExtension_SequenceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectTypeExtension_SequenceWithDirectives :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence
objectTypeExtension_SequenceWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectTypeExtension_SequenceWithFieldsDefinition :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence -> Phantoms.TTerm Syntax.FieldsDefinition -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence
objectTypeExtension_SequenceWithFieldsDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectTypeExtension_SequenceWithImplementsInterfaces :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.ImplementsInterfaces) -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence
objectTypeExtension_SequenceWithImplementsInterfaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectTypeExtension_SequenceWithName :: Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ObjectTypeExtension_Sequence
objectTypeExtension_SequenceWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ImplementsInterfaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "ImplementsInterfaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "FieldsDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "FieldsDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectValueSequence :: Phantoms.TTerm Syntax.ObjectValue_Sequence -> Phantoms.TTerm Syntax.ObjectValue
objectValueSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectValueSequence2 :: Phantoms.TTerm [Syntax.ObjectField] -> Phantoms.TTerm Syntax.ObjectValue
objectValueSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.ObjectValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectValue_Sequence :: Phantoms.TTerm Syntax.ObjectValue_Sequence
objectValue_Sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ObjectValue_Sequence"),
      Core.recordFields = []}))

operationDefinitionSelectionSet :: Phantoms.TTerm Syntax.SelectionSet -> Phantoms.TTerm Syntax.OperationDefinition
operationDefinitionSelectionSet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "SelectionSet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

operationDefinitionSequence :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm Syntax.OperationDefinition
operationDefinitionSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

operationDefinition_Sequence :: Phantoms.TTerm Syntax.OperationType -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm (Maybe Syntax.VariablesDefinition) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.SelectionSet -> Phantoms.TTerm Syntax.OperationDefinition_Sequence
operationDefinition_Sequence operationType name variablesDefinition directives selectionSet =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "OperationType"),
          Core.fieldTerm = (Phantoms.unTTerm operationType)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "VariablesDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm variablesDefinition)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Phantoms.unTTerm selectionSet)}]}))

operationDefinition_SequenceDirectives :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives)
operationDefinition_SequenceDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

operationDefinition_SequenceName :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Name)
operationDefinition_SequenceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

operationDefinition_SequenceOperationType :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm Syntax.OperationType
operationDefinition_SequenceOperationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
        Core.projectionField = (Core.Name "OperationType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

operationDefinition_SequenceSelectionSet :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm Syntax.SelectionSet
operationDefinition_SequenceSelectionSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
        Core.projectionField = (Core.Name "SelectionSet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

operationDefinition_SequenceVariablesDefinition :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.VariablesDefinition)
operationDefinition_SequenceVariablesDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
        Core.projectionField = (Core.Name "VariablesDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

operationDefinition_SequenceWithDirectives :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.OperationDefinition_Sequence
operationDefinition_SequenceWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "OperationType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "OperationType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "VariablesDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "VariablesDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

operationDefinition_SequenceWithName :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.OperationDefinition_Sequence
operationDefinition_SequenceWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "OperationType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "OperationType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "VariablesDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "VariablesDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

operationDefinition_SequenceWithOperationType :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm Syntax.OperationType -> Phantoms.TTerm Syntax.OperationDefinition_Sequence
operationDefinition_SequenceWithOperationType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "OperationType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "VariablesDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "VariablesDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

operationDefinition_SequenceWithSelectionSet :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm Syntax.SelectionSet -> Phantoms.TTerm Syntax.OperationDefinition_Sequence
operationDefinition_SequenceWithSelectionSet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "OperationType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "OperationType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "VariablesDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "VariablesDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

operationDefinition_SequenceWithVariablesDefinition :: Phantoms.TTerm Syntax.OperationDefinition_Sequence -> Phantoms.TTerm (Maybe Syntax.VariablesDefinition) -> Phantoms.TTerm Syntax.OperationDefinition_Sequence
operationDefinition_SequenceWithVariablesDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "OperationType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "OperationType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "VariablesDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SelectionSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.OperationDefinition_Sequence"),
              Core.projectionField = (Core.Name "SelectionSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

operationTypeMutation :: Phantoms.TTerm Syntax.OperationType
operationTypeMutation =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.OperationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Mutation"),
        Core.fieldTerm = Core.TermUnit}}))

operationTypeQuery :: Phantoms.TTerm Syntax.OperationType
operationTypeQuery =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.OperationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Query"),
        Core.fieldTerm = Core.TermUnit}}))

operationTypeSubscription :: Phantoms.TTerm Syntax.OperationType
operationTypeSubscription =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.OperationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Subscription"),
        Core.fieldTerm = Core.TermUnit}}))

rootOperationTypeDefinition :: Phantoms.TTerm Syntax.OperationType -> Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.RootOperationTypeDefinition
rootOperationTypeDefinition operationType namedType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.RootOperationTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "OperationType"),
          Core.fieldTerm = (Phantoms.unTTerm operationType)},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Phantoms.unTTerm namedType)}]}))

rootOperationTypeDefinitionNamedType :: Phantoms.TTerm Syntax.RootOperationTypeDefinition -> Phantoms.TTerm Syntax.NamedType
rootOperationTypeDefinitionNamedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.RootOperationTypeDefinition"),
        Core.projectionField = (Core.Name "NamedType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rootOperationTypeDefinitionOperationType :: Phantoms.TTerm Syntax.RootOperationTypeDefinition -> Phantoms.TTerm Syntax.OperationType
rootOperationTypeDefinitionOperationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.RootOperationTypeDefinition"),
        Core.projectionField = (Core.Name "OperationType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rootOperationTypeDefinitionWithNamedType :: Phantoms.TTerm Syntax.RootOperationTypeDefinition -> Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.RootOperationTypeDefinition
rootOperationTypeDefinitionWithNamedType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.RootOperationTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "OperationType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.RootOperationTypeDefinition"),
              Core.projectionField = (Core.Name "OperationType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rootOperationTypeDefinitionWithOperationType :: Phantoms.TTerm Syntax.RootOperationTypeDefinition -> Phantoms.TTerm Syntax.OperationType -> Phantoms.TTerm Syntax.RootOperationTypeDefinition
rootOperationTypeDefinitionWithOperationType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.RootOperationTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "OperationType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.RootOperationTypeDefinition"),
              Core.projectionField = (Core.Name "NamedType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

scalarTypeDefinition :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.ScalarTypeDefinition
scalarTypeDefinition description name directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

scalarTypeDefinitionDescription :: Phantoms.TTerm Syntax.ScalarTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Description)
scalarTypeDefinitionDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scalarTypeDefinitionDirectives :: Phantoms.TTerm Syntax.ScalarTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Directives)
scalarTypeDefinitionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scalarTypeDefinitionName :: Phantoms.TTerm Syntax.ScalarTypeDefinition -> Phantoms.TTerm Syntax.Name
scalarTypeDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scalarTypeDefinitionWithDescription :: Phantoms.TTerm Syntax.ScalarTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.ScalarTypeDefinition
scalarTypeDefinitionWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

scalarTypeDefinitionWithDirectives :: Phantoms.TTerm Syntax.ScalarTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.ScalarTypeDefinition
scalarTypeDefinitionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

scalarTypeDefinitionWithName :: Phantoms.TTerm Syntax.ScalarTypeDefinition -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ScalarTypeDefinition
scalarTypeDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

scalarTypeExtension :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.ScalarTypeExtension
scalarTypeExtension name directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeExtension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

scalarTypeExtensionDirectives :: Phantoms.TTerm Syntax.ScalarTypeExtension -> Phantoms.TTerm Syntax.Directives
scalarTypeExtensionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeExtension"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scalarTypeExtensionName :: Phantoms.TTerm Syntax.ScalarTypeExtension -> Phantoms.TTerm Syntax.Name
scalarTypeExtensionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeExtension"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scalarTypeExtensionWithDirectives :: Phantoms.TTerm Syntax.ScalarTypeExtension -> Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.ScalarTypeExtension
scalarTypeExtensionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeExtension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeExtension"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

scalarTypeExtensionWithName :: Phantoms.TTerm Syntax.ScalarTypeExtension -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ScalarTypeExtension
scalarTypeExtensionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeExtension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.ScalarTypeExtension"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaDefinition :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.RootOperationTypeDefinition -> Phantoms.TTerm Syntax.SchemaDefinition
schemaDefinition description directives rootOperationTypeDefinition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "RootOperationTypeDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm rootOperationTypeDefinition)}]}))

schemaDefinitionDescription :: Phantoms.TTerm Syntax.SchemaDefinition -> Phantoms.TTerm (Maybe Syntax.Description)
schemaDefinitionDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaDefinitionDirectives :: Phantoms.TTerm Syntax.SchemaDefinition -> Phantoms.TTerm (Maybe Syntax.Directives)
schemaDefinitionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaDefinitionRootOperationTypeDefinition :: Phantoms.TTerm Syntax.SchemaDefinition -> Phantoms.TTerm Syntax.RootOperationTypeDefinition
schemaDefinitionRootOperationTypeDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
        Core.projectionField = (Core.Name "RootOperationTypeDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaDefinitionWithDescription :: Phantoms.TTerm Syntax.SchemaDefinition -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.SchemaDefinition
schemaDefinitionWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "RootOperationTypeDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
              Core.projectionField = (Core.Name "RootOperationTypeDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaDefinitionWithDirectives :: Phantoms.TTerm Syntax.SchemaDefinition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.SchemaDefinition
schemaDefinitionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "RootOperationTypeDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
              Core.projectionField = (Core.Name "RootOperationTypeDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaDefinitionWithRootOperationTypeDefinition :: Phantoms.TTerm Syntax.SchemaDefinition -> Phantoms.TTerm Syntax.RootOperationTypeDefinition -> Phantoms.TTerm Syntax.SchemaDefinition
schemaDefinitionWithRootOperationTypeDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "RootOperationTypeDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

schemaExtensionSequence :: Phantoms.TTerm Syntax.SchemaExtension_Sequence -> Phantoms.TTerm Syntax.SchemaExtension
schemaExtensionSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaExtensionSequence2 :: Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.SchemaExtension
schemaExtensionSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaExtension_Sequence :: Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.RootOperationTypeDefinition -> Phantoms.TTerm Syntax.SchemaExtension_Sequence
schemaExtension_Sequence directives rootOperationTypeDefinition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.SchemaExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "RootOperationTypeDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm rootOperationTypeDefinition)}]}))

schemaExtension_SequenceDirectives :: Phantoms.TTerm Syntax.SchemaExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives)
schemaExtension_SequenceDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaExtension_Sequence"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaExtension_SequenceRootOperationTypeDefinition :: Phantoms.TTerm Syntax.SchemaExtension_Sequence -> Phantoms.TTerm Syntax.RootOperationTypeDefinition
schemaExtension_SequenceRootOperationTypeDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaExtension_Sequence"),
        Core.projectionField = (Core.Name "RootOperationTypeDefinition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaExtension_SequenceWithDirectives :: Phantoms.TTerm Syntax.SchemaExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.SchemaExtension_Sequence
schemaExtension_SequenceWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.SchemaExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "RootOperationTypeDefinition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaExtension_Sequence"),
              Core.projectionField = (Core.Name "RootOperationTypeDefinition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaExtension_SequenceWithRootOperationTypeDefinition :: Phantoms.TTerm Syntax.SchemaExtension_Sequence -> Phantoms.TTerm Syntax.RootOperationTypeDefinition -> Phantoms.TTerm Syntax.SchemaExtension_Sequence
schemaExtension_SequenceWithRootOperationTypeDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.SchemaExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.SchemaExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "RootOperationTypeDefinition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selectionField :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Selection
selectionField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Selection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Field"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectionFragmentSpread :: Phantoms.TTerm Syntax.FragmentSpread -> Phantoms.TTerm Syntax.Selection
selectionFragmentSpread x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Selection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "FragmentSpread"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectionInlineFragment :: Phantoms.TTerm Syntax.InlineFragment -> Phantoms.TTerm Syntax.Selection
selectionInlineFragment x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Selection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "InlineFragment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectionSet :: Phantoms.TTerm [Syntax.Selection] -> Phantoms.TTerm Syntax.SelectionSet
selectionSet x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.SelectionSet"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

stringValue :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringValue
stringValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.StringValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

typeConditionNamedType :: Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.TypeCondition
typeConditionNamedType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "NamedType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeConditionOn :: Phantoms.TTerm Syntax.TypeCondition
typeConditionOn =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "On"),
        Core.fieldTerm = Core.TermUnit}}))

typeDefinitionEnum :: Phantoms.TTerm Syntax.EnumTypeDefinition -> Phantoms.TTerm Syntax.TypeDefinition
typeDefinitionEnum x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeDefinitionInputObject :: Phantoms.TTerm Syntax.InputObjectTypeDefinition -> Phantoms.TTerm Syntax.TypeDefinition
typeDefinitionInputObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inputObject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeDefinitionInterface :: Phantoms.TTerm Syntax.InterfaceTypeDefinition -> Phantoms.TTerm Syntax.TypeDefinition
typeDefinitionInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeDefinitionObject :: Phantoms.TTerm Syntax.ObjectTypeDefinition -> Phantoms.TTerm Syntax.TypeDefinition
typeDefinitionObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeDefinitionScalar :: Phantoms.TTerm Syntax.ScalarTypeDefinition -> Phantoms.TTerm Syntax.TypeDefinition
typeDefinitionScalar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scalar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeDefinitionUnion :: Phantoms.TTerm Syntax.UnionTypeDefinition -> Phantoms.TTerm Syntax.TypeDefinition
typeDefinitionUnion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExtensionEnum :: Phantoms.TTerm Syntax.EnumTypeExtension -> Phantoms.TTerm Syntax.TypeExtension
typeExtensionEnum x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExtensionInputObject :: Phantoms.TTerm Syntax.InputObjectTypeExtension -> Phantoms.TTerm Syntax.TypeExtension
typeExtensionInputObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inputObject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExtensionInterface :: Phantoms.TTerm Syntax.InterfaceTypeExtension -> Phantoms.TTerm Syntax.TypeExtension
typeExtensionInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExtensionObject :: Phantoms.TTerm Syntax.ObjectTypeExtension -> Phantoms.TTerm Syntax.TypeExtension
typeExtensionObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExtensionScalar :: Phantoms.TTerm Syntax.ScalarTypeExtension -> Phantoms.TTerm Syntax.TypeExtension
typeExtensionScalar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scalar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExtensionUnion :: Phantoms.TTerm Syntax.UnionTypeExtension -> Phantoms.TTerm Syntax.TypeExtension
typeExtensionUnion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeList :: Phantoms.TTerm Syntax.ListType -> Phantoms.TTerm Syntax.Type
typeList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeNamed :: Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.Type
typeNamed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeNonNull :: Phantoms.TTerm Syntax.NonNullType -> Phantoms.TTerm Syntax.Type
typeNonNull x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonNull"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSystemDefinitionDirective :: Phantoms.TTerm Syntax.DirectiveDefinition -> Phantoms.TTerm Syntax.TypeSystemDefinition
typeSystemDefinitionDirective x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "directive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSystemDefinitionOrExtensionDefinition :: Phantoms.TTerm Syntax.TypeSystemDefinition -> Phantoms.TTerm Syntax.TypeSystemDefinitionOrExtension
typeSystemDefinitionOrExtensionDefinition x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDefinitionOrExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSystemDefinitionOrExtensionExtension :: Phantoms.TTerm Syntax.TypeSystemExtension -> Phantoms.TTerm Syntax.TypeSystemDefinitionOrExtension
typeSystemDefinitionOrExtensionExtension x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDefinitionOrExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extension"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSystemDefinitionSchema :: Phantoms.TTerm Syntax.SchemaDefinition -> Phantoms.TTerm Syntax.TypeSystemDefinition
typeSystemDefinitionSchema x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "schema"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSystemDefinitionType :: Phantoms.TTerm Syntax.TypeDefinition -> Phantoms.TTerm Syntax.TypeSystemDefinition
typeSystemDefinitionType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDefinition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSystemDirectiveLocationARGUMENT_DEFINITION :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationARGUMENT_DEFINITION =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ARGUMENT_DEFINITION"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDirectiveLocationENUM :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationENUM =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ENUM"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDirectiveLocationENUM_VALUE :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationENUM_VALUE =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ENUM_VALUE"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDirectiveLocationFIELD_DEFINITION :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationFIELD_DEFINITION =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "FIELD_DEFINITION"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDirectiveLocationINPUT_FIELD_DEFINITION :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationINPUT_FIELD_DEFINITION =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "INPUT_FIELD_DEFINITION"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDirectiveLocationINPUT_OBJECT :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationINPUT_OBJECT =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "INPUT_OBJECT"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDirectiveLocationINTERFACE :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationINTERFACE =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "INTERFACE"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDirectiveLocationOBJECT :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationOBJECT =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "OBJECT"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDirectiveLocationSCALAR :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationSCALAR =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "SCALAR"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDirectiveLocationSCHEMA :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationSCHEMA =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "SCHEMA"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDirectiveLocationUNION :: Phantoms.TTerm Syntax.TypeSystemDirectiveLocation
typeSystemDirectiveLocationUNION =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDirectiveLocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "UNION"),
        Core.fieldTerm = Core.TermUnit}}))

typeSystemDocment :: Phantoms.TTerm [Syntax.TypeSystemDefinition] -> Phantoms.TTerm Syntax.TypeSystemDocment
typeSystemDocment x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemDocment"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

typeSystemExtensionDocument :: Phantoms.TTerm [Syntax.TypeSystemDefinitionOrExtension] -> Phantoms.TTerm Syntax.TypeSystemExtensionDocument
typeSystemExtensionDocument x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemExtensionDocument"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

typeSystemExtensionSchema :: Phantoms.TTerm Syntax.SchemaExtension -> Phantoms.TTerm Syntax.TypeSystemExtension
typeSystemExtensionSchema x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "schema"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSystemExtensionType :: Phantoms.TTerm Syntax.TypeExtension -> Phantoms.TTerm Syntax.TypeSystemExtension
typeSystemExtensionType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.TypeSystemExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unArguments :: Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm [Syntax.Argument]
unArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.Arguments")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unArgumentsDefinition :: Phantoms.TTerm Syntax.ArgumentsDefinition -> Phantoms.TTerm [Syntax.InputValueDefinition]
unArgumentsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.ArgumentsDefinition")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDefaultValue :: Phantoms.TTerm Syntax.DefaultValue -> Phantoms.TTerm Syntax.Value
unDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.DefaultValue")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDescription :: Phantoms.TTerm Syntax.Description -> Phantoms.TTerm Syntax.StringValue
unDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.Description")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDirectives :: Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm [Syntax.Directive]
unDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.Directives")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDocument :: Phantoms.TTerm Syntax.Document -> Phantoms.TTerm [Syntax.Definition]
unDocument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.Document")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unEnumValue :: Phantoms.TTerm Syntax.EnumValue -> Phantoms.TTerm Syntax.Name
unEnumValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.EnumValue")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unEnumValuesDefinition :: Phantoms.TTerm Syntax.EnumValuesDefinition -> Phantoms.TTerm [Syntax.EnumValueDefinition]
unEnumValuesDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.EnumValuesDefinition")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unExecutableDocument :: Phantoms.TTerm Syntax.ExecutableDocument -> Phantoms.TTerm [Syntax.ExecutableDefinition]
unExecutableDocument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.ExecutableDocument")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unFieldsDefinition :: Phantoms.TTerm Syntax.FieldsDefinition -> Phantoms.TTerm [Syntax.FieldDefinition]
unFieldsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.FieldsDefinition")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unFloatValue :: Phantoms.TTerm Syntax.FloatValue -> Phantoms.TTerm String
unFloatValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.FloatValue")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unFragmentName :: Phantoms.TTerm Syntax.FragmentName -> Phantoms.TTerm Syntax.Name
unFragmentName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.FragmentName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unInputFieldsDefinition :: Phantoms.TTerm Syntax.InputFieldsDefinition -> Phantoms.TTerm [Syntax.InputValueDefinition]
unInputFieldsDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.InputFieldsDefinition")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unIntValue :: Phantoms.TTerm Syntax.IntValue -> Phantoms.TTerm String
unIntValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.IntValue")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unListType :: Phantoms.TTerm Syntax.ListType -> Phantoms.TTerm Syntax.Type
unListType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.ListType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm String
unName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.Name")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNamedType :: Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.Name
unNamedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.NamedType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNullValue :: Phantoms.TTerm Syntax.NullValue -> Phantoms.TTerm ()
unNullValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.NullValue")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSelectionSet :: Phantoms.TTerm Syntax.SelectionSet -> Phantoms.TTerm [Syntax.Selection]
unSelectionSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.SelectionSet")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unStringValue :: Phantoms.TTerm Syntax.StringValue -> Phantoms.TTerm String
unStringValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.StringValue")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unTypeSystemDocment :: Phantoms.TTerm Syntax.TypeSystemDocment -> Phantoms.TTerm [Syntax.TypeSystemDefinition]
unTypeSystemDocment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.TypeSystemDocment")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unTypeSystemExtensionDocument :: Phantoms.TTerm Syntax.TypeSystemExtensionDocument -> Phantoms.TTerm [Syntax.TypeSystemDefinitionOrExtension]
unTypeSystemExtensionDocument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.TypeSystemExtensionDocument")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unVariable :: Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.Name
unVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphql.syntax.Variable")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionMemberTypesSequence :: Phantoms.TTerm Syntax.UnionMemberTypes_Sequence -> Phantoms.TTerm Syntax.UnionMemberTypes
unionMemberTypesSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unionMemberTypesSequence2 :: Phantoms.TTerm Syntax.UnionMemberTypes_Sequence2 -> Phantoms.TTerm Syntax.UnionMemberTypes
unionMemberTypesSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unionMemberTypes_Sequence :: Phantoms.TTerm Syntax.UnionMemberTypes -> Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.UnionMemberTypes_Sequence
unionMemberTypes_Sequence unionMemberTypes namedType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Phantoms.unTTerm unionMemberTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Phantoms.unTTerm namedType)}]}))

unionMemberTypes_Sequence2 :: Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.UnionMemberTypes_Sequence2
unionMemberTypes_Sequence2 or namedType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Or"),
          Core.fieldTerm = (Phantoms.unTTerm or)},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Phantoms.unTTerm namedType)}]}))

unionMemberTypes_Sequence2NamedType :: Phantoms.TTerm Syntax.UnionMemberTypes_Sequence2 -> Phantoms.TTerm Syntax.NamedType
unionMemberTypes_Sequence2NamedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence2"),
        Core.projectionField = (Core.Name "NamedType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionMemberTypes_Sequence2Or :: Phantoms.TTerm Syntax.UnionMemberTypes_Sequence2 -> Phantoms.TTerm (Maybe ())
unionMemberTypes_Sequence2Or x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence2"),
        Core.projectionField = (Core.Name "Or")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionMemberTypes_Sequence2WithNamedType :: Phantoms.TTerm Syntax.UnionMemberTypes_Sequence2 -> Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.UnionMemberTypes_Sequence2
unionMemberTypes_Sequence2WithNamedType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Or"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence2"),
              Core.projectionField = (Core.Name "Or")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unionMemberTypes_Sequence2WithOr :: Phantoms.TTerm Syntax.UnionMemberTypes_Sequence2 -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.UnionMemberTypes_Sequence2
unionMemberTypes_Sequence2WithOr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Or"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence2"),
              Core.projectionField = (Core.Name "NamedType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionMemberTypes_SequenceNamedType :: Phantoms.TTerm Syntax.UnionMemberTypes_Sequence -> Phantoms.TTerm Syntax.NamedType
unionMemberTypes_SequenceNamedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence"),
        Core.projectionField = (Core.Name "NamedType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionMemberTypes_SequenceUnionMemberTypes :: Phantoms.TTerm Syntax.UnionMemberTypes_Sequence -> Phantoms.TTerm Syntax.UnionMemberTypes
unionMemberTypes_SequenceUnionMemberTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence"),
        Core.projectionField = (Core.Name "UnionMemberTypes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionMemberTypes_SequenceWithNamedType :: Phantoms.TTerm Syntax.UnionMemberTypes_Sequence -> Phantoms.TTerm Syntax.NamedType -> Phantoms.TTerm Syntax.UnionMemberTypes_Sequence
unionMemberTypes_SequenceWithNamedType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence"),
              Core.projectionField = (Core.Name "UnionMemberTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unionMemberTypes_SequenceWithUnionMemberTypes :: Phantoms.TTerm Syntax.UnionMemberTypes_Sequence -> Phantoms.TTerm Syntax.UnionMemberTypes -> Phantoms.TTerm Syntax.UnionMemberTypes_Sequence
unionMemberTypes_SequenceWithUnionMemberTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "NamedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionMemberTypes_Sequence"),
              Core.projectionField = (Core.Name "NamedType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTypeDefinition :: Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm (Maybe Syntax.UnionMemberTypes) -> Phantoms.TTerm Syntax.UnionTypeDefinition
unionTypeDefinition description name directives unionMemberTypes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Phantoms.unTTerm unionMemberTypes)}]}))

unionTypeDefinitionDescription :: Phantoms.TTerm Syntax.UnionTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Description)
unionTypeDefinitionDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
        Core.projectionField = (Core.Name "Description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTypeDefinitionDirectives :: Phantoms.TTerm Syntax.UnionTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Directives)
unionTypeDefinitionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTypeDefinitionName :: Phantoms.TTerm Syntax.UnionTypeDefinition -> Phantoms.TTerm Syntax.Name
unionTypeDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTypeDefinitionUnionMemberTypes :: Phantoms.TTerm Syntax.UnionTypeDefinition -> Phantoms.TTerm (Maybe Syntax.UnionMemberTypes)
unionTypeDefinitionUnionMemberTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
        Core.projectionField = (Core.Name "UnionMemberTypes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTypeDefinitionWithDescription :: Phantoms.TTerm Syntax.UnionTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Description) -> Phantoms.TTerm Syntax.UnionTypeDefinition
unionTypeDefinitionWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "UnionMemberTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTypeDefinitionWithDirectives :: Phantoms.TTerm Syntax.UnionTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.UnionTypeDefinition
unionTypeDefinitionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "UnionMemberTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTypeDefinitionWithName :: Phantoms.TTerm Syntax.UnionTypeDefinition -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.UnionTypeDefinition
unionTypeDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "UnionMemberTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTypeDefinitionWithUnionMemberTypes :: Phantoms.TTerm Syntax.UnionTypeDefinition -> Phantoms.TTerm (Maybe Syntax.UnionMemberTypes) -> Phantoms.TTerm Syntax.UnionTypeDefinition
unionTypeDefinitionWithUnionMemberTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "Description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unionTypeExtensionSequence :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence -> Phantoms.TTerm Syntax.UnionTypeExtension
unionTypeExtensionSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unionTypeExtensionSequence2 :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.UnionTypeExtension
unionTypeExtensionSequence2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unionTypeExtension_Sequence :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.UnionMemberTypes -> Phantoms.TTerm Syntax.UnionTypeExtension_Sequence
unionTypeExtension_Sequence name directives unionMemberTypes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)},
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Phantoms.unTTerm unionMemberTypes)}]}))

unionTypeExtension_Sequence2 :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.UnionTypeExtension_Sequence2
unionTypeExtension_Sequence2 name directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

unionTypeExtension_Sequence2Directives :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Directives
unionTypeExtension_Sequence2Directives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTypeExtension_Sequence2Name :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Name
unionTypeExtension_Sequence2Name x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence2"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTypeExtension_Sequence2WithDirectives :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Directives -> Phantoms.TTerm Syntax.UnionTypeExtension_Sequence2
unionTypeExtension_Sequence2WithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unionTypeExtension_Sequence2WithName :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence2 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.UnionTypeExtension_Sequence2
unionTypeExtension_Sequence2WithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence2"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTypeExtension_SequenceDirectives :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives)
unionTypeExtension_SequenceDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTypeExtension_SequenceName :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence -> Phantoms.TTerm Syntax.Name
unionTypeExtension_SequenceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "Name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTypeExtension_SequenceUnionMemberTypes :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence -> Phantoms.TTerm Syntax.UnionMemberTypes
unionTypeExtension_SequenceUnionMemberTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
        Core.projectionField = (Core.Name "UnionMemberTypes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTypeExtension_SequenceWithDirectives :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.UnionTypeExtension_Sequence
unionTypeExtension_SequenceWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "UnionMemberTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTypeExtension_SequenceWithName :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.UnionTypeExtension_Sequence
unionTypeExtension_SequenceWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "UnionMemberTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTypeExtension_SequenceWithUnionMemberTypes :: Phantoms.TTerm Syntax.UnionTypeExtension_Sequence -> Phantoms.TTerm Syntax.UnionMemberTypes -> Phantoms.TTerm Syntax.UnionTypeExtension_Sequence
unionTypeExtension_SequenceWithUnionMemberTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.UnionTypeExtension_Sequence"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "UnionMemberTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

valueBoolean :: Phantoms.TTerm Syntax.BooleanValue -> Phantoms.TTerm Syntax.Value
valueBoolean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueEnum :: Phantoms.TTerm Syntax.EnumValue -> Phantoms.TTerm Syntax.Value
valueEnum x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueFloat :: Phantoms.TTerm Syntax.FloatValue -> Phantoms.TTerm Syntax.Value
valueFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueInt :: Phantoms.TTerm Syntax.IntValue -> Phantoms.TTerm Syntax.Value
valueInt x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueList :: Phantoms.TTerm Syntax.ListValue -> Phantoms.TTerm Syntax.Value
valueList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueNull :: Phantoms.TTerm Syntax.NullValue -> Phantoms.TTerm Syntax.Value
valueNull x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueObject :: Phantoms.TTerm Syntax.ObjectValue -> Phantoms.TTerm Syntax.Value
valueObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueString :: Phantoms.TTerm Syntax.StringValue -> Phantoms.TTerm Syntax.Value
valueString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueVariable :: Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.Value
valueVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphql.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

variable :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Variable
variable x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphql.syntax.Variable"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

variablesDefinition :: Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm (Maybe Syntax.DefaultValue) -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.VariablesDefinition
variablesDefinition variable type_ defaultValue directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm defaultValue)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))

variablesDefinitionDefaultValue :: Phantoms.TTerm Syntax.VariablesDefinition -> Phantoms.TTerm (Maybe Syntax.DefaultValue)
variablesDefinitionDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
        Core.projectionField = (Core.Name "DefaultValue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variablesDefinitionDirectives :: Phantoms.TTerm Syntax.VariablesDefinition -> Phantoms.TTerm (Maybe Syntax.Directives)
variablesDefinitionDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
        Core.projectionField = (Core.Name "Directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variablesDefinitionType :: Phantoms.TTerm Syntax.VariablesDefinition -> Phantoms.TTerm Syntax.Type
variablesDefinitionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
        Core.projectionField = (Core.Name "Type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variablesDefinitionVariable :: Phantoms.TTerm Syntax.VariablesDefinition -> Phantoms.TTerm Syntax.Variable
variablesDefinitionVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
        Core.projectionField = (Core.Name "Variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variablesDefinitionWithDefaultValue :: Phantoms.TTerm Syntax.VariablesDefinition -> Phantoms.TTerm (Maybe Syntax.DefaultValue) -> Phantoms.TTerm Syntax.VariablesDefinition
variablesDefinitionWithDefaultValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "Variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variablesDefinitionWithDirectives :: Phantoms.TTerm Syntax.VariablesDefinition -> Phantoms.TTerm (Maybe Syntax.Directives) -> Phantoms.TTerm Syntax.VariablesDefinition
variablesDefinitionWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "Variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "DefaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variablesDefinitionWithType :: Phantoms.TTerm Syntax.VariablesDefinition -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.VariablesDefinition
variablesDefinitionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "Variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "DefaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variablesDefinitionWithVariable :: Phantoms.TTerm Syntax.VariablesDefinition -> Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.VariablesDefinition
variablesDefinitionWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "Type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "DefaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "DefaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphql.syntax.VariablesDefinition"),
              Core.projectionField = (Core.Name "Directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
