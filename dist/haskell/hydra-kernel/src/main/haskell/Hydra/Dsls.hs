-- Note: this is an automatically generated file. Do not edit.
-- | Functions for generating domain-specific DSL modules from type modules

module Hydra.Dsls where
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Strip as Strip
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Collect forall type variable names from a type
collectForallVars :: Core.Type -> [Core.Name]
collectForallVars typ =
    case typ of
      Core.TypeAnnotated v0 -> collectForallVars (Core.annotatedTypeBody v0)
      Core.TypeForall v0 -> Lists.cons (Core.forallTypeParameter v0) (collectForallVars (Core.forallTypeBody v0))
      _ -> []
-- | Deduplicate bindings by appending numeric suffixes to duplicate names
deduplicateBindings :: [Core.Binding] -> [Core.Binding]
deduplicateBindings bindings =
    Lists.foldl (\acc -> \b ->
      let usedNames = Sets.fromList (Lists.map (\a -> Core.bindingName a) acc)
          uniqueName = Lexical.chooseUniqueName usedNames (Core.bindingName b)
      in (Lists.concat2 acc [
        Core.Binding {
          Core.bindingName = uniqueName,
          Core.bindingTerm = (Core.bindingTerm b),
          Core.bindingTypeScheme = (Core.bindingTypeScheme b)}])) [] bindings
-- | Generate a binding name for a DSL function from a type name
dslBindingName :: Core.Name -> Core.Name
dslBindingName n =

      let parts = Strings.splitOn "." (Core.unName n)
          localPart = Formatting.decapitalize (Names.localNameOf n)
          localResult = Core.Name localPart
      in (Maybes.maybe localResult (\nsParts -> Maybes.maybe localResult (\nsHeadTail ->
        let dslNsParts =
                Logic.ifElse (Equality.equal (Pairs.first nsHeadTail) "hydra") (Lists.concat2 [
                  "hydra",
                  "dsl"] (Pairs.second nsHeadTail)) (Lists.concat2 [
                  "hydra",
                  "dsl"] nsParts)
        in (Core.Name (Strings.intercalate "." (Lists.concat2 dslNsParts [
          localPart])))) (Lists.uncons nsParts)) (Lists.maybeInit parts))
-- | Generate a qualified DSL element name from a type name and local element name
dslDefinitionName :: Core.Name -> String -> Core.Name
dslDefinitionName typeName localName =

      let parts = Strings.splitOn "." (Core.unName typeName)
      in (Maybes.maybe (Core.Name localName) (\nsParts ->
        let dslNsParts =
                Maybes.maybe [
                  "hydra",
                  "dsl"] (\nsHeadTail -> Logic.ifElse (Equality.equal (Pairs.first nsHeadTail) "hydra") (Lists.concat2 [
                  "hydra",
                  "dsl"] (Pairs.second nsHeadTail)) (Lists.concat2 [
                  "hydra",
                  "dsl"] nsParts)) (Lists.uncons nsParts)
        in (Core.Name (Strings.intercalate "." (Lists.concat2 dslNsParts [
          localName])))) (Lists.maybeInit parts))
-- | Transform a type module into a DSL module
dslModule :: t0 -> Graph.Graph -> Packaging.Module -> Either Errors.Error (Maybe Packaging.Module)
dslModule cx graph mod =
    Eithers.bind (filterTypeBindings cx graph (Maybes.cat (Lists.map (\d -> case d of
      Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
        let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
            dataTerm =
                    Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (EncodeCore.type_ typ),
                      Core.annotatedTermAnnotation = (Maps.fromList [
                        (Constants.keyType, schemaTerm)])}))
        in Core.Binding {
          Core.bindingName = name,
          Core.bindingTerm = dataTerm,
          Core.bindingTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
            Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme v0)))
      _ -> Nothing) (Packaging.moduleDefinitions mod)))) (\typeBindings -> Logic.ifElse (Lists.null typeBindings) (Right Nothing) (Eithers.bind (Eithers.mapList (\b -> Eithers.bimap (\_e -> Errors.ErrorDecoding _e) (\x -> x) (generateBindingsForType cx graph b)) typeBindings) (\dslBindings -> Right (Just (Packaging.Module {
      Packaging.moduleDescription = (Just (Strings.cat [
        "DSL functions for ",
        (Packaging.unModuleName (Packaging.moduleName mod))])),
      Packaging.moduleName = (dslNamespace (Packaging.moduleName mod)),
      Packaging.moduleDependencies = (Lists.map (\ns -> Packaging.ModuleDependency {
        Packaging.moduleDependencyModule = ns,
        Packaging.moduleDependencyPackage = Nothing}) (Lists.nub (Lists.concat2 [
        Packaging.moduleName mod,
        (Packaging.ModuleName "hydra.typed")] (Lists.concat2 (Lists.map (\dep -> Packaging.moduleDependencyModule dep) (Packaging.moduleDependencies mod)) (Lists.map dslNamespace (Lists.map (\dep -> Packaging.moduleDependencyModule dep) (Packaging.moduleDependencies mod))))))),
      Packaging.moduleDefinitions = (Lists.map (\b -> Packaging.DefinitionTerm (Packaging.TermDefinition {
        Packaging.termDefinitionName = (Core.bindingName b),
        Packaging.termDefinitionTerm = (Core.bindingTerm b),
        Packaging.termDefinitionSignature = (Maybes.map Scoping.typeSchemeToTermSignature (Core.bindingTypeScheme b))})) (deduplicateBindings (Lists.concat dslBindings)))})))))
-- | Generate a DSL module namespace from a source module namespace
dslNamespace :: Packaging.ModuleName -> Packaging.ModuleName
dslNamespace ns =

      let parts = Strings.splitOn "." (Packaging.unModuleName ns)
          prefixFull =
                  Packaging.ModuleName (Strings.cat [
                    "hydra.dsl.",
                    (Packaging.unModuleName ns)])
      in (Maybes.maybe prefixFull (\ht -> Logic.ifElse (Equality.equal (Pairs.first ht) "hydra") (Packaging.ModuleName (Strings.cat [
        "hydra.dsl.",
        (Strings.intercalate "." (Pairs.second ht))])) prefixFull) (Lists.uncons parts))
-- | Build a TypeScheme with TypedTerm-wrapped parameter and result types
dslTypeScheme :: Core.Type -> [Core.Type] -> Core.Type -> Core.TypeScheme
dslTypeScheme origType paramTypes resultType =

      let typeVars = collectForallVars origType
          wrappedResult =
                  Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                    Core.applicationTypeArgument = resultType})
          funType =
                  Lists.foldr (\paramType -> \acc -> Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                      Core.applicationTypeArgument = paramType})),
                    Core.functionTypeCodomain = acc})) wrappedResult paramTypes
      in Core.TypeScheme {
        Core.typeSchemeVariables = typeVars,
        Core.typeSchemeBody = funType,
        Core.typeSchemeConstraints = Nothing}
-- | Filter bindings to only DSL-eligible type definitions
filterTypeBindings :: t0 -> t1 -> [Core.Binding] -> Either t2 [Core.Binding]
filterTypeBindings cx graph bindings =
    Eithers.map Maybes.cat (Eithers.mapList (isDslEligibleBinding cx graph) (Lists.filter Annotations.isNativeType bindings))
-- | Generate all DSL bindings for a type binding
generateBindingsForType :: t0 -> Graph.Graph -> Core.Binding -> Either Errors.DecodingError [Core.Binding]
generateBindingsForType cx graph b =

      let typeName = Core.bindingName b
      in (Eithers.bind (DecodeCore.type_ graph (Core.bindingTerm b)) (\rawType ->
        let typ = Strip.deannotateTypeParameters (Strip.deannotateType rawType)
        in (Right (case typ of
          Core.TypeRecord v0 -> Lists.concat [
            generateRecordConstructor rawType typeName v0,
            (Lists.map (generateRecordAccessor rawType typeName) v0),
            (Lists.map (generateRecordWithUpdater rawType typeName v0) v0)]
          Core.TypeUnion v0 -> Lists.map (generateUnionInjector rawType typeName) v0
          Core.TypeWrap v0 -> generateWrappedTypeAccessors rawType typeName v0
          _ -> []))))
-- | Generate a record field accessor function
generateRecordAccessor :: Core.Type -> Core.Name -> Core.FieldType -> Core.Binding
generateRecordAccessor origType typeName ft =

      let fieldName = Core.fieldTypeName ft
          accessorLocalName =
                  Strings.cat [
                    Formatting.decapitalize (Names.localNameOf typeName),
                    (Strings.intercalate "" (Lists.map (\s -> Formatting.capitalize s) (Strings.splitOn "." (Core.unName fieldName))))]
          accessorName = dslDefinitionName typeName accessorLocalName
          paramDomain =
                  Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                    Core.applicationTypeArgument = (nominalResultType typeName origType)})
          rawBody =
                  Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just paramDomain),
                    Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.typed.TypedTerm"),
                      Core.wrappedTermBody = (Core.TermInject (Core.Injection {
                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "application"),
                          Core.fieldTerm = (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Application"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "function"),
                                Core.fieldTerm = (Core.TermInject (Core.Injection {
                                  Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                  Core.injectionField = Core.Field {
                                    Core.fieldName = (Core.Name "project"),
                                    Core.fieldTerm = (Core.TermRecord (Core.Record {
                                      Core.recordTypeName = (Core.Name "hydra.core.Projection"),
                                      Core.recordFields = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "typeName"),
                                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName typeName)))}))},
                                        Core.Field {
                                          Core.fieldName = (Core.Name "fieldName"),
                                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName fieldName)))}))}]}))}}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "argument"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typed.TypedTerm")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}]}))}}))}))})
          description =
                  Strings.cat [
                    "DSL accessor for the ",
                    (Core.unName fieldName),
                    " field of ",
                    (Core.unName typeName)]
          body = Annotations.setTermDescription (Just description) rawBody
          ts = dslTypeScheme origType [
                nominalResultType typeName origType] (Core.fieldTypeType ft)
      in Core.Binding {
        Core.bindingName = accessorName,
        Core.bindingTerm = body,
        Core.bindingTypeScheme = (Just ts)}
-- | Generate a record constructor function
generateRecordConstructor :: Core.Type -> Core.Name -> [Core.FieldType] -> [Core.Binding]
generateRecordConstructor origType typeName fieldTypes =

      let dFields =
              Lists.map (\ft -> Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "name"),
                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldTypeName ft))))}))},
                  Core.Field {
                    Core.fieldName = (Core.Name "term"),
                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typed.TypedTerm")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name (Formatting.decapitalize (Names.localNameOf (Core.fieldTypeName ft)))))}))}]})) fieldTypes
          recordTerm =
                  Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.Name "hydra.typed.TypedTerm"),
                    Core.wrappedTermBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "record"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Record"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName typeName)))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "fields"),
                              Core.fieldTerm = (Core.TermList dFields)}]}))}}))})
          paramPairs =
                  Lists.map (\ft -> (
                    Formatting.decapitalize (Names.localNameOf (Core.fieldTypeName ft)),
                    (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                      Core.applicationTypeArgument = (Core.fieldTypeType ft)})))) fieldTypes
          rawBody =
                  Lists.foldl (\acc -> \pp -> Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name (Pairs.first pp)),
                    Core.lambdaDomain = (Just (Pairs.second pp)),
                    Core.lambdaBody = acc})) recordTerm (Lists.reverse paramPairs)
          description =
                  Strings.cat [
                    "DSL constructor for ",
                    (Core.unName typeName)]
          body = Annotations.setTermDescription (Just description) rawBody
          paramTypes = Lists.map (\ft -> Core.fieldTypeType ft) fieldTypes
          resultType = nominalResultType typeName origType
          ts = dslTypeScheme origType paramTypes resultType
      in [
        Core.Binding {
          Core.bindingName = (dslBindingName typeName),
          Core.bindingTerm = body,
          Core.bindingTypeScheme = (Just ts)}]
-- | Generate a withXxx record field updater function
generateRecordWithUpdater :: Core.Type -> Core.Name -> [Core.FieldType] -> Core.FieldType -> Core.Binding
generateRecordWithUpdater origType typeName allFields targetField =

      let targetFieldName = Core.fieldTypeName targetField
          updaterLocalName =
                  Strings.cat [
                    Formatting.decapitalize (Names.localNameOf typeName),
                    "With",
                    (Strings.intercalate "" (Lists.map (\s -> Formatting.capitalize s) (Strings.splitOn "." (Core.unName targetFieldName))))]
          updaterName = dslDefinitionName typeName updaterLocalName
          dFields =
                  Lists.map (\ft -> Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.core.Field"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "name"),
                        Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldTypeName ft))))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "term"),
                        Core.fieldTerm = (Logic.ifElse (Equality.equal (Core.unName (Core.fieldTypeName ft)) (Core.unName targetFieldName)) (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typed.TypedTerm")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "newVal"))})) (Core.TermInject (Core.Injection {
                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                          Core.injectionField = Core.Field {
                            Core.fieldName = (Core.Name "application"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Application"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "function"),
                                  Core.fieldTerm = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "project"),
                                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.core.Projection"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "typeName"),
                                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName typeName)))}))},
                                          Core.Field {
                                            Core.fieldName = (Core.Name "fieldName"),
                                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldTypeName ft))))}))}]}))}}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "argument"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typed.TypedTerm")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "original"))}))}]}))}})))}]})) allFields
          recDomain =
                  Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                    Core.applicationTypeArgument = (nominalResultType typeName origType)})
          fieldDomain =
                  Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                    Core.applicationTypeArgument = (Core.fieldTypeType targetField)})
          rawBody =
                  Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "original"),
                    Core.lambdaDomain = (Just recDomain),
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "newVal"),
                      Core.lambdaDomain = (Just fieldDomain),
                      Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.typed.TypedTerm"),
                        Core.wrappedTermBody = (Core.TermInject (Core.Injection {
                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                          Core.injectionField = Core.Field {
                            Core.fieldName = (Core.Name "record"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Record"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "typeName"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName typeName)))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "fields"),
                                  Core.fieldTerm = (Core.TermList dFields)}]}))}}))}))}))})
          description =
                  Strings.cat [
                    "DSL updater for the ",
                    (Core.unName targetFieldName),
                    " field of ",
                    (Core.unName typeName)]
          body = Annotations.setTermDescription (Just description) rawBody
          recType = nominalResultType typeName origType
          ts =
                  dslTypeScheme origType [
                    recType,
                    (Core.fieldTypeType targetField)] recType
      in Core.Binding {
        Core.bindingName = updaterName,
        Core.bindingTerm = body,
        Core.bindingTypeScheme = (Just ts)}
-- | Generate a union injection helper
generateUnionInjector :: Core.Type -> Core.Name -> Core.FieldType -> Core.Binding
generateUnionInjector origType typeName ft =

      let fieldName = Core.fieldTypeName ft
          fieldType = Core.fieldTypeType ft
          injectorLocalName =
                  Strings.cat [
                    Formatting.decapitalize (Names.localNameOf typeName),
                    (Strings.intercalate "" (Lists.map (\s -> Formatting.capitalize s) (Strings.splitOn "." (Core.unName fieldName))))]
          injectorName = dslDefinitionName typeName injectorLocalName
          isUnit =
                  (\t -> case (Strip.deannotateType t) of
                    Core.TypeUnit -> True
                    _ -> False) fieldType
          dFieldValue =
                  Logic.ifElse isUnit (Core.TermInject (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "unit"),
                      Core.fieldTerm = Core.TermUnit}})) (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typed.TypedTerm")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))
          injectionTerm =
                  Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.Name "hydra.typed.TypedTerm"),
                    Core.wrappedTermBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName typeName)))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName fieldName)))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = dFieldValue}]}))}]}))}}))})
          variantDomain =
                  Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                    Core.applicationTypeArgument = (Core.fieldTypeType ft)})
          rawBody =
                  Logic.ifElse isUnit injectionTerm (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just variantDomain),
                    Core.lambdaBody = injectionTerm}))
          description =
                  Strings.cat [
                    "DSL injection for the ",
                    (Core.unName fieldName),
                    " variant of ",
                    (Core.unName typeName)]
          body = Annotations.setTermDescription (Just description) rawBody
          unionType = nominalResultType typeName origType
          ts = Logic.ifElse isUnit (dslTypeScheme origType [] unionType) (dslTypeScheme origType [
                Core.fieldTypeType ft] unionType)
      in Core.Binding {
        Core.bindingName = injectorName,
        Core.bindingTerm = body,
        Core.bindingTypeScheme = (Just ts)}
-- | Generate wrap/unwrap accessors for a wrapped type
generateWrappedTypeAccessors :: Core.Type -> Core.Name -> Core.Type -> [Core.Binding]
generateWrappedTypeAccessors origType typeName innerType =

      let localName = Names.localNameOf typeName
          wrapName = dslDefinitionName typeName (Formatting.decapitalize localName)
          unwrapLocalName =
                  Strings.cat [
                    "un",
                    localName]
          unwrapName = dslDefinitionName typeName unwrapLocalName
          wrapperType = nominalResultType typeName origType
          wrapDomain =
                  Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                    Core.applicationTypeArgument = innerType})
          rawWrapBody =
                  Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just wrapDomain),
                    Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.typed.TypedTerm"),
                      Core.wrappedTermBody = (Core.TermInject (Core.Injection {
                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "wrap"),
                          Core.fieldTerm = (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "typeName"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName typeName)))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "body"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typed.TypedTerm")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}]}))}}))}))})
          wrapDescription =
                  Strings.cat [
                    "DSL constructor for the ",
                    (Core.unName typeName),
                    " wrapper"]
          wrapBody = Annotations.setTermDescription (Just wrapDescription) rawWrapBody
          unwrapDomain =
                  Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                    Core.applicationTypeArgument = wrapperType})
          rawUnwrapBody =
                  Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just unwrapDomain),
                    Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.typed.TypedTerm"),
                      Core.wrappedTermBody = (Core.TermInject (Core.Injection {
                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "application"),
                          Core.fieldTerm = (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Application"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "function"),
                                Core.fieldTerm = (Core.TermInject (Core.Injection {
                                  Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                  Core.injectionField = Core.Field {
                                    Core.fieldName = (Core.Name "unwrap"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName typeName)))}))}}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "argument"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typed.TypedTerm")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}]}))}}))}))})
          unwrapDescription =
                  Strings.cat [
                    "DSL accessor for the body of ",
                    (Core.unName typeName)]
          unwrapBody = Annotations.setTermDescription (Just unwrapDescription) rawUnwrapBody
          wrapTs = dslTypeScheme origType [
                innerType] wrapperType
          unwrapTs = dslTypeScheme origType [
                wrapperType] innerType
      in [
        Core.Binding {
          Core.bindingName = wrapName,
          Core.bindingTerm = wrapBody,
          Core.bindingTypeScheme = (Just wrapTs)},
        Core.Binding {
          Core.bindingName = unwrapName,
          Core.bindingTerm = unwrapBody,
          Core.bindingTypeScheme = (Just unwrapTs)}]
-- | Check if a binding is eligible for DSL generation
isDslEligibleBinding :: t0 -> t1 -> Core.Binding -> Either t2 (Maybe Core.Binding)
isDslEligibleBinding cx graph b =

      let ns = Names.namespaceOf (Core.bindingName b)
      in (Logic.ifElse (Equality.equal (Maybes.maybe "" Packaging.unModuleName ns) "hydra.typed") (Right Nothing) (Right (Just b)))
-- | Build the nominal result type with type applications for forall variables
nominalResultType :: Core.Name -> Core.Type -> Core.Type
nominalResultType typeName origType =

      let vars = collectForallVars origType
      in (Lists.foldl (\acc -> \v -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = acc,
        Core.applicationTypeArgument = (Core.TypeVariable v)})) (Core.TypeVariable typeName) vars)
