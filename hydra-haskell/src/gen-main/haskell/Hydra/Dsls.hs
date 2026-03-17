-- Note: this is an automatically generated file. Do not edit.

-- | Functions for generating domain-specific DSL modules from type modules

module Hydra.Dsls where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Error as Error
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Generate a binding name for a DSL function from a type name
dslBindingName :: (Core.Name -> Core.Name)
dslBindingName n = (Logic.ifElse (Logic.not (Lists.null (Lists.tail (Strings.splitOn "." (Core.unName n))))) (Core.Name (Strings.intercalate "." (Lists.concat2 [
  "hydra",
  "dsl"] (Lists.concat2 (Lists.tail (Lists.init (Strings.splitOn "." (Core.unName n)))) [
  Formatting.decapitalize (Names.localNameOf n)])))) (Core.Name (Formatting.decapitalize (Names.localNameOf n))))

-- | Generate a qualified DSL element name from a type name and local element name
dslElementName :: (Core.Name -> String -> Core.Name)
dslElementName typeName localName =  
  let parts = (Strings.splitOn "." (Core.unName typeName))
  in  
    let nsParts = (Lists.init parts)
    in  
      let dslNsParts = (Lists.concat2 [
              "hydra",
              "dsl"] (Lists.tail nsParts))
      in (Core.Name (Strings.intercalate "." (Lists.concat2 dslNsParts [
        localName])))

-- | Transform a type module into a DSL module
dslModule :: (Context.Context -> Graph.Graph -> Module.Module -> Either (Context.InContext Error.Error) (Maybe Module.Module))
dslModule cx graph mod = (Eithers.bind (filterTypeBindings cx graph (Module.moduleElements mod)) (\typeBindings -> Logic.ifElse (Lists.null typeBindings) (Right Nothing) (Eithers.bind (Eithers.mapList (\b -> Eithers.bimap (\ic -> Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError (Context.inContextObject ic)))),
  Context.inContextContext = (Context.inContextContext ic)}) (\x -> x) (generateBindingsForType cx graph b)) typeBindings) (\dslBindings -> Right (Just (Module.Module {
  Module.moduleNamespace = (dslNamespace (Module.moduleNamespace mod)),
  Module.moduleElements = (deduplicateBindings (Lists.concat dslBindings)),
  Module.moduleTermDependencies = (Lists.nub (Lists.map dslNamespace (Module.moduleTypeDependencies mod))),
  Module.moduleTypeDependencies = (Lists.nub (Lists.concat2 [
    Module.moduleNamespace mod,
    (Module.Namespace "hydra.phantoms")] (Module.moduleTypeDependencies mod))),
  Module.moduleDescription = (Just (Strings.cat [
    "DSL functions for ",
    (Module.unNamespace (Module.moduleNamespace mod))]))}))))))

-- | Generate a DSL module namespace from a source module namespace
dslNamespace :: (Module.Namespace -> Module.Namespace)
dslNamespace ns = (Module.Namespace (Strings.cat [
  "hydra.dsl.",
  (Strings.intercalate "." (Lists.tail (Strings.splitOn "." (Module.unNamespace ns))))]))

-- | Filter bindings to only DSL-eligible type definitions
filterTypeBindings :: (t0 -> t1 -> [Core.Binding] -> Either t2 [Core.Binding])
filterTypeBindings cx graph bindings = (Eithers.map Maybes.cat (Eithers.mapList (isDslEligibleBinding cx graph) (Lists.filter Annotations.isNativeType bindings)))

-- | Generate all DSL bindings for a type binding
generateBindingsForType :: (Context.Context -> Graph.Graph -> Core.Binding -> Either (Context.InContext Error.DecodingError) [Core.Binding])
generateBindingsForType cx graph b =  
  let typeName = (Core.bindingName b)
  in (Eithers.bind (Eithers.bimap (\_wc_e -> Context.InContext {
    Context.inContextObject = _wc_e,
    Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Core_.type_ graph (Core.bindingTerm b))) (\rawType ->  
    let typ = (Rewriting.deannotateTypeParameters (Rewriting.deannotateType rawType))
    in (Right ((\x -> case x of
      Core.TypeRecord v0 -> (Lists.concat [
        generateRecordConstructor rawType typeName v0,
        (Lists.map (generateRecordAccessor rawType typeName) (Core.rowTypeFields v0)),
        (Lists.map (generateRecordWithUpdater rawType typeName (Core.rowTypeFields v0)) (Core.rowTypeFields v0))])
      Core.TypeUnion v0 -> (Lists.map (generateUnionInjector rawType typeName) (Core.rowTypeFields v0))
      Core.TypeWrap v0 -> (generateWrappedTypeAccessors rawType typeName v0)
      _ -> []) typ))))

-- | Generate a record field accessor function
generateRecordAccessor :: (Core.Type -> Core.Name -> Core.FieldType -> Core.Binding)
generateRecordAccessor origType typeName ft =  
  let fieldName = (Core.fieldTypeName ft)
  in  
    let accessorLocalName = (Strings.cat [
            Formatting.decapitalize (Names.localNameOf typeName),
            (Formatting.capitalize (Names.localNameOf fieldName))])
    in  
      let accessorName = (dslElementName typeName accessorLocalName)
      in  
        let paramDomain = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TTerm")),
                Core.applicationTypeArgument = (nominalResultType typeName origType)}))
        in  
          let body = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = (Just paramDomain),
                  Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.Name "hydra.phantoms.TTerm"),
                    Core.wrappedTermBody = (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "application"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Application"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "function"),
                              Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                Core.injectionField = Core.Field {
                                  Core.fieldName = (Core.Name "function"),
                                  Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Function"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "elimination"),
                                      Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "record"),
                                          Core.fieldTerm = (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.core.Projection"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "typeName"),
                                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName typeName)))}))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "field"),
                                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName fieldName)))}))}]}))}}))}}))}}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "argument"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.phantoms.TTerm")))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}]}))}}))}))})))
          in  
            let ts = (dslTypeScheme origType [
                    nominalResultType typeName origType] (Core.fieldTypeType ft))
            in Core.Binding {
              Core.bindingName = accessorName,
              Core.bindingTerm = body,
              Core.bindingType = (Just ts)}

-- | Generate a record constructor function
generateRecordConstructor :: (Core.Type -> Core.Name -> Core.RowType -> [Core.Binding])
generateRecordConstructor origType typeName rt =  
  let fieldTypes = (Core.rowTypeFields rt)
  in  
    let dFields = (Lists.map (\ft -> Core.TermRecord (Core.Record {
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
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.phantoms.TTerm")))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name (Formatting.decapitalize (Names.localNameOf (Core.fieldTypeName ft)))))}))}]})) fieldTypes)
    in  
      let recordTerm = (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = (Core.Name "hydra.phantoms.TTerm"),
              Core.wrappedTermBody = (Core.TermUnion (Core.Injection {
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
                        Core.fieldTerm = (Core.TermList dFields)}]}))}}))}))
      in  
        let paramPairs = (Lists.map (\ft -> (Formatting.decapitalize (Names.localNameOf (Core.fieldTypeName ft)), (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TTerm")),
                Core.applicationTypeArgument = (Core.fieldTypeType ft)})))) fieldTypes)
        in  
          let body = (Lists.foldl (\acc -> \pp -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name (Pairs.first pp)),
                  Core.lambdaDomain = (Just (Pairs.second pp)),
                  Core.lambdaBody = acc}))) recordTerm (Lists.reverse paramPairs))
          in  
            let paramTypes = (Lists.map (\ft -> Core.fieldTypeType ft) fieldTypes)
            in  
              let resultType = (nominalResultType typeName origType)
              in  
                let ts = (dslTypeScheme origType paramTypes resultType)
                in [
                  Core.Binding {
                    Core.bindingName = (dslBindingName typeName),
                    Core.bindingTerm = body,
                    Core.bindingType = (Just ts)}]

-- | Generate a withXxx record field updater function
generateRecordWithUpdater :: (Core.Type -> Core.Name -> [Core.FieldType] -> Core.FieldType -> Core.Binding)
generateRecordWithUpdater origType typeName allFields targetField =  
  let targetFieldName = (Core.fieldTypeName targetField)
  in  
    let updaterLocalName = (Strings.cat [
            Formatting.decapitalize (Names.localNameOf typeName),
            "With",
            (Formatting.capitalize (Names.localNameOf targetFieldName))])
    in  
      let updaterName = (dslElementName typeName updaterLocalName)
      in  
        let dFields = (Lists.map (\ft -> Core.TermRecord (Core.Record {
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
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.phantoms.TTerm")))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "newVal"))})) (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "application"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Application"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "function"),
                              Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                Core.injectionField = Core.Field {
                                  Core.fieldName = (Core.Name "function"),
                                  Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Function"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "elimination"),
                                      Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "record"),
                                          Core.fieldTerm = (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.core.Projection"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "typeName"),
                                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName typeName)))}))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "field"),
                                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldTypeName ft))))}))}]}))}}))}}))}}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "argument"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.phantoms.TTerm")))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "original"))}))}]}))}})))}]})) allFields)
        in  
          let recDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TTerm")),
                  Core.applicationTypeArgument = (nominalResultType typeName origType)}))
          in  
            let fieldDomain = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TTerm")),
                    Core.applicationTypeArgument = (Core.fieldTypeType targetField)}))
            in  
              let body = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "original"),
                      Core.lambdaDomain = (Just recDomain),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "newVal"),
                        Core.lambdaDomain = (Just fieldDomain),
                        Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.phantoms.TTerm"),
                          Core.wrappedTermBody = (Core.TermUnion (Core.Injection {
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
                                    Core.fieldTerm = (Core.TermList dFields)}]}))}}))}))})))})))
              in  
                let recType = (nominalResultType typeName origType)
                in  
                  let ts = (dslTypeScheme origType [
                          recType,
                          (Core.fieldTypeType targetField)] recType)
                  in Core.Binding {
                    Core.bindingName = updaterName,
                    Core.bindingTerm = body,
                    Core.bindingType = (Just ts)}

-- | Generate a union injection helper
generateUnionInjector :: (Core.Type -> Core.Name -> Core.FieldType -> Core.Binding)
generateUnionInjector origType typeName ft =  
  let fieldName = (Core.fieldTypeName ft)
  in  
    let fieldType = (Core.fieldTypeType ft)
    in  
      let injectorLocalName = (Strings.cat [
              Formatting.decapitalize (Names.localNameOf typeName),
              (Formatting.capitalize (Names.localNameOf fieldName))])
      in  
        let injectorName = (dslElementName typeName injectorLocalName)
        in  
          let isUnit = ((\t -> (\x -> case x of
                  Core.TypeUnit -> True
                  _ -> False) (Rewriting.deannotateType t)) fieldType)
          in  
            let dFieldValue = (Logic.ifElse isUnit (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "unit"),
                      Core.fieldTerm = Core.TermUnit}})) (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.phantoms.TTerm")))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})))
            in  
              let injectionTerm = (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.phantoms.TTerm"),
                      Core.wrappedTermBody = (Core.TermUnion (Core.Injection {
                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "union"),
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
                                      Core.fieldTerm = dFieldValue}]}))}]}))}}))}))
              in  
                let variantDomain = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TTerm")),
                        Core.applicationTypeArgument = (Core.fieldTypeType ft)}))
                in  
                  let body = (Logic.ifElse isUnit injectionTerm (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = (Just variantDomain),
                          Core.lambdaBody = injectionTerm}))))
                  in  
                    let unionType = (nominalResultType typeName origType)
                    in  
                      let ts = (Logic.ifElse isUnit (dslTypeScheme origType [] unionType) (dslTypeScheme origType [
                              Core.fieldTypeType ft] unionType))
                      in Core.Binding {
                        Core.bindingName = injectorName,
                        Core.bindingTerm = body,
                        Core.bindingType = (Just ts)}

-- | Generate wrap/unwrap accessors for a wrapped type
generateWrappedTypeAccessors :: (Core.Type -> Core.Name -> Core.WrappedType -> [Core.Binding])
generateWrappedTypeAccessors origType typeName wt =  
  let localName = (Names.localNameOf typeName)
  in  
    let wrapName = (dslElementName typeName (Formatting.decapitalize localName))
    in  
      let unwrapLocalName = (Strings.cat [
              "un",
              localName])
      in  
        let unwrapName = (dslElementName typeName unwrapLocalName)
        in  
          let innerType = (Core.wrappedTypeBody wt)
          in  
            let wrapperType = (nominalResultType typeName origType)
            in  
              let wrapDomain = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TTerm")),
                      Core.applicationTypeArgument = innerType}))
              in  
                let wrapBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = (Just wrapDomain),
                        Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.phantoms.TTerm"),
                          Core.wrappedTermBody = (Core.TermUnion (Core.Injection {
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
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.phantoms.TTerm")))),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}]}))}}))}))})))
                in  
                  let unwrapDomain = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TTerm")),
                          Core.applicationTypeArgument = wrapperType}))
                  in  
                    let unwrapBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = (Just unwrapDomain),
                            Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.phantoms.TTerm"),
                              Core.wrappedTermBody = (Core.TermUnion (Core.Injection {
                                Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                Core.injectionField = Core.Field {
                                  Core.fieldName = (Core.Name "application"),
                                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                                    Core.recordTypeName = (Core.Name "hydra.core.Application"),
                                    Core.recordFields = [
                                      Core.Field {
                                        Core.fieldName = (Core.Name "function"),
                                        Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "function"),
                                            Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                              Core.injectionTypeName = (Core.Name "hydra.core.Function"),
                                              Core.injectionField = Core.Field {
                                                Core.fieldName = (Core.Name "elimination"),
                                                Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                                  Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
                                                  Core.injectionField = Core.Field {
                                                    Core.fieldName = (Core.Name "wrap"),
                                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName typeName)))}))}}))}}))}}))},
                                      Core.Field {
                                        Core.fieldName = (Core.Name "argument"),
                                        Core.fieldTerm = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.phantoms.TTerm")))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}]}))}}))}))})))
                    in  
                      let wrapTs = (dslTypeScheme origType [
                              innerType] wrapperType)
                      in  
                        let unwrapTs = (dslTypeScheme origType [
                                wrapperType] innerType)
                        in [
                          Core.Binding {
                            Core.bindingName = wrapName,
                            Core.bindingTerm = wrapBody,
                            Core.bindingType = (Just wrapTs)},
                          Core.Binding {
                            Core.bindingName = unwrapName,
                            Core.bindingTerm = unwrapBody,
                            Core.bindingType = (Just unwrapTs)}]

-- | Deduplicate bindings by appending underscore to duplicate names
deduplicateBindings :: ([Core.Binding] -> [Core.Binding])
deduplicateBindings bindings = (Lists.foldl (\acc -> \b ->  
  let n = (Core.unName (Core.bindingName b))
  in  
    let alreadySeen = (Logic.not (Lists.null (Lists.filter (\a -> Equality.equal (Core.unName (Core.bindingName a)) n) acc)))
    in (Logic.ifElse alreadySeen (Lists.concat2 acc [
      Core.Binding {
        Core.bindingName = (Core.Name (Strings.cat [
          n,
          "_"])),
        Core.bindingTerm = (Core.bindingTerm b),
        Core.bindingType = (Core.bindingType b)}]) (Lists.concat2 acc [
      b]))) [] bindings)

-- | Check if a binding is eligible for DSL generation
isDslEligibleBinding :: (t0 -> t1 -> Core.Binding -> Either t2 (Maybe Core.Binding))
isDslEligibleBinding cx graph b =  
  let ns = (Names.namespaceOf (Core.bindingName b))
  in (Logic.ifElse (Equality.equal (Maybes.maybe "" Module.unNamespace ns) "hydra.phantoms") (Right Nothing) (Right (Just b)))

-- | Build a TypeScheme with TTerm-wrapped parameter and result types
dslTypeScheme :: (Core.Type -> [Core.Type] -> Core.Type -> Core.TypeScheme)
dslTypeScheme origType paramTypes resultType =  
  let typeVars = (collectForallVars origType)
  in  
    let wrappedResult = (Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TTerm")),
            Core.applicationTypeArgument = resultType}))
    in  
      let funType = (Lists.foldr (\paramType -> \acc -> Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TTerm")),
                Core.applicationTypeArgument = paramType})),
              Core.functionTypeCodomain = acc})) wrappedResult paramTypes)
      in Core.TypeScheme {
        Core.typeSchemeVariables = typeVars,
        Core.typeSchemeType = funType,
        Core.typeSchemeConstraints = Nothing}

-- | Collect forall type variable names from a type
collectForallVars :: (Core.Type -> [Core.Name])
collectForallVars typ = ((\x -> case x of
  Core.TypeAnnotated v0 -> (collectForallVars (Core.annotatedTypeBody v0))
  Core.TypeForall v0 -> (Lists.cons (Core.forallTypeParameter v0) (collectForallVars (Core.forallTypeBody v0)))
  _ -> []) typ)

-- | Build the nominal result type with type applications for forall variables
nominalResultType :: (Core.Name -> Core.Type -> Core.Type)
nominalResultType typeName origType =  
  let vars = (collectForallVars origType)
  in (Lists.foldl (\acc -> \v -> Core.TypeApplication (Core.ApplicationType {
    Core.applicationTypeFunction = acc,
    Core.applicationTypeArgument = (Core.TypeVariable v)})) (Core.TypeVariable typeName) vars)
