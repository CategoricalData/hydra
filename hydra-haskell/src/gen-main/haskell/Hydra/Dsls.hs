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
  Module.moduleTypeDependencies = [
    Module.moduleNamespace mod],
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
        generateRecordConstructor typeName v0,
        (Lists.map (generateRecordAccessor typeName) (Core.rowTypeFields v0)),
        (Lists.map (generateRecordWithUpdater typeName (Core.rowTypeFields v0)) (Core.rowTypeFields v0))])
      Core.TypeUnion v0 -> (Lists.map (generateUnionInjector typeName) (Core.rowTypeFields v0))
      Core.TypeWrap v0 -> (generateWrappedTypeAccessors typeName v0)
      _ -> []) typ))))

-- | Generate a record field accessor function
generateRecordAccessor :: (Core.Name -> Core.FieldType -> Core.Binding)
generateRecordAccessor typeName ft =  
  let fieldName = (Core.fieldTypeName ft)
  in  
    let accessorLocalName = (Strings.cat [
            Formatting.decapitalize (Names.localNameOf typeName),
            (Formatting.capitalize (Names.localNameOf fieldName))])
    in  
      let accessorName = (dslElementName typeName accessorLocalName)
      in Core.Binding {
        Core.bindingName = accessorName,
        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = typeName,
          Core.projectionField = fieldName})))),
        Core.bindingType = Nothing}

-- | Generate a record constructor function
generateRecordConstructor :: (Core.Name -> Core.RowType -> [Core.Binding])
generateRecordConstructor typeName rt =  
  let fieldTypes = (Core.rowTypeFields rt)
  in  
    let paramNames = (Lists.map (\ft -> Names.localNameOf (Core.fieldTypeName ft)) fieldTypes)
    in  
      let fieldTerms = (Lists.map (\ft -> Core.Field {
              Core.fieldName = (Core.fieldTypeName ft),
              Core.fieldTerm = (Core.TermVariable (Core.Name (Names.localNameOf (Core.fieldTypeName ft))))}) fieldTypes)
      in  
        let recordTerm = (Core.TermRecord (Core.Record {
                Core.recordTypeName = typeName,
                Core.recordFields = fieldTerms}))
        in  
          let body = (Lists.foldl (\acc -> \pname -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name pname),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = acc}))) recordTerm (Lists.reverse paramNames))
          in [
            Core.Binding {
              Core.bindingName = (dslBindingName typeName),
              Core.bindingTerm = body,
              Core.bindingType = Nothing}]

-- | Generate a withXxx record field updater function
generateRecordWithUpdater :: (Core.Name -> [Core.FieldType] -> Core.FieldType -> Core.Binding)
generateRecordWithUpdater typeName allFields targetField =  
  let targetFieldName = (Core.fieldTypeName targetField)
  in  
    let updaterLocalName = (Strings.cat [
            Formatting.decapitalize (Names.localNameOf typeName),
            "With",
            (Formatting.capitalize (Names.localNameOf targetFieldName))])
    in  
      let updaterName = (dslElementName typeName updaterLocalName)
      in  
        let newFields = (Lists.map (\ft -> Core.Field {
                Core.fieldName = (Core.fieldTypeName ft),
                Core.fieldTerm = (Logic.ifElse (Equality.equal (Core.unName (Core.fieldTypeName ft)) (Core.unName targetFieldName)) (Core.TermVariable (Core.Name "newVal")) (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                    Core.projectionTypeName = typeName,
                    Core.projectionField = (Core.fieldTypeName ft)})))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "original"))})))}) allFields)
        in  
          let body = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "original"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "newVal"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = typeName,
                      Core.recordFields = newFields}))})))})))
          in Core.Binding {
            Core.bindingName = updaterName,
            Core.bindingTerm = body,
            Core.bindingType = Nothing}

-- | Generate a union injection helper
generateUnionInjector :: (Core.Name -> Core.FieldType -> Core.Binding)
generateUnionInjector typeName ft =  
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
            let injectionField = Core.Field {
                    Core.fieldName = fieldName,
                    Core.fieldTerm = (Logic.ifElse isUnit Core.TermUnit (Core.TermVariable (Core.Name "x")))}
            in  
              let injectionTerm = (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = typeName,
                      Core.injectionField = injectionField}))
              in  
                let body = (Logic.ifElse isUnit injectionTerm (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = injectionTerm}))))
                in Core.Binding {
                  Core.bindingName = injectorName,
                  Core.bindingTerm = body,
                  Core.bindingType = Nothing}

-- | Generate wrap/unwrap accessors for a wrapped type
generateWrappedTypeAccessors :: (Core.Name -> t0 -> [Core.Binding])
generateWrappedTypeAccessors typeName wt =  
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
          let wrapBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = typeName,
                    Core.wrappedTermBody = (Core.TermVariable (Core.Name "x"))}))})))
          in  
            let unwrapBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap typeName)))
            in [
              Core.Binding {
                Core.bindingName = wrapName,
                Core.bindingTerm = wrapBody,
                Core.bindingType = Nothing},
              Core.Binding {
                Core.bindingName = unwrapName,
                Core.bindingTerm = unwrapBody,
                Core.bindingType = Nothing}]

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
isDslEligibleBinding :: (t0 -> t1 -> t2 -> Either t3 (Maybe t2))
isDslEligibleBinding cx graph b = (Right (Just b))
