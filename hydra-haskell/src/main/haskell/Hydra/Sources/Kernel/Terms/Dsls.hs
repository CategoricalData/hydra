{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Kernel.Terms.Dsls where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Meta.Module       as Module
import qualified Hydra.Dsl.Meta.Phantoms     as Phantoms
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (
  elimination, field, fieldType, floatType, floatValue, function, injection, integerType, integerValue, lambda, literal,
  literalType, record, term, type_, typeScheme, wrap)
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Meta.Error        as Error
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Dsl.Meta.DeepCore as DC
import           Hydra.Dsl.Meta.DeepCore ((@@@))
import           Prelude hiding ((++))
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S


ns :: Namespace
ns = Namespace "hydra.dsls"

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, Formatting.ns, Names.ns, Rewriting.ns, Schemas.ns]
    kernelTypesNamespaces $
    Just "Functions for generating domain-specific DSL modules from type modules"
  where
    elements = [
      toBinding dslBindingName,
      toBinding dslElementName,
      toBinding dslModule,
      toBinding dslNamespace,
      toBinding filterTypeBindings,
      toBinding generateBindingsForType,
      toBinding generateRecordAccessor,
      toBinding generateRecordConstructor,
      toBinding generateRecordWithUpdater,
      toBinding generateUnionInjector,
      toBinding generateWrappedTypeAccessors,
      toBinding deduplicateBindings,
      toBinding isDslEligibleBinding]

define :: String -> TTerm x -> TBinding x
define = definitionInModule module_



-- | Generate a DSL module namespace from a source module namespace
-- For example, "hydra.core" -> "hydra.dsl.core"
dslNamespace :: TBinding (Namespace -> Namespace)
dslNamespace = define "dslNamespace" $
  doc "Generate a DSL module namespace from a source module namespace" $
  "ns" ~> (
    Module.namespace (
      Strings.cat $ list [
        string "hydra.dsl.",
        Strings.intercalate (string ".")
          (Lists.tail (Strings.splitOn (string ".") (Module.unNamespace (var "ns"))))]))

-- | Generate a fully qualified binding name for a DSL function from a type name
-- For example, "hydra.core.AnnotatedTerm" -> "hydra.dsl.core.annotatedTerm"
-- For local types (no namespace), returns just the decapitalized local name
dslBindingName :: TBinding (Name -> Name)
dslBindingName = define "dslBindingName" $
  doc "Generate a binding name for a DSL function from a type name" $
  "n" ~>
    -- Check if name has a namespace (contains ".")
    Logic.ifElse (Logic.not (Lists.null
      (Lists.tail (Strings.splitOn (string ".") (Core.unName (var "n"))))))
      -- Qualified type: e.g., "hydra.core.AnnotatedTerm" -> "hydra.dsl.core.annotatedTerm"
      (Core.name (
        Strings.intercalate (string ".") (
          Lists.concat2
            (list [string "hydra", string "dsl"])
            (Lists.concat2
              (Lists.tail (Lists.init (Strings.splitOn (string ".") (Core.unName (var "n")))))
              (list [Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))])))))
      -- Local type: just decapitalize
      (Core.name (Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))))

-- | Generate a DSL element name from a type name and a local element name.
-- For example, ("hydra.core.AnnotatedTerm", "annotatedTermBody") -> "hydra.dsl.core.annotatedTermBody"
-- This extracts the namespace from the type name, transforms it to the DSL namespace,
-- and appends the local element name.
dslElementName :: TBinding (Name -> String -> Name)
dslElementName = define "dslElementName" $
  doc "Generate a qualified DSL element name from a type name and local element name" $
  "typeName" ~> "localName" ~>
  "parts" <~ (Strings.splitOn (string ".") (Core.unName (var "typeName"))) $
  -- Extract namespace parts (all but last), transform to DSL namespace
  "nsParts" <~ (Lists.init (var "parts")) $
  "dslNsParts" <~ (Lists.concat2 (list [string "hydra", string "dsl"]) (Lists.tail (var "nsParts"))) $
  Core.name (Strings.intercalate (string ".") (Lists.concat2 (var "dslNsParts") (list [var "localName"])))

-- | Generate a record constructor function.
-- For a record type like {name: Name, term: Term, type: Maybe TypeScheme},
-- produces a function: Name -> Term -> Maybe TypeScheme -> Binding
-- The generated function builds a record using field projections.
generateRecordConstructor :: TBinding (Name -> RowType -> [Binding])
generateRecordConstructor = define "generateRecordConstructor" $
  doc "Generate a record constructor function" $
  "typeName" ~> "rt" ~>
  "fieldTypes" <~ (Core.rowTypeFields $ var "rt") $
  -- Build a lambda that takes one parameter per field and constructs the record
  -- Parameters are named after the field local names
  "paramNames" <~ (Lists.map
    ("ft" ~> Names.localNameOf @@ Core.fieldTypeName (var "ft"))
    (var "fieldTypes")) $
  -- Build field list: [Field name (variable paramName) for each field]
  "fieldTerms" <~ (Lists.map
    ("ft" ~> Core.field
      (Core.fieldTypeName (var "ft"))
      (Core.termVariable (Core.name (Names.localNameOf @@ Core.fieldTypeName (var "ft")))))
    (var "fieldTypes")) $
  -- Build the record term
  "recordTerm" <~ (Core.termRecord $ Core.record (var "typeName") (var "fieldTerms")) $
  -- Wrap in lambdas for each parameter (right to left)
  "body" <~ (Lists.foldl
    ("acc" ~> "pname" ~>
      Core.termFunction $ Core.functionLambda $
        Core.lambda (Core.name (var "pname")) nothing (var "acc"))
    (var "recordTerm")
    (Lists.reverse (var "paramNames"))) $
  -- Return as a single-element list with the constructor binding
  list [Core.binding
    (dslBindingName @@ var "typeName")
    (var "body")
    nothing]

-- | Generate a record field accessor function.
-- For a field "name" in record type "Binding", produces:
--   bindingName :: Binding -> Name
--   bindingName x = x.name
generateRecordAccessor :: TBinding (Name -> FieldType -> Binding)
generateRecordAccessor = define "generateRecordAccessor" $
  doc "Generate a record field accessor function" $
  "typeName" ~> "ft" ~>
  "fieldName" <~ (Core.fieldTypeName (var "ft")) $
  "accessorLocalName" <~ (Strings.cat $ list [
    Formatting.decapitalize @@ (Names.localNameOf @@ var "typeName"),
    Formatting.capitalize @@ (Names.localNameOf @@ var "fieldName")]) $
  "accessorName" <~ (dslElementName @@ var "typeName" @@ var "accessorLocalName") $
  Core.binding
    (var "accessorName")
    (Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $
      Core.projection (var "typeName") (var "fieldName"))
    nothing

-- | Generate a "withXxx" record field updater function.
-- For a field "name" in record type "Binding" (with fields name, term, type), produces:
--   bindingWithName :: Binding -> Name -> Binding
--   bindingWithName b newName = Binding newName (bindingTerm b) (bindingType b)
-- This constructs a new record with the specified field replaced and all others projected.
generateRecordWithUpdater :: TBinding (Name -> [FieldType] -> FieldType -> Binding)
generateRecordWithUpdater = define "generateRecordWithUpdater" $
  doc "Generate a withXxx record field updater function" $
  "typeName" ~> "allFields" ~> "targetField" ~>
  "targetFieldName" <~ (Core.fieldTypeName (var "targetField")) $
  -- Build the updater name: e.g., "bindingWithName"
  "updaterLocalName" <~ (Strings.cat $ list [
    Formatting.decapitalize @@ (Names.localNameOf @@ var "typeName"),
    string "With",
    Formatting.capitalize @@ (Names.localNameOf @@ var "targetFieldName")]) $
  "updaterName" <~ (dslElementName @@ var "typeName" @@ var "updaterLocalName") $
  -- Build fields for the new record: project all fields except the target, use "newVal" for the target
  "newFields" <~ (Lists.map
    ("ft" ~> Core.field
      (Core.fieldTypeName (var "ft"))
      (Logic.ifElse (Equality.equal
        (Core.unName (Core.fieldTypeName (var "ft")))
        (Core.unName (var "targetFieldName")))
        (Core.termVariable (Core.name (string "newVal")))
        -- Project this field from the original record
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $
            Core.projection (var "typeName") (Core.fieldTypeName (var "ft")))
          (Core.termVariable (Core.name (string "original"))))))
    (var "allFields")) $
  -- Build the body: \original -> \newVal -> TypeName { field1 = ..., field2 = ..., ... }
  "body" <~ (
    Core.termFunction $ Core.functionLambda $ Core.lambda (Core.name (string "original")) nothing $
    Core.termFunction $ Core.functionLambda $ Core.lambda (Core.name (string "newVal")) nothing $
    Core.termRecord $ Core.record (var "typeName") (var "newFields")) $
  Core.binding
    (var "updaterName")
    (var "body")
    nothing

-- | Generate a union injection helper.
-- For a variant "lambda" in union type "Function", produces:
--   functionLambda :: Lambda -> Function
--   functionLambda x = Function.lambda x
-- For unit variants (field type is unit), produces:
--   comparisonLessThan :: Comparison
--   comparisonLessThan = Comparison.lessThan ()
generateUnionInjector :: TBinding (Name -> FieldType -> Binding)
generateUnionInjector = define "generateUnionInjector" $
  doc "Generate a union injection helper" $
  "typeName" ~> "ft" ~>
  "fieldName" <~ (Core.fieldTypeName (var "ft")) $
  "fieldType" <~ (Core.fieldTypeType (var "ft")) $
  -- Build the injector name: e.g., "functionLambda" or "comparisonLessThan"
  "injectorLocalName" <~ (Strings.cat $ list [
    Formatting.decapitalize @@ (Names.localNameOf @@ var "typeName"),
    Formatting.capitalize @@ (Names.localNameOf @@ var "fieldName")]) $
  "injectorName" <~ (dslElementName @@ var "typeName" @@ var "injectorLocalName") $
  -- Check if field type is unit (for unit variants like enum members)
  "isUnit" <~ (isUnitType_ @@ var "fieldType") $
  -- Build the injection term
  "injectionField" <~ (Core.field (var "fieldName") (
    Logic.ifElse (var "isUnit")
      Core.termUnit
      (Core.termVariable (Core.name (string "x"))))) $
  "injectionTerm" <~ (Core.termUnion $ Core.injection (var "typeName") (var "injectionField")) $
  -- For non-unit variants, wrap in a lambda
  "body" <~ (Logic.ifElse (var "isUnit")
    (var "injectionTerm")
    (Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "x")) nothing (var "injectionTerm"))) $
  Core.binding
    (var "injectorName")
    (var "body")
    nothing

-- | Check if a type is the unit type (stripping annotations first)
-- Note: uses stripAnnotations to avoid recursive Haskell-level DSL terms,
-- which would cause infinite recursion during code generation.
isUnitType_ :: TTerm (Type -> Bool)
isUnitType_ = "t" ~> cases _Type (Rewriting.deannotateType @@ var "t") (Just Phantoms.false) [
  _Type_unit>>: constant Phantoms.true]

-- | Generate wrap/unwrap accessors for a wrapped type.
-- For a wrapped type like "Name" wrapping String, produces:
--   name :: String -> Name      (constructor/wrap)
--   unName :: Name -> String    (unwrap)
generateWrappedTypeAccessors :: TBinding (Name -> WrappedType -> [Binding])
generateWrappedTypeAccessors = define "generateWrappedTypeAccessors" $
  doc "Generate wrap/unwrap accessors for a wrapped type" $
  "typeName" ~> "wt" ~>
  "localName" <~ (Names.localNameOf @@ var "typeName") $
  -- Wrap function: decapitalized type name
  "wrapName" <~ (dslElementName @@ var "typeName" @@ (Formatting.decapitalize @@ var "localName")) $
  -- Unwrap function: "un" + type local name
  "unwrapLocalName" <~ (Strings.cat $ list [string "un", var "localName"]) $
  "unwrapName" <~ (dslElementName @@ var "typeName" @@ var "unwrapLocalName") $
  -- Wrap: \x -> TypeName x (i.e., TermWrap)
  "wrapBody" <~ (
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "x")) nothing $
        Core.termWrap $ Core.wrappedTerm (var "typeName") (Core.termVariable (Core.name (string "x")))) $
  -- Unwrap: elimination (wrap TypeName)
  "unwrapBody" <~ (
    Core.termFunction $ Core.functionElimination $ Core.eliminationWrap (var "typeName")) $
  list [
    Core.binding (var "wrapName") (var "wrapBody") nothing,
    Core.binding (var "unwrapName") (var "unwrapBody") nothing]

-- | Generate all DSL bindings for a single type binding.
-- Inspects the type definition and generates appropriate helpers:
-- - Records: constructor + accessors + withXxx updaters
-- - Unions: injection helpers
-- - Wrapped types: wrap + unwrap
generateBindingsForType :: TBinding (Context -> Graph -> Binding -> Either (InContext DecodingError) [Binding])
generateBindingsForType = define "generateBindingsForType" $
  doc "Generate all DSL bindings for a type binding" $
  "cx" ~> "graph" ~> "b" ~>
  "typeName" <~ (Core.bindingName (var "b")) $
  Eithers.bind (Ctx.withContext (var "cx") (decoderFor _Type @@ var "graph" @@ (Core.bindingTerm (var "b")))) (
    "rawType" ~>
    "typ" <~ (Rewriting.deannotateTypeParameters @@ (Rewriting.deannotateType @@ var "rawType")) $
    right (cases _Type (var "typ") (Just $ list ([] :: [TTerm Binding])) [
      _Type_record>>: "rt" ~>
        Lists.concat $ list [
          generateRecordConstructor @@ var "typeName" @@ var "rt",
          Lists.map (generateRecordAccessor @@ var "typeName") (Core.rowTypeFields (var "rt")),
          Lists.map (generateRecordWithUpdater @@ var "typeName" @@ Core.rowTypeFields (var "rt"))
            (Core.rowTypeFields (var "rt"))],
      _Type_union>>: "rt" ~>
        Lists.map (generateUnionInjector @@ var "typeName") (Core.rowTypeFields (var "rt")),
      _Type_wrap>>: "wt" ~>
        generateWrappedTypeAccessors @@ var "typeName" @@ var "wt"]))

-- | Deduplicate bindings by appending "_" to duplicate names.
-- Later bindings (e.g., record accessors) get the suffix; earlier ones (e.g., wrapped type constructors) keep their name.
deduplicateBindings :: TBinding ([Binding] -> [Binding])
deduplicateBindings = define "deduplicateBindings" $
  doc "Deduplicate bindings by appending underscore to duplicate names" $
  "bindings" ~>
  Lists.foldl
    ("acc" ~> "b" ~>
      "n" <~ (Core.unName (Core.bindingName (var "b"))) $
      "alreadySeen" <~ (Logic.not (Lists.null (Lists.filter
        ("a" ~> Equality.equal (Core.unName (Core.bindingName (var "a"))) (var "n"))
        (var "acc")))) $
      Logic.ifElse (var "alreadySeen")
        -- Duplicate: rename with "_" suffix
        (Lists.concat2 (var "acc") (list [
          Core.binding
            (Core.name (Strings.cat $ list [var "n", string "_"]))
            (Core.bindingTerm (var "b"))
            (Core.bindingType (var "b"))]))
        -- First occurrence: keep as-is
        (Lists.concat2 (var "acc") (list [var "b"])))
    (list ([] :: [TTerm Binding]))
    (var "bindings")

-- | Filter bindings to only DSL-eligible type definitions
filterTypeBindings :: TBinding (Context -> Graph -> [Binding] -> Either (InContext Error) [Binding])
filterTypeBindings = define "filterTypeBindings" $
  doc "Filter bindings to only DSL-eligible type definitions" $
  "cx" ~> "graph" ~> "bindings" ~>
    Eithers.map (primitive _maybes_cat) $
      Eithers.mapList (isDslEligibleBinding @@ var "cx" @@ var "graph") $
        primitive _lists_filter @@ Annotations.isNativeType @@ var "bindings"

-- | Check if a binding is eligible for DSL generation
isDslEligibleBinding :: TBinding (Context -> Graph -> Binding -> Either (InContext Error) (Maybe Binding))
isDslEligibleBinding = define "isDslEligibleBinding" $
  doc "Check if a binding is eligible for DSL generation" $
  "cx" ~> "graph" ~> "b" ~>
    right (just (var "b"))

-- | Transform a type module into a DSL module.
-- Returns Nothing if the module has no eligible type definitions.
dslModule :: TBinding (Context -> Graph -> Module -> Either (InContext Error) (Maybe Module))
dslModule = define "dslModule" $
  doc "Transform a type module into a DSL module" $
  "cx" ~> "graph" ~> "mod" ~>
    "typeBindings" <<~ (filterTypeBindings @@ var "cx" @@ var "graph" @@ (Module.moduleElements (var "mod"))) $
    Logic.ifElse (Lists.null (var "typeBindings"))
      (right nothing)
      ("dslBindings" <<~ Eithers.mapList ("b" ~>
        Eithers.bimap
          ("ic" ~> Ctx.inContext (Error.errorOther $ Error.otherError (Error.unDecodingError @@ Ctx.inContextObject (var "ic"))) (Ctx.inContextContext (var "ic")))
          ("x" ~> var "x")
          (generateBindingsForType @@ var "cx" @@ var "graph" @@ var "b")) (var "typeBindings") $
        right (just (Module.module_
          (dslNamespace @@ (Module.moduleNamespace (var "mod")))
          (deduplicateBindings @@ Lists.concat (var "dslBindings"))
          -- DSL modules depend on DSL modules for type dependencies (to reference other types' DSL functions)
          (Lists.nub (primitive _lists_map @@ dslNamespace @@ (Module.moduleTypeDependencies (var "mod"))))
          -- Type dependencies: the original module's types
          (list [Module.moduleNamespace (var "mod")])
          (just (Strings.cat $ list [
            string "DSL functions for ",
            Module.unNamespace (Module.moduleNamespace (var "mod"))])))))
