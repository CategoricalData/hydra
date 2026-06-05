{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Kernel.Terms.Dsls where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
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
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import qualified Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Meta.Phantoms     as Phantoms
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (
  elimination, field, fieldType, floatType, floatValue, function, injection, integerType, integerValue, lambda, literal,
  literalType, record, term, type_, typeScheme, wrap)
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Scoping as Scoping
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Strip as Strip
import qualified Hydra.Dsl.Meta.DeepCore as DeepCore
import           Hydra.Dsl.Meta.DeepCore ((@@@))
import           Prelude hiding ((++))
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S


ns :: ModuleName
ns = ModuleName "hydra.dsls"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Annotations.ns, Formatting.ns, Lexical.ns, Names.ns, Strip.ns, ModuleName "hydra.constants", ModuleName "hydra.decode.core", ModuleName "hydra.encode.core"] L.++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Functions for generating domain-specific DSL modules from type modules")}
  where
    definitions = [
      toDefinition collectForallVars,
      toDefinition deduplicateBindings,
      toDefinition dslBindingName,
      toDefinition dslDefinitionName,
      toDefinition dslModule,
      toDefinition dslModuleName,
      toDefinition dslTypeScheme,
      toDefinition filterTypeBindings,
      toDefinition generateBindingsForType,
      toDefinition generateRecordAccessor,
      toDefinition generateRecordConstructor,
      toDefinition generateRecordWithUpdater,
      toDefinition generateUnionInjector,
      toDefinition generateWrappedTypeAccessors,
      toDefinition isDslEligibleBinding,
      toDefinition nominalResultType]

define :: String -> TypedTerm x -> TypedTermDefinition x
define = definitionInModule module_

-- | Collect forall type variables from a type (stripping annotations)
collectForallVars :: TypedTermDefinition (Type -> [Name])
collectForallVars = define "collectForallVars" $
  doc "Collect forall type variable names from a type" $
  "typ" ~> cases _Type (var "typ") (Just $ list ([] :: [TypedTerm Name])) [
    _Type_annotated>>: "at" ~>
      collectForallVars @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~>
      Lists.cons (Core.forallTypeParameter (var "ft"))
        (collectForallVars @@ Core.forallTypeBody (var "ft"))]

-- | Build the nominal result type for a type definition.
-- For non-polymorphic types: TypeVariable typeName
-- For polymorphic types like (forall n. ModuleNames n): TypeApplication (TypeVariable typeName) (TypeVariable n)
-- | Deduplicate bindings by giving duplicate names a numeric suffix.
-- Later bindings get suffixes; earlier ones keep their name.
-- Multiple duplicates get increasing numeric suffixes: name, name2, name3, etc.
deduplicateBindings :: TypedTermDefinition ([Binding] -> [Binding])
deduplicateBindings = define "deduplicateBindings" $
  doc "Deduplicate bindings by appending numeric suffixes to duplicate names" $
  "bindings" ~>
  Lists.foldl
    ("acc" ~> "b" ~>
      "usedNames" <~ Sets.fromList (Lists.map ("a" ~> Core.bindingName (var "a")) (var "acc")) $
      "uniqueName" <~ (Lexical.chooseUniqueName @@ var "usedNames" @@ Core.bindingName (var "b")) $
      Lists.concat2 (var "acc") (list [
        Core.binding
          (var "uniqueName")
          (Core.bindingTerm (var "b"))
          (Core.bindingTypeScheme (var "b"))]))
    (list ([] :: [TypedTerm Binding]))
    (var "bindings")

-- | Build a deep Application as a Term value
deepApplication :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
deepApplication fun arg =
  injectTermApplication $
    Core.termRecord $ Core.record (Core.nameLift _Application) (list [
      Core.field (Core.nameLift _Application_function) fun,
      Core.field (Core.nameLift _Application_argument) arg])

-- | Build a deep Field record
deepField :: TypedTerm Name -> TypedTerm Term -> TypedTerm Term
deepField name term =
  Core.termRecord $ Core.record (Core.nameLift _Field) (list [
    Core.field (Core.nameLift _Field_name) (deepName (Core.unName name)),
    Core.field (Core.nameLift _Field_term) term])

-- | Build a deep Injection as a Term value (injected into Term.union)
deepInjection :: TypedTerm Name -> TypedTerm Term -> TypedTerm Term
deepInjection typeName fieldTerm =
  injectTermUnion $
    Core.termRecord $ Core.record (Core.nameLift _Injection) (list [
      Core.field (Core.nameLift _Injection_typeName) (deepName (Core.unName typeName)),
      Core.field (Core.nameLift _Injection_field) fieldTerm])

-- | Build a deep Name: TermWrap(_Name, TermLiteral(LiteralString(s)))
deepName :: TypedTerm String -> TypedTerm Term
deepName s = Core.termWrap $ Core.wrappedTerm (Core.nameLift _Name) (Core.termLiteral $ Core.literalString s)

-- | Build a deep Projection as a Term value
deepProjection :: TypedTerm Name -> TypedTerm Name -> TypedTerm Term
deepProjection typeName fieldName =
  injectTermProject $
    Core.termRecord $ Core.record (Core.nameLift _Projection) (list [
      Core.field (Core.nameLift _Projection_typeName) (deepName (Core.unName typeName)),
      Core.field (Core.nameLift _Projection_fieldName) (deepName (Core.unName fieldName))])

-- | Build a deep Record as a Term value (injected into Term.record)
deepRecord :: TypedTerm Name -> TypedTerm [Term] -> TypedTerm Term
deepRecord typeName fields =
  injectTermRecord $
    Core.termRecord $ Core.record (Core.nameLift _Record) (list [
      Core.field (Core.nameLift _Record_typeName) (deepName (Core.unName typeName)),
      Core.field (Core.nameLift _Record_fields) (Core.termList fields)])

-- | Build a deep TermUnwrap value
deepUnwrap :: TypedTerm Name -> TypedTerm Term
deepUnwrap typeName =
  injectTermUnwrap $
    deepName (Core.unName typeName)

-- | Build a deep WrappedTerm as a Term value (injected into Term.wrap)
deepWrap :: TypedTerm Name -> TypedTerm Term -> TypedTerm Term
deepWrap typeName body =
  injectTermWrap $
    Core.termRecord $ Core.record (Core.nameLift _WrappedTerm) (list [
      Core.field (Core.nameLift _WrappedTerm_typeName) (deepName (Core.unName typeName)),
      Core.field (Core.nameLift _WrappedTerm_body) body])

-- | Filter bindings to only DSL-eligible type definitions
-- | Generate a fully qualified binding name for a DSL function from a type name
-- For example, "hydra.core.AnnotatedTerm" -> "hydra.dsl.core.annotatedTerm"
-- For local types (no namespace), returns just the decapitalized local name
dslBindingName :: TypedTermDefinition (Name -> Name)
dslBindingName = define "dslBindingName" $
  doc "Generate a binding name for a DSL function from a type name" $
  "n" ~>
  "parts" <~ (Strings.splitOn (string ".") (Core.unName (var "n"))) $
  "localPart" <~ (Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))) $
  "localResult" <~ (Core.name (var "localPart")) $
  -- nsParts = parts minus the last element (the namespace components).
  -- Nothing means parts was empty (unreachable for a valid name);
  -- Just [] means the name has no namespace (local type).
  Maybes.cases (Lists.maybeInit (var "parts")) (var "localResult") ("nsParts" ~>
      Maybes.cases
        (Lists.uncons (var "nsParts"))
        -- single-element parts: local type, no namespace
        (var "localResult")
        ("nsHeadTail" ~>
          "dslNsParts" <~ (Logic.ifElse
            (Equality.equal (Pairs.first (var "nsHeadTail")) (string "hydra"))
            -- hydra.core.Foo -> [hydra, dsl] ++ tail nsParts
            (Lists.concat2 (list [string "hydra", string "dsl"]) (Pairs.second (var "nsHeadTail")))
            -- openGql.grammar.Foo -> [hydra, dsl] ++ nsParts
            (Lists.concat2 (list [string "hydra", string "dsl"]) (var "nsParts"))) $
          Core.name (Strings.intercalate (string ".")
            (Lists.concat2 (var "dslNsParts") (list [var "localPart"])))))

-- | Generate a DSL element name from a type name and a local element name.
-- For example, ("hydra.core.AnnotatedTerm", "annotatedTermBody") -> "hydra.dsl.core.annotatedTermBody"
-- This extracts the namespace from the type name, transforms it to the DSL namespace,
-- and appends the local element name.
dslDefinitionName :: TypedTermDefinition (Name -> String -> Name)
dslDefinitionName = define "dslDefinitionName" $
  doc "Generate a qualified DSL element name from a type name and local element name" $
  "typeName" ~> "localName" ~>
  "parts" <~ (Strings.splitOn (string ".") (Core.unName (var "typeName"))) $
  -- Extract namespace parts (all but last); fall back to the bare local name
  -- when the type name has no namespace (unreachable for well-formed inputs).
  Maybes.cases (Lists.maybeInit (var "parts")) (Core.name (var "localName")) ("nsParts" ~>
      "dslNsParts" <~ (Maybes.cases
        (Lists.uncons (var "nsParts"))
        -- nsParts empty: just prepend hydra.dsl
        (list [string "hydra", string "dsl"])
        ("nsHeadTail" ~> Logic.ifElse
          (Equality.equal (Pairs.first (var "nsHeadTail")) (string "hydra"))
          -- hydra.core -> hydra.dsl.core (drop the leading "hydra", keep the rest)
          (Lists.concat2 (list [string "hydra", string "dsl"]) (Pairs.second (var "nsHeadTail")))
          -- openGql.grammar -> hydra.dsl.openGql.grammar
          (Lists.concat2 (list [string "hydra", string "dsl"]) (var "nsParts")))) $
      Core.name (Strings.intercalate (string ".")
        (Lists.concat2 (var "dslNsParts") (list [var "localName"]))))

-- | Generate a record constructor function.
-- For a record type like {body: Term, annotation: Map(Name, Term)},
-- produces a deep (meta) term:
--   \body -> \annotation -> TermRecord (Record "hydra.core.AnnotatedTerm" [Field "body" body, ...])
-- When code-generated into Haskell, this becomes:
--   annotatedTerm body annotation = Core.TermRecord (Core.Record { ... })
-- | Transform a type module into a DSL module.
-- Returns Nothing if the module has no eligible type definitions.
dslModule :: TypedTermDefinition (InferenceContext -> Graph -> Module -> Either Error (Maybe Module))
dslModule = define "dslModule" $
  doc "Transform a type module into a DSL module" $
  "cx" ~> "graph" ~> "mod" ~>
    "typeBindings" <<~ (filterTypeBindings @@ var "cx" @@ var "graph" @@
      (Maybes.cat $ Lists.map
        ("d" ~> cases _Definition (var "d") (Just nothing) [
          _Definition_type>>: "td" ~>
            just (Annotations.typeBinding @@ (Packaging.typeDefinitionName $ var "td") @@ (Core.typeSchemeBody $ Packaging.typeDefinitionBody $ var "td"))])
        (Packaging.moduleDefinitions (var "mod")))) $
    Logic.ifElse (Lists.null (var "typeBindings"))
      (right nothing)
      ("dslBindings" <<~ Eithers.mapList ("b" ~>
        Eithers.bimap
          ("_e" ~> Error.errorDecoding $ var "_e")
          ("x" ~> var "x")
          (generateBindingsForType @@ var "cx" @@ var "graph" @@ var "b")) (var "typeBindings") $
        right (just (Packaging.module_
          (dslModuleName @@ (Packaging.moduleName (var "mod")))
          (just (Packaging.entityMetadata
            (just (Strings.cat $ list [
              string "DSL functions for ",
              Packaging.unModuleName (Packaging.moduleName (var "mod"))]))
            (list ([] :: [TypedTerm String])) (list ([] :: [TypedTerm EntityReference])) nothing))
          -- DSL modules depend on:
          -- (1) the original module + its source dependencies + hydra.typed (for TypedTerm), and
          -- (2) DSL modules for the source's dependencies (to reference other types' DSL functions)
          (Lists.map ("ns" ~> Packaging.moduleDependency (var "ns") nothing) (Lists.nub (Lists.concat2
            (list [Packaging.moduleName (var "mod"), Packaging.moduleName2 (string "hydra.typed")])
            (Lists.concat2
              (Lists.map ("dep" ~> Packaging.moduleDependencyModule (var "dep")) (Packaging.moduleDependencies (var "mod")))
              (primitive _lists_map @@ dslModuleName @@ (Lists.map ("dep" ~> Packaging.moduleDependencyModule (var "dep")) (Packaging.moduleDependencies (var "mod"))))))))
          (Lists.map ("b" ~> Packaging.definitionTerm (Packaging.termDefinition
            (Core.bindingName $ var "b")
            nothing
            (Maybes.map Scoping.typeSchemeToTermSignature $ Core.bindingTypeScheme $ var "b")
            (Core.bindingTerm $ var "b")))
            (deduplicateBindings @@ Lists.concat (var "dslBindings"))))))
-- | Generate a DSL module name from a source module name
-- For example, "hydra.core" -> "hydra.dsl.core"
dslModuleName :: TypedTermDefinition (ModuleName -> ModuleName)
dslModuleName = define "dslModuleName" $
  doc "Generate a DSL module name from a source module name" $
  "ns" ~>
  "parts" <~ (Strings.splitOn (string ".") (Packaging.unModuleName (var "ns"))) $
  "prefixFull" <~ (Packaging.moduleName2 (Strings.cat $ list [
    string "hydra.dsl.",
    Packaging.unModuleName (var "ns")])) $
  -- For hydra.* namespaces: hydra.foo -> hydra.dsl.foo
  -- For other namespaces: foo.bar -> hydra.dsl.foo.bar (preserve full path)
  -- An empty parts list is unreachable for a well-formed namespace; fall back
  -- to the full-prefix form.
  Maybes.cases (Lists.uncons (var "parts")) (var "prefixFull") ("ht" ~>
      Logic.ifElse (Equality.equal (Pairs.first (var "ht")) (string "hydra"))
        (Packaging.moduleName2 (Strings.cat $ list [
          string "hydra.dsl.",
          Strings.intercalate (string ".") (Pairs.second (var "ht"))]))
        (var "prefixFull"))

-- | Generate a fully qualified binding name for a DSL function from a type name
-- For example, "hydra.core.AnnotatedTerm" -> "hydra.dsl.core.annotatedTerm"
-- For local types (no namespace), returns just the decapitalized local name
-- | Build a TypeScheme from a list of parameter types and a result type.
-- All types are wrapped in TypedTerm. Forall variables are collected from the original type.
dslTypeScheme :: TypedTermDefinition (Type -> [Type] -> Type -> TypeScheme)
dslTypeScheme = define "dslTypeScheme" $
  doc "Build a TypeScheme with TypedTerm-wrapped parameter and result types" $
  "origType" ~> "paramTypes" ~> "resultType" ~>
  "typeVars" <~ (collectForallVars @@ var "origType") $
  "wrappedResult" <~ (wrapInTypedTerm (var "resultType")) $
  "funType" <~ (Lists.foldr
    ("paramType" ~> "acc" ~> Core.typeFunction $ Core.functionType (wrapInTypedTerm (var "paramType")) (var "acc"))
    (var "wrappedResult")
    (var "paramTypes")) $
  Core.typeScheme (var "typeVars") (var "funType") nothing

-- | Collect forall type variables from a type (stripping annotations)
-- | Filter bindings to only DSL-eligible type definitions
filterTypeBindings :: TypedTermDefinition (InferenceContext -> Graph -> [Binding] -> Either Error [Binding])
filterTypeBindings = define "filterTypeBindings" $
  doc "Filter bindings to only DSL-eligible type definitions" $
  "cx" ~> "graph" ~> "bindings" ~>
    Eithers.map (primitive _maybes_cat) $
      Eithers.mapList (isDslEligibleBinding @@ var "cx" @@ var "graph") $
        primitive _lists_filter @@ Annotations.isNativeType @@ var "bindings"

-- | Check if a binding is eligible for DSL generation.
-- Excludes phantom types (TypedTerm, TypedBinding) since they are meta-infrastructure.
-- | Generate all DSL bindings for a single type binding.
-- Inspects the type definition and generates appropriate helpers:
-- - Records: constructor + accessors + withXxx updaters
-- - Unions: injection helpers
-- - Wrapped types: wrap + unwrap
generateBindingsForType :: TypedTermDefinition (InferenceContext -> Graph -> Binding -> Either DecodingError [Binding])
generateBindingsForType = define "generateBindingsForType" $
  doc "Generate all DSL bindings for a type binding" $
  "cx" ~> "graph" ~> "b" ~>
  "typeName" <~ (Core.bindingName (var "b")) $
  Eithers.bind (decoderFor _Type @@ var "graph" @@ (Core.bindingTerm (var "b"))) (
    "rawType" ~>
    "typ" <~ (Strip.deannotateTypeParameters @@ (Strip.deannotateType @@ var "rawType")) $
    right (cases _Type (var "typ") (Just $ list ([] :: [TypedTerm Binding])) [
      _Type_record>>: "fts" ~>
        Lists.concat $ list [
          generateRecordConstructor @@ var "rawType" @@ var "typeName" @@ var "fts",
          Lists.map (generateRecordAccessor @@ var "rawType" @@ var "typeName") (var "fts"),
          Lists.map (generateRecordWithUpdater @@ var "rawType" @@ var "typeName" @@ var "fts")
            (var "fts")],
      _Type_union>>: "fts" ~>
        Lists.map (generateUnionInjector @@ var "rawType" @@ var "typeName") (var "fts"),
      _Type_wrap>>: "innerType" ~>
        generateWrappedTypeAccessors @@ var "rawType" @@ var "typeName" @@ var "innerType"]))

-- | Deduplicate bindings by giving duplicate names a numeric suffix.
-- Later bindings get suffixes; earlier ones keep their name.
-- Multiple duplicates get increasing numeric suffixes: name, name2, name3, etc.
-- | Generate a record field accessor function.
-- For a field "name" in record type "Binding", produces:
--   bindingName :: Binding -> Name
--   bindingName x = x.name
generateRecordAccessor :: TypedTermDefinition (Type -> Name -> FieldType -> Binding)
generateRecordAccessor = define "generateRecordAccessor" $
  doc "Generate a record field accessor function" $
  "origType" ~> "typeName" ~> "ft" ~>
  "fieldName" <~ (Core.fieldTypeName (var "ft")) $
  "accessorLocalName" <~ (Strings.cat $ list [
    Formatting.decapitalize @@ (Names.localNameOf @@ var "typeName"),
    Strings.intercalate (string "") (Lists.map ("s" ~> Formatting.capitalize @@ var "s") (Strings.splitOn (string ".") (Core.unName (var "fieldName"))))]) $
  "accessorName" <~ (dslDefinitionName @@ var "typeName" @@ var "accessorLocalName") $
  -- Body: projection as a simple elimination
  "paramDomain" <~ (wrapInTypedTerm (nominalResultType @@ var "typeName" @@ var "origType")) $
  "rawBody" <~ (Core.termLambda $ Core.lambda (Core.name (string "x")) (just (var "paramDomain")) $
      wrapTermInTypedTerm (deepApplication
        (deepProjection (var "typeName") (var "fieldName"))
        (unwrapTypedTerm (Core.termVariable (Core.name (string "x")))))) $
  "description" <~ (Strings.cat $ list [
    string "DSL accessor for the ",
    Core.unName (var "fieldName"),
    string " field of ",
    Core.unName (var "typeName")]) $
  "body" <~ (Annotations.setTermDescription @@ (just (var "description")) @@ var "rawBody") $
  "ts" <~ (dslTypeScheme @@ var "origType"
    @@ list [nominalResultType @@ var "typeName" @@ var "origType"]
    @@ Core.fieldTypeType (var "ft")) $
  Core.binding
    (var "accessorName")
    (var "body")
    (just (var "ts"))

-- | Generate a record constructor function.
-- For a record type like {body: Term, annotation: Map(Name, Term)},
-- produces a deep (meta) term:
--   \body -> \annotation -> TermRecord (Record "hydra.core.AnnotatedTerm" [Field "body" body, ...])
-- When code-generated into Haskell, this becomes:
--   annotatedTerm body annotation = Core.TermRecord (Core.Record { ... })
generateRecordConstructor :: TypedTermDefinition (Type -> Name -> [FieldType] -> [Binding])
generateRecordConstructor = define "generateRecordConstructor" $
  doc "Generate a record constructor function" $
  "origType" ~> "typeName" ~> "fieldTypes" ~>
  -- Build deep fields and record using helpers
  "dFields" <~ (Lists.map
    ("ft" ~> deepField (Core.fieldTypeName (var "ft"))
      (unwrapTypedTerm (Core.termVariable (Core.name (Formatting.decapitalize @@ (Names.localNameOf @@ Core.fieldTypeName (var "ft")))))))
    (var "fieldTypes")) $
  "recordTerm" <~ (wrapTermInTypedTerm (deepRecord (var "typeName") (var "dFields"))) $
  -- Build (paramName, TypedTerm<fieldType>) pairs for lambda construction
  "paramPairs" <~ (Lists.map
    ("ft" ~> pair
      (Formatting.decapitalize @@ (Names.localNameOf @@ Core.fieldTypeName (var "ft")))
      (wrapInTypedTerm (Core.fieldTypeType (var "ft"))))
    (var "fieldTypes")) $
  -- Wrap in typed lambdas for each parameter (right to left)
  "rawBody" <~ (Lists.foldl
    ("acc" ~> "pp" ~>
      Core.termLambda $ Core.lambda (Core.name (Pairs.first (var "pp"))) (just (Pairs.second (var "pp"))) (var "acc"))
    (var "recordTerm")
    (Lists.reverse (var "paramPairs"))) $
  "description" <~ (Strings.cat $ list [
    string "DSL constructor for ",
    Core.unName (var "typeName")]) $
  "body" <~ (Annotations.setTermDescription @@ (just (var "description")) @@ var "rawBody") $
  -- Type: TypedTerm<FieldType1> -> TypedTerm<FieldType2> -> ... -> TypedTerm<RecordType>
  "paramTypes" <~ (Lists.map ("ft" ~> Core.fieldTypeType (var "ft")) (var "fieldTypes")) $
  "resultType" <~ (nominalResultType @@ var "typeName" @@ var "origType") $
  "ts" <~ (dslTypeScheme @@ var "origType" @@ var "paramTypes" @@ var "resultType") $
  -- Return as a single-element list with the constructor binding
  list [Core.binding
    (dslBindingName @@ var "typeName")
    (var "body")
    (just (var "ts"))]

-- | Generate a record field accessor function.
-- For a field "name" in record type "Binding", produces:
--   bindingName :: Binding -> Name
--   bindingName x = x.name
-- | Generate a "withXxx" record field updater function.
-- For a field "name" in record type "Binding" (with fields name, term, type), produces:
--   bindingWithName :: Binding -> Name -> Binding
--   bindingWithName b newName = Binding newName (bindingTerm b) (bindingType b)
-- This constructs a new record with the specified field replaced and all others projected.
generateRecordWithUpdater :: TypedTermDefinition (Type -> Name -> [FieldType] -> FieldType -> Binding)
generateRecordWithUpdater = define "generateRecordWithUpdater" $
  doc "Generate a withXxx record field updater function" $
  "origType" ~> "typeName" ~> "allFields" ~> "targetField" ~>
  "targetFieldName" <~ (Core.fieldTypeName (var "targetField")) $
  -- Build the updater name: e.g., "bindingWithName"
  "updaterLocalName" <~ (Strings.cat $ list [
    Formatting.decapitalize @@ (Names.localNameOf @@ var "typeName"),
    string "With",
    Strings.intercalate (string "") (Lists.map ("s" ~> Formatting.capitalize @@ var "s") (Strings.splitOn (string ".") (Core.unName (var "targetFieldName"))))]) $
  "updaterName" <~ (dslDefinitionName @@ var "typeName" @@ var "updaterLocalName") $
  -- Build deep fields: project from unwrapped original, except target uses unwrapped newVal
  "dFields" <~ (Lists.map
    ("ft" ~> deepField (Core.fieldTypeName (var "ft"))
      (Logic.ifElse (Equality.equal
        (Core.unName (Core.fieldTypeName (var "ft")))
        (Core.unName (var "targetFieldName")))
        (unwrapTypedTerm (Core.termVariable (Core.name (string "newVal"))))
        (deepApplication
          (deepProjection (var "typeName") (Core.fieldTypeName (var "ft")))
          (unwrapTypedTerm (Core.termVariable (Core.name (string "original")))))))
    (var "allFields")) $
  "recDomain" <~ (wrapInTypedTerm (nominalResultType @@ var "typeName" @@ var "origType")) $
  "fieldDomain" <~ (wrapInTypedTerm (Core.fieldTypeType (var "targetField"))) $
  "rawBody" <~ (
    Core.termLambda $ Core.lambda (Core.name (string "original")) (just (var "recDomain")) $
    Core.termLambda $ Core.lambda (Core.name (string "newVal")) (just (var "fieldDomain")) $
    wrapTermInTypedTerm (deepRecord (var "typeName") (var "dFields"))) $
  "description" <~ (Strings.cat $ list [
    string "DSL updater for the ",
    Core.unName (var "targetFieldName"),
    string " field of ",
    Core.unName (var "typeName")]) $
  "body" <~ (Annotations.setTermDescription @@ (just (var "description")) @@ var "rawBody") $
  "recType" <~ (nominalResultType @@ var "typeName" @@ var "origType") $
  "ts" <~ (dslTypeScheme @@ var "origType"
    @@ list [var "recType", Core.fieldTypeType (var "targetField")]
    @@ var "recType") $
  Core.binding
    (var "updaterName")
    (var "body")
    (just (var "ts"))

-- | Generate a union injection helper.
-- For a variant "lambda" in union type "Function", produces:
--   functionLambda :: Lambda -> Function
--   functionLambda x = Function.lambda x
-- For unit variants (field type is unit), produces:
--   comparisonLessThan :: Comparison
--   comparisonLessThan = Comparison.lessThan ()
-- | Generate a union injection helper.
-- For a variant "lambda" in union type "Function", produces:
--   functionLambda :: Lambda -> Function
--   functionLambda x = Function.lambda x
-- For unit variants (field type is unit), produces:
--   comparisonLessThan :: Comparison
--   comparisonLessThan = Comparison.lessThan ()
generateUnionInjector :: TypedTermDefinition (Type -> Name -> FieldType -> Binding)
generateUnionInjector = define "generateUnionInjector" $
  doc "Generate a union injection helper" $
  "origType" ~> "typeName" ~> "ft" ~>
  "fieldName" <~ (Core.fieldTypeName (var "ft")) $
  "fieldType" <~ (Core.fieldTypeType (var "ft")) $
  -- Build the injector name: e.g., "functionLambda" or "comparisonLessThan"
  "injectorLocalName" <~ (Strings.cat $ list [
    Formatting.decapitalize @@ (Names.localNameOf @@ var "typeName"),
    Strings.intercalate (string "") (Lists.map ("s" ~> Formatting.capitalize @@ var "s") (Strings.splitOn (string ".") (Core.unName (var "fieldName"))))]) $
  "injectorName" <~ (dslDefinitionName @@ var "typeName" @@ var "injectorLocalName") $
  -- Check if field type is unit (for unit variants like enum members)
  "isUnit" <~ (isUnitType_ @@ var "fieldType") $
  -- Build simple injection
  -- Build deep injection: deepField for the variant, then deepInjection into the union type
  "dFieldValue" <~ (Logic.ifElse (var "isUnit")
    (Core.termInject $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_unit) Core.termUnit))
    (unwrapTypedTerm (Core.termVariable (Core.name (string "x"))))) $
  "injectionTerm" <~ (wrapTermInTypedTerm (deepInjection (var "typeName") (deepField (var "fieldName") (var "dFieldValue")))) $
  -- For non-unit variants, wrap in a typed lambda; for unit, it's a constant TypedTerm
  "variantDomain" <~ (wrapInTypedTerm (Core.fieldTypeType (var "ft"))) $
  "rawBody" <~ (Logic.ifElse (var "isUnit")
    (var "injectionTerm")
    (Core.termLambda $ Core.lambda (Core.name (string "x")) (just (var "variantDomain")) (var "injectionTerm"))) $
  "description" <~ (Strings.cat $ list [
    string "DSL injection for the ",
    Core.unName (var "fieldName"),
    string " variant of ",
    Core.unName (var "typeName")]) $
  "body" <~ (Annotations.setTermDescription @@ (just (var "description")) @@ var "rawBody") $
  "unionType" <~ (nominalResultType @@ var "typeName" @@ var "origType") $
  "ts" <~ (Logic.ifElse (var "isUnit")
    (dslTypeScheme @@ var "origType" @@ list ([] :: [TypedTerm Type]) @@ var "unionType")
    (dslTypeScheme @@ var "origType" @@ list [Core.fieldTypeType (var "ft")] @@ var "unionType")) $
  Core.binding
    (var "injectorName")
    (var "body")
    (just (var "ts"))

-- | Check if a type is the unit type (stripping annotations first)
-- Note: uses stripAnnotations to avoid recursive Haskell-level DSL terms,
-- which would cause infinite recursion during code generation.
-- | Generate wrap/unwrap accessors for a wrapped type.
-- For a wrapped type like "Name" wrapping String, produces:
--   name :: String -> Name      (constructor/wrap)
--   unName :: Name -> String    (unwrap)
generateWrappedTypeAccessors :: TypedTermDefinition (Type -> Name -> Type -> [Binding])
generateWrappedTypeAccessors = define "generateWrappedTypeAccessors" $
  doc "Generate wrap/unwrap accessors for a wrapped type" $
  "origType" ~> "typeName" ~> "innerType" ~>
  "localName" <~ (Names.localNameOf @@ var "typeName") $
  -- Wrap function: decapitalized type name
  "wrapName" <~ (dslDefinitionName @@ var "typeName" @@ (Formatting.decapitalize @@ var "localName")) $
  -- Unwrap function: "un" + type local name
  "unwrapLocalName" <~ (Strings.cat $ list [string "un", var "localName"]) $
  "unwrapName" <~ (dslDefinitionName @@ var "typeName" @@ var "unwrapLocalName") $
  "wrapperType" <~ (nominalResultType @@ var "typeName" @@ var "origType") $
  -- Wrap: \(x :: TypedTerm<InnerType>) -> WrappedTerm typeName x
  "wrapDomain" <~ (wrapInTypedTerm (var "innerType")) $
  "rawWrapBody" <~ (
    Core.termLambda $ Core.lambda (Core.name (string "x")) (just (var "wrapDomain")) $
        wrapTermInTypedTerm (deepWrap (var "typeName")
          (unwrapTypedTerm (Core.termVariable (Core.name (string "x")))))) $
  "wrapDescription" <~ (Strings.cat $ list [
    string "DSL constructor for the ",
    Core.unName (var "typeName"),
    string " wrapper"]) $
  "wrapBody" <~ (Annotations.setTermDescription @@ (just (var "wrapDescription")) @@ var "rawWrapBody") $
  -- Unwrap: \(x :: TypedTerm<WrapperType>) -> TypedTerm(apply (unwrap typeName) (unTypedTerm x))
  "unwrapDomain" <~ (wrapInTypedTerm (var "wrapperType")) $
  "rawUnwrapBody" <~ (
    Core.termLambda $ Core.lambda (Core.name (string "x")) (just (var "unwrapDomain")) $
        wrapTermInTypedTerm (deepApplication
          (deepUnwrap (var "typeName"))
          (unwrapTypedTerm (Core.termVariable (Core.name (string "x")))))) $
  "unwrapDescription" <~ (Strings.cat $ list [
    string "DSL accessor for the body of ",
    Core.unName (var "typeName")]) $
  "unwrapBody" <~ (Annotations.setTermDescription @@ (just (var "unwrapDescription")) @@ var "rawUnwrapBody") $
  "wrapTs" <~ (dslTypeScheme @@ var "origType" @@ list [var "innerType"] @@ var "wrapperType") $
  "unwrapTs" <~ (dslTypeScheme @@ var "origType" @@ list [var "wrapperType"] @@ var "innerType") $
  list [
    Core.binding (var "wrapName") (var "wrapBody") (just (var "wrapTs")),
    Core.binding (var "unwrapName") (var "unwrapBody") (just (var "unwrapTs"))]

-- | Inject a term into the Term.application variant
injectTermApplication :: TypedTerm Term -> TypedTerm Term
injectTermApplication t = Core.termInject $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_application) t)

-- | Inject a term into the Term.cases variant
injectTermCases :: TypedTerm Term -> TypedTerm Term
injectTermCases t = Core.termInject $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_cases) t)

-- | Inject a term into the Term.project variant
injectTermProject :: TypedTerm Term -> TypedTerm Term
injectTermProject t = Core.termInject $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_project) t)

-- | Generate a DSL element name from a type name and a local element name.
-- For example, ("hydra.core.AnnotatedTerm", "annotatedTermBody") -> "hydra.dsl.core.annotatedTermBody"
-- This extracts the namespace from the type name, transforms it to the DSL namespace,
-- and appends the local element name.
injectTermRecord :: TypedTerm Term -> TypedTerm Term
injectTermRecord t = Core.termInject $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_record) t)

-- | Inject a term into the Term.union variant
injectTermUnion :: TypedTerm Term -> TypedTerm Term
injectTermUnion t = Core.termInject $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_inject) t)

-- | Inject a term into the Term.unwrap variant
injectTermUnwrap :: TypedTerm Term -> TypedTerm Term
injectTermUnwrap t = Core.termInject $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_unwrap) t)

-- | Inject a term into the Term.wrap variant
injectTermWrap :: TypedTerm Term -> TypedTerm Term
injectTermWrap t = Core.termInject $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_wrap) t)

-- | Generate all DSL bindings for a single type binding.
-- Inspects the type definition and generates appropriate helpers:
-- - Records: constructor + accessors + withXxx updaters
-- - Unions: injection helpers
-- - Wrapped types: wrap + unwrap
-- | Check if a binding is eligible for DSL generation.
-- Excludes phantom types (TypedTerm, TypedBinding) since they are meta-infrastructure.
isDslEligibleBinding :: TypedTermDefinition (InferenceContext -> Graph -> Binding -> Either Error (Maybe Binding))
isDslEligibleBinding = define "isDslEligibleBinding" $
  doc "Check if a binding is eligible for DSL generation" $
  "cx" ~> "graph" ~> "b" ~>
  "ns" <~ (Names.moduleNameOf @@ Core.bindingName (var "b")) $
  Logic.ifElse (Equality.equal (Maybes.cases (var "ns") (string "") (reify Packaging.unModuleName)) (string "hydra.typed"))
    (right nothing)
    (right (just (var "b")))

-- | Generate a "withXxx" record field updater function.
-- For a field "name" in record type "Binding" (with fields name, term, type), produces:
--   bindingWithName :: Binding -> Name -> Binding
--   bindingWithName b newName = Binding newName (bindingTerm b) (bindingType b)
-- This constructs a new record with the specified field replaced and all others projected.
isUnitType_ :: TypedTerm (Type -> Bool)
isUnitType_ = "t" ~> cases _Type (Strip.deannotateType @@ var "t") (Just Phantoms.false) [
  _Type_unit>>: constant Phantoms.true]

-- | Transform a type module into a DSL module.
-- Returns Nothing if the module has no eligible type definitions.
-- | Build the nominal result type for a type definition.
-- For non-polymorphic types: TypeVariable typeName
-- For polymorphic types like (forall n. ModuleNames n): TypeApplication (TypeVariable typeName) (TypeVariable n)
nominalResultType :: TypedTermDefinition (Name -> Type -> Type)
nominalResultType = define "nominalResultType" $
  doc "Build the nominal result type with type applications for forall variables" $
  "typeName" ~> "origType" ~>
  "vars" <~ (collectForallVars @@ var "origType") $
  Lists.foldl
    ("acc" ~> "v" ~> Core.typeApplication $ Core.applicationType (var "acc") (Core.typeVariable (var "v")))
    (Core.typeVariable (var "typeName"))
    (var "vars")

-- | Inject a Record-typed term into the Term.record variant
-- Produces TermInject(Injection _Term (Field _Term_record innerRecord))

-- | Unwrap a TypedTerm argument: apply (TermUnwrap _TypedTerm) to the variable
unwrapTypedTerm :: TypedTerm Term -> TypedTerm Term
unwrapTypedTerm v = Core.termApplication $ Core.application
  (Core.termUnwrap (Core.nameLift _TypedTerm))
  v

-- | Wrap a type in TypedTerm: TypeApplication (TypeVariable "hydra.typed.TypedTerm") innerType
wrapInTypedTerm :: TypedTerm Type -> TypedTerm Type
wrapInTypedTerm t = Core.typeApplication $ Core.applicationType (Core.typeVariable (Core.nameLift _TypedTerm)) t

-- | Wrap a term in TypedTerm: WrappedTerm _TypedTerm term
wrapTermInTypedTerm :: TypedTerm Term -> TypedTerm Term
wrapTermInTypedTerm t = Core.termWrap $ Core.wrappedTerm (Core.nameLift _TypedTerm) t


