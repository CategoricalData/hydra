{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Dsls where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Typing          as Typing
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms hiding (
  elimination, field, fieldType, floatType, floatValue, function, injection, integerType, integerValue, lambda, literal,
  literalType, record, term, type_, typeScheme, wrap)
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Scoping as Scoping
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Strip as Strip
import qualified Hydra.Overlay.Haskell.Dsl.Typed.DeepCore as DeepCore
import           Hydra.Overlay.Haskell.Dsl.Typed.DeepCore ((@@@))
import           Prelude hiding ((++))
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Hydra.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Lib.Lists as DefLists
import qualified Hydra.Lib.Optionals as DefOptionals


ns :: ModuleName
ns = ModuleName "hydra.dsls"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Annotations.ns, Formatting.ns, Lexical.ns, Names.ns, Scoping.ns, Strip.ns, ModuleName "hydra.constants", ModuleName "hydra.decode.core", ModuleName "hydra.encode.core"] L.++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Functions for generating domain-specific DSL modules from type modules")}
  where
    definitions = [
      toDefinition collectForallVars,
      toDefinition deduplicateBindings,
      toDefinition dslBindingName,
      toDefinition dslDefinitionName,
      toDefinition dslModule,
      toDefinition dslModuleName,
      toDefinition dslSignatureTypeScheme,
      toDefinition dslTypeScheme,
      toDefinition filterTypeBindings,
      toDefinition generateBindingsForType,
      toDefinition generateParametricRefBuilders,
      toDefinition generateRecordAccessor,
      toDefinition generateRecordConstructor,
      toDefinition generateRecordWithUpdater,
      toDefinition generateRefBindings,
      toDefinition generateSignatureRef,
      toDefinition generateTypeNameToken,
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
      "usedNames" <~ (Sets.fromList (Lists.map ("a" ~> Core.bindingName (var "a")) (var "acc")) :: TypedTerm (S.Set Name)) $
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

-- | Build a deep variable reference as a Term value: TermVariable name.
-- A primitive reference lowers to the same construct (Terms.primitive = TermVariable),
-- so this serves both term refs and primitive refs. The argument is the (already
-- qualified) referenced name; it is embedded as a deep Name within a deep Term.variable.
deepVariable :: TypedTerm Name -> TypedTerm Term
deepVariable refName =
  injectTermVariable $ deepName (Core.unName refName)

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
  Names.derivedBindingName @@ list [string "hydra", string "dsl"] @@ boolean False

-- | Generate a DSL element name from a type name and a local element name.
-- For example, ("hydra.core.AnnotatedTerm", "annotatedTermBody") -> "hydra.dsl.core.annotatedTermBody"
-- This extracts the namespace from the type name, transforms it to the DSL namespace,
-- and appends the local element name.
dslDefinitionName :: TypedTermDefinition (Name -> String -> Name)
dslDefinitionName = define "dslDefinitionName" $
  doc "Generate a qualified DSL element name from a type name and local element name" $
  Names.derivedDefinitionName @@ list [string "hydra", string "dsl"] @@ boolean False @@ boolean True

-- | Generate a record constructor function.
-- For a record type like {body: Term, annotation: Map(Name, Term)},
-- produces a deep (meta) term:
--   \body -> \annotation -> TermRecord (Record "hydra.core.AnnotatedTerm" [Field "body" body, ...])
-- When code-generated into Haskell, this becomes:
--   annotatedTerm body annotation = Core.TermRecord (Core.Record { ... })
-- | Transform a source module into a DSL module.
-- Walks every definition and dispatches on its variant:
--   - type definitions      -> record/union/wrap helpers (generateBindingsForType)
--   - term definitions      -> a typed reference wrapper (generateSignatureRef)
--   - primitive definitions -> a typed reference wrapper (generateSignatureRef)
-- A module may mix all three; the DSL module unions whatever its definitions produce.
-- Returns Nothing if the module yields no DSL bindings at all.
dslModule :: TypedTermDefinition (InferenceContext -> Graph -> Module -> Either Error (Maybe Module))
dslModule = define "dslModule" $
  doc "Transform a source module into a DSL module" $
  "cx" ~> "graph" ~> "mod" ~>
    -- Type path (unchanged): collect eligible type bindings, then generate helpers.
    "typeBindings" <<~ (filterTypeBindings @@ var "cx" @@ var "graph" @@
      (Optionals.cat $ Lists.map
        ("d" ~> cases _Definition (var "d") (Just nothing) [
          _Definition_type>>: "td" ~>
            just (Annotations.typeBinding @@ (Packaging.typeDefinitionName $ var "td") @@ (Core.typeSchemeBody $ Packaging.typeDefinitionBody $ var "td"))])
        (Packaging.moduleDefinitions (var "mod")))) $
    "typeDslBindings" <<~ (Eithers.mapList ("b" ~>
        Eithers.bimap
          ("_e" ~> Error.errorDecoding $ var "_e")
          ("x" ~> var "x")
          (generateBindingsForType @@ var "cx" @@ var "graph" @@ var "b")) (var "typeBindings")) $
    -- Ref path (new): one typed reference wrapper per term/primitive definition.
    "refDslBindings" <<~ (Eithers.mapList ("d" ~> generateRefBindings @@ var "d")
        (Packaging.moduleDefinitions (var "mod"))) $
    "allBindings" <~ (deduplicateBindings @@ Lists.concat2
      (Lists.concat (var "typeDslBindings"))
      (Lists.concat (var "refDslBindings"))) $
    Logic.ifElse (Lists.null (var "allBindings"))
      (right nothing)
      (right (just (Packaging.module_
        (dslModuleName @@ (Packaging.moduleName (var "mod")))
        (just (Packaging.entityMetadata
          (just (Strings.cat $ list [
            string "DSL functions for ",
            Packaging.unModuleName (Packaging.moduleName (var "mod"))]))
          (list ([] :: [TypedTerm String])) (list ([] :: [TypedTerm EntityReference])) nothing))
        -- DSL modules depend on:
        -- (1) the original module + its source dependencies + hydra.typed (for TypedTerm),
        -- (2) DSL modules for the source's dependencies (to reference other types' DSL functions), and
        -- (3) the original module's own encode/decode modules (referenced by
        --     generateParametricRefBuilders's composition builders)
        (Lists.map ("ns" ~> Packaging.moduleDependency (var "ns") nothing) (Lists.nub (Lists.concat2
          (list [Packaging.moduleName (var "mod"), Packaging.moduleName2 (string "hydra.typed"),
            Names.derivedModuleName @@ list [string "hydra", string "encode"] @@ boolean True @@ (Packaging.moduleName (var "mod")),
            Names.derivedModuleName @@ list [string "hydra", string "decode"] @@ boolean True @@ (Packaging.moduleName (var "mod"))])
          (Lists.concat2
            (Lists.map ("dep" ~> Packaging.moduleDependencyModule (var "dep")) (Packaging.moduleDependencies (var "mod")))
            (primitive DefLists.map @@ dslModuleName @@ (Lists.map ("dep" ~> Packaging.moduleDependencyModule (var "dep")) (Packaging.moduleDependencies (var "mod"))))))))
        (Lists.map ("b" ~> Packaging.definitionTerm (Packaging.termDefinition
          (Core.bindingName $ var "b")
          nothing
          (Optionals.map (asTerm Scoping.typeSchemeToTermSignature) $ Core.bindingTypeScheme $ var "b")
          (Core.bindingTerm $ var "b")))
          (var "allBindings")))))

-- | Generate the DSL reference bindings for a single definition: a one-element list
-- holding the typed reference wrapper for a primitive (or a signature-carrying term)
-- definition, or the empty list otherwise.
--
-- Primitive definitions always carry an explicit signature on the definition, so the
-- primitive path is unconditional and needs no inference (#467, primitives-first).
--
-- Term definitions only yield a ref when they already carry a signature. Inference is
-- NEVER run on derived modules, and the raw in-memory term modules fed to this pass have
-- termDefinitionSignature = Nothing; so a term definition without a signature is SKIPPED
-- (empty list), not an error. Wiring term-level DSL refs (which need a signature source)
-- is a later phase; until then the type path covers each module's type definitions and
-- the primitive path covers each library's primitives.
generateRefBindings :: TypedTermDefinition (Definition -> Either Error [Binding])
generateRefBindings = define "generateRefBindings" $
  doc "Generate typed reference DSL bindings for a primitive (or signature-carrying term) definition" $
  "d" ~>
  cases _Definition (var "d") (Just $ right (list ([] :: [TypedTerm Binding]))) [
    _Definition_type>>: constant (right (list ([] :: [TypedTerm Binding]))),
    _Definition_term>>: "td" ~>
      Optionals.cases (Packaging.termDefinitionSignature (var "td"))
        (right (list ([] :: [TypedTerm Binding])))
        ("sig" ~> right (list [generateSignatureRef @@ (Packaging.termDefinitionName (var "td")) @@ var "sig"])),
    _Definition_primitive>>: "pd" ~>
      right (list [generateSignatureRef @@ (Packaging.primitiveDefinitionName (var "pd")) @@ (Packaging.primitiveDefinitionSignature (var "pd"))])]
-- | Generate a DSL module name from a source module name
-- For example, "hydra.core" -> "hydra.dsl.core"
dslModuleName :: TypedTermDefinition (ModuleName -> ModuleName)
dslModuleName = define "dslModuleName" $
  doc "Generate a DSL module name from a source module name" $
  Names.derivedModuleName @@ list [string "hydra", string "dsl"] @@ boolean False

-- | Build a "functions of phantom terms" TypeScheme from a TermSignature.
-- Each value parameter type and the result type are wrapped in TypedTerm, then folded
-- into a chain of function arrows; the signature's type parameters become the foralls.
-- For a signature p1 .. pn -> r this yields:
--   forall vars. TypedTerm p1 -> ... -> TypedTerm pn -> TypedTerm r
-- (NOT TypedTerm (p1 -> ... -> r) — the wrapper is a function of phantom terms, #467.)
dslSignatureTypeScheme :: TypedTermDefinition (TermSignature -> TypeScheme)
dslSignatureTypeScheme = define "dslSignatureTypeScheme" $
  doc "Build a TypedTerm-wrapped TypeScheme (functions of phantom terms) from a TermSignature" $
  "sig" ~>
  "typeVars" <~ (Lists.map ("tp" ~> Typing.typeParameterName (var "tp")) (Typing.termSignatureTypeParameters (var "sig"))) $
  "paramTypes" <~ (Lists.map ("p" ~> Typing.parameterType (var "p")) (Typing.termSignatureParameters (var "sig"))) $
  "resultType" <~ (Typing.resultType (Typing.termSignatureResult (var "sig"))) $
  "wrappedResult" <~ (wrapInTypedTerm (var "resultType")) $
  "funType" <~ (Lists.foldr
    ("paramType" ~> "acc" ~> Core.typeFunction $ Core.functionType (wrapInTypedTerm (var "paramType")) (var "acc"))
    (var "wrappedResult")
    (var "paramTypes")) $
  Core.typeScheme (var "typeVars") (var "funType") nothing

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
    Eithers.map (primitive DefOptionals.cat) $
      Eithers.mapList (isDslEligibleBinding @@ var "cx" @@ var "graph") $
        primitive DefLists.filter @@ Annotations.isNativeType @@ var "bindings"

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
    -- Token + parametric composition builders are generated only alongside a
    -- concrete nominal shape (record/union/wrap) — never for a transparent
    -- alias (e.g. SymmetricAdapter = Adapter t t v v e, a bare Type_application),
    -- since hosts do not materialize a distinct class/type for aliases and a
    -- token/builder referencing one would be dead weight at best, a broken
    -- reference at worst.
    right (cases _Type (var "typ") (Just $ list ([] :: [TypedTerm Binding])) [
      _Type_record>>: "fts" ~>
        Lists.concat $ list [
          list [generateTypeNameToken @@ var "rawType" @@ var "typeName"],
          generateParametricRefBuilders @@ var "rawType" @@ var "typeName",
          generateRecordConstructor @@ var "rawType" @@ var "typeName" @@ var "fts",
          Lists.map (generateRecordAccessor @@ var "rawType" @@ var "typeName") (var "fts"),
          Lists.map (generateRecordWithUpdater @@ var "rawType" @@ var "typeName" @@ var "fts")
            (var "fts")],
      _Type_union>>: "fts" ~>
        Lists.concat $ list [
          list [generateTypeNameToken @@ var "rawType" @@ var "typeName"],
          generateParametricRefBuilders @@ var "rawType" @@ var "typeName",
          Lists.map (generateUnionInjector @@ var "rawType" @@ var "typeName") (var "fts")],
      _Type_wrap>>: "innerType" ~>
        Lists.concat $ list [
          list [generateTypeNameToken @@ var "rawType" @@ var "typeName"],
          generateParametricRefBuilders @@ var "rawType" @@ var "typeName",
          generateWrappedTypeAccessors @@ var "rawType" @@ var "typeName" @@ var "innerType"]]))

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

-- | Generate a typed-reference wrapper for a term or primitive definition.
-- Given the (already qualified) referenced name and its TermSignature, produces a
-- "function of phantom terms" DSL binding. For a signature with value parameters p1..pn:
--   <dslName> :: TypedTerm p1 -> ... -> TypedTerm pn -> TypedTerm r
--   <dslName> a1 .. an = TypedTerm (ref @@ unTypedTerm a1 @@ ... @@ unTypedTerm an)
-- where `ref` is the deep variable reference to the underlying definition/primitive
-- (a primitive lowers to TermVariable, so the same construct serves both).
-- For a nullary signature (no value parameters) the body is the bare wrapped reference,
-- a constant TypedTerm with no lambda.
generateSignatureRef :: TypedTermDefinition (Name -> TermSignature -> Binding)
generateSignatureRef = define "generateSignatureRef" $
  doc "Generate a typed-reference DSL wrapper from a term/primitive name and signature" $
  "refName" ~> "sig" ~>
  "params" <~ (Typing.termSignatureParameters (var "sig")) $
  -- Parameter (name, TypedTerm<paramType>) pairs for the lambda chain. Names come straight
  -- from the signature; they are already unique within the signature.
  "paramPairs" <~ (Lists.map
    ("p" ~> pair
      (Core.unName (Typing.parameterName (var "p")))
      (wrapInTypedTerm (Typing.parameterType (var "p"))))
    (var "params")) $
  -- Body: apply the deep reference to each unwrapped phantom argument, left to right.
  "appBody" <~ (Lists.foldl
    ("acc" ~> "pp" ~> deepApplication (var "acc")
      (unwrapTypedTerm (Core.termVariable (Core.name (Pairs.first (var "pp"))))))
    (deepVariable (var "refName"))
    (var "paramPairs")) $
  "refTerm" <~ (wrapTermInTypedTerm (var "appBody")) $
  -- Wrap in typed lambdas for each parameter (right to left). Nullary -> no lambda.
  "rawBody" <~ (Lists.foldl
    ("acc" ~> "pp" ~>
      Core.termLambda $ Core.lambda (Core.name (Pairs.first (var "pp"))) (just (Pairs.second (var "pp"))) (var "acc"))
    (var "refTerm")
    (Lists.reverse (var "paramPairs"))) $
  "description" <~ (Strings.cat $ list [
    string "DSL reference to ",
    Core.unName (var "refName")]) $
  "body" <~ (Annotations.setTermDescription @@ (just (var "description")) @@ var "rawBody") $
  "ts" <~ (dslSignatureTypeScheme @@ var "sig") $
  Core.binding
    (dslDefinitionName @@ var "refName" @@ (Names.localNameOf @@ var "refName"))
    (var "body")
    (just (var "ts"))

-- | Generate encode/decode composition builders for a parametric type definition.
-- A bare TypedName token cannot select the right encoder/decoder for a parametric
-- type's arguments (a Name carries no information about which coder to use for a type
-- parameter), so a parametric type gets one small typed builder per direction instead:
-- given an encoder/decoder for each of its forall-bound type parameters, produces an
-- encoder/decoder for the fully-applied type. For a type "ParseResult a" this produces
--   encodeParseResult :: TypedTerm (a -> Term) -> TypedTerm (ParseResult a -> Term)
--   decodeParseResult :: TypedTerm (Graph -> Term -> Either DecodingError a)
--     -> TypedTerm (Graph -> Term -> Either DecodingError (ParseResult a))
-- by partially applying the synthesized (already forall-polymorphic) encoder/decoder
-- binding to the given per-parameter coder arguments. These compose for nesting
-- (encodeValidationResult (encodeParseResult (encodeRef nameName))) and stay at kind
-- *, since each parameter is a single coder argument (no HKT). Non-parametric types
-- (no forall vars) get no builder — generateTypeNameToken's bare TypedName token
-- already suffices for them via hydra.refs's encodeRef/decodeRef.
generateParametricRefBuilders :: TypedTermDefinition (Type -> Name -> [Binding])
generateParametricRefBuilders = define "generateParametricRefBuilders" $
  doc "Generate encode/decode composition builders for a parametric type definition" $
  "origType" ~> "typeName" ~>
  "vars" <~ (collectForallVars @@ var "origType") $
  Logic.ifElse (Lists.null (var "vars"))
    (list ([] :: [TypedTerm Binding]))
    (list [
      generateParametricCoderBuilder ["hydra", "encode"] encoderVarType encoderResultType "encode" (var "origType") (var "typeName"),
      generateParametricCoderBuilder ["hydra", "decode"] decoderVarType decoderVarType "decode" (var "origType") (var "typeName")])
  where
    encoderVarType v = Core.typeFunction $ Core.functionType v (Core.typeVariable (Core.nameLift _Term))
    encoderResultType = encoderVarType
    decoderVarType t =
      Core.typeFunction $ Core.functionType (Core.typeVariable (Core.nameLift _Graph)) $
      Core.typeFunction $ Core.functionType (Core.typeVariable (Core.nameLift _Term)) $
      Core.typeEither $ Core.eitherType (Core.typeVariable (Core.nameLift _DecodingError)) t

-- | Generate one direction's (encode or decode) composition builder for a parametric
-- type, given: the category's namespace segments (for looking up the synthesized
-- per-type coder binding); a function from a bound type variable (or the fully-applied
-- result type) to its coder type; the category's local-name prefix (e.g. "encode"); the
-- type's original (forall-quantified) Type and Name.
generateParametricCoderBuilder :: [String] -> (TypedTerm Type -> TypedTerm Type) -> (TypedTerm Type -> TypedTerm Type) -> String -> TypedTerm Type -> TypedTerm Name -> TypedTerm Binding
generateParametricCoderBuilder categoryPrefix varCoderType resultCoderType categoryLocalPrefix origType typeName =
  "vars" <~ (collectForallVars @@ origType) $
  "localName" <~ (Names.localNameOf @@ typeName) $
  "builderLocalName" <~ (Strings.cat $ list [string categoryLocalPrefix, var "localName"]) $
  "builderName" <~ (dslDefinitionName @@ typeName @@ var "builderLocalName") $
  "refName" <~ (Names.derivedBindingName @@ list (string <$> categoryPrefix) @@ boolean True @@ typeName) $
  -- Parameter (var, TypedTerm<varCoderType>) pairs for the lambda chain, one per forall var.
  "paramPairs" <~ (Lists.map
    ("v" ~> pair (Core.unName (var "v")) (wrapInTypedTerm (varCoderType (Core.typeVariable (var "v")))))
    (var "vars")) $
  -- Body: apply the deep reference to each unwrapped phantom coder argument, left to right.
  "appBody" <~ (Lists.foldl
    ("acc" ~> "pp" ~> deepApplication (var "acc")
      (unwrapTypedTerm (Core.termVariable (Core.name (Pairs.first (var "pp"))))))
    (deepVariable (var "refName"))
    (var "paramPairs")) $
  "builderTerm" <~ (wrapTermInTypedTerm (var "appBody")) $
  "rawBody" <~ (Lists.foldl
    ("acc" ~> "pp" ~>
      Core.termLambda $ Core.lambda (Core.name (Pairs.first (var "pp"))) (just (Pairs.second (var "pp"))) (var "acc"))
    (var "builderTerm")
    (Lists.reverse (var "paramPairs"))) $
  "description" <~ (Strings.cat $ list [
    string "DSL composition builder for the ",
    string categoryLocalPrefix,
    string "r of ",
    Core.unName typeName]) $
  "body" <~ (Annotations.setTermDescription @@ (just (var "description")) @@ var "rawBody") $
  "resultType" <~ (nominalResultType @@ typeName @@ origType) $
  "paramTypes" <~ (Lists.map ("v" ~> varCoderType (Core.typeVariable (var "v"))) (var "vars")) $
  "ts" <~ (dslTypeScheme @@ origType @@ var "paramTypes" @@ (resultCoderType (var "resultType"))) $
  Core.binding
    (var "builderName")
    (var "body")
    (just (var "ts"))

-- | Generate a compile-time name token for a type definition: a TypedName constant
-- tying the type's Name to its host type, so it can be passed to hydra.refs helpers
-- (encodeRef, decodeRef, showRef) without an unsafely bare Name. For a type "Name" in
-- module "hydra.core", produces:
--   nameName :: TypedName Name
--   nameName = TypedName "hydra.core.Name"
generateTypeNameToken :: TypedTermDefinition (Type -> Name -> Binding)
generateTypeNameToken = define "generateTypeNameToken" $
  doc "Generate a TypedName token constant for a type definition" $
  "origType" ~> "typeName" ~>
  "localName" <~ (Names.localNameOf @@ var "typeName") $
  "tokenLocalName" <~ (Strings.cat $ list [
    Formatting.decapitalize @@ var "localName",
    var "localName"]) $
  "tokenName" <~ (dslDefinitionName @@ var "typeName" @@ var "tokenLocalName") $
  "description" <~ (Strings.cat $ list [
    string "DSL name token for ",
    Core.unName (var "typeName")]) $
  "body" <~ (Annotations.setTermDescription @@ (just (var "description")) @@ (wrapNameInTypedName (var "typeName"))) $
  "ts" <~ (Core.typeScheme
    (collectForallVars @@ var "origType")
    (wrapInTypedName (nominalResultType @@ var "typeName" @@ var "origType"))
    nothing) $
  Core.binding
    (var "tokenName")
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

-- | Inject a term into the Term.variable variant
injectTermVariable :: TypedTerm Term -> TypedTerm Term
injectTermVariable t = Core.termInject $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_variable) t)

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
  Logic.ifElse (Equality.equal (Optionals.cases (var "ns") (string "") (reify Packaging.unModuleName)) (string "hydra.typed"))
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

-- | Wrap a type in TypedName: TypeApplication (TypeVariable "hydra.typed.TypedName") innerType
wrapInTypedName :: TypedTerm Type -> TypedTerm Type
wrapInTypedName t = Core.typeApplication $ Core.applicationType (Core.typeVariable (Core.nameLift _TypedName)) t

-- | Wrap a type in TypedTerm: TypeApplication (TypeVariable "hydra.typed.TypedTerm") innerType
wrapInTypedTerm :: TypedTerm Type -> TypedTerm Type
wrapInTypedTerm t = Core.typeApplication $ Core.applicationType (Core.typeVariable (Core.nameLift _TypedTerm)) t

-- | Wrap a name in TypedName: WrappedTerm _TypedName (deep Name term)
wrapNameInTypedName :: TypedTerm Name -> TypedTerm Term
wrapNameInTypedName n = Core.termWrap $ Core.wrappedTerm (Core.nameLift _TypedName) (deepName (Core.unName n))

-- | Wrap a term in TypedTerm: WrappedTerm _TypedTerm term
wrapTermInTypedTerm :: TypedTerm Term -> TypedTerm Term
wrapTermInTypedTerm t = Core.termWrap $ Core.wrappedTerm (Core.nameLift _TypedTerm) t


