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
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Meta.Phantoms     as Phantoms
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (
  elimination, field, fieldType, floatType, floatValue, function, injection, integerType, integerValue, lambda, literal,
  literalType, record, term, type_, typeScheme, wrap)
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Error        as Error
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
      toBinding isDslEligibleBinding,
      toBinding dslTypeScheme,
      toBinding collectForallVars,
      toBinding nominalResultType]

define :: String -> TTerm x -> TBinding x
define = definitionInModule module_

-- | Wrap a type in TTerm: TypeApplication (TypeVariable "hydra.phantoms.TTerm") innerType
wrapInTTerm :: TTerm Type -> TTerm Type
wrapInTTerm t = Core.typeApplication $ Core.applicationType (Core.typeVariable (Core.nameLift _TTerm)) t

-- | Build a TypeScheme from a list of parameter types and a result type.
-- All types are wrapped in TTerm. Forall variables are collected from the original type.
dslTypeScheme :: TBinding (Type -> [Type] -> Type -> TypeScheme)
dslTypeScheme = define "dslTypeScheme" $
  doc "Build a TypeScheme with TTerm-wrapped parameter and result types" $
  "origType" ~> "paramTypes" ~> "resultType" ~>
  "typeVars" <~ (collectForallVars @@ var "origType") $
  "wrappedResult" <~ (wrapInTTerm (var "resultType")) $
  "funType" <~ (Lists.foldr
    ("paramType" ~> "acc" ~> Core.typeFunction $ Core.functionType (wrapInTTerm (var "paramType")) (var "acc"))
    (var "wrappedResult")
    (var "paramTypes")) $
  Core.typeScheme (var "typeVars") (var "funType") nothing

-- | Collect forall type variables from a type (stripping annotations)
collectForallVars :: TBinding (Type -> [Name])
collectForallVars = define "collectForallVars" $
  doc "Collect forall type variable names from a type" $
  "typ" ~> cases _Type (var "typ") (Just $ list ([] :: [TTerm Name])) [
    _Type_annotated>>: "at" ~>
      collectForallVars @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~>
      Lists.cons (Core.forallTypeParameter (var "ft"))
        (collectForallVars @@ Core.forallTypeBody (var "ft"))]

-- | Build the nominal result type for a type definition.
-- For non-polymorphic types: TypeVariable typeName
-- For polymorphic types like (forall n. Namespaces n): TypeApplication (TypeVariable typeName) (TypeVariable n)
nominalResultType :: TBinding (Name -> Type -> Type)
nominalResultType = define "nominalResultType" $
  doc "Build the nominal result type with type applications for forall variables" $
  "typeName" ~> "origType" ~>
  "vars" <~ (collectForallVars @@ var "origType") $
  Lists.foldl
    ("acc" ~> "v" ~> Core.typeApplication $ Core.applicationType (var "acc") (Core.typeVariable (var "v")))
    (Core.typeVariable (var "typeName"))
    (var "vars")

-- | Inject a Record-typed term into the Term.record variant
-- Produces TermUnion(Injection _Term (Field _Term_record innerRecord))
injectTermRecord :: TTerm Term -> TTerm Term
injectTermRecord t = Core.termUnion $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_record) t)

-- | Inject a term into the Term.function variant
injectTermFunction :: TTerm Term -> TTerm Term
injectTermFunction t = Core.termUnion $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_function) t)

-- | Inject a term into the Term.application variant
injectTermApplication :: TTerm Term -> TTerm Term
injectTermApplication t = Core.termUnion $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_application) t)

-- | Inject a term into the Term.union variant
injectTermUnion :: TTerm Term -> TTerm Term
injectTermUnion t = Core.termUnion $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_union) t)

-- | Inject a term into the Term.wrap variant
injectTermWrap :: TTerm Term -> TTerm Term
injectTermWrap t = Core.termUnion $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_wrap) t)

-- | Inject into Function.elimination
injectFunctionElimination :: TTerm Term -> TTerm Term
injectFunctionElimination t = Core.termUnion $ Core.injection (Core.nameLift _Function) (Core.field (Core.nameLift _Function_elimination) t)

-- | Inject into Elimination.record
injectEliminationRecord :: TTerm Term -> TTerm Term
injectEliminationRecord t = Core.termUnion $ Core.injection (Core.nameLift _Elimination) (Core.field (Core.nameLift _Elimination_record) t)

-- | Inject into Elimination.wrap
injectEliminationWrap :: TTerm Term -> TTerm Term
injectEliminationWrap t = Core.termUnion $ Core.injection (Core.nameLift _Elimination) (Core.field (Core.nameLift _Elimination_wrap) t)

-- | Build a deep Name: TermWrap(_Name, TermLiteral(LiteralString(s)))
deepName :: TTerm String -> TTerm Term
deepName s = Core.termWrap $ Core.wrappedTerm (Core.nameLift _Name) (Core.termLiteral $ Core.literalString s)

-- | Build a deep Projection as a Term value
deepProjection :: TTerm Name -> TTerm Name -> TTerm Term
deepProjection typeName fieldName =
  injectTermFunction $ injectFunctionElimination $ injectEliminationRecord $
    Core.termRecord $ Core.record (Core.nameLift _Projection) (list [
      Core.field (Core.nameLift _Projection_typeName) (deepName (Core.unName typeName)),
      Core.field (Core.nameLift _Projection_field) (deepName (Core.unName fieldName))])

-- | Build a deep Application as a Term value
deepApplication :: TTerm Term -> TTerm Term -> TTerm Term
deepApplication fun arg =
  injectTermApplication $
    Core.termRecord $ Core.record (Core.nameLift _Application) (list [
      Core.field (Core.nameLift _Application_function) fun,
      Core.field (Core.nameLift _Application_argument) arg])

-- | Build a deep Field record
deepField :: TTerm Name -> TTerm Term -> TTerm Term
deepField name term =
  Core.termRecord $ Core.record (Core.nameLift _Field) (list [
    Core.field (Core.nameLift _Field_name) (deepName (Core.unName name)),
    Core.field (Core.nameLift _Field_term) term])

-- | Build a deep Record as a Term value (injected into Term.record)
deepRecord :: TTerm Name -> TTerm [Term] -> TTerm Term
deepRecord typeName fields =
  injectTermRecord $
    Core.termRecord $ Core.record (Core.nameLift _Record) (list [
      Core.field (Core.nameLift _Record_typeName) (deepName (Core.unName typeName)),
      Core.field (Core.nameLift _Record_fields) (Core.termList fields)])

-- | Build a deep Injection as a Term value (injected into Term.union)
deepInjection :: TTerm Name -> TTerm Term -> TTerm Term
deepInjection typeName fieldTerm =
  injectTermUnion $
    Core.termRecord $ Core.record (Core.nameLift _Injection) (list [
      Core.field (Core.nameLift _Injection_typeName) (deepName (Core.unName typeName)),
      Core.field (Core.nameLift _Injection_field) fieldTerm])

-- | Build a deep WrappedTerm as a Term value (injected into Term.wrap)
deepWrap :: TTerm Name -> TTerm Term -> TTerm Term
deepWrap typeName body =
  injectTermWrap $
    Core.termRecord $ Core.record (Core.nameLift _WrappedTerm) (list [
      Core.field (Core.nameLift _WrappedTerm_typeName) (deepName (Core.unName typeName)),
      Core.field (Core.nameLift _WrappedTerm_body) body])

-- | Build a deep EliminationWrap as a Term.function value
deepUnwrap :: TTerm Name -> TTerm Term
deepUnwrap typeName =
  injectTermFunction $ injectFunctionElimination $ injectEliminationWrap $
    deepName (Core.unName typeName)

-- | Unwrap a TTerm argument: apply (EliminationWrap _TTerm) to the variable
unwrapTTerm :: TTerm Term -> TTerm Term
unwrapTTerm v = Core.termApplication $ Core.application
  (Core.termFunction $ Core.functionElimination $ Core.eliminationWrap (Core.nameLift _TTerm))
  v

-- | Wrap a term in TTerm: WrappedTerm _TTerm term
wrapTermInTTerm :: TTerm Term -> TTerm Term
wrapTermInTTerm t = Core.termWrap $ Core.wrappedTerm (Core.nameLift _TTerm) t



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
-- For a record type like {body: Term, annotation: Map(Name, Term)},
-- produces a deep (meta) term:
--   \body -> \annotation -> TermRecord (Record "hydra.core.AnnotatedTerm" [Field "body" body, ...])
-- When code-generated into Haskell, this becomes:
--   annotatedTerm body annotation = Core.TermRecord (Core.Record { ... })
generateRecordConstructor :: TBinding (Type -> Name -> RowType -> [Binding])
generateRecordConstructor = define "generateRecordConstructor" $
  doc "Generate a record constructor function" $
  "origType" ~> "typeName" ~> "rt" ~>
  "fieldTypes" <~ (Core.rowTypeFields $ var "rt") $
  -- Build deep fields and record using helpers
  "dFields" <~ (Lists.map
    ("ft" ~> deepField (Core.fieldTypeName (var "ft"))
      (unwrapTTerm (Core.termVariable (Core.name (Names.localNameOf @@ Core.fieldTypeName (var "ft"))))))
    (var "fieldTypes")) $
  "recordTerm" <~ (wrapTermInTTerm (deepRecord (var "typeName") (var "dFields"))) $
  -- Build (paramName, TTerm<fieldType>) pairs for lambda construction
  "paramPairs" <~ (Lists.map
    ("ft" ~> pair
      (Names.localNameOf @@ Core.fieldTypeName (var "ft"))
      (wrapInTTerm (Core.fieldTypeType (var "ft"))))
    (var "fieldTypes")) $
  -- Wrap in typed lambdas for each parameter (right to left)
  "body" <~ (Lists.foldl
    ("acc" ~> "pp" ~>
      Core.termFunction $ Core.functionLambda $
        Core.lambda (Core.name (Pairs.first (var "pp"))) (just (Pairs.second (var "pp"))) (var "acc"))
    (var "recordTerm")
    (Lists.reverse (var "paramPairs"))) $
  -- Type: TTerm<FieldType1> -> TTerm<FieldType2> -> ... -> TTerm<RecordType>
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
generateRecordAccessor :: TBinding (Type -> Name -> FieldType -> Binding)
generateRecordAccessor = define "generateRecordAccessor" $
  doc "Generate a record field accessor function" $
  "origType" ~> "typeName" ~> "ft" ~>
  "fieldName" <~ (Core.fieldTypeName (var "ft")) $
  "accessorLocalName" <~ (Strings.cat $ list [
    Formatting.decapitalize @@ (Names.localNameOf @@ var "typeName"),
    Formatting.capitalize @@ (Names.localNameOf @@ var "fieldName")]) $
  "accessorName" <~ (dslElementName @@ var "typeName" @@ var "accessorLocalName") $
  -- Body: projection as a simple elimination
  "paramDomain" <~ (wrapInTTerm (nominalResultType @@ var "typeName" @@ var "origType")) $
  "body" <~ (Core.termFunction $ Core.functionLambda $
    Core.lambda (Core.name (string "x")) (just (var "paramDomain")) $
      wrapTermInTTerm (deepApplication
        (deepProjection (var "typeName") (var "fieldName"))
        (unwrapTTerm (Core.termVariable (Core.name (string "x")))))) $
  "ts" <~ (dslTypeScheme @@ var "origType"
    @@ list [nominalResultType @@ var "typeName" @@ var "origType"]
    @@ Core.fieldTypeType (var "ft")) $
  Core.binding
    (var "accessorName")
    (var "body")
    (just (var "ts"))

-- | Generate a "withXxx" record field updater function.
-- For a field "name" in record type "Binding" (with fields name, term, type), produces:
--   bindingWithName :: Binding -> Name -> Binding
--   bindingWithName b newName = Binding newName (bindingTerm b) (bindingType b)
-- This constructs a new record with the specified field replaced and all others projected.
generateRecordWithUpdater :: TBinding (Type -> Name -> [FieldType] -> FieldType -> Binding)
generateRecordWithUpdater = define "generateRecordWithUpdater" $
  doc "Generate a withXxx record field updater function" $
  "origType" ~> "typeName" ~> "allFields" ~> "targetField" ~>
  "targetFieldName" <~ (Core.fieldTypeName (var "targetField")) $
  -- Build the updater name: e.g., "bindingWithName"
  "updaterLocalName" <~ (Strings.cat $ list [
    Formatting.decapitalize @@ (Names.localNameOf @@ var "typeName"),
    string "With",
    Formatting.capitalize @@ (Names.localNameOf @@ var "targetFieldName")]) $
  "updaterName" <~ (dslElementName @@ var "typeName" @@ var "updaterLocalName") $
  -- Build deep fields: project from unwrapped original, except target uses unwrapped newVal
  "dFields" <~ (Lists.map
    ("ft" ~> deepField (Core.fieldTypeName (var "ft"))
      (Logic.ifElse (Equality.equal
        (Core.unName (Core.fieldTypeName (var "ft")))
        (Core.unName (var "targetFieldName")))
        (unwrapTTerm (Core.termVariable (Core.name (string "newVal"))))
        (deepApplication
          (deepProjection (var "typeName") (Core.fieldTypeName (var "ft")))
          (unwrapTTerm (Core.termVariable (Core.name (string "original")))))))
    (var "allFields")) $
  "recDomain" <~ (wrapInTTerm (nominalResultType @@ var "typeName" @@ var "origType")) $
  "fieldDomain" <~ (wrapInTTerm (Core.fieldTypeType (var "targetField"))) $
  "body" <~ (
    Core.termFunction $ Core.functionLambda $ Core.lambda (Core.name (string "original")) (just (var "recDomain")) $
    Core.termFunction $ Core.functionLambda $ Core.lambda (Core.name (string "newVal")) (just (var "fieldDomain")) $
    wrapTermInTTerm (deepRecord (var "typeName") (var "dFields"))) $
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
generateUnionInjector :: TBinding (Type -> Name -> FieldType -> Binding)
generateUnionInjector = define "generateUnionInjector" $
  doc "Generate a union injection helper" $
  "origType" ~> "typeName" ~> "ft" ~>
  "fieldName" <~ (Core.fieldTypeName (var "ft")) $
  "fieldType" <~ (Core.fieldTypeType (var "ft")) $
  -- Build the injector name: e.g., "functionLambda" or "comparisonLessThan"
  "injectorLocalName" <~ (Strings.cat $ list [
    Formatting.decapitalize @@ (Names.localNameOf @@ var "typeName"),
    Formatting.capitalize @@ (Names.localNameOf @@ var "fieldName")]) $
  "injectorName" <~ (dslElementName @@ var "typeName" @@ var "injectorLocalName") $
  -- Check if field type is unit (for unit variants like enum members)
  "isUnit" <~ (isUnitType_ @@ var "fieldType") $
  -- Build simple injection
  -- Build deep injection: deepField for the variant, then deepInjection into the union type
  "dFieldValue" <~ (Logic.ifElse (var "isUnit")
    (Core.termUnion $ Core.injection (Core.nameLift _Term) (Core.field (Core.nameLift _Term_unit) Core.termUnit))
    (unwrapTTerm (Core.termVariable (Core.name (string "x"))))) $
  "injectionTerm" <~ (wrapTermInTTerm (deepInjection (var "typeName") (deepField (var "fieldName") (var "dFieldValue")))) $
  -- For non-unit variants, wrap in a typed lambda; for unit, it's a constant TTerm
  "variantDomain" <~ (wrapInTTerm (Core.fieldTypeType (var "ft"))) $
  "body" <~ (Logic.ifElse (var "isUnit")
    (var "injectionTerm")
    (Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "x")) (just (var "variantDomain")) (var "injectionTerm"))) $
  "unionType" <~ (nominalResultType @@ var "typeName" @@ var "origType") $
  "ts" <~ (Logic.ifElse (var "isUnit")
    (dslTypeScheme @@ var "origType" @@ list ([] :: [TTerm Type]) @@ var "unionType")
    (dslTypeScheme @@ var "origType" @@ list [Core.fieldTypeType (var "ft")] @@ var "unionType")) $
  Core.binding
    (var "injectorName")
    (var "body")
    (just (var "ts"))

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
generateWrappedTypeAccessors :: TBinding (Type -> Name -> WrappedType -> [Binding])
generateWrappedTypeAccessors = define "generateWrappedTypeAccessors" $
  doc "Generate wrap/unwrap accessors for a wrapped type" $
  "origType" ~> "typeName" ~> "wt" ~>
  "localName" <~ (Names.localNameOf @@ var "typeName") $
  -- Wrap function: decapitalized type name
  "wrapName" <~ (dslElementName @@ var "typeName" @@ (Formatting.decapitalize @@ var "localName")) $
  -- Unwrap function: "un" + type local name
  "unwrapLocalName" <~ (Strings.cat $ list [string "un", var "localName"]) $
  "unwrapName" <~ (dslElementName @@ var "typeName" @@ var "unwrapLocalName") $
  "innerType" <~ (Core.wrappedTypeBody (var "wt")) $
  "wrapperType" <~ (nominalResultType @@ var "typeName" @@ var "origType") $
  -- Wrap: \(x :: TTerm<InnerType>) -> WrappedTerm typeName x
  "wrapDomain" <~ (wrapInTTerm (var "innerType")) $
  "wrapBody" <~ (
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "x")) (just (var "wrapDomain")) $
        wrapTermInTTerm (deepWrap (var "typeName")
          (unwrapTTerm (Core.termVariable (Core.name (string "x")))))) $
  -- Unwrap: \(x :: TTerm<WrapperType>) -> TTerm(apply (unwrap typeName) (unTTerm x))
  "unwrapDomain" <~ (wrapInTTerm (var "wrapperType")) $
  "unwrapBody" <~ (
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "x")) (just (var "unwrapDomain")) $
        wrapTermInTTerm (deepApplication
          (deepUnwrap (var "typeName"))
          (unwrapTTerm (Core.termVariable (Core.name (string "x")))))) $
  "wrapTs" <~ (dslTypeScheme @@ var "origType" @@ list [var "innerType"] @@ var "wrapperType") $
  "unwrapTs" <~ (dslTypeScheme @@ var "origType" @@ list [var "wrapperType"] @@ var "innerType") $
  list [
    Core.binding (var "wrapName") (var "wrapBody") (just (var "wrapTs")),
    Core.binding (var "unwrapName") (var "unwrapBody") (just (var "unwrapTs"))]

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
          generateRecordConstructor @@ var "rawType" @@ var "typeName" @@ var "rt",
          Lists.map (generateRecordAccessor @@ var "rawType" @@ var "typeName") (Core.rowTypeFields (var "rt")),
          Lists.map (generateRecordWithUpdater @@ var "rawType" @@ var "typeName" @@ Core.rowTypeFields (var "rt"))
            (Core.rowTypeFields (var "rt"))],
      _Type_union>>: "rt" ~>
        Lists.map (generateUnionInjector @@ var "rawType" @@ var "typeName") (Core.rowTypeFields (var "rt")),
      _Type_wrap>>: "wt" ~>
        generateWrappedTypeAccessors @@ var "rawType" @@ var "typeName" @@ var "wt"]))

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

-- | Check if a binding is eligible for DSL generation.
-- Excludes phantom types (TTerm, TBinding) since they are meta-infrastructure.
isDslEligibleBinding :: TBinding (Context -> Graph -> Binding -> Either (InContext Error) (Maybe Binding))
isDslEligibleBinding = define "isDslEligibleBinding" $
  doc "Check if a binding is eligible for DSL generation" $
  "cx" ~> "graph" ~> "b" ~>
  "ns" <~ (Names.namespaceOf @@ Core.bindingName (var "b")) $
  Logic.ifElse (Equality.equal (Maybes.maybe (string "") (unaryFunction Module.unNamespace) (var "ns")) (string "hydra.phantoms"))
    (right nothing)
    (right (just (var "b")))

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
          ("ic" ~> Ctx.inContext (Error.errorOther $ Error.otherError (unwrap _DecodingError @@ Ctx.inContextObject (var "ic"))) (Ctx.inContextContext (var "ic")))
          ("x" ~> var "x")
          (generateBindingsForType @@ var "cx" @@ var "graph" @@ var "b")) (var "typeBindings") $
        right (just (Module.module_
          (dslNamespace @@ (Module.moduleNamespace (var "mod")))
          (deduplicateBindings @@ Lists.concat (var "dslBindings"))
          -- DSL modules depend on DSL modules for type dependencies (to reference other types' DSL functions)
          (Lists.nub (primitive _lists_map @@ dslNamespace @@ (Module.moduleTypeDependencies (var "mod"))))
          -- Type dependencies: the original module + its type deps + hydra.phantoms (for TTerm)
          (Lists.nub (Lists.concat2
            (list [Module.moduleNamespace (var "mod"), Module.namespace (string "hydra.phantoms")])
            (Module.moduleTypeDependencies (var "mod"))))
          (just (Strings.cat $ list [
            string "DSL functions for ",
            Module.unNamespace (Module.moduleNamespace (var "mod"))])))))
