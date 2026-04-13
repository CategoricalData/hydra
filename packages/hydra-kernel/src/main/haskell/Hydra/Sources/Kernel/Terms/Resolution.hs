
module Hydra.Sources.Kernel.Terms.Resolution where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  dereferenceType,
  fTypeIsPolymorphic,
  fieldMap,
  fieldTypeMap,
  fieldTypes,
  findFieldType,
  fullyStripAndNormalizeType,
  fullyStripType,
  instantiateType,
  instantiateTypeScheme,
  nominalApplication,
  requireRecordType,
  requireRowType,
  requireSchemaType,
  requireType,
  requireUnionField_,
  requireUnionType,
  resolveType,
  typeToTypeScheme)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Scoping      as Scoping
import qualified Hydra.Sources.Kernel.Terms.Strip        as Strip
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution
import qualified Hydra.Sources.Kernel.Terms.Variables    as Variables
import qualified Hydra.Sources.Kernel.Terms.Names        as Names
import qualified Hydra.Sources.Kernel.Terms.Show.Core    as ShowCore


ns :: Namespace
ns = Namespace "hydra.resolution"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    [Lexical.ns, Names.ns, Scoping.ns, ShowCore.ns, Strip.ns, Substitution.ns, Variables.ns]
    kernelTypesNamespaces $
    Just ("Type dereference, lookup, requirements, and instantiation")
  where
    definitions = [
      toDefinition dereferenceType,
      toDefinition fTypeIsPolymorphic,
      toDefinition fieldMap,
      toDefinition fieldTypeMap,
      toDefinition fieldTypes,
      toDefinition findFieldType,
      toDefinition fullyStripAndNormalizeType,
      toDefinition fullyStripType,
      toDefinition instantiateType,
      toDefinition instantiateTypeScheme,
      toDefinition nominalApplication,
      toDefinition requireRecordType,
      toDefinition requireRowType,
      toDefinition requireSchemaType,
      toDefinition requireType,
      toDefinition requireUnionField_,
      toDefinition requireUnionType,
      toDefinition resolveType,
      toDefinition typeToTypeScheme]

dereferenceType :: TTermDefinition (Context -> Graph -> Name -> Either Error (Maybe Type))
dereferenceType = define "dereferenceType" $
  doc "Dereference a type name to get the actual type (Either version)" $
  "cx" ~> "graph" ~> "name" ~>
  "mel" <~ Lexical.lookupBinding @@ var "graph" @@ var "name" $
  optCases (var "mel")
    (right nothing)
    ("el" ~> Eithers.map (unaryFunction just)
      (Eithers.bimap ("_e" ~> Error.errorResolution $ Error.resolutionErrorUnexpectedShape $ Error.unexpectedShapeError (string "type") (unwrap _DecodingError @@ var "_e")) ("_a" ~> var "_a")
          (decoderFor _Type @@ var "graph" @@ Core.bindingTerm (var "el"))))

fieldMap :: TTermDefinition ([Field] -> M.Map Name Term)
fieldMap = define "fieldMap" $
  "fields" ~>
  "toPair" <~ ("f" ~> pair (Core.fieldName $ var "f") (Core.fieldTerm $ var "f")) $
  Maps.fromList $ Lists.map (var "toPair") (var "fields")

fieldTypeMap :: TTermDefinition ([FieldType] -> M.Map Name Type)
fieldTypeMap = define "fieldTypeMap" $
  "fields" ~>
  "toPair" <~ ("f" ~> pair (Core.fieldTypeName $ var "f") (Core.fieldTypeType $ var "f")) $
  Maps.fromList $ Lists.map (var "toPair") (var "fields")

fieldTypes :: TTermDefinition (Context -> Graph -> Type -> Either Error (M.Map Name Type))
fieldTypes = define "fieldTypes" $
  doc "Get field types from a record or union type (Either version)" $
  "cx" ~> "graph" ~> "t" ~>
  "toMap" <~ ("fields" ~> Maps.fromList (Lists.map
    ("ft" ~> pair (Core.fieldTypeName (var "ft")) (Core.fieldTypeType (var "ft")))
    (var "fields"))) $
  match _Type (Just (Ctx.failInContext (Error.errorResolution $ Error.resolutionErrorUnexpectedShape $
    Error.unexpectedShapeError (string "record or union type") (ShowCore.type_ @@ var "t")) (var "cx"))) [
    _Type_forall>>: "ft" ~> fieldTypes @@ var "cx" @@ var "graph" @@ Core.forallTypeBody (var "ft"),
    _Type_record>>: "rt" ~> right (var "toMap" @@ var "rt"),
    _Type_union>>: "rt" ~> right (var "toMap" @@ var "rt"),
    _Type_variable>>: "name" ~>
      -- Try graphSchemaTypes first (type definitions), then fall back to graphBoundTerms (legacy)
      Maybes.maybe
        (Eithers.bind (Lexical.requireBinding @@ var "graph" @@ var "name") (
          "el" ~>
          Eithers.bind (Eithers.bimap ("_e" ~> Error.errorResolution $ Error.resolutionErrorUnexpectedShape $ Error.unexpectedShapeError (string "type") (unwrap _DecodingError @@ var "_e")) ("_a" ~> var "_a")
              (decoderFor _Type @@ var "graph" @@ Core.bindingTerm (var "el"))) (
            "decodedType" ~> fieldTypes @@ var "cx" @@ var "graph" @@ var "decodedType")))
        ("ts" ~> fieldTypes @@ var "cx" @@ var "graph" @@ Core.typeSchemeType (var "ts"))
        (Maps.lookup (var "name") (Graph.graphSchemaTypes $ var "graph"))]
  @@ (Strip.deannotateType @@ var "t")

findFieldType :: TTermDefinition (Context -> Name -> [FieldType] -> Either Error Type)
findFieldType = define "findFieldType" $
  doc "Find a field type by name in a list of field types" $
  "cx" ~> "fname" ~> "fields" ~>
  "matchingFields" <~ Lists.filter
    ("ft" ~> Equality.equal (Core.unName (Core.fieldTypeName (var "ft"))) (Core.unName (var "fname")))
    (var "fields") $
  Logic.ifElse (Lists.null (var "matchingFields"))
    (Ctx.failInContext (Error.errorResolution $ Error.resolutionErrorNoMatchingField $ Error.noMatchingFieldError (var "fname")) (var "cx"))
    (Logic.ifElse (Equality.equal (Lists.length (var "matchingFields")) (int32 1))
      (right (Core.fieldTypeType (Lists.head (var "matchingFields"))))
      (Ctx.failInContext (Error.errorExtraction $ Error.extractionErrorMultipleFields $ Error.multipleFieldsError (var "fname")) (var "cx")))

fTypeIsPolymorphic :: TTermDefinition (Type -> Bool)
fTypeIsPolymorphic = define "fTypeIsPolymorphic" $
  doc "Test whether a given System F type is polymorphic (i.e., a forall type)" $
  "typ" ~> cases _Type (var "typ")
    (Just false) [
    _Type_annotated>>: "at" ~> fTypeIsPolymorphic @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~> true]

fullyStripAndNormalizeType :: TTermDefinition (Type -> Type)
fullyStripAndNormalizeType = define "fullyStripAndNormalizeType" $
  doc "Fully strip a type of forall quantifiers, normalizing bound variable names for alpha-equivalence comparison" $
  "typ" ~>
  -- Collect forall-bound variables and the body in one pass
  "go" <~ ("depth" ~> "subst" ~> "t" ~> cases _Type (Strip.deannotateType @@ var "t")
      (Just $ pair (var "subst") (var "t")) [
      _Type_forall>>: "ft" ~>
        "oldVar" <~ Core.forallTypeParameter (var "ft") $
        "newVar" <~ Core.name (Strings.cat2 (string "_") (Literals.showInt32 $ var "depth")) $
        var "go"
          @@ (Math.add (var "depth") (int32 1))
          @@ (Maps.insert (var "oldVar") (var "newVar") (var "subst"))
          @@ (Core.forallTypeBody (var "ft"))]) $
  "result" <~ var "go" @@ int32 0 @@ Maps.empty @@ var "typ" $
  "subst" <~ Pairs.first (var "result") $
  "body" <~ Pairs.second (var "result") $
  -- Apply the renaming substitution
  Variables.substituteTypeVariables @@ var "subst" @@ var "body"

fullyStripType :: TTermDefinition (Type -> Type)
fullyStripType = define "fullyStripType" $
  doc "Fully strip a type of forall quantifiers" $
  "typ" ~>
  match _Type (Just (var "typ")) [
    _Type_forall>>: "ft" ~> fullyStripType @@ Core.forallTypeBody (var "ft")]
  @@ (Strip.deannotateType @@ var "typ")

instantiateType :: TTermDefinition (Context -> Type -> (Type, Context))
instantiateType = define "instantiateType" $
  doc "Instantiate a type by replacing all forall-bound type variables with fresh variables, threading Context" $
  "cx" ~> "typ" ~>
  "result" <~ instantiateTypeScheme @@ var "cx" @@ (typeToTypeScheme @@ var "typ") $
  pair (Scoping.typeSchemeToFType @@ Pairs.first (var "result")) (Pairs.second (var "result"))

instantiateTypeScheme :: TTermDefinition (Context -> TypeScheme -> (TypeScheme, Context))
instantiateTypeScheme = define "instantiateTypeScheme" $
  doc "Instantiate a type scheme with fresh variables, threading Context" $
  "cx" ~> "scheme" ~>
  "oldVars" <~ Core.typeSchemeVariables (var "scheme") $
  "result" <~ Names.freshNames @@ Lists.length (var "oldVars") @@ var "cx" $
  "newVars" <~ Pairs.first (var "result") $
  "cx2" <~ Pairs.second (var "result") $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "oldVars") (Lists.map (unaryFunction Core.typeVariable) $ var "newVars")) $
  -- Build a name-to-name substitution for renaming constraint keys
  "nameSubst" <~ Maps.fromList (Lists.zip (var "oldVars") (var "newVars")) $
  -- Rename the keys in the constraints map using the name substitution
  "renamedConstraints" <~ Maybes.map
    ("oldConstraints" ~> Maps.fromList (Lists.map
      ("kv" ~> pair
        (Maybes.fromMaybe (Pairs.first $ var "kv") (Maps.lookup (Pairs.first $ var "kv") (var "nameSubst")))
        (Pairs.second $ var "kv"))
      (Maps.toList $ var "oldConstraints")))
    (Core.typeSchemeConstraints (var "scheme")) $
  pair
    (Core.typeScheme (var "newVars")
      (Substitution.substInType @@ var "subst" @@ Core.typeSchemeType (var "scheme"))
      (var "renamedConstraints"))
    (var "cx2")

nominalApplication :: TTermDefinition (Name -> [Type] -> Type)
nominalApplication = define "nominalApplication" $
  doc "Apply type arguments to a nominal type" $
  "tname" ~> "args" ~>
  Lists.foldl
    ("t" ~> "a" ~> Core.typeApplication $ Core.applicationType (var "t") (var "a"))
    (Core.typeVariable $ var "tname")
    (var "args")

requireRecordType :: TTermDefinition (Context -> Graph -> Name -> Either Error [FieldType])
requireRecordType = define "requireRecordType" $
  doc "Require a name to resolve to a record type" $
  "cx" ~> "graph" ~> "name" ~>
  "toRecord" <~ ("t" ~> cases _Type (var "t") (Just nothing) [
    _Type_record>>: "rt" ~> just (var "rt")]) $
  requireRowType @@ var "cx" @@ string "record type" @@ var "toRecord" @@ var "graph" @@ var "name"

requireRowType :: TTermDefinition (Context -> String -> (Type -> Maybe [FieldType]) -> Graph -> Name -> Either Error [FieldType])
requireRowType = define "requireRowType" $
  doc "Require a name to resolve to a row type" $
  "cx" ~> "label" ~> "getter" ~> "graph" ~> "name" ~>
  "rawType" <~ ("t" ~> cases _Type (var "t") (Just (var "t")) [
    _Type_annotated>>: "at" ~> var "rawType" @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~> var "rawType" @@ Core.forallTypeBody (var "ft")]) $
  Eithers.bind (requireType @@ var "cx" @@ var "graph" @@ var "name") (
    "t" ~>
    Maybes.maybe
      (Ctx.failInContext (Error.errorResolution $ Error.resolutionErrorUnexpectedShape $ Error.unexpectedShapeError
        (Strings.cat2 (var "label") (string " type"))
        (Strings.cat2 (Core.unName (var "name")) (Strings.cat2 (string ": ") (ShowCore.type_ @@ var "t")))) (var "cx"))
      (unaryFunction right)
      (var "getter" @@ (var "rawType" @@ var "t")))

requireSchemaType :: TTermDefinition (Context -> M.Map Name TypeScheme -> Name -> Either Error (TypeScheme, Context))
requireSchemaType = define "requireSchemaType" $
  doc "Look up a schema type and instantiate it, threading Context" $
  "cx" ~> "types" ~> "tname" ~>
  Maybes.maybe
    (left $
      Error.errorResolution $ Error.resolutionErrorNoSuchBinding $ Error.noSuchBindingError (var "tname"))
    ("ts" ~> right $ instantiateTypeScheme @@ var "cx" @@ (Strip.deannotateTypeSchemeRecursive @@ var "ts"))
    (Maps.lookup (var "tname") (var "types"))

requireType :: TTermDefinition (Context -> Graph -> Name -> Either Error Type)
requireType = define "requireType" $
  doc "Require a type by name" $
  "cx" ~> "graph" ~> "name" ~>
  -- Look up in schema types first, then fall back to bound types
  Maybes.maybe
    (Maybes.maybe
      (Ctx.failInContext (Error.errorResolution $ Error.resolutionErrorNoSuchBinding $ Error.noSuchBindingError (var "name")) (var "cx"))
      ("ts" ~> right (Scoping.typeSchemeToFType @@ var "ts"))
      (Maps.lookup (var "name") (Graph.graphBoundTypes (var "graph"))))
    ("ts" ~> right (Scoping.typeSchemeToFType @@ var "ts"))
    (Maps.lookup (var "name") (Graph.graphSchemaTypes (var "graph")))

requireUnionField_ :: TTermDefinition (Context -> Graph -> Name -> Name -> Either Error Type)
requireUnionField_ = define "requireUnionField" $
  doc "Require a field type from a union type" $
  "cx" ~> "graph" ~> "tname" ~> "fname" ~>
  "withRowType" <~ ("rt" ~>
    "matches" <~ (Lists.filter
      ("ft" ~> Equality.equal (Core.fieldTypeName $ var "ft") (var "fname"))
      (var "rt")) $
    Logic.ifElse (Lists.null $ var "matches")
      (Ctx.failInContext (Error.errorResolution $ Error.resolutionErrorNoMatchingField $ Error.noMatchingFieldError (var "fname")) (var "cx"))
      (right $ Core.fieldTypeType $ Lists.head $ var "matches")) $
  Eithers.bind (requireUnionType @@ var "cx" @@ var "graph" @@ var "tname") (var "withRowType")

requireUnionType :: TTermDefinition (Context -> Graph -> Name -> Either Error [FieldType])
requireUnionType = define "requireUnionType" $
  doc "Require a name to resolve to a union type" $
  "cx" ~> "graph" ~> "name" ~>
  "toUnion" <~ ("t" ~> cases _Type (var "t")
    (Just nothing) [
    _Type_union>>: "rt" ~> just (var "rt")]) $
  requireRowType @@ var "cx" @@ string "union" @@ var "toUnion" @@ var "graph" @@ var "name"

resolveType :: TTermDefinition (Graph -> Type -> Maybe Type)
resolveType = define "resolveType" $
  doc "Resolve a type, dereferencing type variables" $
  "graph" ~> "typ" ~>
  match _Type (Just (just (var "typ"))) [
    _Type_variable>>: "name" ~>
      -- Look up in schema types first, then fall back to bound types
      Maybes.maybe
        (Maybes.map ("ts" ~> Scoping.typeSchemeToFType @@ var "ts") (Maps.lookup (var "name") (Graph.graphBoundTypes (var "graph"))))
        ("ts" ~> just (Scoping.typeSchemeToFType @@ var "ts"))
        (Maps.lookup (var "name") (Graph.graphSchemaTypes (var "graph")))]
  @@ (Strip.deannotateType @@ var "typ")

typeToTypeScheme :: TTermDefinition (Type -> TypeScheme)
typeToTypeScheme = define "typeToTypeScheme" $
  doc "Convert a (System F -style) type to a type scheme" $
  "t0" ~>
  "helper" <~ ("vars" ~> "t" ~> cases _Type (Strip.deannotateType @@ var "t")
    (Just $ Core.typeScheme (Lists.reverse $ var "vars") (var "t") Phantoms.nothing) [
    _Type_forall>>: "ft" ~> var "helper"
      @@ (Lists.cons (Core.forallTypeParameter $ var "ft") $ var "vars")
      @@ (Core.forallTypeBody $ var "ft")]) $
  var "helper" @@ list ([] :: [TTerm Name]) @@ var "t0"
