{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Schemas where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Encode.Core as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Sorting as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.schemas") elements
    [Annotations.module_, Constants.module_, DecodeCore.module_, EncodeCore.module_, Names.module_, Rewriting.module_,
      ShowCore.module_, Sorting.module_, Substitution.module_, Variants.module_]
    kernelTypesModules $
    Just ("Various functions for dereferencing and decoding schema types.")
  where
    elements = [
      el definitionDependencyNamespacesDef,
      el dependencyNamespacesDef,
      el dereferenceTypeDef,
      el elementAsTypeApplicationTermDef,
      el elementsWithDependenciesDef,
      el extendTypeContextForLambdaDef,
      el extendTypeContextForLetDef,
      el extendTypeContextForTypeLambdaDef,
      el fieldMapDef,
      el fieldTypeMapDef,
      el findFieldTypeDef,
      el fieldTypesDef,
      el fTypeToTypeSchemeDef,
      el freshNameDef,
      el freshNamesDef,
      el fullyStripTypeDef,
      el graphAsTermDef,
      el graphAsTypesDef,
      el instantiateTypeDef,
      el instantiateTypeSchemeDef,
      el isEnumRowTypeDef,
      el isEnumTypeDef,
      el isSerializableDef,
      el moduleDependencyNamespacesDef,
      el namespacesForDefinitionsDef,
      el nominalApplicationDef,
      el normalTypeVariableDef,
      el requireRecordTypeDef,
      el requireRowTypeDef,
      el requireSchemaTypeDef,
      el requireTypeDef,
      el requireUnionTypeDef,
      el resolveTypeDef,
      el schemaGraphToTypingEnvironmentDef,
      el termAsGraphDef,
      el topologicalSortTypeDefinitionsDef,
      el typeDependenciesDef,
      el typeSchemeToFTypeDef,
      el typeToTypeSchemeDef,
      el typesToElementsDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

definitionDependencyNamespacesDef :: TBinding ([Definition] -> S.Set Namespace)
definitionDependencyNamespacesDef = define "definitionDependencyNamespaces" $
  doc "Get dependency namespaces from definitions" $
  "defs" ~>
  "defNames" <~ ("def" ~>
    match _Definition Nothing [
      _Definition_type>>: "typeDef" ~>
        ref Rewriting.typeDependencyNamesDef @@ true @@ Module.typeDefinitionType (var "typeDef"),
      _Definition_term>>: "termDef" ~>
        ref Rewriting.termDependencyNamesDef @@ true @@ true @@ true @@ Module.termDefinitionTerm (var "termDef")]
    @@ var "def") $
  "allNames" <~ Sets.unions (Lists.map (var "defNames") (var "defs")) $
  Sets.fromList (Optionals.cat (Lists.map (ref Names.namespaceOfDef) (Sets.toList (var "allNames"))))

dependencyNamespacesDef :: TBinding (Bool -> Bool -> Bool -> Bool -> [Binding] -> Flow Graph (S.Set Namespace))
dependencyNamespacesDef = define "dependencyNamespaces" $
  doc "Find dependency namespaces in all of a set of terms" $
  "binds" ~> "withPrims" ~> "withNoms" ~> "withSchema" ~> "els" ~>
  "depNames" <~ ("el" ~>
    "term" <~ Core.bindingTerm (var "el") $
    "dataNames" <~ ref Rewriting.termDependencyNamesDef @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "term" $
    "schemaNames" <~ Logic.ifElse (var "withSchema")
      (Optionals.maybe Sets.empty
        ("ts" ~> ref Rewriting.typeDependencyNamesDef @@ true @@ Core.typeSchemeType (var "ts"))
        (Core.bindingType (var "el")))
      Sets.empty $
    Logic.ifElse (ref EncodeCore.isEncodedTypeDef @@ (ref Rewriting.deannotateTermDef @@ var "term"))
      (Flows.bind (ref DecodeCore.typeDef @@ var "term") (
        "typ" ~> Flows.pure (Sets.unions (list [
          var "dataNames", var "schemaNames",
          ref Rewriting.typeDependencyNamesDef @@ true @@ var "typ"]))))
      (Flows.pure (Sets.unions (list [var "dataNames", var "schemaNames"])))) $
  Flows.bind (Flows.mapList (var "depNames") (var "els")) (
    "namesList" ~> Flows.pure (Sets.fromList (Optionals.cat (Lists.map (ref Names.namespaceOfDef) (
      Sets.toList (Sets.delete (ref Constants.placeholderNameDef) (Sets.unions (var "namesList"))))))))

dereferenceTypeDef :: TBinding (Name -> Flow Graph (Maybe Type))
dereferenceTypeDef = define "dereferenceType" $
  doc "Dereference a type name to get the actual type" $
  "name" ~>
  "mel" <<~ ref Lexical.dereferenceElementDef @@ var "name" $
  optCases (var "mel")
    (Flows.pure nothing)
    ("el" ~> Flows.map (unaryFunction just) $ ref DecodeCore.typeDef @@ Core.bindingTerm (var "el"))

elementAsTypeApplicationTermDef :: TBinding (Binding -> Flow Graph TypeApplicationTerm)
elementAsTypeApplicationTermDef = define "elementAsTypeApplicationTerm" $
  doc "Convert an element to a typed term" $
  "el" ~>
  Optionals.maybe (Flows.fail (string "missing element type"))
    ("ts" ~> Flows.pure (Core.typeApplicationTerm (Core.bindingTerm (var "el")) (Core.typeSchemeType (var "ts"))))
    (Core.bindingType (var "el"))

elementsWithDependenciesDef :: TBinding ([Binding] -> Flow Graph [Binding])
elementsWithDependenciesDef = define "elementsWithDependencies" $
  doc "Get elements with their dependencies" $
  "original" ~>
  "depNames" <~ ("el" ~> Sets.toList (ref Rewriting.termDependencyNamesDef @@ true @@ false @@ false @@ (Core.bindingTerm (var "el")))) $
  "allDepNames" <~ Lists.nub (Lists.concat2
    (Lists.map (unaryFunction Core.bindingName) (var "original"))
    (Lists.concat (Lists.map (var "depNames") (var "original")))) $
  Flows.mapList (ref Lexical.requireElementDef) (var "allDepNames")

extendTypeContextForLambdaDef :: TBinding (TypeContext -> Lambda -> TypeContext)
extendTypeContextForLambdaDef = define "extendTypeContextForLambda" $
  doc "Extend a type context by descending into a System F lambda body" $
  "tcontext" ~> "lam" ~>
  "var" <~ Core.lambdaParameter (var "lam") $
  "dom" <~ Optionals.fromJust (Core.lambdaDomain (var "lam")) $
  Typing.typeContextWithTypes
    (var "tcontext")
    (Maps.insert (var "var") (var "dom") (Typing.typeContextTypes (var "tcontext")))

extendTypeContextForLetDef :: TBinding (TypeContext -> Let -> TypeContext)
extendTypeContextForLetDef = define "extendTypeContextForLet" $
  doc "Extend a type context by descending into a let body" $
  "tcontext" ~> "letrec" ~>
  "bindings" <~ Core.letBindings (var "letrec") $
  Typing.typeContextWithTypes
    (var "tcontext")
    (Maps.union
      (Typing.typeContextTypes (var "tcontext"))
      (Maps.fromList $ Lists.map
        ("b" ~> pair
          (Core.bindingName $ var "b")
          (ref typeSchemeToFTypeDef @@ (Optionals.fromJust $ Core.bindingType $ var "b")))
        (var "bindings")))

extendTypeContextForTypeLambdaDef :: TBinding (TypeContext -> TypeLambda -> TypeContext)
extendTypeContextForTypeLambdaDef = define "extendTypeContextForTypeLambda" $
  doc "Extend a type context by descending into a System F type lambda body" $
  "tcontext" ~> "tlam" ~>
  "name" <~ Core.typeLambdaParameter (var "tlam") $
  Typing.typeContextWithVariables
    (var "tcontext")
    (Sets.insert (var "name") (Typing.typeContextVariables (var "tcontext")))

fieldMapDef :: TBinding ([Field] -> M.Map Name Term)
fieldMapDef = define "fieldMap" $
  "fields" ~>
  "toPair" <~ ("f" ~> pair (Core.fieldName $ var "f") (Core.fieldTerm $ var "f")) $
  Maps.fromList $ Lists.map (var "toPair") (var "fields")

fieldTypeMapDef :: TBinding ([FieldType] -> M.Map Name Type)
fieldTypeMapDef = define "fieldTypeMap" $
  "fields" ~> 
  "toPair" <~ ("f" ~> pair (Core.fieldTypeName $ var "f") (Core.fieldTypeType $ var "f")) $
  Maps.fromList $ Lists.map (var "toPair") (var "fields")

fieldTypesDef :: TBinding (Type -> Flow Graph (M.Map Name Type))
fieldTypesDef = define "fieldTypes" $
  doc "Get field types from a record or union type" $
  "t" ~>
  "toMap" <~ ("fields" ~> Maps.fromList (Lists.map
    ("ft" ~> pair (Core.fieldTypeName (var "ft")) (Core.fieldTypeType (var "ft")))
    (var "fields"))) $
  match _Type (Just (ref Monads.unexpectedDef @@ string "record or union type" @@ (ref ShowCore.typeDef @@ var "t"))) [
    _Type_forall>>: "ft" ~> ref fieldTypesDef @@ Core.forallTypeBody (var "ft"),
    _Type_record>>: "rt" ~> Flows.pure (var "toMap" @@ Core.rowTypeFields (var "rt")),
    _Type_union>>: "rt" ~> Flows.pure (var "toMap" @@ Core.rowTypeFields (var "rt")),
    _Type_variable>>: "name" ~>
      trace (Strings.cat2 (string "field types of ") (Core.unName (var "name"))) (
      Flows.bind (ref Lexical.requireElementDef @@ var "name") (
        "el" ~>
        Flows.bind (ref DecodeCore.typeDef @@ Core.bindingTerm (var "el")) (
          ref fieldTypesDef)))]
  @@ (ref Rewriting.deannotateTypeDef @@ var "t")

findFieldTypeDef :: TBinding (Name -> [FieldType] -> Flow s Type)
findFieldTypeDef = define "findFieldType" $
  doc "Find a field type by name in a list of field types" $
  "fname" ~> "fields" ~>
  "matchingFields" <~ Lists.filter
    ("ft" ~> Equality.equal (Core.unName (Core.fieldTypeName (var "ft"))) (Core.unName (var "fname")))
    (var "fields") $
  Logic.ifElse (Lists.null (var "matchingFields"))
    (Flows.fail (Strings.cat2 (string "No such field: ") (Core.unName (var "fname"))))
    (Logic.ifElse (Equality.equal (Lists.length (var "matchingFields")) (int32 1))
      (Flows.pure (Core.fieldTypeType (Lists.head (var "matchingFields"))))
      (Flows.fail (Strings.cat2 (string "Multiple fields named ") (Core.unName (var "fname")))))

fTypeToTypeSchemeDef :: TBinding (Type -> TypeScheme)
fTypeToTypeSchemeDef = define "fTypeToTypeScheme" $
  doc "Convert a forall type to a type scheme" $
  "typ" ~>
  "gatherForall" <~ ("vars" ~> "typ" ~> cases _Type (ref Rewriting.deannotateTypeDef @@ var "typ")
     (Just $ Core.typeScheme (Lists.reverse $ var "vars") (var "typ")) [
     _Type_forall>>: "ft" ~> var "gatherForall" @@
       (Lists.cons (Core.forallTypeParameter $ var "ft") (var "vars")) @@
       (Core.forallTypeBody $ var "ft")]) $
  var "gatherForall" @@ list [] @@ var "typ"

freshNameDef :: TBinding (Flow s Name)
freshNameDef = define "freshName" $
  doc "Generate a fresh type variable name" $
  Flows.map (ref normalTypeVariableDef) (ref Annotations.nextCountDef @@ ref Constants.key_freshTypeVariableCountDef)

freshNamesDef :: TBinding (Int -> Flow s [Name])
freshNamesDef = define "freshNames" $
  doc "Generate multiple fresh type variable names" $
  "n" ~> Flows.sequence $ Lists.replicate (var "n") (ref freshNameDef)

fullyStripTypeDef :: TBinding (Type -> Type)
fullyStripTypeDef = define "fullyStripType" $
  doc "Fully strip a type of forall quantifiers" $
  "typ" ~>
  match _Type (Just (var "typ")) [
    _Type_forall>>: "ft" ~> ref fullyStripTypeDef @@ Core.forallTypeBody (var "ft")]
  @@ (ref Rewriting.deannotateTypeDef @@ var "typ")

graphAsTermDef :: TBinding (Graph -> Term)
graphAsTermDef = define "graphAsTerm" $
  doc "Convert a graph to a term, taking advantage of the built-in duality between graphs and terms" $
  "g" ~>
  "toBinding" <~ ("el" ~>
    "name" <~ Core.bindingName (var "el") $
    "term" <~ Core.bindingTerm (var "el") $
    "mts" <~ Core.bindingType (var "el") $
    Core.binding (var "name") (var "term") (var "mts")) $
  Core.termLet $ Core.let_
    (Lists.map (var "toBinding") (Maps.elems $ Graph.graphElements (var "g")))
    (Graph.graphBody (var "g"))

graphAsTypesDef :: TBinding (Graph -> Flow s (M.Map Name Type))
graphAsTypesDef = define "graphAsTypes" $
  doc "Decode a schema graph which encodes a set of named types" $
  "sg" ~>
  "els" <~ Maps.elems (Graph.graphElements (var "sg")) $
  "toPair" <~ ("el" ~>
    "typ" <<~ ref DecodeCore.typeDef @@ (Core.bindingTerm $ var "el") $
    produce $ pair (Core.bindingName $ var "el") (var "typ")) $
  "pairs" <<~ Flows.mapList (var "toPair") (var "els") $
  produce $ Maps.fromList $ var "pairs"

instantiateTypeDef :: TBinding (Type -> Flow s Type)
instantiateTypeDef = define "instantiateType" $
  doc "Instantiate a type by replacing all forall-bound type variables with fresh variables" $
  "typ" ~>
  "ts" <<~ ref instantiateTypeSchemeDef @@ (ref typeToTypeSchemeDef @@ var "typ") $
  produce $ ref typeSchemeToFTypeDef @@ var "ts"

instantiateTypeSchemeDef :: TBinding (TypeScheme -> Flow s TypeScheme)
instantiateTypeSchemeDef = define "instantiateTypeScheme" $
  doc "Instantiate a type scheme with fresh variables" $
  "scheme" ~>
  "oldVars" <~ Core.typeSchemeVariables (var "scheme") $
  "newVars" <<~ ref freshNamesDef @@ Lists.length (var "oldVars") $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "oldVars") (Lists.map (unaryFunction Core.typeVariable) $ var "newVars")) $
  produce $ Core.typeScheme (var "newVars") $
    ref Substitution.substInTypeDef @@ var "subst" @@ Core.typeSchemeType (var "scheme")

isEnumRowTypeDef :: TBinding (RowType -> Bool)
isEnumRowTypeDef = define "isEnumRowType" $
  doc "Check if a row type represents an enum (all fields are unit-typed)" $
  "rt" ~> Lists.foldl (binaryFunction Logic.and) true (
    Lists.map ("f" ~> ref EncodeCore.isUnitTypeDef @@ (Core.fieldTypeType (var "f"))) (
      Core.rowTypeFields (var "rt")))

isEnumTypeDef :: TBinding (Type -> Bool)
isEnumTypeDef = define "isEnumType" $
  doc "Check if a type is an enum type" $
  "typ" ~>
  match _Type (Just false) [
    _Type_union>>: "rt" ~> ref isEnumRowTypeDef @@ var "rt"]
  @@ (ref Rewriting.deannotateTypeDef @@ var "typ")

isSerializableDef :: TBinding (Binding -> Flow Graph Bool)
isSerializableDef = define "isSerializable" $
  doc "Check if an element is serializable (no function types in dependencies)" $
  "el" ~>
  "variants" <~ ("typ" ~>
    Lists.map (ref Variants.typeVariantDef) (ref Rewriting.foldOverTypeDef @@ Coders.traversalOrderPre @@
      ("m" ~> "t" ~> Lists.cons (var "t") (var "m")) @@ list [] @@ var "typ")) $
  Flows.map
    ("deps" ~>
      "allVariants" <~ Sets.fromList (Lists.concat (Lists.map (var "variants") (Maps.elems (var "deps")))) $
      Logic.not (Sets.member Mantle.typeVariantFunction (var "allVariants")))
    (ref typeDependenciesDef @@ false @@ (unaryFunction Equality.identity) @@ Core.bindingName (var "el"))

moduleDependencyNamespacesDef :: TBinding (Bool -> Bool -> Bool -> Bool -> Module -> Flow Graph (S.Set Namespace))
moduleDependencyNamespacesDef = define "moduleDependencyNamespaces" $
  doc "Find dependency namespaces in all elements of a module, excluding the module's own namespace" $
  "binds" ~> "withPrims" ~> "withNoms" ~> "withSchema" ~> "mod" ~>
  Flows.bind (ref dependencyNamespacesDef @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "withSchema" @@
    Module.moduleElements (var "mod")) (
    "deps" ~> Flows.pure (Sets.delete (Module.moduleNamespace (var "mod")) (var "deps")))

namespacesForDefinitionsDef :: TBinding ((Namespace -> a) -> Namespace -> [Definition] -> Namespaces a)
namespacesForDefinitionsDef = define "namespacesForDefinitions" $
  doc "Create namespaces mapping for definitions" $
  "encodeNamespace" ~> "focusNs" ~> "defs" ~>
  "nss" <~ Sets.delete (var "focusNs") (ref definitionDependencyNamespacesDef @@ var "defs") $
  "toPair" <~ ("ns" ~> pair (var "ns") (var "encodeNamespace" @@ var "ns")) $
  Module.namespaces (var "toPair" @@ var "focusNs") (Maps.fromList (Lists.map (var "toPair") (Sets.toList (var "nss"))))

nominalApplicationDef :: TBinding (Name -> [Type] -> Type)
nominalApplicationDef = define "nominalApplication" $
  doc "Apply type arguments to a nominal type" $
  "tname" ~> "args" ~>
  Lists.foldl
    ("t" ~> "a" ~> Core.typeApplication $ Core.applicationType (var "t") (var "a"))
    (Core.typeVariable $ var "tname")
    (var "args")

normalTypeVariableDef :: TBinding (Int -> Name)
normalTypeVariableDef = define "normalTypeVariable" $
  doc "Type variable naming convention follows Haskell: t0, t1, etc." $
  "i" ~> Core.name (Strings.cat2 (string "t") (Literals.showInt32 $ var "i"))

requireRecordTypeDef :: TBinding (Name -> Flow Graph RowType)
requireRecordTypeDef = define "requireRecordType" $
  doc "Require a name to resolve to a record type" $
  ref requireRowTypeDef @@ string "record type" @@
    ("t" ~>
      cases _Type (var "t") (Just nothing) [
        _Type_record>>: "rt" ~> just (var "rt")])

requireRowTypeDef :: TBinding (String -> (Type -> Maybe RowType) -> Name -> Flow Graph RowType)
requireRowTypeDef = define "requireRowType" $
  doc "Require a name to resolve to a row type" $
  "label" ~> "getter" ~> "name" ~>
  "rawType" <~ ("t" ~> cases _Type (var "t") (Just (var "t")) [
    _Type_annotated>>: "at" ~> var "rawType" @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~> var "rawType" @@ Core.forallTypeBody (var "ft")]) $
  Flows.bind (ref requireTypeDef @@ var "name") (
    "t" ~>
    Optionals.maybe
      (Flows.fail (Strings.cat (list [
        Core.unName (var "name"),
        string " does not resolve to a ",
        var "label",
        string " type: ",
        ref ShowCore.typeDef @@ var "t"])))
      (unaryFunction Flows.pure)
      (var "getter" @@ (var "rawType" @@ var "t")))

requireSchemaTypeDef :: TBinding (InferenceContext -> Name -> Flow s TypeScheme)
requireSchemaTypeDef = define "requireSchemaType" $
  doc "Look up a schema type in the context and instantiate it" $
  "cx" ~> "tname" ~>
  Optionals.maybe
    (Flows.fail $ Strings.cat2 (string "No such schema type: ") (Core.unName $ var "tname"))
    -- TODO: the deannotation is probably superfluous
    ("ts" ~> ref instantiateTypeSchemeDef @@ (ref Rewriting.deannotateTypeSchemeRecursiveDef @@ var "ts"))
    (Maps.lookup (var "tname") (Typing.inferenceContextSchemaTypes $ var "cx"))

requireTypeDef :: TBinding (Name -> Flow Graph Type)
requireTypeDef = define "requireType" $
  doc "Require a type by name" $
  "name" ~>
  trace (Strings.cat2 (string "require type ") (Core.unName (var "name"))) (
  Flows.bind (ref Lexical.withSchemaContextDef @@ (ref Lexical.requireElementDef @@ var "name")) (
    "el" ~> ref DecodeCore.typeDef @@ Core.bindingTerm (var "el")))

requireUnionTypeDef :: TBinding (Name -> Flow Graph RowType)
requireUnionTypeDef = define "requireUnionType" $
  doc "Require a name to resolve to a union type" $
  ref requireRowTypeDef @@ string "union" @@
    ("t" ~>
      match _Type (Just nothing) [
        _Type_union>>: "rt" ~> just (var "rt")]
      @@ var "t")

resolveTypeDef :: TBinding (Type -> Flow Graph (Maybe Type))
resolveTypeDef = define "resolveType" $
  doc "Resolve a type, dereferencing type variables" $
  "typ" ~>
  match _Type (Just (Flows.pure (just (var "typ")))) [
    _Type_variable>>: "name" ~>
      ref Lexical.withSchemaContextDef @@
        (Flows.bind (ref Lexical.resolveTermDef @@ var "name") (
          "mterm" ~>
          Optionals.maybe (Flows.pure nothing)
            ("t" ~> Flows.map (unaryFunction just) (ref DecodeCore.typeDef @@ var "t"))
            (var "mterm")))]
  @@ (ref Rewriting.deannotateTypeDef @@ var "typ")

schemaGraphToTypingEnvironmentDef :: TBinding (Graph -> Flow s (M.Map Name TypeScheme))
schemaGraphToTypingEnvironmentDef = define "schemaGraphToTypingEnvironment" $
  doc "Convert a schema graph to a typing environment" $
  "g" ~>
  "toTypeScheme" <~ ("vars" ~> "typ" ~> cases _Type (ref Rewriting.deannotateTypeDef @@ var "typ")
    (Just (Core.typeScheme (Lists.reverse (var "vars")) (var "typ"))) [
    _Type_forall>>: "ft" ~> var "toTypeScheme"
      @@ Lists.cons (Core.forallTypeParameter (var "ft")) (var "vars")
      @@ Core.forallTypeBody (var "ft")]) $
  "toPair" <~ ("el" ~> Flows.map
    ("mts" ~> Optionals.map ("ts" ~> pair (Core.bindingName (var "el")) (var "ts")) (var "mts"))
    (optCases (Core.bindingType (var "el"))
      ("typ" <<~ ref DecodeCore.typeDef @@ (Core.bindingTerm (var "el")) $
       "ts" <~ ref fTypeToTypeSchemeDef @@ var "typ" $
       produce (just (var "ts")))
      ("ts" ~> Logic.ifElse
        (Equality.equal (var "ts") (Core.typeScheme (list []) (Core.typeVariable (Core.nameLift _TypeScheme))))
        (Flows.map (unaryFunction just) (ref DecodeCore.typeSchemeDef @@ Core.bindingTerm (var "el")))
        (Logic.ifElse
          (Equality.equal (var "ts") (Core.typeScheme (list []) (Core.typeVariable (Core.nameLift _Type))))
          (Flows.map ("decoded" ~> just (var "toTypeScheme" @@ list [] @@ var "decoded")) (ref DecodeCore.typeDef @@ Core.bindingTerm (var "el")))
          (cases _Term (ref Rewriting.deannotateTermDef @@ (Core.bindingTerm (var "el"))) (Just (Flows.pure nothing)) [
            _Term_record>>: "r" ~>
              Logic.ifElse
                (Equality.equal (Core.recordTypeName (var "r")) (Core.nameLift _TypeScheme))
                (Flows.map
                  (unaryFunction just)
                  (ref DecodeCore.typeSchemeDef @@ Core.bindingTerm (var "el")))
                (Flows.pure nothing),
            _Term_union>>: "i" ~>
              Logic.ifElse (Equality.equal (Core.injectionTypeName (var "i")) (Core.nameLift _Type))
                (Flows.map
                  ("decoded" ~> just (var "toTypeScheme" @@ list [] @@ var "decoded"))
                  (ref DecodeCore.typeDef @@ Core.bindingTerm (var "el")))
                (Flows.pure nothing)]))))) $
  ref Monads.withStateDef @@ var "g" @@
    (Flows.bind (Flows.mapList (var "toPair") (Maps.elems (Graph.graphElements (var "g")))) (
      "mpairs" ~> Flows.pure (Maps.fromList (Optionals.cat (var "mpairs")))))

-- Note: this is lossy, as it throws away the term body
termAsGraphDef :: TBinding (Term -> (M.Map Name Term, Term))
termAsGraphDef = define "termAsGraph" $
  doc "Find the equivalent graph representation of a term" $
  "term" ~> cases _Term (ref Rewriting.deannotateTermDef @@ var "term")
    (Just Maps.empty) [
    _Term_let>>: "lt" ~>
      "bindings" <~ Core.letBindings (var "lt") $
      "fromBinding" <~ ("b" ~>
        "name" <~ Core.bindingName (var "b") $
        "term" <~ Core.bindingTerm (var "b") $
        "ts" <~ Core.bindingType (var "b") $
        pair (var "name") (Core.binding (var "name") (var "term") (var "ts"))) $
      Maps.fromList $ Lists.map (var "fromBinding") (var "bindings")]

topologicalSortTypeDefinitionsDef :: TBinding ([TypeDefinition] -> [[TypeDefinition]])
topologicalSortTypeDefinitionsDef = define "topologicalSortTypeDefinitions" $
  doc "Topologically sort type definitions by dependencies" $
  "defs" ~>
  "toPair" <~ ("def" ~> pair
    (Module.typeDefinitionName (var "def"))
    (Sets.toList (ref Rewriting.typeDependencyNamesDef @@ false @@ Module.typeDefinitionType (var "def")))) $
  "nameToDef" <~ Maps.fromList (Lists.map
    ("d" ~> pair (Module.typeDefinitionName (var "d")) (var "d"))
    (var "defs")) $
  "sorted" <~ ref Sorting.topologicalSortComponentsDef @@ Lists.map (var "toPair") (var "defs") $
  Lists.map ("names" ~> Optionals.cat (Lists.map ("n" ~> Maps.lookup (var "n") (var "nameToDef")) (var "names"))) (
    var "sorted")

typeDependenciesDef :: TBinding (Bool -> (Type -> Type) -> Name -> Flow Graph (M.Map Name Type))
typeDependenciesDef = define "typeDependencies" $
  doc "Get all type dependencies for a given type name" $
  "withSchema" ~> "transform" ~> "name" ~>
  "requireType" <~ ("name" ~>
    trace (Strings.cat2 (string "type dependencies of ") (Core.unName (var "name"))) (
    Flows.bind (ref Lexical.requireElementDef @@ var "name") (
      "el" ~> ref DecodeCore.typeDef @@ Core.bindingTerm (var "el")))) $
  "toPair" <~ ("name" ~>
    Flows.bind (var "requireType" @@ var "name") (
      "typ" ~> Flows.pure (pair (var "name") (var "transform" @@ var "typ")))) $
  "deps" <~ ("seeds" ~> "names" ~>
    Logic.ifElse (Sets.null (var "seeds"))
      (Flows.pure (var "names"))
      (Flows.bind (Flows.mapList (var "toPair") (Sets.toList (var "seeds"))) (
        "pairs" ~>
        "newNames" <~ Maps.union (var "names") (Maps.fromList (var "pairs")) $
        "refs" <~ Lists.foldl (binaryFunction Sets.union) Sets.empty (Lists.map
          ("pair" ~> ref Rewriting.typeDependencyNamesDef @@ var "withSchema" @@ second (var "pair"))
          (var "pairs")) $
        "visited" <~ Sets.fromList (Maps.keys (var "names")) $
        "newSeeds" <~ Sets.difference (var "refs") (var "visited") $
        var "deps" @@ var "newSeeds" @@ var "newNames"))) $
  var "deps" @@ Sets.singleton (var "name") @@ Maps.empty

typeSchemeToFTypeDef :: TBinding (TypeScheme -> Type)
typeSchemeToFTypeDef = define "typeSchemeToFType" $
  doc "Convert a type scheme to a forall type" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "body" <~ Core.typeSchemeType (var "ts") $
  Lists.foldl
    ("t" ~> "v" ~> Core.typeForall $ Core.forallType (var "v") (var "t"))
    (var "body")
    -- Put the variables in the same order in which they are introduced by the type scheme.
    (Lists.reverse $ var "vars")

typeToTypeSchemeDef :: TBinding (Type -> TypeScheme)
typeToTypeSchemeDef = define "typeToTypeScheme" $
  doc "Convert a (System F -style) type to a type scheme" $
  "t0" ~>
  "helper" <~ ("vars" ~> "t" ~> cases _Type (ref Rewriting.deannotateTypeDef @@ var "t")
    (Just $ Core.typeScheme (Lists.reverse $ var "vars") (var "t")) [
    _Type_forall>>: "ft" ~> var "helper"
      @@ (Lists.cons (Core.forallTypeParameter $ var "ft") $ var "vars")
      @@ (Core.forallTypeBody $ var "ft")]) $
  var "helper" @@ list [] @@ var "t0"

typesToElementsDef :: TBinding (M.Map Name Type -> M.Map Name Binding)
typesToElementsDef = define "typesToElements" $
  doc "Encode a map of named types to a map of elements" $
  "typeMap" ~>
  "toElement" <~ ("pair" ~>
    "name" <~ first (var "pair") $
    pair
      (var "name")
      (Core.binding
        (var "name")
        (ref EncodeCore.typeDef @@ (second $ var "pair"))
        nothing)) $
  Maps.fromList $ Lists.map (var "toElement") $ Maps.toList $ var "typeMap"
