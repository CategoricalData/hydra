
module Hydra.Sources.Kernel.Terms.Schemas where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  addNamesToNamespaces,
  definitionDependencyNamespaces,
  dependencyNamespaces,
  dereferenceType,
  elementAsTypeApplicationTerm,
  elementsWithDependencies,
  extendTypeContextForLambda,
  extendTypeContextForLet,
  extendTypeContextForTypeLambda,
  fTypeToTypeScheme,
  fieldMap,
  fieldTypeMap,
  fieldTypes,
  findFieldType,
  freshName,
  freshNames,
  fullyStripType,
  graphAsTerm,
  graphAsTypes,
  graphToInferenceContext,
  graphToTypeContext,
  instantiateType,
  instantiateTypeScheme,
  isEncodedType,
  isEnumRowType,
  isEnumType,
  isSerializable,
  isSerializableByName,
  isSerializableType,
  isUnitTerm,
  isUnitType,
  isType,
  moduleDependencyNamespaces,
  namespacesForDefinitions,
  nominalApplication,
  normalTypeVariable,
  partitionDefinitions,
  requireRecordType,
  requireRowType,
  requireSchemaType,
  requireType,
  requireUnionType,
  resolveType,
  schemaGraphToTypingEnvironment,
  termAsGraph,
  topologicalSortTypeDefinitions,
  typeDependencies,
  typeSchemeToFType,
  typeToTypeScheme,
  typesToElements,
  withLambdaContext,
  withLetContext,
  withTypeLambdaContext)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Annotations  as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants    as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core  as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads       as Monads
import qualified Hydra.Sources.Kernel.Terms.Names        as Names
import qualified Hydra.Sources.Kernel.Terms.Reflect      as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting    as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core    as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Sorting      as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution

import qualified Hydra.Sources.Encode.Core  as EncodeCore


ns :: Namespace
ns = Namespace "hydra.schemas"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, Constants.ns, DecodeCore.ns, moduleNamespace EncodeCore.module_, Lexical.ns, Monads.ns,
      Names.ns, Reflect.ns, Rewriting.ns, ShowCore.ns, Sorting.ns, Substitution.ns]
    kernelTypesNamespaces $
    Just ("Various functions for dereferencing and decoding schema types.")
  where
    elements = [
      toBinding addNamesToNamespaces,
      toBinding definitionDependencyNamespaces,
      toBinding dependencyNamespaces,
      toBinding dereferenceType,
      toBinding elementAsTypeApplicationTerm,
      toBinding elementsWithDependencies,
      toBinding extendTypeContextForLambda,
      toBinding extendTypeContextForLet,
      toBinding extendTypeContextForTypeLambda,
      toBinding fieldMap,
      toBinding fieldTypeMap,
      toBinding fieldTypes,
      toBinding findFieldType,
      toBinding freshName,
      toBinding freshNames,
      toBinding fTypeToTypeScheme,
      toBinding fullyStripType,
      toBinding graphAsTerm,
      toBinding graphAsTypes,
      toBinding graphToInferenceContext,
      toBinding graphToTypeContext,
      toBinding instantiateType,
      toBinding instantiateTypeScheme,
      toBinding isEncodedType,
      toBinding isEnumRowType,
      toBinding isEnumType,
      toBinding isSerializable,
      toBinding isSerializableType,
      toBinding isSerializableByName,
      toBinding isType,
      toBinding isUnitTerm,
      toBinding isUnitType,
      toBinding moduleDependencyNamespaces,
      toBinding namespacesForDefinitions,
      toBinding nominalApplication,
      toBinding normalTypeVariable,
      toBinding partitionDefinitions,
      toBinding requireRecordType,
      toBinding requireRowType,
      toBinding requireSchemaType,
      toBinding requireType,
      toBinding requireUnionType,
      toBinding resolveType,
      toBinding schemaGraphToTypingEnvironment,
      toBinding termAsGraph,
      toBinding topologicalSortTypeDefinitions,
      toBinding typeDependencies,
      toBinding typeSchemeToFType,
      toBinding typeToTypeScheme,
      toBinding typesToElements,
      toBinding withLambdaContext,
      toBinding withLetContext,
      toBinding withTypeLambdaContext]

addNamesToNamespaces :: TBinding ((Namespace -> a) -> S.Set Name -> Namespaces a -> Namespaces a)
addNamesToNamespaces = define "addNamesToNamespaces" $
  doc "Add names to existing namespaces mapping" $
  "encodeNamespace" ~> "names" ~> "ns0" ~>
--  "nss" <~ Sets.empty $
  "nss" <~ Sets.fromList (Maybes.cat $ Lists.map (Names.namespaceOf) $ Sets.toList $ var "names") $
  "toPair" <~ ("ns" ~> pair (var "ns") (var "encodeNamespace" @@ var "ns")) $
  Module.namespacesWithMapping (var "ns0") $ Maps.union
    (Module.namespacesMapping $ var "ns0")
    (Maps.fromList $ Lists.map (var "toPair") $ Sets.toList $ var "nss")

definitionDependencyNamespaces :: TBinding ([Definition] -> S.Set Namespace)
definitionDependencyNamespaces = define "definitionDependencyNamespaces" $
  doc "Get dependency namespaces from definitions" $
  "defs" ~>
  "defNames" <~ ("def" ~> cases _Definition (var "def")
    Nothing [
    _Definition_type>>: "typeDef" ~>
      Rewriting.typeDependencyNames @@ true @@ Module.typeDefinitionType (var "typeDef"),
    _Definition_term>>: "termDef" ~>
      Rewriting.termDependencyNames @@ true @@ true @@ true @@ Module.termDefinitionTerm (var "termDef")]) $
  "allNames" <~ Sets.unions (Lists.map (var "defNames") (var "defs")) $
  Sets.fromList (Maybes.cat (Lists.map (Names.namespaceOf) (Sets.toList (var "allNames"))))

dependencyNamespaces :: TBinding (Bool -> Bool -> Bool -> Bool -> [Binding] -> Flow Graph (S.Set Namespace))
dependencyNamespaces = define "dependencyNamespaces" $
  doc "Find dependency namespaces in all of a set of terms" $
  "binds" ~> "withPrims" ~> "withNoms" ~> "withSchema" ~> "els" ~>
  "depNames" <~ ("el" ~>
    "term" <~ Core.bindingTerm (var "el") $
    "dataNames" <~ Rewriting.termDependencyNames @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "term" $
    "schemaNames" <~ Logic.ifElse (var "withSchema")
      (Maybes.maybe Sets.empty
        ("ts" ~> Rewriting.typeDependencyNames @@ true @@ Core.typeSchemeType (var "ts"))
        (Core.bindingType (var "el")))
      Sets.empty $
    Logic.ifElse (isEncodedType @@ (Rewriting.deannotateTerm @@ var "term"))
      (Flows.bind (trace (string "dependency namespace") $ DecodeCore.type_ @@ var "term") (
        "typ" ~> Flows.pure (Sets.unions (list [
          var "dataNames", var "schemaNames",
          Rewriting.typeDependencyNames @@ true @@ var "typ"]))))
      (Flows.pure (Sets.unions (list [var "dataNames", var "schemaNames"])))) $
  Flows.bind (Flows.mapList (var "depNames") (var "els")) (
    "namesList" ~> Flows.pure (Sets.fromList (Maybes.cat (Lists.map (Names.namespaceOf) (
      Sets.toList (Sets.delete (Constants.placeholderName) (Sets.unions (var "namesList"))))))))

dereferenceType :: TBinding (Name -> Flow Graph (Maybe Type))
dereferenceType = define "dereferenceType" $
  doc "Dereference a type name to get the actual type" $
  "name" ~>
  "mel" <<~ Lexical.dereferenceElement @@ var "name" $
  optCases (var "mel")
    (Flows.pure nothing)
    ("el" ~> Flows.map (unaryFunction just) $ (trace (string "dereference type") $ DecodeCore.type_ @@ Core.bindingTerm (var "el")))

elementAsTypeApplicationTerm :: TBinding (Binding -> Flow Graph TypeApplicationTerm)
elementAsTypeApplicationTerm = define "elementAsTypeApplicationTerm" $
  doc "Convert an element to a typed term" $
  "el" ~>
  Maybes.maybe (Flows.fail (string "missing element type"))
    ("ts" ~> Flows.pure (Core.typeApplicationTerm (Core.bindingTerm (var "el")) (Core.typeSchemeType (var "ts"))))
    (Core.bindingType (var "el"))

elementsWithDependencies :: TBinding ([Binding] -> Flow Graph [Binding])
elementsWithDependencies = define "elementsWithDependencies" $
  doc "Get elements with their dependencies" $
  "original" ~>
  "depNames" <~ ("el" ~> Sets.toList (Rewriting.termDependencyNames @@ true @@ false @@ false @@ (Core.bindingTerm (var "el")))) $
  "allDepNames" <~ Lists.nub (Lists.concat2
    (Lists.map (unaryFunction Core.bindingName) (var "original"))
    (Lists.concat (Lists.map (var "depNames") (var "original")))) $
  Flows.mapList (Lexical.requireElement) (var "allDepNames")

extendTypeContextForLambda :: TBinding (TypeContext -> Lambda -> TypeContext)
extendTypeContextForLambda = define "extendTypeContextForLambda" $
  doc "Extend a type context by descending into a System F lambda body" $
  "tcontext" ~> "lam" ~>
  "var" <~ Core.lambdaParameter (var "lam") $
  "dom" <~ Maybes.fromJust (Core.lambdaDomain (var "lam")) $
  Typing.typeContext
    (Maps.insert (var "var") (var "dom") (Typing.typeContextTypes (var "tcontext")))
    (Maps.delete (var "var") (Typing.typeContextMetadata $ var "tcontext"))
    (Typing.typeContextTypeVariables $ var "tcontext")
    (Sets.insert (var "var") $ Typing.typeContextLambdaVariables $ var "tcontext")
    (Typing.typeContextInferenceContext $ var "tcontext")

extendTypeContextForLet :: TBinding ((TypeContext -> Binding -> Maybe Term) -> TypeContext -> Let -> TypeContext)
extendTypeContextForLet = define "extendTypeContextForLet" $
  doc "Extend a type context by descending into a let body" $
  "forBinding" ~> "tcontext" ~> "letrec" ~>
  "bindings" <~ Core.letBindings (var "letrec") $
  Typing.typeContext
    (Maps.union
      (Typing.typeContextTypes (var "tcontext"))
      (Maps.fromList $ Lists.map
        ("b" ~> pair
          (Core.bindingName $ var "b")
          (typeSchemeToFType @@ (Maybes.fromJust $ Core.bindingType $ var "b")))
        (var "bindings")))
    (Lists.foldl
      ("m" ~> "b" ~> optCases (var "forBinding" @@ var "tcontext" @@ var "b")
        (Maps.delete (Core.bindingName $ var "b") (var "m"))
        ("t" ~> Maps.insert (Core.bindingName $ var "b") (var "t") (var "m")))
      (Typing.typeContextMetadata $ var "tcontext")
      (var "bindings"))
    (Typing.typeContextTypeVariables $ var "tcontext")
    (Lists.foldl
      ("s" ~> "b" ~> Sets.delete (Core.bindingName $ var "b") (var "s"))
      (Typing.typeContextLambdaVariables $ var "tcontext")
      (var "bindings"))
    (Typing.typeContextInferenceContext $ var "tcontext")

extendTypeContextForTypeLambda :: TBinding (TypeContext -> TypeLambda -> TypeContext)
extendTypeContextForTypeLambda = define "extendTypeContextForTypeLambda" $
  doc "Extend a type context by descending into a System F type lambda body" $
  "tcontext" ~> "tlam" ~>
  "name" <~ Core.typeLambdaParameter (var "tlam") $
  Typing.typeContextWithTypeVariables
    (var "tcontext")
    (Sets.insert (var "name") (Typing.typeContextTypeVariables (var "tcontext")))

fieldMap :: TBinding ([Field] -> M.Map Name Term)
fieldMap = define "fieldMap" $
  "fields" ~>
  "toPair" <~ ("f" ~> pair (Core.fieldName $ var "f") (Core.fieldTerm $ var "f")) $
  Maps.fromList $ Lists.map (var "toPair") (var "fields")

fieldTypeMap :: TBinding ([FieldType] -> M.Map Name Type)
fieldTypeMap = define "fieldTypeMap" $
  "fields" ~>
  "toPair" <~ ("f" ~> pair (Core.fieldTypeName $ var "f") (Core.fieldTypeType $ var "f")) $
  Maps.fromList $ Lists.map (var "toPair") (var "fields")

fieldTypes :: TBinding (Type -> Flow Graph (M.Map Name Type))
fieldTypes = define "fieldTypes" $
  doc "Get field types from a record or union type" $
  "t" ~>
  "toMap" <~ ("fields" ~> Maps.fromList (Lists.map
    ("ft" ~> pair (Core.fieldTypeName (var "ft")) (Core.fieldTypeType (var "ft")))
    (var "fields"))) $
  match _Type (Just (Monads.unexpected @@ string "record or union type" @@ (ShowCore.type_ @@ var "t"))) [
    _Type_forall>>: "ft" ~> fieldTypes @@ Core.forallTypeBody (var "ft"),
    _Type_record>>: "rt" ~> Flows.pure (var "toMap" @@ Core.rowTypeFields (var "rt")),
    _Type_union>>: "rt" ~> Flows.pure (var "toMap" @@ Core.rowTypeFields (var "rt")),
    _Type_variable>>: "name" ~>
      trace (Strings.cat2 (string "field types of ") (Core.unName (var "name"))) (
      Flows.bind (Lexical.requireElement @@ var "name") (
        "el" ~>
        Flows.bind (trace (string "field types") $ DecodeCore.type_ @@ Core.bindingTerm (var "el")) (
          fieldTypes)))]
  @@ (Rewriting.deannotateType @@ var "t")

findFieldType :: TBinding (Name -> [FieldType] -> Flow s Type)
findFieldType = define "findFieldType" $
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

fTypeToTypeScheme :: TBinding (Type -> TypeScheme)
fTypeToTypeScheme = define "fTypeToTypeScheme" $
  doc "Convert a forall type to a type scheme" $
  "typ" ~>
  "gatherForall" <~ ("vars" ~> "typ" ~> cases _Type (Rewriting.deannotateType @@ var "typ")
     (Just $ Core.typeScheme (Lists.reverse $ var "vars") (var "typ")) [
     _Type_forall>>: "ft" ~> var "gatherForall" @@
       (Lists.cons (Core.forallTypeParameter $ var "ft") (var "vars")) @@
       (Core.forallTypeBody $ var "ft")]) $
  var "gatherForall" @@ list ([] :: [TTerm Name]) @@ var "typ"

freshName :: TBinding (Flow s Name)
freshName = define "freshName" $
  doc "Generate a fresh type variable name" $
  Flows.map (normalTypeVariable) (Annotations.nextCount @@ Constants.key_freshTypeVariableCount)

freshNames :: TBinding (Int -> Flow s [Name])
freshNames = define "freshNames" $
  doc "Generate multiple fresh type variable names" $
  "n" ~> Flows.sequence $ Lists.replicate (var "n") (freshName)

fullyStripType :: TBinding (Type -> Type)
fullyStripType = define "fullyStripType" $
  doc "Fully strip a type of forall quantifiers" $
  "typ" ~>
  match _Type (Just (var "typ")) [
    _Type_forall>>: "ft" ~> fullyStripType @@ Core.forallTypeBody (var "ft")]
  @@ (Rewriting.deannotateType @@ var "typ")

graphAsTerm :: TBinding (Graph -> Term)
graphAsTerm = define "graphAsTerm" $
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

graphAsTypes :: TBinding (Graph -> Flow s (M.Map Name Type))
graphAsTypes = define "graphAsTypes" $
  doc "Decode a schema graph which encodes a set of named types" $
  "sg" ~>
  "els" <~ Maps.elems (Graph.graphElements (var "sg")) $
  "toPair" <~ ("el" ~>
    "typ" <<~ (trace ((string "graph as types: ") ++ Core.unName (Core.bindingName $ var "el")) $ DecodeCore.type_ @@ (Core.bindingTerm $ var "el")) $
    produce $ pair (Core.bindingName $ var "el") (var "typ")) $
  "pairs" <<~ Flows.mapList (var "toPair") (var "els") $
  produce $ Maps.fromList $ var "pairs"

graphToInferenceContext :: TBinding (Graph -> Flow s InferenceContext)
graphToInferenceContext = define "graphToInferenceContext" $
  doc "Convert a graph to an inference context" $
  "graph" ~>
  "schema" <~ Maybes.fromMaybe (var "graph") (Graph.graphSchema $ var "graph") $
  "primTypes" <~ Maps.fromList (Lists.map
    ("p" ~> pair (Graph.primitiveName $ var "p") (Graph.primitiveType $ var "p"))
    (Maps.elems $ Graph.graphPrimitives $ var "graph")) $
  "varTypes" <~ Maps.fromList (Maybes.cat $ Lists.map
    ("b" ~> Maybes.map ("ts" ~> pair (Core.bindingName $ var "b") (var "ts")) $ Core.bindingType $ var "b")
    (Maps.elems $ Graph.graphElements $ var "graph")) $
  "schemaTypes" <<~ schemaGraphToTypingEnvironment @@ var "schema" $
  produce $ Typing.inferenceContext (var "schemaTypes") (var "primTypes") (var "varTypes") false

graphToTypeContext :: TBinding (Graph -> Flow s TypeContext)
graphToTypeContext = define "graphToTypeContext" $
  doc "Convert a graph to a top-level type context (outside of the bindings of the graph)" $
  "graph" ~>
  "ix" <<~ graphToInferenceContext @@ var "graph" $
  produce $ Typing.typeContext Maps.empty Maps.empty Sets.empty Sets.empty (var "ix")

instantiateType :: TBinding (Type -> Flow s Type)
instantiateType = define "instantiateType" $
  doc "Instantiate a type by replacing all forall-bound type variables with fresh variables" $
  "typ" ~>
  "ts" <<~ instantiateTypeScheme @@ (typeToTypeScheme @@ var "typ") $
  produce $ typeSchemeToFType @@ var "ts"

instantiateTypeScheme :: TBinding (TypeScheme -> Flow s TypeScheme)
instantiateTypeScheme = define "instantiateTypeScheme" $
  doc "Instantiate a type scheme with fresh variables" $
  "scheme" ~>
  "oldVars" <~ Core.typeSchemeVariables (var "scheme") $
  "newVars" <<~ freshNames @@ Lists.length (var "oldVars") $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "oldVars") (Lists.map (unaryFunction Core.typeVariable) $ var "newVars")) $
  produce $ Core.typeScheme (var "newVars") $
    Substitution.substInType @@ var "subst" @@ Core.typeSchemeType (var "scheme")

isEnumRowType :: TBinding (RowType -> Bool)
isEnumRowType = define "isEnumRowType" $
  doc "Check if a row type represents an enum (all fields are unit-typed)" $
  "rt" ~> Lists.foldl (binaryFunction Logic.and) true $
    Lists.map ("f" ~> isUnitType @@ (Rewriting.deannotateType @@ (Core.fieldTypeType (var "f")))) $
      Core.rowTypeFields $ var "rt"

isEncodedType :: TBinding (Term -> Bool)
isEncodedType = define "isEncodedType" $
  doc "Determines whether a given term is an encoded type" $
  "t" ~> cases _Term (Rewriting.deannotateTerm @@ var "t") (Just false) [
    _Term_application>>: "a" ~>
      isEncodedType @@ (Core.applicationFunction (var "a")),
    _Term_union>>: "i" ~>
      Equality.equal (string (unName _Type)) (Core.unName (Core.injectionTypeName (var "i")))]

isEnumType :: TBinding (Type -> Bool)
isEnumType = define "isEnumType" $
  doc "Check if a type is an enum type" $
  "typ" ~>
  match _Type (Just false) [
    _Type_union>>: "rt" ~> isEnumRowType @@ var "rt"]
  @@ (Rewriting.deannotateType @@ var "typ")

isSerializable :: TBinding (Binding -> Flow Graph Bool)
isSerializable = define "isSerializable" $
  doc "Check if an element is serializable (no function types in dependencies)" $
  "el" ~>
  "variants" <~ ("typ" ~>
    Lists.map (Reflect.typeVariant) (Rewriting.foldOverType @@ Coders.traversalOrderPre @@
      ("m" ~> "t" ~> Lists.cons (var "t") (var "m")) @@ list ([] :: [TTerm Type]) @@ var "typ")) $
  Flows.map
    ("deps" ~>
      "allVariants" <~ Sets.fromList (Lists.concat (Lists.map (var "variants") (Maps.elems (var "deps")))) $
      Logic.not (Sets.member Variants.typeVariantFunction (var "allVariants")))
    (typeDependencies @@ false @@ (unaryFunction Equality.identity) @@ Core.bindingName (var "el"))

isSerializableType :: TBinding (Type -> Bool)
isSerializableType = define "isSerializableType" $
  doc "Check if a type is serializable (no function types in the type itself)" $
  "typ" ~>
  "allVariants" <~ Sets.fromList (Lists.map (Reflect.typeVariant)
    (Rewriting.foldOverType @@ Coders.traversalOrderPre @@
      ("m" ~> "t" ~> Lists.cons (var "t") (var "m")) @@ list ([] :: [TTerm Type]) @@ var "typ")) $
  Logic.not (Sets.member Variants.typeVariantFunction (var "allVariants"))

isSerializableByName :: TBinding (Name -> Flow Graph Bool)
isSerializableByName = define "isSerializableByName" $
  doc "Check if a type (by name) is serializable, resolving all type dependencies" $
  "name" ~>
  "variants" <~ ("typ" ~>
    Lists.map (Reflect.typeVariant) (Rewriting.foldOverType @@ Coders.traversalOrderPre @@
      ("m" ~> "t" ~> Lists.cons (var "t") (var "m")) @@ list ([] :: [TTerm Type]) @@ var "typ")) $
  Flows.map
    ("deps" ~>
      "allVariants" <~ Sets.fromList (Lists.concat (Lists.map (var "variants") (Maps.elems (var "deps")))) $
      Logic.not (Sets.member Variants.typeVariantFunction (var "allVariants")))
    (typeDependencies @@ false @@ (unaryFunction Equality.identity) @@ var "name")

isType :: TBinding (Type -> Bool)
isType = define "isType" $
  doc "Check whether a type is a type (always true for non-encoded types)" $
  "t" ~> cases _Type (Rewriting.deannotateType @@ var "t") (Just false) [
    _Type_application>>: "a" ~>
      isType @@ (Core.applicationTypeFunction (var "a")),
    _Type_forall>>: "l" ~>
      isType @@ (Core.forallTypeBody (var "l")),
    _Type_union>>: "rt" ~>
      Equality.equal (string (unName _Type)) (Core.unName (Core.rowTypeTypeName (var "rt"))),
    _Type_variable>>: "v" ~> Equality.equal (var "v") (Core.nameLift _Type)]

isUnitTerm :: TBinding (Term -> Bool)
isUnitTerm = define "isUnitTerm" $
  doc "Check whether a term is the unit term" $
  match _Term (Just false) [_Term_unit>>: constant true]

isUnitType :: TBinding (Type -> Bool)
isUnitType = define "isUnitType" $
  doc "Check whether a type is the unit type" $
  match _Type (Just false) [_Type_unit>>: constant true]

moduleDependencyNamespaces :: TBinding (Bool -> Bool -> Bool -> Bool -> Module -> Flow Graph (S.Set Namespace))
moduleDependencyNamespaces = define "moduleDependencyNamespaces" $
  doc "Find dependency namespaces in all elements of a module, excluding the module's own namespace" $
  "binds" ~> "withPrims" ~> "withNoms" ~> "withSchema" ~> "mod" ~>
  Flows.bind (dependencyNamespaces @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "withSchema" @@
    Module.moduleElements (var "mod")) (
    "deps" ~> Flows.pure (Sets.delete (Module.moduleNamespace (var "mod")) (var "deps")))

namespacesForDefinitions :: TBinding ((Namespace -> a) -> Namespace -> [Definition] -> Namespaces a)
namespacesForDefinitions = define "namespacesForDefinitions" $
  doc "Create namespaces mapping for definitions" $
  "encodeNamespace" ~> "focusNs" ~> "defs" ~>
  "nss" <~ Sets.delete (var "focusNs") (definitionDependencyNamespaces @@ var "defs") $
  "toPair" <~ ("ns" ~> pair (var "ns") (var "encodeNamespace" @@ var "ns")) $
  Module.namespaces (var "toPair" @@ var "focusNs") (Maps.fromList (Lists.map (var "toPair") (Sets.toList (var "nss"))))

nominalApplication :: TBinding (Name -> [Type] -> Type)
nominalApplication = define "nominalApplication" $
  doc "Apply type arguments to a nominal type" $
  "tname" ~> "args" ~>
  Lists.foldl
    ("t" ~> "a" ~> Core.typeApplication $ Core.applicationType (var "t") (var "a"))
    (Core.typeVariable $ var "tname")
    (var "args")

normalTypeVariable :: TBinding (Int -> Name)
normalTypeVariable = define "normalTypeVariable" $
  doc "Type variable naming convention follows Haskell: t0, t1, etc." $
  "i" ~> Core.name (Strings.cat2 (string "t") (Literals.showInt32 $ var "i"))

partitionDefinitions :: TBinding ([Definition] -> ([TypeDefinition], [TermDefinition]))
partitionDefinitions = define "partitionDefinitions" $
  doc "Partition a list of definitions into type definitions and term definitions" $
  "defs" ~>
  "getType" <~ ("def" ~> cases _Definition (var "def") Nothing [
    _Definition_type>>: "td" ~> just (var "td"),
    _Definition_term>>: "_" ~> nothing]) $
  "getTerm" <~ ("def" ~> cases _Definition (var "def") Nothing [
    _Definition_type>>: "_" ~> nothing,
    _Definition_term>>: "td" ~> just (var "td")]) $
  pair
    (Maybes.cat $ Lists.map (var "getType") (var "defs"))
    (Maybes.cat $ Lists.map (var "getTerm") (var "defs"))

requireRecordType :: TBinding (Name -> Flow Graph RowType)
requireRecordType = define "requireRecordType" $
  doc "Require a name to resolve to a record type" $
  "name" ~>
  "toRecord" <~ ("t" ~> cases _Type (var "t") (Just nothing) [
    _Type_record>>: "rt" ~> just (var "rt")]) $
  requireRowType @@ string "record type" @@ var "toRecord" @@ var "name"

requireRowType :: TBinding (String -> (Type -> Maybe RowType) -> Name -> Flow Graph RowType)
requireRowType = define "requireRowType" $
  doc "Require a name to resolve to a row type" $
  "label" ~> "getter" ~> "name" ~>
  "rawType" <~ ("t" ~> cases _Type (var "t") (Just (var "t")) [
    _Type_annotated>>: "at" ~> var "rawType" @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~> var "rawType" @@ Core.forallTypeBody (var "ft")]) $
  Flows.bind (requireType @@ var "name") (
    "t" ~>
    Maybes.maybe
      (Flows.fail (Strings.cat (list [
        Core.unName (var "name"),
        (string " does not resolve to a "),
        var "label",
        (string " type: "),
        ShowCore.type_ @@ var "t"])))
      (unaryFunction Flows.pure)
      (var "getter" @@ (var "rawType" @@ var "t")))

requireSchemaType :: TBinding (InferenceContext -> Name -> Flow s TypeScheme)
requireSchemaType = define "requireSchemaType" $
  doc "Look up a schema type in the context and instantiate it" $
  "cx" ~> "tname" ~>
  "types" <~ (Typing.inferenceContextSchemaTypes $ var "cx") $
  Maybes.maybe
    (Flows.fail $ Strings.cat $ list [
      (string "No such schema type: "),
      Core.unName $ var "tname",
      (string ". Available types are: "),
      Strings.intercalate (string ", ") (Lists.map (unaryFunction Core.unName) $ Maps.keys $ var "types")])
    -- TODO: the deannotation is probably superfluous
    ("ts" ~> instantiateTypeScheme @@ (Rewriting.deannotateTypeSchemeRecursive @@ var "ts"))
    (Maps.lookup (var "tname") (var "types"))

requireType :: TBinding (Name -> Flow Graph Type)
requireType = define "requireType" $
  doc "Require a type by name" $
  "name" ~>
  trace (Strings.cat2 (string "require type ") (Core.unName (var "name"))) $
  Flows.bind (Lexical.withSchemaContext @@ (Lexical.requireElement @@ var "name")) (
    "el" ~> DecodeCore.type_ @@ Core.bindingTerm (var "el"))

requireUnionType :: TBinding (Name -> Flow Graph RowType)
requireUnionType = define "requireUnionType" $
  doc "Require a name to resolve to a union type" $
  "name" ~>
  "toUnion" <~ ("t" ~> cases _Type (var "t")
    (Just nothing) [
    _Type_union>>: "rt" ~> just (var "rt")]) $
  requireRowType @@ string "union" @@ var "toUnion" @@ var "name"

resolveType :: TBinding (Type -> Flow Graph (Maybe Type))
resolveType = define "resolveType" $
  doc "Resolve a type, dereferencing type variables" $
  "typ" ~>
--  trace "resolve type" $
  match _Type (Just (Flows.pure (just (var "typ")))) [
    _Type_variable>>: "name" ~>
      Lexical.withSchemaContext @@
        (Flows.bind (Lexical.resolveTerm @@ var "name") (
          "mterm" ~>
          Maybes.maybe (Flows.pure nothing)
            ("t" ~> Flows.map (unaryFunction just) (DecodeCore.type_ @@ var "t"))
            (var "mterm")))]
  @@ (Rewriting.deannotateType @@ var "typ")

schemaGraphToTypingEnvironment :: TBinding (Graph -> Flow s (M.Map Name TypeScheme))
schemaGraphToTypingEnvironment = define "schemaGraphToTypingEnvironment" $
  doc "Convert a schema graph to a typing environment" $
  "g" ~>
  "toTypeScheme" <~ ("vars" ~> "typ" ~> cases _Type (Rewriting.deannotateType @@ var "typ")
    (Just (Core.typeScheme (Lists.reverse (var "vars")) (var "typ"))) [
    _Type_forall>>: "ft" ~> var "toTypeScheme"
      @@ Lists.cons (Core.forallTypeParameter (var "ft")) (var "vars")
      @@ Core.forallTypeBody (var "ft")]) $
  "toPair" <~ ("el" ~>
    "forTerm" <~ ("term" ~> cases _Term (var "term") (Just (Flows.pure nothing)) [
      _Term_record>>: "r" ~>
        Logic.ifElse
          (Equality.equal (Core.recordTypeName (var "r")) (Core.nameLift _TypeScheme))
          (Flows.map
            (unaryFunction just)
            (DecodeCore.typeScheme @@ Core.bindingTerm (var "el")))
          (Flows.pure nothing),
      _Term_union>>: "i" ~>
        Logic.ifElse (Equality.equal (Core.injectionTypeName (var "i")) (Core.nameLift _Type))
          (Flows.map
            ("decoded" ~> just (var "toTypeScheme" @@ list ([] :: [TTerm Name]) @@ var "decoded"))
            (DecodeCore.type_ @@ Core.bindingTerm (var "el")))
          (Flows.pure nothing)]) $
    "mts" <<~ optCases (Core.bindingType (var "el"))
      (Flows.map ("typ" ~> just $ fTypeToTypeScheme @@ var "typ") $ DecodeCore.type_ @@ (Core.bindingTerm (var "el")))
      ("ts" ~> Logic.ifElse
        (Equality.equal (var "ts") (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable (Core.nameLift _TypeScheme))))
        (Flows.map (unaryFunction just) (DecodeCore.typeScheme @@ Core.bindingTerm (var "el")))
        (Logic.ifElse
          (Equality.equal (var "ts") (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable (Core.nameLift _Type))))
          (Flows.map ("decoded" ~> just (var "toTypeScheme" @@ list ([] :: [TTerm Name]) @@ var "decoded")) (DecodeCore.type_ @@ Core.bindingTerm (var "el")))
          (var "forTerm" @@ (Rewriting.deannotateTerm @@ (Core.bindingTerm (var "el")))))) $
    produce $ Maybes.map ("ts" ~> pair (Core.bindingName (var "el")) (var "ts")) (var "mts")) $
  trace (string "schema graph to typing environment") $
  Monads.withState @@ var "g" @@
    (Flows.bind (Flows.mapList (var "toPair") (Maps.elems (Graph.graphElements (var "g")))) (
      "mpairs" ~> Flows.pure (Maps.fromList (Maybes.cat (var "mpairs")))))

-- Note: this is lossy, as it throws away the term body
termAsGraph :: TBinding (Term -> (M.Map Name Term, Term))
termAsGraph = define "termAsGraph" $
  doc "Find the equivalent graph representation of a term" $
  "term" ~> cases _Term (Rewriting.deannotateTerm @@ var "term")
    (Just Maps.empty) [
    _Term_let>>: "lt" ~>
      "bindings" <~ Core.letBindings (var "lt") $
      "fromBinding" <~ ("b" ~>
        "name" <~ Core.bindingName (var "b") $
        "term" <~ Core.bindingTerm (var "b") $
        "ts" <~ Core.bindingType (var "b") $
        pair (var "name") (Core.binding (var "name") (var "term") (var "ts"))) $
      Maps.fromList $ Lists.map (var "fromBinding") (var "bindings")]

topologicalSortTypeDefinitions :: TBinding ([TypeDefinition] -> [[TypeDefinition]])
topologicalSortTypeDefinitions = define "topologicalSortTypeDefinitions" $
  doc "Topologically sort type definitions by dependencies" $
  "defs" ~>
  "toPair" <~ ("def" ~> pair
    (Module.typeDefinitionName (var "def"))
    (Sets.toList (Rewriting.typeDependencyNames @@ false @@ Module.typeDefinitionType (var "def")))) $
  "nameToDef" <~ Maps.fromList (Lists.map
    ("d" ~> pair (Module.typeDefinitionName (var "d")) (var "d"))
    (var "defs")) $
  "sorted" <~ Sorting.topologicalSortComponents @@ Lists.map (var "toPair") (var "defs") $
  Lists.map ("names" ~> Maybes.cat (Lists.map ("n" ~> Maps.lookup (var "n") (var "nameToDef")) (var "names"))) (
    var "sorted")

typeDependencies :: TBinding (Bool -> (Type -> Type) -> Name -> Flow Graph (M.Map Name Type))
typeDependencies = define "typeDependencies" $
  doc "Get all type dependencies for a given type name" $
  "withSchema" ~> "transform" ~> "name" ~>
  "requireType" <~ ("name" ~>
    trace (Strings.cat2 (string "type dependencies of ") (Core.unName (var "name"))) (
    Flows.bind (Lexical.requireElement @@ var "name") (
      "el" ~> DecodeCore.type_ @@ Core.bindingTerm (var "el")))) $
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
          ("pair" ~> Rewriting.typeDependencyNames @@ var "withSchema" @@ Pairs.second (var "pair"))
          (var "pairs")) $
        "visited" <~ Sets.fromList (Maps.keys (var "names")) $
        "newSeeds" <~ Sets.difference (var "refs") (var "visited") $
        var "deps" @@ var "newSeeds" @@ var "newNames"))) $
  trace (string "type dependencies") $
  var "deps" @@ Sets.singleton (var "name") @@ Maps.empty

typeSchemeToFType :: TBinding (TypeScheme -> Type)
typeSchemeToFType = define "typeSchemeToFType" $
  doc "Convert a type scheme to a forall type" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "body" <~ Core.typeSchemeType (var "ts") $
  Lists.foldl
    ("t" ~> "v" ~> Core.typeForall $ Core.forallType (var "v") (var "t"))
    (var "body")
    -- Put the variables in the same order in which they are introduced by the type scheme.
    (Lists.reverse $ var "vars")

typeToTypeScheme :: TBinding (Type -> TypeScheme)
typeToTypeScheme = define "typeToTypeScheme" $
  doc "Convert a (System F -style) type to a type scheme" $
  "t0" ~>
  "helper" <~ ("vars" ~> "t" ~> cases _Type (Rewriting.deannotateType @@ var "t")
    (Just $ Core.typeScheme (Lists.reverse $ var "vars") (var "t")) [
    _Type_forall>>: "ft" ~> var "helper"
      @@ (Lists.cons (Core.forallTypeParameter $ var "ft") $ var "vars")
      @@ (Core.forallTypeBody $ var "ft")]) $
  var "helper" @@ list ([] :: [TTerm Name]) @@ var "t0"

typesToElements :: TBinding (M.Map Name Type -> M.Map Name Binding)
typesToElements = define "typesToElements" $
  doc "Encode a map of named types to a map of elements" $
  "typeMap" ~>
  "toElement" <~ ("pair" ~>
    "name" <~ Pairs.first (var "pair") $
    pair
      (var "name")
      (Core.binding
        (var "name")
        (encoderFor _Type @@ (Pairs.second $ var "pair"))
        nothing)) $
  Maps.fromList $ Lists.map (var "toElement") $ Maps.toList $ var "typeMap"

withLambdaContext :: TBinding ((e -> TypeContext) -> (TypeContext -> e -> e) -> e -> Lambda -> (e -> Flow s a) -> Flow s a)
withLambdaContext = define "withLambdaContext" $
  doc "Execute a computation in the context of a lambda body, extending the type context with the lambda parameter" $
  "getContext" ~> "setContext" ~> "env" ~> "lam" ~> "body" ~>
  "newContext" <~ extendTypeContextForLambda @@ (var "getContext" @@ var "env") @@ var "lam" $
  var "body" @@ (var "setContext" @@ var "newContext" @@ var "env")

withLetContext :: TBinding ((e -> TypeContext) -> (TypeContext -> e -> e) -> (TypeContext -> Binding -> Maybe Term) -> e -> Let -> (e -> Flow s a) -> Flow s a)
withLetContext = define "withLetContext" $
  doc "Execute a computation in the context of a let body, extending the type context with the let bindings" $
  "getContext" ~> "setContext" ~> "forBinding" ~> "env" ~> "letrec" ~> "body" ~>
  "newContext" <~ extendTypeContextForLet @@ var "forBinding" @@ (var "getContext" @@ var "env") @@ var "letrec" $
  var "body" @@ (var "setContext" @@ var "newContext" @@ var "env")

withTypeLambdaContext :: TBinding ((e -> TypeContext) -> (TypeContext -> e -> e) -> e -> TypeLambda -> (e -> Flow s a) -> Flow s a)
withTypeLambdaContext = define "withTypeLambdaContext" $
  doc "Execute a computation in the context of a type lambda body, extending the type context with the type parameter" $
  "getContext" ~> "setContext" ~> "env" ~> "tlam" ~> "body" ~>
  "newContext" <~ extendTypeContextForTypeLambda @@ (var "getContext" @@ var "env") @@ var "tlam" $
  var "body" @@ (var "setContext" @@ var "newContext" @@ var "env")
