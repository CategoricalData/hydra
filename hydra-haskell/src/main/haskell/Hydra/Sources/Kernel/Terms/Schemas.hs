
module Hydra.Sources.Kernel.Terms.Schemas where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  addNamesToNamespaces,
  definitionDependencyNamespaces,
  dependencyNamespaces,
  dereferenceType,
  elementAsTypeApplicationTerm,
  elementsWithDependencies,
  extendGraphForLambda,
  extendGraphForLet,
  extendGraphForTypeLambda,
  fTypeIsPolymorphic,
  fieldMap,
  fieldTypeMap,
  fieldTypes,
  findFieldType,
  freshName,
  freshNames,
  fullyStripAndNormalizeType,
  fullyStripType,
  graphAsLet,
  graphAsTerm,
  graphAsTypes,
  instantiateType,
  instantiateTypeScheme,
  isEncodedTerm,
  isEncodedType,
  isEnumRowType,
  isEnumType,
  isSerializable,
  isSerializableByName,
  isSerializableType,
  isUnitTerm,
  isUnitType,
  isType,
  moduleContainsBinaryLiterals,
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
  termAsBindings,
  topologicalSortTypeDefinitions,
  typeDependencies,
  typeToTypeScheme,
  typesToElements,
  withLambdaContext,
  withLetContext,
  withTypeLambdaContext)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors    as Accessors
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Meta.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Coders       as Coders
import qualified Hydra.Dsl.Meta.Compute      as Compute
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Grammar      as Grammar
import qualified Hydra.Dsl.Grammars          as Grammars
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Meta.Json         as Json
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
import qualified Hydra.Dsl.Meta.Module       as Module
import qualified Hydra.Dsl.Meta.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Meta.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Meta.Typing       as Typing
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Meta.Error        as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Annotations  as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants    as Constants
import qualified Hydra.Sources.Decode.Core  as DecodeCore
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

formatOtherError :: TTerm (InContext OtherError -> String)
formatOtherError = "ic" ~> Error.unOtherError @@ Ctx.inContextObject (var "ic")

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, Constants.ns, moduleNamespace DecodeCore.module_, moduleNamespace EncodeCore.module_, Lexical.ns, Monads.ns,
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
      toBinding extendGraphForLambda,
      toBinding extendGraphForLet,
      toBinding extendGraphForTypeLambda,
      toBinding fieldMap,
      toBinding fieldTypeMap,
      toBinding fieldTypes,
      toBinding findFieldType,
      toBinding freshName,
      toBinding freshNames,
      toBinding fTypeIsPolymorphic,
      toBinding fullyStripAndNormalizeType,
      toBinding fullyStripType,
      toBinding graphAsLet,
      toBinding graphAsTerm,
      toBinding graphAsTypes,
      toBinding instantiateType,
      toBinding instantiateTypeScheme,
      toBinding isEncodedTerm,
      toBinding isEncodedType,
      toBinding isEnumRowType,
      toBinding isEnumType,
      toBinding isSerializable,
      toBinding isSerializableType,
      toBinding isSerializableByName,
      toBinding isType,
      toBinding isUnitTerm,
      toBinding isUnitType,
      toBinding moduleContainsBinaryLiterals,
      toBinding moduleDependencyNamespaces,
      toBinding namespacesForDefinitions,
      toBinding nominalApplication,
      toBinding normalTypeVariable,
      toBinding partitionDefinitions,
      toBinding requireRecordType,
      toBinding requireRowType,
      toBinding requireSchemaType,
      toBinding requireType,
      toBinding requireUnionField_,
      toBinding requireUnionType,
      toBinding resolveType,
      toBinding schemaGraphToTypingEnvironment,
      toBinding termAsBindings,
      toBinding topologicalSortTypeDefinitions,
      toBinding typeDependencies,
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

dependencyNamespaces :: TBinding (Context -> Graph -> Bool -> Bool -> Bool -> Bool -> [Binding] -> Either (InContext OtherError) (S.Set Namespace))
dependencyNamespaces = define "dependencyNamespaces" $
  doc "Find dependency namespaces in all of a set of terms (Either version)" $
  "cx" ~> "graph" ~> "binds" ~> "withPrims" ~> "withNoms" ~> "withSchema" ~> "els" ~>
  "depNames" <~ ("el" ~>
    "term" <~ Core.bindingTerm (var "el") $
    "deannotatedTerm" <~ Rewriting.deannotateTerm @@ var "term" $
    "dataNames" <~ Rewriting.termDependencyNames @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "term" $
    "schemaNames" <~ Logic.ifElse (var "withSchema")
      (Maybes.maybe Sets.empty
        ("ts" ~> Rewriting.typeDependencyNames @@ true @@ Core.typeSchemeType (var "ts"))
        (Core.bindingType (var "el")))
      Sets.empty $
    -- Handle encoded types: decode as Type and extract type dependency names
    Logic.ifElse (isEncodedType @@ var "deannotatedTerm")
      (Eithers.map ("typ" ~> Sets.unions (list [
          var "dataNames", var "schemaNames",
          Rewriting.typeDependencyNames @@ true @@ var "typ"]))
        (Ctx.withContext (Ctx.pushTrace (string "dependency namespace (type)") (var "cx"))
          (Eithers.bimap ("_e" ~> Error.otherError (Error.unDecodingError @@ var "_e")) ("_a" ~> var "_a")
            (decoderFor _Type @@ var "graph" @@ var "term"))))
      -- Handle encoded terms: decode as Term and extract term dependency names
      (Logic.ifElse (isEncodedTerm @@ var "deannotatedTerm")
        (Eithers.map ("decodedTerm" ~> Sets.unions (list [
            var "dataNames", var "schemaNames",
            Rewriting.termDependencyNames @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "decodedTerm"]))
          (Ctx.withContext (Ctx.pushTrace (string "dependency namespace (term)") (var "cx"))
            (Eithers.bimap ("_e" ~> Error.otherError (Error.unDecodingError @@ var "_e")) ("_a" ~> var "_a")
              (decoderFor _Term @@ var "graph" @@ var "term"))))
        (right (Sets.unions (list [var "dataNames", var "schemaNames"]))))) $
  Eithers.map ("namesList" ~> Sets.fromList (Maybes.cat (Lists.map (Names.namespaceOf) (
      Sets.toList (Sets.delete (Constants.placeholderName) (Sets.unions (var "namesList")))))))
    (Eithers.mapList (var "depNames") (var "els"))

dereferenceType :: TBinding (Context -> Graph -> Name -> Either (InContext OtherError) (Maybe Type))
dereferenceType = define "dereferenceType" $
  doc "Dereference a type name to get the actual type (Either version)" $
  "cx" ~> "graph" ~> "name" ~>
  "mel" <~ Lexical.dereferenceElement @@ var "graph" @@ var "name" $
  optCases (var "mel")
    (right nothing)
    ("el" ~> Eithers.map (unaryFunction just)
      (Ctx.withContext (var "cx")
        (Eithers.bimap ("_e" ~> Error.otherError (Error.unDecodingError @@ var "_e")) ("_a" ~> var "_a")
          (decoderFor _Type @@ var "graph" @@ Core.bindingTerm (var "el")))))

elementAsTypeApplicationTerm :: TBinding (Context -> Binding -> Either (InContext OtherError) TypeApplicationTerm)
elementAsTypeApplicationTerm = define "elementAsTypeApplicationTerm" $
  doc "Convert an element to a typed term" $
  "cx" ~> "el" ~>
  Maybes.maybe (Ctx.failInContext (Error.otherError (string "missing element type")) (var "cx"))
    ("ts" ~> right (Core.typeApplicationTerm (Core.bindingTerm (var "el")) (Core.typeSchemeType (var "ts"))))
    (Core.bindingType (var "el"))

elementsWithDependencies :: TBinding (Context -> Graph -> [Binding] -> Either (InContext OtherError) [Binding])
elementsWithDependencies = define "elementsWithDependencies" $
  doc "Get elements with their dependencies" $
  "cx" ~> "graph" ~> "original" ~>
  "depNames" <~ ("el" ~> Sets.toList (Rewriting.termDependencyNames @@ true @@ false @@ false @@ (Core.bindingTerm (var "el")))) $
  "allDepNames" <~ Lists.nub (Lists.concat2
    (Lists.map (unaryFunction Core.bindingName) (var "original"))
    (Lists.concat (Lists.map (var "depNames") (var "original")))) $
  Eithers.mapList ("name" ~> Lexical.requireElement @@ var "cx" @@ var "graph" @@ var "name") (var "allDepNames")

extendGraphForLambda :: TBinding (Graph -> Lambda -> Graph)
extendGraphForLambda = define "extendGraphForLambda" $
  doc "Extend a graph by descending into a lambda body" $
  "g" ~> "lam" ~>
  "var" <~ Core.lambdaParameter (var "lam") $
  Graph.graph
    (Graph.graphBoundTerms $ var "g")
    (optCases (Core.lambdaDomain $ var "lam")
      (Graph.graphBoundTypes $ var "g")
      ("dom" ~> Maps.insert (var "var") (Rewriting.fTypeToTypeScheme @@ var "dom") $ Graph.graphBoundTypes $ var "g"))
    (Graph.graphClassConstraints $ var "g")
    (Sets.insert (var "var") $ Graph.graphLambdaVariables $ var "g")
    (Maps.delete (var "var") $ Graph.graphMetadata $ var "g")
    (Graph.graphPrimitives $ var "g")
    (Graph.graphSchemaTypes $ var "g")
    (Graph.graphTypeVariables $ var "g")

extendGraphForLet :: TBinding ((Graph -> Binding -> Maybe Term) -> Graph -> Let -> Graph)
extendGraphForLet = define "extendGraphForLet" $
  doc "Extend a graph by descending into a let body" $
  "forBinding" ~> "g" ~> "letrec" ~>
  "bindings" <~ Core.letBindings (var "letrec") $
  -- Pre-extend graph with sibling bindings so forBinding can resolve them
  "g2" <~ (Lexical.extendGraphWithBindings @@ var "bindings" @@ var "g") $
  Graph.graph
    -- Add all binding terms
    (Maps.union
      (Maps.fromList $ Lists.map ("b" ~> pair (Core.bindingName $ var "b") (Core.bindingTerm $ var "b")) (var "bindings"))
      (Graph.graphBoundTerms $ var "g"))
    -- Add typed binding type schemes; untyped bindings are not added, so outer types are shadowed by union precedence
    (Maps.union
      (Maps.fromList $ Maybes.cat $ Lists.map
        ("b" ~> Maybes.map ("ts" ~> pair (Core.bindingName $ var "b") (var "ts"))
          (Core.bindingType $ var "b"))
        (var "bindings"))
      (Graph.graphBoundTypes $ var "g"))
    (Graph.graphClassConstraints $ var "g")
    -- Remove all binding names from lambda variables; they are shadowed
    (Lists.foldl ("s" ~> "b" ~> Sets.delete (Core.bindingName $ var "b") (var "s"))
      (Graph.graphLambdaVariables $ var "g")
      (var "bindings"))
    -- Update metadata per binding, accumulating a full graph so each binding sees earlier siblings' metadata
    (Graph.graphMetadata $ Lists.foldl
      ("gAcc" ~> "b" ~>
        "m" <~ (Graph.graphMetadata $ var "gAcc") $
        "newMeta" <~ (optCases (var "forBinding" @@ var "gAcc" @@ var "b")
          (Maps.delete (Core.bindingName $ var "b") (var "m"))
          ("t" ~> Maps.insert (Core.bindingName $ var "b") (var "t") (var "m"))) $
        Graph.graph
          (Graph.graphBoundTerms $ var "gAcc")
          (Graph.graphBoundTypes $ var "gAcc")
          (Graph.graphClassConstraints $ var "gAcc")
          (Graph.graphLambdaVariables $ var "gAcc")
          (var "newMeta")
          (Graph.graphPrimitives $ var "gAcc")
          (Graph.graphSchemaTypes $ var "gAcc")
          (Graph.graphTypeVariables $ var "gAcc"))
      (var "g2")
      (var "bindings"))
    (Graph.graphPrimitives $ var "g")
    (Graph.graphSchemaTypes $ var "g")
    (Graph.graphTypeVariables $ var "g")

extendGraphForTypeLambda :: TBinding (Graph -> TypeLambda -> Graph)
extendGraphForTypeLambda = define "extendGraphForTypeLambda" $
  doc "Extend a graph by descending into a type lambda body" $
  "g" ~> "tlam" ~>
  "name" <~ Core.typeLambdaParameter (var "tlam") $
  Graph.graph
    (Graph.graphBoundTerms $ var "g")
    (Graph.graphBoundTypes $ var "g")
    (Graph.graphClassConstraints $ var "g")
    (Graph.graphLambdaVariables $ var "g")
    (Graph.graphMetadata $ var "g")
    (Graph.graphPrimitives $ var "g")
    (Graph.graphSchemaTypes $ var "g")
    (Sets.insert (var "name") $ Graph.graphTypeVariables $ var "g")

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

fieldTypes :: TBinding (Context -> Graph -> Type -> Either (InContext OtherError) (M.Map Name Type))
fieldTypes = define "fieldTypes" $
  doc "Get field types from a record or union type (Either version)" $
  "cx" ~> "graph" ~> "t" ~>
  "toMap" <~ ("fields" ~> Maps.fromList (Lists.map
    ("ft" ~> pair (Core.fieldTypeName (var "ft")) (Core.fieldTypeType (var "ft")))
    (var "fields"))) $
  match _Type (Just (Ctx.failInContext (Error.otherError (
    Strings.cat (list [string "expected record or union type but found ", ShowCore.type_ @@ var "t"]))) (var "cx"))) [
    _Type_forall>>: "ft" ~> fieldTypes @@ var "cx" @@ var "graph" @@ Core.forallTypeBody (var "ft"),
    _Type_record>>: "rt" ~> right (var "toMap" @@ Core.rowTypeFields (var "rt")),
    _Type_union>>: "rt" ~> right (var "toMap" @@ Core.rowTypeFields (var "rt")),
    _Type_variable>>: "name" ~>
      Eithers.bind (Lexical.requireElement @@ var "cx" @@ var "graph" @@ var "name") (
        "el" ~>
        Eithers.bind (Ctx.withContext (var "cx")
          (Eithers.bimap ("_e" ~> Error.otherError (Error.unDecodingError @@ var "_e")) ("_a" ~> var "_a")
            (decoderFor _Type @@ var "graph" @@ Core.bindingTerm (var "el")))) (
          "decodedType" ~> fieldTypes @@ var "cx" @@ var "graph" @@ var "decodedType"))]
  @@ (Rewriting.deannotateType @@ var "t")

findFieldType :: TBinding (Context -> Name -> [FieldType] -> Either (InContext OtherError) Type)
findFieldType = define "findFieldType" $
  doc "Find a field type by name in a list of field types" $
  "cx" ~> "fname" ~> "fields" ~>
  "matchingFields" <~ Lists.filter
    ("ft" ~> Equality.equal (Core.unName (Core.fieldTypeName (var "ft"))) (Core.unName (var "fname")))
    (var "fields") $
  Logic.ifElse (Lists.null (var "matchingFields"))
    (Ctx.failInContext (Error.otherError (Strings.cat2 (string "No such field: ") (Core.unName (var "fname")))) (var "cx"))
    (Logic.ifElse (Equality.equal (Lists.length (var "matchingFields")) (int32 1))
      (right (Core.fieldTypeType (Lists.head (var "matchingFields"))))
      (Ctx.failInContext (Error.otherError (Strings.cat2 (string "Multiple fields named ") (Core.unName (var "fname")))) (var "cx")))

fTypeIsPolymorphic :: TBinding (Type -> Bool)
fTypeIsPolymorphic = define "fTypeIsPolymorphic" $
  doc "Test whether a given System F type is polymorphic (i.e., a forall type)" $
  "typ" ~> cases _Type (var "typ")
    (Just false) [
    _Type_annotated>>: "at" ~> fTypeIsPolymorphic @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~> true]

freshName :: TBinding (Context -> (Name, Context))
freshName = define "freshName" $
  doc "Generate a fresh type variable name, threading Context" $
  "cx" ~>
  "count" <~ Annotations.getCount @@ Constants.key_freshTypeVariableCount @@ var "cx" $
  pair
    (normalTypeVariable @@ var "count")
    (Annotations.putCount @@ Constants.key_freshTypeVariableCount @@ Math.add (var "count") (int32 1) @@ var "cx")

freshNames :: TBinding (Int -> Context -> ([Name], Context))
freshNames = define "freshNames" $
  doc "Generate multiple fresh type variable names, threading Context" $
  "n" ~> "cx" ~>
  -- Fold over n units, accumulating names and threading context
  "go" <~ ("acc" ~> "_" ~>
    "names" <~ Pairs.first (var "acc") $
    "cx0" <~ Pairs.second (var "acc") $
    "result" <~ freshName @@ var "cx0" $
    "name" <~ Pairs.first (var "result") $
    "cx1" <~ Pairs.second (var "result") $
    pair (Lists.concat2 (var "names") (Lists.pure (var "name"))) (var "cx1")) $
  Lists.foldl (var "go") (pair (list ([] :: [TTerm Name])) (var "cx")) (Lists.replicate (var "n") unit)

fullyStripAndNormalizeType :: TBinding (Type -> Type)
fullyStripAndNormalizeType = define "fullyStripAndNormalizeType" $
  doc "Fully strip a type of forall quantifiers, normalizing bound variable names for alpha-equivalence comparison" $
  "typ" ~>
  -- Collect forall-bound variables and the body in one pass
  "go" <~ ("depth" ~> "subst" ~> "t" ~> cases _Type (Rewriting.deannotateType @@ var "t")
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
  Rewriting.substituteTypeVariables @@ var "subst" @@ var "body"

fullyStripType :: TBinding (Type -> Type)
fullyStripType = define "fullyStripType" $
  doc "Fully strip a type of forall quantifiers" $
  "typ" ~>
  match _Type (Just (var "typ")) [
    _Type_forall>>: "ft" ~> fullyStripType @@ Core.forallTypeBody (var "ft")]
  @@ (Rewriting.deannotateType @@ var "typ")

graphAsLet :: TBinding ([Binding] -> Term -> Let)
graphAsLet = define "graphAsLet" $
  doc "Convert bindings and a body to a let expression" $
  "bindings" ~> "body" ~>
  Core.let_
    (var "bindings")
    (var "body")

graphAsTerm :: TBinding ([Binding] -> Term -> Term)
graphAsTerm = define "graphAsTerm" $
  doc "Convert bindings and a body to a term, using let-term duality" $
  "bindings" ~> "body" ~> Core.termLet (graphAsLet @@ var "bindings" @@ var "body")

graphAsTypes :: TBinding (Context -> Graph -> [Binding] -> Either (InContext DecodingError) (M.Map Name Type))
graphAsTypes = define "graphAsTypes" $
  doc "Decode a list of type-encoding bindings into a map of named types" $
  "cx" ~> "graph" ~> "els" ~>
  "toPair" <~ ("el" ~>
    Eithers.map
      ("typ" ~> pair (Core.bindingName $ var "el") (var "typ"))
      (Ctx.withContext (var "cx") (decoderFor _Type @@ var "graph" @@ (Core.bindingTerm $ var "el")))) $
  Eithers.map (unaryFunction Maps.fromList) (Eithers.mapList (var "toPair") (var "els"))

instantiateType :: TBinding (Context -> Type -> (Type, Context))
instantiateType = define "instantiateType" $
  doc "Instantiate a type by replacing all forall-bound type variables with fresh variables, threading Context" $
  "cx" ~> "typ" ~>
  "result" <~ instantiateTypeScheme @@ var "cx" @@ (typeToTypeScheme @@ var "typ") $
  pair (Rewriting.typeSchemeToFType @@ Pairs.first (var "result")) (Pairs.second (var "result"))

instantiateTypeScheme :: TBinding (Context -> TypeScheme -> (TypeScheme, Context))
instantiateTypeScheme = define "instantiateTypeScheme" $
  doc "Instantiate a type scheme with fresh variables, threading Context" $
  "cx" ~> "scheme" ~>
  "oldVars" <~ Core.typeSchemeVariables (var "scheme") $
  "result" <~ freshNames @@ Lists.length (var "oldVars") @@ var "cx" $
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

isEncodedTerm :: TBinding (Term -> Bool)
isEncodedTerm = define "isEncodedTerm" $
  doc "Determines whether a given term is an encoded term (meta-level term)" $
  "t" ~> cases _Term (Rewriting.deannotateTerm @@ var "t") (Just false) [
    _Term_application>>: "a" ~>
      isEncodedTerm @@ (Core.applicationFunction (var "a")),
    _Term_union>>: "i" ~>
      Equality.equal (string (unName _Term)) (Core.unName (Core.injectionTypeName (var "i")))]

isEnumType :: TBinding (Type -> Bool)
isEnumType = define "isEnumType" $
  doc "Check if a type is an enum type" $
  "typ" ~>
  match _Type (Just false) [
    _Type_union>>: "rt" ~> isEnumRowType @@ var "rt"]
  @@ (Rewriting.deannotateType @@ var "typ")

isSerializable :: TBinding (Context -> Graph -> Binding -> Either (InContext OtherError) Bool)
isSerializable = define "isSerializable" $
  doc "Check if an element is serializable (no function types in dependencies) (Either version)" $
  "cx" ~> "graph" ~> "el" ~>
  "variants" <~ ("typ" ~>
    Lists.map (Reflect.typeVariant) (Rewriting.foldOverType @@ Coders.traversalOrderPre @@
      ("m" ~> "t" ~> Lists.cons (var "t") (var "m")) @@ list ([] :: [TTerm Type]) @@ var "typ")) $
  Eithers.map
    ("deps" ~>
      "allVariants" <~ Sets.fromList (Lists.concat (Lists.map (var "variants") (Maps.elems (var "deps")))) $
      Logic.not (Sets.member Variants.typeVariantFunction (var "allVariants")))
    (typeDependencies @@ var "cx" @@ var "graph" @@ false @@ (unaryFunction Equality.identity) @@ Core.bindingName (var "el"))

isSerializableType :: TBinding (Type -> Bool)
isSerializableType = define "isSerializableType" $
  doc "Check if a type is serializable (no function types in the type itself)" $
  "typ" ~>
  "allVariants" <~ Sets.fromList (Lists.map (Reflect.typeVariant)
    (Rewriting.foldOverType @@ Coders.traversalOrderPre @@
      ("m" ~> "t" ~> Lists.cons (var "t") (var "m")) @@ list ([] :: [TTerm Type]) @@ var "typ")) $
  Logic.not (Sets.member Variants.typeVariantFunction (var "allVariants"))

isSerializableByName :: TBinding (Context -> Graph -> Name -> Either (InContext OtherError) Bool)
isSerializableByName = define "isSerializableByName" $
  doc "Check if a type (by name) is serializable, resolving all type dependencies (Either version)" $
  "cx" ~> "graph" ~> "name" ~>
  "variants" <~ ("typ" ~>
    Lists.map (Reflect.typeVariant) (Rewriting.foldOverType @@ Coders.traversalOrderPre @@
      ("m" ~> "t" ~> Lists.cons (var "t") (var "m")) @@ list ([] :: [TTerm Type]) @@ var "typ")) $
  Eithers.map
    ("deps" ~>
      "allVariants" <~ Sets.fromList (Lists.concat (Lists.map (var "variants") (Maps.elems (var "deps")))) $
      Logic.not (Sets.member Variants.typeVariantFunction (var "allVariants")))
    (typeDependencies @@ var "cx" @@ var "graph" @@ false @@ (unaryFunction Equality.identity) @@ var "name")

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

moduleContainsBinaryLiterals :: TBinding (Module -> Bool)
moduleContainsBinaryLiterals = define "moduleContainsBinaryLiterals" $
  doc "Check whether a module contains any binary literal values" $
  "mod" ~>
  "checkTerm" <~ ("found" ~> "term" ~> Logic.or (var "found") $
    cases _Term (var "term") (Just false) [
      _Term_literal>>: "lit" ~>
        cases _Literal (var "lit") (Just false) [
          _Literal_binary>>: constant true]]) $
  "termContainsBinary" <~ ("term" ~>
    Rewriting.foldOverTerm @@ Coders.traversalOrderPre @@ var "checkTerm" @@ false @@ var "term") $
  Lists.foldl
    ("acc" ~> "el" ~> Logic.or (var "acc") (var "termContainsBinary" @@ Core.bindingTerm (var "el")))
    false
    (Module.moduleElements (var "mod"))

moduleDependencyNamespaces :: TBinding (Context -> Graph -> Bool -> Bool -> Bool -> Bool -> Module -> Either (InContext OtherError) (S.Set Namespace))
moduleDependencyNamespaces = define "moduleDependencyNamespaces" $
  doc "Find dependency namespaces in all elements of a module, excluding the module's own namespace (Either version)" $
  "cx" ~> "graph" ~> "binds" ~> "withPrims" ~> "withNoms" ~> "withSchema" ~> "mod" ~>
  Eithers.map
    ("deps" ~> Sets.delete (Module.moduleNamespace (var "mod")) (var "deps"))
    (dependencyNamespaces @@ var "cx" @@ var "graph" @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "withSchema" @@
      Module.moduleElements (var "mod"))

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

requireRecordType :: TBinding (Context -> Graph -> Name -> Either (InContext OtherError) RowType)
requireRecordType = define "requireRecordType" $
  doc "Require a name to resolve to a record type" $
  "cx" ~> "graph" ~> "name" ~>
  "toRecord" <~ ("t" ~> cases _Type (var "t") (Just nothing) [
    _Type_record>>: "rt" ~> just (var "rt")]) $
  requireRowType @@ var "cx" @@ string "record type" @@ var "toRecord" @@ var "graph" @@ var "name"

requireRowType :: TBinding (Context -> String -> (Type -> Maybe RowType) -> Graph -> Name -> Either (InContext OtherError) RowType)
requireRowType = define "requireRowType" $
  doc "Require a name to resolve to a row type" $
  "cx" ~> "label" ~> "getter" ~> "graph" ~> "name" ~>
  "rawType" <~ ("t" ~> cases _Type (var "t") (Just (var "t")) [
    _Type_annotated>>: "at" ~> var "rawType" @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~> var "rawType" @@ Core.forallTypeBody (var "ft")]) $
  Eithers.bind (requireType @@ var "cx" @@ var "graph" @@ var "name") (
    "t" ~>
    Maybes.maybe
      (Ctx.failInContext (Error.otherError (Strings.cat (list [
        Core.unName (var "name"),
        (string " does not resolve to a "),
        var "label",
        (string " type: "),
        ShowCore.type_ @@ var "t"]))) (var "cx"))
      (unaryFunction right)
      (var "getter" @@ (var "rawType" @@ var "t")))

requireSchemaType :: TBinding (Context -> M.Map Name TypeScheme -> Name -> Either (InContext OtherError) (TypeScheme, Context))
requireSchemaType = define "requireSchemaType" $
  doc "Look up a schema type and instantiate it, threading Context" $
  "cx" ~> "types" ~> "tname" ~>
  Maybes.maybe
    (left $ Ctx.inContext
      (Error.otherError $ Strings.cat $ list [
        (string "No such schema type: "),
        Core.unName $ var "tname",
        (string ". Available types are: "),
        Strings.intercalate (string ", ") (Lists.map (unaryFunction Core.unName) $ Maps.keys $ var "types")])
      (var "cx"))
    ("ts" ~> right $ instantiateTypeScheme @@ var "cx" @@ (Rewriting.deannotateTypeSchemeRecursive @@ var "ts"))
    (Maps.lookup (var "tname") (var "types"))

requireType :: TBinding (Context -> Graph -> Name -> Either (InContext OtherError) Type)
requireType = define "requireType" $
  doc "Require a type by name" $
  "cx" ~> "graph" ~> "name" ~>
  -- Look up in schema types first, then fall back to bound types
  Maybes.maybe
    (Maybes.maybe
      (Ctx.failInContext (Error.otherError (Strings.cat2 (string "no such type: ") (Core.unName (var "name")))) (var "cx"))
      ("ts" ~> right (Rewriting.typeSchemeToFType @@ var "ts"))
      (Maps.lookup (var "name") (Graph.graphBoundTypes (var "graph"))))
    ("ts" ~> right (Rewriting.typeSchemeToFType @@ var "ts"))
    (Maps.lookup (var "name") (Graph.graphSchemaTypes (var "graph")))

requireUnionField_ :: TBinding (Context -> Graph -> Name -> Name -> Either (InContext OtherError) Type)
requireUnionField_ = define "requireUnionField" $
  doc "Require a field type from a union type" $
  "cx" ~> "graph" ~> "tname" ~> "fname" ~>
  "withRowType" <~ ("rt" ~>
    "matches" <~ (Lists.filter
      ("ft" ~> Equality.equal (Core.fieldTypeName $ var "ft") (var "fname"))
      (Core.rowTypeFields $ var "rt")) $
    Logic.ifElse (Lists.null $ var "matches")
      (Ctx.failInContext (Error.otherError $ Strings.cat $ list [
        string "no field \"",
        Core.unName (var "fname"),
        string "\" in union type \"",
        Core.unName (var "tname")]) (var "cx"))
      (right $ Core.fieldTypeType $ Lists.head $ var "matches")) $
  Eithers.bind (requireUnionType @@ var "cx" @@ var "graph" @@ var "tname") (var "withRowType")

requireUnionType :: TBinding (Context -> Graph -> Name -> Either (InContext OtherError) RowType)
requireUnionType = define "requireUnionType" $
  doc "Require a name to resolve to a union type" $
  "cx" ~> "graph" ~> "name" ~>
  "toUnion" <~ ("t" ~> cases _Type (var "t")
    (Just nothing) [
    _Type_union>>: "rt" ~> just (var "rt")]) $
  requireRowType @@ var "cx" @@ string "union" @@ var "toUnion" @@ var "graph" @@ var "name"

resolveType :: TBinding (Graph -> Type -> Maybe Type)
resolveType = define "resolveType" $
  doc "Resolve a type, dereferencing type variables" $
  "graph" ~> "typ" ~>
  match _Type (Just (just (var "typ"))) [
    _Type_variable>>: "name" ~>
      -- Look up in schema types first, then fall back to bound types
      Maybes.maybe
        (Maybes.map ("ts" ~> Rewriting.typeSchemeToFType @@ var "ts") (Maps.lookup (var "name") (Graph.graphBoundTypes (var "graph"))))
        ("ts" ~> just (Rewriting.typeSchemeToFType @@ var "ts"))
        (Maps.lookup (var "name") (Graph.graphSchemaTypes (var "graph")))]
  @@ (Rewriting.deannotateType @@ var "typ")

schemaGraphToTypingEnvironment :: TBinding (Context -> Graph -> Either (InContext OtherError) (M.Map Name TypeScheme))
schemaGraphToTypingEnvironment = define "schemaGraphToTypingEnvironment" $
  doc "Convert a schema graph to a typing environment (Either version)" $
  "cx" ~> "g" ~>
  "toTypeScheme" <~ ("vars" ~> "typ" ~> cases _Type (Rewriting.deannotateType @@ var "typ")
    (Just (Core.typeScheme (Lists.reverse (var "vars")) (var "typ") Phantoms.nothing)) [
    _Type_forall>>: "ft" ~> var "toTypeScheme"
      @@ Lists.cons (Core.forallTypeParameter (var "ft")) (var "vars")
      @@ Core.forallTypeBody (var "ft")]) $
  "decodeType" <~ ("term" ~>
    Ctx.withContext (var "cx")
      (Eithers.bimap ("_e" ~> Error.otherError (Error.unDecodingError @@ var "_e")) ("_a" ~> var "_a")
        (decoderFor _Type @@ var "g" @@ var "term"))) $
  "decodeTypeScheme" <~ ("term" ~>
    Ctx.withContext (var "cx")
      (Eithers.bimap ("_e" ~> Error.otherError (Error.unDecodingError @@ var "_e")) ("_a" ~> var "_a")
        (decoderFor _TypeScheme @@ var "g" @@ var "term"))) $
  "toPair" <~ ("el" ~>
    "forTerm" <~ ("term" ~> cases _Term (var "term") (Just (right nothing)) [
      _Term_record>>: "r" ~>
        Logic.ifElse
          (Equality.equal (Core.recordTypeName (var "r")) (Core.nameLift _TypeScheme))
          (Eithers.map (unaryFunction just) (var "decodeTypeScheme" @@ Core.bindingTerm (var "el")))
          (right nothing),
      _Term_union>>: "i" ~>
        Logic.ifElse (Equality.equal (Core.injectionTypeName (var "i")) (Core.nameLift _Type))
          (Eithers.map
            ("decoded" ~> just (var "toTypeScheme" @@ list ([] :: [TTerm Name]) @@ var "decoded"))
            (var "decodeType" @@ Core.bindingTerm (var "el")))
          (right nothing)]) $
    "mts" <<=  optCases (Core.bindingType (var "el"))
      (Eithers.map ("typ" ~> just $ Rewriting.fTypeToTypeScheme @@ var "typ") $ var "decodeType" @@ (Core.bindingTerm (var "el")))
      ("ts" ~> Logic.ifElse
        (Equality.equal (var "ts") (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable (Core.nameLift _TypeScheme)) Phantoms.nothing))
        (Eithers.map (unaryFunction just) (var "decodeTypeScheme" @@ Core.bindingTerm (var "el")))
        (Logic.ifElse
          (Equality.equal (var "ts") (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable (Core.nameLift _Type)) Phantoms.nothing))
          (Eithers.map ("decoded" ~> just (var "toTypeScheme" @@ list ([] :: [TTerm Name]) @@ var "decoded")) (var "decodeType" @@ Core.bindingTerm (var "el")))
          (var "forTerm" @@ (Rewriting.deannotateTerm @@ (Core.bindingTerm (var "el")))))) $
    right $ Maybes.map ("ts" ~> pair (Core.bindingName (var "el")) (var "ts")) (var "mts")) $
  Eithers.map ("mpairs" ~> Maps.fromList (Maybes.cat (var "mpairs")))
    (Eithers.mapList (var "toPair") (Lexical.graphToBindings @@ var "g"))

-- Note: this is lossy, as it throws away the term body
termAsBindings :: TBinding (Term -> [Binding])
termAsBindings = define "termAsBindings" $
  doc "Extract the bindings from a let term, or return an empty list for other terms" $
  "term" ~> cases _Term (Rewriting.deannotateTerm @@ var "term")
    (Just (list ([] :: [TTerm Binding]))) [
    _Term_let>>: "lt" ~> Core.letBindings (var "lt")]

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

typeDependencies :: TBinding (Context -> Graph -> Bool -> (Type -> Type) -> Name -> Either (InContext OtherError) (M.Map Name Type))
typeDependencies = define "typeDependencies" $
  doc "Get all type dependencies for a given type name (Either version)" $
  "cx" ~> "graph" ~> "withSchema" ~> "transform" ~> "name" ~>
  "requireType" <~ ("name" ~>
    "cx1" <~ Ctx.pushTrace (Strings.cat2 (string "type dependencies of ") (Core.unName (var "name"))) (var "cx") $
    Eithers.bind (Lexical.requireElement @@ var "cx1" @@ var "graph" @@ var "name") (
      "el" ~> Ctx.withContext (var "cx1")
        (Eithers.bimap ("_e" ~> Error.otherError (Error.unDecodingError @@ var "_e")) ("_a" ~> var "_a")
          (decoderFor _Type @@ var "graph" @@ Core.bindingTerm (var "el"))))) $
  "toPair" <~ ("name" ~>
    Eithers.map ("typ" ~> pair (var "name") (var "transform" @@ var "typ"))
      (var "requireType" @@ var "name")) $
  "deps" <~ ("seeds" ~> "names" ~>
    Logic.ifElse (Sets.null (var "seeds"))
      (right (var "names"))
      (Eithers.bind (Eithers.mapList (var "toPair") (Sets.toList (var "seeds"))) (
        "pairs" ~>
        "newNames" <~ Maps.union (var "names") (Maps.fromList (var "pairs")) $
        "refs" <~ Lists.foldl (binaryFunction Sets.union) Sets.empty (Lists.map
          ("pair" ~> Rewriting.typeDependencyNames @@ var "withSchema" @@ Pairs.second (var "pair"))
          (var "pairs")) $
        "visited" <~ Sets.fromList (Maps.keys (var "names")) $
        "newSeeds" <~ Sets.difference (var "refs") (var "visited") $
        var "deps" @@ var "newSeeds" @@ var "newNames"))) $
  var "deps" @@ Sets.singleton (var "name") @@ Maps.empty

typeToTypeScheme :: TBinding (Type -> TypeScheme)
typeToTypeScheme = define "typeToTypeScheme" $
  doc "Convert a (System F -style) type to a type scheme" $
  "t0" ~>
  "helper" <~ ("vars" ~> "t" ~> cases _Type (Rewriting.deannotateType @@ var "t")
    (Just $ Core.typeScheme (Lists.reverse $ var "vars") (var "t") Phantoms.nothing) [
    _Type_forall>>: "ft" ~> var "helper"
      @@ (Lists.cons (Core.forallTypeParameter $ var "ft") $ var "vars")
      @@ (Core.forallTypeBody $ var "ft")]) $
  var "helper" @@ list ([] :: [TTerm Name]) @@ var "t0"

typesToElements :: TBinding (M.Map Name Type -> [Binding])
typesToElements = define "typesToElements" $
  doc "Encode a map of named types to a list of elements" $
  "typeMap" ~>
  "toElement" <~ ("pair" ~>
    "name" <~ Pairs.first (var "pair") $
    Core.binding
      (var "name")
      (encoderFor _Type @@ (Pairs.second $ var "pair"))
      nothing) $
  Lists.map (var "toElement") $ Maps.toList $ var "typeMap"

withLambdaContext :: TBinding ((e -> Graph) -> (Graph -> e -> f) -> e -> Lambda -> (f -> a) -> a)
withLambdaContext = define "withLambdaContext" $
  doc "Execute a computation in the context of a lambda body, extending the type context with the lambda parameter" $
  "getContext" ~> "setContext" ~> "env" ~> "lam" ~> "body" ~>
  "newContext" <~ extendGraphForLambda @@ (var "getContext" @@ var "env") @@ var "lam" $
  var "body" @@ (var "setContext" @@ var "newContext" @@ var "env")

withLetContext :: TBinding ((e -> Graph) -> (Graph -> e -> f) -> (Graph -> Binding -> Maybe Term) -> e -> Let -> (f -> a) -> a)
withLetContext = define "withLetContext" $
  doc "Execute a computation in the context of a let body, extending the type context with the let bindings" $
  "getContext" ~> "setContext" ~> "forBinding" ~> "env" ~> "letrec" ~> "body" ~>
  "newContext" <~ extendGraphForLet @@ var "forBinding" @@ (var "getContext" @@ var "env") @@ var "letrec" $
  var "body" @@ (var "setContext" @@ var "newContext" @@ var "env")

withTypeLambdaContext :: TBinding ((e -> Graph) -> (Graph -> e -> f) -> e -> TypeLambda -> (f -> a) -> a)
withTypeLambdaContext = define "withTypeLambdaContext" $
  doc "Execute a computation in the context of a type lambda body, extending the type context with the type parameter" $
  "getContext" ~> "setContext" ~> "env" ~> "tlam" ~> "body" ~>
  "newContext" <~ extendGraphForTypeLambda @@ (var "getContext" @@ var "env") @@ var "tlam" $
  var "body" @@ (var "setContext" @@ var "newContext" @@ var "env")
