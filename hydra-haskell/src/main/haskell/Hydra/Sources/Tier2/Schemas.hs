{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Schemas where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Encode.Core as EncodeCore
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Functions    as Functions
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Messages     as Messages
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
import qualified Hydra.Sources.Tier2.Errors as Errors
import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.TermEncoding as TermEncoding
--import qualified Hydra.Sources.Tier2.Unification as Unification
import qualified Hydra.Sources.Tier2.Variants as Variants


schemasDefinition :: String -> TTerm a -> TElement a
schemasDefinition = definitionInModule hydraSchemasModule

hydraSchemasModule :: Module
hydraSchemasModule = Module (Namespace "hydra.schemas") elements
    [DecodeCore.decodeCoreModule, EncodeCore.encodeCoreModule, Qnames.hydraQnamesModule, Rewriting.hydraRewritingModule,
      ShowCore.showCoreModule, Sorting.hydraSortingModule]
    [Tier1.hydraCodersModule, Tier1.hydraModuleModule, Tier1.hydraTopologyModule] $
    Just ("Various functions for dereferencing and decoding schema types.")
  where
   elements = [
     el definitionDependencyNamespacesDef,
     el dependencyNamespacesDef,
     el dereferenceTypeDef,
     el elementAsTypedTermDef,
     el findFieldTypeDef,
     el fieldTypesDef,
     el fullyStripTypeDef,
     el isEnumRowTypeDef,
     el isEnumTypeDef,
     el isSerializableDef,
     el moduleDependencyNamespacesDef,
     el namespacesForDefinitionsDef,
     el requireRecordTypeDef,
     el requireRowTypeDef,
     el requireTypeDef,
     el requireUnionTypeDef,
     el resolveTypeDef,
     el schemaGraphToTypingEnvironmentDef,
     el topologicalSortTypeDefinitionsDef,
     el typeDependenciesDef]

definitionDependencyNamespacesDef :: TElement (Bool -> [Definition] -> S.Set Namespace)
definitionDependencyNamespacesDef = schemasDefinition "definitionDependencyNamespaces" $
  doc "Get dependency namespaces from definitions" $
  lambdas ["excludeUnit", "defs"] $ lets [
    "defNames">: lambda "def" $
      match _Definition Nothing [
        _Definition_type>>: lambda "typeDef" $
          ref Rewriting.typeDependencyNamesDef @@ true @@ var "excludeUnit" @@ Module.typeDefinitionType (var "typeDef"),
        _Definition_term>>: lambda "termDef" $
          ref Rewriting.termDependencyNamesDef @@ true @@ true @@ true @@ Module.termDefinitionTerm (var "termDef")]
      @@ var "def",
    "allNames">: Sets.unions $ Lists.map (var "defNames") (var "defs")]
    $ Sets.fromList $ Optionals.cat $ Lists.map (ref Qnames.namespaceOfDef) (Sets.toList $ var "allNames")

dependencyNamespacesDef :: TElement (Bool -> Bool -> Bool -> Bool -> [Element] -> Flow Graph (S.Set Namespace))
dependencyNamespacesDef = schemasDefinition "dependencyNamespaces" $
  doc "Find dependency namespaces in all of a set of terms" $
  lambdas ["withVars", "withPrims", "withNoms", "withSchema", "els"] $ lets [
    "depNames">: lambda "el" $ lets [
      "term">: Graph.elementTerm $ var "el",
      "dataNames">: ref Rewriting.termDependencyNamesDef @@ var "withVars" @@ var "withPrims" @@ var "withNoms" @@ var "term",
      "schemaNames">: Logic.ifElse (var "withSchema")
        (Optionals.maybe Sets.empty
          (lambda "ts" $ ref Rewriting.typeDependencyNamesDef @@ true @@ true @@ Core.typeSchemeType (var "ts"))
          (Graph.elementType $ var "el"))
        Sets.empty]
      $ Logic.ifElse (ref EncodeCore.isEncodedTypeDef @@ (ref Strip.fullyStripTermDef @@ var "term"))
          (Flows.bind (ref DecodeCore.typeDef @@ var "term") $
            lambda "typ" $ Flows.pure $ Sets.unions $ list [
              var "dataNames", var "schemaNames",
              ref Rewriting.typeDependencyNamesDef @@ true @@ true @@ var "typ"])
          (Flows.pure $ Sets.unions $ list [var "dataNames", var "schemaNames"])]
    $ Flows.bind (Flows.mapList (var "depNames") (var "els")) $
      lambda "namesList" $ Flows.pure $ Sets.fromList $ Optionals.cat $ Lists.map (ref Qnames.namespaceOfDef) $
        Sets.toList $ Sets.delete (ref Constants.placeholderNameDef) $ Sets.unions $ var "namesList"

dereferenceTypeDef :: TElement (Name -> Flow Graph (Maybe Type))
dereferenceTypeDef = schemasDefinition "dereferenceType" $
  doc "Dereference a type name to get the actual type" $
  lambda "name" $
    Flows.bind (ref Lexical.dereferenceElementDef @@ var "name") $
      lambda "mel" $
        Optionals.maybe (Flows.pure nothing)
          (lambda "el" $ Flows.map (unaryFunction just) $ ref DecodeCore.typeDef @@ Graph.elementTerm (var "el"))
          (var "mel")

elementAsTypedTermDef :: TElement (Element -> Flow Graph TypedTerm)
elementAsTypedTermDef = schemasDefinition "elementAsTypedTerm" $
  doc "Convert an element to a typed term" $
  lambda "el" $
    Optionals.maybe (Flows.fail $ string "missing element type")
      (lambda "ts" $ Flows.pure $ Core.typedTerm (Graph.elementTerm $ var "el") (Core.typeSchemeType $ var "ts"))
      (Graph.elementType $ var "el")

findFieldTypeDef :: TElement (Name -> [FieldType] -> Flow s Type)
findFieldTypeDef = schemasDefinition "findFieldType" $
  doc "Find a field type by name in a list of field types" $
  lambda "fname" $ lambda "fields" $ lets [
    "matchingFields">: Lists.filter
      (lambda "ft" $ Equality.equalString (Core.unName $ Core.fieldTypeName $ var "ft") (Core.unName $ var "fname"))
      (var "fields")]
    $ Logic.ifElse (Lists.null $ var "matchingFields")
        (Flows.fail $ Strings.cat2 (string "No such field: ") (Core.unName $ var "fname"))
        (Logic.ifElse (Equality.equalInt32 (Lists.length $ var "matchingFields") (int32 1))
          (Flows.pure $ Core.fieldTypeType $ Lists.head $ var "matchingFields")
          (Flows.fail $ Strings.cat2 (string "Multiple fields named ") (Core.unName $ var "fname")))

fieldTypesDef :: TElement (Type -> Flow Graph (M.Map Name Type))
fieldTypesDef = schemasDefinition "fieldTypes" $
  doc "Get field types from a record or union type" $
  lambda "t" $ lets [
    "toMap">: lambda "fields" $ Maps.fromList $ Lists.map
      (lambda "ft" $ pair (Core.fieldTypeName $ var "ft") (Core.fieldTypeType $ var "ft"))
      (var "fields")]
    $ match _Type (Just $ ref Errors.unexpectedDef @@ string "record or union type" @@ (ref ShowCore.typeDef @@ var "t")) [
      _Type_forall>>: lambda "ft" $ ref fieldTypesDef @@ Core.forallTypeBody (var "ft"),
      _Type_record>>: lambda "rt" $ Flows.pure $ var "toMap" @@ Core.rowTypeFields (var "rt"),
      _Type_union>>: lambda "rt" $ Flows.pure $ var "toMap" @@ Core.rowTypeFields (var "rt"),
      _Type_variable>>: lambda "name" $
        ref Monads.withTraceDef @@ (Strings.cat2 (string "field types of ") (Core.unName $ var "name")) @@
          (Flows.bind (ref Lexical.requireElementDef @@ var "name") $
            lambda "el" $
              Flows.bind (ref DecodeCore.typeDef @@ Graph.elementTerm (var "el")) $
                ref fieldTypesDef)]
    @@ (ref Strip.stripTypeDef @@ var "t")

fullyStripTypeDef :: TElement (Type -> Type)
fullyStripTypeDef = schemasDefinition "fullyStripType" $
  doc "Fully strip a type of forall quantifiers" $
  lambda "typ" $
    match _Type (Just $ var "typ") [
      _Type_forall>>: lambda "ft" $ ref fullyStripTypeDef @@ Core.forallTypeBody (var "ft")]
    @@ (ref Strip.stripTypeDef @@ var "typ")

isEnumRowTypeDef :: TElement (RowType -> Bool)
isEnumRowTypeDef = schemasDefinition "isEnumRowType" $
  doc "Check if a row type represents an enum (all fields are unit-typed)" $
  lambda "rt" $ Lists.foldl (binaryFunction Logic.and) true $
    Lists.map (lambda "f" $ ref EncodeCore.isUnitTypeDef @@ (Core.fieldTypeType $ var "f")) $
      Core.rowTypeFields $ var "rt"

isEnumTypeDef :: TElement (Type -> Bool)
isEnumTypeDef = schemasDefinition "isEnumType" $
  doc "Check if a type is an enum type" $
  lambda "typ" $
    match _Type (Just false) [
      _Type_union>>: lambda "rt" $ ref isEnumRowTypeDef @@ var "rt"]
    @@ (ref Strip.stripTypeDef @@ var "typ")

isSerializableDef :: TElement (Element -> Flow Graph Bool)
isSerializableDef = schemasDefinition "isSerializable" $
  doc "Check if an element is serializable (no function types in dependencies)" $
  lambda "el" $ lets [
    "variants">: lambda "typ" $
      Lists.map (ref Variants.typeVariantDef) $ ref Rewriting.foldOverTypeDef @@ Coders.traversalOrderPre @@
        (lambda "m" $ lambda "t" $ Lists.cons (var "t") (var "m")) @@ list [] @@ var "typ"]
    $ Flows.map
      (lambda "deps" $ lets [
        "allVariants">: Sets.fromList $ Lists.concat $ Lists.map (var "variants") $ Maps.elems $ var "deps"]
        $ Logic.not $ Sets.member Mantle.typeVariantFunction $ var "allVariants")
      (ref typeDependenciesDef @@ false @@ (unaryFunction Equality.identity) @@ Graph.elementName (var "el"))

moduleDependencyNamespacesDef :: TElement (Bool -> Bool -> Bool -> Bool -> Module -> Flow Graph (S.Set Namespace))
moduleDependencyNamespacesDef = schemasDefinition "moduleDependencyNamespaces" $
  doc "Find dependency namespaces in all elements of a module, excluding the module's own namespace" $
  lambdas ["withVars", "withPrims", "withNoms", "withSchema", "mod"] $
    Flows.bind (ref dependencyNamespacesDef @@ var "withVars" @@ var "withPrims" @@ var "withNoms" @@ var "withSchema" @@
      Module.moduleElements (var "mod")) $
      lambda "deps" $ Flows.pure $ Sets.delete (Module.moduleNamespace $ var "mod") (var "deps")

namespacesForDefinitionsDef :: TElement (Bool -> (Namespace -> a) -> Namespace -> [Definition] -> Namespaces a)
namespacesForDefinitionsDef = schemasDefinition "namespacesForDefinitions" $
  doc "Create namespaces mapping for definitions" $
  lambda "excludeUnit" $ lambda "encodeNamespace" $ lambda "focusNs" $ lambda "defs" $ lets [
    "nss">: Sets.delete (var "focusNs") $ ref definitionDependencyNamespacesDef @@ var "excludeUnit" @@ var "defs",
    "toPair">: lambda "ns" $ pair (var "ns") (var "encodeNamespace" @@ var "ns")]
    $ Module.namespaces (var "toPair" @@ var "focusNs") $ Maps.fromList $ Lists.map (var "toPair") $ Sets.toList $ var "nss"

requireRecordTypeDef :: TElement (Name -> Flow Graph RowType)
requireRecordTypeDef = schemasDefinition "requireRecordType" $
  doc "Require a name to resolve to a record type" $
  ref requireRowTypeDef @@ string "record type" @@
    (lambda "t" $
      cases _Type (var "t") (Just nothing) [
        _Type_record>>: lambda "rt" $ just $ var "rt"])

requireRowTypeDef :: TElement (String -> (Type -> Maybe RowType) -> Name -> Flow Graph RowType)
requireRowTypeDef = schemasDefinition "requireRowType" $
  doc "Require a name to resolve to a row type" $
  lambdas ["label", "getter", "name"] $ lets [
    "rawType">: lambda "t" $ cases _Type (var "t") (Just $ var "t") [
      _Type_annotated>>: lambda "at" $ var "rawType" @@ Core.annotatedTypeSubject (var "at"),
      _Type_forall>>: lambda "ft" $ var "rawType" @@ Core.forallTypeBody (var "ft")]]
    $ Flows.bind (ref requireTypeDef @@ var "name") $
      lambda "t" $
        Optionals.maybe
          (Flows.fail $ Strings.cat $ list [
            Core.unName $ var "name",
            string " does not resolve to a ",
            var "label",
            string " type: ",
            ref ShowCore.typeDef @@ var "t"])
          (unaryFunction Flows.pure)
          (var "getter" @@ (var "rawType" @@ var "t"))

requireTypeDef :: TElement (Name -> Flow Graph Type)
requireTypeDef = schemasDefinition "requireType" $
  doc "Require a type by name" $
  lambda "name" $
    ref Monads.withTraceDef @@ (Strings.cat2 (string "require type ") (Core.unName $ var "name")) @@
      (Flows.bind (ref Lexical.withSchemaContextDef @@ (ref Lexical.requireElementDef @@ var "name")) $
        lambda "el" $ ref DecodeCore.typeDef @@ Graph.elementTerm (var "el"))

requireUnionTypeDef :: TElement (Name -> Flow Graph RowType)
requireUnionTypeDef = schemasDefinition "requireUnionType" $
  doc "Require a name to resolve to a union type" $
  ref requireRowTypeDef @@ string "union" @@
    (lambda "t" $
      match _Type (Just nothing) [
        _Type_union>>: lambda "rt" $ just $ var "rt"]
      @@ var "t")

resolveTypeDef :: TElement (Type -> Flow Graph (Maybe Type))
resolveTypeDef = schemasDefinition "resolveType" $
  doc "Resolve a type, dereferencing type variables" $
  lambda "typ" $
    match _Type (Just $ Flows.pure $ just $ var "typ") [
      _Type_variable>>: lambda "name" $
        ref Lexical.withSchemaContextDef @@
          (Flows.bind (ref Lexical.resolveTermDef @@ var "name") $
            lambda "mterm" $
              Optionals.maybe (Flows.pure nothing)
                (lambda "t" $ Flows.map (unaryFunction just) $ ref DecodeCore.typeDef @@ var "t")
                (var "mterm"))]
    @@ (ref Strip.stripTypeDef @@ var "typ")

schemaGraphToTypingEnvironmentDef :: TElement (Graph -> Flow s (M.Map Name TypeScheme))
schemaGraphToTypingEnvironmentDef = schemasDefinition "schemaGraphToTypingEnvironment" $
  doc "Convert a schema graph to a typing environment" $
  lambda "g" $ lets [
    "toTypeScheme">: lambda "vars" $ lambda "typ" $
      match _Type (Just $ Core.typeScheme (Lists.reverse $ var "vars") (var "typ")) [
        _Type_forall>>: lambda "ft" $
          var "toTypeScheme" @@ Lists.cons (Core.forallTypeParameter $ var "ft") (var "vars") @@ Core.forallTypeBody (var "ft")]
      @@ (ref Strip.stripTypeDef @@ var "typ"),
    "toPair">: lambda "el" $
      Flows.map
        (lambda "mts" $ Optionals.map (lambda "ts" $ pair (Graph.elementName $ var "el") (var "ts")) (var "mts"))
        (Optionals.maybe
          (Flows.pure nothing)
          (lambda "ts" $
            Logic.ifElse
              (Equality.equal (var "ts") (Core.typeScheme (list []) (Core.typeVariable $ Core.nameLift _TypeScheme)))
              (Flows.map (unaryFunction just) $ ref DecodeCore.typeSchemeDef @@ Graph.elementTerm (var "el"))
              (Logic.ifElse
                (Equality.equal (var "ts") (Core.typeScheme (list []) (Core.typeVariable $ Core.nameLift _Type)))
                (Flows.map (lambda "decoded" $ just $ var "toTypeScheme" @@ list [] @@ var "decoded") $ ref DecodeCore.typeDef @@ Graph.elementTerm (var "el"))
                (cases _Term (ref Strip.fullyStripTermDef @@ (Graph.elementTerm $ var "el")) (Just $ Flows.pure nothing) [
                  _Term_record>>: lambda "r" $
                    Logic.ifElse
                      (Equality.equal (Core.recordTypeName $ var "r") (Core.nameLift _TypeScheme))
                      (Flows.map
                        (unaryFunction just)
                        (ref DecodeCore.typeSchemeDef @@ Graph.elementTerm (var "el")))
                      (Flows.pure nothing),
                  _Term_union>>: lambda "i" $
                    Logic.ifElse (Equality.equal (Core.injectionTypeName $ var "i") (Core.nameLift _Type))
                      (Flows.map
                        (lambda "decoded" $ just $ var "toTypeScheme" @@ list [] @@ var "decoded")
                        (ref DecodeCore.typeDef @@ Graph.elementTerm (var "el")))
                      (Flows.pure nothing)])))
          (Graph.elementType $ var "el"))]
    $ ref Monads.withStateDef @@ var "g" @@
      (Flows.bind (Flows.mapList (var "toPair") $ Maps.elems $ Graph.graphElements $ var "g") $
        lambda "mpairs" $ Flows.pure $ Maps.fromList $ Optionals.cat $ var "mpairs")

topologicalSortTypeDefinitionsDef :: TElement ([TypeDefinition] -> [[TypeDefinition]])
topologicalSortTypeDefinitionsDef = schemasDefinition "topologicalSortTypeDefinitions" $
  doc "Topologically sort type definitions by dependencies" $
  lambda "defs" $ lets [
    "toPair">: lambda "def" $ pair
      (Module.typeDefinitionName $ var "def")
      (Sets.toList $ ref Rewriting.typeDependencyNamesDef @@ false @@ true @@ Module.typeDefinitionType (var "def")),
    "nameToDef">: Maps.fromList $ Lists.map
      (lambda "d" $ pair (Module.typeDefinitionName $ var "d") (var "d"))
      (var "defs"),
    "sorted">: ref Sorting.topologicalSortComponentsDef @@ Lists.map (var "toPair") (var "defs")]
    $ Lists.map (lambda "names" $ Optionals.cat $ Lists.map (lambda "n" $ Maps.lookup (var "n") (var "nameToDef")) (var "names")) $
      var "sorted"

typeDependenciesDef :: TElement (Bool -> (Type -> Type) -> Name -> Flow Graph (M.Map Name Type))
typeDependenciesDef = schemasDefinition "typeDependencies" $
  doc "Get all type dependencies for a given type name" $
  lambda "withSchema" $ lambda "transform" $ lambda "name" $ lets [
    "requireType">: lambda "name" $
      ref Monads.withTraceDef @@ (Strings.cat2 (string "type dependencies of ") (Core.unName $ var "name")) @@
        (Flows.bind (ref Lexical.requireElementDef @@ var "name") $
          lambda "el" $ ref DecodeCore.typeDef @@ Graph.elementTerm (var "el")),
    "toPair">: lambda "name" $
      Flows.bind (var "requireType" @@ var "name") $
        lambda "typ" $ Flows.pure $ pair (var "name") (var "transform" @@ var "typ"),
    "deps">: lambda "seeds" $ lambda "names" $
      Logic.ifElse (Sets.null $ var "seeds")
        (Flows.pure $ var "names")
        (Flows.bind (Flows.mapList (var "toPair") $ Sets.toList $ var "seeds") $
          lambda "pairs" $ lets [
            "newNames">: Maps.union (var "names") $ Maps.fromList $ var "pairs",
            "refs">: Lists.foldl (binaryFunction Sets.union) Sets.empty $ Lists.map
              (lambda "pair" $ ref Rewriting.typeDependencyNamesDef @@ var "withSchema" @@ true @@ second (var "pair"))
              (var "pairs"),
            "visited">: Sets.fromList $ Maps.keys $ var "names",
            "newSeeds">: Sets.difference (var "refs") (var "visited")]
            $ var "deps" @@ var "newSeeds" @@ var "newNames")]
    $ var "deps" @@ Sets.singleton (var "name") @@ Maps.empty
