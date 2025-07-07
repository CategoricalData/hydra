{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Lexical where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors            as Accessors
import qualified Hydra.Dsl.Ast                  as Ast
import qualified Hydra.Dsl.Coders               as Coders
import qualified Hydra.Dsl.Compute              as Compute
import qualified Hydra.Dsl.Core                 as Core
import qualified Hydra.Dsl.Graph                as Graph
import qualified Hydra.Dsl.Lib.Chars            as Chars
import qualified Hydra.Dsl.Lib.Equality         as Equality
import qualified Hydra.Dsl.Lib.Flows            as Flows
import qualified Hydra.Dsl.Lib.Lists            as Lists
import qualified Hydra.Dsl.Lib.Literals         as Literals
import qualified Hydra.Dsl.Lib.Logic            as Logic
import qualified Hydra.Dsl.Lib.Maps             as Maps
import qualified Hydra.Dsl.Lib.Math             as Math
import qualified Hydra.Dsl.Lib.Optionals        as Optionals
import           Hydra.Dsl.Phantoms             as Phantoms
import qualified Hydra.Dsl.Lib.Sets             as Sets
import           Hydra.Dsl.Lib.Strings          as Strings
import qualified Hydra.Dsl.Mantle               as Mantle
import qualified Hydra.Dsl.Module               as Module
import qualified Hydra.Dsl.TTerms               as TTerms
import qualified Hydra.Dsl.TTypes               as TTypes
import qualified Hydra.Dsl.Terms                as Terms
import qualified Hydra.Dsl.Topology             as Topology
import qualified Hydra.Dsl.Types                as Types
import qualified Hydra.Dsl.Typing               as Typing
import qualified Hydra.Sources.Tier1.All        as Tier1
import qualified Hydra.Sources.Tier1.Constants  as Constants
import qualified Hydra.Sources.Tier1.Formatting as Formatting
import qualified Hydra.Sources.Tier1.Literals   as Literals
import qualified Hydra.Sources.Tier1.Strip      as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.Adapt.Literals as AdaptLiterals
--import qualified Hydra.Sources.Tier2.Adapt.Modules as AdaptModules
--import qualified Hydra.Sources.Tier2.Adapt.Terms as AdaptTerms
--import qualified Hydra.Sources.Tier2.Adapt.Utils as AdaptUtils
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.Decoding as Decoding
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Describe.Mantle as DescribeMantle
--import qualified Hydra.Sources.Tier2.Encode.Core as EncodeCore
--import qualified Hydra.Sources.Tier2.Errors as Errors
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
--import qualified Hydra.Sources.Tier2.Extract.Mantle as ExtractMantle
--import qualified Hydra.Sources.Tier2.Grammars as Grammars
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Languages as Languages
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.Names as Names
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Show.Graph as ShowGraph
--import qualified Hydra.Sources.Tier2.Show.Mantle as ShowMantle
--import qualified Hydra.Sources.Tier2.Show.Typing as ShowTyping
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templates as Templates
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants


lexicalDefinition :: String -> TTerm a -> TElement a
lexicalDefinition = definitionInModule hydraLexicalModule

hydraLexicalModule :: Module
hydraLexicalModule = Module (Namespace "hydra.lexical") elements
   [Monads.hydraMonadsModule, Strip.hydraStripModule, ShowCore.showCoreModule]
   [Tier1.hydraGraphModule, Tier1.hydraMantleModule] $
    Just ("A module for lexical operations over graphs.")
  where
    elements = [
      el dereferenceElementDef,
      el elementsToGraphDef,
      el emptyGraphDef,
      el extendGraphWithBindingsDef,
      el fieldsOfDef,
      el getFieldDef,
      el lookupElementDef,
      el lookupPrimitiveDef,
      el matchEnumDef,
      el matchRecordDef,
      el matchUnionDef,
      el matchUnitFieldDef,
      el requireElementDef,
      el requirePrimitiveDef,
      el requireTermDef,
      el resolveTermDef,
      el schemaContextDef,
      el stripAndDereferenceTermDef,
      el typeOfPrimitiveDef,
      el withSchemaContextDef]

dereferenceElementDef :: TElement (Name -> Flow Graph (Maybe Element))
dereferenceElementDef = lexicalDefinition "dereferenceElement" $
  lambda "name" $ Flows.map
    (lambda "g" $ ref lookupElementDef @@ var "g" @@ var "name")
    (ref Monads.getStateDef)

elementsToGraphDef :: TElement (Graph -> Maybe Graph -> [Element] -> Graph)
elementsToGraphDef = lexicalDefinition "elementsToGraph" $
  lambda "parent" $ lambda "schema" $ lambda "elements" $ lets [
    "toPair" >: lambda "el" $ pair (Graph.elementName $ var "el") (var "el")]
    $ Graph.graph
      (Maps.fromList (Lists.map (var "toPair") $ var "elements"))
      (Graph.graphEnvironment $ var "parent")
      (Graph.graphTypes $ var "parent")
      (Graph.graphBody $ var "parent")
      (Graph.graphPrimitives $ var "parent")
      (var "schema")

emptyGraphDef :: TElement Graph
emptyGraphDef = lexicalDefinition "emptyGraph" $
  doc "An empty graph; no elements, no primitives, no schema, and an arbitrary body." $
  Graph.graph
    Maps.empty
    Maps.empty
    Maps.empty
    (Core.termLiteral $ Core.literalString "empty graph")
    Maps.empty
    nothing

extendGraphWithBindingsDef :: TElement ([LetBinding] -> Graph -> Graph)
extendGraphWithBindingsDef = lexicalDefinition "extendGraphWithBindings" $
  lambdas ["bindings", "g"] $ lets [
    "newEls">: Maps.fromList $ Lists.map (var "toEl") (var "bindings"),
    "toEl">: lambda "binding" $ lets [
      "name">: Core.letBindingName $ var "binding",
      "term">: Core.letBindingTerm $ var "binding",
      "mts">: Core.letBindingType $ var "binding"]
      $ pair (var "name") (Graph.element (var "name") (var "term") (var "mts"))]
    $ Graph.graphWithElements (var "g") (Maps.union (var "newEls") (Graph.graphElements $ var "g"))

fieldsOfDef :: TElement (Type -> [FieldType])
fieldsOfDef = lexicalDefinition "fieldsOf" $
  lambda "t" $ lets [
    "stripped">: ref Strip.stripTypeDef @@ var "t"]
    $ cases _Type (var "stripped") (Just $ list []) [
      _Type_forall>>: lambda "forallType" $ ref fieldsOfDef @@ (Core.forallTypeBody $ var "forallType"),
      _Type_record>>: lambda "rt" $ Core.rowTypeFields $ var "rt",
      _Type_union>>: lambda "rt" $ Core.rowTypeFields $ var "rt"]

getFieldDef :: TElement (M.Map Name Term -> Name -> (Term -> Flow Graph b) -> Flow Graph b)
getFieldDef = lexicalDefinition "getField" $
  lambdas ["m", "fname", "decode"] $
    Optionals.maybe
      (Flows.fail $ "expected field " ++ (Core.unName $ var "fname") ++ " not found")
      (var "decode")
      (Maps.lookup (var "fname") (var "m"))

lookupElementDef :: TElement (Graph -> Name -> Maybe Element)
lookupElementDef = lexicalDefinition "lookupElement" $
  lambdas ["g", "name"] $ Maps.lookup (var "name") (Graph.graphElements $ var "g")

lookupPrimitiveDef :: TElement (Graph -> Name -> Maybe Primitive)
lookupPrimitiveDef = lexicalDefinition "lookupPrimitive" $
  lambda "g" $ lambda "name" $
    Maps.lookup (var "name") (Graph.graphPrimitives $ var "g")

matchEnumDef :: TElement (Name -> [(Name, b)] -> Term -> Flow Graph b)
matchEnumDef = lexicalDefinition "matchEnum" $
  lambdas ["tname", "pairs"] $
    ref matchUnionDef @@ var "tname" @@ (Lists.map (lambda "pair" $
      ref matchUnitFieldDef @@ (first $ var "pair") @@ (second $ var "pair")) $ var "pairs")

matchRecordDef :: TElement ((M.Map Name Term -> Flow Graph b) -> Term -> Flow Graph b)
matchRecordDef = lexicalDefinition "matchRecord" $
  lambdas ["decode", "term"] $ lets [
    "stripped">: ref Strip.stripTermDef @@ var "term"]
    $ cases _Term (var "stripped")
        (Just $ ref Monads.unexpectedDef @@ string "record" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_record>>: lambda "record" $ var "decode" @@
        (Maps.fromList $ Lists.map
          (lambda "field" $ pair (Core.fieldName $ var "field") (Core.fieldTerm $ var "field"))
          (Core.recordFields $ var "record"))]

matchUnionDef :: TElement (Name -> [(Name, Term -> Flow Graph b)] -> Term -> Flow Graph b)
matchUnionDef = lexicalDefinition "matchUnion" $
  lambdas ["tname", "pairs", "term"] $ lets [
    "stripped">: ref Strip.stripTermDef @@ var "term",
    "mapping">: Maps.fromList $ var "pairs"] $
    cases _Term (var "stripped")
      (Just $ ref Monads.unexpectedDef @@
        ("union with one of {" ++ (Strings.intercalate ", " $ Lists.map (lambda "pair" $ Core.unName $ first $ var "pair") $ var "pairs") ++ "}") @@
        (ref ShowCore.termDef @@ var "stripped")) [
      _Term_variable>>: lambda "name" $
        Flows.bind (ref requireElementDef @@ var "name") $
        lambda "el" $ ref matchUnionDef @@ var "tname" @@ var "pairs" @@ (Graph.elementTerm $ var "el"),
      _Term_union>>: lambda "injection" $
        Logic.ifElse (Core.equalName_ (Core.injectionTypeName $ var "injection") (var "tname"))
          (lets [
            "fname">: Core.fieldName $ Core.injectionField $ var "injection",
            "val">: Core.fieldTerm $ Core.injectionField $ var "injection"] $
            Optionals.maybe
              (Flows.fail $ "no matching case for field " ++ (Core.unName $ var "fname")
                ++ " in union type " ++ (Core.unName $ var "tname"))
              (lambda "f" $ var "f" @@ var "val")
              (Maps.lookup (var "fname") (var "mapping")))
          (ref Monads.unexpectedDef @@ ("injection for type " ++ (Core.unName $ var "tname")) @@ (ref ShowCore.termDef @@ var "term"))]

matchUnitFieldDef :: TElement (Name -> y -> (Name, x -> Flow Graph y))
matchUnitFieldDef = lexicalDefinition "matchUnitField" $
  lambdas ["fname", "x"] $ pair (var "fname") (lambda "ignored" $ Flows.pure $ var "x")

requireElementDef :: TElement (Name -> Flow Graph Element)
requireElementDef = lexicalDefinition "requireElement" $
  lambda "name" $ lets [
    "showAll">: false,
    "ellipsis">: lambda "strings" $
      Logic.ifElse (Logic.and (Equality.gt (Lists.length $ var "strings") (int32 3)) (Logic.not $ var "showAll"))
        (Lists.concat2 (Lists.take (int32 3) (var "strings")) (list [string "..."]))
        (var "strings"),
    "err">: lambda "g" $ Flows.fail $
      "no such element: " ++ (Core.unName $ var "name") ++
      ". Available elements: {" ++
      (Strings.intercalate ", " $ var "ellipsis" @@ (Lists.map (lambda "el" $ Core.unName $ Graph.elementName $ var "el") $ Maps.elems $ Graph.graphElements $ var "g")) ++
      "}"]
    $ Flows.bind (ref dereferenceElementDef @@ var "name") $
      lambda "mel" $ Optionals.maybe
        (Flows.bind (ref Monads.getStateDef) $ var "err")
        (unaryFunction Flows.pure)
        (var "mel")

requirePrimitiveDef :: TElement (Name -> Flow Graph Primitive)
requirePrimitiveDef = lexicalDefinition "requirePrimitive" $
  lambda "name" $
    Flows.bind (ref Monads.getStateDef) $
    lambda "g" $ Optionals.maybe
      (Flows.fail $ "no such primitive function: " ++ (Core.unName $ var "name"))
      (unaryFunction Flows.pure)
      (ref lookupPrimitiveDef @@ var "g" @@ var "name")

requireTermDef :: TElement (Name -> Flow Graph Term)
requireTermDef = lexicalDefinition "requireTerm" $
  lambda "name" $
    Flows.bind (ref resolveTermDef @@ var "name") $
    lambda "mt" $ Optionals.maybe
      (Flows.fail $ "no such element: " ++ (Core.unName $ var "name"))
      (unaryFunction Flows.pure)
      (var "mt")

resolveTermDef :: TElement (Name -> Flow Graph (Maybe Term))
resolveTermDef = lexicalDefinition "resolveTerm" $
  doc "TODO: distinguish between lambda-bound and let-bound variables" $
  lambda "name" $ lets [
    "recurse">: lambda "el" $ lets [
      "stripped">: ref Strip.stripTermDef @@ (Graph.elementTerm $ var "el")]
      $ cases _Term (var "stripped") (Just $ Flows.pure $ just $ Graph.elementTerm $ var "el") [
        _Term_variable>>: lambda "name'" $ ref resolveTermDef @@ var "name'"]]
    $ Flows.bind (ref Monads.getStateDef) $
      lambda "g" $ Optionals.maybe
        (Flows.pure nothing)
        (var "recurse")
        (Maps.lookup (var "name") (Graph.graphElements $ var "g"))

schemaContextDef :: TElement (Graph -> Graph)
schemaContextDef = lexicalDefinition "schemaContext" $
  doc "Note: assuming for now that primitive functions are the same in the schema graph" $
  lambda "g" $ Optionals.fromMaybe (var "g") (Graph.graphSchema $ var "g")

stripAndDereferenceTermDef :: TElement (Term -> Flow Graph Term)
stripAndDereferenceTermDef = lexicalDefinition "stripAndDereferenceTerm" $
  lambda "term" $ lets [
    "stripped">: ref Strip.stripTermDef @@ var "term"]
    $ cases _Term (var "stripped") (Just $ Flows.pure $ var "stripped") [
      _Term_variable>>: lambda "v" $
        Flows.bind (ref requireTermDef @@ var "v") $
        lambda "t" $ ref stripAndDereferenceTermDef @@ var "t"]

typeOfPrimitiveDef :: TElement (Name -> Flow Graph TypeScheme)
typeOfPrimitiveDef = lexicalDefinition "typeOfPrimitive" $
  lambda "name" $ Flows.map (unaryFunction Graph.primitiveType) $ ref requirePrimitiveDef @@ var "name"

withSchemaContextDef :: TElement (Flow Graph x -> Flow Graph x)
withSchemaContextDef = lexicalDefinition "withSchemaContext" $
  lambda "f" $
    Flows.bind (ref Monads.getStateDef) $
    lambda "g" $ ref Monads.withStateDef @@ (ref schemaContextDef @@ var "g") @@ var "f"
