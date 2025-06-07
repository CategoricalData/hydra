{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Lexical where

-- Standard Tier-2 imports
import qualified Hydra.Dsl.Coders          as Coders
import qualified Hydra.Dsl.Compute         as Compute
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Chars       as Chars
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import           Hydra.Dsl.Phantoms        as Phantoms
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

import Hydra.Sources.Libraries
import Hydra.Sources.Tier2.Errors
import Hydra.Sources.Tier2.Flows


lexicalDefinition :: String -> TTerm a -> TElement a
lexicalDefinition = definitionInModule hydraLexicalModule

hydraLexicalModule :: Module
hydraLexicalModule = Module (Namespace "hydra.lexical") elements
   [hydraErrorsModule, hydraFlowsModule, hydraComputeModule, hydraStripModule] [hydraGraphModule, hydraMantleModule] $
    Just ("A module for lexical operations over graphs.")
  where
    elements = [
      el dereferenceElementDef,
      el elementsToGraphDef,
      el emptyGraphDef,
      el extendGraphWithBindingsDef,
      el fieldsOfDef,
      el lookupElementDef,
      el lookupPrimitiveDef,
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
    (ref getStateDef)

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
    "stripped">: ref stripTypeDef @@ var "t"]
    $ cases _Type (var "stripped") (Just $ list []) [
      _Type_forall>>: lambda "forallType" $ ref fieldsOfDef @@ (Core.forallTypeBody $ var "forallType"),
      _Type_record>>: lambda "rt" $ Core.rowTypeFields $ var "rt",
      _Type_union>>: lambda "rt" $ Core.rowTypeFields $ var "rt"]

lookupElementDef :: TElement (Graph -> Name -> Maybe Element)
lookupElementDef = lexicalDefinition "lookupElement" $
  lambdas ["g", "name"] $ Maps.lookup (var "name") (Graph.graphElements $ var "g")

lookupPrimitiveDef :: TElement (Graph -> Name -> Maybe Primitive)
lookupPrimitiveDef = lexicalDefinition "lookupPrimitive" $
  lambda "g" $ lambda "name" $
    Maps.lookup (var "name") (Graph.graphPrimitives $ var "g")

requireElementDef :: TElement (Name -> Flow Graph Element)
requireElementDef = lexicalDefinition "requireElement" $
  lambda "name" $ lets [
    "showAll">: false,
    "ellipsis">: lambda "strings" $
      Logic.ifElse (Logic.and (Equality.gtInt32 (Lists.length $ var "strings") (int32 3)) (Logic.not $ var "showAll"))
        (Lists.concat2 (Lists.take (int32 3) (var "strings")) (list [string "..."]))
        (var "strings"),
    "err">: lambda "g" $ Flows.fail $
      "no such element: " ++ (Core.unName $ var "name") ++
      ". Available elements: {" ++
      (Strings.intercalate ", " $ var "ellipsis" @@ (Lists.map (lambda "el" $ Core.unName $ Graph.elementName $ var "el") $ Maps.elems $ Graph.graphElements $ var "g")) ++
      "}"]
    $ Flows.bind (ref dereferenceElementDef @@ var "name") $
      lambda "mel" $ Optionals.maybe
        (Flows.bind (ref getStateDef) $ var "err")
        (asFunction Flows.pure)
        (var "mel")

requirePrimitiveDef :: TElement (Name -> Flow Graph Primitive)
requirePrimitiveDef = lexicalDefinition "requirePrimitive" $
  lambda "name" $
    Flows.bind (ref getStateDef) $
    lambda "g" $ Optionals.maybe
      (Flows.fail $ "no such primitive function: " ++ (Core.unName $ var "name"))
      (asFunction Flows.pure)
      (ref lookupPrimitiveDef @@ var "g" @@ var "name")

requireTermDef :: TElement (Name -> Flow Graph Term)
requireTermDef = lexicalDefinition "requireTerm" $
  lambda "name" $
    Flows.bind (ref resolveTermDef @@ var "name") $
    lambda "mt" $ Optionals.maybe
      (Flows.fail $ "no such element: " ++ (Core.unName $ var "name"))
      (asFunction Flows.pure)
      (var "mt")

resolveTermDef :: TElement (Name -> Flow Graph (Maybe Term))
resolveTermDef = lexicalDefinition "resolveTerm" $
  doc "TODO: distinguish between lambda-bound and let-bound variables" $
  lambda "name" $ lets [
    "recurse">: lambda "el" $ lets [
      "stripped">: ref fullyStripTermDef @@ (Graph.elementTerm $ var "el")]
      $ cases _Term (var "stripped") (Just $ Flows.pure $ just $ Graph.elementTerm $ var "el") [
        _Term_variable>>: lambda "name'" $ ref resolveTermDef @@ var "name'"]]
    $ Flows.bind (ref getStateDef) $
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
    "stripped">: ref fullyStripTermDef @@ var "term"]
    $ cases _Term (var "stripped") (Just $ Flows.pure $ var "stripped") [
      _Term_variable>>: lambda "v" $
        Flows.bind (ref requireTermDef @@ var "v") $
        lambda "t" $ ref stripAndDereferenceTermDef @@ var "t"]

typeOfPrimitiveDef :: TElement (Name -> Flow Graph TypeScheme)
typeOfPrimitiveDef = lexicalDefinition "typeOfPrimitive" $
  lambda "name" $ Flows.map (asFunction Graph.primitiveType) $ ref requirePrimitiveDef @@ var "name"

withSchemaContextDef :: TElement (Flow Graph x -> Flow Graph x)
withSchemaContextDef = lexicalDefinition "withSchemaContext" $
  lambda "f" $
    Flows.bind (ref getStateDef) $
    lambda "g" $ ref withStateDef @@ (ref schemaContextDef @@ var "g") @@ var "f"
