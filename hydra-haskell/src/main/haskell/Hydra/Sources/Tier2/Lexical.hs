{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Lexical where

-- Standard Tier-2 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All


lexicalDefinition :: String -> TTerm a -> TElement a
lexicalDefinition = definitionInModule hydraLexicalModule

hydraLexicalModule :: Module
hydraLexicalModule = Module (Namespace "hydra.lexical") elements
   [hydraGraphModule, hydraMantleModule, hydraComputeModule, hydraStripModule] [hydraCoreModule] $
    Just ("A module for lexical operations over graphs.")
  where
    elements = [
      el elementsToGraphDef,
      el lookupPrimitiveDef]

elementsToGraphDef :: TElement (Graph -> Maybe Graph -> [Element] -> Graph)
elementsToGraphDef = lexicalDefinition "elementsToGraph" $
  lambda "parent" $ lambda "schema" $ lambda "elements" $ lets [
    "toPair" >: lambda "el" $ pair (Graph.elementName @@ var "el") (var "el")]
    $ Graph.graph
      (Maps.fromList (Lists.map (var "toPair") $ var "elements"))
      (Graph.graphEnvironment @@ var "parent")
      (Graph.graphTypes @@ var "parent")
      (Graph.graphBody @@ var "parent")
      (Graph.graphPrimitives @@ var "parent")
      (var "schema")

lookupPrimitiveDef :: TElement (Graph -> Name -> Maybe Primitive)
lookupPrimitiveDef = lexicalDefinition "lookupPrimitive" $
  lambda "g" $ lambda "name" $
    Maps.lookup (var "name") (Graph.graphPrimitives @@ var "g")
