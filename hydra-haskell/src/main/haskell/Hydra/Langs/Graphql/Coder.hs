module Hydra.Langs.Graphql.Coder where -- (printGraph) where

import Hydra.Kernel
import Hydra.Langs.Graphql.Language
import Hydra.Langs.Graphql.Serde
import qualified Hydra.Langs.Graphql.Syntax as G
--import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Tools.Script

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


printModule :: (Ord a, Read a, Show a) => Module a -> GraphFlow a (M.Map FilePath String)
printModule mod = do
    files <- moduleToGraphqlSchemas mod
    return $ M.fromList (mapPair <$> M.toList files)
  where
    mapPair (path, sf) = (path, printExpr $ parenthesize $ exprDocument sf)

moduleToGraphqlSchemas :: (Ord a, Read a, Show a) => Module a -> GraphFlow a (M.Map FilePath G.Document)
moduleToGraphqlSchemas mod = transformModule graphqlLanguage encodeTerm constructModule mod

constructModule :: (Ord a, Read a, Show a)
  => Module a
  -> M.Map (Type a) (Coder (Graph a) (Graph a) (Term a) ())
  -> [(Element a, TypedTerm a)]
  -> GraphFlow a (M.Map FilePath G.Document)
constructModule mod coders pairs = do
    tdefs <- CM.mapM toTypeDef pairs
    let doc = G.Document $ (G.DefinitionTypeSystem . G.TypeSystemDefinitionOrExtensionDefinition . G.TypeSystemDefinitionType) <$> tdefs
    return $ M.fromList [(filePath, doc)]
  where
    filePath = namespaceToFilePath False (FileExtension "sdl") (moduleNamespace mod)
    toTypeDef (el, TypedTerm typ term) = do
      if isType typ
        then epsilonDecodeType term >>= typeToGraphqlTypeDefinition el
        else fail $ "mapping of non-type elements to GraphQL is not yet supported: " ++ unName (elementName el)

encodeTerm :: (Eq a, Ord a, Read a, Show a) => Term a -> GraphFlow a ()
encodeTerm term = fail "not yet implemented"

typeToGraphqlTypeDefinition :: Element a -> Type a -> GraphFlow a G.TypeDefinition
typeToGraphqlTypeDefinition el typ = do
  fail "not yet implemented"
