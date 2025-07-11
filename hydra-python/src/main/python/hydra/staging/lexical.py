"""Functions for retrieving elements and primitive functions from a graph context."""


# module Hydra.Staging.Lexical where

# import Hydra.Qnames
# import Hydra.Strip
# import Hydra.Core
# import Hydra.Graph
# import Hydra.Compute
# import Hydra.Flows
# import Hydra.Errors
# import Hydra.Module
# import Hydra.Lexical

# import qualified Data.List as L
# import qualified Data.Map as M
# import qualified Data.Maybe as Y
# import Control.Monad


# dereferenceElement :: Name -> Flow Graph (Maybe Element)
# dereferenceElement name = lookupElement <$> getState <*> pure name

# extendGraphWithBindings :: [LetBinding] -> Graph -> Graph
# extendGraphWithBindings bindings g = g {graphElements = M.union newEls $ graphElements g}
#   where
#     newEls = M.fromList $ fmap toEl bindings
#     toEl (LetBinding name term mts) = (name, Element name term mts)

# fieldsOf :: Type -> [FieldType]
# fieldsOf t = case stripType t of
#   TypeForall (ForallType _ body) -> fieldsOf body
#   TypeRecord rt -> rowTypeFields rt
#   TypeUnion rt -> rowTypeFields rt
#   _ -> []

# lookupElement :: Graph -> Name -> Maybe Element
# lookupElement g name = M.lookup name $ graphElements g

# requireElement :: Name -> Flow Graph Element
# requireElement name = do
#     mel <- dereferenceElement name
#     case mel of
#       Just el -> return el
#       Nothing -> getState >>= err
#   where
#     err g = fail $ "no such element: " ++ unName name
#         ++ ". Available elements: {" ++ L.intercalate ", " (ellipsis (unName . elementName <$> M.elems (graphElements g))) ++ "}"
#       where
#         showAll = False
#         ellipsis = id
# --        ellipsis strings = if L.length strings > 3 && not showAll
# --          then L.take 3 strings ++ ["..."]
# --          else strings

# requirePrimitive :: Name -> Flow Graph Primitive
# requirePrimitive name = do
#     g <- getState
#     Y.maybe err pure $ lookupPrimitive g name
#   where
#     err = fail $ "no such primitive function: " ++ unName name


# requireTerm :: Name -> Flow Graph Term
# requireTerm name = do
#   mt <- resolveTerm name
#   case mt of
#     Nothing -> fail $ "no such element: " ++ unName name
#     Just term -> pure term


# -- TODO: distinguish between lambda-bound and let-bound variables
# resolveTerm :: Name -> Flow Graph (Maybe Term)
# resolveTerm name = do
#     g <- getState
#     Y.maybe (pure Nothing) recurse $ M.lookup name $ graphElements g
#   where
#     recurse el = case fullyStripTerm (elementTerm el) of
#       TermVariable name' -> resolveTerm name'
#       _ -> pure $ Just $ elementTerm el


# -- Note: assuming for now that primitive functions are the same in the schema graph
# schemaContext :: Graph -> Graph
# schemaContext g = Y.fromMaybe g (graphSchema g)


# stripAndDereferenceTerm :: Term -> Flow Graph Term
# stripAndDereferenceTerm term = case fullyStripTerm term of
#   TermVariable v -> do
#     t <- requireTerm v
#     stripAndDereferenceTerm t
#   t -> pure t


# typeOfPrimitive :: Name -> Flow Graph TypeScheme
# typeOfPrimitive name = primitiveType <$> requirePrimitive name

# withSchemaContext :: Flow Graph x -> Flow Graph x
# withSchemaContext f = do
#   g <- getState
#   withState (schemaContext g) f
