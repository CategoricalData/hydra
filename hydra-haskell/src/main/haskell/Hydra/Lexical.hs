-- | Functions for retrieving elements and primitive functions from a graph context

module Hydra.Lexical (
  module Hydra.Lexical,
  module Hydra.Common,
  ) where

import Hydra.Common
import Hydra.Core
import Hydra.Graph
import Hydra.Compute
import Hydra.Monads

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Control.Monad


deref :: Term a -> GraphFlow a (Term a)
deref term = case stripTerm term of
  TermElement name -> dereferenceElement name >>= deref
  TermWrap (Nominal _ term') -> deref term'
  _ -> pure term

dereferenceElement :: Name -> GraphFlow a (Term a)
dereferenceElement en = do
    cx <- getState
    case M.lookup en (graphElements cx) of
      Nothing -> fail $ "element " ++ unName en ++ " does not exist"
      Just e -> pure $ elementData e

lookupPrimitive :: Graph a -> Name -> Maybe (Primitive a)
lookupPrimitive cx fn = M.lookup fn $ graphPrimitives cx

primitiveFunctionArity :: Primitive a -> Int
primitiveFunctionArity = typeArity . primitiveType

requireElement :: Name -> GraphFlow a (Element a)
requireElement name = do
    cx <- getState
    Y.maybe (err cx) pure $ M.lookup name $ graphElements cx
  where
    err cx = fail $ "no such element: " ++ unName name
        ++ ". Available elements: {" ++ L.intercalate ", " (ellipsis (unName . elementName <$> M.elems (graphElements cx))) ++ "}"
      where
        ellipsis strings = if L.length strings > 3
--        ellipsis strings = if L.length strings < 0
          then L.take 3 strings ++ ["..."]
          else strings

requirePrimitive :: Name -> GraphFlow a (Primitive a)
requirePrimitive fn = do
    cx <- getState
    Y.maybe err pure $ lookupPrimitive cx fn
  where
    err = fail $ "no such primitive function: " ++ unName fn

-- TODO: distinguish between lambda-bound and let-bound variables
resolveTerm :: Name -> GraphFlow a (Maybe (Term a))
resolveTerm name = do
    g <- getState
    Y.maybe (pure Nothing) recurse $ M.lookup name $ graphElements g
  where
    recurse el = case stripTerm (elementData el) of
      TermVariable name' -> resolveTerm name'
      _ -> pure $ Just $ elementData el
      
-- Note: assuming for now that primitive functions and evaluation strategy are the same in the schema graph
schemaContext :: Graph a -> Graph a
schemaContext g = Y.fromMaybe g (graphSchema g)

typeArity :: Type a -> Int
typeArity t = case stripType t of
  TypeApplication (ApplicationType l r) -> typeArity l
  TypeLambda (LambdaType _ body) -> typeArity body
  TypeFunction (FunctionType _ cod) -> 1 + typeArity cod
  _ -> 0

withSchemaContext :: GraphFlow a x -> GraphFlow a x
withSchemaContext f = do
  cx <- getState
  withState (schemaContext cx) f
