-- | Functions for working with term and type annotations

module Hydra.Staging.Annotations (
  module Hydra.Annotations,
  module Hydra.Staging.Annotations,
) where

import Hydra.Annotations
import Hydra.Strip
import Hydra.Core
import Hydra.Compute
import Hydra.Constants
import Hydra.Graph
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Mantle
import Hydra.Staging.Rewriting
import Hydra.Flows
import Hydra.Errors
import Hydra.Lexical
import qualified Hydra.Decode as Decode
import qualified Hydra.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


debugIf :: String -> String -> Flow s ()
debugIf debugId message = do
  desc <- getDebugId
  if desc == Just debugId
    then fail message
    else return ()

getDebugId :: Flow s (Maybe String)
getDebugId = withEmptyGraph $ do
  desc <- getAttr key_debugId
  traverse Expect.string desc

failOnFlag :: Name -> String -> Flow s ()
failOnFlag flag msg = do
  val <- hasFlag flag
  if val
    then fail msg
    else pure ()

getAttr :: Name -> Flow s (Maybe Term)
getAttr key = Flow q
  where
    q s0 t0 = FlowState (Just $ M.lookup key $ traceOther t0) s0 t0

getAttrWithDefault :: Name -> Term -> Flow s Term
getAttrWithDefault key def = Y.fromMaybe def <$> getAttr key

getCount :: Name -> Flow s Int
getCount key = withEmptyGraph $ getAttrWithDefault key (Terms.int32 0) >>= Expect.int32

getDescription :: M.Map Name Term -> Flow Graph (Y.Maybe String)
getDescription anns = case M.lookup key_description anns of
  Nothing -> pure Nothing
  Just term -> case term of
    TermLiteral (LiteralString s) -> pure $ Just s
    _ -> fail $ "unexpected value for " ++ show key_description ++ ": " ++ show term

getTermAnnotation :: Name -> Term -> Y.Maybe Term
getTermAnnotation key = M.lookup key . termAnnotationInternal

getTermDescription :: Term -> Flow Graph (Y.Maybe String)
getTermDescription = getDescription . termAnnotationInternal

getType :: M.Map Name Term -> Flow Graph (Y.Maybe Type)
getType anns = case M.lookup key_type anns of
  Nothing -> pure Nothing
  Just dat -> Just <$> coreDecodeType dat

getTypeAnnotation :: Name -> Type -> Y.Maybe Term
getTypeAnnotation key = M.lookup key . typeAnnotationInternal

getTypeClasses :: Term -> Flow Graph (M.Map Name (S.Set TypeClass))
getTypeClasses term = case getTermAnnotation key_classes term of
    Nothing -> pure M.empty
    Just term -> Expect.map_ coreDecodeName (Expect.set decodeClass) term
  where
    decodeClass term = Expect.unitVariant _TypeClass term >>= \fn -> Y.maybe
      (unexpected "type class" $ show term) pure $ M.lookup fn byName
    byName = M.fromList [(_TypeClass_equality, TypeClassEquality), (_TypeClass_ordering, TypeClassOrdering)]

getTypeDescription :: Type -> Flow Graph (Y.Maybe String)
getTypeDescription = getDescription . typeAnnotationInternal

-- | For a typed term, decide whether a coder should encode it as a native type expression,
--   or as a Hydra type expression.
isNativeType :: Element -> Bool
isNativeType el = case elementType el of
    Nothing -> False
    Just ts -> ts == TypeScheme [] (TypeVariable _Type) && not isFlaggedAsFirstClassType
  where
    isFlaggedAsFirstClassType = Y.fromMaybe False (getTermAnnotation key_firstClassType (elementTerm el) >>= Decode.boolean)

hasDescription :: M.Map Name Term -> Bool
hasDescription anns = case M.lookup key_description anns of
  Nothing -> False
  Just _ -> True

hasFlag :: Name -> Flow s Bool
hasFlag flag = withEmptyGraph $ getAttrWithDefault flag (Terms.boolean False) >>= Expect.boolean

hasTypeDescription :: Type -> Bool
hasTypeDescription = hasDescription . typeAnnotationInternal

-- | Return a zero-indexed counter for the given key: 0, 1, 2, ...
nextCount :: Name -> Flow s Int
nextCount key = do
  count <- getCount key
  putCount key $ count + 1
  return count

whenFlag :: Name -> Flow s a -> Flow s a -> Flow s a
whenFlag flag fthen felse = do
  b <- hasFlag flag
  if b
    then fthen
    else felse

-- TODO: move out of Annotations and into Rewriting
unshadowVariables :: Term -> Term
unshadowVariables term = Y.fromJust $ flowStateValue $ unFlow (rewriteTermM rewrite term) (S.empty, M.empty) emptyTrace
  where
    rewrite recurse term = do
      (reserved, subst) <- getState
      case term of
        TermVariable v -> pure $ TermVariable $ Y.fromMaybe v $ M.lookup v subst
        TermFunction (FunctionLambda (Lambda v d body)) -> if S.member v reserved
          then do
            v' <- freshName
            putState (S.insert v' reserved, M.insert v v' subst)
            body' <- recurse body
            putState (reserved, subst)
            pure $ TermFunction $ FunctionLambda $ Lambda v' d body'
          else do
            putState (S.insert v reserved, subst)
            body' <- recurse body
            return $ TermFunction $ FunctionLambda $ Lambda v d body'
        _ -> recurse term
    freshName = (\n -> Name $ "s" ++ show n) <$> nextCount (Name "unshadow")

-- | Provide an one-indexed, integer-valued 'depth' to a flow, where the depth is the number of nested calls.
--   This is useful for generating variable names while avoiding conflicts between the variables of parents and children.
--   E.g. a variable in an outer case/match statement might be "v1", whereas the variable of another case/match statement
--   inside of the first one becomes "v2". See also nextCount.
withDepth :: Name -> (Int -> Flow s a) -> Flow s a
withDepth key f = do
  count <- getCount key
  let inc = count + 1
  putCount key inc
  r <- f inc
  putCount key count
  return r
