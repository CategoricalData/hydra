-- | Functions for working with term and type annotations

module Hydra.Staging.Annotations where

import Hydra.Annotations
import Hydra.Strip
import Hydra.Core
import Hydra.Compute
import Hydra.Constants
import Hydra.Graph
import Hydra.Staging.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Mantle
import Hydra.Staging.Rewriting
import Hydra.Flows
import Hydra.Errors
import Hydra.Lexical
import qualified Hydra.Decode as Decode
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


-- | A flag which tells the language coders to encode a given encoded type as a term rather than a native type
key_firstClassType = Name "firstClassType"

aggregateAnnotations :: (x -> Maybe y) -> (y -> x) -> (y -> M.Map Name Term) -> x -> M.Map Name Term
aggregateAnnotations getValue getX getAnns t = M.fromList $ L.concat $ toPairs [] t
  where
    toPairs rest t = case getValue t of
      Nothing -> rest
      Just yy -> toPairs ((M.toList (getAnns yy)):rest) (getX yy)

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
getCount key = getAttrWithDefault key (Terms.int32 0) >>= Expect.int32

getDescription :: M.Map Name Term -> Flow Graph (Y.Maybe String)
getDescription anns = case getAnnotation key_description anns of
  Nothing -> pure Nothing
  Just term -> case term of
    TermLiteral (LiteralString s) -> pure $ Just s
    _ -> fail $ "unexpected value for " ++ show key_description ++ ": " ++ show term

getTermAnnotation :: Name -> Term -> Y.Maybe Term
getTermAnnotation key = getAnnotation key . termAnnotationInternal

getTermDescription :: Term -> Flow Graph (Y.Maybe String)
getTermDescription = getDescription . termAnnotationInternal

getType :: M.Map Name Term -> Flow Graph (Y.Maybe Type)
getType anns = case getAnnotation key_type anns of
  Nothing -> pure Nothing
  Just dat -> Just <$> coreDecodeType dat

getTypeAnnotation :: Name -> Type -> Y.Maybe Term
getTypeAnnotation key = getAnnotation key . typeAnnotationInternal

getTypeClasses :: Type -> Flow Graph (M.Map Name (S.Set TypeClass))
getTypeClasses typ = case getTypeAnnotation key_classes typ of
    Nothing -> pure M.empty
    Just term -> Expect.map coreDecodeName (Expect.set decodeClass) term
  where
    decodeClass term = Expect.unitVariant _TypeClass term >>= \fn -> Y.maybe
      (unexpected "type class" $ show term) pure $ M.lookup fn byName
    byName = M.fromList [(_TypeClass_equality, TypeClassEquality), (_TypeClass_ordering, TypeClassOrdering)]

getTypeDescription :: Type -> Flow Graph (Y.Maybe String)
getTypeDescription = getDescription . typeAnnotationInternal

-- | For a typed term, decide whether a coder should encode it as a native type expression,
--   or as a Hydra type expression.
isNativeType :: TypedTerm -> Bool
isNativeType (TypedTerm term typ) = isType typ && not isFlaggedAsFirstClassType
  where
    isFlaggedAsFirstClassType = Y.fromMaybe False (getTermAnnotation key_firstClassType term >>= Decode.boolean)

hasDescription :: M.Map Name Term -> Bool
hasDescription anns = case getAnnotation key_description anns of
  Nothing -> False
  Just _ -> True

hasFlag :: Name -> Flow s Bool
hasFlag flag = getAttrWithDefault flag (Terms.boolean False) >>= Expect.boolean

hasTypeDescription :: Type -> Bool
hasTypeDescription = hasDescription . typeAnnotationInternal

-- | Return a zero-indexed counter for the given key: 0, 1, 2, ...
nextCount :: Name -> Flow s Int
nextCount key = do
  count <- getCount key
  putCount key $ count + 1
  return count

putCount :: Name -> Int -> Flow s ()
putCount key count = putAttr key (Terms.int32 count)

resetCount :: Name -> Flow s ()
resetCount key = do
  putAttr key (Terms.int32 0)
  return ()

normalizeTermAnnotations :: Term -> Term
normalizeTermAnnotations term = if M.null anns
    then stripped
    else TermAnnotated $ AnnotatedTerm stripped anns
  where
    anns = termAnnotationInternal term
    stripped = stripTerm term

normalizeTypeAnnotations :: Type -> Type
normalizeTypeAnnotations typ = if M.null anns
    then stripped
    else TypeAnnotated $ AnnotatedType stripped anns
  where
    anns = typeAnnotationInternal typ
    stripped = stripType typ

putAttr :: Name -> Term -> Flow s ()
putAttr key val = Flow q
  where
    q s0 t0 = FlowState (Just ()) s0 (t0 {traceOther = M.insert key val $ traceOther t0})

setAnnotation :: Name -> Y.Maybe Term -> M.Map Name Term -> M.Map Name Term
setAnnotation key val m = M.alter (const val) key m

setDescription :: Y.Maybe String -> M.Map Name Term -> M.Map Name Term
setDescription d = setAnnotation key_description (Terms.string <$> d)

setTermAnnotation :: Name -> Y.Maybe Term -> Term -> Term
setTermAnnotation key val term = if anns == M.empty
    then term'
    else TermAnnotated $ AnnotatedTerm term' anns
  where
    term' = stripTerm term
    anns = setAnnotation key val $ termAnnotationInternal term

setTermDescription :: Y.Maybe String -> Term -> Term
setTermDescription d = setTermAnnotation key_description (Terms.string <$> d)

-- TODO: temporary. Move this function out of Annotations
setTermType :: Y.Maybe Type -> Term -> Term
setTermType mtyp term = case mtyp of
    Nothing -> withoutType term
    Just typ -> TermTyped $ TypedTerm (withoutType term) typ
  where
    withoutType term = case term of
      TermAnnotated (AnnotatedTerm term1 ann) -> TermAnnotated $ AnnotatedTerm (withoutType term1) ann
      TermTyped (TypedTerm term1 _) -> term1
      _ -> term

setType mt = setAnnotation key_type (coreEncodeType <$> mt)

setTypeAnnotation :: Name -> Y.Maybe Term -> Type -> Type
setTypeAnnotation key val typ = if anns == M.empty
    then typ'
    else TypeAnnotated $ AnnotatedType typ' anns
  where
    typ' = stripType typ
    anns = setAnnotation key val $ typeAnnotationInternal typ

setTypeClasses :: M.Map Name (S.Set TypeClass) -> Type -> Type
setTypeClasses m = setTypeAnnotation key_classes encoded
  where
    encoded = if M.null m
      then Nothing
      else Just $ Terms.map $ M.fromList (encodePair <$> M.toList m)
    encodePair (name, classes) = (coreEncodeName name, Terms.set $ S.fromList (encodeClass <$> (S.toList classes)))
    encodeClass tc = Terms.unitVariant _TypeClass $ case tc of
      TypeClassEquality -> _TypeClass_equality
      TypeClassOrdering -> _TypeClass_ordering

setTypeDescription :: Y.Maybe String -> Type -> Type
setTypeDescription d = setTypeAnnotation key_description (Terms.string <$> d)

termAnnotationInternal :: Term -> M.Map Name Term
termAnnotationInternal = aggregateAnnotations getAnn annotatedTermSubject annotatedTermAnnotation
  where
    getAnn t = case t of
      TermAnnotated a -> Just a
      TermTyped (TypedTerm t1 _) -> getAnn t1
      _ -> Nothing

typeAnnotationInternal :: Type -> M.Map Name Term
typeAnnotationInternal = aggregateAnnotations getAnn annotatedTypeSubject annotatedTypeAnnotation
  where
    getAnn t = case t of
      TypeAnnotated a -> Just a
      _ -> Nothing

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
