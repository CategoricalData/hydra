module Hydra.Ext.Yaml.Adapter (
  termAdapter,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Helpers
import Hydra.Prototyping.Steps
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreDecoding

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


bidirectional :: ((Step a a -> a -> Either String a) -> b -> Either String b) -> Step b b
bidirectional step = Step (step stepOut) (step stepIn)

atomicTypePassThrough :: Context -> Type -> Either String (Step Term Term)
atomicTypePassThrough _ _ = pure $ Step (pure . id) (pure . id)

elementTypeToStringType :: Context -> Type -> Either String (Step Term Term)
elementTypeToStringType context (TypeElement et) = pure $ Step encode decode
  where
    encode (TermElement name) = pure $ stringValue name
    decode (TermAtomic (AtomicValueString name)) = pure $ TermElement name
                                                             
fieldAdapter :: Context -> FieldType -> Either String (Step Field Field) 
fieldAdapter context ftyp = do
  adapter <- termAdapter context $ fieldTypeType ftyp
  return $ bidirectional $ \dir (Field name term) -> Field <$> pure name <*> dir adapter term

functionTypeToUnionType :: Context -> Type -> Either String (Step Term Term)
functionTypeToUnionType context (TypeFunction (FunctionType dom cod)) = do
    adapter <- termAdapter context $ TypeUnion [
      FieldType _Term_cases stringType, -- TODO (TypeRecord cases)
      FieldType _Term_compareTo dom,
      FieldType _Term_data unitType,
      FieldType _Term_function stringType,
      FieldType _Term_lambda stringType, -- TODO (TypeRecord [FieldType _Lambda_parameter stringType, FieldType _Lambda_body cod]),
      FieldType _Term_projection stringType,
      FieldType _Term_variable stringType] -- TODO
    return $ Step (encode adapter) (decode adapter)
  where
    encode adapter term = stepOut adapter $ case term of
      TermCases cases -> variant _Term_cases $ stringValue $ show term -- TODO TermRecord cases
      TermCompareTo other -> variant _Term_compareTo other
      TermData -> unitVariant _Term_data
      TermFunction name -> variant _Term_function $ stringValue name
      TermLambda (Lambda v b) -> variant _Term_lambda $ stringValue $ show term -- TODO
      TermProjection fname -> variant _Term_projection $ stringValue fname
      TermVariable var -> variant _Term_lambda $ stringValue var -- TODO
    decode adapter term = do
        (Field fname fterm) <- (stepIn adapter term >>= expectUnionTerm)
        Y.fromMaybe (notFound fname) $ M.lookup fname $ M.fromList [
          (_Term_cases, forCases fterm),
          (_Term_compareTo, forCompareTo fterm),
          (_Term_data, forData fterm),
          (_Term_function, forFunction fterm),
          (_Term_lambda, forLambda fterm),
          (_Term_projection, forProjection fterm),
          (_Term_variable, forVariable fterm)]
      where
        notFound fname = Left $ "unexpected field: " ++ fname
        forCases fterm = TermCases <$> (read <$> expectStringTerm fterm) -- TODO
        forCompareTo fterm = pure $ TermCompareTo fterm
        forData fterm = pure TermData
        forFunction fterm = TermFunction <$> expectStringTerm fterm
        forLambda fterm = TermLambda <$> (read <$> expectStringTerm fterm) -- TODO
        forProjection fterm = TermProjection <$> expectStringTerm fterm
        forVariable fterm = TermVariable <$> expectStringTerm fterm -- TODO

listTypePassThrough :: Context -> Type -> Either String (Step Term Term)
listTypePassThrough context (TypeList lt) = do
  adapter <- termAdapter context lt
  return $ bidirectional $ \dir (TermList terms) -> TermList <$> CM.mapM (dir adapter) terms
        
mapTypePassThrough :: Context -> Type -> Either String (Step Term Term)
mapTypePassThrough context (TypeMap (MapType kt vt)) = do
  kadapter <- termAdapter context kt
  vadapter <- termAdapter context vt
  return $ bidirectional $ \dir (TermMap m)
    -> (TermMap . M.fromList) <$> (CM.mapM (\(k, v) -> (,) <$> dir kadapter k <*> dir vadapter v) $ M.toList m)

nominalTypePassThrough :: Context -> Type -> Either String (Step Term Term)
nominalTypePassThrough context (TypeNominal name) = do
  -- TODO: precompute the schema graph; don't construct it anew for each adapter
  scontext <- schemaContext context
  -- Note: we just assume the schema term is a reference to hydra/core.Type
  adapter <- requireElement scontext name >>= decodeType . elementData >>= termAdapter context
  return $ bidirectional $ \dir -> dir adapter

recordTypePassThrough :: Context -> Type -> Either String (Step Term Term)
recordTypePassThrough context (TypeRecord sfields) = do
  adapters <- CM.mapM (fieldAdapter context) sfields
  return $ bidirectional $ \dir (TermRecord dfields) -> TermRecord <$> (CM.mapM id $ L.zipWith dir adapters dfields)

setTypeToListType :: Context -> Type -> Either String (Step Term Term)
setTypeToListType context (TypeSet st) = do
    adapter <- termAdapter context $ TypeList st
    return $ Step (encode adapter) (decode adapter)
  where
    encode adapter (TermSet s) = stepOut adapter $ TermList $ S.toList s
    decode adapter term = (TermSet . S.fromList . (\(TermList l') -> l')) <$> stepIn adapter term

--  TODO:
--    term constructors
--      - application
--      - cases
--      - lambda
--      - variable
-- 
-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings using Haskell's derived show/read format.
termAdapter :: Context -> Type -> Either SchemaError (Step Term Term)
termAdapter context typ = case typeVariant typ of
    TypeVariantAtomic -> atomicTypePassThrough context typ
    TypeVariantElement -> elementTypeToStringType context typ
    TypeVariantFunction -> functionTypeToUnionType context typ
    TypeVariantList -> listTypePassThrough context typ
    TypeVariantMap -> mapTypePassThrough context typ
    TypeVariantNominal -> nominalTypePassThrough context typ
    TypeVariantRecord -> recordTypePassThrough context typ
    TypeVariantSet -> setTypeToListType context typ
    TypeVariantUnion -> unionTypePassThrough context typ

unionTypePassThrough :: Context -> Type -> Either String (Step Term Term)
unionTypePassThrough context (TypeUnion sfields) = do
    adapters <- M.fromList <$> (CM.mapM (\f -> (,) <$> pure (fieldTypeName f) <*> fieldAdapter context f) sfields)
    return $ bidirectional $ \dir (TermUnion dfield) -> do
      adapter <- getMutator adapters dfield
      TermUnion <$> dir adapter dfield
  where
    getMutator adapters f = Y.maybe (Left $ "no such field: " ++ fieldName f) pure $ M.lookup (fieldName f) adapters
