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


type DataError = String
type SchemaError = String

bidirectional :: ((Step a a -> a -> Either String a) -> b -> Either String b) -> Step b b
bidirectional step = Step (step stepOut) (step stepIn)

expectRecordTerm :: Term -> Either String [Field]
expectRecordTerm term = case term of
  TermRecord fields -> pure fields
  _ -> Left $ "expected a record, got " ++ show term

expectStringTerm :: Term -> Either String String
expectStringTerm term = case term of
  TermAtomic (AtomicValueString s) -> pure s
  _ -> Left $ "expected a string, got " ++ show term

expectUnionTerm :: Term -> Either String Field
expectUnionTerm term = case term of
  TermUnion field -> pure field
  _ -> Left $ "expected a union, got " ++ show term

fieldsToMap :: [Field] -> M.Map FieldName Term
fieldsToMap fields = M.fromList $ (\(Field name term) -> (name, term)) <$> fields

fieldTypesToMap :: [FieldType] -> M.Map FieldName Type
fieldTypesToMap fields = M.fromList $ (\(FieldType name typ) -> (name, typ)) <$> fields

requireField :: M.Map FieldName Term -> FieldName -> Either String Term
requireField fields fname = Y.maybe error Right $ M.lookup fname fields
  where
    error = Left $ "no such field: " ++ fname


--  TODO:
--    term constructors
--      - application
--      - cases
--      - lambda
--      - variable
--    
--    type constructors:
--      - nominal

-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings using Haskell's derived show/read format.
termAdapter :: Context -> Type -> Either SchemaError (Step Term Term)
termAdapter context = innerAdapter
  where
    fieldAdapter ftyp = do
      adapter <- innerAdapter $ fieldTypeType ftyp
      return $ bidirectional $ \dir (Field name term) -> Field <$> pure name <*> dir adapter term

    defaultMutator = Step encode decode
      where
        encode = pure . stringValue . show
        decode (TermAtomic (AtomicValueString s)) = pure $ read s
                               
    innerAdapter :: Type -> Either SchemaError (Step Term Term)
    innerAdapter typ = case typ of
      TypeAtomic at -> pure $ Step (pure . id) (pure . id)
      TypeElement et -> pure $ Step encode decode
        where
          encode (TermElement name) = pure $ stringValue name
          decode (TermAtomic (AtomicValueString name)) = pure $ TermElement name
      TypeFunction (FunctionType dom cod) -> do
          adapter <- innerAdapter $ TypeUnion [
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
--              TermRecord [
--                Field _Lambda_parameter (stringValue v),
--                Field _Lambda_body b]
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
--                dfields <- expectRecordTerm fterm
--                let dmap = fieldsToMap dfields
--                v <- requireField dmap _Lambda_parameter >>= expectStringTerm
--                b <- requireField dmap _Lambda_body
--                return $ TermLambda $ Lambda v b
              forProjection fterm = TermProjection <$> expectStringTerm fterm
              forVariable fterm = TermVariable <$> expectStringTerm fterm -- TODO
      TypeList lt -> do
        adapter <- innerAdapter lt
        return $ bidirectional $ \dir (TermList terms) -> TermList <$> CM.mapM (dir adapter) terms
      TypeMap (MapType kt vt) -> do
          kadapter <- innerAdapter kt
          vadapter <- innerAdapter vt
          return $ bidirectional $ \dir (TermMap m)
              -> (TermMap . M.fromList) <$> (CM.mapM (\(k, v) -> (,) <$> dir kadapter k <*> dir vadapter v) $ M.toList m)
      TypeNominal name -> do
        -- TODO: precompute the schema graph; don't construct it anew for each adapter
        scontext <- schemaContext context
        -- Note: we just assume the schema term is a reference to hydra/core.Type
        adapter <- requireElement scontext name >>= decodeType . elementData >>= innerAdapter
        return $ bidirectional $ \dir -> dir adapter
--        prim <- requirePrimitiveFunction context name
--        adapter <- innerAdapter $ TypeFunction $ primitiveFunctionType prim
--        return $ bidirectional $ \dir -> dir adapter
      TypeRecord sfields -> do
          adapters <- CM.mapM fieldAdapter sfields
          return $ bidirectional $ \dir (TermRecord dfields) -> TermRecord <$> (CM.mapM id $ L.zipWith dir adapters dfields)
      TypeSet st -> do
          adapter <- innerAdapter $ TypeList st
          return $ Step (encode adapter) (decode adapter)
        where
          encode adapter (TermSet s) = stepOut adapter $ TermList $ S.toList s
          decode adapter term = (TermSet . S.fromList . (\(TermList l') -> l')) <$> stepIn adapter term
      TypeUnion sfields -> do
          adapters <- M.fromList <$> (CM.mapM (\f -> (,) <$> pure (fieldTypeName f) <*> fieldAdapter f) sfields)
          return $ bidirectional $ \dir (TermUnion dfield) -> do
            adapter <- getMutator adapters dfield
            TermUnion <$> dir adapter dfield
        where
          getMutator adapters f = Y.maybe (Left $ "no such field: " ++ fieldName f) pure $ M.lookup (fieldName f) adapters
      _ -> pure defaultMutator
