module Hydra.Prototyping.Adapters.Term (
  eitherToQualified,
  qualifiedToEither,
  
  termAdapter,
  
  module Hydra.Errors,
) where

import Hydra.Core
import Hydra.Errors
import Hydra.Graph
import Hydra.Translation
import Hydra.Prototyping.Basics
import Hydra.Ext.Haskell.Dsl
import Hydra.Prototyping.Steps
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreDecoding

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


-- TODO: move --------------------------
instance Functor Qualified where
  fmap f (Qualified x msgs) = Qualified (fmap f x) msgs
instance Applicative Qualified where
  pure x = Qualified (Just x) []
  Qualified f mf <*> Qualified x mx = Qualified (f <*> x) $ mf <> mx
instance Monad Qualified where
  Qualified x m >>= f = case x of
    Nothing -> Qualified Nothing m
    Just x' -> Qualified fx $ m2 <> m
      where Qualified fx m2 = f x'

eitherToQualified :: Either String a -> Qualified a
eitherToQualified e = case e of
  Left msg -> Qualified Nothing [msg]
  Right x -> Qualified (Just x) []

qualifiedToEither :: Qualified a -> Either String a
qualifiedToEither (Qualified x m) = case x of
  Nothing -> Left $ L.head m
  Just x' -> Right x'
----------------------------------------

bidirectional :: (StepDirection -> b -> Either String b) -> Step b b
bidirectional m = Step (m StepDirectionOut) (m StepDirectionIn)

atomicTypePassThrough :: TranslationContext -> Type -> Qualified (Step Term Term)
atomicTypePassThrough _ _ = pure idStep

elementTypePassThrough :: TranslationContext -> Type -> Qualified (Step Term Term)
elementTypePassThrough context (TypeElement et) = pure idStep

elementTypeToStringType :: TranslationContext -> Type -> Qualified (Step Term Term)
elementTypeToStringType context (TypeElement et) = pure $ Step encode decode
  where
    encode (TermElement name) = pure $ stringValue name
    decode (TermAtomic (AtomicValueString name)) = pure $ TermElement name

fieldAdapter :: TranslationContext -> FieldType -> Qualified (Step Field Field)
fieldAdapter context ftyp = do
  adapter <- termAdapter context $ fieldTypeType ftyp
  return $ bidirectional $ \dir (Field name term) -> Field name <$> stepEither dir adapter term

functionTypePassThrough :: TranslationContext -> Type -> Qualified (Step Term Term)
functionTypePassThrough context (TypeFunction (FunctionType dom cod)) = do
    codomainStep <- termAdapter context cod
    caseSteps <- case dom of
      TypeUnion sfields -> (M.fromList . L.zip (fieldTypeName <$> sfields)) <$> CM.mapM (fieldAdapter context) sfields
      _ -> pure M.empty
    return $ bidirectional $ \dir term -> case term of
      TermCases cases -> TermCases <$> (CM.mapM (\f -> stepEither dir (getStep $ fieldName f) f) cases)
        where
          -- Note: this causes unrecognized cases to simply be passed through;
          --       it is not the job of this adapter to catch validation issues.
          getStep fname = Y.fromMaybe idStep $ M.lookup fname caseSteps
      TermCompareTo other -> TermCompareTo <$> stepEither dir codomainStep other
      TermLambda (Lambda var body) -> TermLambda <$> (Lambda <$> pure var <*> stepEither dir codomainStep body)
      _ -> pure term

functionTypeToUnionType :: TranslationContext -> Type -> Qualified (Step Term Term)
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
      TermCases _ -> variant _Term_cases $ stringValue $ show term -- TODO TermRecord cases
      TermCompareTo other -> variant _Term_compareTo other
      TermData -> unitVariant _Term_data
      TermFunction name -> variant _Term_function $ stringValue name
      TermLambda _ -> variant _Term_lambda $ stringValue $ show term -- TODO
      TermProjection fname -> variant _Term_projection $ stringValue fname
      TermVariable var -> variant _Term_lambda $ stringValue var -- TODO
    decode adapter term = do
        (Field fname fterm) <- stepIn adapter term >>= expectUnionTerm
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
        forData _ = pure TermData
        forFunction fterm = TermFunction <$> expectStringTerm fterm
        forLambda fterm = TermLambda <$> (read <$> expectStringTerm fterm) -- TODO
        forProjection fterm = TermProjection <$> expectStringTerm fterm
        forVariable fterm = TermVariable <$> expectStringTerm fterm -- TODO

listTypePassThrough :: TranslationContext -> Type -> Qualified (Step Term Term)
listTypePassThrough context (TypeList lt) = do
  adapter <- termAdapter context lt
  return $ bidirectional $ \dir (TermList terms) -> TermList <$> CM.mapM (stepEither dir adapter) terms

mapTypePassThrough :: TranslationContext -> Type -> Qualified (Step Term Term)
mapTypePassThrough context (TypeMap (MapType kt vt)) = do
  kadapter <- termAdapter context kt
  vadapter <- termAdapter context vt
  return $ bidirectional $ \dir (TermMap m)
    -> TermMap . M.fromList
      <$> CM.mapM (\(k, v) -> (,) <$> stepEither dir kadapter k <*> stepEither dir vadapter v)
        (M.toList m)

nominalTypePassThrough :: TranslationContext -> Type -> Qualified (Step Term Term)
nominalTypePassThrough context (TypeNominal name) = do
    typ <- eitherToQualified $ do
      -- TODO: precompute the schema graph; don't construct it anew for each adapter
      scontext <- schemaContext $ translationContextEvaluation context
      -- Note: we just assume the schema term is a reference to hydra/core.Type
      requireElement scontext name >>= decodeType . elementData
    adapter <- termAdapter context typ
    return $ bidirectional $ \dir -> stepEither dir adapter

recordTypePassThrough :: TranslationContext -> Type -> Qualified (Step Term Term)
recordTypePassThrough context (TypeRecord sfields) = do
  adapters <- CM.mapM (fieldAdapter context) sfields
  return $ bidirectional $ \dir (TermRecord dfields) -> TermRecord
    <$> CM.zipWithM (stepEither dir) adapters dfields

setTypePassThrough :: TranslationContext -> Type -> Qualified (Step Term Term)
setTypePassThrough context (TypeSet st) = do
  adapter <- termAdapter context st
  return $ bidirectional $ \dir (TermSet terms) -> TermSet . S.fromList
    <$> CM.mapM (stepEither dir adapter) (S.toList terms)

setTypeToListType :: TranslationContext -> Type -> Qualified (Step Term Term)
setTypeToListType context (TypeSet st) = do
    adapter <- termAdapter context $ TypeList st
    return $ Step (encode adapter) (decode adapter)
  where
    encode adapter (TermSet s) = stepOut adapter $ TermList $ S.toList s
    decode adapter term = TermSet . S.fromList . (\(TermList l') -> l') <$> stepIn adapter term

--  TODO:
--    term constructors
--      - application
--      - cases
--      - lambda
--      - variable
--
-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings using Haskell's derived show/read format.
termAdapter :: TranslationContext -> Type -> Qualified (Step Term Term)
termAdapter context typ = if (not $ isRelevant var)
    then pure idStep
    else if isSupported var
    then Y.maybe (pure idStep) (\a -> a context typ) $ M.lookup var passThroughs
    else if L.null alts
    then Qualified Nothing ["no adapter could be constructed for " ++ show var]
    else snd (L.head alts) context typ
  where
    source = translationContextSource context
    target = translationContextTarget context
    var = typeVariant typ
    isSupported var = S.member var $ languageConstraintsTypeVariants $ languageConstraints target
    isRelevant var = S.member var $ languageConstraintsTypeVariants $ languageConstraints source
    alts = L.filter (\p -> isSupported $ fst p) $ Y.fromMaybe [] $ M.lookup var mutators
    passThroughs :: M.Map TypeVariant (TranslationContext -> Type -> Qualified (Step Term Term))
    passThroughs = M.fromList [
      (TypeVariantAtomic, atomicTypePassThrough),
      (TypeVariantElement, elementTypePassThrough),
      (TypeVariantFunction, functionTypePassThrough),
      (TypeVariantList, listTypePassThrough),
      (TypeVariantMap, mapTypePassThrough),
      (TypeVariantNominal, nominalTypePassThrough),
      (TypeVariantRecord, recordTypePassThrough),
      (TypeVariantSet, setTypePassThrough),
      (TypeVariantUnion, unionTypePassThrough)]
    mutators :: M.Map TypeVariant [(TypeVariant, TranslationContext -> Type -> Qualified (Step Term Term))]
    mutators = M.fromList [
      (TypeVariantElement, [(TypeVariantAtomic, elementTypeToStringType)]),
      (TypeVariantFunction, [(TypeVariantUnion, functionTypeToUnionType)]),
      (TypeVariantSet, [(TypeVariantList, setTypeToListType)])]

unionTypePassThrough :: TranslationContext -> Type -> Qualified (Step Term Term)
unionTypePassThrough context (TypeUnion sfields) = do
    adapters <- M.fromList <$> CM.mapM (\f -> pure ((,) (fieldTypeName f)) <*> fieldAdapter context f) sfields
    return $ bidirectional $ \dir (TermUnion dfield) -> do
      adapter <- getAdapter adapters dfield
      TermUnion <$> stepEither dir adapter dfield
  where
    getAdapter adapters f = Y.maybe (Left $ "no such field: " ++ fieldName f) pure $ M.lookup (fieldName f) adapters
