module Hydra.Prototyping.Adapters.Term (
  fieldAdapter,
  termAdapter,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Adapter
import Hydra.Prototyping.Adapters.Atomic
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl
import Hydra.Prototyping.Steps
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreDecoding
import Hydra.Prototyping.Adapters.Utils

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


dereferenceNominal :: AdapterContext -> Type -> Qualified (Adapter Type Term)
dereferenceNominal context t@(TypeNominal name) = do
  typ <- eitherToQualified $ do
    -- TODO: precompute the schema graph; don't construct it anew for each adapter
    scontext <- schemaContext $ adapterContextEvaluation context
    -- Note: we just assume the schema term is a reference to hydra/core.Type
    requireElement scontext name >>= decodeType . elementData
  ad <- termAdapter context typ
  return ad { adapterSource = t }

elementToString :: AdapterContext -> Type -> Qualified (Adapter Type Term)
elementToString context t@(TypeElement _) = pure $ Adapter False t stringType $ Step encode decode
  where
    encode (TermElement name) = pure $ stringValue name
    decode (TermAtomic (AtomicValueString name)) = pure $ TermElement name

fieldAdapter :: AdapterContext -> FieldType -> Qualified (Adapter FieldType Field)
fieldAdapter context ftyp = do
  ad <- termAdapter context $ fieldTypeType ftyp
  return $ Adapter (adapterIsLossy ad) ftyp (ftyp { fieldTypeType = adapterTarget ad })
    $ bidirectional $ \dir (Field name term) -> Field name <$> stepBoth dir (adapterStep ad) term

functionToUnion :: AdapterContext -> Type -> Qualified (Adapter Type Term)
functionToUnion context t@(TypeFunction (FunctionType dom _)) = do
    ut <- unionType
    ad <- termAdapter context ut
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad)
      $ Step (encode ad) (decode ad)
  where
    encode ad term = stepOut (adapterStep ad) $ case term of
      TermFunction f -> case f of
        FunctionCases _ -> variant _Function_cases $ stringValue $ show term -- TODO TermRecord cases
        FunctionCompareTo other -> variant _Function_compareTo other
        FunctionData -> unitVariant _Function_data
        FunctionLambda _ -> variant _Function_lambda $ stringValue $ show term -- TODO
        FunctionPrimitive name -> variant _Function_primitive $ stringValue name
        FunctionProjection fname -> variant _Function_projection $ stringValue fname
      TermVariable var -> variant _Term_variable $ stringValue var
    decode ad term = do
        (Field fname fterm) <- stepIn (adapterStep ad) term >>= expectUnionTerm
        Y.fromMaybe (notFound fname) $ M.lookup fname $ M.fromList [
          (_Function_cases, forCases fterm),
          (_Function_compareTo, forCompareTo fterm),
          (_Function_data, forData fterm),
          (_Function_lambda, forLambda fterm),
          (_Function_primitive, forPrimitive fterm),
          (_Function_projection, forProjection fterm),
          (_Term_variable, forVariable fterm)]
      where
        notFound fname = fail $ "unexpected field: " ++ fname
        forCases fterm = read <$> expectStringTerm fterm -- TODO
        forCompareTo fterm = pure $ compareTo fterm
        forData _ = pure dataTerm
        forPrimitive fterm = primitive <$> expectStringTerm fterm
        forLambda fterm = read <$> expectStringTerm fterm -- TODO
        forProjection fterm = projection <$> expectStringTerm fterm
        forVariable fterm = variable <$> expectStringTerm fterm

    unionType = do
      domAd <- termAdapter context dom
      return $ TypeUnion [
        FieldType _Function_cases stringType, -- TODO (TypeRecord cases)
        FieldType _Function_compareTo (adapterTarget domAd),
        FieldType _Function_data unitType,
        FieldType _Function_lambda stringType, -- TODO (TypeRecord [FieldType _Lambda_parameter stringType, FieldType _Lambda_body cod]),
        FieldType _Function_primitive stringType,
        FieldType _Function_projection stringType,
        FieldType _Term_variable stringType]

listToSet :: AdapterContext -> Type -> Qualified (Adapter Type Term)
listToSet context t@(TypeSet st) = do
    ad <- termAdapter context $ TypeList st
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad)
      $ Step (encode ad) (decode ad)
  where
    encode ad (TermSet s) = stepOut (adapterStep ad) $ TermList $ S.toList s
    decode ad term = TermSet . S.fromList . (\(TermList l') -> l') <$> stepIn (adapterStep ad) term

optionalToUnion :: AdapterContext -> Type -> Qualified (Adapter Type Term)
optionalToUnion context t@(TypeOptional ot) = do
    ad <- termAdapter context ot
    let t' = TypeUnion [FieldType "nothing" unitType, FieldType "just" (adapterTarget ad)]
    return $ Adapter (adapterIsLossy ad) t t' $ Step {
      stepOut = \(TermOptional m) -> case m of
        Nothing -> pure $ TermUnion $ Field "nothing" unitTerm
        Just term -> do
          term' <- stepOut (adapterStep ad) term
          return $ TermUnion $ Field "just" term',
      stepIn = \(TermUnion (Field fn term)) -> if fn == "nothing"
        then pure $ TermOptional Nothing
        else do
          term' <- stepIn (adapterStep ad) term
          return $ TermOptional $ Just term'}

passAtomic :: AdapterContext -> Type -> Qualified (Adapter Type Term)
passAtomic context (TypeAtomic at) = do
  ad <- atomicAdapter context at
  let step = bidirectional $ \dir (TermAtomic av) -> TermAtomic <$> stepBoth dir (adapterStep ad) av
  return $ Adapter (adapterIsLossy ad) (TypeAtomic $ adapterSource ad) (TypeAtomic $ adapterTarget ad) step

passFunction :: AdapterContext -> Type -> Qualified (Adapter Type Term)
passFunction context t@(TypeFunction (FunctionType dom cod)) = do
    domAd <- termAdapter context dom
    codAd <- termAdapter context cod
    caseAds <- case dom of
      TypeUnion sfields -> M.fromList . L.zip (fieldTypeName <$> sfields)
        <$> CM.mapM (fieldAdapter context) sfields
      _ -> pure M.empty
    let lossy = adapterIsLossy codAd || or (adapterIsLossy . snd <$> M.toList caseAds)
    let dom' = adapterTarget domAd
    let cod' = adapterTarget codAd
    return $ Adapter lossy t (TypeFunction (FunctionType dom' cod'))
      $ bidirectional $ \dir (TermFunction f) -> TermFunction <$> case f of
        FunctionCases cases -> FunctionCases <$> CM.mapM (\f -> stepBoth dir (getStep $ fieldName f) f) cases
          where
            -- Note: this causes unrecognized cases to simply be passed through;
            --       it is not the job of this adapter to catch validation issues.
            getStep fname = Y.maybe idStep adapterStep $ M.lookup fname caseAds
        FunctionCompareTo other -> FunctionCompareTo <$> stepBoth dir (adapterStep codAd) other
        FunctionLambda (Lambda var body) -> FunctionLambda <$> (Lambda var <$> stepBoth dir (adapterStep codAd) body)
        _ -> pure f

passList :: AdapterContext -> Type -> Qualified (Adapter Type Term)
passList context t@(TypeList lt) = do
  ad <- termAdapter context lt
  return $ Adapter (adapterIsLossy ad) t (TypeList $ adapterTarget ad)
    $ bidirectional $ \dir (TermList terms) -> TermList <$> CM.mapM (stepBoth dir $ adapterStep ad) terms

passMap :: AdapterContext -> Type -> Qualified (Adapter Type Term)
passMap context t@(TypeMap (MapType kt vt)) = do
  kad <- termAdapter context kt
  vad <- termAdapter context vt
  return $ Adapter (adapterIsLossy kad || adapterIsLossy vad)
    t (TypeMap (MapType (adapterTarget kad) (adapterTarget vad)))
    $ bidirectional $ \dir (TermMap m) -> TermMap . M.fromList
      <$> CM.mapM (\(k, v) -> (,) <$> stepBoth dir (adapterStep kad) k <*> stepBoth dir (adapterStep vad) v)
        (M.toList m)

passOptional :: AdapterContext -> Type -> Qualified (Adapter Type Term)
passOptional context t@(TypeOptional ot) = do
  ad <- termAdapter context ot
  return $ Adapter (adapterIsLossy ad) t (TypeOptional $ adapterTarget ad) $
    bidirectional $ \dir (TermOptional m) -> TermOptional <$> case m of
      Nothing -> pure Nothing
      Just term -> Just <$> stepBoth dir (adapterStep ad) term

passRecord :: AdapterContext -> Type -> Qualified (Adapter Type Term)
passRecord context t@(TypeRecord sfields) = do
  adapters <- CM.mapM (fieldAdapter context) sfields
  let lossy = or $ adapterIsLossy <$> adapters
  let sfields' = adapterTarget <$> adapters
  return $ Adapter lossy t (TypeRecord sfields') $ bidirectional
    $ \dir (TermRecord dfields) -> TermRecord <$> CM.zipWithM (stepBoth dir . adapterStep) adapters dfields

passSet :: AdapterContext -> Type -> Qualified (Adapter Type Term)
passSet context t@(TypeSet st) = do
  ad <- termAdapter context st
  return $ Adapter (adapterIsLossy ad) t (TypeSet $ adapterTarget ad)
    $ bidirectional $ \dir (TermSet terms) -> TermSet . S.fromList
      <$> CM.mapM (stepBoth dir (adapterStep ad)) (S.toList terms)

passUnion :: AdapterContext -> Type -> Qualified (Adapter Type Term)
passUnion context t@(TypeUnion sfields) = do
    adapters <- M.fromList <$> CM.mapM (\f -> pure ((,) (fieldTypeName f)) <*> fieldAdapter context f) sfields
    let lossy = or $ adapterIsLossy <$> adapters
    let sfields' = adapterTarget . snd <$> M.toList adapters
    return $ Adapter lossy t (TypeUnion sfields')
      $ bidirectional $ \dir (TermUnion dfield) -> do
        ad <- getAdapter adapters dfield
        TermUnion <$> stepBoth dir (adapterStep ad) dfield
  where
    getAdapter adapters f = Y.maybe (fail $ "no such field: " ++ fieldName f) pure $ M.lookup (fieldName f) adapters

--  TODO:
--    term constructors
--      - application
--      - cases
--      - lambda
--      - variable
--
-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings using Haskell's derived show/read format.
termAdapter :: AdapterContext -> Type -> Qualified (Adapter Type Term)
termAdapter context = chooseAdapter alts supported describeType
  where
    alts t = (\c -> c context t) <$> if variantIsSupported t
      then case typeVariant t of
        TypeVariantAtomic -> pure passAtomic
        TypeVariantFunction ->  pure passFunction
        TypeVariantList -> pure passList
        TypeVariantMap -> pure passMap
        TypeVariantOptional -> pure passOptional
        TypeVariantRecord -> pure passRecord
        TypeVariantSet -> pure passSet
        TypeVariantUnion -> pure passUnion
        _ -> []
      else case typeVariant t of
        TypeVariantElement -> [elementToString]
        TypeVariantFunction -> [functionToUnion]
        TypeVariantNominal -> [dereferenceNominal]
        TypeVariantOptional -> [optionalToUnion]
        TypeVariantSet ->  [listToSet]
        TypeVariantUnion -> [unionToRecord]
        _ -> []

    constraints = languageConstraints $ adapterContextTarget context
    supported = typeIsSupported constraints
    variantIsSupported t = S.member (typeVariant t) $ languageConstraintsTypeVariants constraints

---- Caution: possibility of an infinite loop if neither unions nor optionals are supported
unionToRecord :: AdapterContext -> Type -> Qualified (Adapter Type Term)
unionToRecord context t@(TypeUnion sfields) = do
  let target = TypeRecord $ makeOptional <$> sfields
  ad <- termAdapter context target
  return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ Step {
    stepOut = \(TermUnion (Field fn term)) -> stepOut (adapterStep ad)
      $ TermRecord (toRecordField term fn <$> sfields),
    stepIn = \term -> do
      (TermRecord fields) <- stepIn (adapterStep ad) term
      return $ TermUnion $ fromRecordFields fields}
  where
    makeOptional (FieldType fn t) = FieldType fn $ TypeOptional t
    toRecordField term fn (FieldType fn' _) = Field fn' $ TermOptional $ if fn' == fn then Just term else Nothing
    fromRecordFields fields = L.head matches
      where
        matches :: [Field]
        matches = Y.mapMaybe (\(Field fn (TermOptional opt)) -> (Just . Field fn) =<< opt) fields
