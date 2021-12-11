module Hydra.Prototyping.Adapters.Term (
  fieldAdapter,
  termAdapter,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Adapter
import Hydra.Prototyping.Adapters.Atomic
import Hydra.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Prototyping.Steps
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreDecoding
import Hydra.Prototyping.Adapters.Utils
import Hydra.Prototyping.CoreEncoding

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


dereferenceNominal :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
dereferenceNominal context t@(TypeNominal name) = do
  typ <- eitherToQualified $ do
    -- TODO: precompute the schema graph; don't construct it anew for each adapter
    scontext <- schemaContext $ adapterContextEvaluation context
    -- Note: we just assume the schema term is a reference to hydra/core.Type
    requireElement scontext name >>= decodeType (adapterContextEvaluation context) . elementData
  ad <- termAdapter context typ
  return ad { adapterSource = t }

elementToString :: Default a => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
elementToString context t@(TypeElement _) = pure $ Adapter False t stringType $ Step encode decode
  where
    encode (Term (ExpressionElement name) _) = pure $ stringValue name
    decode (Term (ExpressionLiteral (LiteralString name)) meta) = pure $ withData meta $ ExpressionElement name

fieldAdapter :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> FieldType -> Qualified (Adapter FieldType (Field a))
fieldAdapter context ftyp = do
  ad <- termAdapter context $ fieldTypeType ftyp
  return $ Adapter (adapterIsLossy ad) ftyp (ftyp { fieldTypeType = adapterTarget ad })
    $ bidirectional $ \dir (Field name term) -> Field name <$> stepEither dir (adapterStep ad) term

functionToUnion :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
functionToUnion context t@(TypeFunction (FunctionType dom _)) = do
    ut <- unionType
    ad <- termAdapter context ut
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad)
      $ Step (encode ad) (decode ad)
  where
    cx = adapterContextEvaluation context
    encode ad term = stepOut (adapterStep ad) $ case termData term of
      ExpressionFunction f -> case f of
        FunctionCases _ -> nominalVariant cx _Function _Function_cases $ stringValue $ show term -- TODO ExpressionRecord cases
        FunctionCompareTo other -> nominalVariant cx _Function _Function_compareTo other
        FunctionData -> nominalUnitVariant cx _Function _Function_data
        FunctionLambda _ -> nominalVariant cx _Function _Function_lambda $ stringValue $ show term -- TODO
        FunctionOptionalCases _ -> nominalVariant cx _Function _Function_optionalCases $ stringValue $ show term -- TODO
        FunctionPrimitive name -> nominalVariant cx _Function _Function_primitive $ stringValue name
        FunctionProjection fname -> nominalVariant cx _Function _Function_projection $ stringValue fname
      ExpressionVariable var -> nominalVariant cx _Function _Expression_variable $ stringValue var
    decode ad term = do
        (Field fname fterm) <- stepIn (adapterStep ad) term >>= expectUnion
        Y.fromMaybe (notFound fname) $ M.lookup fname $ M.fromList [
          (_Function_cases, forCases fterm),
          (_Function_compareTo, forCompareTo fterm),
          (_Function_data, forData fterm),
          (_Function_lambda, forLambda fterm),
          (_Function_optionalCases, forOptionalCases fterm),
          (_Function_primitive, forPrimitive fterm),
          (_Function_projection, forProjection fterm),
          (_Expression_variable, forVariable fterm)]
      where
        notFound fname = fail $ "unexpected field: " ++ fname
        forCases fterm = read <$> expectString fterm -- TODO
        forCompareTo fterm = pure $ compareTo fterm
        forData _ = pure dataTerm
        forLambda fterm = read <$> expectString fterm -- TODO
        forOptionalCases fterm = read <$> expectString fterm -- TODO
        forPrimitive fterm = primitive <$> expectString fterm
        forProjection fterm = projection <$> expectString fterm
        forVariable fterm = variable <$> expectString fterm

    unionType = do
      domAd <- termAdapter context dom
      return $ TypeUnion [
        FieldType _Function_cases stringType, -- TODO (TypeRecord cases)
        FieldType _Function_compareTo (adapterTarget domAd),
        FieldType _Function_data unitType,
        FieldType _Function_lambda stringType, -- TODO (TypeRecord [FieldType _Lambda_parameter stringType, FieldType _Lambda_body cod]),
        FieldType _Function_primitive stringType,
        FieldType _Function_projection stringType,
        FieldType _Expression_variable stringType]

listToSet :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
listToSet context t@(TypeSet st) = do
    ad <- termAdapter context $ TypeList st
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad)
      $ Step (encode ad) (decode ad)
  where
    encode ad (Term (ExpressionSet s) meta) = stepOut (adapterStep ad) $ withData meta $ ExpressionList $ S.toList s
    decode ad term = withData (termMeta term) . ExpressionSet . S.fromList . (\(Term (ExpressionList l') _) -> l') <$> stepIn (adapterStep ad) term

optionalToList :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
optionalToList context t@(TypeOptional ot) = do
  ad <- termAdapter context ot
  return $ Adapter False t (TypeList $ adapterTarget ad) $ Step {
    stepOut = \(Term (ExpressionOptional m) _) -> Y.maybe
      (pure $ list [])
      (fmap (\ r -> list [r]) . stepOut (adapterStep ad)) m,
    stepIn = \(Term (ExpressionList l) _) -> optional <$> if L.null l then
      pure Nothing
      else Just <$> stepIn (adapterStep ad) (L.head l)}

--optionalToUnion :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
--optionalToUnion context t@(TypeOptional ot) = do
--    ad <- termAdapter context ot
--    return $ Adapter (adapterIsLossy ad) t (nominalType _OptionalExpression) $ Step {
--      stepOut = \(Term (ExpressionOptional m) _) -> case m of
--        Nothing -> pure $ union _OptionalExpression $ Field _OptionalExpression_nothing unitTerm
--        Just term -> do
--          term' <- stepOut (adapterStep ad) term
--          return $ union _OptionalExpression $ Field _OptionalExpression_just term',
--      stepIn = \(Term (ExpressionUnion (UnionExpression _ (Field fn term))) _) -> if fn == _OptionalExpression_nothing
--        then pure $ optional Nothing
--        else do
--          term' <- stepIn (adapterStep ad) term
--          return $ optional $ Just term'}

passAtomic :: Default a => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
passAtomic context (TypeLiteral at) = do
  ad <- atomicAdapter context at
  let step = bidirectional $ \dir (Term (ExpressionLiteral av) _) -> atomic <$> stepEither dir (adapterStep ad) av
  return $ Adapter (adapterIsLossy ad) (TypeLiteral $ adapterSource ad) (TypeLiteral $ adapterTarget ad) step

passFunction :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
passFunction context t@(TypeFunction (FunctionType dom cod)) = do
    domAd <- termAdapter context dom
    codAd <- termAdapter context cod
    caseAds <- case dom of
      TypeUnion sfields -> M.fromList . L.zip (fieldTypeName <$> sfields)
        <$> CM.mapM (fieldAdapter context) sfields
      _ -> pure M.empty
    optionAd <- case dom of
      TypeOptional ot -> Just <$> termAdapter context (functionType ot cod)
      _ -> pure Nothing 
    let lossy = adapterIsLossy codAd || or (adapterIsLossy . snd <$> M.toList caseAds)
    let dom' = adapterTarget domAd
    let cod' = adapterTarget codAd
    return $ Adapter lossy t (TypeFunction (FunctionType dom' cod'))
      $ bidirectional $ \dir (Term (ExpressionFunction f) meta) -> withData meta . ExpressionFunction <$> case f of
        FunctionCases cases -> FunctionCases <$> CM.mapM (\f -> stepEither dir (getStep $ fieldName f) f) cases
          where
            -- Note: this causes unrecognized cases to simply be passed through;
            --       it is not the job of this adapter to catch validation issues.
            getStep fname = Y.maybe idStep adapterStep $ M.lookup fname caseAds
        FunctionCompareTo other -> FunctionCompareTo <$> stepEither dir (adapterStep codAd) other
        FunctionLambda (Lambda var body) -> FunctionLambda <$> (Lambda var <$> stepEither dir (adapterStep codAd) body)
        FunctionOptionalCases (OptionalCases nothing just) -> FunctionOptionalCases <$> (
          OptionalCases
            <$> stepEither dir (adapterStep codAd) nothing
            <*> (stepEither dir (adapterStep $ Y.fromJust optionAd) just))

passList :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
passList context t@(TypeList lt) = do
  ad <- termAdapter context lt
  return $ Adapter (adapterIsLossy ad) t (TypeList $ adapterTarget ad)
    $ bidirectional $ \dir (Term (ExpressionList terms) _) -> list <$> CM.mapM (stepEither dir $ adapterStep ad) terms

passMap :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
passMap context t@(TypeMap (MapType kt vt)) = do
  kad <- termAdapter context kt
  vad <- termAdapter context vt
  return $ Adapter (adapterIsLossy kad || adapterIsLossy vad)
    t (TypeMap (MapType (adapterTarget kad) (adapterTarget vad)))
    $ bidirectional $ \dir (Term (ExpressionMap m) meta) -> withData meta . ExpressionMap . M.fromList
      <$> CM.mapM (\(k, v) -> (,) <$> stepEither dir (adapterStep kad) k <*> stepEither dir (adapterStep vad) v)
        (M.toList m)

passOptional :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
passOptional context t@(TypeOptional ot) = do
  ad <- termAdapter context ot
  return $ Adapter (adapterIsLossy ad) t (TypeOptional $ adapterTarget ad) $
    bidirectional $ \dir term -> case term of
      (Term (ExpressionOptional m) meta) -> withData meta . ExpressionOptional <$> case m of
        Nothing -> pure Nothing
        Just term' -> Just <$> stepEither dir (adapterStep ad) term'
      _ -> fail $ "expected optional term, found: " ++ show term

passRecord :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
passRecord context t@(TypeRecord sfields) = do
  adapters <- CM.mapM (fieldAdapter context) sfields
  let lossy = or $ adapterIsLossy <$> adapters
  let sfields' = adapterTarget <$> adapters
  return $ Adapter lossy t (TypeRecord sfields') $ bidirectional
    $ \dir (Term (ExpressionRecord dfields) _) -> record <$> CM.zipWithM (stepEither dir . adapterStep) adapters dfields

passSet :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
passSet context t@(TypeSet st) = do
  ad <- termAdapter context st
  return $ Adapter (adapterIsLossy ad) t (TypeSet $ adapterTarget ad)
    $ bidirectional $ \dir (Term (ExpressionSet terms) _) -> set . S.fromList
      <$> CM.mapM (stepEither dir (adapterStep ad)) (S.toList terms)

passUnion :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
passUnion context t@(TypeUnion sfields) = do
    adapters <- M.fromList <$> CM.mapM (\f -> pure ((,) (fieldTypeName f)) <*> fieldAdapter context f) sfields
    let lossy = or $ adapterIsLossy <$> adapters
    let sfields' = adapterTarget . snd <$> M.toList adapters
    return $ Adapter lossy t (TypeUnion sfields')
      $ bidirectional $ \dir (Term (ExpressionUnion dfield) meta) -> do
        ad <- getAdapter adapters dfield
        (\f -> Term (ExpressionUnion f) meta) <$> stepEither dir (adapterStep ad) dfield
  where
    getAdapter adapters f = Y.maybe (fail $ "no such field: " ++ fieldName f) pure $ M.lookup (fieldName f) adapters

--  TODO:
--    term constructors
--      - application
--      - cases
--      - lambda
--      - variable
--    type constructors
--      - abstract
--      - variable
--
-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings using Haskell's derived show/read format.
termAdapter :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
termAdapter context typ = case typ of
    TypeUniversal (UniversalType _ body) -> termAdapter context body
    _ -> chooseAdapter alts supported describeType typ
  where
    alts t = (\c -> c context t) <$> if variantIsSupported t
      then case typeVariant t of
        TypeVariantLiteral -> pure passAtomic
        TypeVariantFunction ->  pure passFunction
        TypeVariantList -> pure passList
        TypeVariantMap -> pure passMap
        TypeVariantOptional -> [passOptional, optionalToList]
        TypeVariantRecord -> pure passRecord
        TypeVariantSet -> pure passSet
        TypeVariantUnion -> pure passUnion
        _ -> []
      else case typeVariant t of
        TypeVariantElement -> [elementToString]
        TypeVariantFunction -> [functionToUnion]
        TypeVariantNominal -> [dereferenceNominal]
        TypeVariantOptional -> [optionalToList]
        TypeVariantSet ->  [listToSet]
        TypeVariantUnion -> [unionToRecord]
        _ -> []

    constraints = languageConstraints $ adapterContextTarget context
    supported = typeIsSupported constraints
    variantIsSupported t = S.member (typeVariant t) $ languageConstraintsTypeVariants constraints

---- Caution: possibility of an infinite loop if neither unions, optionals, nor lists are supported
unionToRecord :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
unionToRecord context t@(TypeUnion sfields) = do
    let target = TypeRecord [
                  FieldType "context" stringType,
                  FieldType "record" $ TypeRecord $ makeOptional <$> sfields]
    ad <- termAdapter context target
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ Step {
      stepOut = \(Term (ExpressionUnion (Field fn term)) meta) -> stepOut (adapterStep ad)
        $ record [
          Field "context" $ stringValue $ show meta, -- TODO: use encoded metadata once supported
          Field "record" $ record (toRecordField term fn <$> sfields)],
      stepIn = \term -> do
        (Term (ExpressionRecord [
           Field "context" (Term (ExpressionLiteral (LiteralString metaStr)) _),
           Field "record" (Term (ExpressionRecord fields) _)]) _) <- stepIn (adapterStep ad) term
        (\t -> t {termMeta = read metaStr})
          <$> (union <$> fromRecordFields term (ExpressionRecord fields) (adapterTarget ad) fields)}
  where
    cx = adapterContextEvaluation context
    makeOptional (FieldType fn t) = FieldType fn $ TypeOptional t

    toRecordField term fn (FieldType fn' _) = Field fn' $ withData (termMeta term)
      $ ExpressionOptional $ if fn' == fn then Just term else Nothing
    fromRecordFields term term' t' fields = if L.null matches
        then fail $ "cannot convert term back to union: " ++ show term ++ " -- becomes " ++ show term'
          ++ " where type = " ++ show t ++ "    and target type = " ++ show t'
        else pure $ L.head matches
      where
        matches = Y.mapMaybe (\(Field fn (Term (ExpressionOptional opt) _)) -> (Just . Field fn) =<< opt) fields

withData :: a -> Expression a -> Term a
withData meta expr = Term expr meta
