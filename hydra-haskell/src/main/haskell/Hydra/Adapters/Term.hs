module Hydra.Adapters.Term (
  fieldAdapter,
  termAdapter,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Adapter
import Hydra.Adapters.Atomic
import Hydra.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Steps
import Hydra.Primitives
import Hydra.CoreDecoding
import Hydra.Adapters.Utils
import Hydra.Adapters.UtilsEtc
import Hydra.CoreEncoding
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


dereferenceNominal :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
dereferenceNominal acx t@(Type (TypeExprNominal name) _) = do
  typ <- eitherToQualified $ do
    -- TODO: precompute the schema graph; don't construct it anew for each adapter
    scx <- schemaContext $ adapterContextEvaluation acx
    -- Note: we just assume the schema term is a reference to hydra/core.Type
    requireElement scx name >>= decodeType (adapterContextEvaluation acx) . elementData
  ad <- termAdapter acx typ
  return ad { adapterSource = t }

elementToString :: Default m => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
elementToString acx t@(Type (TypeExprElement _) _) = pure $ Adapter False t Types.string $ Step encode decode
  where
    encode (Term (ExpressionElement name) _) = pure $ stringValue name
    decode (Term (ExpressionLiteral (LiteralString name)) meta) = pure $ withData meta $ ExpressionElement name

fieldAdapter :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> FieldType m -> Qualified (Adapter (FieldType m) (Field m))
fieldAdapter acx ftyp = do
  ad <- termAdapter acx $ fieldTypeType ftyp
  return $ Adapter (adapterIsLossy ad) ftyp (ftyp { fieldTypeType = adapterTarget ad })
    $ bidirectional $ \dir (Field name term) -> Field name <$> stepEither dir (adapterStep ad) term

functionToUnion :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
functionToUnion acx t@(Type (TypeExprFunction (FunctionType dom _)) _) = do
    ut <- unionType
    ad <- termAdapter acx ut
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad)
      $ Step (encode ad) (decode ad)
  where
    cx = adapterContextEvaluation acx
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
      domAd <- termAdapter acx dom
      return $ Types.union [
        FieldType _Function_cases Types.string, -- TODO (TypeExprRecord cases)
        FieldType _Function_compareTo (adapterTarget domAd),
        FieldType _Function_data Types.unit,
        FieldType _Function_lambda Types.string, -- TODO (TypeExprRecord [FieldType _Lambda_parameter Types.string, FieldType _Lambda_body cod]),
        FieldType _Function_primitive Types.string,
        FieldType _Function_projection Types.string,
        FieldType _Expression_variable Types.string]

listToSet :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
listToSet acx t@(Type (TypeExprSet st) _ ) = do
    ad <- termAdapter acx $ Types.list st
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad)
      $ Step (encode ad) (decode ad)
  where
    encode ad (Term (ExpressionSet s) meta) = stepOut (adapterStep ad) $ withData meta $ ExpressionList $ S.toList s
    decode ad term = withData (termMeta term) . ExpressionSet . S.fromList . (\(Term (ExpressionList l') _) -> l') <$> stepIn (adapterStep ad) term

optionalToList :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
optionalToList acx t@(Type (TypeExprOptional ot) _) = do
  ad <- termAdapter acx ot
  return $ Adapter False t (Types.list $ adapterTarget ad) $ Step {
    stepOut = \(Term (ExpressionOptional m) _) -> Y.maybe
      (pure $ list [])
      (fmap (\ r -> list [r]) . stepOut (adapterStep ad)) m,
    stepIn = \(Term (ExpressionList l) _) -> optional <$> if L.null l then
      pure Nothing
      else Just <$> stepIn (adapterStep ad) (L.head l)}

--optionalToUnion :: (Default a, Ord a, Read a, Show a) => AdapterContext a -> Type -> Qualified (Adapter Type (Term a))
--optionalToUnion acx t@(TypeExprOptional ot) = do
--    ad <- termAdapter acx ot
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

passAtomic :: Default m => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passAtomic acx (Type (TypeExprLiteral at) _) = do
  ad <- atomicAdapter acx at
  let step = bidirectional $ \dir (Term (ExpressionLiteral av) _) -> atomic <$> stepEither dir (adapterStep ad) av
  return $ Adapter (adapterIsLossy ad) (Types.literal $ adapterSource ad) (Types.literal $ adapterTarget ad) step

passFunction :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passFunction acx t@(Type (TypeExprFunction (FunctionType dom cod)) _) = do
    domAd <- termAdapter acx dom
    codAd <- termAdapter acx cod
    caseAds <- case typeData dom of
      TypeExprUnion sfields -> M.fromList . L.zip (fieldTypeName <$> sfields)
        <$> CM.mapM (fieldAdapter acx) sfields
      _ -> pure M.empty
    optionAd <- case typeData dom of
      TypeExprOptional ot -> Just <$> termAdapter acx (Types.function ot cod)
      _ -> pure Nothing
    let lossy = adapterIsLossy codAd || or (adapterIsLossy . snd <$> M.toList caseAds)
    let dom' = adapterTarget domAd
    let cod' = adapterTarget codAd
    return $ Adapter lossy t (Types.function dom' cod')
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

passList :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passList acx t@(Type (TypeExprList lt) _) = do
  ad <- termAdapter acx lt
  return $ Adapter (adapterIsLossy ad) t (Types.list $ adapterTarget ad)
    $ bidirectional $ \dir (Term (ExpressionList terms) _) -> list <$> CM.mapM (stepEither dir $ adapterStep ad) terms

passMap :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passMap acx t@(Type (TypeExprMap (MapType kt vt)) _) = do
  kad <- termAdapter acx kt
  vad <- termAdapter acx vt
  return $ Adapter (adapterIsLossy kad || adapterIsLossy vad)
    t (Types.map (adapterTarget kad) (adapterTarget vad))
    $ bidirectional $ \dir (Term (ExpressionMap m) meta) -> withData meta . ExpressionMap . M.fromList
      <$> CM.mapM (\(k, v) -> (,) <$> stepEither dir (adapterStep kad) k <*> stepEither dir (adapterStep vad) v)
        (M.toList m)

passOptional :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passOptional acx t@(Type (TypeExprOptional ot) _) = do
  ad <- termAdapter acx ot
  return $ Adapter (adapterIsLossy ad) t (Types.optional $ adapterTarget ad) $
    bidirectional $ \dir term -> case term of
      (Term (ExpressionOptional m) meta) -> withData meta . ExpressionOptional <$> case m of
        Nothing -> pure Nothing
        Just term' -> Just <$> stepEither dir (adapterStep ad) term'
      _ -> fail $ "expected optional term, found: " ++ show term

passRecord :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passRecord acx t@(Type (TypeExprRecord sfields) _) = do
  adapters <- CM.mapM (fieldAdapter acx) sfields
  let lossy = or $ adapterIsLossy <$> adapters
  let sfields' = adapterTarget <$> adapters
  return $ Adapter lossy t (Types.record sfields') $ bidirectional
    $ \dir (Term (ExpressionRecord dfields) _) -> record <$> CM.zipWithM (stepEither dir . adapterStep) adapters dfields

passSet :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passSet acx t@(Type (TypeExprSet st) _) = do
  ad <- termAdapter acx st
  return $ Adapter (adapterIsLossy ad) t (Types.set $ adapterTarget ad)
    $ bidirectional $ \dir (Term (ExpressionSet terms) _) -> set . S.fromList
      <$> CM.mapM (stepEither dir (adapterStep ad)) (S.toList terms)

passUnion :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passUnion acx t@(Type (TypeExprUnion sfields) _) = do
    adapters <- M.fromList <$> CM.mapM (\f -> pure ((,) (fieldTypeName f)) <*> fieldAdapter acx f) sfields
    let lossy = or $ adapterIsLossy <$> adapters
    let sfields' = adapterTarget . snd <$> M.toList adapters
    return $ Adapter lossy t (Types.union sfields')
      $ bidirectional $ \dir (Term (ExpressionUnion dfield) meta) -> do
        ad <- getAdapter adapters dfield
        (\f -> Term (ExpressionUnion f) meta) <$> stepEither dir (adapterStep ad) dfield
  where
    getAdapter adapters f = Y.maybe (fail $ "no such field: " ++ fieldName f) pure $ M.lookup (fieldName f) adapters

passUniversal :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passUniversal acx t@(Type (TypeExprUniversal (UniversalType v body)) _) = do
  ad <- termAdapter acx body
  return $ Adapter (adapterIsLossy ad) t (Types.universal v $ adapterTarget ad)
    $ bidirectional $ \dir term -> stepEither dir (adapterStep ad) term

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
termAdapter :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
termAdapter acx typ = case typ of
--    TypeExprUniversal (UniversalType _ body) -> termAdapter acx body
    _ -> chooseAdapter alts supported describeType typ
  where
    alts t = (\c -> c acx t) <$> if variantIsSupported t
      then case typeVariant t of
        TypeVariantLiteral -> pure passAtomic
        TypeVariantFunction ->  pure passFunction
        TypeVariantList -> pure passList
        TypeVariantMap -> pure passMap
        TypeVariantOptional -> [passOptional, optionalToList]
        TypeVariantRecord -> pure passRecord
        TypeVariantSet -> pure passSet
        TypeVariantUnion -> pure passUnion
        TypeVariantUniversal -> pure passUniversal
        _ -> []
      else case typeVariant t of
        TypeVariantElement -> [elementToString]
        TypeVariantFunction -> [functionToUnion]
        TypeVariantNominal -> [dereferenceNominal]
        TypeVariantOptional -> [optionalToList]
        TypeVariantSet ->  [listToSet]
        TypeVariantUnion -> [unionToRecord]
        TypeVariantUniversal -> [universalToMonotype]
        _ -> []

    constraints = languageConstraints $ adapterContextTarget acx
    supported = typeIsSupported constraints
    variantIsSupported t = S.member (typeVariant t) $ languageConstraintsTypeVariants constraints

---- Caution: possibility of an infinite loop if neither unions, optionals, nor lists are supported
unionToRecord :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
unionToRecord acx t@(Type (TypeExprUnion sfields) _) = do
    let target = Types.record [
                  FieldType "context" Types.string,
                  FieldType "record" $ Types.record $ makeOptional <$> sfields]
    ad <- termAdapter acx target
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
    makeOptional (FieldType fn t) = FieldType fn $ Types.optional t

    toRecordField term fn (FieldType fn' _) = Field fn' $ withData (termMeta term)
      $ ExpressionOptional $ if fn' == fn then Just term else Nothing
    fromRecordFields term term' t' fields = if L.null matches
        then fail $ "cannot convert term back to union: " ++ show term ++ " -- becomes " ++ show term'
          ++ " where type = " ++ show t ++ "    and target type = " ++ show t'
        else pure $ L.head matches
      where
        matches = Y.mapMaybe (\(Field fn (Term (ExpressionOptional opt) _)) -> (Just . Field fn) =<< opt) fields

universalToMonotype :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
universalToMonotype acx t@(Type (TypeExprUniversal (UniversalType _ body)) _) = do
  ad <- termAdapter acx body
  return ad {adapterSource = t}

withData :: m -> Expression m -> Term m
withData meta expr = Term expr meta
