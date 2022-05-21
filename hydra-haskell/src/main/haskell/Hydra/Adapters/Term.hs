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
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


_context = FieldName "context"
_record = FieldName "record"

dereferenceNominal :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
dereferenceNominal acx t@(Type (TypeTermNominal name) _) = do
  typ <- eitherToQualified $ do
    -- TODO: precompute the schema graph; don't construct it anew for each adapter
    scx <- schemaContext $ adapterContextEvaluation acx
    -- Note: we just assume the schema term is a reference to hydra/core.Type
    requireElement scx name >>= decodeType (adapterContextEvaluation acx) . elementData
  ad <- termAdapter acx typ
  return ad { adapterSource = t }

elementToString :: Default m => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
elementToString acx t@(Type (TypeTermElement _) _) = pure $ Adapter False t Types.string $ Step encode decode
  where
    encode (Data (DataTermElement (Name name)) _) = pure $ stringValue name
    decode (Data (DataTermLiteral (LiteralString name)) meta) = pure $ withData meta $ DataTermElement $ Name name

fieldAdapter :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> FieldType m -> Qualified (Adapter (FieldType m) (Field m))
fieldAdapter acx ftyp = do
  ad <- termAdapter acx $ fieldTypeType ftyp
  return $ Adapter (adapterIsLossy ad) ftyp (ftyp { fieldTypeType = adapterTarget ad })
    $ bidirectional $ \dir (Field name term) -> Field name <$> stepEither dir (adapterStep ad) term

functionToUnion :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
functionToUnion acx t@(Type (TypeTermFunction (FunctionType dom _)) _) = do
    ut <- unionType
    ad <- termAdapter acx ut
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad)
      $ Step (encode ad) (decode ad)
  where
    cx = adapterContextEvaluation acx
    encode ad term = stepOut (adapterStep ad) $ case dataTerm term of
      DataTermFunction f -> case f of
        FunctionCases _ -> variant _Function_cases $ stringValue $ show term -- TODO DataTermRecord cases
        FunctionCompareTo other -> variant _Function_compareTo other
        FunctionDelta -> unitVariant _Function_delta
        FunctionLambda _ -> variant _Function_lambda $ stringValue $ show term -- TODO
        FunctionOptionalCases _ -> variant _Function_optionalCases $ stringValue $ show term -- TODO
        FunctionPrimitive (Name name) -> variant _Function_primitive $ stringValue name
        FunctionProjection (FieldName fname) -> variant _Function_projection $ stringValue fname
      DataTermVariable (Variable var) -> variant _DataTerm_variable $ stringValue var
    decode ad term = do
        (Field fname fterm) <- stepIn (adapterStep ad) term >>= expectUnion
        Y.fromMaybe (notFound fname) $ M.lookup fname $ M.fromList [
          (_Function_cases, forCases fterm),
          (_Function_compareTo, forCompareTo fterm),
          (_Function_delta, forData fterm),
          (_Function_lambda, forLambda fterm),
          (_Function_optionalCases, forOptionalCases fterm),
          (_Function_primitive, forPrimitive fterm),
          (_Function_projection, forProjection fterm),
          (_DataTerm_variable, forVariable fterm)]
      where
        notFound fname = fail $ "unexpected field: " ++ showFieldName fname
        forCases fterm = read <$> expectString fterm -- TODO
        forCompareTo fterm = pure $ compareTo fterm
        forData _ = pure delta
        forLambda fterm = read <$> expectString fterm -- TODO
        forOptionalCases fterm = read <$> expectString fterm -- TODO
        forPrimitive fterm = primitive . Name <$> expectString fterm
        forProjection fterm = projection . FieldName <$> expectString fterm
        forVariable fterm = variable <$> expectString fterm

    unionType = do
      domAd <- termAdapter acx dom
      return $ Types.union [
        FieldType _Function_cases Types.string, -- TODO (TypeTermRecord cases)
        FieldType _Function_compareTo (adapterTarget domAd),
        FieldType _Function_delta Types.unit,
        FieldType _Function_lambda Types.string, -- TODO (TypeTermRecord [FieldType _Lambda_parameter Types.string, FieldType _Lambda_body cod]),
        FieldType _Function_primitive Types.string,
        FieldType _Function_projection Types.string,
        FieldType _DataTerm_variable Types.string]

listToSet :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
listToSet acx t@(Type (TypeTermSet st) _ ) = do
    ad <- termAdapter acx $ Types.list st
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad)
      $ Step (encode ad) (decode ad)
  where
    encode ad (Data (DataTermSet s) meta) = stepOut (adapterStep ad) $ withData meta $ DataTermList $ S.toList s
    decode ad term = withData (dataMeta term) . DataTermSet . S.fromList . (\(Data (DataTermList l') _) -> l') <$> stepIn (adapterStep ad) term

optionalToList :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
optionalToList acx t@(Type (TypeTermOptional ot) _) = do
  ad <- termAdapter acx ot
  return $ Adapter False t (Types.list $ adapterTarget ad) $ Step {
    stepOut = \(Data (DataTermOptional m) _) -> Y.maybe
      (pure $ list [])
      (fmap (\ r -> list [r]) . stepOut (adapterStep ad)) m,
    stepIn = \(Data (DataTermList l) _) -> optional <$> if L.null l then
      pure Nothing
      else Just <$> stepIn (adapterStep ad) (L.head l)}

passAtomic :: Default m => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
passAtomic acx (Type (TypeTermLiteral at) _) = do
  ad <- atomicAdapter acx at
  let step = bidirectional $ \dir (Data (DataTermLiteral av) _) -> atomic <$> stepEither dir (adapterStep ad) av
  return $ Adapter (adapterIsLossy ad) (Types.literal $ adapterSource ad) (Types.literal $ adapterTarget ad) step

passFunction :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
passFunction acx t@(Type (TypeTermFunction (FunctionType dom cod)) _) = do
    domAd <- termAdapter acx dom
    codAd <- termAdapter acx cod
    caseAds <- case typeTerm dom of
      TypeTermUnion sfields -> M.fromList . L.zip (fieldTypeName <$> sfields)
        <$> CM.mapM (fieldAdapter acx) sfields
      _ -> pure M.empty
    optionAd <- case typeTerm dom of
      TypeTermOptional ot -> Just <$> termAdapter acx (Types.function ot cod)
      _ -> pure Nothing
    let lossy = adapterIsLossy codAd || or (adapterIsLossy . snd <$> M.toList caseAds)
    let dom' = adapterTarget domAd
    let cod' = adapterTarget codAd
    return $ Adapter lossy t (Types.function dom' cod')
      $ bidirectional $ \dir (Data (DataTermFunction f) meta) -> withData meta . DataTermFunction <$> case f of
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

passList :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
passList acx t@(Type (TypeTermList lt) _) = do
  ad <- termAdapter acx lt
  return $ Adapter (adapterIsLossy ad) t (Types.list $ adapterTarget ad)
    $ bidirectional $ \dir (Data (DataTermList terms) _) -> list <$> CM.mapM (stepEither dir $ adapterStep ad) terms

passMap :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
passMap acx t@(Type (TypeTermMap (MapType kt vt)) _) = do
  kad <- termAdapter acx kt
  vad <- termAdapter acx vt
  return $ Adapter (adapterIsLossy kad || adapterIsLossy vad)
    t (Types.map (adapterTarget kad) (adapterTarget vad))
    $ bidirectional $ \dir (Data (DataTermMap m) meta) -> withData meta . DataTermMap . M.fromList
      <$> CM.mapM (\(k, v) -> (,) <$> stepEither dir (adapterStep kad) k <*> stepEither dir (adapterStep vad) v)
        (M.toList m)

passOptional :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
passOptional acx t@(Type (TypeTermOptional ot) _) = do
  ad <- termAdapter acx ot
  return $ Adapter (adapterIsLossy ad) t (Types.optional $ adapterTarget ad) $
    bidirectional $ \dir term -> case term of
      (Data (DataTermOptional m) meta) -> withData meta . DataTermOptional <$> case m of
        Nothing -> pure Nothing
        Just term' -> Just <$> stepEither dir (adapterStep ad) term'
      _ -> fail $ "expected optional term, found: " ++ show term

passRecord :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
passRecord acx t@(Type (TypeTermRecord sfields) _) = do
  adapters <- CM.mapM (fieldAdapter acx) sfields
  let lossy = or $ adapterIsLossy <$> adapters
  let sfields' = adapterTarget <$> adapters
  return $ Adapter lossy t (Types.record sfields') $ bidirectional
    $ \dir (Data (DataTermRecord dfields) _) -> record <$> CM.zipWithM (stepEither dir . adapterStep) adapters dfields

passSet :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
passSet acx t@(Type (TypeTermSet st) _) = do
  ad <- termAdapter acx st
  return $ Adapter (adapterIsLossy ad) t (Types.set $ adapterTarget ad)
    $ bidirectional $ \dir (Data (DataTermSet terms) _) -> set . S.fromList
      <$> CM.mapM (stepEither dir (adapterStep ad)) (S.toList terms)

passUnion :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
passUnion acx t@(Type (TypeTermUnion sfields) _) = do
    adapters <- M.fromList <$> CM.mapM (\f -> pure ((,) (fieldTypeName f)) <*> fieldAdapter acx f) sfields
    let lossy = or $ adapterIsLossy <$> adapters
    let sfields' = adapterTarget . snd <$> M.toList adapters
    return $ Adapter lossy t (Types.union sfields')
      $ bidirectional $ \dir (Data (DataTermUnion dfield) meta) -> do
        ad <- getAdapter adapters dfield
        (\f -> Data (DataTermUnion f) meta) <$> stepEither dir (adapterStep ad) dfield
  where
    getAdapter adapters f = Y.maybe (fail $ "no such field: " ++ showFieldName (fieldName f)) pure $ M.lookup (fieldName f) adapters

passUniversal :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
passUniversal acx t@(Type (TypeTermUniversal (UniversalType (TypeVariable v) body)) _) = do
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
termAdapter :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
termAdapter acx typ = case typ of
--    TypeTermUniversal (UniversalType _ body) -> termAdapter acx body
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
unionToRecord :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
unionToRecord acx t@(Type (TypeTermUnion sfields) _) = do
    let target = Types.record [
                  FieldType _context Types.string,
                  FieldType _record $ Types.record $ makeOptional <$> sfields]
    ad <- termAdapter acx target
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ Step {
      stepOut = \(Data (DataTermUnion (Field fn term)) meta) -> stepOut (adapterStep ad)
        $ record [
          Field _context $ stringValue $ show meta, -- TODO: use encoded metadata once supported
          Field _record $ record (toRecordField term fn <$> sfields)],
      stepIn = \term -> do
        (Data (DataTermRecord [
           Field _context (Data (DataTermLiteral (LiteralString metaStr)) _),
           Field _record (Data (DataTermRecord fields) _)]) _) <- stepIn (adapterStep ad) term
        (\t -> t {dataMeta = read metaStr})
          <$> (union <$> fromRecordFields term (DataTermRecord fields) (adapterTarget ad) fields)}
  where
    makeOptional (FieldType fn t) = FieldType fn $ Types.optional t

    toRecordField term fn (FieldType fn' _) = Field fn' $ withData (dataMeta term)
      $ DataTermOptional $ if fn' == fn then Just term else Nothing
    fromRecordFields term term' t' fields = if L.null matches
        then fail $ "cannot convert term back to union: " ++ show term ++ " -- becomes " ++ show term'
          ++ " where type = " ++ show t ++ "    and target type = " ++ show t'
        else pure $ L.head matches
      where
        matches = Y.mapMaybe (\(Field fn (Data (DataTermOptional opt) _)) -> (Just . Field fn) =<< opt) fields

universalToMonotype :: (Default m, Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Data m))
universalToMonotype acx t@(Type (TypeTermUniversal (UniversalType _ body)) _) = do
  ad <- termAdapter acx body
  return ad {adapterSource = t}

withData :: m -> DataTerm m -> Data m
withData meta expr = Data expr meta
