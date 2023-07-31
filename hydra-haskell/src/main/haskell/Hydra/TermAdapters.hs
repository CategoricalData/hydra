-- | Adapter framework for types and terms

module Hydra.TermAdapters (
  fieldAdapter,
  functionProxyName,
  functionProxyType,
  termAdapter,
) where

import Hydra.Printing
import Hydra.AdapterUtils
import Hydra.Basics
import Hydra.Tier1
import Hydra.Coders
import Hydra.Common
import Hydra.Compute
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.Graph
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Flows
import Hydra.Reduction
import Hydra.Rewriting
import Hydra.LiteralAdapters
import Hydra.Dsl.Terms
import Hydra.Reduction
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.Read as TR
import qualified Data.Maybe as Y


_context :: FieldName
_context = FieldName "context"

_record :: FieldName
_record = FieldName "record"

fieldAdapter :: (Ord a, Read a, Show a) => FieldType a -> Flow (AdapterContext a) (SymmetricAdapter (AdapterContext a) (FieldType a) (Field a))
fieldAdapter ftyp = do
  ad <- termAdapter $ fieldTypeType ftyp
  return $ Adapter (adapterIsLossy ad) ftyp (ftyp { fieldTypeType = adapterTarget ad })
    $ bidirectional $ \dir (Field name term) -> Field name <$> encodeDecode dir (adapterCoder ad) term

-- | This function accounts for recursive type definitions
forTypeReference :: (Ord a, Read a, Show a) => Name -> Flow (AdapterContext a) (SymmetricAdapter (AdapterContext a) (Type a) (Term a))
forTypeReference name = withTrace ("adapt named type " ++ unName name) $ do
  let lossy = False -- Note: we cannot know in advance whether the adapter is lossy or not
  let placeholder = Adapter lossy (TypeVariable name) (TypeVariable name) $ bidirectional $
        \dir term -> do
          cx <- getState
          case M.lookup name (adapterContextAdapters cx) of
            Nothing -> fail $ "no adapter for reference type " ++ unName name
            Just ad -> encodeDecode dir (adapterCoder ad) term
  cx <- getState
  let adapters = adapterContextAdapters cx
  case M.lookup name adapters of
    Nothing -> do
      -- Insert a placeholder until the actual adapter has been constructed, in case of
      putState (cx {adapterContextAdapters = M.insert name placeholder adapters})
      mt <- withGraphContext $ resolveType $ TypeVariable name
      case mt of
        Nothing -> pure $ Adapter lossy (TypeVariable name) (TypeVariable name) $ bidirectional $ const pure
          -- fail $ "type variable did not resolve to a type: " ++ unName name
        Just t -> do
          actual <- termAdapter t
          putState (cx {adapterContextAdapters = M.insert name actual adapters})
          return actual
    Just ad -> pure ad

functionProxyName :: Name
functionProxyName = Name "hydra/core.FunctionProxy"

functionProxyType :: Type a -> Type a
functionProxyType dom = TypeUnion $ RowType functionProxyName Nothing [
  FieldType _Elimination_wrap Types.string,
  FieldType _Elimination_optional Types.string,
  FieldType _Elimination_record Types.string,
  FieldType _Elimination_union Types.string, -- TODO (TypeRecord cases)
  FieldType _Function_lambda Types.string, -- TODO (TypeRecord [FieldType _Lambda_parameter Types.string, FieldType _Lambda_body cod]),
  FieldType _Function_primitive Types.string,
  FieldType _Term_variable Types.string]

functionToUnion :: (Ord a, Read a, Show a) => TypeAdapter a
functionToUnion t@(TypeFunction (FunctionType dom _)) = do
    ut <- unionType
    ad <- termAdapter ut
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ Coder (encode ad) (decode ad)
  where
    encode ad term = coderEncode (adapterCoder ad) $ case stripTerm term of
      TermFunction f -> case f of
        FunctionElimination e -> case e of
          EliminationWrap (Name name) -> variant functionProxyName _Elimination_wrap $ string name
          EliminationOptional _ -> variant functionProxyName _Elimination_optional $ string $ show term -- TODO
          EliminationRecord _ -> variant functionProxyName _Elimination_record $ string $ show term -- TODO
          EliminationUnion _ -> variant functionProxyName _Elimination_union $ string $ show term -- TODO
        FunctionLambda _ -> variant functionProxyName _Function_lambda $ string $ show term -- TODO
        FunctionPrimitive (Name name) -> variant functionProxyName _Function_primitive $ string name
      TermVariable (Name var) -> variant functionProxyName _Term_variable $ string var

    decode ad term = do
        (Field fname fterm) <- coderDecode (adapterCoder ad) term >>= Expect.injection
        Y.fromMaybe (notFound fname) $ M.lookup fname $ M.fromList [
          (_Elimination_wrap, forWrapped fterm),
          (_Elimination_optional, forOptionalCases fterm),
          (_Elimination_record, forProjection fterm),
          (_Elimination_union, forCases fterm),
          (_Function_lambda, forLambda fterm),
          (_Function_primitive, forPrimitive fterm),
          (_Term_variable, forVariable fterm)]
      where
        notFound fname = fail $ "unexpected field: " ++ unFieldName fname
        forCases fterm = read <$> Expect.string fterm -- TODO
        forLambda fterm = read <$> Expect.string fterm -- TODO
        forWrapped fterm = unwrap . Name <$> Expect.string fterm
        forOptionalCases fterm = read <$> Expect.string fterm -- TODO
        forPrimitive fterm = primitive . Name <$> Expect.string fterm
        forProjection fterm = read <$> Expect.string fterm -- TODO
        forVariable fterm = var <$> Expect.string fterm

    unionType = do
      domAd <- termAdapter dom
      return $ TypeUnion $ RowType functionProxyName Nothing [
        FieldType _Elimination_wrap Types.string,
        FieldType _Elimination_optional Types.string,
        FieldType _Elimination_record Types.string,
        FieldType _Elimination_union Types.string, -- TODO (TypeRecord cases)
        FieldType _Function_lambda Types.string, -- TODO (TypeRecord [FieldType _Lambda_parameter Types.string, FieldType _Lambda_body cod]),
        FieldType _Function_primitive Types.string,
        FieldType _Term_variable Types.string]

lambdaToMonotype :: (Ord a, Read a, Show a) => TypeAdapter a
lambdaToMonotype t@(TypeLambda (LambdaType _ body)) = do
  ad <- termAdapter body
  return ad {adapterSource = t}

listToSet :: (Ord a, Read a, Show a) => TypeAdapter a
listToSet t@(TypeSet st) = do
    ad <- termAdapter $ Types.list st
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ Coder (encode ad) (decode ad)
  where
    encode ad (TermSet s) = coderEncode (adapterCoder ad) $ TermList $ S.toList s
    decode ad term = TermSet . S.fromList . (\(TermList l') -> l') <$> coderDecode (adapterCoder ad) term

optionalToList :: (Ord a, Read a, Show a) => TypeAdapter a
optionalToList t@(TypeOptional ot) = do
  ad <- termAdapter ot
  return $ Adapter False t (Types.list $ adapterTarget ad) $ Coder {
    coderEncode = \(TermOptional m) -> Y.maybe
      (pure $ list [])
      (fmap (\ r -> list [r]) . coderEncode (adapterCoder ad)) m,
    coderDecode = \(TermList l) -> optional <$> if L.null l then
      pure Nothing
      else Just <$> coderDecode (adapterCoder ad) (L.head l)}

passAnnotated :: (Ord a, Read a, Show a) => TypeAdapter a
passAnnotated t@(TypeAnnotated (Annotated at ann)) = do
  ad <- termAdapter at
  return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ bidirectional $
    \dir term -> encodeDecode dir (adapterCoder ad) term

-- TODO: only tested for type mappings; not yet for types+terms
passApplication :: (Ord a, Read a, Show a) => TypeAdapter a
passApplication t = do
    reduced <- withGraphContext $ betaReduceType t
    ad <- termAdapter reduced
    return $ Adapter (adapterIsLossy ad) t reduced $ bidirectional $
      \dir term -> encodeDecode dir (adapterCoder ad) term

passFunction :: (Ord a, Read a, Show a) => TypeAdapter a
passFunction t@(TypeFunction (FunctionType dom cod)) = do
    domAd <- termAdapter dom
    codAd <- termAdapter cod
    caseAds <- case stripType dom of
      TypeUnion rt -> M.fromList . L.zip (fieldTypeName <$> rowTypeFields rt)
        <$> CM.mapM (\f -> fieldAdapter $ FieldType (fieldTypeName f) (TypeFunction $ FunctionType (fieldTypeType f) cod)) (rowTypeFields rt)
      _ -> pure M.empty
    optionAd <- case stripType dom of
      TypeOptional ot -> Just <$> termAdapter (Types.function ot cod)
      _ -> pure Nothing
    let lossy = adapterIsLossy codAd || or (adapterIsLossy . snd <$> M.toList caseAds)
    let target = Types.function (adapterTarget domAd) (adapterTarget codAd)
    return $ Adapter lossy t target
      $ bidirectional $ \dir term -> case stripTerm term of
        TermFunction f -> TermFunction <$> case f of
          FunctionElimination e -> FunctionElimination <$> case e of
            EliminationOptional (OptionalCases nothing just) -> EliminationOptional <$> (
              OptionalCases
                <$> encodeDecode dir (adapterCoder codAd) nothing
                <*> (encodeDecode dir (adapterCoder $ Y.fromJust optionAd) just))
            EliminationUnion (CaseStatement n def cases) -> do
                rcases <- CM.mapM (\f -> encodeDecode dir (getCoder $ fieldName f) f) cases
                rdef <- case def of
                  Nothing -> pure Nothing
                  Just d -> Just <$> encodeDecode dir (adapterCoder codAd) d
                return $ EliminationUnion $ CaseStatement n rdef rcases

              where
                -- Note: this causes unrecognized cases to simply be passed through;
                --       it is not the job of this adapter to catch validation issues.
                getCoder fname = Y.maybe idCoder adapterCoder $ M.lookup fname caseAds
          FunctionLambda (Lambda var body) -> FunctionLambda <$> (Lambda var <$> encodeDecode dir (adapterCoder codAd) body)
        _ -> unexpected "function term" term

passLambda :: (Ord a, Read a, Show a) => TypeAdapter a
passLambda t@(TypeLambda (LambdaType (Name v) body)) = do
  ad <- termAdapter body
  return $ Adapter (adapterIsLossy ad) t (Types.lambda v $ adapterTarget ad)
    $ bidirectional $ \dir term -> encodeDecode dir (adapterCoder ad) term

passLiteral :: (Ord a, Show a) => TypeAdapter a
passLiteral (TypeLiteral at) = do
  ad <- literalAdapter at
  let step = bidirectional $ \dir term -> do
        l <- Expect.literal term
        literal <$> encodeDecode dir (adapterCoder ad) l
  return $ Adapter (adapterIsLossy ad) (Types.literal $ adapterSource ad) (Types.literal $ adapterTarget ad) step

passList :: (Ord a, Read a, Show a) => TypeAdapter a
passList t@(TypeList lt) = do
  ad <- termAdapter lt
  return $ Adapter (adapterIsLossy ad) t (Types.list $ adapterTarget ad)
    $ bidirectional $ \dir (TermList terms) -> list <$> CM.mapM (encodeDecode dir $ adapterCoder ad) terms

passMap :: (Ord a, Read a, Show a) => TypeAdapter a
passMap t@(TypeMap (MapType kt vt)) = do
  kad <- termAdapter kt
  vad <- termAdapter vt
  return $ Adapter (adapterIsLossy kad || adapterIsLossy vad)
    t (Types.map (adapterTarget kad) (adapterTarget vad))
    $ bidirectional $ \dir (TermMap m) -> TermMap . M.fromList
      <$> CM.mapM (\(k, v) -> (,) <$> encodeDecode dir (adapterCoder kad) k <*> encodeDecode dir (adapterCoder vad) v)
        (M.toList m)

passOptional :: (Ord a, Read a, Show a) => TypeAdapter a
passOptional t@(TypeOptional ot) = do
  ad <- termAdapter ot
  return $ Adapter (adapterIsLossy ad) t (Types.optional $ adapterTarget ad) $
    bidirectional $ \dir term -> case term of
      (TermOptional m) -> TermOptional <$> case m of
        Nothing -> pure Nothing
        Just term' -> Just <$> encodeDecode dir (adapterCoder ad) term'
      _ -> fail $ "expected optional term, found: " ++ show term

passProduct :: (Ord a, Read a, Show a) => TypeAdapter a
passProduct t@(TypeProduct types) = do
  ads <- CM.mapM termAdapter types
  let lossy = L.foldl (\b ad -> b || adapterIsLossy ad) False ads
  return $ Adapter lossy t (Types.product (adapterTarget <$> ads))
    $ bidirectional $ \dir (TermProduct tuple) -> TermProduct <$> (CM.zipWithM (\term ad -> encodeDecode dir (adapterCoder ad) term) tuple ads)

passRecord :: (Ord a, Read a, Show a) => TypeAdapter a
passRecord t@(TypeRecord rt) = do
  adapters <- CM.mapM fieldAdapter (rowTypeFields rt)
  let lossy = or $ adapterIsLossy <$> adapters
  let sfields' = adapterTarget <$> adapters
  return $ Adapter lossy t (TypeRecord $ rt {rowTypeFields = sfields'}) $ bidirectional
    $ \dir (TermRecord (Record _ dfields)) -> record (rowTypeTypeName rt) <$> CM.zipWithM (encodeDecode dir . adapterCoder) adapters dfields

passSet :: (Ord a, Read a, Show a) => TypeAdapter a
passSet t@(TypeSet st) = do
  ad <- termAdapter st
  return $ Adapter (adapterIsLossy ad) t (Types.set $ adapterTarget ad)
    $ bidirectional $ \dir (TermSet terms) -> set . S.fromList
      <$> CM.mapM (encodeDecode dir (adapterCoder ad)) (S.toList terms)

passSum :: (Ord a, Read a, Show a) => TypeAdapter a
passSum t@(TypeSum types) = do
  ads <- CM.mapM termAdapter types
  let lossy = L.foldl (\b ad -> b || adapterIsLossy ad) False ads
  return $ Adapter lossy t (Types.sum (adapterTarget <$> ads))
    $ bidirectional $ \dir (TermSum (Sum i n term)) -> TermSum . Sum i n <$> encodeDecode dir (adapterCoder $ ads !! i) term

passUnion :: (Ord a, Read a, Show a) => TypeAdapter a
passUnion t@(TypeUnion rt) = do
    adapters <- M.fromList <$> CM.mapM (\f -> pure ((,) (fieldTypeName f)) <*> fieldAdapter f) sfields
    let lossy = or $ adapterIsLossy <$> adapters
    let sfields' = adapterTarget . snd <$> M.toList adapters
    return $ Adapter lossy t (TypeUnion $ rt {rowTypeFields = sfields'})
      $ bidirectional $ \dir term -> do
        dfield <- Expect.injection term
        ad <- getAdapter adapters dfield
        TermUnion . Injection nm <$> encodeDecode dir (adapterCoder ad) dfield
  where
    getAdapter adapters f = Y.maybe (fail $ "no such field: " ++ unFieldName (fieldName f)) pure $ M.lookup (fieldName f) adapters
    sfields = rowTypeFields rt
    nm = rowTypeTypeName rt

simplifyApplication :: (Ord a, Read a, Show a) => TypeAdapter a
simplifyApplication t@(TypeApplication (ApplicationType lhs _)) = do
  ad <- termAdapter lhs
  return $ Adapter False t (adapterTarget ad) $ bidirectional $ \dir term -> encodeDecode dir (adapterCoder ad) term

-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings using Haskell's derived show/read format.
termAdapter :: (Ord a, Read a, Show a) => TypeAdapter a
termAdapter typ = withTrace ("adapter for " ++ describeType typ ) $ do
  case typ of
    -- Account for let-bound variables
    TypeVariable name -> forTypeReference name
    _ -> do
        cx <- getState
        chooseAdapter (alts cx) (supported cx) describeType typ
      where
        alts cx t = (\c -> c t) <$>
            if supportedAtTopLevel cx t
              then pass t
              else trySubstitution t
          where
            supportedAtTopLevel cx t = variantIsSupported cx t && languageConstraintsTypes (constraints cx) t
            pass t = case typeVariant t of
              TypeVariantAnnotated -> [passAnnotated]
              TypeVariantApplication -> [passApplication]
              TypeVariantFunction ->  [passFunction]
              TypeVariantLambda -> [passLambda]
              TypeVariantList -> [passList]
              TypeVariantLiteral -> [passLiteral]
              TypeVariantMap -> [passMap]
              TypeVariantOptional -> [passOptional, optionalToList]
              TypeVariantProduct -> [passProduct]
              TypeVariantRecord -> [passRecord]
              TypeVariantSet -> [passSet]
              TypeVariantSum -> [passSum]
              TypeVariantUnion -> [passUnion]
              _ -> []
            trySubstitution t = case typeVariant t of
              TypeVariantAnnotated -> [passAnnotated]
              TypeVariantApplication -> [simplifyApplication]
              TypeVariantFunction -> [functionToUnion]
              TypeVariantLambda -> [lambdaToMonotype]
              TypeVariantOptional -> [optionalToList]
              TypeVariantSet ->  [listToSet]
              TypeVariantUnion -> [unionToRecord]
              TypeVariantWrap -> [wrapToUnwrapped]
              _ -> [unsupportedToString]
  where
    constraints = languageConstraints . adapterContextLanguage
    supported = typeIsSupported . constraints
    variantIsSupported cx t = S.member (typeVariant t) $ languageConstraintsTypeVariants (constraints cx)

---- Caution: possibility of an infinite loop if neither unions, optionals, nor lists are supported
unionToRecord :: (Ord a, Read a, Show a) => TypeAdapter a
unionToRecord t@(TypeUnion rt) = do
    let target = TypeRecord $ rt {rowTypeFields = makeOptional <$> sfields}
    ad <- termAdapter target
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ Coder {
      coderEncode = \term' -> do
        (Field fn term) <- Expect.injectionWithName (rowTypeTypeName rt) term'
        coderEncode (adapterCoder ad) $ record nm (toRecordField term fn <$> sfields),
      coderDecode = \term -> do
        TermRecord (Record _ fields) <- coderDecode (adapterCoder ad) term
        inject nm <$> fromRecordFields term (TermRecord (Record nm fields)) (adapterTarget ad) fields}
  where
    nm = rowTypeTypeName rt
    sfields = rowTypeFields rt

    makeOptional (FieldType fn ft) = FieldType fn $ Types.optional ft

    toRecordField term fn (FieldType fn' _) = Field fn' $
      TermOptional $ if fn' == fn then Just term else Nothing

    fromRecordFields term term' t' fields = if L.null matches
        then fail $ "cannot convert term back to union: " ++ show term ++ " -- becomes " ++ show term'
          ++ " where type = " ++ show t ++ "    and target type = " ++ show t'
        else pure $ L.head matches
      where
        matches = Y.mapMaybe (\(Field fn (TermOptional opt)) -> (Just . Field fn) =<< opt) fields

unsupportedToString :: (Ord a, Read a, Show a) => TypeAdapter a
unsupportedToString t = pure $ Adapter False t Types.string $ Coder encode decode
  where
    -- TODO: use JSON for encoding and decoding unsupported terms, rather than Haskell's read/show
    encode = pure . string . show
    decode term = do
      s <- Expect.string term
      case TR.readEither s of
        Left msg -> fail $ "could not decode unsupported term: " ++ s
        Right t -> pure t

wrapToUnwrapped :: (Ord a, Read a, Show a) => TypeAdapter a
wrapToUnwrapped t@(TypeWrap (Nominal tname typ)) = do
    ad <- termAdapter typ
    return $ Adapter False t (adapterTarget ad) $ Coder (encode ad) (decode ad)
  where
    encode ad term = Expect.wrap tname term >>= coderEncode (adapterCoder ad)
    decode ad term = do
      decoded <- coderDecode (adapterCoder ad) term
      return $ TermWrap $ Nominal tname decoded

withGraphContext :: GraphFlow a x -> Flow (AdapterContext a) x
withGraphContext f = do
  cx <- getState
  withState (adapterContextGraph cx) f
