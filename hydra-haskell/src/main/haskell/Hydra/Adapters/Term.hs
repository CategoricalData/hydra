-- | Adapter framework for types and terms

module Hydra.Adapters.Term (
  fieldAdapter,
  functionProxyName,
  functionProxyType,
  termAdapter,
) where

import Hydra.Adapters.Utils
import Hydra.Basics
import Hydra.Coders
import Hydra.Common
import Hydra.Compute
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.Graph
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Monads
import Hydra.Reduction
import Hydra.Adapters.Literal
import Hydra.Adapters.UtilsEtc
import Hydra.Dsl.Terms
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.Read as TR
import qualified Data.Maybe as Y


type TypeAdapter m = Type m -> Flow (AdapterContext m) (SymmetricAdapter (Graph m) (Type m) (Term m))

_context :: FieldName
_context = FieldName "context"

_record :: FieldName
_record = FieldName "record"

dereferenceNominal :: (Ord m, Read m, Show m) => TypeAdapter m
dereferenceNominal t@(TypeWrap name) = do
  typ <- withEvaluationContext $ do
    -- Note: we just assume the schema term is a reference to hydra/core.Type
    withTrace ("dereference nominal type " ++ unName name) $ do
      el <- withSchemaContext $ requireElement name
      epsilonDecodeType $ elementData el
  ad <- termAdapter typ
  return ad { adapterSource = t }

dropAnnotation :: (Ord m, Read m, Show m) => TypeAdapter m
dropAnnotation t@(TypeAnnotated (Annotated t' _)) = do
  ad <- termAdapter t'
  return $ Adapter False t (adapterTarget ad) $ Coder pure pure

elementToString :: TypeAdapter m
elementToString t@(TypeElement _) = pure $ Adapter False t Types.string $ Coder encode decode
  where
    encode (TermElement (Name name)) = pure $ string name
    decode (TermLiteral (LiteralString name)) = pure $ TermElement $ Name name

fieldAdapter :: (Ord m, Read m, Show m) => FieldType m -> Flow (AdapterContext m) (SymmetricAdapter (Graph m) (FieldType m) (Field m))
fieldAdapter ftyp = do
  ad <- termAdapter $ fieldTypeType ftyp
  return $ Adapter (adapterIsLossy ad) ftyp (ftyp { fieldTypeType = adapterTarget ad })
    $ bidirectional $ \dir (Field name term) -> Field name <$> encodeDecode dir (adapterCoder ad) term

functionProxyName :: Name
functionProxyName = Name "hydra/core.FunctionProxy"

functionProxyType :: Type m -> Type m
functionProxyType dom = TypeUnion $ RowType functionProxyName Nothing [
  FieldType _Elimination_element Types.unit,
  FieldType _Elimination_wrap Types.string,
  FieldType _Elimination_optional Types.string,
  FieldType _Elimination_record Types.string,
  FieldType _Elimination_union Types.string, -- TODO (TypeRecord cases)
  FieldType _Function_lambda Types.string, -- TODO (TypeRecord [FieldType _Lambda_parameter Types.string, FieldType _Lambda_body cod]),
  FieldType _Function_primitive Types.string,
  FieldType _Term_variable Types.string]

functionToUnion :: (Ord m, Read m, Show m) => TypeAdapter m
functionToUnion t@(TypeFunction (FunctionType dom _)) = do
    ut <- unionType
    ad <- termAdapter ut
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ Coder (encode ad) (decode ad)
  where
    encode ad term = coderEncode (adapterCoder ad) $ case stripTerm term of
      TermFunction f -> case f of
        FunctionElimination e -> case e of
          EliminationElement -> unitVariant functionProxyName _Elimination_element
          EliminationWrap (Name name) -> variant functionProxyName _Elimination_wrap $ string name
          EliminationOptional _ -> variant functionProxyName _Elimination_optional $ string $ show term -- TODO
          EliminationRecord _ -> variant functionProxyName _Elimination_record $ string $ show term -- TODO
          EliminationUnion _ -> variant functionProxyName _Elimination_union $ string $ show term -- TODO
        FunctionLambda _ -> variant functionProxyName _Function_lambda $ string $ show term -- TODO
        FunctionPrimitive (Name name) -> variant functionProxyName _Function_primitive $ string name
      TermVariable (Name var) -> variant functionProxyName _Term_variable $ string var
    decode ad term = do
        (Field fname fterm) <- coderDecode (adapterCoder ad) term >>= expectUnion
        Y.fromMaybe (notFound fname) $ M.lookup fname $ M.fromList [
          (_Elimination_element, forTerm fterm),
          (_Elimination_wrap, forWrapped fterm),
          (_Elimination_optional, forOptionalCases fterm),
          (_Elimination_record, forProjection fterm),
          (_Elimination_union, forCases fterm),
          (_Function_lambda, forLambda fterm),
          (_Function_primitive, forPrimitive fterm),
          (_Term_variable, forVariable fterm)]
      where
        notFound fname = fail $ "unexpected field: " ++ unFieldName fname
        forCases fterm = read <$> expectString fterm -- TODO
        forTerm _ = pure delta
        forLambda fterm = read <$> expectString fterm -- TODO
        forWrapped fterm = unwrap . Name <$> expectString fterm
        forOptionalCases fterm = read <$> expectString fterm -- TODO
        forPrimitive fterm = primitive . Name <$> expectString fterm
        forProjection fterm = read <$> expectString fterm -- TODO
        forVariable fterm = variable <$> expectString fterm

    unionType = do
      domAd <- termAdapter dom
      return $ TypeUnion $ RowType functionProxyName Nothing [
        FieldType _Elimination_element Types.unit,
        FieldType _Elimination_wrap Types.string,
        FieldType _Elimination_optional Types.string,
        FieldType _Elimination_record Types.string,
        FieldType _Elimination_union Types.string, -- TODO (TypeRecord cases)
        FieldType _Function_lambda Types.string, -- TODO (TypeRecord [FieldType _Lambda_parameter Types.string, FieldType _Lambda_body cod]),
        FieldType _Function_primitive Types.string,
        FieldType _Term_variable Types.string]

lambdaToMonotype :: (Ord m, Read m, Show m) => TypeAdapter m
lambdaToMonotype t@(TypeLambda (LambdaType _ body)) = do
  ad <- termAdapter body
  return ad {adapterSource = t}

listToSet :: (Ord m, Read m, Show m) => TypeAdapter m
listToSet t@(TypeSet st) = do
    ad <- termAdapter $ Types.list st
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ Coder (encode ad) (decode ad)
  where
    encode ad (TermSet s) = coderEncode (adapterCoder ad) $ TermList $ S.toList s
    decode ad term = TermSet . S.fromList . (\(TermList l') -> l') <$> coderDecode (adapterCoder ad) term

optionalToList :: (Ord m, Read m, Show m) => TypeAdapter m
optionalToList t@(TypeOptional ot) = do
  ad <- termAdapter ot
  return $ Adapter False t (Types.list $ adapterTarget ad) $ Coder {
    coderEncode = \(TermOptional m) -> Y.maybe
      (pure $ list [])
      (fmap (\ r -> list [r]) . coderEncode (adapterCoder ad)) m,
    coderDecode = \(TermList l) -> optional <$> if L.null l then
      pure Nothing
      else Just <$> coderDecode (adapterCoder ad) (L.head l)}

--passAnnotated :: (Ord m, Read m, Show m) => Type m -> TypeAdapter m
passAnnotated :: (Ord m, Read m, Show m) => Type m -> Flow (AdapterContext m) (SymmetricAdapter (Graph m) (Type m) v)
passAnnotated t@(TypeAnnotated (Annotated at ann)) = do
  ad <- termAdapter at
  return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ bidirectional $ \dir term -> pure term

--passAnnotated :: (Ord m, Read m, Show m) => Type m -> TypeAdapter m
--passAnnotated t@(TypeAnnotated (Annotated at ann)) = do
--  ad <- termAdapter at
--  return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ bidirectional $
--    \dir term -> encodeDecode dir (adapterCoder ad) term

-- TODO: only tested for type mappings; not yet for types+terms
passApplication :: (Ord m, Read m, Show m) => TypeAdapter m
passApplication t = do
    reduced <- withEvaluationContext $ betaReduceType t
    ad <- termAdapter reduced
    return $ Adapter (adapterIsLossy ad) t reduced $ bidirectional $
      \dir term -> encodeDecode dir (adapterCoder ad) term

passFunction :: (Ord m, Read m, Show m) => TypeAdapter m
passFunction t@(TypeFunction (FunctionType dom cod)) = do
    domAd <- termAdapter dom
    codAd <- termAdapter cod
    caseAds <- case stripType dom of
      TypeUnion rt -> M.fromList . L.zip (fieldTypeName <$> rowTypeFields rt)
        <$> CM.mapM fieldAdapter (rowTypeFields rt)
      _ -> pure M.empty
    optionAd <- case stripType dom of
      TypeOptional ot -> Just <$> termAdapter (Types.function ot cod)
      _ -> pure Nothing
    let lossy = adapterIsLossy codAd || or (adapterIsLossy . snd <$> M.toList caseAds)
    let dom' = adapterTarget domAd
    let cod' = adapterTarget codAd
    return $ Adapter lossy t (Types.function dom' cod')
      $ bidirectional $ \dir term -> case stripTerm term of
        TermFunction f -> TermFunction <$> case f of
          FunctionElimination e -> FunctionElimination <$> case e of
            EliminationOptional (OptionalCases nothing just) -> EliminationOptional <$> (
              OptionalCases
                <$> encodeDecode dir (adapterCoder codAd) nothing
                <*> (encodeDecode dir (adapterCoder $ Y.fromJust optionAd) just))
            EliminationUnion (CaseStatement n cases) -> EliminationUnion . CaseStatement n <$>
                CM.mapM (\f -> encodeDecode dir (getCoder $ fieldName f) f) cases
              where
                -- Note: this causes unrecognized cases to simply be passed through;
                --       it is not the job of this adapter to catch validation issues.
                getCoder fname = Y.maybe idCoder adapterCoder $ M.lookup fname caseAds
          FunctionLambda (Lambda var body) -> FunctionLambda <$> (Lambda var <$> encodeDecode dir (adapterCoder codAd) body)
        _ -> unexpected "function term" $ show term

passLambda :: (Ord m, Read m, Show m) => TypeAdapter m
passLambda t@(TypeLambda (LambdaType (Name v) body)) = do
  ad <- termAdapter body
  return $ Adapter (adapterIsLossy ad) t (Types.lambda v $ adapterTarget ad)
    $ bidirectional $ \dir term -> encodeDecode dir (adapterCoder ad) term

passLiteral :: TypeAdapter m
passLiteral (TypeLiteral at) = do
  ad <- literalAdapter at
  let step = bidirectional $ \dir (TermLiteral av) -> literal <$> encodeDecode dir (adapterCoder ad) av
  return $ Adapter (adapterIsLossy ad) (Types.literal $ adapterSource ad) (Types.literal $ adapterTarget ad) step

passList :: (Ord m, Read m, Show m) => TypeAdapter m
passList t@(TypeList lt) = do
  ad <- termAdapter lt
  return $ Adapter (adapterIsLossy ad) t (Types.list $ adapterTarget ad)
    $ bidirectional $ \dir (TermList terms) -> list <$> CM.mapM (encodeDecode dir $ adapterCoder ad) terms

passMap :: (Ord m, Read m, Show m) => TypeAdapter m
passMap t@(TypeMap (MapType kt vt)) = do
  kad <- termAdapter kt
  vad <- termAdapter vt
  return $ Adapter (adapterIsLossy kad || adapterIsLossy vad)
    t (Types.map (adapterTarget kad) (adapterTarget vad))
    $ bidirectional $ \dir (TermMap m) -> TermMap . M.fromList
      <$> CM.mapM (\(k, v) -> (,) <$> encodeDecode dir (adapterCoder kad) k <*> encodeDecode dir (adapterCoder vad) v)
        (M.toList m)

passOptional :: (Ord m, Read m, Show m) => TypeAdapter m
passOptional t@(TypeOptional ot) = do
  ad <- termAdapter ot
  return $ Adapter (adapterIsLossy ad) t (Types.optional $ adapterTarget ad) $
    bidirectional $ \dir term -> case term of
      (TermOptional m) -> TermOptional <$> case m of
        Nothing -> pure Nothing
        Just term' -> Just <$> encodeDecode dir (adapterCoder ad) term'
      _ -> fail $ "expected optional term, found: " ++ show term

passProduct :: (Ord m, Read m, Show m) => TypeAdapter m
passProduct t@(TypeProduct types) = do
  ads <- CM.mapM termAdapter types
  let lossy = L.foldl (\b ad -> b || adapterIsLossy ad) False ads
  return $ Adapter lossy t (Types.product (adapterTarget <$> ads))
    $ bidirectional $ \dir (TermProduct tuple) -> TermProduct <$> (CM.zipWithM (\term ad -> encodeDecode dir (adapterCoder ad) term) tuple ads)

passRecord :: (Ord m, Read m, Show m) => TypeAdapter m
passRecord t@(TypeRecord rt) = do
  adapters <- CM.mapM fieldAdapter (rowTypeFields rt)
  let lossy = or $ adapterIsLossy <$> adapters
  let sfields' = adapterTarget <$> adapters
  return $ Adapter lossy t (TypeRecord $ rt {rowTypeFields = sfields'}) $ bidirectional
    $ \dir (TermRecord (Record _ dfields)) -> record (rowTypeTypeName rt) <$> CM.zipWithM (encodeDecode dir . adapterCoder) adapters dfields

passSet :: (Ord m, Read m, Show m) => TypeAdapter m
passSet t@(TypeSet st) = do
  ad <- termAdapter st
  return $ Adapter (adapterIsLossy ad) t (Types.set $ adapterTarget ad)
    $ bidirectional $ \dir (TermSet terms) -> set . S.fromList
      <$> CM.mapM (encodeDecode dir (adapterCoder ad)) (S.toList terms)

passSum :: (Ord m, Read m, Show m) => TypeAdapter m
passSum t@(TypeSum types) = do
  ads <- CM.mapM termAdapter types
  let lossy = L.foldl (\b ad -> b || adapterIsLossy ad) False ads
  return $ Adapter lossy t (Types.sum (adapterTarget <$> ads))
    $ bidirectional $ \dir (TermSum (Sum i n term)) -> TermSum . Sum i n <$> encodeDecode dir (adapterCoder $ ads !! i) term

passUnion :: (Ord m, Read m, Show m) => TypeAdapter m
passUnion t@(TypeUnion rt) = do
    adapters <- M.fromList <$> CM.mapM (\f -> pure ((,) (fieldTypeName f)) <*> fieldAdapter f) sfields
    let lossy = or $ adapterIsLossy <$> adapters
    let sfields' = adapterTarget . snd <$> M.toList adapters
    return $ Adapter lossy t (TypeUnion $ rt {rowTypeFields = sfields'})
      $ bidirectional $ \dir term -> do
        dfield <- expectUnion term
        ad <- getAdapter adapters dfield
        TermUnion . Injection nm <$> encodeDecode dir (adapterCoder ad) dfield
  where
    getAdapter adapters f = Y.maybe (fail $ "no such field: " ++ unFieldName (fieldName f)) pure $ M.lookup (fieldName f) adapters
    sfields = rowTypeFields rt
    nm = rowTypeTypeName rt

simplifyApplication :: (Ord m, Read m, Show m) => TypeAdapter m
simplifyApplication t@(TypeApplication (ApplicationType lhs _)) = do
  ad <- termAdapter lhs
  return $ Adapter False t (adapterTarget ad) $ bidirectional $ \dir term -> encodeDecode dir (adapterCoder ad) term

-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings using Haskell's derived show/read format.
termAdapter :: (Ord m, Read m, Show m) => TypeAdapter m
termAdapter typ = do
   acx <- getState
   chooseAdapter (alts acx) (supported acx) describeType typ
  where
    alts acx t = (\c -> c t) <$> if variantIsSupported acx t
      then case typeVariant t of
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
      else case typeVariant t of
        TypeVariantAnnotated -> [dropAnnotation]
        TypeVariantApplication -> [simplifyApplication]
        TypeVariantElement -> [elementToString]
        TypeVariantFunction -> [functionToUnion]
        TypeVariantLambda -> [lambdaToMonotype]
        TypeVariantWrap -> [dereferenceNominal]
        TypeVariantOptional -> [optionalToList]
        TypeVariantSet ->  [listToSet]
        TypeVariantUnion -> [unionToRecord]
        _ -> [unsupportedToString]

    constraints acx = languageConstraints $ adapterContextTarget acx
    supported acx = typeIsSupported (constraints acx)
    variantIsSupported acx t = S.member (typeVariant t) $ languageConstraintsTypeVariants (constraints acx)

---- Caution: possibility of an infinite loop if neither unions, optionals, nor lists are supported
unionToRecord :: (Ord m, Read m, Show m) => TypeAdapter m
unionToRecord t@(TypeUnion rt) = do
    let target = TypeRecord $ rt {rowTypeFields = makeOptional <$> sfields}
    ad <- termAdapter target
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ Coder {
      coderEncode = \(TermUnion (Injection _ (Field fn term))) -> coderEncode (adapterCoder ad)
        $ record nm (toRecordField term fn <$> sfields),
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

unsupportedToString :: (Ord m, Read m, Show m) => TypeAdapter m
unsupportedToString t = pure $ Adapter False t Types.string $ Coder encode decode
  where
    -- TODO: use JSON for encoding and decoding unsupported terms, rather than Haskell's read/show
    encode = pure . string . show
    decode term = do
      s <- expectString term
      case TR.readEither s of
        Left msg -> fail $ "could not decode unsupported term: " ++ s
        Right t -> pure t

withEvaluationContext :: GraphFlow m a -> Flow (AdapterContext m) a
withEvaluationContext f = do
  acx <- getState
  withState (adapterContextGraph acx) f
