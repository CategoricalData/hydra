module Hydra.Adapters.Term (
  fieldAdapter,
  termAdapter,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Adapter
import Hydra.Adapters.Literal
import Hydra.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Steps
import Hydra.Primitives
import Hydra.CoreDecoding
import Hydra.Adapters.Utils
import Hydra.Adapters.UtilsEtc
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Reduction

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


_context :: FieldName
_context = FieldName "context"

_record :: FieldName
_record = FieldName "record"

dereferenceNominal :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
dereferenceNominal acx t@(TypeNominal name) = do
  typ <- eitherToQualified $ do
    -- TODO: precompute the schema graph; don't construct it anew for each adapter
    scx <- schemaContext $ adapterContextEvaluation acx
    -- Note: we just assume the schema term is a reference to hydra/core.Type
    requireElement (Just "dereference nominal") scx name >>= decodeType (adapterContextEvaluation acx) . elementData
  ad <- termAdapter acx typ
  return ad { adapterSource = t }

elementToString :: AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
elementToString acx t@(TypeElement _) = pure $ Adapter False t Types.string $ Step encode decode
  where
    encode (TermElement (Name name)) = pure $ string name
    decode (TermLiteral (LiteralString name)) = pure $ TermElement $ Name name

fieldAdapter :: (Ord m, Read m, Show m) => AdapterContext m -> FieldType m -> Qualified (Adapter (FieldType m) (Field m))
fieldAdapter acx ftyp = do
  ad <- termAdapter acx $ fieldTypeType ftyp
  return $ Adapter (adapterIsLossy ad) ftyp (ftyp { fieldTypeType = adapterTarget ad })
    $ bidirectional $ \dir (Field name term) -> Field name <$> stepEither dir (adapterStep ad) term

functionToUnion :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
functionToUnion acx t@(TypeFunction (FunctionType dom _)) = do
    ut <- unionType
    ad <- termAdapter acx ut
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad)
      $ Step (encode ad) (decode ad)
  where
    cx = adapterContextEvaluation acx
    encode ad term = stepOut (adapterStep ad) $ case termExpr term of
      TermFunction f -> case f of
        FunctionCompareTo other -> variant _Function_compareTo other
        FunctionElimination e -> case e of
          EliminationElement -> unitVariant _Elimination_element
          EliminationNominal (Name name) -> variant _Elimination_nominal $ string name
          EliminationOptional _ -> variant _Elimination_optional $ string $ show term -- TODO
          EliminationRecord (FieldName fname) -> variant _Elimination_record $ string fname
          EliminationUnion _ -> variant _Elimination_union $ string $ show term -- TODO TermRecord cases
        FunctionLambda _ -> variant _Function_lambda $ string $ show term -- TODO
        FunctionPrimitive (Name name) -> variant _Function_primitive $ string name
      TermVariable (Variable var) -> variant _Term_variable $ string var
    decode ad term = do
        (Field fname fterm) <- stepIn (adapterStep ad) term >>= expectUnion
        Y.fromMaybe (notFound fname) $ M.lookup fname $ M.fromList [
          (_Elimination_element, forTerm fterm),
          (_Elimination_nominal, forNominal fterm),
          (_Elimination_optional, forOptionalCases fterm),
          (_Elimination_record, forProjection fterm),
          (_Elimination_union, forCases fterm),
          (_Function_compareTo, forCompareTo fterm),
          (_Function_lambda, forLambda fterm),
          (_Function_primitive, forPrimitive fterm),
          (_Term_variable, forVariable fterm)]
      where
        notFound fname = fail $ "unexpected field: " ++ unFieldName fname
        forCases fterm = read <$> expectString fterm -- TODO
        forCompareTo fterm = pure $ compareTo fterm
        forTerm _ = pure delta
        forLambda fterm = read <$> expectString fterm -- TODO
        forNominal fterm = eliminateNominal . Name <$> expectString fterm
        forOptionalCases fterm = read <$> expectString fterm -- TODO
        forPrimitive fterm = primitive . Name <$> expectString fterm
        forProjection fterm = projection . FieldName <$> expectString fterm
        forVariable fterm = variable <$> expectString fterm

    unionType = do
      domAd <- termAdapter acx dom
      return $ Types.union [
        FieldType _Elimination_element Types.unit,
        FieldType _Elimination_nominal Types.string,
        FieldType _Elimination_optional Types.string,
        FieldType _Elimination_record Types.string,
        FieldType _Elimination_union Types.string, -- TODO (TypeRecord cases)
        FieldType _Function_compareTo (adapterTarget domAd),
        FieldType _Function_lambda Types.string, -- TODO (TypeRecord [FieldType _Lambda_parameter Types.string, FieldType _Lambda_body cod]),
        FieldType _Function_primitive Types.string,
        FieldType _Term_variable Types.string]

listToSet :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
listToSet acx t@(TypeSet st) = do
    ad <- termAdapter acx $ Types.list st
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad)
      $ Step (encode ad) (decode ad)
  where
    encode ad (TermSet s) = stepOut (adapterStep ad) $ TermList $ S.toList s
    decode ad term = TermSet . S.fromList . (\(TermList l') -> l') <$> stepIn (adapterStep ad) term

optionalToList :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
optionalToList acx t@(TypeOptional ot) = do
  ad <- termAdapter acx ot
  return $ Adapter False t (Types.list $ adapterTarget ad) $ Step {
    stepOut = \(TermOptional m) -> Y.maybe
      (pure $ list [])
      (fmap (\ r -> list [r]) . stepOut (adapterStep ad)) m,
    stepIn = \(TermList l) -> optional <$> if L.null l then
      pure Nothing
      else Just <$> stepIn (adapterStep ad) (L.head l)}

passAnnotated :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) v)
passAnnotated acx t@(TypeAnnotated (Annotated at ann)) = do
  ad <- termAdapter acx at
  return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ bidirectional $ \dir term -> pure term

-- TODO: only tested for type mappings; not yet for types+terms
passApplication :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passApplication acx t = do
    ad <- termAdapter acx reduced
    return $ Adapter (adapterIsLossy ad) t reduced $ bidirectional $ \dir term -> stepEither dir (adapterStep ad) term
  where
    reduced = betaReduceType True (adapterContextEvaluation acx) t

passFunction :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passFunction acx t@(TypeFunction (FunctionType dom cod)) = do
    domAd <- termAdapter acx dom
    codAd <- termAdapter acx cod
    caseAds <- case typeExpr dom of
      TypeUnion sfields -> M.fromList . L.zip (fieldTypeName <$> sfields)
        <$> CM.mapM (fieldAdapter acx) sfields
      _ -> pure M.empty
    optionAd <- case typeExpr dom of
      TypeOptional ot -> Just <$> termAdapter acx (Types.function ot cod)
      _ -> pure Nothing
    let lossy = adapterIsLossy codAd || or (adapterIsLossy . snd <$> M.toList caseAds)
    let dom' = adapterTarget domAd
    let cod' = adapterTarget codAd
    return $ Adapter lossy t (Types.function dom' cod')
      $ bidirectional $ \dir (TermFunction f) -> TermFunction <$> case f of
        FunctionCompareTo other -> FunctionCompareTo <$> stepEither dir (adapterStep codAd) other
        FunctionElimination e -> FunctionElimination <$> case e of
          EliminationOptional (OptionalCases nothing just) -> EliminationOptional <$> (
            OptionalCases
              <$> stepEither dir (adapterStep codAd) nothing
              <*> (stepEither dir (adapterStep $ Y.fromJust optionAd) just))
          EliminationUnion cases -> EliminationUnion <$> CM.mapM (\f -> stepEither dir (getStep $ fieldName f) f) cases
            where
              -- Note: this causes unrecognized cases to simply be passed through;
              --       it is not the job of this adapter to catch validation issues.
              getStep fname = Y.maybe idStep adapterStep $ M.lookup fname caseAds
        FunctionLambda (Lambda var body) -> FunctionLambda <$> (Lambda var <$> stepEither dir (adapterStep codAd) body)

passLambda :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passLambda acx t@(TypeLambda (LambdaType (VariableType v) body)) = do
  ad <- termAdapter acx body
  return $ Adapter (adapterIsLossy ad) t (Types.lambda v $ adapterTarget ad)
    $ bidirectional $ \dir term -> stepEither dir (adapterStep ad) term

passLiteral :: AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passLiteral acx (TypeLiteral at) = do
  ad <- literalAdapter acx at
  let step = bidirectional $ \dir (TermLiteral av) -> literal <$> stepEither dir (adapterStep ad) av
  return $ Adapter (adapterIsLossy ad) (Types.literal $ adapterSource ad) (Types.literal $ adapterTarget ad) step

passList :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passList acx t@(TypeList lt) = do
  ad <- termAdapter acx lt
  return $ Adapter (adapterIsLossy ad) t (Types.list $ adapterTarget ad)
    $ bidirectional $ \dir (TermList terms) -> list <$> CM.mapM (stepEither dir $ adapterStep ad) terms

passMap :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passMap acx t@(TypeMap (MapType kt vt)) = do
  kad <- termAdapter acx kt
  vad <- termAdapter acx vt
  return $ Adapter (adapterIsLossy kad || adapterIsLossy vad)
    t (Types.map (adapterTarget kad) (adapterTarget vad))
    $ bidirectional $ \dir (TermMap m) -> TermMap . M.fromList
      <$> CM.mapM (\(k, v) -> (,) <$> stepEither dir (adapterStep kad) k <*> stepEither dir (adapterStep vad) v)
        (M.toList m)

passOptional :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passOptional acx t@(TypeOptional ot) = do
  ad <- termAdapter acx ot
  return $ Adapter (adapterIsLossy ad) t (Types.optional $ adapterTarget ad) $
    bidirectional $ \dir term -> case term of
      (TermOptional m) -> TermOptional <$> case m of
        Nothing -> pure Nothing
        Just term' -> Just <$> stepEither dir (adapterStep ad) term'
      _ -> fail $ "expected optional term, found: " ++ show term

passRecord :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passRecord acx t@(TypeRecord sfields) = do
  adapters <- CM.mapM (fieldAdapter acx) sfields
  let lossy = or $ adapterIsLossy <$> adapters
  let sfields' = adapterTarget <$> adapters
  return $ Adapter lossy t (Types.record sfields') $ bidirectional
    $ \dir (TermRecord dfields) -> record <$> CM.zipWithM (stepEither dir . adapterStep) adapters dfields

passSet :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passSet acx t@(TypeSet st) = do
  ad <- termAdapter acx st
  return $ Adapter (adapterIsLossy ad) t (Types.set $ adapterTarget ad)
    $ bidirectional $ \dir (TermSet terms) -> set . S.fromList
      <$> CM.mapM (stepEither dir (adapterStep ad)) (S.toList terms)

passUnion :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
passUnion acx t@(TypeUnion sfields) = do
    adapters <- M.fromList <$> CM.mapM (\f -> pure ((,) (fieldTypeName f)) <*> fieldAdapter acx f) sfields
    let lossy = or $ adapterIsLossy <$> adapters
    let sfields' = adapterTarget . snd <$> M.toList adapters
    return $ Adapter lossy t (Types.union sfields')
      $ bidirectional $ \dir (TermUnion dfield) -> do
        ad <- getAdapter adapters dfield
        TermUnion <$> stepEither dir (adapterStep ad) dfield
  where
    getAdapter adapters f = Y.maybe (fail $ "no such field: " ++ unFieldName (fieldName f)) pure $ M.lookup (fieldName f) adapters

-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings using Haskell's derived show/read format.
termAdapter :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
termAdapter acx typ = chooseAdapter alts supported describeType typ
  where
    alts t = (\c -> c acx t) <$> if variantIsSupported t
      then case typeVariant t of
        TypeVariantAnnotated -> pure passAnnotated
        TypeVariantApplication -> pure passApplication
        TypeVariantFunction ->  pure passFunction
        TypeVariantLambda -> pure passLambda
        TypeVariantList -> pure passList
        TypeVariantLiteral -> pure passLiteral
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
        TypeVariantLambda -> [lambdaToMonotype]
        _ -> []

    constraints = languageConstraints $ adapterContextTarget acx
    supported = typeIsSupported constraints
    variantIsSupported t = S.member (typeVariant t) $ languageConstraintsTypeVariants constraints

---- Caution: possibility of an infinite loop if neither unions, optionals, nor lists are supported
unionToRecord :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
unionToRecord acx t@(TypeUnion sfields) = do
    let target = Types.record $ makeOptional <$> sfields
    ad <- termAdapter acx target
    return $ Adapter (adapterIsLossy ad) t (adapterTarget ad) $ Step {
      stepOut = \(TermUnion (Field fn term)) -> stepOut (adapterStep ad)
        $ record (toRecordField term fn <$> sfields),
      stepIn = \term -> do
        TermRecord fields <- stepIn (adapterStep ad) term
        union <$> fromRecordFields term (TermRecord fields) (adapterTarget ad) fields}
  where
    makeOptional (FieldType fn ft) = FieldType fn $ Types.optional ft

    toRecordField term fn (FieldType fn' _) = Field fn' $
      TermOptional $ if fn' == fn then Just term else Nothing

    fromRecordFields term term' t' fields = if L.null matches
        then fail $ "cannot convert term back to union: " ++ show term ++ " -- becomes " ++ show term'
          ++ " where type = " ++ show t ++ "    and target type = " ++ show t'
        else pure $ L.head matches
      where
        matches = Y.mapMaybe (\(Field fn (TermOptional opt)) -> (Just . Field fn) =<< opt) fields

lambdaToMonotype :: (Ord m, Read m, Show m) => AdapterContext m -> Type m -> Qualified (Adapter (Type m) (Term m))
lambdaToMonotype acx t@(TypeLambda (LambdaType _ body)) = do
  ad <- termAdapter acx body
  return ad {adapterSource = t}
