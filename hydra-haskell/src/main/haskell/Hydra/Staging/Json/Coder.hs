module Hydra.Staging.Json.Coder (jsonCoder, literalJsonCoder, untypedTermToJson) where

import Hydra.Core
import Hydra.Compute
import Hydra.Graph
import Hydra.Variants
import qualified Hydra.Encode.Core as EncodeCore
import Hydra.Literals
import Hydra.Rewriting
import Hydra.Monads hiding (fail, pure)
import Hydra.Adapt.Modules
import Hydra.Adapt.Terms
import Hydra.Staging.Json.Language
import Hydra.Lib.Literals
import Hydra.Adapt.Utils
import qualified Hydra.Json as Json
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


jsonCoder :: Type -> Flow Graph (Coder Graph Graph Term Json.Value)
jsonCoder typ = do
  adapter <- languageAdapter jsonLanguage typ
  coder <- termCoder $ adapterTarget adapter
  return $ composeCoders (adapterCoder adapter) coder

literalJsonCoder :: LiteralType -> Flow Graph (Coder Graph Graph Literal Json.Value)
literalJsonCoder at = pure $ case at of
  LiteralTypeBoolean -> Coder {
    coderEncode = \(LiteralBoolean b) -> pure $ Json.ValueBoolean b,
    coderDecode = \s -> case s of
      Json.ValueBoolean b -> pure $ LiteralBoolean b
      _ -> unexpected "boolean" $ show s}
  LiteralTypeFloat _ -> Coder {
    coderEncode = \(LiteralFloat (FloatValueBigfloat f)) -> pure $ Json.ValueNumber f,
    coderDecode = \s -> case s of
      Json.ValueNumber f -> pure $ LiteralFloat $ FloatValueBigfloat f
      _ -> unexpected "number" $ show s}
  LiteralTypeInteger _ -> Coder {
    coderEncode = \(LiteralInteger (IntegerValueBigint i)) -> pure $ Json.ValueNumber $ bigintToBigfloat i,
    coderDecode = \s -> case s of
      Json.ValueNumber f -> pure $ LiteralInteger $ IntegerValueBigint $ bigfloatToBigint f
      _ -> unexpected "number" $ show s}
  LiteralTypeString -> Coder {
    coderEncode = \(LiteralString s) -> pure $ Json.ValueString s,
    coderDecode = \s -> case s of
      Json.ValueString s' -> pure $ LiteralString s'
      _ -> unexpected "string" $ show s}

recordCoder :: RowType -> Flow Graph (Coder Graph Graph Term Json.Value)
recordCoder rt = do
    coders <- CM.mapM (\f -> (,) <$> pure f <*> termCoder (fieldTypeType f)) (rowTypeFields rt)
    return $ Coder (encode coders) (decode coders)
  where
    encode coders term = case deannotateTerm term of
      TermRecord (Record _ fields) -> Json.ValueObject . M.fromList . Y.catMaybes <$> CM.zipWithM encodeField coders fields
        where
          encodeField (ft, coder) (Field fname fv) = case (fieldTypeType ft, fv) of
            (TypeOptional _, TermOptional Nothing) -> pure Nothing
            _ -> Just <$> ((,) <$> pure (unName fname) <*> coderEncode coder fv)
      _ -> unexpected "record" $ show term
    decode coders n = case n of
      Json.ValueObject m -> Terms.record (rowTypeTypeName rt) <$> CM.mapM (decodeField m) coders -- Note: unknown fields are ignored
        where
          decodeField a (FieldType fname _, coder) = do
            v <- coderDecode coder $ Y.fromMaybe Json.ValueNull $ M.lookup (unName fname) m
            return $ Field fname v
      _ -> unexpected "object" $ show n
    getCoder coders fname = Y.maybe error pure $ M.lookup fname coders
      where
        error = fail $ "no such field: " ++ fname

termCoder :: Type -> Flow Graph (Coder Graph Graph Term Json.Value)
termCoder typ = case deannotateType typ of
  TypeLiteral at -> do
    ac <- literalJsonCoder at
    return Coder {
      coderEncode = \(TermLiteral av) -> coderEncode ac av,
      coderDecode = \n -> case n of
        s -> Terms.literal <$> coderDecode ac s}
  TypeList lt -> do
    lc <- termCoder lt
    return Coder {
      coderEncode = \(TermList els) -> Json.ValueArray <$> CM.mapM (coderEncode lc) els,
      coderDecode = \n -> case n of
        Json.ValueArray nodes -> Terms.list <$> CM.mapM (coderDecode lc) nodes
        _ -> unexpected "sequence" $ show n}
  TypeOptional ot -> do
    oc <- termCoder ot
    return Coder {
      coderEncode = \t -> case deannotateTerm t of
        TermOptional el -> Y.maybe (pure Json.ValueNull) (coderEncode oc) el
        _ -> unexpected "optional term" $ show t,
      coderDecode = \n -> case n of
        Json.ValueNull -> pure $ Terms.optional Nothing
        _ -> Terms.optional . Just <$> coderDecode oc n}
  TypeMap (MapType kt vt) -> do
      kc <- termCoder kt
      vc <- termCoder vt
      cx <- getState
      let encodeEntry (k, v) = (,) (toString cx k) <$> coderEncode vc v
      let decodeEntry (k, v) = (,) (fromString cx k) <$> coderDecode vc v
      return Coder {
        coderEncode = \(TermMap m) -> Json.ValueObject . M.fromList <$> CM.mapM encodeEntry (M.toList m),
        coderDecode = \n -> case n of
          Json.ValueObject m -> Terms.map . M.fromList <$> CM.mapM decodeEntry (M.toList m)
          _ -> unexpected "mapping" $ show n}
    where
      toString cx v = if isStringKey cx
        then case deannotateTerm v of
          TermLiteral (LiteralString s) -> s
        else show v
      fromString cx s = Terms.string $ if isStringKey cx then s else read s
      isStringKey cx = deannotateType kt == Types.string
  TypeRecord rt -> recordCoder rt
  TypeUnit -> pure $ unitCoder
  TypeVariable name -> return $ Coder encode decode
    where
      encode term = pure $ Json.ValueString $ "variable '" ++ unName name ++ "' for: " ++ show term
      decode term = fail $ "type variable " ++ unName name ++ " does not support decoding"
  _ -> fail $ "unsupported type in JSON: " ++ show (typeVariant typ)

-- Note: unused because we currently use JSON unit for optionals
unitCoder :: Coder Graph Graph Term Json.Value
unitCoder = Coder encode decode
  where
    encode term = case deannotateTerm term of
      TermUnit -> pure Json.ValueNull
      _ -> unexpected "unit" $ show term
    decode n = case n of
      Json.ValueNull -> pure Terms.unit
      _ -> unexpected "null" $ show n

-- | A simplistic, unidirectional encoding for terms as JSON values. Not type-aware; best used for human consumption.
untypedTermToJson :: Term -> Flow s Json.Value
untypedTermToJson term = case term of
    TermAnnotated (AnnotatedTerm term1 ann) -> do
        json <- untypedTermToJson term1
        pairs <- CM.mapM encodePair $ M.toList ann
        return $ Json.ValueObject $ M.fromList $ [
          ("term", json),
          ("annotations", Json.ValueObject $ M.fromList pairs)]
      where
        encodePair (Name k, v) = do
          json <- untypedTermToJson v
          return (k, json)
    TermApplication (Application lhs rhs) -> asRecord [
      Field _Application_function lhs,
      Field _Application_argument rhs]
    TermFunction f -> case f of
      FunctionElimination elm -> case elm of
        EliminationRecord (Projection _ fname) -> asVariant "project" $ TermVariable fname
        _ -> unexp $ "unexpected elimination variant: " ++ show (eliminationVariant elm)
      FunctionLambda (Lambda v d body) -> asRecord [
        Field _Lambda_parameter $ TermVariable v,
        Field _Lambda_domain $ TermOptional (EncodeCore.type_ <$> d),
        Field _Lambda_body body]
      FunctionPrimitive name -> pure $ Json.ValueString $ unName name
    TermLet (Let bindings env) -> asRecord [
        Field _Let_bindings $ TermRecord $ Record (Name "") (fromBinding <$> bindings),
        Field _Let_environment env]
      where
        fromBinding (LetBinding k v _) = Field k v
    TermList terms -> Json.ValueArray <$> (CM.mapM untypedTermToJson terms)
    TermLiteral lit -> pure $ case lit of
      LiteralBinary s -> Json.ValueString s
      LiteralBoolean b -> Json.ValueBoolean b
      LiteralFloat f -> Json.ValueNumber $ floatValueToBigfloat f
      LiteralInteger i -> Json.ValueNumber $ bigintToBigfloat $ integerValueToBigint i
      LiteralString s -> Json.ValueString s
    TermOptional mt -> case mt of
      Nothing -> pure Json.ValueNull
      Just t -> untypedTermToJson t
    TermProduct els -> untypedTermToJson $ TermList els
    TermRecord (Record _ fields) -> do
      keyvals <- CM.mapM fieldToKeyval fields
      return $ Json.ValueObject $ M.fromList $ Y.catMaybes keyvals
    TermSet vals -> untypedTermToJson $ TermList $ S.toList vals
    TermSum (Sum idx size term1) -> asRecord [
      Field _Sum_index $ TermLiteral $ LiteralInteger $ IntegerValueInt32 idx,
      Field _Sum_size $ TermLiteral $ LiteralInteger $ IntegerValueInt32 size,
      Field _Sum_term term1]
    TermTypeAbstraction (TypeAbstraction v term) -> asRecord [
      Field _TypeAbstraction_parameter $ TermVariable v,
      Field _TypeAbstraction_body term]
    TermTypeApplication (TypedTerm term1 typ) -> asRecord [ -- Note: TermTypeApplication and TermTyped appear identical
      Field _TypedTerm_term term1,
      Field _TypedTerm_type $ EncodeCore.type_ typ]
    TermUnion (Injection _ field) -> if fieldTerm field == Terms.unit
      then return $ Json.ValueString $ unName $ fieldName field
      else do
        mkeyval <- fieldToKeyval field
        return $ Json.ValueObject $ M.fromList $ case mkeyval of
          Nothing -> []
          Just keyval -> [keyval]
    TermVariable v -> pure $ Json.ValueString $ unName v
    TermWrap (WrappedTerm _ t) -> untypedTermToJson t
    t -> unexp $ "unsupported term variant: " ++ show (termVariant t)
--     t -> fail $ "unexpected term variant: " ++ show (termVariant t)
  where
    unexp msg = pure $ Json.ValueString $ "FAIL: " ++ msg
    asRecord = untypedTermToJson . TermRecord . Record (Name "")
    asVariant name term = untypedTermToJson $ TermUnion $ Injection (Name "") $ Field (Name name) term
    fieldToKeyval f = do
        mjson <- forTerm $ fieldTerm f
        return $ case mjson of
          Nothing -> Nothing
          Just j -> Just (unName $ fieldName f, j)
      where
        forTerm t = case t of
          TermOptional mt -> case mt of
            Nothing -> pure Nothing
            Just t' -> forTerm t'
          t' -> Just <$> untypedTermToJson t'
