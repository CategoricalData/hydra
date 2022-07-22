module Hydra.Ext.Json.Coder (jsonCoder) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Adapter
import Hydra.Adapters.Term
import Hydra.CoreLanguage
import Hydra.Impl.Haskell.Extras
import Hydra.Steps
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Ext.Json.Language
import qualified Hydra.Ext.Json.Model as Json
import Hydra.Lib.Literals

import qualified Control.Monad as CM
import qualified Data.Map as M
import qualified Data.Maybe as Y


jsonCoder :: (Eq m, Ord m, Read m, Show m) => Context m -> Type m -> Qualified (Coder (Term m) Json.Value)
jsonCoder cx typ = do
    adapter <- termAdapter adContext typ
    coder <- termCoder cx $ adapterTarget adapter
    return $ composeSteps (adapterCoder adapter) coder
  where
    adContext = AdapterContext cx hydraCoreLanguage (language cx)

literalCoder :: LiteralType -> Qualified (Coder Literal Json.Value)
literalCoder at = pure $ case at of
  LiteralTypeBoolean -> Coder {
    coderEncode = \(LiteralBoolean b) -> pure $ Json.ValueBoolean b,
    coderDecode = \s -> case s of
      Json.ValueBoolean b -> pure $ LiteralBoolean b
      _ -> unexpected "boolean" s}
  LiteralTypeFloat _ -> Coder {
    coderEncode = \(LiteralFloat (FloatValueBigfloat f)) -> pure $ Json.ValueNumber f,
    coderDecode = \s -> case s of
      Json.ValueNumber f -> pure $ LiteralFloat $ FloatValueBigfloat f
      _ -> unexpected "number" s}
  LiteralTypeInteger _ -> Coder {
    coderEncode = \(LiteralInteger (IntegerValueBigint i)) -> pure $ Json.ValueNumber $ bigintToBigfloat i,
    coderDecode = \s -> case s of
      Json.ValueNumber f -> pure $ LiteralInteger $ IntegerValueBigint $ bigfloatToBigint f
      _ -> unexpected "number" s}
  LiteralTypeString -> Coder {
    coderEncode = \(LiteralString s) -> pure $ Json.ValueString s,
    coderDecode = \s -> case s of
      Json.ValueString s' -> pure $ LiteralString s'
      _ -> unexpected "string" s}

recordCoder :: (Eq m, Ord m, Read m, Show m) => Context m -> [FieldType m] -> Qualified (Coder (Term m) Json.Value)
recordCoder cx sfields = do
    coders <- CM.mapM (\f -> (,) <$> pure f <*> termCoder cx (fieldTypeType f)) sfields
    return $ Coder (encode coders) (decode coders)
  where
    encode coders term = case termExpr cx term of
      TermRecord fields -> Json.ValueObject . M.fromList . Y.catMaybes <$> CM.zipWithM encodeField coders fields
        where
          encodeField (ft, coder) (Field fname fv) = case (fieldTypeType ft, fv) of
            (TypeOptional _, TermOptional Nothing) -> pure Nothing
            _ -> Just <$> ((,) <$> pure (unFieldName fname) <*> coderEncode coder fv)
      _ -> unexpected "record" term
    decode coders n = case n of
      Json.ValueObject m -> Terms.record <$> CM.mapM (decodeField m) coders -- Note: unknown fields are ignored
        where
          decodeField m (FieldType fname _, coder) = do
            v <- coderDecode coder $ Y.fromMaybe Json.ValueNull $ M.lookup (unFieldName fname) m
            return $ Field fname v
      _ -> unexpected "mapping" n
    getCoder coders fname = Y.maybe error pure $ M.lookup fname coders
      where
        error = fail $ "no such field: " ++ fname

termCoder :: (Eq m, Ord m, Read m, Show m) => Context m -> Type m -> Qualified (Coder (Term m) Json.Value)
termCoder cx typ = case typeExpr cx typ of
  TypeLiteral at -> do
    ac <- literalCoder at
    return Coder {
      coderEncode = \(TermLiteral av) -> coderEncode ac av,
      coderDecode = \n -> case n of
        s -> Terms.literal <$> coderDecode ac s}
  TypeList lt -> do
    lc <- termCoder cx lt
    return Coder {
      coderEncode = \(TermList els) -> Json.ValueArray <$> CM.mapM (coderEncode lc) els,
      coderDecode = \n -> case n of
        Json.ValueArray nodes -> Terms.list <$> CM.mapM (coderDecode lc) nodes
        _ -> unexpected "sequence" n}
  TypeOptional ot -> do
    oc <- termCoder cx ot
    return Coder {
      coderEncode = \t -> case t of
        TermOptional el -> Y.maybe (pure Json.ValueNull) (coderEncode oc) el
        _ -> unexpected "optional term" t,
      coderDecode = \n -> case n of
        Json.ValueNull -> pure $ Terms.optional Nothing
        _ -> Terms.optional . Just <$> coderDecode oc n}
  TypeMap (MapType kt vt) -> do
      kc <- termCoder cx kt
      vc <- termCoder cx vt
      let encodeEntry (k, v) = (,) (toString k) <$> coderEncode vc v
      let decodeEntry (k, v) = (,) (fromString k) <$> coderDecode vc v
      return Coder {
        coderEncode = \(TermMap m) -> Json.ValueObject . M.fromList <$> CM.mapM encodeEntry (M.toList m),
        coderDecode = \n -> case n of
          Json.ValueObject m -> Terms.map . M.fromList <$> CM.mapM decodeEntry (M.toList m)
          _ -> unexpected "mapping" n}
    where
      toString v = if isStringKey
        then case termExpr cx v of
          TermLiteral (LiteralString s) -> s
        else show v
      fromString s = Terms.string $ if isStringKey then s else read s
      isStringKey = typeExpr cx kt == Types.string
  TypeRecord sfields -> recordCoder cx sfields
