module Hydra.Ext.Json.Coder (jsonCoder) where

import Hydra.All
import Hydra.Adapters.Term
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Ext.Json.Language
import qualified Hydra.Ext.Json.Model as Json
import Hydra.Lib.Literals
import Hydra.Adapters.UtilsEtc

import qualified Control.Monad as CM
import qualified Data.Map as M
import qualified Data.Maybe as Y


jsonCoder :: (Eq m, Ord m, Read m, Show m) => Type m -> GraphFlow m (Coder (Context m) (Context m) (Term m) Json.Value)
jsonCoder typ = do
    cx <- getState
    let acx = AdapterContext cx hydraCoreLanguage (language cx)
    adapter <- withState acx $ termAdapter typ
    coder <- termCoder $ adapterTarget adapter
    return $ composeCoders (adapterCoder adapter) coder

literalCoder :: LiteralType -> GraphFlow m (Coder (Context m) (Context m) Literal Json.Value)
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

recordCoder :: (Eq m, Ord m, Read m, Show m) => RowType m -> GraphFlow m (Coder (Context m) (Context m) (Term m) Json.Value)
recordCoder rt = do
    coders <- CM.mapM (\f -> (,) <$> pure f <*> termCoder (fieldTypeType f)) (rowTypeFields rt)
    return $ Coder (encode coders) (decode coders)
  where
    encode coders term = case stripTerm term of
      TermRecord (Record _ fields) -> Json.ValueObject . M.fromList . Y.catMaybes <$> CM.zipWithM encodeField coders fields
        where
          encodeField (ft, coder) (Field fname fv) = case (fieldTypeType ft, fv) of
            (TypeOptional _, TermOptional Nothing) -> pure Nothing
            _ -> Just <$> ((,) <$> pure (unFieldName fname) <*> coderEncode coder fv)
      _ -> unexpected "record" term
    decode coders n = case n of
      Json.ValueObject m -> Terms.record (rowTypeTypeName rt) <$> CM.mapM (decodeField m) coders -- Note: unknown fields are ignored
        where
          decodeField m (FieldType fname _, coder) = do
            v <- coderDecode coder $ Y.fromMaybe Json.ValueNull $ M.lookup (unFieldName fname) m
            return $ Field fname v
      _ -> unexpected "mapping" n
    getCoder coders fname = Y.maybe error pure $ M.lookup fname coders
      where
        error = fail $ "no such field: " ++ fname

termCoder :: (Eq m, Ord m, Read m, Show m) => Type m -> GraphFlow m (Coder (Context m) (Context m) (Term m) Json.Value)
termCoder typ = case stripType typ of
  TypeLiteral at -> do
    ac <- literalCoder at
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
        _ -> unexpected "sequence" n}
  TypeOptional ot -> do
    oc <- termCoder ot
    return Coder {
      coderEncode = \t -> case t of
        TermOptional el -> Y.maybe (pure Json.ValueNull) (coderEncode oc) el
        _ -> unexpected "optional term" t,
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
          _ -> unexpected "mapping" n}
    where
      toString cx v = if isStringKey cx
        then case stripTerm v of
          TermLiteral (LiteralString s) -> s
        else show v
      fromString cx s = Terms.string $ if isStringKey cx then s else read s
      isStringKey cx = stripType kt == Types.string
  TypeRecord rt -> recordCoder rt
