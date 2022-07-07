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
import Hydra.Rewriting

import qualified Control.Monad as CM
import qualified Data.Map as M
import qualified Data.Maybe as Y


jsonCoder :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> Type m -> Qualified (Step (Term m) Json.Value)
jsonCoder context typ = do
    adapter <- termAdapter adContext typ
    coder <- termCoder $ adapterTarget adapter
    return $ composeSteps (adapterStep adapter) coder
  where
    adContext = AdapterContext context hydraCoreLanguage language

literalCoder :: LiteralType -> Qualified (Step Literal Json.Value)
literalCoder at = pure $ case at of
  LiteralTypeBoolean -> Step {
    stepOut = \(LiteralBoolean b) -> pure $ Json.ValueBoolean b,
    stepIn = \s -> case s of
      Json.ValueBoolean b -> pure $ LiteralBoolean b
      _ -> unexpected "boolean" s}
  LiteralTypeFloat _ -> Step {
    stepOut = \(LiteralFloat (FloatValueBigfloat f)) -> pure $ Json.ValueNumber f,
    stepIn = \s -> case s of
      Json.ValueNumber f -> pure $ LiteralFloat $ FloatValueBigfloat f
      _ -> unexpected "number" s}
  LiteralTypeInteger _ -> Step {
    stepOut = \(LiteralInteger (IntegerValueBigint i)) -> pure $ Json.ValueNumber $ bigintToBigfloat i,
    stepIn = \s -> case s of
      Json.ValueNumber f -> pure $ LiteralInteger $ IntegerValueBigint $ bigfloatToBigint f
      _ -> unexpected "number" s}
  LiteralTypeString -> Step {
    stepOut = \(LiteralString s) -> pure $ Json.ValueString s,
    stepIn = \s -> case s of
      Json.ValueString s' -> pure $ LiteralString s'
      _ -> unexpected "string" s}

recordCoder :: (Default m, Eq m, Ord m, Read m, Show m) => [FieldType m] -> Qualified (Step (Term m) Json.Value)
recordCoder sfields = do
    coders <- CM.mapM (\f -> (,) <$> pure f <*> termCoder (fieldTypeType f)) sfields
    return $ Step (encode coders) (decode coders)
  where
    encode coders term = case termExpr term of
      TermRecord fields -> Json.ValueObject . M.fromList . Y.catMaybes <$> CM.zipWithM encodeField coders fields
        where
          encodeField (ft, coder) (Field fname fv) = case (fieldTypeType ft, fv) of
            (TypeOptional _, TermOptional Nothing) -> pure Nothing
            _ -> Just <$> ((,) <$> pure (unFieldName fname) <*> stepOut coder fv)
      _ -> unexpected "record" term
    decode coders n = case n of
      Json.ValueObject m -> Terms.record <$> CM.mapM (decodeField m) coders -- Note: unknown fields are ignored
        where
          decodeField m (FieldType fname _, coder) = do
            v <- stepIn coder $ Y.fromMaybe Json.ValueNull $ M.lookup (unFieldName fname) m
            return $ Field fname v
      _ -> unexpected "mapping" n
    getCoder coders fname = Y.maybe error pure $ M.lookup fname coders
      where
        error = fail $ "no such field: " ++ fname

termCoder :: (Default m, Eq m, Ord m, Read m, Show m) => Type m -> Qualified (Step (Term m) Json.Value)
termCoder typ = case typeExpr typ of
  TypeLiteral at -> do
    ac <- literalCoder at
    return Step {
      stepOut = \(TermLiteral av) -> stepOut ac av,
      stepIn = \n -> case n of
        s -> Terms.literal <$> stepIn ac s}
  TypeList lt -> do
    lc <- termCoder lt
    return Step {
      stepOut = \(TermList els) -> Json.ValueArray <$> CM.mapM (stepOut lc) els,
      stepIn = \n -> case n of
        Json.ValueArray nodes -> Terms.list <$> CM.mapM (stepIn lc) nodes
        _ -> unexpected "sequence" n}
  TypeOptional ot -> do
    oc <- termCoder ot
    return Step {
      stepOut = \(TermOptional el) -> Y.maybe (pure Json.ValueNull) (stepOut oc) el,
      stepIn = \n -> case n of
        Json.ValueNull -> pure $ Terms.optional Nothing
        _ -> Terms.optional . Just <$> stepIn oc n}
  TypeMap (MapType kt vt) -> do
      kc <- termCoder kt
      vc <- termCoder vt
      let encodeEntry (k, v) = (,) (toString k) <$> stepOut vc v
      let decodeEntry (k, v) = (,) (fromString k) <$> stepIn vc v
      return Step {
        stepOut = \(TermMap m) -> Json.ValueObject . M.fromList <$> CM.mapM encodeEntry (M.toList m),
        stepIn = \n -> case n of
          Json.ValueObject m -> Terms.map . M.fromList <$> CM.mapM decodeEntry (M.toList m)
          _ -> unexpected "mapping" n}
    where
      toString v = if isStringKey
        then case termExpr v of
          TermLiteral (LiteralString s) -> s
        else show v
      fromString s = Terms.string $ if isStringKey then s else read s
      isStringKey = typeExpr kt == Types.string
  TypeRecord sfields -> recordCoder sfields
