module Hydra.Staging.Json.Coder (jsonCoder, literalJsonCoder, untypedTermToJson) where

import qualified Hydra.Adapt.Modules as AdaptModules
import qualified Hydra.Adapt.Terms as AdaptTerms
import qualified Hydra.Adapt.Utils as AdaptUtils
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Literals as HydraLiterals
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Staging.Json.Language as JsonLanguage
import qualified Hydra.Variants as Variants
import qualified Hydra.Json as Json
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


jsonCoder :: Core.Type -> Compute.Flow Graph.Graph (Compute.Coder Graph.Graph Graph.Graph Core.Term Json.Value)
jsonCoder typ = do
  adapter <- AdaptModules.languageAdapter JsonLanguage.jsonLanguage typ
  coder <- termCoder $ Compute.adapterTarget adapter
  return $ AdaptUtils.composeCoders (Compute.adapterCoder adapter) coder

literalJsonCoder :: Core.LiteralType -> Compute.Flow Graph.Graph (Compute.Coder Graph.Graph Graph.Graph Core.Literal Json.Value)
literalJsonCoder at = Monads.pure $ case at of
  Core.LiteralTypeBoolean -> Compute.Coder {
    Compute.coderEncode = \(Core.LiteralBoolean b) -> Monads.pure $ Json.ValueBoolean b,
    Compute.coderDecode = \s -> case s of
      Json.ValueBoolean b -> Monads.pure $ Core.LiteralBoolean b
      _ -> Monads.unexpected "boolean" $ show s}
  Core.LiteralTypeFloat _ -> Compute.Coder {
    Compute.coderEncode = \(Core.LiteralFloat (Core.FloatValueBigfloat f)) -> Monads.pure $ Json.ValueNumber f,
    Compute.coderDecode = \s -> case s of
      Json.ValueNumber f -> Monads.pure $ Core.LiteralFloat $ Core.FloatValueBigfloat f
      _ -> Monads.unexpected "number" $ show s}
  Core.LiteralTypeInteger _ -> Compute.Coder {
    Compute.coderEncode = \(Core.LiteralInteger (Core.IntegerValueBigint i)) -> Monads.pure $ Json.ValueNumber $ Literals.bigintToBigfloat i,
    Compute.coderDecode = \s -> case s of
      Json.ValueNumber f -> Monads.pure $ Core.LiteralInteger $ Core.IntegerValueBigint $ Literals.bigfloatToBigint f
      _ -> Monads.unexpected "number" $ show s}
  Core.LiteralTypeString -> Compute.Coder {
    Compute.coderEncode = \(Core.LiteralString s) -> Monads.pure $ Json.ValueString s,
    Compute.coderDecode = \s -> case s of
      Json.ValueString s' -> Monads.pure $ Core.LiteralString s'
      _ -> Monads.unexpected "string" $ show s}

recordCoder :: Core.RowType -> Compute.Flow Graph.Graph (Compute.Coder Graph.Graph Graph.Graph Core.Term Json.Value)
recordCoder rt = do
    coders <- CM.mapM (\f -> (,) <$> Monads.pure f <*> termCoder (Core.fieldTypeType f)) (Core.rowTypeFields rt)
    return $ Compute.Coder (encode coders) (decode coders)
  where
    encode coders term = case Rewriting.deannotateTerm term of
      Core.TermRecord (Core.Record _ fields) -> Json.ValueObject . M.fromList . Y.catMaybes <$> CM.zipWithM encodeField coders fields
        where
          encodeField (ft, coder) (Core.Field fname fv) = case (Core.fieldTypeType ft, fv) of
            (Core.TypeOptional _, Core.TermOptional Nothing) -> Monads.pure Nothing
            _ -> Just <$> ((,) <$> Monads.pure (Core.unName fname) <*> Compute.coderEncode coder fv)
      _ -> Monads.unexpected "record" $ show term
    decode coders n = case n of
      Json.ValueObject m -> Terms.record (Core.rowTypeTypeName rt) <$> CM.mapM (decodeField m) coders -- Note: unknown fields are ignored
        where
          decodeField a (Core.FieldType fname _, coder) = do
            v <- Compute.coderDecode coder $ Y.fromMaybe Json.ValueNull $ M.lookup (Core.unName fname) m
            return $ Core.Field fname v
      _ -> Monads.unexpected "object" $ show n
    getCoder coders fname = Y.maybe error Monads.pure $ M.lookup fname coders
      where
        error = Monads.fail $ "no such field: " ++ fname

termCoder :: Core.Type -> Compute.Flow Graph.Graph (Compute.Coder Graph.Graph Graph.Graph Core.Term Json.Value)
termCoder typ = case Rewriting.deannotateType typ of
  Core.TypeLiteral at -> do
    ac <- literalJsonCoder at
    return Compute.Coder {
      Compute.coderEncode = \(Core.TermLiteral av) -> Compute.coderEncode ac av,
      Compute.coderDecode = \n -> case n of
        s -> Terms.literal <$> Compute.coderDecode ac s}
  Core.TypeList lt -> do
    lc <- termCoder lt
    return Compute.Coder {
      Compute.coderEncode = \(Core.TermList els) -> Json.ValueArray <$> CM.mapM (Compute.coderEncode lc) els,
      Compute.coderDecode = \n -> case n of
        Json.ValueArray nodes -> Terms.list <$> CM.mapM (Compute.coderDecode lc) nodes
        _ -> Monads.unexpected "sequence" $ show n}
  Core.TypeOptional ot -> do
    oc <- termCoder ot
    return Compute.Coder {
      Compute.coderEncode = \t -> case Rewriting.deannotateTerm t of
        Core.TermOptional el -> Y.maybe (Monads.pure Json.ValueNull) (Compute.coderEncode oc) el
        _ -> Monads.unexpected "optional term" $ show t,
      Compute.coderDecode = \n -> case n of
        Json.ValueNull -> Monads.pure $ Terms.optional Nothing
        _ -> Terms.optional . Just <$> Compute.coderDecode oc n}
  Core.TypeMap (Core.MapType kt vt) -> do
      kc <- termCoder kt
      vc <- termCoder vt
      cx <- Monads.getState
      let encodeEntry (k, v) = (,) (toString cx k) <$> Compute.coderEncode vc v
      let decodeEntry (k, v) = (,) (fromString cx k) <$> Compute.coderDecode vc v
      return Compute.Coder {
        Compute.coderEncode = \(Core.TermMap m) -> Json.ValueObject . M.fromList <$> CM.mapM encodeEntry (M.toList m),
        Compute.coderDecode = \n -> case n of
          Json.ValueObject m -> Terms.map . M.fromList <$> CM.mapM decodeEntry (M.toList m)
          _ -> Monads.unexpected "mapping" $ show n}
    where
      toString cx v = if isStringKey cx
        then case Rewriting.deannotateTerm v of
          Core.TermLiteral (Core.LiteralString s) -> s
        else show v
      fromString cx s = Terms.string $ if isStringKey cx then s else read s
      isStringKey cx = Rewriting.deannotateType kt == Types.string
  Core.TypeRecord rt -> recordCoder rt
  Core.TypeUnit -> Monads.pure $ unitCoder
  Core.TypeVariable name -> return $ Compute.Coder encode decode
    where
      encode term = Monads.pure $ Json.ValueString $ "variable '" ++ Core.unName name ++ "' for: " ++ show term
      decode term = Monads.fail $ "type variable " ++ Core.unName name ++ " does not support decoding"
  _ -> Monads.fail $ "unsupported type in JSON: " ++ show (Variants.typeVariant typ)

-- Note: unused because we currently use JSON unit for optionals
unitCoder :: Compute.Coder Graph.Graph Graph.Graph Core.Term Json.Value
unitCoder = Compute.Coder encode decode
  where
    encode term = case Rewriting.deannotateTerm term of
      Core.TermUnit -> Monads.pure Json.ValueNull
      _ -> Monads.unexpected "unit" $ show term
    decode n = case n of
      Json.ValueNull -> Monads.pure Terms.unit
      _ -> Monads.unexpected "null" $ show n

-- | A simplistic, unidirectional encoding for terms as JSON values. Not type-aware; best used for human consumption.
untypedTermToJson :: Core.Term -> Compute.Flow s Json.Value
untypedTermToJson term = case term of
    Core.TermAnnotated (Core.AnnotatedTerm term1 ann) -> do
        json <- untypedTermToJson term1
        pairs <- CM.mapM encodePair $ M.toList ann
        return $ Json.ValueObject $ M.fromList $ [
          ("term", json),
          ("annotations", Json.ValueObject $ M.fromList pairs)]
      where
        encodePair (Core.Name k, v) = do
          json <- untypedTermToJson v
          return (k, json)
    Core.TermApplication (Core.Application lhs rhs) -> asRecord [
      Core.Field (Core.Name "function") lhs,
      Core.Field (Core.Name "argument") rhs]
    Core.TermFunction f -> case f of
      Core.FunctionElimination elm -> case elm of
        Core.EliminationRecord (Core.Projection _ fname) -> asVariant "project" $ Core.TermVariable fname
        _ -> unexp $ "unexpected elimination variant: " ++ show (Variants.eliminationVariant elm)
      Core.FunctionLambda (Core.Lambda v d body) -> asRecord [
        Core.Field (Core.Name "parameter") $ Core.TermVariable v,
        Core.Field (Core.Name "domain") $ Core.TermOptional (EncodeCore.type_ <$> d),
        Core.Field (Core.Name "body") body]
      Core.FunctionPrimitive name -> Monads.pure $ Json.ValueString $ Core.unName name
    Core.TermLet (Core.Let bindings env) -> asRecord [
        Core.Field (Core.Name "bindings") $ Core.TermRecord $ Core.Record (Core.Name "") (fromBinding <$> bindings),
        Core.Field (Core.Name "environment") env]
      where
        fromBinding (Core.LetBinding k v _) = Core.Field k v
    Core.TermList terms -> Json.ValueArray <$> (CM.mapM untypedTermToJson terms)
    Core.TermLiteral lit -> Monads.pure $ case lit of
      Core.LiteralBinary s -> Json.ValueString s
      Core.LiteralBoolean b -> Json.ValueBoolean b
      Core.LiteralFloat f -> Json.ValueNumber $ HydraLiterals.floatValueToBigfloat f
      Core.LiteralInteger i -> Json.ValueNumber $ Literals.bigintToBigfloat $ HydraLiterals.integerValueToBigint i
      Core.LiteralString s -> Json.ValueString s
    Core.TermOptional mt -> case mt of
      Nothing -> Monads.pure Json.ValueNull
      Just t -> untypedTermToJson t
    Core.TermProduct els -> untypedTermToJson $ Core.TermList els
    Core.TermRecord (Core.Record _ fields) -> do
      keyvals <- CM.mapM fieldToKeyval fields
      return $ Json.ValueObject $ M.fromList $ Y.catMaybes keyvals
    Core.TermSet vals -> untypedTermToJson $ Core.TermList $ S.toList vals
    Core.TermSum (Core.Sum idx size term1) -> asRecord [
      Core.Field (Core.Name "index") $ Core.TermLiteral $ Core.LiteralInteger $ Core.IntegerValueInt32 idx,
      Core.Field (Core.Name "size") $ Core.TermLiteral $ Core.LiteralInteger $ Core.IntegerValueInt32 size,
      Core.Field (Core.Name "term") term1]
    Core.TermTypeAbstraction (Core.TypeAbstraction v term) -> asRecord [
      Core.Field (Core.Name "parameter") $ Core.TermVariable v,
      Core.Field (Core.Name "body") term]
    Core.TermTypeApplication (Core.TypedTerm term1 typ) -> asRecord [ -- Note: TermTypeApplication and TermTyped appear identical
      Core.Field (Core.Name "term") term1,
      Core.Field (Core.Name "type") $ EncodeCore.type_ typ]
    Core.TermUnion (Core.Injection _ field) -> if Core.fieldTerm field == Terms.unit
      then return $ Json.ValueString $ Core.unName $ Core.fieldName field
      else do
        mkeyval <- fieldToKeyval field
        return $ Json.ValueObject $ M.fromList $ case mkeyval of
          Nothing -> []
          Just keyval -> [keyval]
    Core.TermVariable v -> Monads.pure $ Json.ValueString $ Core.unName v
    Core.TermWrap (Core.WrappedTerm _ t) -> untypedTermToJson t
    t -> unexp $ "unsupported term variant: " ++ show (Variants.termVariant t)
--     t -> fail $ "unexpected term variant: " ++ show (termVariant t)
  where
    unexp msg = Monads.pure $ Json.ValueString $ "FAIL: " ++ msg
    asRecord = untypedTermToJson . Core.TermRecord . Core.Record (Core.Name "")
    asVariant name term = untypedTermToJson $ Core.TermUnion $ Core.Injection (Core.Name "") $ Core.Field (Core.Name name) term
    fieldToKeyval f = do
        mjson <- forTerm $ Core.fieldTerm f
        return $ case mjson of
          Nothing -> Nothing
          Just j -> Just (Core.unName $ Core.fieldName f, j)
      where
        forTerm t = case t of
          Core.TermOptional mt -> case mt of
            Nothing -> Monads.pure Nothing
            Just t' -> forTerm t'
          t' -> Just <$> untypedTermToJson t'