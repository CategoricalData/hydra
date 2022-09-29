module Hydra.Ext.Yaml.Coder (yamlCoder) where

import Hydra.Core
import Hydra.Compute
import Hydra.Adapter
import Hydra.Adapters.Term
import Hydra.CoreLanguage
import Hydra.Monads
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Ext.Yaml.Language
import qualified Hydra.Ext.Yaml.Model as YM
import Hydra.Lexical
import Hydra.Adapters.UtilsEtc

import qualified Control.Monad as CM
import qualified Data.Map as M
import qualified Data.Maybe as Y


literalCoder :: LiteralType -> GraphFlow m (Coder (Context m) Literal YM.Scalar)
literalCoder at = pure $ case at of
  LiteralTypeBoolean -> Coder {
    coderEncode = \(LiteralBoolean b) -> pure $ YM.ScalarBool b,
    coderDecode = \s -> case s of
      YM.ScalarBool b -> pure $ LiteralBoolean b
      _ -> unexpected "boolean" s}
  LiteralTypeFloat _ -> Coder {
    coderEncode = \(LiteralFloat (FloatValueBigfloat f)) -> pure $ YM.ScalarFloat f,
    coderDecode = \s -> case s of
      YM.ScalarFloat f -> pure $ LiteralFloat $ FloatValueBigfloat f
      _ -> unexpected "floating-point value" s}
  LiteralTypeInteger _ -> Coder {
    coderEncode = \(LiteralInteger (IntegerValueBigint i)) -> pure $ YM.ScalarInt i,
    coderDecode = \s -> case s of
      YM.ScalarInt i -> pure $ LiteralInteger $ IntegerValueBigint i
      _ -> unexpected "integer" s}
  LiteralTypeString -> Coder {
    coderEncode = \(LiteralString s) -> pure $ YM.ScalarStr s,
    coderDecode = \s -> case s of
      YM.ScalarStr s' -> pure $ LiteralString s'
      _ -> unexpected "string" s}

recordCoder :: (Eq m, Ord m, Read m, Show m) => RowType m -> GraphFlow m (Coder (Context m) (Term m) YM.Node)
recordCoder rt = do
    coders <- CM.mapM (\f -> (,) <$> pure f <*> termCoder (fieldTypeType f)) (rowTypeFields rt)
    return $ Coder (encode coders) (decode coders)
  where
    encode coders term = case stripTerm term of
      TermRecord (Record _ fields) -> YM.NodeMapping . M.fromList . Y.catMaybes <$> CM.zipWithM encodeField coders fields
        where
          encodeField (ft, coder) (Field (FieldName fn) fv) = case (fieldTypeType ft, fv) of
            (TypeOptional _, TermOptional Nothing) -> pure Nothing
            _ -> Just <$> ((,) <$> pure (yamlString fn) <*> coderEncode coder fv)
      _ -> unexpected "record" term
    decode coders n = case n of
      YM.NodeMapping m -> Terms.record (rowTypeTypeName rt) <$>
          CM.mapM (decodeField m) coders -- Note: unknown fields are ignored
        where
          decodeField m (FieldType fname@(FieldName fn) ft, coder) = do
            v <- coderDecode coder $ Y.fromMaybe yamlNull $ M.lookup (yamlString fn) m
            return $ Field fname v
      _ -> unexpected "mapping" n
    getCoder coders fname = Y.maybe error pure $ M.lookup fname coders
      where
        error = fail $ "no such field: " ++ fname

termCoder :: (Eq m, Ord m, Read m, Show m) => Type m -> GraphFlow m (Coder (Context m) (Term m) YM.Node)
termCoder typ = case stripType typ of
  TypeLiteral at -> do
    ac <- literalCoder at
    return Coder {
      coderEncode = \t -> case t of
         TermLiteral av -> YM.NodeScalar <$> coderEncode ac av
         _ -> unexpected "literal" t,
      coderDecode = \n -> case n of
        YM.NodeScalar s -> Terms.literal <$> coderDecode ac s
        _ -> unexpected "scalar node" n}
  TypeList lt -> do
    lc <- termCoder lt
    return Coder {
--      coderEncode = \(Term (TermList els) _) -> YM.NodeSequence <$> CM.mapM (coderEncode lc) els,
      coderEncode = \t -> case t of
         TermList els -> YM.NodeSequence <$> CM.mapM (coderEncode lc) els
         _ -> unexpected "list" t,
      coderDecode = \n -> case n of
        YM.NodeSequence nodes -> Terms.list <$> CM.mapM (coderDecode lc) nodes
        _ -> unexpected "sequence" n}
  TypeOptional ot -> do
    oc <- termCoder ot
    return Coder {
      coderEncode = \t -> case t of
         TermOptional el -> Y.maybe (pure yamlNull) (coderEncode oc) el
         _ -> unexpected "optional" t,
      coderDecode = \n -> case n of
        YM.NodeScalar YM.ScalarNull -> pure $ Terms.optional Nothing
        _ -> Terms.optional . Just <$> coderDecode oc n}
  TypeMap (MapType kt vt) -> do
    kc <- termCoder kt
    vc <- termCoder vt
    let encodeEntry (k, v) = (,) <$> coderEncode kc k <*> coderEncode vc v
    let decodeEntry (k, v) = (,) <$> coderDecode kc k <*> coderDecode vc v
    return Coder {
      coderEncode = \t -> case t of
        TermMap m -> YM.NodeMapping . M.fromList <$> CM.mapM encodeEntry (M.toList m)
        _ -> unexpected "term" t,
      coderDecode = \n -> case n of
        YM.NodeMapping m -> Terms.map . M.fromList <$> CM.mapM decodeEntry (M.toList m)
        _ -> unexpected "mapping" n}
  TypeRecord rt -> recordCoder rt

yamlCoder :: (Eq m, Ord m, Read m, Show m) => Type m -> GraphFlow m (Coder (Context m) (Term m) YM.Node)
yamlCoder typ = do
    cx <- getState
    let acx = AdapterContext cx hydraCoreLanguage (language cx)
    adapter <- withState acx $ termAdapter typ
    coder <- termCoder $ adapterTarget adapter
    return $ composeCoders (adapterCoder adapter) coder
  where

yamlNull :: YM.Node
yamlNull = YM.NodeScalar YM.ScalarNull

yamlString :: String -> YM.Node
yamlString = YM.NodeScalar . YM.ScalarStr
