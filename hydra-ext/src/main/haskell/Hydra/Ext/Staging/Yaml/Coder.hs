-- | Bidirectional coder between Hydra terms and YAML nodes

module Hydra.Ext.Staging.Yaml.Coder (yamlCoder) where

import Hydra.Kernel
import Hydra.Adapt.Modules (languageAdapter)
import Hydra.Ext.Staging.Yaml.Language
import Hydra.Adapt.Utils
import qualified Hydra.Ext.Org.Yaml.Model as YM
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.Map as M
import qualified Data.Maybe as Y


literalCoder :: LiteralType -> Coder Literal YM.Scalar
literalCoder at = case at of
  LiteralTypeBoolean -> Coder {
    coderEncode = \_ (LiteralBoolean b) -> Right $ YM.ScalarBool b,
    coderDecode = \cx s -> case s of
      YM.ScalarBool b -> Right $ LiteralBoolean b
      _ -> unexpectedE cx "boolean" $ show s}
  LiteralTypeFloat _ -> Coder {
    coderEncode = \_ (LiteralFloat (FloatValueBigfloat f)) -> Right $ YM.ScalarFloat f,
    coderDecode = \cx s -> case s of
      YM.ScalarFloat f -> Right $ LiteralFloat $ FloatValueBigfloat f
      _ -> unexpectedE cx "floating-point value" $ show s}
  LiteralTypeInteger _ -> Coder {
    coderEncode = \_ (LiteralInteger (IntegerValueBigint i)) -> Right $ YM.ScalarInt i,
    coderDecode = \cx s -> case s of
      YM.ScalarInt i -> Right $ LiteralInteger $ IntegerValueBigint i
      _ -> unexpectedE cx "integer" $ show s}
  LiteralTypeString -> Coder {
    coderEncode = \_ (LiteralString s) -> Right $ YM.ScalarStr s,
    coderDecode = \cx s -> case s of
      YM.ScalarStr s' -> Right $ LiteralString s'
      _ -> unexpectedE cx "string" $ show s}
  where
    unexpectedE cx expected actual = Left (InContext (OtherError ("expected " ++ expected ++ ", found " ++ actual)) cx)

recordCoder :: RowType -> Coder Term YM.Node
recordCoder rt = Coder encode decode
  where
    encode cx term = case deannotateTerm term of
      TermRecord (Record _ fields) ->
        YM.NodeMapping . M.fromList . Y.catMaybes <$> zipWithM' (encodeField cx) (rowTypeFields rt) fields
      _ -> Left $ InContext (OtherError ("expected record, found " ++ show term)) cx
    decode cx n = case n of
      YM.NodeMapping m ->
        Terms.record (rowTypeTypeName rt) <$> mapM' (decodeField cx m) (rowTypeFields rt)
      _ -> Left $ InContext (OtherError ("expected mapping, found " ++ show n)) cx

    encodeField cx ft (Field (Name fn) fv) = case (fieldTypeType ft, fv) of
      (TypeMaybe _, TermMaybe Nothing) -> Right Nothing
      _ -> do
        let coder = termCoder (fieldTypeType ft)
        node <- coderEncode coder cx fv
        Right $ Just (yamlString fn, node)

    decodeField cx m (FieldType fname@(Name fn) ft) = do
      let coder = termCoder ft
      v <- coderDecode coder cx $ Y.fromMaybe yamlNull $ M.lookup (yamlString fn) m
      Right $ Field fname v

termCoder :: Type -> Coder Term YM.Node
termCoder typ = case deannotateType typ of
  TypeLiteral at -> Coder encode decode
    where
      ac = literalCoder at
      encode cx t = case t of
        TermLiteral av -> YM.NodeScalar <$> coderEncode ac cx av
        _ -> Left $ InContext (OtherError ("expected literal, found " ++ show t)) cx
      decode cx n = case n of
        YM.NodeScalar s -> Terms.literal <$> coderDecode ac cx s
        _ -> Left $ InContext (OtherError ("expected scalar node, found " ++ show n)) cx
  TypeList lt -> Coder encode decode
    where
      lc = termCoder lt
      encode cx t = case t of
        TermList els -> YM.NodeSequence <$> mapM' (\e -> coderEncode lc cx e) els
        _ -> Left $ InContext (OtherError ("expected list, found " ++ show t)) cx
      decode cx n = case n of
        YM.NodeSequence nodes -> Terms.list <$> mapM' (\nd -> coderDecode lc cx nd) nodes
        _ -> Left $ InContext (OtherError ("expected sequence, found " ++ show n)) cx
  TypeMaybe ot -> Coder encode decode
    where
      oc = termCoder ot
      encode cx t = case t of
        TermMaybe el -> case el of
          Nothing -> Right yamlNull
          Just v -> coderEncode oc cx v
        _ -> Left $ InContext (OtherError ("expected maybe, found " ++ show t)) cx
      decode cx n = case n of
        YM.NodeScalar YM.ScalarNull -> Right $ Terms.optional Nothing
        _ -> Terms.optional . Just <$> coderDecode oc cx n
  TypeMap (MapType kt vt) -> Coder encode decode
    where
      kc = termCoder kt
      vc = termCoder vt
      encode cx t = case t of
        TermMap m -> YM.NodeMapping . M.fromList <$> mapM' (encodeEntry cx) (M.toList m)
        _ -> Left $ InContext (OtherError ("expected map, found " ++ show t)) cx
      decode cx n = case n of
        YM.NodeMapping m -> Terms.map . M.fromList <$> mapM' (decodeEntry cx) (M.toList m)
        _ -> Left $ InContext (OtherError ("expected mapping, found " ++ show n)) cx
      encodeEntry cx (k, v) = do
        k' <- coderEncode kc cx k
        v' <- coderEncode vc cx v
        Right (k', v')
      decodeEntry cx (k, v) = do
        k' <- coderDecode kc cx k
        v' <- coderDecode vc cx v
        Right (k', v')
  TypeRecord rt -> recordCoder rt
  TypeUnit -> unitCoder
  _ -> Coder
    (\cx _ -> Left $ InContext (OtherError ("unsupported type variant: " ++ show (typeVariant typ))) cx)
    (\cx _ -> Left $ InContext (OtherError ("unsupported type variant: " ++ show (typeVariant typ))) cx)

unitCoder :: Coder Term YM.Node
unitCoder = Coder encode decode
  where
    encode cx term = case deannotateTerm term of
      TermUnit -> Right $ YM.NodeScalar $ YM.ScalarNull
      _ -> Left $ InContext (OtherError ("expected unit, found " ++ show term)) cx
    decode cx n = case n of
      (YM.NodeScalar YM.ScalarNull) -> Right Terms.unit
      _ -> Left $ InContext (OtherError ("expected null, found " ++ show n)) cx

yamlCoder :: Context -> Graph -> Type -> Either (InContext OtherError) (Coder Term YM.Node)
yamlCoder cx g typ = do
  adapter <- case languageAdapter yamlLanguage cx g typ of
    Left err -> Left $ InContext (OtherError err) cx
    Right a -> Right a
  let coder = termCoder $ adapterTarget adapter
  Right $ composeCoders (adapterCoder adapter) coder

yamlNull :: YM.Node
yamlNull = YM.NodeScalar YM.ScalarNull

yamlString :: String -> YM.Node
yamlString = YM.NodeScalar . YM.ScalarStr

-- | Either-based mapM
mapM' :: (a -> Either e b) -> [a] -> Either e [b]
mapM' _ [] = Right []
mapM' f (x:xs) = do
  y <- f x
  ys <- mapM' f xs
  Right (y:ys)

-- | Either-based zipWithM
zipWithM' :: (a -> b -> Either e c) -> [a] -> [b] -> Either e [c]
zipWithM' _ [] _ = Right []
zipWithM' _ _ [] = Right []
zipWithM' f (a:as) (b:bs) = do
  c <- f a b
  cs <- zipWithM' f as bs
  Right (c:cs)
