-- | Decoding of encoded types (as terms) back to types according to the epsilon encoding

module Hydra.CoreDecoding (
  epsilonDecodeLiteralType,
  epsilonDecodeFieldType,
  epsilonDecodeFieldTypes,
  epsilonDecodeFloatType,
  epsilonDecodeFunctionType,
  epsilonDecodeIntegerType,
  epsilonDecodeMapType,
  epsilonDecodeRowType,
  epsilonDecodeString,
  epsilonDecodeType,
  epsilonDecodeLambdaType,
  elementAsTypedTerm,
  fieldTypes,
  requireRecordType,
  requireType,
  requireUnionType,
  requireWrappedType,
  resolveType,
  typeDependencies,
  typeDependencyNames,
  ) where

import Hydra.Coders
import Hydra.Common
import Hydra.Core
import Hydra.Graph
import Hydra.Mantle
import Hydra.Lexical
import Hydra.Flows
import Hydra.Rewriting
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


epsilonDecodeApplicationType :: Show a => Term a -> GraphFlow a (ApplicationType a)
epsilonDecodeApplicationType = matchRecord $ \m -> ApplicationType
  <$> getField m _ApplicationType_function epsilonDecodeType
  <*> getField m _ApplicationType_argument epsilonDecodeType

epsilonDecodeFieldType :: Show a => Term a -> GraphFlow a (FieldType a)
epsilonDecodeFieldType = matchRecord $ \m -> FieldType
  <$> (FieldName <$> getField m _FieldType_name epsilonDecodeString)
  <*> getField m _FieldType_type epsilonDecodeType

epsilonDecodeFieldTypes :: Show a => Term a -> GraphFlow a [FieldType a]
epsilonDecodeFieldTypes term = case stripTerm term of
  TermList els -> CM.mapM epsilonDecodeFieldType els
  _ -> unexpected "list" term

epsilonDecodeFloatType :: Show a => Term a -> GraphFlow a FloatType
epsilonDecodeFloatType = matchEnum [
  (_FloatType_bigfloat, FloatTypeBigfloat),
  (_FloatType_float32, FloatTypeFloat32),
  (_FloatType_float64, FloatTypeFloat64)]

epsilonDecodeFunctionType :: Show a => Term a -> GraphFlow a (FunctionType a)
epsilonDecodeFunctionType = matchRecord $ \m -> FunctionType
  <$> getField m _FunctionType_domain epsilonDecodeType
  <*> getField m _FunctionType_codomain epsilonDecodeType

epsilonDecodeIntegerType :: Show a => Term a -> GraphFlow a IntegerType
epsilonDecodeIntegerType = matchEnum [
  (_IntegerType_bigint, IntegerTypeBigint),
  (_IntegerType_int8, IntegerTypeInt8),
  (_IntegerType_int16, IntegerTypeInt16),
  (_IntegerType_int32, IntegerTypeInt32),
  (_IntegerType_int64, IntegerTypeInt64),
  (_IntegerType_uint8, IntegerTypeUint8),
  (_IntegerType_uint16, IntegerTypeUint16),
  (_IntegerType_uint32, IntegerTypeUint32),
  (_IntegerType_uint64, IntegerTypeUint64)]

epsilonDecodeLambdaType :: Show a => Term a -> GraphFlow a (LambdaType a)
epsilonDecodeLambdaType = matchRecord $ \m -> LambdaType
  <$> (Name <$> getField m _LambdaType_parameter epsilonDecodeString)
  <*> getField m _LambdaType_body epsilonDecodeType

epsilonDecodeLiteralType :: Show a => Term a -> GraphFlow a LiteralType
epsilonDecodeLiteralType = matchUnion [
  matchUnitField _LiteralType_binary LiteralTypeBinary,
  matchUnitField _LiteralType_boolean LiteralTypeBoolean,
  (_LiteralType_float, fmap LiteralTypeFloat . epsilonDecodeFloatType),
  (_LiteralType_integer, fmap LiteralTypeInteger . epsilonDecodeIntegerType),
  matchUnitField _LiteralType_string LiteralTypeString]

epsilonDecodeMapType :: Show a => Term a -> GraphFlow a (MapType a)
epsilonDecodeMapType = matchRecord $ \m -> MapType
  <$> getField m _MapType_keys epsilonDecodeType
  <*> getField m _MapType_values epsilonDecodeType

epsilonDecodeRowType :: Show a => Term a -> GraphFlow a (RowType a)
epsilonDecodeRowType = matchRecord $ \m -> RowType
  <$> (Name <$> getField m _RowType_typeName epsilonDecodeString)
  <*> getField m _RowType_extends (Expect.optional (\term -> Name <$> Expect.string term))
  <*> getField m _RowType_fields epsilonDecodeFieldTypes

epsilonDecodeString :: Show a => Term a -> GraphFlow a String
epsilonDecodeString = Expect.string . stripTerm

epsilonDecodeType :: Show a => Term a -> GraphFlow a (Type a)
epsilonDecodeType dat = case dat of
  TermAnnotated (Annotated term ann) -> (\t -> TypeAnnotated $ Annotated t ann) <$> epsilonDecodeType term
  _ -> matchUnion [
--    (_Type_annotated, fmap TypeAnnotated . epsilonDecodeAnnotated),
    (_Type_application, fmap TypeApplication . epsilonDecodeApplicationType),
    (_Type_function, fmap TypeFunction . epsilonDecodeFunctionType),
    (_Type_lambda, fmap TypeLambda . epsilonDecodeLambdaType),
    (_Type_list, fmap TypeList . epsilonDecodeType),
    (_Type_literal, fmap TypeLiteral . epsilonDecodeLiteralType),
    (_Type_map, fmap TypeMap . epsilonDecodeMapType),
    (_Type_optional, fmap TypeOptional . epsilonDecodeType),
    (_Type_product, \l -> do
      types <- Expect.list pure l
      TypeProduct <$> (CM.mapM epsilonDecodeType types)),
    (_Type_record, fmap TypeRecord . epsilonDecodeRowType),
    (_Type_set, fmap TypeSet . epsilonDecodeType),
    (_Type_sum, \(TermList types) -> TypeSum <$> (CM.mapM epsilonDecodeType types)),
    (_Type_union, fmap TypeUnion . epsilonDecodeRowType),
    (_Type_variable, fmap (TypeVariable . Name) . epsilonDecodeString),
    (_Type_wrap, fmap TypeWrap . Expect.variable)] dat

elementAsTypedTerm :: (Show a) => Element a -> GraphFlow a (TypedTerm a)
elementAsTypedTerm el = do
  typ <- requireTypeAnnotation (elementData el)
  return $ TypedTerm typ (elementData el)

fieldTypes :: Show a => Type a -> GraphFlow a (M.Map FieldName (Type a))
fieldTypes t = case stripType t of
    TypeRecord rt -> pure $ toMap $ rowTypeFields rt
    TypeUnion rt -> pure $ toMap $ rowTypeFields rt
    TypeWrap name -> do
      withTrace ("field types of " ++ unName name) $ do
        el <- requireElement name
        epsilonDecodeType (elementData el) >>= fieldTypes
    TypeLambda (LambdaType _ body) -> fieldTypes body
    _ -> unexpected "record or union type" t
  where
    toMap fields = M.fromList (toPair <$> fields)
    toPair (FieldType fname ftype) = (fname, ftype)

getField :: M.Map FieldName (Term a) -> FieldName -> (Term a -> GraphFlow a b) -> GraphFlow a b
getField m fname decode = case M.lookup fname m of
  Nothing -> fail $ "expected field " ++ show fname ++ " not found"
  Just val -> decode val

matchEnum :: Show a => [(FieldName, b)] -> Term a -> GraphFlow a b
matchEnum = matchUnion . fmap (uncurry matchUnitField)

matchRecord :: Show a => (M.Map FieldName (Term a) -> GraphFlow a b) -> Term a -> GraphFlow a b
matchRecord decode term = do
  term1 <- deref term
  case stripTerm term1 of
    TermRecord (Record _ fields) -> decode $ M.fromList $ fmap (\(Field fname val) -> (fname, val)) fields
    _ -> unexpected "record" term1

matchUnion :: Show a => [(FieldName, Term a -> GraphFlow a b)] -> Term a -> GraphFlow a b
matchUnion pairs term = do
    term1 <- deref term
    case stripTerm term1 of
      TermVariable name -> do
        el <- requireElement name
        matchUnion pairs (elementData el)
      TermUnion (Injection _ (Field fname val)) -> case M.lookup fname mapping of
        Nothing -> fail $ "no matching case for field " ++ show fname
        Just f -> f val
      _ -> unexpected ("union with one of {" ++ L.intercalate ", " (unFieldName . fst <$> pairs) ++ "}") term
  where
    mapping = M.fromList pairs

matchUnitField :: FieldName -> y -> (FieldName, x -> GraphFlow a y)
matchUnitField fname x = (fname, \_ -> pure x)

requireRecordType :: Show a => Bool -> Name -> GraphFlow a (RowType a)
requireRecordType infer = requireRowType "record" infer $ \t -> case t of
  TypeRecord rt -> Just rt
  _ -> Nothing

requireRowType :: Show a => String -> Bool -> (Type a -> Maybe (RowType a)) -> Name -> GraphFlow a (RowType a)
requireRowType label infer getter name = do
  t <- withSchemaContext $ requireType name
  case getter (rawType t) of
    Just rt -> if infer
      then case rowTypeExtends rt of
        Nothing -> return rt
        Just name' -> do
          rt' <- requireRowType label True getter name'
          return $ RowType name Nothing (rowTypeFields rt' ++ rowTypeFields rt)
      else return rt
    Nothing -> fail $ show name ++ " does not resolve to a " ++ label ++ " type: " ++ show t
  where
    rawType t = case t of
      TypeAnnotated (Annotated t' _) -> rawType t'
      TypeLambda (LambdaType _ body) -> rawType body -- Note: throwing away quantification here
      _ -> t

-- TODO: this should operate on the primary graph, not the schema graph
requireType :: Show a => Name -> GraphFlow a (Type a)
requireType name = withTrace "require type" $ do
  el <- requireElement name
  epsilonDecodeType $ elementData el

requireUnionType :: Show a => Bool -> Name -> GraphFlow a (RowType a)
requireUnionType infer = requireRowType "union" infer $ \t -> case t of
  TypeUnion rt -> Just rt
  _ -> Nothing

requireWrappedType :: Show a => Name -> GraphFlow a (Type a)
requireWrappedType name = withSchemaContext $ requireType name

resolveType :: Show a => Type a -> GraphFlow a (Maybe (Type a))
resolveType typ = case stripType typ of
    TypeVariable name -> withSchemaContext $ do
      mterm <- resolveTerm name
      case mterm of
        Nothing -> pure Nothing
        Just t -> Just <$> epsilonDecodeType t
    _ -> pure $ Just typ

typeDependencies :: Show a => Name -> GraphFlow a (M.Map Name (Type a))
typeDependencies name = deps (S.fromList [name]) M.empty
  where
    deps seeds names = if S.null seeds
        then return names
        else do
          pairs <- CM.mapM toPair $ S.toList seeds
          let newNames = M.union names (M.fromList pairs)
          let refs = L.foldl S.union S.empty (typeDependencyNames <$> (snd <$> pairs))
          let visited = S.fromList $ M.keys names
          let newSeeds = S.difference refs visited
          deps newSeeds newNames
      where
        toPair name = do
          typ <- requireType name
          return (name, typ)

    requireType name = do
      withTrace ("type dependencies of " ++ unName name) $ do
        el <- requireElement name
        epsilonDecodeType (elementData el)

typeDependencyNames :: Type a -> S.Set Name
typeDependencyNames = foldOverType TraversalOrderPre addNames S.empty
  where
    addNames names typ = case typ of
      TypeWrap name -> S.insert name names
      _ -> names
