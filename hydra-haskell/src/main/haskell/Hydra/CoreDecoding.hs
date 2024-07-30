-- | Decoding of encoded types (as terms) back to types according to LambdaGraph's epsilon encoding

module Hydra.CoreDecoding (
  coreDecodeFieldType,
  coreDecodeFieldTypes,
  coreDecodeFloatType,
  coreDecodeFunctionType,
  coreDecodeIntegerType,
  coreDecodeLambdaType,
  coreDecodeLiteralType,
  coreDecodeMapType,
  coreDecodeName,
  coreDecodeRowType,
  coreDecodeString,
  coreDecodeType,
  dereferenceType,
  elementAsTypedTerm,
  fieldTypes,
  fullyStripTerm,
  isSerializable,
  moduleDependencyNamespaces,
  requireRecordType,
  requireType,
  requireUnionType,
  requireWrappedType,
  resolveType,
  typeDependencies,
  typeDependencyNames,
  ) where

import Hydra.Basics
import Hydra.Strip
import Hydra.Coders
import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import Hydra.Mantle
import Hydra.Module
import Hydra.Lexical
import Hydra.Rewriting
import Hydra.Tier1
import Hydra.Tier2
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


coreDecodeApplicationType :: Term -> Flow Graph (ApplicationType)
coreDecodeApplicationType = matchRecord $ \m -> ApplicationType
  <$> getField m _ApplicationType_function coreDecodeType
  <*> getField m _ApplicationType_argument coreDecodeType

coreDecodeFieldType :: Term -> Flow Graph (FieldType)
coreDecodeFieldType = matchRecord $ \m -> FieldType
  <$> getField m _FieldType_name coreDecodeName
  <*> getField m _FieldType_type coreDecodeType

coreDecodeFieldTypes :: Term -> Flow Graph [FieldType]
coreDecodeFieldTypes term = case fullyStripTerm term of
  TermList els -> CM.mapM coreDecodeFieldType els
  _ -> unexpected "list" $ show term

coreDecodeFloatType :: Term -> Flow Graph FloatType
coreDecodeFloatType = matchEnum _FloatType [
  (_FloatType_bigfloat, FloatTypeBigfloat),
  (_FloatType_float32, FloatTypeFloat32),
  (_FloatType_float64, FloatTypeFloat64)]

coreDecodeFunctionType :: Term -> Flow Graph (FunctionType)
coreDecodeFunctionType = matchRecord $ \m -> FunctionType
  <$> getField m _FunctionType_domain coreDecodeType
  <*> getField m _FunctionType_codomain coreDecodeType

coreDecodeIntegerType :: Term -> Flow Graph IntegerType
coreDecodeIntegerType = matchEnum _IntegerType [
  (_IntegerType_bigint, IntegerTypeBigint),
  (_IntegerType_int8, IntegerTypeInt8),
  (_IntegerType_int16, IntegerTypeInt16),
  (_IntegerType_int32, IntegerTypeInt32),
  (_IntegerType_int64, IntegerTypeInt64),
  (_IntegerType_uint8, IntegerTypeUint8),
  (_IntegerType_uint16, IntegerTypeUint16),
  (_IntegerType_uint32, IntegerTypeUint32),
  (_IntegerType_uint64, IntegerTypeUint64)]

coreDecodeLambdaType :: Term -> Flow Graph (LambdaType)
coreDecodeLambdaType = matchRecord $ \m -> LambdaType
  <$> (getField m _LambdaType_parameter coreDecodeName)
  <*> getField m _LambdaType_body coreDecodeType

coreDecodeLiteralType :: Term -> Flow Graph LiteralType
coreDecodeLiteralType = matchUnion _LiteralType [
  matchUnitField _LiteralType_binary LiteralTypeBinary,
  matchUnitField _LiteralType_boolean LiteralTypeBoolean,
  (_LiteralType_float, fmap LiteralTypeFloat . coreDecodeFloatType),
  (_LiteralType_integer, fmap LiteralTypeInteger . coreDecodeIntegerType),
  matchUnitField _LiteralType_string LiteralTypeString]

coreDecodeMapType :: Term -> Flow Graph (MapType)
coreDecodeMapType = matchRecord $ \m -> MapType
  <$> getField m _MapType_keys coreDecodeType
  <*> getField m _MapType_values coreDecodeType

coreDecodeName :: Term -> Flow Graph Name
coreDecodeName term = Name <$> (Expect.wrap _Name term >>= Expect.string)

coreDecodeWrappedType :: Term -> Flow Graph WrappedType
coreDecodeWrappedType term = do
  fields <- Expect.recordWithName _WrappedType term
  name <- Expect.field _WrappedType_typeName coreDecodeName fields
  obj <- Expect.field _WrappedType_object coreDecodeType fields
  pure $ WrappedType name obj

coreDecodeRowType :: Term -> Flow Graph (RowType)
coreDecodeRowType = matchRecord $ \m -> RowType
  <$> getField m _RowType_typeName coreDecodeName
  <*> getField m _RowType_extends (Expect.optional coreDecodeName)
  <*> getField m _RowType_fields coreDecodeFieldTypes

coreDecodeString :: Term -> Flow Graph String
coreDecodeString = Expect.string . fullyStripTerm

coreDecodeType :: Term -> Flow Graph Type
coreDecodeType dat = case dat of
  TermAnnotated (AnnotatedTerm term ann) -> (\t -> TypeAnnotated $ AnnotatedType t ann) <$> coreDecodeType term
  _ -> matchUnion _Type [
--    (_Type_annotated, fmap TypeAnnotated . coreDecodeAnnotated),
    (_Type_application, fmap TypeApplication . coreDecodeApplicationType),
    (_Type_function, fmap TypeFunction . coreDecodeFunctionType),
    (_Type_lambda, fmap TypeLambda . coreDecodeLambdaType),
    (_Type_list, fmap TypeList . coreDecodeType),
    (_Type_literal, fmap TypeLiteral . coreDecodeLiteralType),
    (_Type_map, fmap TypeMap . coreDecodeMapType),
    (_Type_optional, fmap TypeOptional . coreDecodeType),
    (_Type_product, \l -> do
      types <- Expect.list pure l
      TypeProduct <$> (CM.mapM coreDecodeType types)),
    (_Type_record, fmap TypeRecord . coreDecodeRowType),
    (_Type_set, fmap TypeSet . coreDecodeType),
    (_Type_sum, \(TermList types) -> TypeSum <$> (CM.mapM coreDecodeType types)),
    (_Type_union, fmap TypeUnion . coreDecodeRowType),
    (_Type_variable, fmap TypeVariable . coreDecodeName),
    (_Type_wrap, fmap TypeWrap . (coreDecodeWrappedType))] dat

dereferenceType :: Name -> Flow Graph (Maybe Type)
dereferenceType name = do
  mel <- dereferenceElement name
  case mel of
    Nothing -> return Nothing
    Just el -> Just <$> coreDecodeType (elementData el)

elementAsTypedTerm :: Element -> Flow Graph TypedTerm
elementAsTypedTerm el = do
  typ <- requireTermType $ elementData el
  return $ TypedTerm (elementData el) typ

fieldTypes :: Type -> Flow Graph (M.Map Name Type)
fieldTypes t = case stripType t of
    TypeLambda (LambdaType _ body) -> fieldTypes body
    TypeRecord rt -> pure $ toMap $ rowTypeFields rt
    TypeUnion rt -> pure $ toMap $ rowTypeFields rt
    TypeVariable name -> do
      withTrace ("field types of " ++ unName name) $ do
        el <- requireElement name
        coreDecodeType (elementData el) >>= fieldTypes
    _ -> unexpected "record or union type" $ show t
  where
    toMap fields = M.fromList (toPair <$> fields)
    toPair (FieldType fname ftype) = (fname, ftype)

getField :: M.Map Name (Term) -> Name -> (Term -> Flow Graph b) -> Flow Graph b
getField m fname decode = case M.lookup fname m of
  Nothing -> fail $ "expected field " ++ show fname ++ " not found"
  Just val -> decode val

isSerializable :: Element -> Flow Graph Bool
isSerializable el = do
    deps <- typeDependencies (elementName el)
    let allVariants = S.fromList $ L.concat (variants <$> M.elems deps)
    return $ not $ S.member TypeVariantFunction allVariants
  where
    variants typ = typeVariant <$> foldOverType TraversalOrderPre (\m t -> t:m) [] typ

matchEnum :: Name -> [(Name, b)] -> Term -> Flow Graph b
matchEnum tname = matchUnion tname . fmap (uncurry matchUnitField)

matchRecord :: (M.Map Name (Term) -> Flow Graph b) -> Term -> Flow Graph b
matchRecord decode term = case fullyStripTerm term of
  TermRecord (Record _ fields) -> decode $ M.fromList $ fmap (\(Field fname val) -> (fname, val)) fields
  _ -> unexpected "record" $ show term

matchUnion :: Name -> [(Name, Term -> Flow Graph b)] -> Term -> Flow Graph b
matchUnion tname pairs term = case fullyStripTerm term of
    TermVariable name -> do
      el <- requireElement name
      matchUnion tname pairs (elementData el)
    TermUnion (Injection tname' (Field fname val)) -> if tname' == tname
      then case M.lookup fname mapping of
        Nothing -> fail $ "no matching case for field " ++ show fname
        Just f -> f val
      else unexpected ("injection for type " ++ show tname) $ show term
    t -> unexpected ("union with one of {" ++ L.intercalate ", " (unName . fst <$> pairs) ++ "}") $ show t
  where
    mapping = M.fromList pairs

matchUnitField :: Name -> y -> (Name, x -> Flow Graph y)
matchUnitField fname x = (fname, \_ -> pure x)

-- | Find dependency namespaces in various dimensions of a term: va
moduleDependencyNamespaces :: Bool -> Bool -> Bool -> Bool -> Module -> Flow Graph (S.Set Namespace)
moduleDependencyNamespaces withVars withPrims withNoms withSchema mod = do
    allNames <- S.unions <$> (CM.mapM elNames $ moduleElements mod)
    let namespaces = S.fromList $ Y.catMaybes (namespaceOfEager <$> S.toList allNames)
    return $ S.delete (moduleNamespace mod) namespaces
  where
    elNames el = do
      let term = elementData el
      let dataNames = termDependencyNames withVars withPrims withNoms term

      schemaNames <- if withSchema
        then typeDependencyNames <$> requireTermType term
        else pure S.empty

      typeNames <- if isEncodedType (fullyStripTerm term)
        then typeDependencyNames <$> coreDecodeType term
        else pure S.empty

      return $ S.unions [dataNames, schemaNames, typeNames]

requireRecordType :: Bool -> Name -> Flow Graph (RowType)
requireRecordType infer = requireRowType "record type" infer $ \t -> case t of
  TypeRecord rt -> Just rt
  _ -> Nothing

requireRowType :: String -> Bool -> (Type -> Maybe (RowType)) -> Name -> Flow Graph (RowType)
requireRowType label infer getter name = do
  t <- requireType name
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
      TypeAnnotated (AnnotatedType t' _) -> rawType t'
      TypeLambda (LambdaType _ body) -> rawType body -- Note: throwing away quantification here
      _ -> t

requireType :: Name -> Flow Graph Type
requireType name = withTrace ("require type " ++ unName name) $
  (withSchemaContext $ requireElement name) >>= (coreDecodeType . elementData)

requireUnionType :: Bool -> Name -> Flow Graph (RowType)
requireUnionType infer = requireRowType "union" infer $ \t -> case t of
  TypeUnion rt -> Just rt
  _ -> Nothing

requireWrappedType :: Name -> Flow Graph Type
requireWrappedType name = do
  typ <- requireType name
  case stripType typ of
    TypeWrap (WrappedType name t) -> return t
    _ -> return typ -- TODO: stop allowing this "slop" once typedefs are clearly separated from newtypes
--     _ -> fail $ "expected wrapped type for " ++ unName name ++ " but got " ++ show typ

resolveType :: Type -> Flow Graph (Maybe Type)
resolveType typ = case stripType typ of
    TypeVariable name -> withSchemaContext $ do
      mterm <- resolveTerm name
      case mterm of
        Nothing -> pure Nothing
        Just t -> Just <$> coreDecodeType t
    _ -> pure $ Just typ

typeDependencies :: Name -> Flow Graph (M.Map Name Type)
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
        coreDecodeType (elementData el)
