-- | Decoding of encoded types (as terms) back to types according to LambdaGraph's epsilon encoding

module Hydra.Staging.CoreDecoding (
  coreDecodeFieldType,
  coreDecodeFieldTypes,
  coreDecodeFloatType,
  coreDecodeFunctionType,
  coreDecodeIntegerType,
  coreDecodeForallType,
  coreDecodeLiteralType,
  coreDecodeMapType,
  coreDecodeName,
  coreDecodeRowType,
  coreDecodeString,
  coreDecodeType,
  coreDecodeTypeScheme,
  ) where

import Hydra.Strip
import Hydra.Coders
import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import Hydra.Mantle
import Hydra.Module
import Hydra.Lexical
import Hydra.Staging.Rewriting
import Hydra.Rewriting
import Hydra.Errors
import qualified Hydra.Lib.Io as Io
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
  _ -> unexpected "list" $ Io.showTerm term

coreDecodeFloatType :: Term -> Flow Graph FloatType
coreDecodeFloatType = matchEnum _FloatType [
  (_FloatType_bigfloat, FloatTypeBigfloat),
  (_FloatType_float32, FloatTypeFloat32),
  (_FloatType_float64, FloatTypeFloat64)]

coreDecodeForallType :: Term -> Flow Graph (ForallType)
coreDecodeForallType = matchRecord $ \m -> ForallType
  <$> (getField m _ForallType_parameter coreDecodeName)
  <*> getField m _ForallType_body coreDecodeType

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
  fields <- Expect.record _WrappedType term
  name <- Expect.field _WrappedType_typeName coreDecodeName fields
  obj <- Expect.field _WrappedType_object coreDecodeType fields
  pure $ WrappedType name obj

coreDecodeRowType :: Term -> Flow Graph RowType
coreDecodeRowType = matchRecord $ \m -> RowType
  <$> getField m _RowType_typeName coreDecodeName
  <*> getField m _RowType_fields coreDecodeFieldTypes

coreDecodeString :: Term -> Flow Graph String
coreDecodeString = Expect.string . fullyStripTerm

coreDecodeType :: Term -> Flow Graph Type
coreDecodeType dat = case dat of
  TermAnnotated (AnnotatedTerm term ann) -> (\t -> TypeAnnotated $ AnnotatedType t ann) <$> coreDecodeType term
  TermTyped (TypedTerm term _) -> coreDecodeType term
  _ -> matchUnion _Type [
--    (_Type_annotated, fmap TypeAnnotated . coreDecodeAnnotated),
    (_Type_application, fmap TypeApplication . coreDecodeApplicationType),
    (_Type_forall, fmap TypeForall . coreDecodeForallType),
    (_Type_function, fmap TypeFunction . coreDecodeFunctionType),
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

coreDecodeTypeScheme :: Term -> Flow Graph TypeScheme
coreDecodeTypeScheme = matchRecord $ \m -> TypeScheme
  <$> getField m _TypeScheme_variables (Expect.list coreDecodeName)
  <*> getField m _TypeScheme_type coreDecodeType

-- helpers ---------------------------------------------------------------------

getField :: M.Map Name (Term) -> Name -> (Term -> Flow Graph b) -> Flow Graph b
getField m fname decode = case M.lookup fname m of
  Nothing -> fail $ "expected field " ++ unName fname ++ " not found"
  Just val -> decode val

matchEnum :: Name -> [(Name, b)] -> Term -> Flow Graph b
matchEnum tname = matchUnion tname . fmap (uncurry matchUnitField)

matchRecord :: (M.Map Name (Term) -> Flow Graph b) -> Term -> Flow Graph b
matchRecord decode term = case fullyStripTerm term of
  TermRecord (Record _ fields) -> decode $ M.fromList $ fmap (\(Field fname val) -> (fname, val)) fields
  _ -> unexpected "record" $ Io.showTerm term

matchUnion :: Name -> [(Name, Term -> Flow Graph b)] -> Term -> Flow Graph b
matchUnion tname pairs term = case fullyStripTerm term of
    TermVariable name -> do
      el <- requireElement name
      matchUnion tname pairs (elementTerm el)
    TermUnion (Injection tname' (Field fname val)) -> if tname' == tname
      then case M.lookup fname mapping of
        Nothing -> fail $ "no matching case for field " ++ unName fname
        Just f -> f val
      else unexpected ("injection for type " ++ unName tname) $ Io.showTerm term
    t -> unexpected ("union with one of {" ++ L.intercalate ", " (unName . fst <$> pairs) ++ "}") $ Io.showTerm t
  where
    mapping = M.fromList pairs

matchUnitField :: Name -> y -> (Name, x -> Flow Graph y)
matchUnitField fname x = (fname, \_ -> pure x)
