module Hydra.Ext.Yaml.Coder (
--  decodeTerm,
  encodeTerm,
  toYaml,
  yamlCoder,
  yamlLanguage,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Adapter
import Hydra.Prototyping.Adapters.Term
import qualified Hydra.Ext.Yaml.Model as YM
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Dsl
import Hydra.Impl.Haskell.Extras
import Hydra.Prototyping.Steps
import qualified Hydra.Prototyping.CoreEncoding as CE

import qualified Control.Monad as CM
import Data.Function ((&))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


yamlCoder :: Context -> Type -> Qualified (Step Term YM.Node)
yamlCoder context typ = do
    adapter <- termAdapter adContext typ
    coder <- termCoder $ adapterTarget adapter
    return $ composeSteps (adapterStep adapter) coder
  where
    adContext = AdapterContext context hydraCoreLanguage yamlLanguage

atomicCoder :: AtomicType -> Qualified (Step AtomicValue YM.Scalar)
atomicCoder at = pure $ case at of
  AtomicTypeBoolean -> Step {
    stepOut = \(AtomicValueBoolean b) -> pure $ YM.ScalarBool $ case b of
      BooleanValueFalse -> False
      BooleanValueTrue -> True,
    stepIn = \s -> case s of
      YM.ScalarBool b -> pure $ AtomicValueBoolean $ if b then BooleanValueTrue else BooleanValueFalse
      _ -> unexpected s "boolean"}
  AtomicTypeFloat _ -> Step {
    stepOut = \(AtomicValueFloat (FloatValueBigfloat f)) -> pure $ YM.ScalarFloat f,
    stepIn = \s -> case s of
      YM.ScalarFloat f -> pure $ AtomicValueFloat $ FloatValueBigfloat f
      _ -> unexpected s "floating-point value"}
  AtomicTypeInteger _ -> Step {
    stepOut = \(AtomicValueInteger (IntegerValueBigint i)) -> pure $ YM.ScalarInt i,
    stepIn = \s -> case s of
      YM.ScalarInt i -> pure $ AtomicValueInteger $ IntegerValueBigint i
      _ -> unexpected s "integer"}
  AtomicTypeString -> Step {
    stepOut = \(AtomicValueString s) -> pure $ YM.ScalarStr s,
    stepIn = \s -> case s of
      YM.ScalarStr s' -> pure $ AtomicValueString s'
      _ -> unexpected s "string"}

-- Note: encodes unit-valued fields as absent key/value pairs
recordCoder :: [FieldType] -> Qualified (Step Term YM.Node)
recordCoder sfields = do
    coders <- CM.mapM (\f -> (,) <$> pure (fieldTypeName f) <*> termCoder (fieldTypeType f)) sfields
    return $ Step (encode coders) (decode coders)
  where
    encode coders (TermRecord fields) = (YM.NodeMapping . M.fromList . Y.catMaybes) <$> CM.zipWithM encodeField coders fields
      where
        encodeField (_, coder) (Field fn fv) = if fv == unitTerm
          then pure Nothing
          else Just <$> ((,) <$> pure (yamlString fn) <*> stepOut coder fv)
    decode coders n = case n of
      YM.NodeMapping m -> TermRecord <$> CM.mapM (decodeField m) coders -- Note: unknown fields are ignored 
        where
          decodeField m (fn, coder) = do
            v <- Y.maybe (pure unitTerm) (stepIn coder) $ M.lookup (yamlString fn) m           
            return $ Field fn v
      _ -> unexpected n "mapping"
    getCoder coders fname = Y.maybe error pure $ M.lookup fname coders
      where
        error = fail $ "no such field: " ++ fname

termCoder :: Type -> Qualified (Step Term YM.Node)
termCoder typ = case typ of
  TypeAtomic at -> do
    ac <- atomicCoder at
    return Step {
      stepOut = \(TermAtomic av) -> YM.NodeScalar <$> stepOut ac av,
      stepIn = \n -> case n of
        YM.NodeScalar s -> TermAtomic <$> stepIn ac s
        _ -> unexpected n "scalar node"}
  TypeList lt -> do
    lc <- termCoder lt
    return Step {
      stepOut = \(TermList els) -> YM.NodeSequence <$> CM.mapM (stepOut lc) els,
      stepIn = \n -> case n of
        YM.NodeSequence nodes -> TermList <$> CM.mapM (stepIn lc) nodes
        _ -> unexpected n "sequence"}
  TypeOptional ot -> do
    oc <- termCoder ot
    return Step {
      stepOut = \(TermOptional el) -> Y.maybe (pure yamlNull) (stepOut oc) el,
      stepIn = \n -> case n of
        YM.NodeScalar YM.ScalarNull -> pure $ TermOptional Nothing
        _ -> TermOptional . Just <$> stepIn oc n}
  TypeMap (MapType kt vt) -> do
    kc <- termCoder kt
    vc <- termCoder vt
    let encodeEntry (k, v) = (,) <$> stepOut kc k <*> stepOut vc v
    let decodeEntry (k, v) = (,) <$> stepIn kc k <*> stepIn vc v
    return Step {
      stepOut = \(TermMap m) -> YM.NodeMapping . M.fromList <$> CM.mapM encodeEntry (M.toList m),
      stepIn = \n -> case n of
        YM.NodeMapping m -> TermMap . M.fromList <$> CM.mapM decodeEntry (M.toList m)
        _ -> unexpected n "mapping"}
  TypeRecord sfields -> recordCoder sfields

unexpected :: Show v => v -> String -> Result a
unexpected x desc = fail $ "expected " ++ desc ++ ", found: " ++ show x

yamlNull :: YM.Node
yamlNull = YM.NodeScalar YM.ScalarNull

yamlString :: String -> YM.Node
yamlString = YM.NodeScalar . YM.ScalarStr










data Varcoder v t1 t2 = Varcoder {
  coderVariant :: v,
  coderName :: String,
  coderEncode :: t1 -> Result t2,
  coderDecode :: t2 -> Result t1 }

atomicCoders :: [Varcoder AtomicVariant AtomicValue YM.Node]
atomicCoders = [
  Varcoder AtomicVariantBinary "binary" notImplemented notImplemented,
  Varcoder AtomicVariantBoolean "boolean"
    (\(AtomicValueBoolean b) -> encodeBooleanValue b)
    (fmap AtomicValueBoolean . decodeBooleanValue),
  Varcoder AtomicVariantFloat "float"
    (\(AtomicValueFloat fv) -> encodeFloatValue fv)
    (fmap AtomicValueFloat . decodeFloatValue),
  Varcoder AtomicVariantInteger "integer"
    (\(AtomicValueInteger iv) -> encodeIntegerValue iv)
    (fmap AtomicValueInteger . decodeIntegerValue),
  Varcoder AtomicVariantString "string"
    (\(AtomicValueString s) -> encodeStringValue s)
    (fmap AtomicValueString . decodeStringValue)]

atomicCodersByName :: M.Map String (Varcoder AtomicVariant AtomicValue YM.Node)
atomicCodersByName = codersByName atomicCoders

atomicCodersByVariant :: M.Map AtomicVariant (Varcoder AtomicVariant AtomicValue YM.Node)
atomicCodersByVariant = codersByVariant atomicCoders

codersByName :: [Varcoder v t1 t2] -> M.Map String (Varcoder v t1 t2)
codersByName coders = M.fromList $ (\c -> (coderName c, c)) <$> coders

codersByVariant :: Ord v => [Varcoder v t1 t2] -> M.Map v (Varcoder v t1 t2)
codersByVariant coders = M.fromList $ (\c -> (coderVariant c, c)) <$> coders

decodeAtomicValue :: AtomicVariant -> YM.Node -> Result AtomicValue
decodeAtomicValue var n = do
  coderDecode <$> lookupCoderByVariant atomicCodersByVariant var
  >>= (n &)

decodeBooleanValue :: YM.Node -> Result BooleanValue
decodeBooleanValue n = expectScalar n >>= expectBoolean

decodeFloatValue :: YM.Node -> Result FloatValue
decodeFloatValue n = FloatValueBigfloat <$> (expectScalar n >>= expectFloat)

decodeIntegerValue :: YM.Node -> Result IntegerValue
decodeIntegerValue n = IntegerValueBigint <$> (expectScalar n >>= expectInteger)

decodeStringValue :: YM.Node -> Result String
decodeStringValue n = expectScalar n >>= expectString

encodeAtomicValue :: AtomicValue -> Result YM.Node
encodeAtomicValue av =
  lookupCoderByVariant atomicCodersByVariant (atomicValueVariant av) >>= (av &) . coderEncode

encodeBooleanValue :: BooleanValue -> Result YM.Node
encodeBooleanValue = pure . YM.NodeScalar . YM.ScalarBool . toBool
  where
    toBool bv = case bv of
      BooleanValueFalse -> False
      BooleanValueTrue -> True

encodeFloatValue :: FloatValue -> Result YM.Node
encodeFloatValue fv =
  (coderEncode <$> lookupCoderByVariant floatCodersByVariant (floatValueVariant fv))
  >>= (fv &)

encodeIntegerValue :: IntegerValue -> Result YM.Node
encodeIntegerValue iv =
  (coderEncode <$> lookupCoderByVariant integerCodersByVariant (integerValueVariant iv))
  >>= (iv &)

encodeStringValue :: String -> Result YM.Node
encodeStringValue = pure . YM.NodeScalar . YM.ScalarStr

encodeTerm :: Term -> Result YM.Node
encodeTerm term = do
  let term' = CE.encodeTerm term
  (coderEncode <$> lookupCoderByVariant termCodersByVariant (termVariant term'))
    >>= (term' &)

expectBoolean :: YM.Scalar -> Result BooleanValue
expectBoolean s = case s of
  YM.ScalarBool b -> pure $ if b then BooleanValueTrue else BooleanValueFalse
  _ -> fail $ "expected a boolean scalar, found " ++ show s

expectFloat :: YM.Scalar -> Result Double
expectFloat s = case s of
  YM.ScalarFloat f -> pure f
  _ -> fail $ "expected a float, found " ++ show s

expectInteger :: YM.Scalar -> Result Integer
expectInteger s = case s of
  YM.ScalarInt i -> pure i
  _ -> fail $ "expected an int, found " ++ show s

expectScalar :: YM.Node -> Result YM.Scalar
expectScalar node = case node of
  YM.NodeScalar s -> pure s
  _ -> fail $ "expected a scalar node, found " ++ show node

expectString :: YM.Scalar -> Result String
expectString s = case s of
  YM.ScalarStr v -> pure v
  _ -> fail $ "expected a string scalar, found " ++ show s

floatCoders :: [Varcoder FloatVariant FloatValue YM.Node]
floatCoders = [
  Varcoder FloatVariantBigfloat "bigfloat" notImplemented notImplemented,
  Varcoder FloatVariantFloat32 "float32" notImplemented notImplemented,
  Varcoder FloatVariantFloat64 "float64" notImplemented notImplemented]

floatCodersByName :: M.Map String (Varcoder FloatVariant FloatValue YM.Node)
floatCodersByName = codersByName floatCoders

floatCodersByVariant :: M.Map FloatVariant (Varcoder FloatVariant FloatValue YM.Node)
floatCodersByVariant = codersByVariant floatCoders

integerCoders :: [Varcoder IntegerVariant IntegerValue YM.Node]
integerCoders = [
  Varcoder IntegerVariantBigint "bigint" notImplemented notImplemented,
  Varcoder IntegerVariantInt8 "int8" notImplemented notImplemented,
  Varcoder IntegerVariantInt16 "int16" notImplemented notImplemented,
  Varcoder IntegerVariantInt32 "int32" notImplemented notImplemented,
  Varcoder IntegerVariantInt64 "int64" notImplemented notImplemented,
  Varcoder IntegerVariantUint8 "uint8" notImplemented notImplemented,
  Varcoder IntegerVariantUint16 "uint16" notImplemented notImplemented,
  Varcoder IntegerVariantUint32 "uint32" notImplemented notImplemented,
  Varcoder IntegerVariantUint64 "uint64" notImplemented notImplemented]

integerCodersByName :: M.Map String (Varcoder IntegerVariant IntegerValue YM.Node)
integerCodersByName = codersByName integerCoders

integerCodersByVariant :: M.Map IntegerVariant (Varcoder IntegerVariant IntegerValue YM.Node)
integerCodersByVariant = codersByVariant integerCoders

lookupCoderByName :: M.Map String a -> String -> Result a
lookupCoderByName m key = Y.maybe (unknownVariant key) pure $ M.lookup key m

lookupCoderByVariant :: (Ord v, Show v) => M.Map v a -> v -> Result a
lookupCoderByVariant m v = Y.maybe (unsupportedVariant v) pure $ M.lookup v m

notImplemented :: a -> Result b
notImplemented _ = fail "not yet implemented"

termCoders :: [Varcoder TermVariant Term YM.Node]
termCoders = [
  Varcoder TermVariantAtomic "atomic"
    (\(TermAtomic av) -> encodeAtomicValue av)
--    (\n -> TermAtomic <$> decodeAtomicValue n),
    notImplemented,
  Varcoder TermVariantList "list" notImplemented notImplemented,
  Varcoder TermVariantMap "map" notImplemented notImplemented,
  Varcoder TermVariantRecord "record" notImplemented notImplemented,
  Varcoder TermVariantSet "set" notImplemented notImplemented,
  Varcoder TermVariantUnion "union" notImplemented notImplemented]

termCodersByName :: M.Map String (Varcoder TermVariant Term YM.Node)
termCodersByName = codersByName termCoders

termCodersByVariant :: M.Map TermVariant (Varcoder TermVariant Term YM.Node)
termCodersByVariant = codersByVariant termCoders

unknownVariant :: [Char] -> Result b
unknownVariant name = fail $ "Unknown variant: " ++ name

unsupportedVariant :: Show a => a -> Result b
unsupportedVariant v = fail $ "Unsupported variant: " ++ show v

stringNode :: String -> YM.Node
stringNode = YM.NodeScalar . YM.ScalarStr

yamlLanguage :: Language
yamlLanguage = Language "hydra/ext/yaml" $ Language_Constraints {
  languageConstraintsAtomicVariants = S.fromList [
    AtomicVariantBoolean, AtomicVariantFloat, AtomicVariantInteger, AtomicVariantString],
  languageConstraintsFloatVariants = S.fromList [FloatVariantBigfloat],
  languageConstraintsIntegerVariants = S.fromList [IntegerVariantBigint],
  languageConstraintsTermVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAtomic, TypeVariantList, TypeVariantMap, TypeVariantOptional, TypeVariantRecord] }

toYaml :: Context -> Type -> Term -> Result YM.Node
toYaml context typ term = do
    ad <- qualifiedToResult $ termAdapter adapterContext typ -- note: mixing error types
    stepOut (adapterStep ad) term >>= encodeTermForYaml typ
  where
    adapterContext = AdapterContext context hydraCoreLanguage yamlLanguage

{-
import qualified Data.Set as S
adapter = termAdapter (TypeRecord [FieldType "foo" stringType, FieldType "bar" (TypeSet stringType)])
term = TermRecord [Field "foo" $ stringValue "FOOO"]
term = TermRecord [Field "foo" $ stringValue "FOOO", Field "bar" (TermSet (S.fromList [stringValue "one", stringValue "two"]))]
pure term
stepOut adapter term
stepOut adapter term >>= stepIn adapter
-}

encodeTermForYaml :: Type -> Term -> Result YM.Node
encodeTermForYaml typ term = case term of
    TermAtomic av -> pure $ YM.NodeScalar $ YM.ScalarStr $ show av
    TermRecord fields -> YM.NodeMapping . M.fromList <$> CM.mapM toPair fields
      where
        toPair (Field name term) = do
          term' <- encodeTermForYaml typ term
          return (stringNode name, term')
