module Hydra.TestUtils (
  checkLiteralAdapter,
  checkFieldAdapter,
  checkFloatAdapter,
  checkIntegerAdapter,
  checkDataAdapter,
  checkSerdeRoundTrip,
  checkSerialization,
  shouldFail,
  shouldSucceedWith,
  strip,
  termTestContext,
  module Hydra.TestGraph,
) where

import Hydra.Kernel
import Hydra.Adapters.Literal
import Hydra.Adapters.Term
import Hydra.Adapters.UtilsEtc

import Hydra.TestGraph
import Hydra.ArbitraryCore()

import qualified Test.Hspec as H
import qualified Test.HUnit.Lang as HL
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Data.ByteString.Lazy as BS


baseLanguage :: Language m
baseLanguage = hydraCoreLanguage

baseContext :: AdapterContext Kv
baseContext = AdapterContext testGraph baseLanguage baseLanguage

checkAdapter :: (Eq t, Eq v, Show t, Show v)
  => (v -> v)
  -> (t -> Flow (AdapterContext Kv) (SymmetricAdapter (Graph Kv) t v))
  -> ([r] -> AdapterContext Kv)
  -> [r] -> t -> t -> Bool -> v -> v -> H.Expectation
checkAdapter normalize mkAdapter mkContext variants source target lossy vs vt = do
    let acx = mkContext variants :: AdapterContext Kv
    let cx = adapterContextGraph acx
    let FlowState adapter' _ trace = unFlow (mkAdapter source) acx emptyTrace
    if Y.isNothing adapter' then HL.assertFailure (traceSummary trace) else pure ()
    let adapter = Y.fromJust adapter'
    let step = adapterCoder adapter
    adapterSource adapter `H.shouldBe` source
    adapterTarget adapter `H.shouldBe` target
    adapterIsLossy adapter `H.shouldBe` lossy
    fromFlow cx (normalize <$> coderEncode step vs) `H.shouldBe` (normalize vt)
    if lossy
      then True `H.shouldBe` True
      else fromFlow cx (coderEncode step vs >>= coderDecode step) `H.shouldBe` vs

checkLiteralAdapter :: [LiteralVariant] -> LiteralType -> LiteralType -> Bool -> Literal -> Literal -> H.Expectation
checkLiteralAdapter = checkAdapter id literalAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
        languageConstraintsLiteralVariants = S.fromList variants,
        languageConstraintsFloatTypes = floatVars,
        languageConstraintsIntegerTypes = integerVars }
      where
        floatVars = S.fromList [FloatTypeFloat32]
        integerVars = S.fromList [IntegerTypeInt16, IntegerTypeInt32]

checkFieldAdapter :: [TypeVariant] -> FieldType Kv -> FieldType Kv -> Bool -> Field Kv -> Field Kv -> H.Expectation
checkFieldAdapter = checkAdapter id fieldAdapter termTestContext

checkFloatAdapter :: [FloatType] -> FloatType -> FloatType -> Bool -> FloatValue -> FloatValue -> H.Expectation
checkFloatAdapter = checkAdapter id floatAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
      languageConstraintsFloatTypes = S.fromList variants }

checkIntegerAdapter :: [IntegerType] -> IntegerType -> IntegerType -> Bool -> IntegerValue -> IntegerValue -> H.Expectation
checkIntegerAdapter = checkAdapter id integerAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
      languageConstraintsIntegerTypes = S.fromList variants }

checkDataAdapter :: [TypeVariant] -> Type Kv -> Type Kv -> Bool -> Term Kv -> Term Kv -> H.Expectation
checkDataAdapter = checkAdapter stripTerm termAdapter termTestContext

checkSerdeRoundTrip :: (Type Kv -> GraphFlow Kv (Coder (Graph Kv) (Graph Kv) (Term Kv) BS.ByteString))
  -> TypedTerm Kv -> H.Expectation
checkSerdeRoundTrip mkSerde (TypedTerm typ term) = do
    case mserde of
      Nothing -> HL.assertFailure (traceSummary trace)
      Just serde -> shouldSucceedWith
        (stripTerm <$> (coderEncode serde term >>= coderDecode serde))
        (stripTerm term)
  where
    FlowState mserde _ trace = unFlow (mkSerde typ) testGraph emptyTrace

checkSerialization :: (Type Kv -> GraphFlow Kv (Coder (Graph Kv) (Graph Kv) (Term Kv) String))
  -> TypedTerm Kv -> String -> H.Expectation
checkSerialization mkSerdeStr (TypedTerm typ term) expected = do
    case mserde of
      Nothing -> HL.assertFailure (traceSummary trace)
      Just serde -> shouldSucceedWith
        (normalize <$> coderEncode serde term)
        (normalize expected)
  where
    normalize = unlines . L.filter (not . L.null) . lines
    FlowState mserde _ trace = unFlow (mkSerdeStr typ) testGraph emptyTrace

shouldFail :: GraphFlow Kv a -> H.Expectation
shouldFail f = H.shouldBe True (Y.isNothing $ flowStateValue $ unFlow f testGraph emptyTrace)

shouldSucceed :: GraphFlow Kv a -> H.Expectation
shouldSucceed f = case my of
    Nothing -> HL.assertFailure (traceSummary trace)
    Just y -> True `H.shouldBe` True
  where
    FlowState my _ trace = unFlow f testGraph emptyTrace

shouldSucceedWith :: (Eq a, Show a) => GraphFlow Kv a -> a -> H.Expectation
shouldSucceedWith f x = case my of
    Nothing -> HL.assertFailure (traceSummary trace)
    Just y -> y `H.shouldBe` x
  where
    FlowState my _ trace = unFlow f testGraph emptyTrace

strip :: Ord m => Term m -> Term m
strip = stripTerm

termTestContext :: [TypeVariant] -> AdapterContext Kv
termTestContext variants = withConstraints $ (languageConstraints baseLanguage) {
    languageConstraintsTypeVariants = S.fromList variants,
    languageConstraintsLiteralVariants = literalVars,
    languageConstraintsFloatTypes = floatVars,
    languageConstraintsIntegerTypes = integerVars }
  where
    literalVars = S.fromList [LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString]
    floatVars = S.fromList [FloatTypeFloat32]
    integerVars = S.fromList [IntegerTypeInt16, IntegerTypeInt32]

withConstraints :: LanguageConstraints Kv -> AdapterContext Kv
withConstraints c = baseContext { adapterContextTarget = baseLanguage { languageConstraints = c }}
