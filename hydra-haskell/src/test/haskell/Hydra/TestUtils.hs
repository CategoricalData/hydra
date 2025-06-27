module Hydra.TestUtils (
  check,
  checkLiteralAdapter,
  checkFieldAdapter,
  checkFloatAdapter,
  checkIntegerAdapter,
  checkDataAdapter,
  checkSerdeRoundTrip,
  checkSerialization,
  eval,
  expectInferenceResult,
  expectSuccess,
  expectTypeOfResult,
  shouldFail,
  shouldSucceedWith,
  strip,
  termTestContext,
  module Hydra.Staging.TestGraph,
) where

import Hydra.Kernel
import Hydra.LiteralAdapters
import Hydra.TermAdapters
import Hydra.AdapterUtils
import Hydra.Staging.TestGraph
import Hydra.ArbitraryCore()
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Show.Core as ShowCore

import qualified Test.Hspec as H
import qualified Test.HUnit.Lang as HL
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Data.ByteString.Lazy as BS


baseLanguage :: Language
baseLanguage = hydraCoreLanguage

baseContext :: AdapterContext
baseContext = AdapterContext testGraph baseLanguage M.empty

check :: String -> H.SpecWith a -> H.SpecWith a
check desc = H.describe $ "Check type inference for " <> desc

checkAdapter :: (Eq t, Eq v, Show t, Show v)
  => (v -> v)
  -> (t -> Flow AdapterContext (SymmetricAdapter AdapterContext t v))
  -> ([r] -> AdapterContext)
  -> [r] -> t -> t -> Bool -> v -> v -> H.Expectation
checkAdapter normalize mkAdapter mkContext variants source target lossy vs vt = do
    let cx0 = mkContext variants :: AdapterContext
    let g = adapterContextGraph cx0
    let FlowState adapter' cx trace = unFlow (mkAdapter source) cx0 emptyTrace
    if Y.isNothing adapter' then HL.assertFailure (traceSummary trace) else pure ()
    let adapter = Y.fromJust adapter'
    let step = Coder encode decode
          where
            encode = withState cx . coderEncode (adapterCoder adapter)
            decode = withState cx . coderDecode (adapterCoder adapter)
    adapterSource adapter `H.shouldBe` source
    adapterTarget adapter `H.shouldBe` target
    adapterIsLossy adapter `H.shouldBe` lossy
    fromFlow vt g (normalize <$> coderEncode step vs) `H.shouldBe` (normalize vt)
    if lossy
      then True `H.shouldBe` True
      else fromFlow vs g (coderEncode step vs >>= coderDecode step) `H.shouldBe` vs

checkLiteralAdapter :: [LiteralVariant] -> LiteralType -> LiteralType -> Bool -> Literal -> Literal -> H.Expectation
checkLiteralAdapter = checkAdapter id literalAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
        languageConstraintsLiteralVariants = variantSet,
        languageConstraintsFloatTypes = floatVars,
        languageConstraintsIntegerTypes = integerVars }
      where
        variantSet = S.fromList variants
        floatVars = if (S.member LiteralVariantFloat variantSet)
          then S.fromList [FloatTypeFloat32]
          else S.empty
        integerVars = if (S.member LiteralVariantInteger variantSet)
          then S.fromList [IntegerTypeInt16, IntegerTypeInt32]
          else S.empty

checkFieldAdapter :: [TypeVariant] -> FieldType -> FieldType -> Bool -> Field -> Field -> H.Expectation
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

checkDataAdapter :: [TypeVariant] -> Type -> Type -> Bool -> Term -> Term -> H.Expectation
checkDataAdapter = checkAdapter stripTerm termAdapter termTestContext

checkSerdeRoundTrip :: (Type -> Flow Graph (Coder Graph Graph Term BS.ByteString))
  -> TypedTerm -> H.Expectation
checkSerdeRoundTrip mkSerde (TypedTerm term typ) = do
    case mserde of
      Nothing -> HL.assertFailure (traceSummary trace)
      Just serde -> shouldSucceedWith
        (stripTerm <$> (coderEncode serde term >>= coderDecode serde))
        (stripTerm term)
  where
    FlowState mserde _ trace = unFlow (mkSerde typ) testGraph emptyTrace

checkSerialization :: (Type -> Flow Graph (Coder Graph Graph Term String))
  -> TypedTerm -> String -> H.Expectation
checkSerialization mkSerdeStr (TypedTerm term typ) expected = do
    case mserde of
      Nothing -> HL.assertFailure (traceSummary trace)
      Just serde -> shouldSucceedWith
        (normalize <$> coderEncode serde term)
        (normalize expected)
  where
    normalize = unlines . L.filter (not . L.null) . lines
    FlowState mserde _ trace = unFlow (mkSerdeStr typ) testGraph emptyTrace

eval :: Term -> Flow Graph Term
eval = reduceTerm True M.empty

expectInferenceResult :: String -> Term -> TypeScheme -> H.Expectation
expectInferenceResult desc term expected = do
    expectSuccess desc (ShowCore.typeScheme . snd <$> result) (ShowCore.typeScheme expected)
    expectSuccess desc (ShowCore.term . stripTypesFromTerm . fst <$> result) (ShowCore.term $ stripTypesFromTerm term)
  where
    result = do
      cx <- graphToInferenceContext testGraph
      inferTypeOf cx term

expectSuccess :: (Eq a, Show a) => String -> Flow () a -> a -> H.Expectation
expectSuccess desc f x = case my of
    Nothing -> HL.assertFailure $ "Error: " ++ traceSummary trace
    Just y -> y `H.shouldBe` x
  where
    FlowState my _ trace = unFlow f2 () emptyTrace
    f2 = do
      putAttr key_debugId $ Terms.string desc
      f

expectTypeOfResult :: String -> S.Set Name -> M.Map Name Type -> Term -> Type -> H.Expectation
expectTypeOfResult desc vars types term expected = do
    expectSuccess desc (ShowCore.type_ <$> result) (ShowCore.type_ expected)
  where
    result = do
      cx <- graphToInferenceContext testGraph
      typeOf cx vars types term

shouldFail :: Flow Graph a -> H.Expectation
shouldFail f = H.shouldBe True (Y.isNothing $ flowStateValue $ unFlow f testGraph emptyTrace)

shouldSucceed :: Flow Graph a -> H.Expectation
shouldSucceed f = case my of
    Nothing -> HL.assertFailure (traceSummary trace)
    Just y -> True `H.shouldBe` True
  where
    FlowState my _ trace = unFlow f testGraph emptyTrace

shouldSucceedWith :: (Eq a, Show a) => Flow Graph a -> a -> H.Expectation
shouldSucceedWith f x = case my of
    Nothing -> HL.assertFailure (traceSummary trace)
    Just y -> y `H.shouldBe` x
  where
    FlowState my _ trace = unFlow f testGraph emptyTrace

strip :: Term -> Term
strip = stripTerm

termTestContext :: [TypeVariant] -> AdapterContext
termTestContext variants = withConstraints $ (languageConstraints baseLanguage) {
    languageConstraintsTypeVariants = S.fromList variants,
    languageConstraintsLiteralVariants = literalVars,
    languageConstraintsFloatTypes = floatVars,
    languageConstraintsIntegerTypes = integerVars }
  where
    literalVars = S.fromList [LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString]
    floatVars = S.fromList [FloatTypeFloat32]
    integerVars = S.fromList [IntegerTypeInt16, IntegerTypeInt32, IntegerTypeBigint]

withConstraints :: LanguageConstraints -> AdapterContext
withConstraints c = baseContext { adapterContextLanguage = baseLanguage { languageConstraints = c }}
