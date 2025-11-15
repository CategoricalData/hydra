module Hydra.TestUtils (
  module Hydra.TestUtils,
  module Hydra.Sources.Libraries,
  module Hydra.Test.TestGraph,
  module Hydra.Test.TestTypes,
  module Hydra.Test.TestTerms,
) where

import Hydra.Kernel
import Hydra.Adapt.Literals
import Hydra.Adapt.Terms
import Hydra.Adapt.Utils
import Hydra.ArbitraryCore()
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Terms
import Hydra.Sources.Kernel.Types.All
import Hydra.Sources.Kernel.Types.Core
import Hydra.Sources.Libraries
import Hydra.Test.TestGraph
import Hydra.Test.TestTypes
import Hydra.Test.TestTerms
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Show.Core as ShowCore

import qualified Test.Hspec as H
import qualified Test.HUnit.Lang as HL
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Data.ByteString.Lazy as BS


testGraph :: Graph
testGraph = elementsToGraph hydraCoreGraph (Just testSchemaGraph) dataBindings
  where
    dataBindings = (\(name, term) -> Binding name term Nothing) <$> M.toList testTerms

testSchemaGraph :: Graph
testSchemaGraph = elementsToGraph hydraCoreGraph (Just hydraCoreGraph)
    -- We include all types from the Hydra kernel, as well as some additional types specifically for tests.
    (kernelElements ++ testElements)
  where
    kernelElements = L.concat $ fmap moduleElements kernelTypesModules
    testElements = fmap
      (\(n, t) -> Binding n (EncodeCore.type_ t) $ Just $ Types.mono $ TypeVariable _Type) $ M.toList testTypes

baseLanguage :: Language
baseLanguage = hydraLanguage

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
checkDataAdapter = checkAdapter deannotateTerm termAdapter termTestContext

checkSerdeRoundTrip :: (Type -> Flow Graph (Coder Graph Graph Term BS.ByteString))
  -> TypeApplicationTerm -> H.Expectation
checkSerdeRoundTrip mkSerde (TypeApplicationTerm term typ) = do
    case mserde of
      Nothing -> HL.assertFailure (traceSummary trace)
      Just serde -> shouldSucceedWith
        (deannotateTerm <$> (coderEncode serde term >>= coderDecode serde))
        (deannotateTerm term)
  where
    FlowState mserde _ trace = unFlow (mkSerde typ) testGraph emptyTrace

checkSerialization :: (Type -> Flow Graph (Coder Graph Graph Term String))
  -> TypeApplicationTerm -> String -> H.Expectation
checkSerialization mkSerdeStr (TypeApplicationTerm term typ) expected = do
    case mserde of
      Nothing -> HL.assertFailure (traceSummary trace)
      Just serde -> shouldSucceedWith
        (normalize <$> coderEncode serde term)
        (normalize expected)
  where
    normalize = unlines . L.filter (not . L.null) . lines
    FlowState mserde _ trace = unFlow (mkSerdeStr typ) testGraph emptyTrace

eval :: Term -> Flow Graph Term
eval = reduceTerm True

expectEtaExpansionResult :: String -> Term -> Term -> H.SpecWith ()
expectEtaExpansionResult desc input output = H.it "eta expansion" $ shouldSucceedWith
  (do
    tx <- graphToTypeContext testGraph
    etaExpandTypedTerm tx input)
  output

expectFailure :: (a -> String) -> String -> Flow () a -> H.Expectation
expectFailure print desc f = case my of
    Nothing -> return ()
    Just v -> HL.assertFailure $ "Failure case succeeded with " ++ print v ++ "\n" ++ traceSummary trace
  where
    FlowState my _ trace = unFlow f2 () emptyTrace
    f2 = do
      putAttr key_debugId $ Terms.string desc
      f

expectInferenceFailure :: String -> Term -> H.Expectation
expectInferenceFailure desc term = expectFailure (ShowCore.typeScheme . snd) desc $ do
  cx <- graphToInferenceContext testGraph
  inferTypeOf cx term

expectInferenceResult :: String -> Term -> TypeScheme -> H.SpecWith ()
expectInferenceResult desc term expected = do
  (iterm, its) <- H.runIO $ fromTestFlow desc $ do
    cx <- graphToInferenceContext testGraph
    inferTypeOf cx term

  H.it "inferred type" $
    H.shouldBe (ShowCore.typeScheme its) (ShowCore.typeScheme expected)
  H.it "inferred term" $
    H.shouldBe (ShowCore.term $ removeTypesFromTerm iterm) (ShowCore.term $ removeTypesFromTerm term)

expectSuccess :: (Eq a, Show a) => String -> Flow () a -> a -> H.Expectation
expectSuccess desc flow x = case my of
    Nothing -> HL.assertFailure $ traceSummary trace
    Just y -> y `H.shouldBe` x
  where
    FlowState my _ trace = unFlow flow2 () emptyTrace
    flow2 = do
      putAttr key_debugId $ Terms.string desc
      flow

expectTypeCheckingResult :: String -> Term -> Term -> Type -> H.SpecWith ()
expectTypeCheckingResult desc input outputTerm outputType = do
  (iterm, itype, rtype) <- H.runIO $ fromTestFlow desc $ do
    cx <- graphToInferenceContext testGraph
    let tx = TypeContext M.empty S.empty cx

    -- typeOf is always called on System F terms
    (iterm, ts) <- inferTypeOf cx input
    let itype = typeSchemeToFType ts

    rtype <- typeOf tx [] iterm
    return (iterm, itype, rtype)

  -- Three labeled assertions as per the type checking specification
  H.it "inferred term" $
    H.shouldBe (ShowCore.term iterm) (ShowCore.term outputTerm)
  H.it "inferred type" $
    H.shouldBe (ShowCore.type_ itype) (ShowCore.type_ outputType)
  H.it "reconstructed type" $
    H.shouldBe (ShowCore.type_ rtype) (ShowCore.type_ outputType)

fromTestFlow :: String -> Flow () a -> IO a
fromTestFlow desc flow = case my of
    Nothing -> fail $ traceSummary trace
    Just y -> return y
  where
    FlowState my _ trace = unFlow flow2 () emptyTrace
    flow2 = do
      putAttr key_debugId $ Terms.string desc
      flow

makeMap :: [(String, Int)] -> Term
makeMap keyvals = Terms.map $ M.fromList $ ((\(k, v) -> (Terms.string k, Terms.int32 v)) <$> keyvals)

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
strip = deannotateTerm

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
