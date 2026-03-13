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
import qualified Hydra.Sources.Kernel.Types.Coders as TypeCoders
import qualified Hydra.Sources.Kernel.Types.Compute as TypeCompute
import qualified Hydra.Sources.Kernel.Types.Context as TypeContext
import qualified Hydra.Sources.Kernel.Types.Core as TypeCore
import qualified Hydra.Sources.Kernel.Types.Error as TypeError
import qualified Hydra.Sources.Kernel.Terms.Annotations as TermAnnotations
import qualified Hydra.Sources.Kernel.Terms.Constants as TermConstants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as TermExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as TermLexical
import qualified Hydra.Sources.Kernel.Terms.Rewriting as TermRewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core as TermShowCore
import qualified Hydra.Sources.Decode.Core as TermDecodeCore
import qualified Hydra.Sources.Encode.Core as TermEncodeCore
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


decodeSchemaTypes :: Graph -> M.Map Name TypeScheme
decodeSchemaTypes sg = case schemaGraphToTypingEnvironment (Context [] [] M.empty) sg of
  Left _ -> M.empty
  Right result -> result

testGraph :: Graph
testGraph = elementsToGraph hydraCoreGraph (decodeSchemaTypes testSchemaGraph) (kernelTermBindings ++ dataBindings)
  where
    -- Include only essential kernel term definitions for interpreter tests.
    -- The evaluator needs hydra.annotations (and its dependencies).
    kernelTermBindings = L.concat $ fmap moduleElements
      [ TermConstants.module_
      , TermShowCore.module_
      , TermExtractCore.module_
      , TermLexical.module_
      , TermRewriting.module_
      , TermDecodeCore.module_
      , TermEncodeCore.module_
      , TermAnnotations.module_
      ]
    dataBindings = (\(name, term) -> Binding name term Nothing) <$> M.toList testTerms

testSchemaGraph :: Graph
testSchemaGraph = elementsToGraph hydraCoreGraph (decodeSchemaTypes hydraCoreGraph)
    -- Only the kernel type modules that define types referenced by the test suite schema graph:
    -- CoderDirection (hydra.coders), Coder (hydra.compute), and Type/Name/ForallType (hydra.core).
    (kernelElements ++ testElements)
  where
    kernelElements = L.concat $ fmap moduleElements
      [ TypeCoders.module_
      , TypeCompute.module_
      , TypeContext.module_
      , TypeCore.module_
      , TypeError.module_
      ]
    testElements = fmap
      (\(n, t) -> Binding n (EncodeCore.type_ t) $ Just $ Types.mono $ TypeVariable _Type) $ M.toList testTypes

testContext :: Context
testContext = Context [] [] M.empty

baseLanguage :: Language
baseLanguage = hydraLanguage

baseContext :: AdapterContext
baseContext = AdapterContext testGraph baseLanguage M.empty

check :: String -> H.SpecWith a -> H.SpecWith a
check desc = H.describe $ "Check type inference for " <> desc

checkAdapter :: (Eq t, Eq v, Show t, Show v)
  => (v -> v)
  -> (AdapterContext -> t -> Either String (SymmetricAdapter t v))
  -> ([r] -> AdapterContext)
  -> [r] -> t -> t -> Bool -> v -> v -> H.Expectation
checkAdapter normalize mkAdapter mkContext variants source target lossy vs vt = do
    let cx0 = mkContext variants :: AdapterContext
    let g = adapterContextGraph cx0
    case mkAdapter cx0 source of
      Left err -> HL.assertFailure err
      Right adapter -> do
        let innerCoder = adapterCoder adapter
        adapterSource adapter `H.shouldBe` source
        adapterTarget adapter `H.shouldBe` target
        adapterIsLossy adapter `H.shouldBe` lossy
        case coderEncode innerCoder testContext vs of
          Left ic -> HL.assertFailure (unOtherError (inContextObject ic))
          Right encoded -> normalize encoded `H.shouldBe` normalize vt
        if lossy
          then True `H.shouldBe` True
          else case coderEncode innerCoder testContext vs >>= coderDecode innerCoder testContext of
            Left ic -> HL.assertFailure (unOtherError (inContextObject ic))
            Right roundTripped -> roundTripped `H.shouldBe` vs

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

checkSerdeRoundTrip :: (Context -> Graph -> Type -> Either (InContext OtherError) (Coder Term BS.ByteString))
  -> TypeApplicationTerm -> H.Expectation
checkSerdeRoundTrip mkSerde (TypeApplicationTerm term typ) = do
    case mkSerde testContext testGraph typ of
      Left ic -> HL.assertFailure (unOtherError (inContextObject ic))
      Right serde -> do
        case coderEncode serde testContext term >>= coderDecode serde testContext of
          Left ic -> HL.assertFailure (unOtherError (inContextObject ic))
          Right roundTripped -> deannotateTerm roundTripped `H.shouldBe` deannotateTerm term

checkSerialization :: (Context -> Graph -> Type -> Either (InContext OtherError) (Coder Term String))
  -> TypeApplicationTerm -> String -> H.Expectation
checkSerialization mkSerdeStr (TypeApplicationTerm term typ) expected = do
    case mkSerdeStr testContext testGraph typ of
      Left ic -> HL.assertFailure (unOtherError (inContextObject ic))
      Right serde -> shouldSucceedWith
        (mapInContextError $ fmap normalize $ coderEncode serde testContext term)
        (normalize expected)
  where
    normalize = unlines . L.filter (not . L.null) . lines

eval :: Term -> Either String Term
eval term = case reduceTerm testContext testGraph True term of
    Left ic -> Left (unOtherError (inContextObject ic))
    Right result -> Right result

expectEtaExpansionResult :: String -> Term -> Term -> H.SpecWith ()
expectEtaExpansionResult desc input output = H.it "eta expansion" $ do
  case etaExpandTypedTerm testContext testGraph input of
    Left ic -> HL.assertFailure (unOtherError (inContextObject ic))
    Right result -> result `H.shouldBe` output

expectFailure :: (a -> String) -> String -> Either String a -> H.Expectation
expectFailure print desc f = case f of
    Left _ -> return ()
    Right v -> HL.assertFailure $ "Failure case succeeded with " ++ print v

expectInferenceFailure :: String -> Term -> H.Expectation
expectInferenceFailure desc term = case inferTypeOf testContext testGraph term of
    Left _ -> return ()
    Right ((_, ts), _) -> HL.assertFailure $ "Expected inference failure but got: " ++ ShowCore.typeScheme ts

expectInferenceResult :: String -> Term -> TypeScheme -> H.SpecWith ()
expectInferenceResult desc term expected = do
  case inferTypeOf testContext testGraph term of
    Left ic -> H.runIO (HL.assertFailure (unOtherError (inContextObject ic))) >> return ()
    Right ((iterm, its), _cx') -> do
      H.it "inferred type" $
        H.shouldBe (ShowCore.typeScheme its) (ShowCore.typeScheme expected)
      H.it "inferred term" $
        H.shouldBe (ShowCore.term $ removeTypesFromTerm iterm) (ShowCore.term $ removeTypesFromTerm term)

expectSuccess :: (Eq a, Show a) => String -> Either String a -> a -> H.Expectation
expectSuccess desc result x = case result of
    Left err -> HL.assertFailure err
    Right y -> y `H.shouldBe` x

expectTypeCheckingResult :: String -> Term -> Term -> Type -> H.SpecWith ()
expectTypeCheckingResult desc input outputTerm outputType = do
  case inferTypeOf testContext testGraph input of
    Left ic -> H.runIO (HL.assertFailure (unOtherError (inContextObject ic))) >> return ()
    Right ((iterm, ts), cx1) -> do
      let itype = typeSchemeToFType ts
      case typeOf cx1 testGraph [] iterm of
        Left ic2 -> H.runIO (HL.assertFailure (unOtherError (inContextObject ic2))) >> return ()
        Right (rtype, _cx2) -> do
          H.it "inferred term" $
            H.shouldBe (ShowCore.term iterm) (ShowCore.term outputTerm)
          H.it "inferred type" $
            H.shouldBe (ShowCore.type_ itype) (ShowCore.type_ outputType)
          H.it "reconstructed type" $
            H.shouldBe (ShowCore.type_ rtype) (ShowCore.type_ outputType)

makeMap :: [(String, Int)] -> Term
makeMap keyvals = Terms.map $ M.fromList $ ((\(k, v) -> (Terms.string k, Terms.int32 v)) <$> keyvals)

shouldFail :: Either String a -> H.Expectation
shouldFail f = H.shouldBe True (either (const True) (const False) f)

shouldSucceed :: Either String a -> H.Expectation
shouldSucceed f = case f of
    Left err -> HL.assertFailure err
    Right _ -> True `H.shouldBe` True

shouldSucceedWith :: (Eq a, Show a) => Either String a -> a -> H.Expectation
shouldSucceedWith f x = case f of
    Left err -> HL.assertFailure err
    Right y -> y `H.shouldBe` x

-- | Map an InContext OtherError to a plain String error
mapInContextError :: Either (InContext OtherError) a -> Either String a
mapInContextError (Left ic) = Left (unOtherError (inContextObject ic))
mapInContextError (Right a) = Right a

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
