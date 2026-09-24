-- | Regression test for #740: decoder emission for a recursive parametric union type
-- (e.g. @T a = single: a | multiple: list<T a>@) produced an untyped (domain = Nothing)
-- lambda when eta-expanded for a target language without 'partialApplication' (e.g. Java),
-- causing Java/Scala coder emission to fail with an UntypedLambda checking error.
--
-- Root cause: 'Hydra.Sources.Kernel.Terms.Decoding'.decodeType/decodeTypeNamed's
-- _Type_application arm emitted a bare recursive self-reference to the (polymorphic,
-- Forall-typed) decoder for the recursive type, instead of an explicit TypeApplication.
-- Reduction.hs's etaExpandTerm can resolve a Forall via an explicit TypeApplication
-- (Variables.replaceFreeTypeVariable), but had no way to do so for a bare Application —
-- so it padded the under-applied recursive-decoder reference with untyped wrapper lambdas
-- (domainTypes/peelFunctionDomains deliberately punt to Nothing on a Type.Forall head).
--
-- Fix: wrap the recursive decoder reference in an explicit TypeApplication carrying
-- 'decoderFullResultType' of the applied argument (matching every sibling decoder —
-- decodeListType, decodeMapType, decodeEitherType, decodeMaybeType, decodeSetType — which
-- already do this). Using the RAW applied type instead of decoderFullResultType would
-- also make the untyped-lambda symptom disappear (compiles) but instantiate the forall to
-- the WRONG type wherever decoded != raw (literal variants, wrapper-transparency, nested
-- containers) — a green-but-semantically-wrong fix. This spec guards against both: no
-- untyped lambda in the Java-emitted term, AND the type argument threading through
-- decodeType's self-recursive case is decoderFullResultType-shaped, not the raw type.
--
-- Placed Haskell-side (not as a translingual kernel Test/... DSL module) because the
-- defect is not visible at the Term/JSON level: the DSL-synthesized decoder term itself
-- has zero untyped lambdas (confirmed during the original investigation via direct
-- instrumentation) -- the untyped lambdas are introduced by Codegen.generateSourceFiles's
-- internal eta-expansion pass (Reduction.etaExpandTerm), gated on a target Language's
-- supportedFeatures, which is Haskell host machinery with no DSL-level lever to invoke or
-- observe (same class of DSL-inexpressibility as bug_742's map-insertion-order case).
--
-- The already-committed translingual fixture
-- (Hydra.Sources.Test.TestTypes.testTypeUnionPolymorphicRecursiveListWrapped) supplies the
-- input type; this spec drives it through the real decoder-synthesis + Java-emission
-- pipeline and inspects the resulting term directly, mirroring #740's own investigation
-- reproducer (Codegen.generateSourceFiles with a printDefinitions callback).

module Hydra.RecursiveParametricUnionDecoderSpec where

import Hydra.Kernel
import qualified Hydra.Core.Codegen as Codegen
import qualified Hydra.Generation as Generation
import qualified Hydra.Core.Languages as Languages
import qualified Hydra.Sources.Kernel.Manifest as KernelManifest
import qualified Hydra.Sources.Kernel.Terms.Decoding as Decoding
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Bootstrap as Bootstrap
import Hydra.Core.Typed (TypedTermDefinition(..), TypedTerm(..))

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import qualified Test.Hspec as H
import qualified Test.HUnit.Lang as HL


-- | A Language matching Java's actual supportedFeatures ({nestedCaseStatements} only,
-- since hydra-java is host-native and its javaLanguage value is Java-authored, not
-- reachable from Haskell). Based on the kernel-generated hydraLanguage record (unrestricted
-- features), overridden to just the one feature Java's coder declares support for -- this
-- is precisely the setting that makes doExpand = True (eta-expansion enabled) in
-- Adapt.dataGraphToDefinitions, the path where #740's bug lived.
javaLikeLanguage :: Language
javaLikeLanguage = Languages.hydraLanguage {
  languageSupportedFeatures = S.fromList [LanguageFeatureNestedCaseStatements] }

-- | A standalone module containing just the recursive parametric union test fixture.
fixtureModule :: Module
fixtureModule = Module {
  moduleName = ModuleName "hydra.core.test.recursiveParametricUnionDecoderFixture",
  moduleDefinitions = [Phantoms.toDefinition TestTypes.testTypeUnionPolymorphicRecursiveListWrapped],
  moduleDependencies = [],
  moduleMetadata = Bootstrap.descriptionMetadata (Just "#740 regression fixture module")}

-- | The full kernel universe, needed for decoder synthesis and hoisting/eta-expansion to
-- resolve cross-references (a narrow universe does not reproduce #740 -- confirmed during
-- the original investigation).
kernelUniverse :: [Module]
kernelUniverse = KernelManifest.mainModules

-- | Find every untyped (domain = Nothing) lambda parameter name in a term, with its path
-- (for diagnostics). Mirrors the walker used in #740's original Java-side reproducer.
untypedLambdaParams :: Term -> [(String, Name)]
untypedLambdaParams = go "root"
  where
    go path t = case t of
      TermAnnotated (AnnotatedTerm body _) -> go (path ++ ".annotated") body
      TermApplication (Application f a) -> go (path ++ ".app.fun") f ++ go (path ++ ".app.arg") a
      TermCases (CaseStatement _ mdflt cases) ->
        maybe [] (go (path ++ ".cases.default")) mdflt
          ++ L.concatMap (\(CaseAlternative n h) -> go (path ++ ".cases[" ++ unName n ++ "]") h) cases
      TermEither (Left l) -> go (path ++ ".either.left") l
      TermEither (Right r) -> go (path ++ ".either.right") r
      TermInject (Injection _ (Field _ ft)) -> go (path ++ ".inject") ft
      TermLambda (Lambda p mdom body) ->
        (if Y.isNothing mdom then [(path, p)] else [])
          ++ go (path ++ ".lambda[" ++ unName p ++ "]") body
      TermLet (Let bindings body) ->
        L.concatMap (\(Binding _ bt _) -> go (path ++ ".let") bt) bindings ++ go (path ++ ".let.body") body
      TermList xs -> L.concatMap (go (path ++ ".list[]")) xs
      TermOptional (Just x) -> go (path ++ ".optional") x
      TermPair (a, b) -> go (path ++ ".pair.first") a ++ go (path ++ ".pair.second") b
      TermTypeApplication (TypeApplicationTerm body _) -> go (path ++ ".tyapp") body
      TermTypeLambda (TypeLambda _ body) -> go (path ++ ".tylambda") body
      TermWrap (WrappedTerm _ body) -> go (path ++ ".wrap") body
      _ -> []

spec :: H.Spec
spec = H.describe "Decoder emission for a recursive parametric union type (#740)" $ do
  decoderModulesResult <- H.runIO $ return $
    Generation.generateDecoderModulesPure kernelUniverse [fixtureModule]

  H.it "decoder synthesis succeeds" $
    case decoderModulesResult of
      Left err -> HL.assertFailure err
      Right _ -> return ()

  case decoderModulesResult of
    Left _ -> return ()  -- already failed above; skip the rest
    Right decoderModules -> do
      let cx = emptyInferenceContext
          fullUniverse = kernelUniverse ++ [fixtureModule] ++ decoderModules
          -- Collect every untyped lambda found across all generated definitions for the
          -- decoder module(s), by intercepting Codegen.generateSourceFiles's
          -- printDefinitions callback (never touches the filesystem). The callback's
          -- required return shape is (Map filePath fileContent) since it's meant for real
          -- coders emitting source files; here there's exactly one "file" per module
          -- (keyed by module name) whose "content" is a diagnostic dump of any untyped
          -- lambdas found, empty when none are found.
          collectUntyped :: Module -> [Definition] -> InferenceContext -> Graph -> Either Error (M.Map String String)
          collectUntyped mod' defs _cx2 _g =
            let allUntyped = L.concat [ untypedLambdaParams (termDefinitionBody td) | DefinitionTerm td <- defs ]
            in Right $ M.singleton (unModuleName (moduleName mod')) (show allUntyped)

      genResult <- H.runIO $ return $
        Codegen.generateSourceFiles collectUntyped javaLikeLanguage False Bootstrap.bootstrapGraph fullUniverse decoderModules cx

      H.it "Java-target emission succeeds with no untyped lambda" $
        case genResult of
          Left err -> HL.assertFailure (Generation.showError err)
          Right results -> do
            let allUntypedDumps = [ v | (_k, v) <- results, v /= show ([] :: [(String, Name)]) ]
            if L.null allUntypedDumps
              then return ()
              else HL.assertFailure $
                "Found untyped lambda(s): " ++ show allUntypedDumps

  -- Guard against the raw-vs-decoderFullResultType trap: the fix must thread the DECODED
  -- result type through the self-recursive decoder's TypeApplication, not the raw applied
  -- type. A regression to the raw type would still pass the test above (compiles, no
  -- untyped lambda) but instantiate the forall incorrectly wherever decoded != raw. Check
  -- this directly on the DSL-level decodeType source structure: the _Type_application
  -- case-alternative's handler must build a TypeApplicationTerm whose "type" field routes
  -- through a call to decoderFullResultType, not a bare projection of the applied type.
  -- This is checked at the reified-term level (pre-emission), which IS DSL-expressible,
  -- unlike the untyped-lambda property above.
  H.it "self-recursive decoder reference (_Type_application arm) instantiates via decoderFullResultType, not the raw type" $ do
    let TypedTerm decodeTypeBody = typedTermDefinitionTerm Decoding.decodeType
    case findApplicationCaseHandler decodeTypeBody of
      Nothing -> HL.assertFailure "Could not find decodeType's _Type_application case alternative"
      Just handler ->
        if usesDecoderFullResultType handler
          then return ()
          else HL.assertFailure
            "decodeType's _Type_application arm does not reference decoderFullResultType \
            \-- likely regressed to the raw applied type (the green-but-wrong trap)"

-- | Find the case-alternative handler tagged "application" within a hydra.core.model.Type
-- CaseStatement anywhere in the term (decodeType's top-level match-on-Type).
findApplicationCaseHandler :: Term -> Maybe Term
findApplicationCaseHandler t = case t of
  TermAnnotated (AnnotatedTerm body _) -> findApplicationCaseHandler body
  TermLambda (Lambda _ _ body) -> findApplicationCaseHandler body
  TermApplication (Application f a) ->
    firstJust [findApplicationCaseHandler f, findApplicationCaseHandler a]
  TermCases (CaseStatement (Name "hydra.core.model.Type") _ cases) ->
    firstJust [Just h | CaseAlternative n h <- cases, unName n == "application"]
  _ -> Nothing
  where
    firstJust [] = Nothing
    firstJust (Just x : _) = Just x
    firstJust (Nothing : rest) = firstJust rest

-- | Does this term reference hydra.core.decoding.decoderFullResultType anywhere?
usesDecoderFullResultType :: Term -> Bool
usesDecoderFullResultType t = case t of
  TermVariable n -> unName n == "hydra.core.decoding.decoderFullResultType"
  TermAnnotated (AnnotatedTerm body _) -> usesDecoderFullResultType body
  TermApplication (Application f a) -> usesDecoderFullResultType f || usesDecoderFullResultType a
  TermInject (Injection _ (Field _ ft)) -> usesDecoderFullResultType ft
  TermLambda (Lambda _ _ body) -> usesDecoderFullResultType body
  TermLet (Let bindings body) ->
    any (\(Binding _ bt _) -> usesDecoderFullResultType bt) bindings || usesDecoderFullResultType body
  TermList xs -> any usesDecoderFullResultType xs
  TermPair (a, b) -> usesDecoderFullResultType a || usesDecoderFullResultType b
  TermRecord (Record _ fields) -> any (\(Field _ ft) -> usesDecoderFullResultType ft) fields
  TermTypeApplication (TypeApplicationTerm body _) -> usesDecoderFullResultType body
  TermWrap (WrappedTerm _ body) -> usesDecoderFullResultType body
  _ -> False
