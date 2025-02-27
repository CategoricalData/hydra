module Hydra.Inference.InferenceTestUtils where

import Hydra.Kernel
import qualified Hydra.Dsl.Types as Types
import Hydra.TestUtils
import Hydra.Staging.Inference.Inference
import Hydra.Staging.Inference.Rules

import qualified Test.Hspec as H


checkType :: Term -> Type -> H.Expectation
checkType term typ = typeAnn term `H.shouldBe` typ
  where
    typeAnn (TermTyped (TypedTerm _ typ)) = typ

executeFlow = fromFlow (TermLiteral $ LiteralString "no term") testGraph

expectFailure :: Term -> H.Expectation
expectFailure term = do
  shouldFail (inferredTypeOf term)

expectMonotype :: Term -> Type -> H.Expectation
expectMonotype term = expectPolytype term []

expectPolytype :: Term-> [String] -> Type -> H.Expectation
expectPolytype term vars typ = do
    shouldSucceedWith
      (inferTypeScheme term)
      (TypeScheme (Name <$> vars) typ)

expectRawType :: Term -> Type -> H.Expectation
expectRawType term typ = do
  shouldSucceedWith
    (inferredType <$> withInferenceContext (infer term))
    typ

expectType :: Term -> Type -> H.Expectation
expectType term typ = do
  shouldSucceedWith
    (inferredTypeOf term)
    typ

expectTypeAnnotation :: (Term -> Flow (Graph) (Term)) -> Term -> Type -> H.Expectation
expectTypeAnnotation path term etyp = shouldSucceedWith atyp etyp
  where
   atyp = do
     iterm <- inferTermType term
     selected <- path iterm
     case selected of
       TermTyped (TypedTerm _ typ) -> return typ
       _ -> fail $ "no type annotation"