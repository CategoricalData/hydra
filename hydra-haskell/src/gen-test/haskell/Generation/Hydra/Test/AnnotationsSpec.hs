-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.annotations"},ModuleName {unModuleName = "Annotations"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.annotations"},ModuleName {unModuleName = "Annotations"}),(Namespace {unNamespace = "hydra.compute"},ModuleName {unModuleName = "Compute"}),(Namespace {unNamespace = "hydra.core"},ModuleName {unModuleName = "Core"}),(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.monads"},ModuleName {unModuleName = "Monads"})]

module Generation.Hydra.Test.AnnotationsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Monads as Monads

spec :: H.Spec
spec = H.describe "annotations" $ do
  H.describe "arbitrary annotations" $ do
    H.it "set single annotation #1" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "k1") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))) (Core.TermLiteral (Core.LiteralString "foo")))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "foo")),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "k1", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))])}))
    H.it "set single annotation #2" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "myKey") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-17))))) (Core.TermLiteral (Core.LiteralString "bar")))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "bar")),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "myKey", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-17)))))])}))
    H.it "set single annotation #3" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "x") (Just (Core.TermLiteral (Core.LiteralString "hello"))) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "x", (Core.TermLiteral (Core.LiteralString "hello")))])}))
    H.it "get existing annotation #1" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "k1") (Annotations.setTermAnnotation (Core.Name "k1") (Just (Core.TermLiteral (Core.LiteralString "value"))) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))
      (Just (Core.TermLiteral (Core.LiteralString "value")))
    H.it "get existing annotation #2" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "foo") (Annotations.setTermAnnotation (Core.Name "foo") (Just (Core.TermLiteral (Core.LiteralString ""))) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))))
      (Just (Core.TermLiteral (Core.LiteralString "")))
    H.it "get existing annotation #3" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "key") (Annotations.setTermAnnotation (Core.Name "key") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 123)))) (Core.TermLiteral (Core.LiteralString "test"))))
      (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 123))))
    H.it "get missing annotation #1" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "k1") (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 42))))
      (Nothing)
    H.it "get missing annotation #2" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "nonexistent") (Core.TermLiteral (Core.LiteralString "hello")))
      (Nothing)
    H.it "get missing annotation #3" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "k1") (Annotations.setTermAnnotation (Core.Name "k2") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))
      (Nothing)
    H.it "set multiple annotations #1" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "k2") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 200)))) (Annotations.setTermAnnotation (Core.Name "k1") (Just (Core.TermLiteral (Core.LiteralString "first"))) (Core.TermLiteral (Core.LiteralBoolean True))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralBoolean True)),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "k1", (Core.TermLiteral (Core.LiteralString "first"))),
            (Core.Name "k2", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 200))))])}))
    H.it "set multiple annotations #2" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "b") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))) (Annotations.setTermAnnotation (Core.Name "a") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-5))))) (Core.TermLiteral (Core.LiteralString "test"))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "test")),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "a", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-5))))),
            (Core.Name "b", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))])}))
    H.it "outer annotation overrides inner #1" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "k1") (Just (Core.TermLiteral (Core.LiteralString "outer"))) (Annotations.setTermAnnotation (Core.Name "k1") (Just (Core.TermLiteral (Core.LiteralString "inner"))) (Core.TermLiteral (Core.LiteralString "bar"))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "bar")),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "k1", (Core.TermLiteral (Core.LiteralString "outer")))])}))
    H.it "outer annotation overrides inner #2" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "x") (Just (Core.TermLiteral (Core.LiteralString "new"))) (Annotations.setTermAnnotation (Core.Name "x") (Just (Core.TermLiteral (Core.LiteralString "old"))) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "x", (Core.TermLiteral (Core.LiteralString "new")))])}))
    H.it "outer annotation overrides inner #3" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "key") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 999)))) (Annotations.setTermAnnotation (Core.Name "key") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))) (Core.TermLiteral (Core.LiteralBoolean False))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralBoolean False)),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "key", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 999))))])}))
    H.it "unset single annotation #1" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "k1") Nothing (Annotations.setTermAnnotation (Core.Name "k1") (Just (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 137)))))
      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 137)))
    H.it "unset single annotation #2" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "x") Nothing (Annotations.setTermAnnotation (Core.Name "x") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))) (Core.TermLiteral (Core.LiteralString "test"))))
      (Core.TermLiteral (Core.LiteralString "test"))
    H.it "unset one of multiple annotations #1" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "k1") Nothing (Annotations.setTermAnnotation (Core.Name "k2") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 200)))) (Annotations.setTermAnnotation (Core.Name "k1") (Just (Core.TermLiteral (Core.LiteralString "first"))) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 137))))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 137))),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "k2", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 200))))])}))
    H.it "unset one of multiple annotations #2" $ H.shouldBe
      (Annotations.setTermAnnotation (Core.Name "b") Nothing (Annotations.setTermAnnotation (Core.Name "b") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))) (Annotations.setTermAnnotation (Core.Name "a") (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))) (Core.TermLiteral (Core.LiteralString "x")))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "x")),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "a", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))])}))
  H.describe "descriptions" $ do
    H.it "set description #1" $ H.shouldBe
      (Annotations.setTermDescription (Just "my description") (Core.TermLiteral (Core.LiteralString "foo")))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "foo")),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "my description")))])}))
    H.it "set description #2" $ H.shouldBe
      (Annotations.setTermDescription (Just "") (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "")))])}))
    H.it "set description #3" $ H.shouldBe
      (Annotations.setTermDescription (Just "A longer description with spaces") (Core.TermLiteral (Core.LiteralBoolean True)))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralBoolean True)),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A longer description with spaces")))])}))
    H.it "get existing description #1" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Annotations.getTermDescription (Annotations.setTermDescription (Just "hello") (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))) Lexical.emptyGraph Monads.emptyTrace))
      (Just (Just "hello"))
    H.it "get existing description #2" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Annotations.getTermDescription (Annotations.setTermDescription (Just "") (Core.TermLiteral (Core.LiteralString "test")))) Lexical.emptyGraph Monads.emptyTrace))
      (Just (Just ""))
    H.it "get existing description #3" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Annotations.getTermDescription (Annotations.setTermDescription (Just "desc") (Core.TermLiteral (Core.LiteralBoolean False)))) Lexical.emptyGraph Monads.emptyTrace))
      (Just (Just "desc"))
    H.it "get missing description #1" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Annotations.getTermDescription (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 42)))) Lexical.emptyGraph Monads.emptyTrace))
      (Just Nothing)
    H.it "get missing description #2" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Annotations.getTermDescription (Core.TermLiteral (Core.LiteralString "no description here"))) Lexical.emptyGraph Monads.emptyTrace))
      (Just Nothing)
    H.it "get missing description #3" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Annotations.getTermDescription (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))) Lexical.emptyGraph Monads.emptyTrace))
      (Just Nothing)
    H.it "outer description overrides inner #1" $ H.shouldBe
      (Annotations.setTermDescription (Just "outer") (Annotations.setTermDescription (Just "inner") (Core.TermLiteral (Core.LiteralString "bar"))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "bar")),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "outer")))])}))
    H.it "outer description overrides inner #2" $ H.shouldBe
      (Annotations.setTermDescription (Just "new") (Annotations.setTermDescription (Just "old") (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))))
      (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99))),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "new")))])}))
    H.it "unset description #1" $ H.shouldBe
      (Annotations.setTermDescription Nothing (Annotations.setTermDescription (Just "desc") (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 137)))))
      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 137)))
    H.it "unset description #2" $ H.shouldBe
      (Annotations.setTermDescription Nothing (Annotations.setTermDescription (Just "to be removed") (Core.TermLiteral (Core.LiteralString "test"))))
      (Core.TermLiteral (Core.LiteralString "test"))
  H.describe "layered annotations" $ do
    H.it "get annotation from unannotated term" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "one") (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))
      (Nothing)
    H.it "get annotation from singly annotated term" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "one") (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "one", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))])})))
      (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))
    H.it "get inner annotation from doubly annotated term" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "one") (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "one", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))])})),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "two", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))])})))
      (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))
    H.it "get outer annotation from doubly annotated term" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "two") (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "one", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))])})),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "two", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))])})))
      (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))
    H.it "get non-overridden annotation from triply annotated term" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "two") (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
              Core.annotatedTermAnnotation = (M.fromList [
                (Core.Name "one", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))])})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "two", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))])})),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "one", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99))))])})))
      (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))
    H.it "outer annotation overrides inner in layered term" $ H.shouldBe
      (Annotations.getTermAnnotation (Core.Name "one") (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
              Core.annotatedTermAnnotation = (M.fromList [
                (Core.Name "one", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))])})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "two", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))])})),
          Core.annotatedTermAnnotation = (M.fromList [
            (Core.Name "one", (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99))))])})))
      (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99))))
