module Hydra.Sources.Test.Inference.Idempotence where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Lib.Eithers as DefEithers
import qualified Hydra.Lib.Lists as DefLists
import qualified Hydra.Lib.Logic as DefLogic
import qualified Hydra.Lib.Math as DefMath


ns :: ModuleName
ns = ModuleName "hydra.test.inference.idempotence"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, ModuleName "hydra.inference", ModuleName "hydra.print.core"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just
              ("Idempotence tests for type inference: infer(infer(e)) == infer(e). A curated corpus"
                ++ " covering lambdas, let-polymorphism, primitives, and phantom type variables --"
                ++ " the term shapes most likely to exercise the re-inference cases"
                ++ " (inferTypeOfTypeApplication, inferTypeOfTypeLambda) named in #611."))}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition testGroupForLambdas,
      Phantoms.toDefinition testGroupForLet,
      Phantoms.toDefinition testGroupForPrimitives]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
  Phantoms.doc "The group of all inference idempotence tests" $
  supergroup "Idempotence" [
    testGroupForLambdas,
    testGroupForLet,
    testGroupForPrimitives]

testGroupForLambdas :: TypedTermDefinition TestGroup
testGroupForLambdas = define "testGroupForLambdas" $
  subgroup "Lambdas" [
    idempotenceCase "#1: identity" []
      (lambda "x" $ var "x"),
    idempotenceCase "#2: nested lambdas" []
      (lambda "x" $ lambda "y" $ primitive DefMath.add @@ var "x" @@ var "y"),
    idempotenceCase "#3: shadowing" []
      (lambda "x" $ lambda "x" $ primitive DefMath.add @@ var "x" @@ int32 42)]

testGroupForLet :: TypedTermDefinition TestGroup
testGroupForLet = define "testGroupForLet" $
  subgroup "Let terms" [
    idempotenceCase "#1: let-polymorphism" []
      (lets [
        "id">: lambda "x" $ var "x"]
        $ pair (var "id" @@ int32 42) (var "id" @@ string "foo")),
    idempotenceCase "#2: nested let-polymorphism" []
      (lets [
        "sng">: lambda "x" $ list [var "x"]]
        $ lets [
          "foo">: var "sng" @@ int32 42,
          "bar">: var "sng" @@ string "bar",
          "quux">: lambda "x" $ var "sng" @@ var "x"]
          $ pair (var "foo") (pair (var "bar") (var "quux" @@ list []))),
    idempotenceCase "#3: recursive let" []
      (lets [
        "f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x")]
        $ var "f")]

testGroupForPrimitives :: TypedTermDefinition TestGroup
testGroupForPrimitives = define "testGroupForPrimitives" $
  subgroup "Primitives" [
    idempotenceCase "#1: polymorphic primitive, bare" []
      (primitive DefLists.concat),
    idempotenceCase "#2: polymorphic primitive, applied" []
      (lambda "lists" (primitive DefLists.length @@ (primitive DefLists.concat @@ var "lists"))),
    -- Phantom type variables: a monomorphic primitive result with an unconstrained slot in the
    -- primitive's own polymorphic signature. See Fundamentals.hs "Phantom type variables" for context.
    idempotenceCase "#3: phantom type variable" []
      (primitive DefEithers.isLeft @@ left (int32 42)),
    idempotenceCase "#4: conditional over polymorphic branches" []
      (lambda "b" $ lambda "x" $ lambda "y" $
        primitive DefLogic.ifElse @@ var "b" @@ (list [var "x"]) @@ (list [var "y"]))]
