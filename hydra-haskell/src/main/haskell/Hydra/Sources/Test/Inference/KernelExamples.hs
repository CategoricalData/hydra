module Hydra.Sources.Test.Inference.KernelExamples (kernelExamplesTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Phantoms as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Test.TestGraph
import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T
import Hydra.Sources.Test.Inference.Fundamentals

import qualified Data.Map as M
import Prelude hiding (map, sum)


kernelExamplesTests :: TTerm TestGroup
kernelExamplesTests = supergroup "Examples from the Hydra kernel" [
  testGroupForNestedLet]

testGroupForNestedLet :: TTerm TestGroup
testGroupForNestedLet = supergroup "Nested let" [
    subgroup "hydra.formatting.mapFirstLetter" [
      expectMono 1 [tag_disabledForMinimalInference]
        (lambda "mapping" $ lambda "s" $ lets [
          "firstLetter">: var "mapping" @@ (primitive _strings_fromList @@ (primitive _lists_pure @@ (primitive _lists_head @@ var "list"))),
          "list">: primitive _strings_toList @@ var "s"] $
          primitive _logic_ifElse
            @@ (primitive _strings_null @@ var "s")
            @@ (var "s")
            @@ (primitive _strings_cat2 @@ var "firstLetter" @@ (primitive _strings_fromList @@ (primitive _lists_tail @@ var "list"))))
        (T.functionMany [T.function T.string T.string, T.string, T.string])]]
