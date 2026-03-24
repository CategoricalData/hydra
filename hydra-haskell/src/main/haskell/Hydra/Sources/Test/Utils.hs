-- | Shared utility functions for test code generation codecs.
-- These functions handle type inference on test group terms and are used by
-- both the Java and Python test codec modules.

module Hydra.Sources.Test.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                  as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                     as Error
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Errors    as ShowError
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_


ns :: Namespace
ns = Namespace "hydra.test.utils"


module_ :: Module
module_ = Module ns elements
    [Inference.ns, ShowError.ns, Lexical.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "Shared utility functions for test code generation codecs"
  where
    elements = [
      toTermDefinition inferTestGroupTerms,
      toTermDefinition inferTestCase,
      toTermDefinition inferTerm]


-- | Run type inference on all terms in a TestGroup
inferTestGroupTerms :: TBinding (Graph -> TestGroup -> Either String TestGroup)
inferTestGroupTerms = define "inferTestGroupTerms" $
  doc "Run type inference on all terms in a TestGroup to ensure lambdas have domain types" $
  lambda "g" $ lambda "tg" $ lets [
    "name_">: project _TestGroup _TestGroup_name @@ var "tg",
    "desc">: project _TestGroup _TestGroup_description @@ var "tg",
    "subgroups">: project _TestGroup _TestGroup_subgroups @@ var "tg",
    "cases_">: project _TestGroup _TestGroup_cases @@ var "tg"] $
    Eithers.bind
      (Eithers.mapList (lambda "sg" $ inferTestGroupTerms @@ var "g" @@ var "sg") (var "subgroups"))
      (lambda "inferredSubgroups" $
        Eithers.map
          (lambda "inferredCases" $
            Testing.testGroup (var "name_") (var "desc") (var "inferredSubgroups") (var "inferredCases"))
          (Eithers.mapList (lambda "tc" $ inferTestCase @@ var "g" @@ var "tc") (var "cases_")))


-- | Run type inference on the terms in a test case
inferTestCase :: TBinding (Graph -> TestCaseWithMetadata -> Either String TestCaseWithMetadata)
inferTestCase = define "inferTestCase" $
  doc "Run type inference on the terms in a test case" $
  lambda "g" $ lambda "tcm" $ lets [
    "name_">: project _TestCaseWithMetadata _TestCaseWithMetadata_name @@ var "tcm",
    "tcase">: project _TestCaseWithMetadata _TestCaseWithMetadata_case @@ var "tcm",
    "desc">: project _TestCaseWithMetadata _TestCaseWithMetadata_description @@ var "tcm",
    "tags_">: project _TestCaseWithMetadata _TestCaseWithMetadata_tags @@ var "tcm"] $
    Eithers.map
      (lambda "inferredCase" $
        Testing.testCaseWithMetadata (var "name_") (var "inferredCase") (var "desc") (var "tags_"))
      (cases _TestCase (var "tcase") (Just (Phantoms.right (var "tcase"))) [
        _TestCase_delegatedEvaluation>>: lambda "delCase" $ lets [
          "input_">: project _DelegatedEvaluationTestCase _DelegatedEvaluationTestCase_input @@ var "delCase",
          "output_">: project _DelegatedEvaluationTestCase _DelegatedEvaluationTestCase_output @@ var "delCase"] $
          Eithers.bind
            (inferTerm @@ var "g" @@ var "input_")
            (lambda "inferredInput" $
              Eithers.map
                (lambda "inferredOutput" $
                  inject _TestCase _TestCase_delegatedEvaluation
                    (record _DelegatedEvaluationTestCase [
                      _DelegatedEvaluationTestCase_input>>: var "inferredInput",
                      _DelegatedEvaluationTestCase_output>>: var "inferredOutput"]))
                (inferTerm @@ var "g" @@ var "output_"))])


-- | Run type inference on a single term
inferTerm :: TBinding (Graph -> Term -> Either String Term)
inferTerm = define "inferTerm" $
  doc "Run type inference on a single term" $
  lambda "g" $ lambda "term" $
    Eithers.bimap
      ("ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic"))
      ("x" ~> Typing.inferenceResultTerm (var "x"))
      (Inference.inferInGraphContext @@ asTerm Lexical.emptyContext @@ var "g" @@ var "term")
