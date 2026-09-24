-- | Shared utility functions for test code generation codecs.
-- These functions handle type inference on test group terms and are used by
-- both the Java and Python test codec modules.

module Hydra.Sources.Test.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import qualified Hydra.Core.Dsl.Lib.Strings                as Strings
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Core.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Core.Dsl.Paths                  as Paths
import qualified Hydra.Core.Dsl.Ast                        as Ast
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base                       as MetaBase
import qualified Hydra.Core.Dsl.Coders                     as Coders
import qualified Hydra.Core.Dsl.Util                    as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core                       as Core
import qualified Hydra.Core.Dsl.Errors                     as Error
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Core.Dsl.Json.Model                       as Json
import qualified Hydra.Core.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality               as Equality
import qualified Hydra.Core.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Core.Dsl.Lib.Literals               as Literals
import qualified Hydra.Core.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Core.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Core.Dsl.Lib.Math                   as Math
import qualified Hydra.Core.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Core.Dsl.Packaging                     as Packaging
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Core.Dsl.Topology                   as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Core.Dsl.Typing                     as Typing
import qualified Hydra.Core.Dsl.Util                       as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types                           as Types
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
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Errors    as PrintError
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants      as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
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


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_


ns :: ModuleName
ns = ModuleName "hydra.core.test.utils"


module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Inference.ns, PrintError.ns, Lexical.ns] L.++ KernelTypes.kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Shared utility functions for test code generation codecs")}
  where
    definitions = [
      toDefinition inferTerm,
      toDefinition inferTestCase,
      toDefinition inferTestGroupTerms]


-- | Run type inference on a single term
inferTerm :: TypedTermDefinition (Graph -> Term -> Either String Term)
inferTerm = define "inferTerm" $
  doc "Run type inference on a single term" $
  lambda "g" $ lambda "term" $
    Eithers.bimap
      ("e" ~> PrintError.error_ @@ var "e")
      ("x" ~> Typing.inferenceResultTerm (var "x"))
      (Inference.inferInGraphContext @@ asTerm Lexical.emptyInferenceContext @@ var "g" @@ var "term")

-- | Run type inference on the terms in a test case
inferTestCase :: TypedTermDefinition (Graph -> TestCaseWithMetadata -> Either String TestCaseWithMetadata)
inferTestCase = define "inferTestCase" $
  doc "Run type inference on the terms in a test case" $
  lambda "g" $ lambda "tcm" $ lets [
    "name_">: project _TestCaseWithMetadata _TestCaseWithMetadata_name @@ var "tcm",
    "tcase">: project _TestCaseWithMetadata _TestCaseWithMetadata_case @@ var "tcm",
    "desc">: project _TestCaseWithMetadata _TestCaseWithMetadata_description @@ var "tcm",
    "tags_">: project _TestCaseWithMetadata _TestCaseWithMetadata_tags @@ var "tcm",
    "provisions_">: project _TestCaseWithMetadata _TestCaseWithMetadata_provisions @@ var "tcm"] $
    Eithers.map
      (lambda "inferredCase" $
        Testing.testCaseWithMetadataAndProvisions (var "name_") (var "inferredCase") (var "desc") (var "tags_") (var "provisions_"))
      (Phantoms.right (var "tcase"))


-- | Run type inference on all terms in a TestGroup
inferTestGroupTerms :: TypedTermDefinition (Graph -> TestGroup -> Either String TestGroup)
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

