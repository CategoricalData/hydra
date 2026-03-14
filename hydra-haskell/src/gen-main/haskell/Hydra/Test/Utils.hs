-- Note: this is an automatically generated file. Do not edit.

-- | Shared utility functions for test code generation codecs

module Hydra.Test.Utils where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Testing as Testing
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Run type inference on all terms in a TestGroup to ensure lambdas have domain types
inferTestGroupTerms :: (Graph.Graph -> Testing.TestGroup -> Either String Testing.TestGroup)
inferTestGroupTerms g tg =  
  let name_ = (Testing.testGroupName tg) 
      desc = (Testing.testGroupDescription tg)
      subgroups = (Testing.testGroupSubgroups tg)
      cases_ = (Testing.testGroupCases tg)
  in (Eithers.bind (Eithers.mapList (\sg -> inferTestGroupTerms g sg) subgroups) (\inferredSubgroups -> Eithers.map (\inferredCases -> Testing.TestGroup {
    Testing.testGroupName = name_,
    Testing.testGroupDescription = desc,
    Testing.testGroupSubgroups = inferredSubgroups,
    Testing.testGroupCases = inferredCases}) (Eithers.mapList (\tc -> inferTestCase g tc) cases_)))

-- | Run type inference on the terms in a test case
inferTestCase :: (Graph.Graph -> Testing.TestCaseWithMetadata -> Either String Testing.TestCaseWithMetadata)
inferTestCase g tcm =  
  let name_ = (Testing.testCaseWithMetadataName tcm) 
      tcase = (Testing.testCaseWithMetadataCase tcm)
      desc = (Testing.testCaseWithMetadataDescription tcm)
      tags_ = (Testing.testCaseWithMetadataTags tcm)
  in (Eithers.map (\inferredCase -> Testing.TestCaseWithMetadata {
    Testing.testCaseWithMetadataName = name_,
    Testing.testCaseWithMetadataCase = inferredCase,
    Testing.testCaseWithMetadataDescription = desc,
    Testing.testCaseWithMetadataTags = tags_}) ((\x -> case x of
    Testing.TestCaseDelegatedEvaluation v0 ->  
      let input_ = (Testing.delegatedEvaluationTestCaseInput v0) 
          output_ = (Testing.delegatedEvaluationTestCaseOutput v0)
      in (Eithers.bind (inferTerm g input_) (\inferredInput -> Eithers.map (\inferredOutput -> Testing.TestCaseDelegatedEvaluation (Testing.DelegatedEvaluationTestCase {
        Testing.delegatedEvaluationTestCaseInput = inferredInput,
        Testing.delegatedEvaluationTestCaseOutput = inferredOutput})) (inferTerm g output_)))
    _ -> (Right tcase)) tcase))

-- | Run type inference on a single term
inferTerm :: (Graph.Graph -> Core.Term -> Either String Core.Term)
inferTerm g term = (Eithers.bimap (\ic -> Error.unOtherError (Context.inContextObject ic)) (\x -> Typing.inferenceResultTerm x) (Inference.inferInGraphContext (Context.Context {
  Context.contextTrace = [],
  Context.contextMessages = [],
  Context.contextOther = Maps.empty}) g term))
