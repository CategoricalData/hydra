-- Note: this is an automatically generated file. Do not edit.
-- | Default term-level implementations of Either functions for the Hydra interpreter.

module Hydra.Lib.Defaults.Eithers where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Interpreter-friendly case analysis for Either terms.
either :: t0 -> t1 -> Core.Term -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
either cx g leftFun rightFun eitherTerm =
    case eitherTerm of
      Core.TermEither v0 -> Right (Eithers.either (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = leftFun,
        Core.applicationArgument = val})) (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = rightFun,
        Core.applicationArgument = val})) v0)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "either value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term eitherTerm)})))
-- | Interpreter-friendly foldl for Either.
foldl :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
foldl cx g funTerm initTerm listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "err"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "a"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = funTerm,
              Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
            Core.applicationArgument = el}))}))})),
      Core.applicationArgument = acc})) (Core.TermEither (Right initTerm)) elements))
-- | Interpreter-friendly lefts for list of Either terms.
lefts :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
lefts cx g listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Core.TermList (Lists.foldl (\acc -> \el -> case el of
      Core.TermEither v0 -> Eithers.either (\val -> Lists.concat2 acc (Lists.pure val)) (\_ -> acc) v0
      _ -> acc) [] elements)))
-- | Interpreter-friendly mapList for Either (traverse).
mapList :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
mapList cx g funTerm listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "err"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "y"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "accErr"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "accErr"))))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "ys"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.cons")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "ys"))}))))}))})),
            Core.applicationArgument = acc}))}))})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = el}))})) (Core.TermEither (Right (Core.TermList []))) (Lists.reverse elements)))
-- | Interpreter-friendly mapMaybe for Either (traverse over Maybe).
mapMaybe :: t0 -> t1 -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
mapMaybe cx g funTerm maybeTerm =
    case maybeTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe (Core.TermEither (Right (Core.TermMaybe Nothing))) (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
            Core.applicationArgument = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "err"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "y"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermEither (Right (Core.TermMaybe (Just (Core.TermVariable (Core.Name "y"))))))}))})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = funTerm,
          Core.applicationArgument = val}))})) v0)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "maybe value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term maybeTerm)})))
-- | Interpreter-friendly mapSet for Either (traverse over Set).
mapSet :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
mapSet cx g funTerm setTerm =
    Eithers.bind (ExtractCore.set g setTerm) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "err"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "y"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "accErr"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "accErr"))))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "ys"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.insert")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "ys"))}))))}))})),
            Core.applicationArgument = acc}))}))})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = el}))})) (Core.TermEither (Right (Core.TermSet (Sets.fromList [])))) (Sets.toList elements)))
-- | Interpreter-friendly partitionEithers for list of Either terms.
partitionEithers :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error ([Core.Term], [Core.Term])
partitionEithers cx g listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Lists.foldl (\acc -> \el ->
      let ls = Pairs.first acc
          rs = Pairs.second acc
      in case el of
        Core.TermEither v0 -> Eithers.either (\val -> (Lists.concat2 ls (Lists.pure val), rs)) (\val -> (ls, (Lists.concat2 rs (Lists.pure val)))) v0
        _ -> acc) ([], []) elements))
-- | Interpreter-friendly rights for list of Either terms.
rights :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
rights cx g listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Core.TermList (Lists.foldl (\acc -> \el -> case el of
      Core.TermEither v0 -> Eithers.either (\_ -> acc) (\val -> Lists.concat2 acc (Lists.pure val)) v0
      _ -> acc) [] elements)))
