-- Note: this is an automatically generated file. Do not edit.
-- | Evaluation-level implementations of List functions for the Hydra interpreter.

module Hydra.Eval.Lib.Lists where
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Reduction as Reduction
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Interpreter-friendly applicative apply for List terms.
apply :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
apply cx g funsTerm argsTerm =
    Eithers.bind (ExtractCore.list g funsTerm) (\funs -> Eithers.bind (ExtractCore.list g argsTerm) (\arguments ->
      let applyOne =
              \f -> Lists.map (\arg -> Core.TermApplication (Core.Application {
                Core.applicationFunction = f,
                Core.applicationArgument = arg})) arguments
      in (Right (Core.TermList (Lists.concat (Lists.map applyOne funs))))))
-- | Interpreter-friendly monadic bind for List terms.
bind :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
bind cx g listTerm funTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat")),
      Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = el})) elements))})))
-- | Interpreter-friendly concat2 for List terms.
concat2 :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
concat2 cx g list1 list2 =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat")),
      Core.applicationArgument = (Core.TermList [
        list1,
        list2])}))
-- | Interpreter-friendly dropWhile for List terms.
dropWhile :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
dropWhile cx g predTerm listTerm =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.span")),
          Core.applicationArgument = predTerm})),
        Core.applicationArgument = listTerm}))}))
-- | Interpreter-friendly elem for List terms.
elem :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
elem cx g x listTerm =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.isJust")),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.find")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.equal")),
            Core.applicationArgument = x}))})),
        Core.applicationArgument = listTerm}))}))
-- | Interpreter-friendly filter for List terms.
filter :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
filter cx g predTerm listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat")),
      Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = predTerm,
              Core.applicationArgument = el}))})),
          Core.applicationArgument = (Core.TermList (Lists.pure el))})),
        Core.applicationArgument = (Core.TermList [])})) elements))})))
-- | Interpreter-friendly find for List terms.
find :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
find cx g predTerm listTerm =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeHead")),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.filter")),
          Core.applicationArgument = predTerm})),
        Core.applicationArgument = listTerm}))}))
-- | Interpreter-friendly left fold for List terms.
foldl :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
foldl cx g funTerm initTerm listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Lists.foldl (\acc -> \el -> Eithers.bind acc (\reducedAcc -> Reduction.reduceTerm cx g True (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = reducedAcc})),
      Core.applicationArgument = el})))) (Right initTerm) elements)
-- | Interpreter-friendly right fold for List terms.
foldr :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
foldr cx g funTerm initTerm listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Lists.foldr (\el -> \acc -> Eithers.bind acc (\reducedAcc -> Reduction.reduceTerm cx g True (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = el})),
      Core.applicationArgument = reducedAcc})))) (Right initTerm) elements)
-- | Interpreter-friendly group for List terms.
group :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
group cx g listTerm =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "foldResult"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.null")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "foldResult"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "foldResult"))}))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "foldResult"))}))})),
            Core.applicationArgument = (Core.TermList [
              Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "foldResult"))})])}))}))})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
            Core.applicationArgument = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "acc"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "el"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                      Core.applicationArgument = (Core.TermPair (Core.TermList [
                        Core.TermVariable (Core.Name "el")], (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))))})),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "h"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.equal")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "el"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "h"))}))})),
                          Core.applicationArgument = (Core.TermPair (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))})),
                            Core.applicationArgument = (Core.TermList [
                              Core.TermVariable (Core.Name "el")])}), (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))))})),
                        Core.applicationArgument = (Core.TermPair (Core.TermList [
                          Core.TermVariable (Core.Name "el")], (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))})),
                          Core.applicationArgument = (Core.TermList [
                            Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))})])}))))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeHead")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))}))}))}))}))})),
          Core.applicationArgument = (Core.TermPair (Core.TermList [], (Core.TermList [])))})),
        Core.applicationArgument = listTerm}))}))
-- | Interpreter-friendly intercalate for List terms.
intercalate :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
intercalate cx g sep listsTerm =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat")),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intersperse")),
          Core.applicationArgument = sep})),
        Core.applicationArgument = listsTerm}))}))
-- | Interpreter-friendly intersperse for List terms.
intersperse :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
intersperse cx g sep listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Core.TermList (Maybes.maybe [] (\p -> Lists.cons (Pairs.first p) (Lists.concat (Lists.map (\el -> [
      sep,
      el]) (Pairs.second p)))) (Lists.uncons elements))))
-- | Interpreter-friendly map for List terms.
map :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
map cx g funTerm listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Core.TermList (Lists.reverse (Lists.foldl (\acc -> \el -> Lists.cons (Core.TermApplication (Core.Application {
      Core.applicationFunction = funTerm,
      Core.applicationArgument = el})) acc) [] elements))))
-- | Interpreter-friendly maybeHead for List terms.
maybeHead :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
maybeHead cx g listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Core.TermMaybe (Maybes.maybe Nothing (\p -> Just (Pairs.first p)) (Lists.uncons elements))))
-- | Interpreter-friendly nub for List terms.
nub :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
nub cx g listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "acc"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.elem")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermVariable (Core.Name "x")])}))}))}))}))})),
        Core.applicationArgument = (Core.TermList [])})),
      Core.applicationArgument = listTerm})))
-- | Interpreter-friendly partition for List terms.
partition :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
partition cx g predTerm listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements ->
      let initialState = Core.TermPair (Core.TermList [], (Core.TermList []))
          finalState =
                  Lists.foldl (\acc -> \el ->
                    let yeses =
                            Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                              Core.applicationArgument = acc})
                        nos =
                                Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                                  Core.applicationArgument = acc})
                    in (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = predTerm,
                            Core.applicationArgument = el}))})),
                        Core.applicationArgument = (Core.TermPair (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                            Core.applicationArgument = yeses})),
                          Core.applicationArgument = (Core.TermList [
                            el])}), nos))})),
                      Core.applicationArgument = (Core.TermPair (yeses, (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                          Core.applicationArgument = nos})),
                        Core.applicationArgument = (Core.TermList [
                          el])}))))}))) initialState elements
      in (Right finalState))
-- | Interpreter-friendly pure for List terms.
pure :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
pure cx g x = Right (Core.TermList [
  x])
-- | Interpreter-friendly replicate for List terms.
replicate :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
replicate cx g n x =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "_"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = x}))})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.range")),
          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
        Core.applicationArgument = n}))}))
-- | Interpreter-friendly singleton for List terms.
singleton :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
singleton cx g x = Right (Core.TermList [
  x])
-- | Interpreter-friendly sort for List terms.
sort :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
sort cx g listTerm =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sortOn")),
        Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.equality.identity"))})),
      Core.applicationArgument = listTerm}))
-- | Interpreter-friendly sortOn for List terms.
sortOn :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
sortOn cx g projTerm listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Lists.foldl (\sorted -> \x ->
      let splitResult =
              Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.span")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lte")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = projTerm,
                          Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = projTerm,
                        Core.applicationArgument = x}))}))}))})),
                Core.applicationArgument = sorted})
          before =
                  Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                    Core.applicationArgument = splitResult})
          after =
                  Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                    Core.applicationArgument = splitResult})
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
          Core.applicationArgument = before})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.cons")),
            Core.applicationArgument = x})),
          Core.applicationArgument = after}))}))) (Core.TermList []) elements))
-- | Interpreter-friendly span for List terms.
span :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
span cx g predTerm listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements ->
      let initialState =
              Core.TermPair (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean True), (Core.TermList [])), (Core.TermList []))
          finalState =
                  Lists.foldl (\acc -> \el ->
                    let takingLeft =
                            Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                              Core.applicationArgument = acc})
                        right =
                                Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                                  Core.applicationArgument = acc})
                        taking =
                                Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                                  Core.applicationArgument = takingLeft})
                        left =
                                Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                                  Core.applicationArgument = takingLeft})
                    in (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.and")),
                              Core.applicationArgument = taking})),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = predTerm,
                              Core.applicationArgument = el}))}))})),
                        Core.applicationArgument = (Core.TermPair (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean True), (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                            Core.applicationArgument = left})),
                          Core.applicationArgument = (Core.TermList [
                            el])}))), right))})),
                      Core.applicationArgument = (Core.TermPair (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean False), left), (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                          Core.applicationArgument = right})),
                        Core.applicationArgument = (Core.TermList [
                          el])}))))}))) initialState elements
      in (Right (Core.TermPair (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
          Core.applicationArgument = finalState}))}), (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
        Core.applicationArgument = finalState}))))))
-- | Interpreter-friendly uncons for List terms.
uncons :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
uncons cx g listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Core.TermMaybe (Maybes.maybe Nothing (\h -> Just (Core.TermPair (h, (Core.TermList (Lists.drop 1 elements))))) (Lists.maybeAt 0 elements))))
-- | Interpreter-friendly zipWith for List terms.
zipWith :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
zipWith cx g funTerm listTerm1 listTerm2 =
    Eithers.bind (ExtractCore.list g listTerm1) (\elements1 -> Eithers.bind (ExtractCore.list g listTerm2) (\elements2 -> Right (Core.TermList (Lists.map (\p ->
      let a = Pairs.first p
          b = Pairs.second p
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = funTerm,
          Core.applicationArgument = a})),
        Core.applicationArgument = b}))) (Lists.zip elements1 elements2)))))
