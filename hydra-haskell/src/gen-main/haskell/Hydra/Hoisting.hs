-- Note: this is an automatically generated file. Do not edit.

-- | Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms.

module Hydra.Hoisting where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Augment bindings with new free variables introduced by substitution, wrapping with lambdas after any type lambdas.
augmentBindingsWithNewFreeVars :: (Typing.TypeContext -> S.Set Core.Name -> [Core.Binding] -> ([Core.Binding], Typing.TermSubst))
augmentBindingsWithNewFreeVars cx boundVars bindings =  
  let types = (Typing.typeContextTypes cx)
  in  
    let wrapAfterTypeLambdas = (\vars -> \term -> (\x -> case x of
            Core.TermTypeLambda v1 -> (Core.TermTypeLambda (Core.TypeLambda {
              Core.typeLambdaParameter = (Core.typeLambdaParameter v1),
              Core.typeLambdaBody = (wrapAfterTypeLambdas vars (Core.typeLambdaBody v1))}))
            _ -> (Lists.foldl (\t -> \p -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Pairs.first p),
              Core.lambdaDomain = (Pairs.second p),
              Core.lambdaBody = t}))) term (Lists.reverse vars))) term)
    in  
      let augment = (\b ->  
              let freeVars = (Sets.toList (Sets.intersection boundVars (Rewriting.freeVariablesInTerm (Core.bindingTerm b))))
              in  
                let varTypePairs = (Lists.map (\v -> (v, (Maps.lookup v types))) freeVars)
                in  
                  let varTypes = (Maybes.cat (Lists.map Pairs.second varTypePairs))
                  in (Logic.ifElse (Logic.or (Lists.null freeVars) (Logic.not (Equality.equal (Lists.length varTypes) (Lists.length varTypePairs)))) (b, Nothing) (Core.Binding {
                    Core.bindingName = (Core.bindingName b),
                    Core.bindingTerm = (wrapAfterTypeLambdas varTypePairs (Core.bindingTerm b)),
                    Core.bindingType = (Maybes.map (\ts -> Core.TypeScheme {
                      Core.typeSchemeVariables = (Core.typeSchemeVariables ts),
                      Core.typeSchemeType = (Lists.foldl (\acc -> \t -> Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = t,
                        Core.functionTypeCodomain = acc})) (Core.typeSchemeType ts) (Lists.reverse varTypes)),
                      Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts)}) (Core.bindingType b))}, (Just (Core.bindingName b, (Lists.foldl (\t -> \v -> Core.TermApplication (Core.Application {
                    Core.applicationFunction = t,
                    Core.applicationArgument = (Core.TermVariable v)})) (Core.TermVariable (Core.bindingName b)) freeVars))))))
      in  
        let results = (Lists.map augment bindings)
        in (Lists.map Pairs.first results, (Typing.TermSubst (Maps.fromList (Maybes.cat (Lists.map Pairs.second results)))))

-- | Check if a binding has a polymorphic type (non-empty list of type scheme variables)
bindingIsPolymorphic :: (Core.Binding -> Bool)
bindingIsPolymorphic binding = (Maybes.maybe False (\ts -> Logic.not (Lists.null (Core.typeSchemeVariables ts))) (Core.bindingType binding))

-- | Check if a binding's type uses any type variables from the given TypeContext. Returns True if the free type variables in the binding's type intersect with the type variables in scope (typeContextTypeVariables).
bindingUsesContextTypeVars :: (Typing.TypeContext -> Core.Binding -> Bool)
bindingUsesContextTypeVars cx binding = (Maybes.maybe False (\ts ->  
  let freeInType = (Rewriting.freeVariablesInType (Core.typeSchemeType ts))
  in  
    let contextTypeVars = (Typing.typeContextTypeVariables cx)
    in (Logic.not (Sets.null (Sets.intersection freeInType contextTypeVars)))) (Core.bindingType binding))

-- | Count the number of occurrences of a variable name in a term. Assumes no variable shadowing.
countVarOccurrences :: (Core.Name -> Core.Term -> Int)
countVarOccurrences name term =  
  let childCount = (Lists.foldl (\acc -> \t -> Math.add acc (countVarOccurrences name t)) 0 (Rewriting.subterms term))
  in ((\x -> case x of
    Core.TermVariable v1 -> (Logic.ifElse (Equality.equal v1 name) (Math.add 1 childCount) childCount)
    _ -> childCount) term)

-- | Transform a let-term by pulling ALL let bindings to the top level. This is useful for targets like Java that don't support nested let expressions at all. If a hoisted binding captures lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first.
hoistAllLetBindings :: (Core.Let -> Core.Let)
hoistAllLetBindings let0 =  
  let emptyIx = Typing.InferenceContext {
          Typing.inferenceContextSchemaTypes = Maps.empty,
          Typing.inferenceContextPrimitiveTypes = Maps.empty,
          Typing.inferenceContextDataTypes = Maps.empty,
          Typing.inferenceContextClassConstraints = Maps.empty,
          Typing.inferenceContextDebug = False}
  in  
    let emptyCx = Typing.TypeContext {
            Typing.typeContextTypes = Maps.empty,
            Typing.typeContextMetadata = Maps.empty,
            Typing.typeContextTypeVariables = Sets.empty,
            Typing.typeContextLambdaVariables = Sets.empty,
            Typing.typeContextLetVariables = Sets.empty,
            Typing.typeContextInferenceContext = emptyIx}
    in (hoistLetBindingsWithPredicate (\_ -> True) shouldHoistAll emptyCx let0)

-- | Hoist case statements into local let bindings. This is useful for targets such as Python which only support case statements (match) at the top level. Case statements are hoisted only when they appear at non-top-level positions. Top level = root, or reachable through annotations, let body/binding, lambda bodies, or ONE application LHS. Once through an application LHS, lambda bodies no longer count as pass-through.
hoistCaseStatements :: (Typing.TypeContext -> Core.Term -> Core.Term)
hoistCaseStatements = (hoistSubterms shouldHoistCaseStatement)

hoistCaseStatementsInGraph :: (Graph.Graph -> Compute.Flow t0 Graph.Graph)
hoistCaseStatementsInGraph graph =  
  let emptyIx = Typing.InferenceContext {
          Typing.inferenceContextSchemaTypes = Maps.empty,
          Typing.inferenceContextPrimitiveTypes = Maps.empty,
          Typing.inferenceContextDataTypes = Maps.empty,
          Typing.inferenceContextClassConstraints = Maps.empty,
          Typing.inferenceContextDebug = False}
  in  
    let emptyTx = Typing.TypeContext {
            Typing.typeContextTypes = Maps.empty,
            Typing.typeContextMetadata = Maps.empty,
            Typing.typeContextTypeVariables = Sets.empty,
            Typing.typeContextLambdaVariables = Sets.empty,
            Typing.typeContextLetVariables = Sets.empty,
            Typing.typeContextInferenceContext = emptyIx}
    in  
      let gterm0 = (Schemas.graphAsTerm graph)
      in  
        let gterm1 = (hoistCaseStatements emptyTx gterm0)
        in  
          let newElements = (Schemas.termAsGraph gterm1)
          in (Flows.pure (Graph.Graph {
            Graph.graphElements = newElements,
            Graph.graphEnvironment = (Graph.graphEnvironment graph),
            Graph.graphTypes = (Graph.graphTypes graph),
            Graph.graphBody = (Graph.graphBody graph),
            Graph.graphPrimitives = (Graph.graphPrimitives graph),
            Graph.graphSchema = (Graph.graphSchema graph)}))

-- | Transform a let-term by pulling polymorphic let bindings to the top level, using TypeContext. A binding is hoisted if: (1) It is polymorphic (has non-empty typeSchemeVariables), OR (2) Its type uses type variables from the TypeContext (i.e., from enclosing type lambdas). Bindings which are already at the top level are not hoisted. If a hoisted binding captures lambda-bound or let-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. If a hoisted binding uses type variables from the context, those type variables are added to the binding's type scheme. Note: we assume that there is no variable shadowing; use hydra.rewriting.unshadowVariables first.
hoistLetBindingsWithContext :: ((Core.Binding -> Bool) -> Typing.TypeContext -> Core.Let -> Core.Let)
hoistLetBindingsWithContext isParentBinding cx let0 = (hoistLetBindingsWithPredicate isParentBinding shouldHoistPolymorphic cx let0)

-- | Transform a let-term by pulling let bindings to the top level. The isParentBinding predicate applies to top-level bindings and determines whether their subterm bindings are eligible for hoisting. The shouldHoistBinding predicate takes the TypeContext and a subterm binding, and returns True if the binding should be hoisted. This is useful for targets like Java that cannot have polymorphic definitions in arbitrary positions. The TypeContext provides information about type variables and lambda variables in scope. If a hoisted binding captures let-bound or lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. If a hoisted binding captures type variables from an enclosing type lambda scope, those type variables are added to the binding's type scheme, and references are replaced with type applications. Note: we assume that there is no variable shadowing; use hydra.rewriting.unshadowVariables first.
hoistLetBindingsWithPredicate :: ((Core.Binding -> Bool) -> (Typing.TypeContext -> Core.Binding -> Bool) -> Typing.TypeContext -> Core.Let -> Core.Let)
hoistLetBindingsWithPredicate isParentBinding shouldHoistBinding cx0 let0 =  
  let hoistOne = (\prefix -> \cx -> \pair -> \bindingWithCapturedVars ->  
          let bindingAndReplacementPairs = (Pairs.first pair)
          in  
            let alreadyUsedNames = (Pairs.second pair)
            in  
              let b = (Pairs.first bindingWithCapturedVars)
              in  
                let capturedTermVars = (Pairs.second bindingWithCapturedVars)
                in  
                  let types = (Typing.typeContextTypes cx)
                  in  
                    let capturedTermVarTypePairs = (Lists.map (\v -> (v, (Maps.lookup v types))) capturedTermVars)
                    in  
                      let capturedTypeVars = (Sets.toList (Sets.intersection (Typing.typeContextTypeVariables cx) (Maybes.maybe Sets.empty (\ts -> Rewriting.freeVariablesInType (Core.typeSchemeType ts)) (Core.bindingType b))))
                      in  
                        let globalBindingName = (Lexical.chooseUniqueName alreadyUsedNames (Core.Name (Strings.cat2 prefix (Core.unName (Core.bindingName b)))))
                        in  
                          let newUsedNames = (Sets.insert globalBindingName alreadyUsedNames)
                          in  
                            let capturedTermVarTypes = (Lists.map (\typ -> Rewriting.deannotateTypeParameters typ) (Maybes.cat (Lists.map Pairs.second capturedTermVarTypePairs)))
                            in  
                              let newTypeScheme = (Logic.ifElse (Equality.equal (Lists.length capturedTermVarTypes) (Lists.length capturedTermVarTypePairs)) (Maybes.map (\ts -> Core.TypeScheme {
                                      Core.typeSchemeVariables = (Lists.nub (Lists.concat2 capturedTypeVars (Core.typeSchemeVariables ts))),
                                      Core.typeSchemeType = (Lists.foldl (\t -> \a -> Core.TypeFunction (Core.FunctionType {
                                        Core.functionTypeDomain = a,
                                        Core.functionTypeCodomain = t})) (Core.typeSchemeType ts) (Lists.reverse capturedTermVarTypes)),
                                      Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts)}) (Core.bindingType b)) Nothing)
                              in  
                                let strippedTerm = (Rewriting.detypeTerm (Core.bindingTerm b))
                                in  
                                  let termWithLambdas = (Lists.foldl (\t -> \p -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                          Core.lambdaParameter = (Pairs.first p),
                                          Core.lambdaDomain = (Maybes.map (\dom -> Rewriting.deannotateTypeParameters dom) (Pairs.second p)),
                                          Core.lambdaBody = t}))) strippedTerm (Lists.reverse capturedTermVarTypePairs))
                                  in  
                                    let termWithTypeLambdas = (Lists.foldl (\t -> \v -> Core.TermTypeLambda (Core.TypeLambda {
                                            Core.typeLambdaParameter = v,
                                            Core.typeLambdaBody = t})) termWithLambdas (Lists.reverse (Maybes.maybe [] Core.typeSchemeVariables newTypeScheme)))
                                    in  
                                      let replacement = (Lists.foldl (\t -> \v -> Core.TermApplication (Core.Application {
                                              Core.applicationFunction = t,
                                              Core.applicationArgument = (Core.TermVariable v)})) (Core.TermVariable globalBindingName) capturedTermVars)
                                      in  
                                        let newBindingAndReplacement = (Core.Binding {
                                                Core.bindingName = globalBindingName,
                                                Core.bindingTerm = termWithTypeLambdas,
                                                Core.bindingType = newTypeScheme}, replacement)
                                        in  
                                          let newPairs = (Lists.cons newBindingAndReplacement bindingAndReplacementPairs)
                                          in (newPairs, newUsedNames))
  in  
    let rewrite = (\prefix -> \recurse -> \cx -> \bindingsAndNames -> \term ->  
            let previouslyFinishedBindings = (Pairs.first bindingsAndNames)
            in  
              let emptyBindingsAndNames = ([], (Pairs.second bindingsAndNames))
              in  
                let result = (recurse emptyBindingsAndNames term)
                in  
                  let newBindingsAndNames = (Pairs.first result)
                  in  
                    let bindingsSoFar = (Pairs.first newBindingsAndNames)
                    in  
                      let alreadyUsedNames = (Pairs.second newBindingsAndNames)
                      in  
                        let newTerm = (Pairs.second result)
                        in ((\x -> case x of
                          Core.TermLet v1 ->  
                            let body = (Core.letBody v1)
                            in  
                              let partitionPair = (Lists.partition (shouldHoistBinding cx) (Core.letBindings v1))
                              in  
                                let hoistUs = (Pairs.first partitionPair)
                                in  
                                  let keepUs = (Pairs.second partitionPair)
                                  in  
                                    let hoistedBindingNames = (Lists.map Core.bindingName hoistUs)
                                    in  
                                      let polyLetVariables = (Sets.fromList (Lists.filter (\v -> Maybes.maybe False Schemas.fTypeIsPolymorphic (Maps.lookup v (Typing.typeContextTypes cx))) (Sets.toList (Typing.typeContextLetVariables cx))))
                                      in  
                                        let boundTermVariables = (Sets.union (Typing.typeContextLambdaVariables cx) (Typing.typeContextLetVariables cx))
                                        in  
                                          let freeVariablesInEachBinding = (Lists.map (\b -> Sets.toList (Sets.intersection boundTermVariables (Rewriting.freeVariablesInTerm (Core.bindingTerm b)))) hoistUs)
                                          in  
                                            let bindingDependencies = (Lists.map (\vars -> Lists.partition (\v -> Sets.member v (Sets.fromList hoistedBindingNames)) vars) freeVariablesInEachBinding)
                                            in  
                                              let bindingEdges = (Lists.zip hoistedBindingNames (Lists.map Pairs.first bindingDependencies))
                                              in  
                                                let bindingImmediateCapturedVars = (Lists.zip hoistedBindingNames (Lists.map Pairs.second bindingDependencies))
                                                in  
                                                  let capturedVarsMap = (Maps.fromList (Sorting.propagateTags bindingEdges bindingImmediateCapturedVars))
                                                  in  
                                                    let bindingsWithCapturedVars = (Lists.map (\b -> (b, (Maybes.maybe [] (\vars -> Sets.toList (Sets.difference vars polyLetVariables)) (Maps.lookup (Core.bindingName b) capturedVarsMap)))) hoistUs)
                                                    in  
                                                      let hoistPairsAndNames = (Lists.foldl (hoistOne prefix cx) ([], alreadyUsedNames) bindingsWithCapturedVars)
                                                      in  
                                                        let hoistPairs = (Lists.reverse (Pairs.first hoistPairsAndNames))
                                                        in  
                                                          let hoistedBindings = (Lists.map Pairs.first hoistPairs)
                                                          in  
                                                            let replacements = (Lists.map Pairs.second hoistPairs)
                                                            in  
                                                              let finalUsedNames = (Pairs.second hoistPairsAndNames)
                                                              in  
                                                                let hoistNameReplacementPairs = (Lists.zip (Lists.map Core.bindingName hoistUs) replacements)
                                                                in  
                                                                  let hoistBindingMap = (Maps.fromList (Lists.map (\b -> (Core.bindingName b, b)) hoistUs))
                                                                  in  
                                                                    let isCacheable = (\name ->  
                                                                            let multiRef = (Equality.gte (countVarOccurrences name body) 2)
                                                                            in  
                                                                              let isPoly = (Maybes.maybe False (\b -> bindingIsPolymorphic b) (Maps.lookup name hoistBindingMap))
                                                                              in (Logic.and multiRef (Logic.not isPoly)))
                                                                    in  
                                                                      let singleRefPairs = (Lists.filter (\p -> Logic.not (isCacheable (Pairs.first p))) hoistNameReplacementPairs)
                                                                      in  
                                                                        let multiRefPairs = (Lists.filter (\p -> isCacheable (Pairs.first p)) hoistNameReplacementPairs)
                                                                        in  
                                                                          let fullSubst = (Typing.TermSubst (Maps.fromList hoistNameReplacementPairs))
                                                                          in  
                                                                            let bodyOnlySubst = (Typing.TermSubst (Maps.fromList singleRefPairs))
                                                                            in  
                                                                              let bodySubst = (Substitution.substituteInTerm bodyOnlySubst body)
                                                                              in  
                                                                                let cacheBindings = (Lists.map (\p -> Core.Binding {
                                                                                        Core.bindingName = (Pairs.first p),
                                                                                        Core.bindingTerm = (Pairs.second p),
                                                                                        Core.bindingType = Nothing}) multiRefPairs)
                                                                                in  
                                                                                  let bodyWithCache = (Logic.ifElse (Lists.null cacheBindings) bodySubst (Core.TermLet (Core.Let {
                                                                                          Core.letBindings = cacheBindings,
                                                                                          Core.letBody = bodySubst})))
                                                                                  in  
                                                                                    let keepUsSubst = (Lists.map (Substitution.substituteInBinding fullSubst) keepUs)
                                                                                    in  
                                                                                      let hoistedBindingsSubst = (Lists.map (Substitution.substituteInBinding fullSubst) hoistedBindings)
                                                                                      in  
                                                                                        let bindingsSoFarSubst = (Lists.map (Substitution.substituteInBinding fullSubst) bindingsSoFar)
                                                                                        in  
                                                                                          let augmentResult = (augmentBindingsWithNewFreeVars cx (Sets.difference boundTermVariables polyLetVariables) bindingsSoFarSubst)
                                                                                          in  
                                                                                            let bindingsSoFarAugmented = (Pairs.first augmentResult)
                                                                                            in  
                                                                                              let augmentSubst = (Pairs.second augmentResult)
                                                                                              in  
                                                                                                let hoistedBindingsFinal = (Lists.map (Substitution.substituteInBinding augmentSubst) hoistedBindingsSubst)
                                                                                                in  
                                                                                                  let bindingsSoFarFinal = (Lists.map (Substitution.substituteInBinding augmentSubst) bindingsSoFarAugmented)
                                                                                                  in  
                                                                                                    let bodyFinal = (Substitution.substituteInTerm augmentSubst bodyWithCache)
                                                                                                    in  
                                                                                                      let keepUsFinal = (Lists.map (Substitution.substituteInBinding augmentSubst) keepUsSubst)
                                                                                                      in  
                                                                                                        let finalTerm = (Logic.ifElse (Lists.null keepUsFinal) bodyFinal (Core.TermLet (Core.Let {
                                                                                                                Core.letBindings = keepUsFinal,
                                                                                                                Core.letBody = bodyFinal})))
                                                                                                        in ((Lists.concat [
                                                                                                          previouslyFinishedBindings,
                                                                                                          hoistedBindingsFinal,
                                                                                                          bindingsSoFarFinal], finalUsedNames), finalTerm)
                          _ -> ((Lists.concat2 previouslyFinishedBindings bindingsSoFar, alreadyUsedNames), newTerm)) newTerm))
    in  
      let cx1 = (Schemas.extendTypeContextForLet (\c -> \b -> Nothing) cx0 let0)
      in  
        let forActiveBinding = (\b ->  
                let prefix = (Strings.cat2 (Core.unName (Core.bindingName b)) "_")
                in  
                  let init = ([], (Sets.singleton (Core.bindingName b)))
                  in  
                    let resultPair = (rewriteAndFoldTermWithTypeContext (rewrite prefix) cx1 init (Core.bindingTerm b))
                    in  
                      let resultBindings = (Pairs.first (Pairs.first resultPair))
                      in  
                        let resultTerm = (Pairs.second resultPair)
                        in (Lists.cons (Core.Binding {
                          Core.bindingName = (Core.bindingName b),
                          Core.bindingTerm = resultTerm,
                          Core.bindingType = (Core.bindingType b)}) resultBindings))
        in  
          let forBinding = (\b -> Logic.ifElse (isParentBinding b) (forActiveBinding b) [
                  b])
          in Core.Let {
            Core.letBindings = (Lists.concat (Lists.map forBinding (Core.letBindings let0))),
            Core.letBody = (Core.letBody let0)}

-- | Transform a let-term by pulling all polymorphic let bindings to the top level. This is useful to ensure that polymorphic bindings are not nested within other terms, which is unsupported by certain targets such as Java. Polymorphic bindings are those with a non-empty list of type scheme variables. If a hoisted binding captures lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first.
hoistPolymorphicLetBindings :: ((Core.Binding -> Bool) -> Core.Let -> Core.Let)
hoistPolymorphicLetBindings isParentBinding let0 =  
  let emptyIx = Typing.InferenceContext {
          Typing.inferenceContextSchemaTypes = Maps.empty,
          Typing.inferenceContextPrimitiveTypes = Maps.empty,
          Typing.inferenceContextDataTypes = Maps.empty,
          Typing.inferenceContextClassConstraints = Maps.empty,
          Typing.inferenceContextDebug = False}
  in  
    let emptyCx = Typing.TypeContext {
            Typing.typeContextTypes = Maps.empty,
            Typing.typeContextMetadata = Maps.empty,
            Typing.typeContextTypeVariables = Sets.empty,
            Typing.typeContextLambdaVariables = Sets.empty,
            Typing.typeContextLetVariables = Sets.empty,
            Typing.typeContextInferenceContext = emptyIx}
    in (hoistLetBindingsWithPredicate isParentBinding shouldHoistPolymorphic emptyCx let0)

-- | Hoist subterms into local let bindings based on a path-aware predicate. The predicate receives a pair of (path, term) where path is the list of TermAccessors from the root to the current term, and returns True if the term should be hoisted. For each let term found, the immediate subterms (binding values and body) are processed: matching subterms within each immediate subterm are collected and hoisted into a local let that wraps that immediate subterm. If a hoisted term contains free variables that are lambda-bound at an enclosing scope, the hoisted binding is wrapped in lambdas for those variables, and the reference is replaced with an application of those variables.
hoistSubterms :: ((([Accessors.TermAccessor], Core.Term) -> Bool) -> Typing.TypeContext -> Core.Term -> Core.Term)
hoistSubterms shouldHoist cx0 term0 =  
  let processImmediateSubterm = (\cx -> \counter -> \namePrefix -> \subterm ->  
          let baselineLambdaVars = (Typing.typeContextLambdaVariables cx)
          in  
            let collectAndReplace = (\recurse -> \path -> \cxInner -> \acc -> \term ->  
                    let currentCounter = (Pairs.first acc)
                    in  
                      let collectedBindings = (Pairs.second acc)
                      in ((\x -> case x of
                        Core.TermLet _ -> (acc, term)
                        Core.TermTypeLambda _ -> (acc, term)
                        _ ->  
                          let result = (recurse acc term)
                          in  
                            let newAcc = (Pairs.first result)
                            in  
                              let processedTerm = (Pairs.second result)
                              in  
                                let newCounter = (Pairs.first newAcc)
                                in  
                                  let newBindings = (Pairs.second newAcc)
                                  in (Logic.ifElse (shouldHoist (path, processedTerm)) ( 
                                    let bindingName = (Core.Name (Strings.cat [
                                            "_hoist_",
                                            namePrefix,
                                            "_",
                                            (Literals.showInt32 newCounter)]))
                                    in  
                                      let allLambdaVars = (Typing.typeContextLambdaVariables cxInner)
                                      in  
                                        let newLambdaVars = (Sets.difference allLambdaVars baselineLambdaVars)
                                        in  
                                          let freeVars = (Rewriting.freeVariablesInTerm processedTerm)
                                          in  
                                            let capturedVars = (Sets.toList (Sets.intersection newLambdaVars freeVars))
                                            in  
                                              let wrappedTerm = (Lists.foldl (\body -> \varName -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                                      Core.lambdaParameter = varName,
                                                      Core.lambdaDomain = Nothing,
                                                      Core.lambdaBody = body}))) processedTerm (Lists.reverse capturedVars))
                                              in  
                                                let reference = (Lists.foldl (\fn -> \varName -> Core.TermApplication (Core.Application {
                                                        Core.applicationFunction = fn,
                                                        Core.applicationArgument = (Core.TermVariable varName)})) (Core.TermVariable bindingName) capturedVars)
                                                in  
                                                  let newBinding = Core.Binding {
                                                          Core.bindingName = bindingName,
                                                          Core.bindingTerm = wrappedTerm,
                                                          Core.bindingType = Nothing}
                                                  in ((Math.add newCounter 1, (Lists.cons newBinding newBindings)), reference)) (newAcc, processedTerm))) term))
            in  
              let result = (rewriteAndFoldTermWithTypeContextAndPath collectAndReplace cx (counter, []) subterm)
              in  
                let finalAcc = (Pairs.first result)
                in  
                  let transformedSubterm = (Pairs.second result)
                  in  
                    let finalCounter = (Pairs.first finalAcc)
                    in  
                      let bindings = (Pairs.second finalAcc)
                      in (Logic.ifElse (Lists.null bindings) (finalCounter, transformedSubterm) ( 
                        let localLet = (Core.TermLet (Core.Let {
                                Core.letBindings = (Lists.reverse bindings),
                                Core.letBody = transformedSubterm}))
                        in (finalCounter, localLet))))
  in  
    let processLetTerm = (\cx -> \counter -> \lt ->  
            let bindings = (Core.letBindings lt)
            in  
              let body = (Core.letBody lt)
              in  
                let processBinding = (\acc -> \binding ->  
                        let namePrefix = (Strings.intercalate "_" (Strings.splitOn "." (Core.unName (Core.bindingName binding))))
                        in  
                          let result = (processImmediateSubterm cx 1 namePrefix (Core.bindingTerm binding))
                          in  
                            let newValue = (Pairs.second result)
                            in  
                              let newBinding = Core.Binding {
                                      Core.bindingName = (Core.bindingName binding),
                                      Core.bindingTerm = newValue,
                                      Core.bindingType = (Core.bindingType binding)}
                              in (Lists.cons newBinding acc))
                in  
                  let newBindingsReversed = (Lists.foldl processBinding [] bindings)
                  in  
                    let newBindings = (Lists.reverse newBindingsReversed)
                    in  
                      let bodyResult = (processImmediateSubterm cx 1 "_body" body)
                      in  
                        let newBody = (Pairs.second bodyResult)
                        in (counter, (Core.TermLet (Core.Let {
                          Core.letBindings = newBindings,
                          Core.letBody = newBody}))))
    in  
      let rewrite = (\recurse -> \cx -> \counter -> \term -> (\x -> case x of
              Core.TermLet _ ->  
                let recursed = (recurse counter term)
                in  
                  let newCounter = (Pairs.first recursed)
                  in  
                    let recursedTerm = (Pairs.second recursed)
                    in ((\x -> case x of
                      Core.TermLet v2 -> (processLetTerm cx newCounter v2)
                      _ -> (newCounter, recursedTerm)) recursedTerm)
              _ -> (recurse counter term)) term)
      in (Pairs.second (rewriteAndFoldTermWithTypeContext rewrite cx0 1 term0))

isApplicationFunction :: (Accessors.TermAccessor -> Bool)
isApplicationFunction acc = ((\x -> case x of
  Accessors.TermAccessorApplicationFunction -> True
  _ -> False) acc)

-- | Check if a function is a union elimination
isEliminationUnion :: (Core.Function -> Bool)
isEliminationUnion f = ((\x -> case x of
  Core.FunctionElimination v1 -> ((\x -> case x of
    Core.EliminationUnion _ -> True
    _ -> False) v1)
  _ -> False) f)

isLambdaBody :: (Accessors.TermAccessor -> Bool)
isLambdaBody acc = ((\x -> case x of
  Accessors.TermAccessorLambdaBody -> True
  _ -> False) acc)

-- | Check if a term is a union elimination (case statement)
isUnionElimination :: (Core.Term -> Bool)
isUnionElimination term = ((\x -> case x of
  Core.TermFunction v1 -> (isEliminationUnion v1)
  _ -> False) term)

-- | Normalize a path for hoisting by treating immediately-applied lambdas as let bindings. Replaces [applicationFunction, lambdaBody, ...] with [letBody, ...].
normalizePathForHoisting :: ([Accessors.TermAccessor] -> [Accessors.TermAccessor])
normalizePathForHoisting path =  
  let go = (\remaining -> Logic.ifElse (Logic.or (Lists.null remaining) (Lists.null (Lists.tail remaining))) remaining ( 
          let first = (Lists.head remaining)
          in  
            let second = (Lists.head (Lists.tail remaining))
            in  
              let rest = (Lists.tail (Lists.tail remaining))
              in (Logic.ifElse (Logic.and (isApplicationFunction first) (isLambdaBody second)) (Lists.cons Accessors.TermAccessorLetBody (go rest)) (Lists.cons first (go (Lists.tail remaining))))))
  in (go path)

rewriteAndFoldTermWithTypeContext :: (((t0 -> Core.Term -> (t0, Core.Term)) -> Typing.TypeContext -> t0 -> Core.Term -> (t0, Core.Term)) -> Typing.TypeContext -> t0 -> Core.Term -> (t0, Core.Term))
rewriteAndFoldTermWithTypeContext f cx0 val0 term0 =  
  let wrapper = (\lowLevelRecurse -> \valAndCx -> \term ->  
          let val = (Pairs.first valAndCx)
          in  
            let cx = (Pairs.second valAndCx)
            in  
              let cx1 = ((\x -> case x of
                      Core.TermFunction v1 -> ((\x -> case x of
                        Core.FunctionLambda v2 -> (Schemas.extendTypeContextForLambda cx v2)
                        _ -> cx) v1)
                      Core.TermLet v1 -> (Schemas.extendTypeContextForLet (\_ -> \_ -> Nothing) cx v1)
                      Core.TermTypeLambda v1 -> (Schemas.extendTypeContextForTypeLambda cx v1)
                      _ -> cx) term)
              in  
                let recurseForUser = (\newVal -> \subterm ->  
                        let result = (lowLevelRecurse (newVal, cx1) subterm)
                        in (Pairs.first (Pairs.first result), (Pairs.second result)))
                in  
                  let fResult = (f recurseForUser cx1 val term)
                  in ((Pairs.first fResult, cx), (Pairs.second fResult)))
  in  
    let result = (Rewriting.rewriteAndFoldTerm wrapper (val0, cx0) term0)
    in (Pairs.first (Pairs.first result), (Pairs.second result))

rewriteAndFoldTermWithTypeContextAndPath :: (((t0 -> Core.Term -> (t0, Core.Term)) -> [Accessors.TermAccessor] -> Typing.TypeContext -> t0 -> Core.Term -> (t0, Core.Term)) -> Typing.TypeContext -> t0 -> Core.Term -> (t0, Core.Term))
rewriteAndFoldTermWithTypeContextAndPath f cx0 val0 term0 =  
  let wrapper = (\recurse -> \path -> \cxAndVal -> \term ->  
          let cx = (Pairs.first cxAndVal)
          in  
            let val = (Pairs.second cxAndVal)
            in  
              let cx1 = ((\x -> case x of
                      Core.TermFunction v1 -> ((\x -> case x of
                        Core.FunctionLambda v2 -> (Schemas.extendTypeContextForLambda cx v2)
                        _ -> cx) v1)
                      Core.TermLet v1 -> (Schemas.extendTypeContextForLet (\_ -> \_ -> Nothing) cx v1)
                      Core.TermTypeLambda v1 -> (Schemas.extendTypeContextForTypeLambda cx v1)
                      _ -> cx) term)
              in  
                let recurseForUser = (\valIn -> \termIn ->  
                        let result = (recurse path (cx1, valIn) termIn)
                        in (Pairs.second (Pairs.first result), (Pairs.second result)))
                in  
                  let fResult = (f recurseForUser path cx1 val term)
                  in ((cx, (Pairs.first fResult)), (Pairs.second fResult)))
  in  
    let result = (Rewriting.rewriteAndFoldTermWithPath wrapper (cx0, val0) term0)
    in (Pairs.second (Pairs.first result), (Pairs.second result))

rewriteTermWithTypeContext :: (((Core.Term -> t0) -> Typing.TypeContext -> Core.Term -> t0) -> Typing.TypeContext -> Core.Term -> t0)
rewriteTermWithTypeContext f cx0 term0 =  
  let f2 = (\recurse -> \cx -> \term ->  
          let recurse1 = (\term -> recurse cx term)
          in ((\x -> case x of
            Core.TermFunction v1 -> ((\x -> case x of
              Core.FunctionLambda v2 ->  
                let cx1 = (Schemas.extendTypeContextForLambda cx v2)
                in  
                  let recurse2 = (\term -> recurse cx1 term)
                  in (f recurse2 cx1 term)
              _ -> (f recurse1 cx term)) v1)
            Core.TermLet v1 ->  
              let cx1 = (Schemas.extendTypeContextForLet (\_ -> \_ -> Nothing) cx v1)
              in  
                let recurse2 = (\term -> recurse cx1 term)
                in (f recurse2 cx1 term)
            Core.TermTypeLambda v1 ->  
              let cx1 = (Schemas.extendTypeContextForTypeLambda cx v1)
              in  
                let recurse2 = (\term -> recurse cx1 term)
                in (f recurse2 cx1 term)
            _ -> (f recurse1 cx term)) term))
  in  
    let rewrite = (\cx -> \term -> f2 rewrite cx term)
    in (rewrite cx0 term0)

shouldHoistAll :: (t0 -> t1 -> Bool)
shouldHoistAll _ _ = True

-- | Predicate for case statement hoisting. Returns True if term is a case statement AND not at top level. Top level = reachable through annotations, let body/binding, lambda bodies, or ONE app LHS. Once through an app LHS, lambda bodies no longer pass through.
shouldHoistCaseStatement :: (([Accessors.TermAccessor], Core.Term) -> Bool)
shouldHoistCaseStatement pathAndTerm =  
  let path = (Pairs.first pathAndTerm)
  in  
    let term = (Pairs.second pathAndTerm)
    in (Logic.ifElse (Logic.not (isUnionElimination term)) False ( 
      let finalState = (Lists.foldl (\st -> \acc -> updateHoistState acc st) (True, False) path)
      in (Logic.not (Pairs.first finalState))))

-- | Predicate for hoisting polymorphic bindings. Returns True if the binding is polymorphic (has type scheme variables) or if its type uses any type variables from the TypeContext.
shouldHoistPolymorphic :: (Typing.TypeContext -> Core.Binding -> Bool)
shouldHoistPolymorphic cx binding = (Logic.or (bindingIsPolymorphic binding) (bindingUsesContextTypeVars cx binding))

-- | Update hoisting state when traversing an accessor. State is (atTopLevel, usedAppLHS). Returns updated state.
updateHoistState :: (Accessors.TermAccessor -> (Bool, Bool) -> (Bool, Bool))
updateHoistState accessor state =  
  let atTop = (Pairs.first state)
  in  
    let usedApp = (Pairs.second state)
    in (Logic.ifElse (Logic.not atTop) (False, usedApp) ((\x -> case x of
      Accessors.TermAccessorAnnotatedBody -> (True, usedApp)
      Accessors.TermAccessorLetBody -> (True, usedApp)
      Accessors.TermAccessorLetBinding _ -> (True, usedApp)
      Accessors.TermAccessorLambdaBody -> (Logic.ifElse usedApp (False, True) (True, False))
      Accessors.TermAccessorUnionCasesBranch _ -> (Logic.ifElse usedApp (False, True) (True, False))
      Accessors.TermAccessorUnionCasesDefault -> (Logic.ifElse usedApp (False, True) (True, False))
      Accessors.TermAccessorApplicationFunction -> (Logic.ifElse usedApp (False, True) (True, True))
      Accessors.TermAccessorApplicationArgument -> (False, usedApp)
      _ -> (False, usedApp)) accessor))
