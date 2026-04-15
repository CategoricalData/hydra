-- Note: this is an automatically generated file. Do not edit.

-- | Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms.

module Hydra.Hoisting where

import qualified Hydra.Core as Core
import qualified Hydra.Environment as Environment
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Paths as Paths
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Strip as Strip
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Set as S

-- | Augment bindings with new free variables introduced by substitution, wrapping with lambdas after any type lambdas.
augmentBindingsWithNewFreeVars :: Graph.Graph -> S.Set Core.Name -> [Core.Binding] -> ([Core.Binding], Typing.TermSubst)
augmentBindingsWithNewFreeVars cx boundVars bindings =

      let types = Maps.map Scoping.typeSchemeToFType (Graph.graphBoundTypes cx)
          wrapAfterTypeLambdas =
                  \vars -> \term -> case term of
                    Core.TermTypeLambda v0 -> Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
                      Core.typeLambdaBody = (wrapAfterTypeLambdas vars (Core.typeLambdaBody v0))})
                    _ -> Lists.foldl (\t -> \p -> Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Pairs.first p),
                      Core.lambdaDomain = (Pairs.second p),
                      Core.lambdaBody = t})) term (Lists.reverse vars)
          augment =
                  \b ->
                    let freeVars = Sets.toList (Sets.intersection boundVars (Variables.freeVariablesInTerm (Core.bindingTerm b)))
                        varTypePairs = Lists.map (\v -> (v, (Maps.lookup v types))) freeVars
                        varTypes = Maybes.cat (Lists.map Pairs.second varTypePairs)
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
                      Core.applicationArgument = (Core.TermVariable v)})) (Core.TermVariable (Core.bindingName b)) freeVars)))))
          results = Lists.map augment bindings
      in (Lists.map Pairs.first results, (Typing.TermSubst (Maps.fromList (Maybes.cat (Lists.map Pairs.second results)))))

-- | Check if a binding has a polymorphic type (non-empty list of type scheme variables)
bindingIsPolymorphic :: Core.Binding -> Bool
bindingIsPolymorphic binding =
    Maybes.maybe False (\ts -> Logic.not (Lists.null (Core.typeSchemeVariables ts))) (Core.bindingType binding)

-- | Check if a binding's type uses any type variables from the given Graph. Returns True if the free type variables in the binding's type intersect with the type variables in scope (graphTypeVariables).
bindingUsesContextTypeVars :: Graph.Graph -> Core.Binding -> Bool
bindingUsesContextTypeVars cx binding =
    Maybes.maybe False (\ts ->
      let freeInType = Variables.freeVariablesInType (Core.typeSchemeType ts)
          contextTypeVars = Graph.graphTypeVariables cx
      in (Logic.not (Sets.null (Sets.intersection freeInType contextTypeVars)))) (Core.bindingType binding)

-- | Count the number of occurrences of a variable name in a term. Assumes no variable shadowing.
countVarOccurrences :: Core.Name -> Core.Term -> Int
countVarOccurrences name term =

      let childCount = Lists.foldl (\acc -> \t -> Math.add acc (countVarOccurrences name t)) 0 (Rewriting.subterms term)
      in case term of
        Core.TermVariable v0 -> Logic.ifElse (Equality.equal v0 name) (Math.add 1 childCount) childCount
        _ -> childCount

-- | Transform a let-term by pulling ALL let bindings to the top level. This is useful for targets like Java that don't support nested let expressions at all. If a hoisted binding captures lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first.
hoistAllLetBindings :: Core.Let -> Core.Let
hoistAllLetBindings let0 =

      let emptyCx =
              Graph.Graph {
                Graph.graphBoundTerms = Maps.empty,
                Graph.graphBoundTypes = Maps.empty,
                Graph.graphClassConstraints = Maps.empty,
                Graph.graphLambdaVariables = Sets.empty,
                Graph.graphMetadata = Maps.empty,
                Graph.graphPrimitives = Maps.empty,
                Graph.graphSchemaTypes = Maps.empty,
                Graph.graphTypeVariables = Sets.empty}
      in (hoistLetBindingsWithPredicate (\_ -> True) shouldHoistAll emptyCx let0)

-- | Hoist case statements into local let bindings. This is useful for targets such as Python which only support case statements (match) at the top level. Case statements are hoisted only when they appear at non-top-level positions. Top level = root, or reachable through annotations, let body/binding, lambda bodies, or ONE application LHS. Once through an application LHS, lambda bodies no longer count as pass-through.
hoistCaseStatements :: Graph.Graph -> Core.Term -> Core.Term
hoistCaseStatements = hoistSubterms shouldHoistCaseStatement

-- | Hoist case statements into local let bindings for a list of bindings. This version operates prior to inference and uses an empty type context. It hoists case statements and their applied arguments into let bindings.
hoistCaseStatementsInGraph :: [Core.Binding] -> [Core.Binding]
hoistCaseStatementsInGraph bindings =

      let emptyTx =
              Graph.Graph {
                Graph.graphBoundTerms = Maps.empty,
                Graph.graphBoundTypes = Maps.empty,
                Graph.graphClassConstraints = Maps.empty,
                Graph.graphLambdaVariables = Sets.empty,
                Graph.graphMetadata = Maps.empty,
                Graph.graphPrimitives = Maps.empty,
                Graph.graphSchemaTypes = Maps.empty,
                Graph.graphTypeVariables = Sets.empty}
          term0 =
                  Core.TermLet (Core.Let {
                    Core.letBindings = bindings,
                    Core.letBody = Core.TermUnit})
          term1 = hoistCaseStatements emptyTx term0
      in (Environment.termAsBindings term1)

-- | Transform a let-term by pulling polymorphic let bindings to the top level, using Graph. A binding is hoisted if: (1) It is polymorphic (has non-empty typeSchemeVariables), OR (2) Its type uses type variables from the Graph (i.e., from enclosing type lambdas). Bindings which are already at the top level are not hoisted. If a hoisted binding captures lambda-bound or let-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. If a hoisted binding uses type variables from the context, those type variables are added to the binding's type scheme. Note: we assume that there is no variable shadowing; use hydra.rewriting.unshadowVariables first.
hoistLetBindingsWithContext :: (Core.Binding -> Bool) -> Graph.Graph -> Core.Let -> Core.Let
hoistLetBindingsWithContext isParentBinding cx let0 =
    hoistLetBindingsWithPredicate isParentBinding shouldHoistPolymorphic cx let0

-- | Transform a let-term by pulling let bindings to the top level. The isParentBinding predicate applies to top-level bindings and determines whether their subterm bindings are eligible for hoisting. The shouldHoistBinding predicate takes the Graph and a subterm binding, and returns True if the binding should be hoisted. This is useful for targets like Java that cannot have polymorphic definitions in arbitrary positions. The Graph provides information about type variables and lambda variables in scope. If a hoisted binding captures let-bound or lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. If a hoisted binding captures type variables from an enclosing type lambda scope, those type variables are added to the binding's type scheme, and references are replaced with type applications. Note: we assume that there is no variable shadowing; use hydra.rewriting.unshadowVariables first.
hoistLetBindingsWithPredicate :: (Core.Binding -> Bool) -> (Graph.Graph -> Core.Binding -> Bool) -> Graph.Graph -> Core.Let -> Core.Let
hoistLetBindingsWithPredicate isParentBinding shouldHoistBinding cx0 let0 =

      let hoistOne =
              \prefix -> \cx -> \pair -> \bindingWithCapturedVars ->
                let bindingAndReplacementPairs = Pairs.first pair
                    alreadyUsedNames = Pairs.second pair
                    b = Pairs.first bindingWithCapturedVars
                    capturedTermVars = Pairs.second bindingWithCapturedVars
                    types = Maps.map Scoping.typeSchemeToFType (Graph.graphBoundTypes cx)
                    capturedTermVarTypePairs = Lists.map (\v -> (v, (Maps.lookup v types))) capturedTermVars
                    capturedTermVarTypes =
                            Lists.map (\typ -> Strip.deannotateTypeParameters typ) (Maybes.cat (Lists.map Pairs.second capturedTermVarTypePairs))
                    freeInBindingType =
                            Maybes.maybe Sets.empty (\ts -> Variables.freeVariablesInType (Core.typeSchemeType ts)) (Core.bindingType b)
                    freeInCapturedVarTypes = Sets.unions (Lists.map (\t -> Variables.freeVariablesInType t) capturedTermVarTypes)
                    capturedTypeVars =
                            Sets.toList (Sets.intersection (Graph.graphTypeVariables cx) (Sets.union freeInBindingType freeInCapturedVarTypes))
                    globalBindingName =
                            Lexical.chooseUniqueName alreadyUsedNames (Core.Name (Strings.cat2 prefix (Core.unName (Core.bindingName b))))
                    newUsedNames = Sets.insert globalBindingName alreadyUsedNames
                    newTypeScheme =
                            Logic.ifElse (Equality.equal (Lists.length capturedTermVarTypes) (Lists.length capturedTermVarTypePairs)) (Maybes.map (\ts -> Core.TypeScheme {
                              Core.typeSchemeVariables = (Lists.nub (Lists.concat2 capturedTypeVars (Core.typeSchemeVariables ts))),
                              Core.typeSchemeType = (Lists.foldl (\t -> \a -> Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = a,
                                Core.functionTypeCodomain = t})) (Core.typeSchemeType ts) (Lists.reverse capturedTermVarTypes)),
                              Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts)}) (Core.bindingType b)) Nothing
                    strippedTerm = Strip.stripTypeLambdas (Core.bindingTerm b)
                    termWithLambdas =
                            Lists.foldl (\t -> \p -> Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Pairs.first p),
                              Core.lambdaDomain = (Maybes.map (\dom -> Strip.deannotateTypeParameters dom) (Pairs.second p)),
                              Core.lambdaBody = t})) strippedTerm (Lists.reverse capturedTermVarTypePairs)
                    termWithTypeLambdas =
                            Lists.foldl (\t -> \v -> Core.TermTypeLambda (Core.TypeLambda {
                              Core.typeLambdaParameter = v,
                              Core.typeLambdaBody = t})) termWithLambdas (Lists.reverse (Maybes.maybe [] Core.typeSchemeVariables newTypeScheme))
                    withTypeApps =
                            Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = t,
                              Core.typeApplicationTermType = (Core.TypeVariable v)})) (Core.TermVariable globalBindingName) capturedTypeVars
                    replacement =
                            Lists.foldl (\t -> \v -> Core.TermApplication (Core.Application {
                              Core.applicationFunction = t,
                              Core.applicationArgument = (Core.TermVariable v)})) withTypeApps capturedTermVars
                    newBindingAndReplacement =
                            (Core.Binding {
                              Core.bindingName = globalBindingName,
                              Core.bindingTerm = termWithTypeLambdas,
                              Core.bindingType = newTypeScheme}, replacement)
                    newPairs = Lists.cons newBindingAndReplacement bindingAndReplacementPairs
                in (newPairs, newUsedNames)
          rewrite =
                  \prefix -> \recurse -> \cx -> \bindingsAndNames -> \term ->
                    let previouslyFinishedBindings = Pairs.first bindingsAndNames
                        emptyBindingsAndNames = ([], (Pairs.second bindingsAndNames))
                        result = recurse emptyBindingsAndNames term
                        newBindingsAndNames = Pairs.first result
                        bindingsSoFar = Pairs.first newBindingsAndNames
                        alreadyUsedNames = Pairs.second newBindingsAndNames
                        newTerm = Pairs.second result
                    in case newTerm of
                      Core.TermLet v0 ->
                        let body = Core.letBody v0
                            partitionPair = Lists.partition (shouldHoistBinding cx) (Core.letBindings v0)
                            hoistUs = Pairs.first partitionPair
                            keepUs = Pairs.second partitionPair
                            hoistedBindingNames = Lists.map Core.bindingName hoistUs
                            polyLetVariables =
                                    Sets.fromList (Lists.filter (\v -> Maybes.maybe False Resolution.fTypeIsPolymorphic (Maybes.map Scoping.typeSchemeToFType (Maps.lookup v (Graph.graphBoundTypes cx)))) (Sets.toList (Sets.difference (Sets.fromList (Maps.keys (Graph.graphBoundTerms cx))) (Graph.graphLambdaVariables cx))))
                            boundTermVariables =
                                    Sets.union (Graph.graphLambdaVariables cx) (Sets.difference (Sets.fromList (Maps.keys (Graph.graphBoundTerms cx))) (Graph.graphLambdaVariables cx))
                            freeVariablesInEachBinding =
                                    Lists.map (\b -> Sets.toList (Sets.intersection boundTermVariables (Variables.freeVariablesInTerm (Core.bindingTerm b)))) hoistUs
                            bindingDependencies =
                                    Lists.map (\vars -> Lists.partition (\v -> Sets.member v (Sets.fromList hoistedBindingNames)) vars) freeVariablesInEachBinding
                            bindingEdges = Lists.zip hoistedBindingNames (Lists.map Pairs.first bindingDependencies)
                            bindingImmediateCapturedVars = Lists.zip hoistedBindingNames (Lists.map Pairs.second bindingDependencies)
                            capturedVarsMap = Maps.fromList (Sorting.propagateTags bindingEdges bindingImmediateCapturedVars)
                            bindingsWithCapturedVars =
                                    Lists.map (\b -> (b, (Maybes.maybe [] (\vars -> Sets.toList (Sets.difference vars polyLetVariables)) (Maps.lookup (Core.bindingName b) capturedVarsMap)))) hoistUs
                            hoistPairsAndNames = Lists.foldl (hoistOne prefix cx) ([], alreadyUsedNames) bindingsWithCapturedVars
                            hoistPairs = Lists.reverse (Pairs.first hoistPairsAndNames)
                            hoistedBindings = Lists.map Pairs.first hoistPairs
                            replacements = Lists.map Pairs.second hoistPairs
                            finalUsedNames = Pairs.second hoistPairsAndNames
                            hoistNameReplacementPairs = Lists.zip (Lists.map Core.bindingName hoistUs) replacements
                            hoistBindingMap = Maps.fromList (Lists.map (\b -> (Core.bindingName b, b)) hoistUs)
                            isCacheable =
                                    \name ->
                                      let multiRef = Equality.gte (countVarOccurrences name body) 2
                                          isPoly = Maybes.maybe False (\b -> bindingIsPolymorphic b) (Maps.lookup name hoistBindingMap)
                                      in (Logic.and multiRef (Logic.not isPoly))
                            singleRefPairs = Lists.filter (\p -> Logic.not (isCacheable (Pairs.first p))) hoistNameReplacementPairs
                            multiRefPairs = Lists.filter (\p -> isCacheable (Pairs.first p)) hoistNameReplacementPairs
                            fullSubst = Typing.TermSubst (Maps.fromList hoistNameReplacementPairs)
                            bodyOnlySubst = Typing.TermSubst (Maps.fromList singleRefPairs)
                            bodySubst = Substitution.substituteInTerm bodyOnlySubst body
                            cacheBindings =
                                    Lists.map (\p ->
                                      let origType = Maybes.maybe Nothing (\b -> Core.bindingType b) (Maps.lookup (Pairs.first p) hoistBindingMap)
                                      in Core.Binding {
                                        Core.bindingName = (Pairs.first p),
                                        Core.bindingTerm = (Pairs.second p),
                                        Core.bindingType = origType}) multiRefPairs
                            bodyWithCache =
                                    Logic.ifElse (Lists.null cacheBindings) bodySubst (Core.TermLet (Core.Let {
                                      Core.letBindings = cacheBindings,
                                      Core.letBody = bodySubst}))
                            keepUsSubst = Lists.map (Substitution.substituteInBinding fullSubst) keepUs
                            hoistedBindingsSubst = Lists.map (Substitution.substituteInBinding fullSubst) hoistedBindings
                            bindingsSoFarSubst = Lists.map (Substitution.substituteInBinding fullSubst) bindingsSoFar
                            augmentResult = augmentBindingsWithNewFreeVars cx (Sets.difference boundTermVariables polyLetVariables) bindingsSoFarSubst
                            bindingsSoFarAugmented = Pairs.first augmentResult
                            augmentSubst = Pairs.second augmentResult
                            hoistedBindingsFinal = Lists.map (Substitution.substituteInBinding augmentSubst) hoistedBindingsSubst
                            bindingsSoFarFinal = Lists.map (Substitution.substituteInBinding augmentSubst) bindingsSoFarAugmented
                            bodyFinal = Substitution.substituteInTerm augmentSubst bodyWithCache
                            keepUsFinal = Lists.map (Substitution.substituteInBinding augmentSubst) keepUsSubst
                            finalTerm =
                                    Logic.ifElse (Lists.null keepUsFinal) bodyFinal (Core.TermLet (Core.Let {
                                      Core.letBindings = keepUsFinal,
                                      Core.letBody = bodyFinal}))
                        in ((Lists.concat [
                          previouslyFinishedBindings,
                          hoistedBindingsFinal,
                          bindingsSoFarFinal], finalUsedNames), finalTerm)
                      _ -> ((Lists.concat2 previouslyFinishedBindings bindingsSoFar, alreadyUsedNames), newTerm)
          cx1 = Scoping.extendGraphForLet (\c -> \b -> Nothing) cx0 let0
          forActiveBinding =
                  \b ->
                    let prefix = Strings.cat2 (Core.unName (Core.bindingName b)) "_"
                        init = ([], (Sets.singleton (Core.bindingName b)))
                        resultPair = Rewriting.rewriteAndFoldTermWithGraph (rewrite prefix) cx1 init (Core.bindingTerm b)
                        resultBindings = Pairs.first (Pairs.first resultPair)
                        resultTerm = Pairs.second resultPair
                    in (Lists.cons (Core.Binding {
                      Core.bindingName = (Core.bindingName b),
                      Core.bindingTerm = resultTerm,
                      Core.bindingType = (Core.bindingType b)}) resultBindings)
          forBinding = \b -> Logic.ifElse (isParentBinding b) (forActiveBinding b) [
                b]
      in Core.Let {
        Core.letBindings = (Lists.concat (Lists.map forBinding (Core.letBindings let0))),
        Core.letBody = (Core.letBody let0)}

-- | Transform a let-term by pulling all polymorphic let bindings to the top level. This is useful to ensure that polymorphic bindings are not nested within other terms, which is unsupported by certain targets such as Java. Polymorphic bindings are those with a non-empty list of type scheme variables. If a hoisted binding captures lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first.
hoistPolymorphicLetBindings :: (Core.Binding -> Bool) -> Core.Let -> Core.Let
hoistPolymorphicLetBindings isParentBinding let0 =

      let emptyCx =
              Graph.Graph {
                Graph.graphBoundTerms = Maps.empty,
                Graph.graphBoundTypes = Maps.empty,
                Graph.graphClassConstraints = Maps.empty,
                Graph.graphLambdaVariables = Sets.empty,
                Graph.graphMetadata = Maps.empty,
                Graph.graphPrimitives = Maps.empty,
                Graph.graphSchemaTypes = Maps.empty,
                Graph.graphTypeVariables = Sets.empty}
      in (hoistLetBindingsWithPredicate isParentBinding shouldHoistPolymorphic emptyCx let0)

-- | Hoist subterms into local let bindings based on a path-aware predicate. The predicate receives a pair of (path, term) where path is the list of SubtermSteps from the root to the current term, and returns True if the term should be hoisted. For each let term found, the immediate subterms (binding values and body) are processed: matching subterms within each immediate subterm are collected and hoisted into a local let that wraps that immediate subterm. If a hoisted term contains free variables that are lambda-bound at an enclosing scope, the hoisted binding is wrapped in lambdas for those variables, and the reference is replaced with an application of those variables.
hoistSubterms :: (([Paths.SubtermStep], Core.Term) -> Bool) -> Graph.Graph -> Core.Term -> Core.Term
hoistSubterms shouldHoist cx0 term0 =

      let processImmediateSubterm =
              \cx -> \counter -> \namePrefix -> \pathPrefix -> \subterm ->
                let baselineLambdaVars = Graph.graphLambdaVariables cx
                    collectAndReplace =
                            \recurse -> \path -> \cxInner -> \acc -> \term ->
                              let currentCounter = Pairs.first acc
                                  collectedBindings = Pairs.second acc
                              in case term of
                                Core.TermLet _ -> (acc, term)
                                Core.TermTypeLambda _ -> (acc, term)
                                _ ->
                                  let result = recurse acc term
                                      newAcc = Pairs.first result
                                      processedTerm = Pairs.second result
                                      newCounter = Pairs.first newAcc
                                      newBindings = Pairs.second newAcc
                                      fullPath = Lists.concat2 pathPrefix path
                                  in (Logic.ifElse (shouldHoist (fullPath, processedTerm)) (
                                    let proposedName =
                                            Core.Name (Strings.cat [
                                              "_hoist_",
                                              namePrefix,
                                              "_",
                                              (Literals.showInt32 newCounter)])
                                        existingNames = Sets.fromList (Lists.map (\b -> Core.bindingName b) newBindings)
                                        freeVarsInSubterm = Variables.freeVariablesInTerm subterm
                                        allReserved = Sets.union existingNames freeVarsInSubterm
                                        bindingName = Lexical.chooseUniqueName allReserved proposedName
                                        allLambdaVars = Graph.graphLambdaVariables cxInner
                                        newLambdaVars = Sets.difference allLambdaVars baselineLambdaVars
                                        freeVars = Variables.freeVariablesInTerm processedTerm
                                        capturedVars = Sets.toList (Sets.intersection newLambdaVars freeVars)
                                        typeMap = Maps.map Scoping.typeSchemeToFType (Graph.graphBoundTypes cxInner)
                                        wrappedTerm =
                                                Lists.foldl (\body -> \varName -> Core.TermLambda (Core.Lambda {
                                                  Core.lambdaParameter = varName,
                                                  Core.lambdaDomain = (Maps.lookup varName typeMap),
                                                  Core.lambdaBody = body})) processedTerm (Lists.reverse capturedVars)
                                        reference =
                                                Lists.foldl (\fn -> \varName -> Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = fn,
                                                  Core.applicationArgument = (Core.TermVariable varName)})) (Core.TermVariable bindingName) capturedVars
                                        newBinding =
                                                Core.Binding {
                                                  Core.bindingName = bindingName,
                                                  Core.bindingTerm = wrappedTerm,
                                                  Core.bindingType = Nothing}
                                    in ((Math.add newCounter 1, (Lists.cons newBinding newBindings)), reference)) (newAcc, processedTerm))
                    result = Rewriting.rewriteAndFoldTermWithGraphAndPath collectAndReplace cx (counter, []) subterm
                    finalAcc = Pairs.first result
                    transformedSubterm = Pairs.second result
                    finalCounter = Pairs.first finalAcc
                    bindings = Pairs.second finalAcc
                in (Logic.ifElse (Lists.null bindings) (finalCounter, transformedSubterm) (
                  let localLet =
                          Core.TermLet (Core.Let {
                            Core.letBindings = (Lists.reverse bindings),
                            Core.letBody = transformedSubterm})
                  in (finalCounter, localLet)))
          processLetTerm =
                  \cx -> \counter -> \path -> \lt ->
                    let bindings = Core.letBindings lt
                        body = Core.letBody lt
                        processBinding =
                                \acc -> \binding ->
                                  let namePrefix = Strings.intercalate "_" (Strings.splitOn "." (Core.unName (Core.bindingName binding)))
                                      bindingPathPrefix = Lists.concat2 path [
                                            Paths.SubtermStepLetBinding (Core.bindingName binding)]
                                      result = processImmediateSubterm cx 1 namePrefix bindingPathPrefix (Core.bindingTerm binding)
                                      newValue = Pairs.second result
                                      newBinding =
                                              Core.Binding {
                                                Core.bindingName = (Core.bindingName binding),
                                                Core.bindingTerm = newValue,
                                                Core.bindingType = (Core.bindingType binding)}
                                  in (Lists.cons newBinding acc)
                        newBindingsReversed = Lists.foldl processBinding [] bindings
                        newBindings = Lists.reverse newBindingsReversed
                        bodyPathPrefix = Lists.concat2 path [
                              Paths.SubtermStepLetBody]
                        firstBindingName =
                                Maybes.maybe "body" (\b -> Strings.intercalate "_" (Strings.splitOn "." (Core.unName (Core.bindingName b)))) (Lists.maybeHead bindings)
                        bodyPrefix = Strings.cat2 firstBindingName "_body"
                        bodyResult = processImmediateSubterm cx 1 bodyPrefix bodyPathPrefix body
                        newBody = Pairs.second bodyResult
                    in (counter, (Core.TermLet (Core.Let {
                      Core.letBindings = newBindings,
                      Core.letBody = newBody})))
          rewrite =
                  \recurse -> \path -> \cx -> \counter -> \term -> case term of
                    Core.TermLet _ ->
                      let recursed = recurse counter term
                          newCounter = Pairs.first recursed
                          recursedTerm = Pairs.second recursed
                      in case recursedTerm of
                        Core.TermLet v1 -> processLetTerm cx newCounter path v1
                        _ -> (newCounter, recursedTerm)
                    _ -> recurse counter term
      in (Pairs.second (Rewriting.rewriteAndFoldTermWithGraphAndPath rewrite cx0 1 term0))

isApplicationFunction :: Paths.SubtermStep -> Bool
isApplicationFunction acc =
    case acc of
      Paths.SubtermStepApplicationFunction -> True
      _ -> False

isLambdaBody :: Paths.SubtermStep -> Bool
isLambdaBody acc =
    case acc of
      Paths.SubtermStepLambdaBody -> True
      _ -> False

-- | Check if a term is a union elimination (case statement)
isUnionElimination :: Core.Term -> Bool
isUnionElimination term =
    case term of
      Core.TermCases _ -> True
      _ -> False

-- | Check if a term is an application of a union elimination (case statement applied to an argument)
isUnionEliminationApplication :: Core.Term -> Bool
isUnionEliminationApplication term =
    case term of
      Core.TermApplication v0 -> isUnionElimination (Strip.deannotateAndDetypeTerm (Core.applicationFunction v0))
      _ -> False

-- | Normalize a path for hoisting by treating immediately-applied lambdas as let bindings. Replaces [applicationFunction, lambdaBody, ...] with [letBody, ...].
normalizePathForHoisting :: [Paths.SubtermStep] -> [Paths.SubtermStep]
normalizePathForHoisting path =

      let go =
              \remaining -> Maybes.maybe remaining (\uc1 ->
                let first = Pairs.first uc1
                    afterFirst = Pairs.second uc1
                in (Maybes.maybe remaining (\uc2 ->
                  let second = Pairs.first uc2
                      rest = Pairs.second uc2
                  in (Logic.ifElse (Logic.and (isApplicationFunction first) (isLambdaBody second)) (Lists.cons Paths.SubtermStepLetBody (go rest)) (Lists.cons first (go afterFirst)))) (Lists.uncons afterFirst))) (Lists.uncons remaining)
      in (go path)

-- | Predicate that always returns True, for hoisting all bindings unconditionally.
shouldHoistAll :: t0 -> t1 -> Bool
shouldHoistAll _ _2 = True

-- | Predicate for case statement hoisting. Returns True if term is a union elimination (bare case function) or a case statement application (union elimination applied to an argument) AND not at top level. Top level = reachable through annotations, let body/binding, lambda bodies, or ONE app LHS. Once through an app LHS, lambda bodies no longer pass through.
shouldHoistCaseStatement :: ([Paths.SubtermStep], Core.Term) -> Bool
shouldHoistCaseStatement pathAndTerm =

      let path = Pairs.first pathAndTerm
          term = Pairs.second pathAndTerm
      in (Logic.ifElse (Logic.not (Logic.or (isUnionElimination term) (isUnionEliminationApplication term))) False (
        let finalState = Lists.foldl (\st -> \acc -> updateHoistState acc st) (True, False) path
        in (Logic.not (Pairs.first finalState))))

-- | Predicate for hoisting polymorphic bindings. Returns True if the binding is polymorphic (has type scheme variables) or if its type uses any type variables from the Graph.
shouldHoistPolymorphic :: Graph.Graph -> Core.Binding -> Bool
shouldHoistPolymorphic cx binding = Logic.or (bindingIsPolymorphic binding) (bindingUsesContextTypeVars cx binding)

-- | Update hoisting state when traversing an accessor. State is (atTopLevel, usedAppLHS). Returns updated state.
updateHoistState :: Paths.SubtermStep -> (Bool, Bool) -> (Bool, Bool)
updateHoistState accessor state =

      let atTop = Pairs.first state
          usedApp = Pairs.second state
      in (Logic.ifElse (Logic.not atTop) (False, usedApp) (case accessor of
        Paths.SubtermStepAnnotatedBody -> (True, usedApp)
        Paths.SubtermStepLetBody -> (True, usedApp)
        Paths.SubtermStepLetBinding _ -> (True, usedApp)
        Paths.SubtermStepLambdaBody -> Logic.ifElse usedApp (False, True) (True, False)
        Paths.SubtermStepUnionCasesBranch _ -> Logic.ifElse usedApp (False, True) (True, False)
        Paths.SubtermStepUnionCasesDefault -> Logic.ifElse usedApp (False, True) (True, False)
        Paths.SubtermStepApplicationFunction -> Logic.ifElse usedApp (False, True) (True, True)
        Paths.SubtermStepApplicationArgument -> (False, usedApp)
        _ -> (False, usedApp)))
