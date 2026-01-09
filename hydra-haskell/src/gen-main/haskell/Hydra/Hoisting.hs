-- Note: this is an automatically generated file. Do not edit.

-- | Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms.

module Hydra.Hoisting where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
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
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Check if a binding has a polymorphic type (non-empty list of type scheme variables)
bindingIsPolymorphic :: (Core.Binding -> Bool)
bindingIsPolymorphic binding = (Maybes.maybe False (\ts -> Logic.not (Lists.null (Core.typeSchemeVariables ts))) (Core.bindingType binding))

-- | Hoist case statements into local let bindings. This is useful for targets such as Python which only support case statements (match) at the top level. Case statements are hoisted only when they appear at non-top-level positions. Top level = root, or reachable through annotations, let body/binding, lambda bodies, or ONE application LHS. Once through an application LHS, lambda bodies no longer count as pass-through.
hoistCaseStatements :: (Typing.TypeContext -> Core.Term -> Core.Term)
hoistCaseStatements = (hoistSubterms shouldHoistCaseStatement)

hoistCaseStatementsInGraph :: (Graph.Graph -> Compute.Flow t0 Graph.Graph)
hoistCaseStatementsInGraph graph =  
  let emptyIx = Typing.InferenceContext {
          Typing.inferenceContextSchemaTypes = M.empty,
          Typing.inferenceContextPrimitiveTypes = M.empty,
          Typing.inferenceContextDataTypes = M.empty,
          Typing.inferenceContextClassConstraints = M.empty,
          Typing.inferenceContextDebug = False}
  in  
    let emptyTx = Typing.TypeContext {
            Typing.typeContextTypes = Maps.empty,
            Typing.typeContextMetadata = Maps.empty,
            Typing.typeContextTypeVariables = Sets.empty,
            Typing.typeContextLambdaVariables = Sets.empty,
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

-- | Transform a let-term by pulling all polymorphic let bindings to the top level. This is useful to ensure that polymorphic bindings are not nested within other terms, which is unsupported by certain targets such as Java. Polymorphic bindings are those with a non-empty list of type scheme variables. If a hoisted binding captures lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first.
hoistPolymorphicLetBindings :: (Core.Let -> Core.Let)
hoistPolymorphicLetBindings let0 =  
  let topLevelNames = (Sets.fromList (Lists.map (\b -> Core.bindingName b) (Core.letBindings let0)))
  in  
    let processTermForHoisting = (\lambdaVars -> \reserved -> \term -> (\x -> case x of
            Core.TermLet v1 ->  
              let bindingsResult = (Lists.foldl (processBinding lambdaVars) ([], (reserved, [])) (Core.letBindings v1))
              in  
                let hoistedFromBindings = (Pairs.first bindingsResult)
                in  
                  let reservedAfterBindings = (Pairs.first (Pairs.second bindingsResult))
                  in  
                    let keptBindingsRaw = (Lists.reverse (Pairs.second (Pairs.second bindingsResult)))
                    in  
                      let bodyResult = (processTermForHoisting lambdaVars reservedAfterBindings (Core.letBody v1))
                      in  
                        let hoistedFromBody = (Pairs.first bodyResult)
                        in  
                          let reservedAfterBody = (Pairs.first (Pairs.second bodyResult))
                          in  
                            let processedBodyRaw = (Pairs.second (Pairs.second bodyResult))
                            in  
                              let allHoisted = (Lists.concat2 hoistedFromBody hoistedFromBindings)
                              in  
                                let keptBindings = (Lists.map (\b ->  
                                        let updatedTerm = (replaceReferences hoistedFromBindings (Core.bindingTerm b))
                                        in Core.Binding {
                                          Core.bindingName = (Core.bindingName b),
                                          Core.bindingTerm = updatedTerm,
                                          Core.bindingType = (Core.bindingType b)}) keptBindingsRaw)
                                in  
                                  let processedBody = (replaceReferences hoistedFromBindings processedBodyRaw)
                                  in (Logic.ifElse (Lists.null keptBindings) (allHoisted, (reservedAfterBody, processedBody)) (allHoisted, (reservedAfterBody, (Core.TermLet (Core.Let {
                                    Core.letBindings = keptBindings,
                                    Core.letBody = processedBody})))))
            Core.TermFunction v1 -> ((\x -> case x of
              Core.FunctionLambda v2 ->  
                let paramName = (Core.lambdaParameter v2)
                in  
                  let paramDomain = (Core.lambdaDomain v2)
                  in  
                    let newLambdaVars = (Lists.concat2 lambdaVars (Lists.pure (paramName, paramDomain)))
                    in  
                      let bodyResult = (processTermForHoisting newLambdaVars reserved (Core.lambdaBody v2))
                      in  
                        let hoisted = (Pairs.first bodyResult)
                        in  
                          let newReserved = (Pairs.first (Pairs.second bodyResult))
                          in  
                            let newBody = (Pairs.second (Pairs.second bodyResult))
                            in (hoisted, (newReserved, (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = paramName,
                              Core.lambdaDomain = paramDomain,
                              Core.lambdaBody = newBody})))))
              _ -> ([], (reserved, term))) v1)
            Core.TermApplication v1 ->  
              let fnResult = (processTermForHoisting lambdaVars reserved (Core.applicationFunction v1))
              in  
                let fnHoisted = (Pairs.first fnResult)
                in  
                  let reservedAfterFn = (Pairs.first (Pairs.second fnResult))
                  in  
                    let newFn = (Pairs.second (Pairs.second fnResult))
                    in  
                      let argResult = (processTermForHoisting lambdaVars reservedAfterFn (Core.applicationArgument v1))
                      in  
                        let argHoisted = (Pairs.first argResult)
                        in  
                          let reservedAfterArg = (Pairs.first (Pairs.second argResult))
                          in  
                            let newArg = (Pairs.second (Pairs.second argResult))
                            in (Lists.concat2 argHoisted fnHoisted, (reservedAfterArg, (Core.TermApplication (Core.Application {
                              Core.applicationFunction = newFn,
                              Core.applicationArgument = newArg}))))
            Core.TermAnnotated v1 ->  
              let bodyResult = (processTermForHoisting lambdaVars reserved (Core.annotatedTermBody v1))
              in  
                let hoisted = (Pairs.first bodyResult)
                in  
                  let newReserved = (Pairs.first (Pairs.second bodyResult))
                  in  
                    let newBody = (Pairs.second (Pairs.second bodyResult))
                    in (hoisted, (newReserved, (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = newBody,
                      Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))))
            Core.TermTypeLambda v1 ->  
              let bodyResult = (processTermForHoisting lambdaVars reserved (Core.typeLambdaBody v1))
              in  
                let hoisted = (Pairs.first bodyResult)
                in  
                  let newReserved = (Pairs.first (Pairs.second bodyResult))
                  in  
                    let newBody = (Pairs.second (Pairs.second bodyResult))
                    in (hoisted, (newReserved, (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.typeLambdaParameter v1),
                      Core.typeLambdaBody = newBody}))))
            Core.TermTypeApplication v1 ->  
              let bodyResult = (processTermForHoisting lambdaVars reserved (Core.typeApplicationTermBody v1))
              in  
                let hoisted = (Pairs.first bodyResult)
                in  
                  let newReserved = (Pairs.first (Pairs.second bodyResult))
                  in  
                    let newBody = (Pairs.second (Pairs.second bodyResult))
                    in (hoisted, (newReserved, (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = newBody,
                      Core.typeApplicationTermType = (Core.typeApplicationTermType v1)}))))
            _ -> ([], (reserved, term))) term) 
        processBinding = (\lambdaVars -> \state -> \binding ->  
                let hoisted = (Pairs.first state)
                in  
                  let reserved = (Pairs.first (Pairs.second state))
                  in  
                    let kept = (Pairs.second (Pairs.second state))
                    in  
                      let processedTerm = (processTermForHoisting lambdaVars reserved (Core.bindingTerm binding))
                      in  
                        let innerHoisted = (Pairs.first processedTerm)
                        in  
                          let newReserved = (Pairs.first (Pairs.second processedTerm))
                          in  
                            let newTerm = (Pairs.second (Pairs.second processedTerm))
                            in (Logic.ifElse (bindingIsPolymorphic binding) ( 
                              let bindingName = (Core.bindingName binding)
                              in  
                                let freeVars = (Rewriting.freeVariablesInTerm newTerm)
                                in  
                                  let lambdaVarNames = (Sets.fromList (Lists.map (\lv -> Pairs.first lv) lambdaVars))
                                  in  
                                    let capturedVarNames = (Sets.intersection lambdaVarNames freeVars)
                                    in  
                                      let capturedVars = (Lists.filter (\lv -> Sets.member (Pairs.first lv) capturedVarNames) lambdaVars)
                                      in  
                                        let wrappedTerm = (Lists.foldl (\body -> \lv -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                                Core.lambdaParameter = (Pairs.first lv),
                                                Core.lambdaDomain = (Pairs.second lv),
                                                Core.lambdaBody = body}))) newTerm (Lists.reverse capturedVars))
                                        in  
                                          let wrappedType = (Maybes.maybe Nothing (\ts ->  
                                                  let origType = (Core.typeSchemeType ts)
                                                  in  
                                                    let newType = (Lists.foldl (\innerType -> \lv -> Maybes.maybe innerType (\domainType -> Core.TypeFunction (Core.FunctionType {
                                                            Core.functionTypeDomain = domainType,
                                                            Core.functionTypeCodomain = innerType})) (Pairs.second lv)) origType (Lists.reverse capturedVars))
                                                    in (Just (Core.TypeScheme {
                                                      Core.typeSchemeVariables = (Core.typeSchemeVariables ts),
                                                      Core.typeSchemeType = newType,
                                                      Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts)}))) (Core.bindingType binding))
                                          in  
                                            let hoistedInfo = (bindingName, (wrappedTerm, (Lists.map (\lv -> Pairs.first lv) capturedVars, wrappedType)))
                                            in (Lists.cons hoistedInfo (Lists.concat2 innerHoisted hoisted), (newReserved, kept))) ( 
                              let processedBinding = Core.Binding {
                                      Core.bindingName = (Core.bindingName binding),
                                      Core.bindingTerm = newTerm,
                                      Core.bindingType = (Core.bindingType binding)}
                              in (Lists.concat2 innerHoisted hoisted, (newReserved, (Lists.cons processedBinding kept))))))
        replaceReferences = (\hoistedInfoList -> \term -> Rewriting.rewriteTerm (\recurse -> \t -> (\x -> case x of
                Core.TermVariable v1 ->  
                  let matchingInfo = (Lists.filter (\info -> Equality.equal v1 (getInfoName info)) hoistedInfoList)
                  in (Logic.ifElse (Lists.null matchingInfo) t ( 
                    let info = (Lists.head matchingInfo)
                    in  
                      let capturedVars = (getInfoCaptured info)
                      in (Lists.foldl (\fn -> \capturedName -> Core.TermApplication (Core.Application {
                        Core.applicationFunction = fn,
                        Core.applicationArgument = (Core.TermVariable capturedName)})) (Core.TermVariable v1) capturedVars)))
                _ -> (recurse t)) t) term)
        getInfoName = (\info -> Pairs.first info)
        getInfoCaptured = (\info -> Pairs.first (Pairs.second (Pairs.second info)))
    in  
      let emptyLambdaVars = []
      in  
        let result = (Lists.foldl (processBinding emptyLambdaVars) ([], (topLevelNames, [])) (Core.letBindings let0))
        in  
          let hoistedInfo = (Lists.reverse (Pairs.first result))
          in  
            let reservedNames = (Pairs.first (Pairs.second result))
            in  
              let keptBindings = (Lists.reverse (Pairs.second (Pairs.second result)))
              in  
                let bodyResult = (processTermForHoisting emptyLambdaVars reservedNames (Core.letBody let0))
                in  
                  let hoistedFromBody = (Lists.reverse (Pairs.first bodyResult))
                  in  
                    let processedBody = (Pairs.second (Pairs.second bodyResult))
                    in  
                      let allHoistedInfo = (Lists.concat2 hoistedInfo hoistedFromBody)
                      in  
                        let hoistedBindings = (Lists.map (\info ->  
                                let name = (getInfoName info)
                                in  
                                  let wrappedTerm = (Pairs.first (Pairs.second info))
                                  in  
                                    let origType = (Pairs.second (Pairs.second (Pairs.second info)))
                                    in Core.Binding {
                                      Core.bindingName = name,
                                      Core.bindingTerm = wrappedTerm,
                                      Core.bindingType = origType}) allHoistedInfo)
                        in  
                          let allBindings = (Lists.concat2 hoistedBindings keptBindings)
                          in Core.Let {
                            Core.letBindings = allBindings,
                            Core.letBody = processedBody}

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

-- | Predicate for case statement hoisting. Returns True if term is a case statement AND not at top level. Top level = reachable through annotations, let body/binding, lambda bodies, or ONE app LHS. Once through an app LHS, lambda bodies no longer pass through.
shouldHoistCaseStatement :: (([Accessors.TermAccessor], Core.Term) -> Bool)
shouldHoistCaseStatement pathAndTerm =  
  let path = (Pairs.first pathAndTerm)
  in  
    let term = (Pairs.second pathAndTerm)
    in (Logic.ifElse (Logic.not (isUnionElimination term)) False ( 
      let finalState = (Lists.foldl (\st -> \acc -> updateHoistState acc st) (True, False) path)
      in (Logic.not (Pairs.first finalState))))

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
