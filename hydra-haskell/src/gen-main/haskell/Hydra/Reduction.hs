-- Note: this is an automatically generated file. Do not edit.

-- | Functions for reducing terms and types, i.e. performing computations.

module Hydra.Reduction where

import qualified Hydra.Arity as Arity
import qualified Hydra.Checking as Checking
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
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
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Errors as Errors_
import qualified Hydra.Strip as Strip
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Alpha convert a variable in a term
alphaConvert :: Core.Name -> Core.Name -> Core.Term -> Core.Term
alphaConvert vold vnew term = Variables.replaceFreeTermVariable vold (Core.TermVariable vnew) term

-- | Eagerly beta-reduce a type by substituting type arguments into type lambdas
betaReduceType :: Context.Context -> Graph.Graph -> Core.Type -> Either (Context.InContext Errors.Error) Core.Type
betaReduceType cx graph typ =

      let reduceApp =
              \app ->
                let lhs = Core.applicationTypeFunction app
                    rhs = Core.applicationTypeArgument app
                in case lhs of
                  Core.TypeAnnotated v0 -> Eithers.bind (reduceApp (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.annotatedTypeBody v0),
                    Core.applicationTypeArgument = rhs})) (\a -> Right (Core.TypeAnnotated (Core.AnnotatedType {
                    Core.annotatedTypeBody = a,
                    Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v0)})))
                  Core.TypeForall v0 -> betaReduceType cx graph (Variables.replaceFreeTypeVariable (Core.forallTypeParameter v0) rhs (Core.forallTypeBody v0))
                  Core.TypeVariable v0 -> Eithers.bind (Resolution.requireType cx graph v0) (\t_ -> betaReduceType cx graph (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = t_,
                    Core.applicationTypeArgument = rhs})))
          mapExpr =
                  \recurse -> \t ->
                    let findApp =
                            \r -> case r of
                              Core.TypeApplication v0 -> reduceApp v0
                              _ -> Right r
                    in (Eithers.bind (recurse t) (\r -> findApp r))
      in (Rewriting.rewriteTypeM mapExpr typ)

-- | Apply the special rules:
-- |     ((\x.e1) e2) == e1, where x does not appear free in e1
-- |   and
-- |      ((\x.e1) e2) = e1[x/e2]
-- | These are both limited forms of beta reduction which help to "clean up" a term without fully evaluating it.
contractTerm :: Core.Term -> Core.Term
contractTerm term =

      let rewrite =
              \recurse -> \t ->
                let rec = recurse t
                in case rec of
                  Core.TermApplication v0 ->
                    let lhs = Core.applicationFunction v0
                        rhs = Core.applicationArgument v0
                    in case (Strip.deannotateTerm lhs) of
                      Core.TermFunction v1 -> case v1 of
                        Core.FunctionLambda v2 ->
                          let v = Core.lambdaParameter v2
                              body = Core.lambdaBody v2
                          in (Logic.ifElse (Variables.isFreeVariableInTerm v body) body (Variables.replaceFreeTermVariable v rhs body))
                        _ -> rec
                      _ -> rec
                  _ -> rec
      in (Rewriting.rewriteTerm rewrite term)

countPrimitiveInvocations :: Bool
countPrimitiveInvocations = True

-- | Recursively transform terms to eliminate partial application, e.g. 'add 42' becomes '\x.add 42 x'. Uses the Graph to look up types for arity calculation. Bare primitives and variables are NOT expanded; eliminations and partial applications are. This version properly tracks the Graph through nested scopes.
etaExpandTerm :: Graph.Graph -> Core.Term -> Core.Term
etaExpandTerm tx0 term0 =

      let primTypes =
              Maps.fromList (Lists.map (\_gpt_p -> (Graph.primitiveName _gpt_p, (Graph.primitiveType _gpt_p))) (Maps.elems (Graph.graphPrimitives tx0)))
          termArityWithContext =
                  \tx -> \term -> case term of
                    Core.TermAnnotated v0 -> termArityWithContext tx (Core.annotatedTermBody v0)
                    Core.TermApplication v0 -> Math.sub (termArityWithContext tx (Core.applicationFunction v0)) 1
                    Core.TermFunction v0 -> case v0 of
                      Core.FunctionElimination _ -> 1
                      Core.FunctionLambda _ -> 0
                      Core.FunctionPrimitive v1 -> Maybes.maybe 0 Arity.typeSchemeArity (Maps.lookup v1 primTypes)
                    Core.TermLet v0 -> termArityWithContext (Scoping.extendGraphForLet (\_ -> \_2 -> Nothing) tx v0) (Core.letBody v0)
                    Core.TermTypeLambda v0 -> termArityWithContext (Scoping.extendGraphForTypeLambda tx v0) (Core.typeLambdaBody v0)
                    Core.TermTypeApplication v0 -> termArityWithContext tx (Core.typeApplicationTermBody v0)
                    Core.TermVariable v0 -> Maybes.maybe (Maybes.maybe 0 Arity.typeSchemeArity (Maps.lookup v0 primTypes)) Arity.typeArity (Maybes.map Scoping.typeSchemeToFType (Maps.lookup v0 (Graph.graphBoundTypes tx)))
                    _ -> 0
          domainTypes =
                  \n -> \mt -> Logic.ifElse (Equality.lte n 0) [] (Maybes.maybe (Lists.map (\_ -> Nothing) (Math.range 1 n)) (\typ -> case typ of
                    Core.TypeFunction v0 -> Lists.cons (Just (Core.functionTypeDomain v0)) (domainTypes (Math.sub n 1) (Just (Core.functionTypeCodomain v0)))
                    Core.TypeAnnotated v0 -> domainTypes n (Just (Core.annotatedTypeBody v0))
                    Core.TypeApplication v0 -> domainTypes n (Just (Core.applicationTypeFunction v0))
                    Core.TypeForall _ -> Lists.map (\_2 -> Nothing) (Math.range 1 n)
                    _ -> Lists.map (\_ -> Nothing) (Math.range 1 n)) mt)
          peelFunctionDomains =
                  \mtyp -> \n -> Logic.ifElse (Equality.lte n 0) mtyp (Maybes.maybe Nothing (\typ -> case typ of
                    Core.TypeFunction v0 -> peelFunctionDomains (Just (Core.functionTypeCodomain v0)) (Math.sub n 1)
                    Core.TypeAnnotated v0 -> peelFunctionDomains (Just (Core.annotatedTypeBody v0)) n
                    Core.TypeApplication v0 -> peelFunctionDomains (Just (Core.applicationTypeFunction v0)) n
                    Core.TypeForall _ -> Nothing
                    _ -> Nothing) mtyp)
          expand =
                  \alwaysPad -> \args -> \arity -> \headTyp -> \head ->
                    let applied =
                            Lists.foldl (\lhs -> \arg -> Core.TermApplication (Core.Application {
                              Core.applicationFunction = lhs,
                              Core.applicationArgument = arg})) head args
                        numArgs = Lists.length args
                        needed = Math.sub arity numArgs
                    in (Logic.ifElse (Logic.and (Equality.gt needed 0) (Logic.or alwaysPad (Equality.gt numArgs 0))) (
                      let indices = Math.range 1 needed
                          remainingType = peelFunctionDomains headTyp numArgs
                          domains = domainTypes needed remainingType
                          codomainType = peelFunctionDomains remainingType needed
                          fullyAppliedRaw =
                                  Lists.foldl (\body -> \i ->
                                    let vn = Core.Name (Strings.cat2 "v" (Literals.showInt32 i))
                                    in (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = body,
                                      Core.applicationArgument = (Core.TermVariable vn)}))) applied indices
                          fullyApplied =
                                  Maybes.maybe fullyAppliedRaw (\ct -> Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = fullyAppliedRaw,
                                    Core.annotatedTermAnnotation = (Maps.singleton (Core.Name "type") (Core_.type_ ct))})) codomainType
                          indexedDomains = Lists.zip indices domains
                      in (Lists.foldl (\body -> \idPair ->
                        let i = Pairs.first idPair
                            dom = Pairs.second idPair
                            vn = Core.Name (Strings.cat2 "v" (Literals.showInt32 i))
                        in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = vn,
                          Core.lambdaDomain = dom,
                          Core.lambdaBody = body})))) fullyApplied (Lists.reverse indexedDomains))) applied)
          rewriteWithArgs =
                  \args -> \tx -> \term ->
                    let recurse = \tx1 -> \term1 -> rewriteWithArgs [] tx1 term1
                        termHeadType =
                                \tx2 -> \trm2 -> case trm2 of
                                  Core.TermAnnotated v0 -> termHeadType tx2 (Core.annotatedTermBody v0)
                                  Core.TermFunction v0 -> case v0 of
                                    Core.FunctionPrimitive v1 -> Maybes.map Scoping.typeSchemeToFType (Maps.lookup v1 primTypes)
                                    _ -> Nothing
                                  Core.TermLet v0 -> termHeadType (Scoping.extendGraphForLet (\_ -> \_2 -> Nothing) tx2 v0) (Core.letBody v0)
                                  Core.TermTypeLambda v0 -> termHeadType (Scoping.extendGraphForTypeLambda tx2 v0) (Core.typeLambdaBody v0)
                                  Core.TermTypeApplication v0 -> Maybes.bind (termHeadType tx2 (Core.typeApplicationTermBody v0)) (\htyp2 -> case htyp2 of
                                    Core.TypeForall v1 -> Just (Variables.replaceFreeTypeVariable (Core.forallTypeParameter v1) (Core.typeApplicationTermType v0) (Core.forallTypeBody v1))
                                    _ -> Just htyp2)
                                  Core.TermVariable v0 -> Maybes.map Scoping.typeSchemeToFType (Maps.lookup v0 (Graph.graphBoundTypes tx2))
                                  _ -> Nothing
                        afterRecursion =
                                \trm ->
                                  let arity = termArityWithContext tx trm
                                      hType = termHeadType tx trm
                                  in (expand False args arity hType trm)
                        forField =
                                \f -> Core.Field {
                                  Core.fieldName = (Core.fieldName f),
                                  Core.fieldTerm = (recurse tx (Core.fieldTerm f))}
                        forCaseBranch =
                                \f ->
                                  let branchBody = recurse tx (Core.fieldTerm f)
                                      arty = termArityWithContext tx branchBody
                                      branchHType = termHeadType tx branchBody
                                  in Core.Field {
                                    Core.fieldName = (Core.fieldName f),
                                    Core.fieldTerm = (expand True [] arty branchHType branchBody)}
                        forElimination =
                                \elm -> case elm of
                                  Core.EliminationRecord v0 -> Core.EliminationRecord v0
                                  Core.EliminationUnion v0 -> Core.EliminationUnion (Core.CaseStatement {
                                    Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                                    Core.caseStatementDefault = (Maybes.map (\t1 -> recurse tx t1) (Core.caseStatementDefault v0)),
                                    Core.caseStatementCases = (Lists.map forCaseBranch (Core.caseStatementCases v0))})
                                  Core.EliminationWrap v0 -> Core.EliminationWrap v0
                        forMap =
                                \mp ->
                                  let forPair = \pr -> (recurse tx (Pairs.first pr), (recurse tx (Pairs.second pr)))
                                  in (Maps.fromList (Lists.map forPair (Maps.toList mp)))
                    in case term of
                      Core.TermAnnotated v0 -> afterRecursion (Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = (recurse tx (Core.annotatedTermBody v0)),
                        Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)}))
                      Core.TermApplication v0 ->
                        let rhs = rewriteWithArgs [] tx (Core.applicationArgument v0)
                        in (rewriteWithArgs (Lists.cons rhs args) tx (Core.applicationFunction v0))
                      Core.TermEither v0 -> afterRecursion (Core.TermEither (Eithers.either (\l -> Left (recurse tx l)) (\r -> Right (recurse tx r)) v0))
                      Core.TermFunction v0 -> case v0 of
                        Core.FunctionElimination v1 ->
                          let padElim =
                                  case v1 of
                                    Core.EliminationRecord _ -> False
                                    Core.EliminationUnion _ -> True
                                    Core.EliminationWrap _ -> False
                              elimTerm = Core.TermFunction (Core.FunctionElimination (forElimination v1))
                              elimHeadType =
                                      case v1 of
                                        Core.EliminationUnion v2 -> Just (Core.TypeFunction (Core.FunctionType {
                                          Core.functionTypeDomain = (Core.TypeVariable (Core.caseStatementTypeName v2)),
                                          Core.functionTypeCodomain = Core.TypeUnit}))
                                        _ -> Nothing
                          in (expand padElim args 1 elimHeadType elimTerm)
                        Core.FunctionLambda v1 ->
                          let tx1 = Scoping.extendGraphForLambda tx v1
                              body = rewriteWithArgs [] tx1 (Core.lambdaBody v1)
                              result =
                                      Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.lambdaParameter v1),
                                        Core.lambdaDomain = (Core.lambdaDomain v1),
                                        Core.lambdaBody = body}))
                              arty = termArityWithContext tx result
                          in (expand False args arty Nothing result)
                        Core.FunctionPrimitive v1 ->
                          let arty = termArityWithContext tx term
                              primType = Maybes.map (\ts -> Core.typeSchemeType ts) (Maps.lookup v1 primTypes)
                          in (expand False args arty primType term)
                      Core.TermLet v0 ->
                        let tx1 = Scoping.extendGraphForLet (\_ -> \_2 -> Nothing) tx v0
                            mapBinding =
                                    \b -> Core.Binding {
                                      Core.bindingName = (Core.bindingName b),
                                      Core.bindingTerm = (rewriteWithArgs [] tx1 (Core.bindingTerm b)),
                                      Core.bindingType = (Core.bindingType b)}
                            result =
                                    Core.TermLet (Core.Let {
                                      Core.letBindings = (Lists.map mapBinding (Core.letBindings v0)),
                                      Core.letBody = (rewriteWithArgs [] tx1 (Core.letBody v0))})
                        in (afterRecursion result)
                      Core.TermList v0 -> afterRecursion (Core.TermList (Lists.map (\el -> recurse tx el) v0))
                      Core.TermLiteral v0 -> Core.TermLiteral v0
                      Core.TermMap v0 -> afterRecursion (Core.TermMap (forMap v0))
                      Core.TermMaybe v0 -> afterRecursion (Core.TermMaybe (Maybes.map (\v -> recurse tx v) v0))
                      Core.TermPair v0 -> afterRecursion (Core.TermPair (recurse tx (Pairs.first v0), (recurse tx (Pairs.second v0))))
                      Core.TermRecord v0 -> afterRecursion (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.recordTypeName v0),
                        Core.recordFields = (Lists.map forField (Core.recordFields v0))}))
                      Core.TermSet v0 -> afterRecursion (Core.TermSet (Sets.fromList (Lists.map (\el -> recurse tx el) (Sets.toList v0))))
                      Core.TermTypeApplication v0 -> afterRecursion (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (recurse tx (Core.typeApplicationTermBody v0)),
                        Core.typeApplicationTermType = (Core.typeApplicationTermType v0)}))
                      Core.TermTypeLambda v0 ->
                        let tx1 = Scoping.extendGraphForTypeLambda tx v0
                            result =
                                    Core.TermTypeLambda (Core.TypeLambda {
                                      Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
                                      Core.typeLambdaBody = (rewriteWithArgs [] tx1 (Core.typeLambdaBody v0))})
                        in (afterRecursion result)
                      Core.TermUnion v0 -> afterRecursion (Core.TermUnion (Core.Injection {
                        Core.injectionTypeName = (Core.injectionTypeName v0),
                        Core.injectionField = (forField (Core.injectionField v0))}))
                      Core.TermUnit -> Core.TermUnit
                      Core.TermVariable v0 ->
                        let arty = termArityWithContext tx term
                            varType = Maybes.map Scoping.typeSchemeToFType (Maps.lookup v0 (Graph.graphBoundTypes tx))
                        in (expand False args arty varType term)
                      Core.TermWrap v0 -> afterRecursion (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.wrappedTermTypeName v0),
                        Core.wrappedTermBody = (recurse tx (Core.wrappedTermBody v0))}))
      in (contractTerm (rewriteWithArgs [] tx0 term0))

-- | Recursively transform arbitrary terms like 'add 42' into terms like '\x.add 42 x', eliminating partial application. Variable references are not expanded. This is useful for targets like Python with weaker support for currying than Hydra or Haskell. Note: this is a "trusty" function which assumes the graph is well-formed, i.e. no dangling references. It also assumes that type inference has already been performed. After eta expansion, type inference needs to be performed again, as new, untyped lambdas may have been added.
etaExpandTypedTerm :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
etaExpandTypedTerm cx tx0 term0 =

      let rewrite =
              \topLevel -> \forced -> \typeArgs -> \recurse -> \tx -> \term ->
                let rewriteSpine =
                        \term2 -> case term2 of
                          Core.TermAnnotated v0 -> Eithers.bind (rewriteSpine (Core.annotatedTermBody v0)) (\body ->
                            let ann = Core.annotatedTermAnnotation v0
                            in (Right (Core.TermAnnotated (Core.AnnotatedTerm {
                              Core.annotatedTermBody = body,
                              Core.annotatedTermAnnotation = ann}))))
                          Core.TermApplication v0 ->
                            let l = Logic.ifElse False [
                                  Core.TypeLiteral Core.LiteralTypeString] []
                            in (Eithers.bind (rewriteSpine (Core.applicationFunction v0)) (\lhs -> Eithers.bind (rewrite True False l recurse tx (Core.applicationArgument v0)) (\rhs -> Right (Core.TermApplication (Core.Application {
                              Core.applicationFunction = lhs,
                              Core.applicationArgument = rhs})))))
                          Core.TermTypeApplication v0 -> Eithers.bind (rewriteSpine (Core.typeApplicationTermBody v0)) (\body ->
                            let typ = Core.typeApplicationTermType v0
                            in (Right (Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = body,
                              Core.typeApplicationTermType = typ}))))
                          _ -> rewrite False False [] recurse tx term2
                    arityOf =
                            \tx2 -> \term2 ->
                              let dflt = Eithers.map (\_tc -> Arity.typeArity (Pairs.first _tc)) (Checking.typeOf cx tx2 [] term2)
                                  forFunction =
                                          \tx3 -> \f -> case f of
                                            Core.FunctionElimination _ -> Right 1
                                            Core.FunctionLambda v0 ->
                                              let txl = Scoping.extendGraphForLambda tx3 v0
                                              in (arityOf txl (Core.lambdaBody v0))
                                            Core.FunctionPrimitive v0 -> Eithers.map (\_ts -> Arity.typeSchemeArity _ts) (Lexical.requirePrimitiveType cx tx3 v0)
                              in case term2 of
                                Core.TermAnnotated v0 -> arityOf tx2 (Core.annotatedTermBody v0)
                                Core.TermFunction v0 -> forFunction tx2 v0
                                Core.TermLet v0 ->
                                  let txl = Scoping.extendGraphForLet (\_ -> \_2 -> Nothing) tx2 v0
                                  in (arityOf txl (Core.letBody v0))
                                Core.TermTypeApplication v0 -> arityOf tx2 (Core.typeApplicationTermBody v0)
                                Core.TermTypeLambda v0 ->
                                  let txt = Scoping.extendGraphForTypeLambda tx2 v0
                                  in (arityOf txt (Core.typeLambdaBody v0))
                                Core.TermVariable v0 -> Maybes.maybe (Eithers.map (\_tc -> Arity.typeArity (Pairs.first _tc)) (Checking.typeOf cx tx2 [] (Core.TermVariable v0))) (\t -> Right (Arity.typeArity t)) (Maybes.map Scoping.typeSchemeToFType (Maps.lookup v0 (Graph.graphBoundTypes tx2)))
                                _ -> dflt
                    extraVariables = \n -> Lists.map (\i -> Core.Name (Strings.cat2 "v" (Literals.showInt32 i))) (Math.range 1 n)
                    pad =
                            \vars -> \body -> Logic.ifElse (Lists.null vars) body (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Lists.head vars),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (pad (Lists.tail vars) (Core.TermApplication (Core.Application {
                                Core.applicationFunction = body,
                                Core.applicationArgument = (Core.TermVariable (Lists.head vars))})))})))
                    padn = \n -> \body -> pad (extraVariables n) body
                    unwind =
                            \term2 -> Lists.foldl (\e -> \t -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = e,
                              Core.typeApplicationTermType = t})) term2 typeArgs
                    forceExpansion =
                            \t -> Eithers.bind (Checking.typeOf cx tx [] t) (\typCx ->
                              let arity = Arity.typeArity (Pairs.first typCx)
                              in (Right (padn arity (unwind t))))
                    recurseOrForce = \term2 -> Logic.ifElse forced (forceExpansion term2) (recurse tx (unwind term2))
                    forCase =
                            \f -> Eithers.bind (rewrite False True [] recurse tx (Core.fieldTerm f)) (\r -> Right (Core.Field {
                              Core.fieldName = (Core.fieldName f),
                              Core.fieldTerm = r}))
                    forCaseStatement =
                            \cs ->
                              let tname = Core.caseStatementTypeName cs
                                  dflt = Core.caseStatementDefault cs
                                  cases = Core.caseStatementCases cs
                              in (Eithers.bind (Eithers.mapMaybe (rewrite False False [] recurse tx) dflt) (\rdflt -> Eithers.bind (Eithers.mapList forCase cases) (\rcases -> Right (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                Core.caseStatementTypeName = tname,
                                Core.caseStatementDefault = rdflt,
                                Core.caseStatementCases = rcases})))))))
                    forElimination =
                            \elm ->
                              let checkBase =
                                      \elm2 -> case elm2 of
                                        Core.EliminationUnion v0 -> forCaseStatement v0
                                        _ -> recurse tx term
                              in (Eithers.bind (Eithers.map unwind (checkBase elm)) (\base -> Right (Logic.ifElse (Logic.or topLevel forced) (padn 1 base) base)))
                in case term of
                  Core.TermApplication v0 ->
                    let lhs = Core.applicationFunction v0
                        rhs = Core.applicationArgument v0
                    in (Eithers.bind (rewrite True False [] recurse tx rhs) (\rhs2 -> Eithers.bind (arityOf tx lhs) (\lhsarity -> Eithers.bind (rewriteSpine lhs) (\lhs2 ->
                      let a2 =
                              Core.TermApplication (Core.Application {
                                Core.applicationFunction = lhs2,
                                Core.applicationArgument = rhs2})
                      in (Right (Logic.ifElse (Equality.gt lhsarity 1) (padn (Math.sub lhsarity 1) a2) a2))))))
                  Core.TermFunction v0 -> case v0 of
                    Core.FunctionElimination v1 -> forElimination v1
                    Core.FunctionLambda v1 ->
                      let txl = Scoping.extendGraphForLambda tx v1
                      in (Eithers.map unwind (recurse txl term))
                    _ -> recurseOrForce term
                  Core.TermLet v0 ->
                    let txlt = Scoping.extendGraphForLet (\_ -> \_2 -> Nothing) tx v0
                    in (recurse txlt term)
                  Core.TermTypeApplication v0 -> rewrite topLevel forced (Lists.cons (Core.typeApplicationTermType v0) typeArgs) recurse tx (Core.typeApplicationTermBody v0)
                  Core.TermTypeLambda v0 ->
                    let txt = Scoping.extendGraphForTypeLambda tx v0
                    in (recurse txt term)
                  _ -> recurseOrForce term
      in (Rewriting.rewriteTermWithContextM (rewrite True False []) tx0 term0)

-- | Calculate the arity for eta expansion Note: this is a "trusty" function which assumes the graph is well-formed, i.e. no dangling references.
etaExpansionArity :: Graph.Graph -> Core.Term -> Int
etaExpansionArity graph term =
    case term of
      Core.TermAnnotated v0 -> etaExpansionArity graph (Core.annotatedTermBody v0)
      Core.TermApplication v0 -> Math.sub (etaExpansionArity graph (Core.applicationFunction v0)) 1
      Core.TermFunction v0 -> case v0 of
        Core.FunctionElimination _ -> 1
        Core.FunctionLambda _ -> 0
        Core.FunctionPrimitive v1 -> Arity.primitiveArity (Maybes.fromJust (Lexical.lookupPrimitive graph v1))
      Core.TermTypeLambda v0 -> etaExpansionArity graph (Core.typeLambdaBody v0)
      Core.TermTypeApplication v0 -> etaExpansionArity graph (Core.typeApplicationTermBody v0)
      Core.TermVariable v0 -> Maybes.maybe 0 (\ts -> Arity.typeArity (Core.typeSchemeType ts)) (Maybes.bind (Lexical.lookupBinding graph v0) (\b -> Core.bindingType b))
      _ -> 0

-- | Eta-reduce a term by removing redundant lambda abstractions
etaReduceTerm :: Core.Term -> Core.Term
etaReduceTerm term =

      let noChange = term
          reduceLambda =
                  \l ->
                    let v = Core.lambdaParameter l
                        d = Core.lambdaDomain l
                        body = Core.lambdaBody l
                    in case (etaReduceTerm body) of
                      Core.TermAnnotated v0 -> reduceLambda (Core.Lambda {
                        Core.lambdaParameter = v,
                        Core.lambdaDomain = d,
                        Core.lambdaBody = (Core.annotatedTermBody v0)})
                      Core.TermApplication v0 ->
                        let lhs = Core.applicationFunction v0
                            rhs = Core.applicationArgument v0
                        in case (etaReduceTerm rhs) of
                          Core.TermAnnotated v1 -> reduceLambda (Core.Lambda {
                            Core.lambdaParameter = v,
                            Core.lambdaDomain = d,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = lhs,
                              Core.applicationArgument = (Core.annotatedTermBody v1)}))})
                          Core.TermVariable v1 -> Logic.ifElse (Logic.and (Equality.equal (Core.unName v) (Core.unName v1)) (Logic.not (Variables.isFreeVariableInTerm v lhs))) (etaReduceTerm lhs) noChange
                          _ -> noChange
                      _ -> noChange
      in case term of
        Core.TermAnnotated v0 -> Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (etaReduceTerm (Core.annotatedTermBody v0)),
          Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})
        Core.TermFunction v0 -> case v0 of
          Core.FunctionLambda v1 -> reduceLambda v1
          _ -> noChange
        _ -> noChange

-- | A term evaluation function which is alternatively lazy or eager
reduceTerm :: Context.Context -> Graph.Graph -> Bool -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
reduceTerm cx graph eager term =

      let reduce = \eager2 -> reduceTerm cx graph eager2
          doRecurse =
                  \eager2 -> \term2 ->
                    let isNonLambda =
                            \f -> case f of
                              Core.FunctionLambda _ -> False
                              _ -> True
                        isNonLambdaTerm =
                                case term2 of
                                  Core.TermFunction v0 -> isNonLambda v0
                                  Core.TermLet _ -> False
                                  _ -> True
                    in (Logic.and eager2 isNonLambdaTerm)
          reduceArg = \eager2 -> \arg -> Logic.ifElse eager2 (Right arg) (reduce False arg)
          applyToArguments =
                  \fun -> \args -> Logic.ifElse (Lists.null args) fun (applyToArguments (Core.TermApplication (Core.Application {
                    Core.applicationFunction = fun,
                    Core.applicationArgument = (Lists.head args)})) (Lists.tail args))
          mapErrorToString =
                  \ic -> Context.InContext {
                    Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Errors_.error (Context.inContextObject ic)))),
                    Context.inContextContext = (Context.inContextContext ic)}
          applyElimination =
                  \elm -> \reducedArg -> case elm of
                    Core.EliminationRecord v0 -> Eithers.bind (Core__.record cx (Core.projectionTypeName v0) graph (Strip.deannotateTerm reducedArg)) (\fields ->
                      let matchingFields = Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.projectionField v0)) fields
                      in (Logic.ifElse (Lists.null matchingFields) (Left (Context.InContext {
                        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                          "no such field: ",
                          (Core.unName (Core.projectionField v0)),
                          " in ",
                          (Core.unName (Core.projectionTypeName v0)),
                          " record"]))),
                        Context.inContextContext = cx})) (Right (Core.fieldTerm (Lists.head matchingFields)))))
                    Core.EliminationUnion v0 -> Eithers.bind (Core__.injection cx (Core.caseStatementTypeName v0) graph reducedArg) (\field ->
                      let matchingFields = Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.fieldName field)) (Core.caseStatementCases v0)
                      in (Logic.ifElse (Lists.null matchingFields) (Maybes.maybe (Left (Context.InContext {
                        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                          "no such field ",
                          (Core.unName (Core.fieldName field)),
                          " in ",
                          (Core.unName (Core.caseStatementTypeName v0)),
                          " case statement"]))),
                        Context.inContextContext = cx})) (\x -> Right x) (Core.caseStatementDefault v0)) (Right (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.fieldTerm (Lists.head matchingFields)),
                        Core.applicationArgument = (Core.fieldTerm field)})))))
                    Core.EliminationWrap v0 -> Core__.wrap cx v0 graph reducedArg
          applyIfNullary =
                  \eager2 -> \original -> \args ->
                    let stripped = Strip.deannotateTerm original
                        forElimination =
                                \elm -> \args2 ->
                                  let arg = Lists.head args2
                                      remainingArgs = Lists.tail args2
                                  in (Eithers.bind (reduceArg eager2 (Strip.deannotateTerm arg)) (\reducedArg -> Eithers.bind (Eithers.bind (applyElimination elm reducedArg) (reduce eager2)) (\reducedResult -> applyIfNullary eager2 reducedResult remainingArgs)))
                        forLambda =
                                \l -> \args2 ->
                                  let param = Core.lambdaParameter l
                                      body = Core.lambdaBody l
                                      arg = Lists.head args2
                                      remainingArgs = Lists.tail args2
                                  in (Eithers.bind (reduce eager2 (Strip.deannotateTerm arg)) (\reducedArg -> Eithers.bind (reduce eager2 (Variables.replaceFreeTermVariable param reducedArg body)) (\reducedResult -> applyIfNullary eager2 reducedResult remainingArgs)))
                        forPrimitive =
                                \prim -> \arity -> \args2 ->
                                  let argList = Lists.take arity args2
                                      remainingArgs = Lists.drop arity args2
                                  in (Eithers.bind (Eithers.mapList (reduceArg eager2) argList) (\reducedArgs ->
                                    let strippedArgs = Lists.map Strip.deannotateTerm reducedArgs
                                    in (Eithers.bind (Eithers.bimap mapErrorToString (\x -> x) (Graph.primitiveImplementation prim cx graph strippedArgs)) (\primResult -> Eithers.bind (reduce eager2 primResult) (\reducedResult -> applyIfNullary eager2 reducedResult remainingArgs)))))
                    in case stripped of
                      Core.TermApplication v0 -> applyIfNullary eager2 (Core.applicationFunction v0) (Lists.cons (Core.applicationArgument v0) args)
                      Core.TermFunction v0 -> case v0 of
                        Core.FunctionElimination v1 -> Logic.ifElse (Lists.null args) (Right original) (forElimination v1 args)
                        Core.FunctionLambda v1 -> Logic.ifElse (Lists.null args) (Right original) (forLambda v1 args)
                        Core.FunctionPrimitive v1 -> Eithers.bind (Lexical.requirePrimitive cx graph v1) (\prim ->
                          let arity = Arity.primitiveArity prim
                          in (Logic.ifElse (Equality.gt arity (Lists.length args)) (Right (applyToArguments original args)) (forPrimitive prim arity args)))
                      Core.TermVariable v0 ->
                        let mBinding = Lexical.lookupBinding graph v0
                        in (Maybes.maybe (
                          let mPrim = Lexical.lookupPrimitive graph v0
                          in (Maybes.maybe (Right (applyToArguments original args)) (\prim ->
                            let arity = Arity.primitiveArity prim
                            in (Logic.ifElse (Equality.gt arity (Lists.length args)) (Right (applyToArguments original args)) (forPrimitive prim arity args))) mPrim)) (\binding -> applyIfNullary eager2 (Core.bindingTerm binding) args) mBinding)
                      Core.TermLet v0 ->
                        let bindings = Core.letBindings v0
                            body = Core.letBody v0
                            letExpr =
                                    \b -> Core.TermLet (Core.Let {
                                      Core.letBindings = [
                                        b],
                                      Core.letBody = (Core.TermVariable (Core.bindingName b))})
                            expandBinding =
                                    \b -> Core.Binding {
                                      Core.bindingName = (Core.bindingName b),
                                      Core.bindingTerm = (Variables.replaceFreeTermVariable (Core.bindingName b) (letExpr b) (Core.bindingTerm b)),
                                      Core.bindingType = (Core.bindingType b)}
                            expandedBindings = Lists.map expandBinding bindings
                            substituteBinding = \term2 -> \b -> Variables.replaceFreeTermVariable (Core.bindingName b) (Core.bindingTerm b) term2
                            substituteAll = \bs -> \term2 -> Lists.foldl substituteBinding term2 bs
                            expandedBody = substituteAll expandedBindings body
                        in (Eithers.bind (reduce eager2 expandedBody) (\reducedBody -> applyIfNullary eager2 reducedBody args))
                      _ -> Right (applyToArguments original args)
          mapping =
                  \recurse -> \mid -> Eithers.bind (Logic.ifElse (doRecurse eager mid) (recurse mid) (Right mid)) (\inner -> applyIfNullary eager inner [])
      in (Rewriting.rewriteTermM mapping term)

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: Core.Term -> Bool
termIsClosed term = Sets.null (Variables.freeVariablesInTerm term)

-- | Whether a term has been fully reduced to a value
termIsValue :: Core.Term -> Bool
termIsValue term =

      let forList = \els -> Lists.foldl (\b -> \t -> Logic.and b (termIsValue t)) True els
          checkField = \f -> termIsValue (Core.fieldTerm f)
          checkFields = \fields -> Lists.foldl (\b -> \f -> Logic.and b (checkField f)) True fields
          functionIsValue =
                  \f -> case f of
                    Core.FunctionElimination v0 -> case v0 of
                      Core.EliminationWrap _ -> True
                      Core.EliminationRecord _ -> True
                      Core.EliminationUnion v1 -> Logic.and (checkFields (Core.caseStatementCases v1)) (Maybes.maybe True termIsValue (Core.caseStatementDefault v1))
                    Core.FunctionLambda v0 -> termIsValue (Core.lambdaBody v0)
                    Core.FunctionPrimitive _ -> True
      in case (Strip.deannotateTerm term) of
        Core.TermApplication _ -> False
        Core.TermEither v0 -> Eithers.either (\l -> termIsValue l) (\r -> termIsValue r) v0
        Core.TermLiteral _ -> True
        Core.TermFunction v0 -> functionIsValue v0
        Core.TermList v0 -> forList v0
        Core.TermMap v0 -> Lists.foldl (\b -> \kv -> Logic.and b (Logic.and (termIsValue (Pairs.first kv)) (termIsValue (Pairs.second kv)))) True (Maps.toList v0)
        Core.TermMaybe v0 -> Maybes.maybe True termIsValue v0
        Core.TermRecord v0 -> checkFields (Core.recordFields v0)
        Core.TermSet v0 -> forList (Sets.toList v0)
        Core.TermUnion v0 -> checkField (Core.injectionField v0)
        Core.TermUnit -> True
        Core.TermVariable _ -> False
        _ -> False
