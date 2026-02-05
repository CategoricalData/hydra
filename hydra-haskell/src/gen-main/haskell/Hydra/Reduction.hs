-- Note: this is an automatically generated file. Do not edit.

-- | Functions for reducing terms and types, i.e. performing computations.

module Hydra.Reduction where

import qualified Hydra.Arity as Arity
import qualified Hydra.Checking as Checking
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
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

-- | Alpha convert a variable in a term
alphaConvert :: (Core.Name -> Core.Name -> Core.Term -> Core.Term)
alphaConvert vold vnew term = (Rewriting.replaceFreeTermVariable vold (Core.TermVariable vnew) term)

-- | Eagerly beta-reduce a type by substituting type arguments into type lambdas
betaReduceType :: (Core.Type -> Compute.Flow Graph.Graph Core.Type)
betaReduceType typ =  
  let reduceApp = (\app ->  
          let lhs = (Core.applicationTypeFunction app)
          in  
            let rhs = (Core.applicationTypeArgument app)
            in ((\x -> case x of
              Core.TypeAnnotated v1 -> (Flows.bind (reduceApp (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.annotatedTypeBody v1),
                Core.applicationTypeArgument = rhs})) (\a -> Flows.pure (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = a,
                Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)}))))
              Core.TypeForall v1 -> (betaReduceType (Rewriting.replaceFreeTypeVariable (Core.forallTypeParameter v1) rhs (Core.forallTypeBody v1)))
              Core.TypeVariable v1 -> (Flows.bind (Schemas.requireType v1) (\t_ -> betaReduceType (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = t_,
                Core.applicationTypeArgument = rhs}))))) lhs))
  in  
    let mapExpr = (\recurse -> \t ->  
            let findApp = (\r -> (\x -> case x of
                    Core.TypeApplication v1 -> (reduceApp v1)
                    _ -> (Flows.pure r)) r)
            in (Flows.bind (recurse t) (\r -> findApp r)))
    in (Rewriting.rewriteTypeM mapExpr typ)

-- | Apply the special rules:
-- |     ((\x.e1) e2) == e1, where x does not appear free in e1
-- |   and
-- |      ((\x.e1) e2) = e1[x/e2]
-- | These are both limited forms of beta reduction which help to "clean up" a term without fully evaluating it.
contractTerm :: (Core.Term -> Core.Term)
contractTerm term =  
  let rewrite = (\recurse -> \t ->  
          let rec = (recurse t)
          in ((\x -> case x of
            Core.TermApplication v1 ->  
              let lhs = (Core.applicationFunction v1)
              in  
                let rhs = (Core.applicationArgument v1)
                in ((\x -> case x of
                  Core.TermFunction v2 -> ((\x -> case x of
                    Core.FunctionLambda v3 ->  
                      let v = (Core.lambdaParameter v3)
                      in  
                        let body = (Core.lambdaBody v3)
                        in (Logic.ifElse (Rewriting.isFreeVariableInTerm v body) body (Rewriting.replaceFreeTermVariable v rhs body))
                    _ -> rec) v2)
                  _ -> rec) (Rewriting.deannotateTerm lhs))
            _ -> rec) rec))
  in (Rewriting.rewriteTerm rewrite term)

countPrimitiveInvocations :: Bool
countPrimitiveInvocations = True

-- | Eta-reduce a term by removing redundant lambda abstractions
etaReduceTerm :: (Core.Term -> Core.Term)
etaReduceTerm term =  
  let noChange = term
  in  
    let reduceLambda = (\l ->  
            let v = (Core.lambdaParameter l)
            in  
              let d = (Core.lambdaDomain l)
              in  
                let body = (Core.lambdaBody l)
                in ((\x -> case x of
                  Core.TermAnnotated v1 -> (reduceLambda (Core.Lambda {
                    Core.lambdaParameter = v,
                    Core.lambdaDomain = d,
                    Core.lambdaBody = (Core.annotatedTermBody v1)}))
                  Core.TermApplication v1 ->  
                    let lhs = (Core.applicationFunction v1)
                    in  
                      let rhs = (Core.applicationArgument v1)
                      in ((\x -> case x of
                        Core.TermAnnotated v2 -> (reduceLambda (Core.Lambda {
                          Core.lambdaParameter = v,
                          Core.lambdaDomain = d,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = lhs,
                            Core.applicationArgument = (Core.annotatedTermBody v2)}))}))
                        Core.TermVariable v2 -> (Logic.ifElse (Logic.and (Equality.equal (Core.unName v) (Core.unName v2)) (Logic.not (Rewriting.isFreeVariableInTerm v lhs))) (etaReduceTerm lhs) noChange)
                        _ -> noChange) (etaReduceTerm rhs))
                  _ -> noChange) (etaReduceTerm body)))
    in ((\x -> case x of
      Core.TermAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (etaReduceTerm (Core.annotatedTermBody v1)),
        Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))
      Core.TermFunction v1 -> ((\x -> case x of
        Core.FunctionLambda v2 -> (reduceLambda v2)
        _ -> noChange) v1)
      _ -> noChange) term)

-- | Recursively transform arbitrary terms like 'add 42' into terms like '\x.add 42 x', in which the implicit parameters of primitive functions and eliminations are made into explicit lambda parameters. Variable references are not expanded. This is useful for targets like Python with weaker support for currying than Hydra or Haskell. Note: this is a "trusty" function which assumes the graph is well-formed, i.e. no dangling references.
etaExpandTerm :: (Graph.Graph -> Core.Term -> Core.Term)
etaExpandTerm graph term =  
  let expand = (\args -> \arity -> \t ->  
          let apps = (Lists.foldl (\lhs -> \arg -> Core.TermApplication (Core.Application {
                  Core.applicationFunction = lhs,
                  Core.applicationArgument = arg})) t args)
          in  
            let is = (Logic.ifElse (Equality.lte arity (Lists.length args)) [] (Math.range 1 (Math.sub arity (Lists.length args))))
            in  
              let pad = (\indices -> \t -> Logic.ifElse (Lists.null indices) t (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name (Strings.cat2 "v" (Literals.showInt32 (Lists.head indices)))),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (pad (Lists.tail indices) (Core.TermApplication (Core.Application {
                        Core.applicationFunction = t,
                        Core.applicationArgument = (Core.TermVariable (Core.Name (Strings.cat2 "v" (Literals.showInt32 (Lists.head indices)))))})))}))))
              in (pad is apps))
  in  
    let rewrite = (\args -> \recurse -> \t ->  
            let afterRecursion = (\term -> expand args (etaExpansionArity graph term) term)
            in  
              let t2 = (Rewriting.detypeTerm t)
              in ((\x -> case x of
                Core.TermApplication v1 ->  
                  let lhs = (Core.applicationFunction v1)
                  in  
                    let rhs = (Core.applicationArgument v1)
                    in  
                      let erhs = (rewrite [] recurse rhs)
                      in (rewrite (Lists.cons erhs args) recurse lhs)
                _ -> (afterRecursion (recurse t2))) t2))
    in (contractTerm (Rewriting.rewriteTerm (rewrite []) term))

-- | Recursively transform terms to eliminate partial application, e.g. 'add 42' becomes '\x.add 42 x'. Uses the TypeContext to look up types for arity calculation. Bare primitives and variables are NOT expanded; eliminations and partial applications are. This version properly tracks the TypeContext through nested scopes.
etaExpandTermNew :: (Typing.TypeContext -> Core.Term -> Core.Term)
etaExpandTermNew tx0 term0 =  
  let termArityWithContext = (\tx -> \term -> (\x -> case x of
          Core.TermAnnotated v1 -> (termArityWithContext tx (Core.annotatedTermBody v1))
          Core.TermApplication v1 -> (Math.sub (termArityWithContext tx (Core.applicationFunction v1)) 1)
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionElimination _ -> 1
            Core.FunctionLambda _ -> 0
            Core.FunctionPrimitive v2 -> (Maybes.maybe 0 Arity.typeSchemeArity (Maps.lookup v2 (Typing.inferenceContextPrimitiveTypes (Typing.typeContextInferenceContext tx))))) v1)
          Core.TermLet v1 -> (termArityWithContext (Schemas.extendTypeContextForLet (\_ -> \_ -> Nothing) tx v1) (Core.letBody v1))
          Core.TermTypeLambda v1 -> (termArityWithContext (Schemas.extendTypeContextForTypeLambda tx v1) (Core.typeLambdaBody v1))
          Core.TermTypeApplication v1 -> (termArityWithContext tx (Core.typeApplicationTermBody v1))
          Core.TermVariable v1 -> (Maybes.maybe 0 Arity.typeArity (Maps.lookup v1 (Typing.typeContextTypes tx)))
          _ -> 0) term)
  in  
    let expand = (\alwaysPad -> \args -> \arity -> \head ->  
            let applied = (Lists.foldl (\lhs -> \arg -> Core.TermApplication (Core.Application {
                    Core.applicationFunction = lhs,
                    Core.applicationArgument = arg})) head args)
            in  
              let numArgs = (Lists.length args)
              in  
                let needed = (Math.sub arity numArgs)
                in (Logic.ifElse (Logic.and (Equality.gt needed 0) (Logic.or alwaysPad (Equality.gt numArgs 0))) ( 
                  let indices = (Math.range 1 needed)
                  in  
                    let fullyApplied = (Lists.foldl (\body -> \i ->  
                            let vn = (Core.Name (Strings.cat2 "v" (Literals.showInt32 i)))
                            in (Core.TermApplication (Core.Application {
                              Core.applicationFunction = body,
                              Core.applicationArgument = (Core.TermVariable vn)}))) applied indices)
                    in (Lists.foldl (\body -> \i ->  
                      let vn = (Core.Name (Strings.cat2 "v" (Literals.showInt32 i)))
                      in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = vn,
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = body})))) fullyApplied (Lists.reverse indices))) applied))
    in  
      let rewriteWithArgs = (\args -> \tx -> \term ->  
              let recurse = (\tx1 -> \term1 -> rewriteWithArgs [] tx1 term1)
              in  
                let afterRecursion = (\trm ->  
                        let arity = (termArityWithContext tx trm)
                        in (expand False args arity trm))
                in  
                  let forField = (\f -> Core.Field {
                          Core.fieldName = (Core.fieldName f),
                          Core.fieldTerm = (recurse tx (Core.fieldTerm f))})
                  in  
                    let forCaseBranch = (\f ->  
                            let branchBody = (recurse tx (Core.fieldTerm f))
                            in  
                              let arty = (termArityWithContext tx branchBody)
                              in Core.Field {
                                Core.fieldName = (Core.fieldName f),
                                Core.fieldTerm = (expand True [] arty branchBody)})
                    in  
                      let forElimination = (\elm -> (\x -> case x of
                              Core.EliminationRecord v1 -> (Core.EliminationRecord v1)
                              Core.EliminationUnion v1 -> (Core.EliminationUnion (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.caseStatementTypeName v1),
                                Core.caseStatementDefault = (Maybes.map (\t1 -> recurse tx t1) (Core.caseStatementDefault v1)),
                                Core.caseStatementCases = (Lists.map forCaseBranch (Core.caseStatementCases v1))}))
                              Core.EliminationWrap v1 -> (Core.EliminationWrap v1)) elm)
                      in  
                        let forMap = (\mp ->  
                                let forPair = (\pr -> (recurse tx (Pairs.first pr), (recurse tx (Pairs.second pr))))
                                in (Maps.fromList (Lists.map forPair (Maps.toList mp))))
                        in ((\x -> case x of
                          Core.TermAnnotated v1 -> (afterRecursion (Core.TermAnnotated (Core.AnnotatedTerm {
                            Core.annotatedTermBody = (recurse tx (Core.annotatedTermBody v1)),
                            Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)})))
                          Core.TermApplication v1 ->  
                            let rhs = (rewriteWithArgs [] tx (Core.applicationArgument v1))
                            in (rewriteWithArgs (Lists.cons rhs args) tx (Core.applicationFunction v1))
                          Core.TermEither v1 -> (afterRecursion (Core.TermEither (Eithers.either (\l -> Left (recurse tx l)) (\r -> Right (recurse tx r)) v1)))
                          Core.TermFunction v1 -> ((\x -> case x of
                            Core.FunctionElimination v2 ->  
                              let padElim = ((\x -> case x of
                                      Core.EliminationRecord _ -> False
                                      Core.EliminationUnion _ -> True
                                      Core.EliminationWrap _ -> False) v2)
                              in (expand padElim args 1 (Core.TermFunction (Core.FunctionElimination (forElimination v2))))
                            Core.FunctionLambda v2 ->  
                              let tx1 = (Schemas.extendTypeContextForLambda tx v2)
                              in  
                                let body = (rewriteWithArgs [] tx1 (Core.lambdaBody v2))
                                in  
                                  let result = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.lambdaParameter v2),
                                          Core.lambdaDomain = (Core.lambdaDomain v2),
                                          Core.lambdaBody = body})))
                                  in  
                                    let arty = (termArityWithContext tx result)
                                    in (expand False args arty result)
                            Core.FunctionPrimitive _ ->  
                              let arty = (termArityWithContext tx term)
                              in (expand False args arty term)) v1)
                          Core.TermLet v1 ->  
                            let tx1 = (Schemas.extendTypeContextForLet (\_ -> \_ -> Nothing) tx v1)
                            in  
                              let mapBinding = (\b -> Core.Binding {
                                      Core.bindingName = (Core.bindingName b),
                                      Core.bindingTerm = (rewriteWithArgs [] tx1 (Core.bindingTerm b)),
                                      Core.bindingType = (Core.bindingType b)})
                              in  
                                let result = (Core.TermLet (Core.Let {
                                        Core.letBindings = (Lists.map mapBinding (Core.letBindings v1)),
                                        Core.letBody = (rewriteWithArgs [] tx1 (Core.letBody v1))}))
                                in (afterRecursion result)
                          Core.TermList v1 -> (afterRecursion (Core.TermList (Lists.map (\el -> recurse tx el) v1)))
                          Core.TermLiteral v1 -> (Core.TermLiteral v1)
                          Core.TermMap v1 -> (afterRecursion (Core.TermMap (forMap v1)))
                          Core.TermMaybe v1 -> (afterRecursion (Core.TermMaybe (Maybes.map (\v -> recurse tx v) v1)))
                          Core.TermPair v1 -> (afterRecursion (Core.TermPair (recurse tx (Pairs.first v1), (recurse tx (Pairs.second v1)))))
                          Core.TermRecord v1 -> (afterRecursion (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.recordTypeName v1),
                            Core.recordFields = (Lists.map forField (Core.recordFields v1))})))
                          Core.TermSet v1 -> (afterRecursion (Core.TermSet (Sets.fromList (Lists.map (\el -> recurse tx el) (Sets.toList v1)))))
                          Core.TermTypeApplication v1 -> (afterRecursion (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (recurse tx (Core.typeApplicationTermBody v1)),
                            Core.typeApplicationTermType = (Core.typeApplicationTermType v1)})))
                          Core.TermTypeLambda v1 ->  
                            let tx1 = (Schemas.extendTypeContextForTypeLambda tx v1)
                            in  
                              let result = (Core.TermTypeLambda (Core.TypeLambda {
                                      Core.typeLambdaParameter = (Core.typeLambdaParameter v1),
                                      Core.typeLambdaBody = (rewriteWithArgs [] tx1 (Core.typeLambdaBody v1))}))
                              in (afterRecursion result)
                          Core.TermUnion v1 -> (afterRecursion (Core.TermUnion (Core.Injection {
                            Core.injectionTypeName = (Core.injectionTypeName v1),
                            Core.injectionField = (forField (Core.injectionField v1))})))
                          Core.TermUnit -> Core.TermUnit
                          Core.TermVariable _ ->  
                            let arty = (termArityWithContext tx term)
                            in (expand False args arty term)
                          Core.TermWrap v1 -> (afterRecursion (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.wrappedTermTypeName v1),
                            Core.wrappedTermBody = (recurse tx (Core.wrappedTermBody v1))})))) term))
      in (contractTerm (rewriteWithArgs [] tx0 term0))

-- | Calculate the arity for eta expansion Note: this is a "trusty" function which assumes the graph is well-formed, i.e. no dangling references.
etaExpansionArity :: (Graph.Graph -> Core.Term -> Int)
etaExpansionArity graph term = ((\x -> case x of
  Core.TermAnnotated v1 -> (etaExpansionArity graph (Core.annotatedTermBody v1))
  Core.TermApplication v1 -> (Math.sub (etaExpansionArity graph (Core.applicationFunction v1)) 1)
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination _ -> 1
    Core.FunctionLambda _ -> 0
    Core.FunctionPrimitive v2 -> (Arity.primitiveArity (Maybes.fromJust (Lexical.lookupPrimitive graph v2)))) v1)
  Core.TermTypeLambda v1 -> (etaExpansionArity graph (Core.typeLambdaBody v1))
  Core.TermTypeApplication v1 -> (etaExpansionArity graph (Core.typeApplicationTermBody v1))
  Core.TermVariable v1 -> (Maybes.maybe 0 (\ts -> Arity.typeArity (Core.typeSchemeType ts)) (Maybes.bind (Lexical.lookupElement graph v1) (\b -> Core.bindingType b)))
  _ -> 0) term)

etaExpandTypedTerm :: (Typing.TypeContext -> Core.Term -> Compute.Flow t0 Core.Term)
etaExpandTypedTerm tx0 term0 =  
  let rewrite = (\topLevel -> \forced -> \typeArgs -> \recurse -> \tx -> \term ->  
          let rewriteSpine = (\term -> (\x -> case x of
                  Core.TermAnnotated v1 -> (Flows.bind (rewriteSpine (Core.annotatedTermBody v1)) (\body ->  
                    let ann = (Core.annotatedTermAnnotation v1)
                    in (Flows.pure (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = body,
                      Core.annotatedTermAnnotation = ann})))))
                  Core.TermApplication v1 ->  
                    let l = (Logic.ifElse False [
                            Core.TypeLiteral Core.LiteralTypeString] [])
                    in (Flows.bind (rewriteSpine (Core.applicationFunction v1)) (\lhs -> Flows.bind (rewrite True False l recurse tx (Core.applicationArgument v1)) (\rhs -> Flows.pure (Core.TermApplication (Core.Application {
                      Core.applicationFunction = lhs,
                      Core.applicationArgument = rhs})))))
                  Core.TermTypeApplication v1 -> (Flows.bind (rewriteSpine (Core.typeApplicationTermBody v1)) (\body ->  
                    let typ = (Core.typeApplicationTermType v1)
                    in (Flows.pure (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = body,
                      Core.typeApplicationTermType = typ})))))
                  _ -> (rewrite False False [] recurse tx term)) term)
          in  
            let arityOf = (\tx -> \term ->  
                    let dflt = (Flows.map Arity.typeArity (Checking.typeOf tx [] term))
                    in  
                      let forFunction = (\tx -> \f -> (\x -> case x of
                              Core.FunctionElimination _ -> (Flows.pure 1)
                              Core.FunctionLambda v1 ->  
                                let txl = (Schemas.extendTypeContextForLambda tx v1)
                                in (arityOf txl (Core.lambdaBody v1))
                              Core.FunctionPrimitive v1 -> (Flows.map Arity.typeSchemeArity (Lexical.requirePrimitiveType tx v1))) f)
                      in ((\x -> case x of
                        Core.TermAnnotated v1 -> (arityOf tx (Core.annotatedTermBody v1))
                        Core.TermFunction v1 -> (forFunction tx v1)
                        Core.TermLet v1 ->  
                          let txl = (Schemas.extendTypeContextForLet (\_ -> \_ -> Nothing) tx v1)
                          in (arityOf txl (Core.letBody v1))
                        Core.TermTypeApplication v1 -> (arityOf tx (Core.typeApplicationTermBody v1))
                        Core.TermTypeLambda v1 ->  
                          let txt = (Schemas.extendTypeContextForTypeLambda tx v1)
                          in (arityOf txt (Core.typeLambdaBody v1))
                        Core.TermVariable v1 -> (Maybes.maybe (Flows.map Arity.typeArity (Checking.typeOf tx [] (Core.TermVariable v1))) (\t -> Flows.pure (Arity.typeArity t)) (Maps.lookup v1 (Typing.typeContextTypes tx)))
                        _ -> dflt) term))
            in  
              let extraVariables = (\n -> Lists.map (\i -> Core.Name (Strings.cat2 "v" (Literals.showInt32 i))) (Math.range 1 n))
              in  
                let pad = (\vars -> \body -> Logic.ifElse (Lists.null vars) body (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Lists.head vars),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (pad (Lists.tail vars) (Core.TermApplication (Core.Application {
                          Core.applicationFunction = body,
                          Core.applicationArgument = (Core.TermVariable (Lists.head vars))})))}))))
                in  
                  let padn = (\n -> \body -> pad (extraVariables n) body)
                  in  
                    let unwind = (\term -> Lists.foldl (\e -> \t -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = e,
                            Core.typeApplicationTermType = t})) term typeArgs)
                    in  
                      let forceExpansion = (\t -> Flows.bind (Checking.typeOf tx [] t) (\typ ->  
                              let arity = (Arity.typeArity typ)
                              in (Flows.pure (padn arity (unwind t)))))
                      in  
                        let recurseOrForce = (\term -> Logic.ifElse forced (forceExpansion term) (recurse tx (unwind term)))
                        in  
                          let forCase = (\f -> Flows.bind (rewrite False True [] recurse tx (Core.fieldTerm f)) (\r -> Flows.pure (Core.Field {
                                  Core.fieldName = (Core.fieldName f),
                                  Core.fieldTerm = r})))
                          in  
                            let forCaseStatement = (\cs ->  
                                    let tname = (Core.caseStatementTypeName cs)
                                    in  
                                      let dflt = (Core.caseStatementDefault cs)
                                      in  
                                        let cases = (Core.caseStatementCases cs)
                                        in (Flows.bind (Flows.mapMaybe (rewrite False False [] recurse tx) dflt) (\rdflt -> Flows.bind (Flows.mapList forCase cases) (\rcases -> Flows.pure (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                          Core.caseStatementTypeName = tname,
                                          Core.caseStatementDefault = rdflt,
                                          Core.caseStatementCases = rcases}))))))))
                            in  
                              let forElimination = (\elm ->  
                                      let checkBase = (\elm -> (\x -> case x of
                                              Core.EliminationUnion v1 -> (forCaseStatement v1)
                                              _ -> (recurse tx term)) elm)
                                      in (Flows.bind (Flows.map unwind (checkBase elm)) (\base -> Flows.pure (Logic.ifElse (Logic.or topLevel forced) (padn 1 base) base))))
                              in ((\x -> case x of
                                Core.TermApplication v1 ->  
                                  let lhs = (Core.applicationFunction v1)
                                  in  
                                    let rhs = (Core.applicationArgument v1)
                                    in (Flows.bind (rewrite True False [] recurse tx rhs) (\rhs2 -> Flows.bind (arityOf tx lhs) (\lhsarity -> Flows.bind (rewriteSpine lhs) (\lhs2 ->  
                                      let a2 = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = lhs2,
                                              Core.applicationArgument = rhs2}))
                                      in (Flows.pure (Logic.ifElse (Equality.gt lhsarity 1) (padn (Math.sub lhsarity 1) a2) a2))))))
                                Core.TermFunction v1 -> ((\x -> case x of
                                  Core.FunctionElimination v2 -> (forElimination v2)
                                  Core.FunctionLambda v2 ->  
                                    let txl = (Schemas.extendTypeContextForLambda tx v2)
                                    in (Flows.map unwind (recurse txl term))
                                  _ -> (recurseOrForce term)) v1)
                                Core.TermLet v1 ->  
                                  let txlt = (Schemas.extendTypeContextForLet (\_ -> \_ -> Nothing) tx v1)
                                  in (recurse txlt term)
                                Core.TermTypeApplication v1 -> (rewrite topLevel forced (Lists.cons (Core.typeApplicationTermType v1) typeArgs) recurse tx (Core.typeApplicationTermBody v1))
                                Core.TermTypeLambda v1 ->  
                                  let txt = (Schemas.extendTypeContextForTypeLambda tx v1)
                                  in (recurse txt term)
                                _ -> (recurseOrForce term)) term))
  in (Rewriting.rewriteTermWithContextM (rewrite True False []) tx0 term0)

-- | A term evaluation function which is alternatively lazy or eager
reduceTerm :: (Bool -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
reduceTerm eager term =  
  let reduce = (\eager -> reduceTerm eager)
  in  
    let doRecurse = (\eager -> \term ->  
            let isNonLambda = (\f -> (\x -> case x of
                    Core.FunctionLambda _ -> False
                    _ -> True) f)
            in  
              let isNonLambdaTerm = ((\x -> case x of
                      Core.TermFunction v1 -> (isNonLambda v1)
                      Core.TermLet _ -> False
                      _ -> True) term)
              in (Logic.and eager isNonLambdaTerm))
    in  
      let reduceArg = (\eager -> \arg -> Logic.ifElse eager (Flows.pure arg) (reduce False arg))
      in  
        let applyToArguments = (\fun -> \args -> Logic.ifElse (Lists.null args) fun (applyToArguments (Core.TermApplication (Core.Application {
                Core.applicationFunction = fun,
                Core.applicationArgument = (Lists.head args)})) (Lists.tail args)))
        in  
          let applyElimination = (\elm -> \reducedArg -> (\x -> case x of
                  Core.EliminationRecord v1 -> (Flows.bind (Core_.record (Core.projectionTypeName v1) (Rewriting.deannotateTerm reducedArg)) (\fields ->  
                    let matchingFields = (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.projectionField v1)) fields)
                    in (Logic.ifElse (Lists.null matchingFields) (Flows.fail (Strings.cat [
                      "no such field: ",
                      (Core.unName (Core.projectionField v1)),
                      " in ",
                      (Core.unName (Core.projectionTypeName v1)),
                      " record"])) (Flows.pure (Core.fieldTerm (Lists.head matchingFields))))))
                  Core.EliminationUnion v1 -> (Flows.bind (Core_.injection (Core.caseStatementTypeName v1) reducedArg) (\field ->  
                    let matchingFields = (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.fieldName field)) (Core.caseStatementCases v1))
                    in (Logic.ifElse (Lists.null matchingFields) (Maybes.maybe (Flows.fail (Strings.cat [
                      "no such field ",
                      (Core.unName (Core.fieldName field)),
                      " in ",
                      (Core.unName (Core.caseStatementTypeName v1)),
                      " case statement"])) Flows.pure (Core.caseStatementDefault v1)) (Flows.pure (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.fieldTerm (Lists.head matchingFields)),
                      Core.applicationArgument = (Core.fieldTerm field)}))))))
                  Core.EliminationWrap v1 -> (Core_.wrap v1 reducedArg)) elm)
          in  
            let applyIfNullary = (\eager -> \original -> \args ->  
                    let stripped = (Rewriting.deannotateTerm original)
                    in  
                      let forElimination = (\elm -> \args ->  
                              let arg = (Lists.head args)
                              in  
                                let remainingArgs = (Lists.tail args)
                                in (Flows.bind (reduceArg eager (Rewriting.deannotateTerm arg)) (\reducedArg -> Flows.bind (Flows.bind (applyElimination elm reducedArg) (reduce eager)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs))))
                      in  
                        let forLambda = (\l -> \args ->  
                                let param = (Core.lambdaParameter l)
                                in  
                                  let body = (Core.lambdaBody l)
                                  in  
                                    let arg = (Lists.head args)
                                    in  
                                      let remainingArgs = (Lists.tail args)
                                      in (Flows.bind (reduce eager (Rewriting.deannotateTerm arg)) (\reducedArg -> Flows.bind (reduce eager (Rewriting.replaceFreeTermVariable param reducedArg body)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs))))
                        in  
                          let forPrimitive = (\prim -> \arity -> \args ->  
                                  let argList = (Lists.take arity args)
                                  in  
                                    let remainingArgs = (Lists.drop arity args)
                                    in (Flows.bind (Flows.mapList (reduceArg eager) argList) (\reducedArgs ->  
                                      let strippedArgs = (Lists.map Rewriting.deannotateTerm reducedArgs)
                                      in (Flows.bind (Flows.bind (Graph.primitiveImplementation prim strippedArgs) (reduce eager)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs)))))
                          in ((\x -> case x of
                            Core.TermApplication v1 -> (applyIfNullary eager (Core.applicationFunction v1) (Lists.cons (Core.applicationArgument v1) args))
                            Core.TermFunction v1 -> ((\x -> case x of
                              Core.FunctionElimination v2 -> (Logic.ifElse (Lists.null args) (Flows.pure original) (forElimination v2 args))
                              Core.FunctionLambda v2 -> (Logic.ifElse (Lists.null args) (Flows.pure original) (forLambda v2 args))
                              Core.FunctionPrimitive v2 -> (Flows.bind (Lexical.requirePrimitive v2) (\prim ->  
                                let arity = (Arity.primitiveArity prim)
                                in (Logic.ifElse (Equality.gt arity (Lists.length args)) (Flows.pure (applyToArguments original args)) (forPrimitive prim arity args))))) v1)
                            Core.TermVariable v1 -> (Flows.bind (Lexical.dereferenceElement v1) (\mBinding -> Maybes.maybe (Flows.pure (applyToArguments original args)) (\binding -> applyIfNullary eager (Core.bindingTerm binding) args) mBinding))
                            Core.TermLet v1 ->  
                              let bindings = (Core.letBindings v1)
                              in  
                                let body = (Core.letBody v1)
                                in  
                                  let letExpr = (\b -> Core.TermLet (Core.Let {
                                          Core.letBindings = [
                                            b],
                                          Core.letBody = (Core.TermVariable (Core.bindingName b))}))
                                  in  
                                    let expandBinding = (\b -> Core.Binding {
                                            Core.bindingName = (Core.bindingName b),
                                            Core.bindingTerm = (Rewriting.replaceFreeTermVariable (Core.bindingName b) (letExpr b) (Core.bindingTerm b)),
                                            Core.bindingType = (Core.bindingType b)})
                                    in  
                                      let expandedBindings = (Lists.map expandBinding bindings)
                                      in  
                                        let substituteBinding = (\term -> \b -> Rewriting.replaceFreeTermVariable (Core.bindingName b) (Core.bindingTerm b) term)
                                        in  
                                          let substituteAll = (\bs -> \term -> Lists.foldl substituteBinding term bs)
                                          in  
                                            let expandedBody = (substituteAll expandedBindings body)
                                            in (Flows.bind (reduce eager expandedBody) (\reducedBody -> applyIfNullary eager reducedBody args))
                            _ -> (Flows.pure (applyToArguments original args))) stripped))
            in  
              let mapping = (\recurse -> \mid -> Flows.bind (Logic.ifElse (doRecurse eager mid) (recurse mid) (Flows.pure mid)) (\inner -> applyIfNullary eager inner []))
              in (Rewriting.rewriteTermM mapping term)

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: (Core.Term -> Bool)
termIsClosed term = (Sets.null (Rewriting.freeVariablesInTerm term))

termIsValue :: (t0 -> Core.Term -> Bool)
termIsValue g term =  
  let forList = (\els -> Lists.foldl (\b -> \t -> Logic.and b (termIsValue g t)) True els)
  in  
    let checkField = (\f -> termIsValue g (Core.fieldTerm f))
    in  
      let checkFields = (\fields -> Lists.foldl (\b -> \f -> Logic.and b (checkField f)) True fields)
      in  
        let functionIsValue = (\f -> (\x -> case x of
                Core.FunctionElimination v1 -> ((\x -> case x of
                  Core.EliminationWrap _ -> True
                  Core.EliminationRecord _ -> True
                  Core.EliminationUnion v2 -> (Logic.and (checkFields (Core.caseStatementCases v2)) (Maybes.maybe True (termIsValue g) (Core.caseStatementDefault v2)))) v1)
                Core.FunctionLambda v1 -> (termIsValue g (Core.lambdaBody v1))
                Core.FunctionPrimitive _ -> True) f)
        in ((\x -> case x of
          Core.TermApplication _ -> False
          Core.TermEither v1 -> (Eithers.either (\l -> termIsValue g l) (\r -> termIsValue g r) v1)
          Core.TermLiteral _ -> True
          Core.TermFunction v1 -> (functionIsValue v1)
          Core.TermList v1 -> (forList v1)
          Core.TermMap v1 -> (Lists.foldl (\b -> \kv -> Logic.and b (Logic.and (termIsValue g (Pairs.first kv)) (termIsValue g (Pairs.second kv)))) True (Maps.toList v1))
          Core.TermMaybe v1 -> (Maybes.maybe True (termIsValue g) v1)
          Core.TermRecord v1 -> (checkFields (Core.recordFields v1))
          Core.TermSet v1 -> (forList (Sets.toList v1))
          Core.TermUnion v1 -> (checkField (Core.injectionField v1))
          Core.TermUnit -> True
          Core.TermVariable _ -> False
          _ -> False) (Rewriting.deannotateTerm term))
