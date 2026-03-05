-- Note: this is an automatically generated file. Do not edit.

-- | Functions for reducing terms and types, i.e. performing computations.

module Hydra.Reduction where

import qualified Hydra.Arity as Arity
import qualified Hydra.Checking as Checking
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Error as Error
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
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
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
betaReduceType :: (Context.Context -> Graph.Graph -> Core.Type -> Either String Core.Type)
betaReduceType cx graph typ =  
  let reduceApp = (\app ->  
          let lhs = (Core.applicationTypeFunction app)
          in  
            let rhs = (Core.applicationTypeArgument app)
            in ((\x -> case x of
              Core.TypeAnnotated v0 -> (Eithers.bind (reduceApp (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.annotatedTypeBody v0),
                Core.applicationTypeArgument = rhs})) (\a -> Right (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = a,
                Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v0)}))))
              Core.TypeForall v0 -> (betaReduceType cx graph (Rewriting.replaceFreeTypeVariable (Core.forallTypeParameter v0) rhs (Core.forallTypeBody v0)))
              Core.TypeVariable v0 -> (Eithers.bind (Eithers.bimap (\ic -> Error.unOtherError (Context.inContextObject ic)) (\x -> x) (Schemas.requireType cx graph v0)) (\t_ -> betaReduceType cx graph (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = t_,
                Core.applicationTypeArgument = rhs}))))) lhs))
  in  
    let mapExpr = (\recurse -> \t ->  
            let findApp = (\r -> (\x -> case x of
                    Core.TypeApplication v0 -> (reduceApp v0)
                    _ -> (Right r)) r)
            in (Eithers.bind (recurse t) (\r -> findApp r)))
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
            Core.TermApplication v0 ->  
              let lhs = (Core.applicationFunction v0)
              in  
                let rhs = (Core.applicationArgument v0)
                in ((\x -> case x of
                  Core.TermFunction v1 -> ((\x -> case x of
                    Core.FunctionLambda v2 ->  
                      let v = (Core.lambdaParameter v2)
                      in  
                        let body = (Core.lambdaBody v2)
                        in (Logic.ifElse (Rewriting.isFreeVariableInTerm v body) body (Rewriting.replaceFreeTermVariable v rhs body))
                    _ -> rec) v1)
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
                  Core.TermAnnotated v0 -> (reduceLambda (Core.Lambda {
                    Core.lambdaParameter = v,
                    Core.lambdaDomain = d,
                    Core.lambdaBody = (Core.annotatedTermBody v0)}))
                  Core.TermApplication v0 ->  
                    let lhs = (Core.applicationFunction v0)
                    in  
                      let rhs = (Core.applicationArgument v0)
                      in ((\x -> case x of
                        Core.TermAnnotated v1 -> (reduceLambda (Core.Lambda {
                          Core.lambdaParameter = v,
                          Core.lambdaDomain = d,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = lhs,
                            Core.applicationArgument = (Core.annotatedTermBody v1)}))}))
                        Core.TermVariable v1 -> (Logic.ifElse (Logic.and (Equality.equal (Core.unName v) (Core.unName v1)) (Logic.not (Rewriting.isFreeVariableInTerm v lhs))) (etaReduceTerm lhs) noChange)
                        _ -> noChange) (etaReduceTerm rhs))
                  _ -> noChange) (etaReduceTerm body)))
    in ((\x -> case x of
      Core.TermAnnotated v0 -> (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (etaReduceTerm (Core.annotatedTermBody v0)),
        Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)}))
      Core.TermFunction v0 -> ((\x -> case x of
        Core.FunctionLambda v1 -> (reduceLambda v1)
        _ -> noChange) v0)
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
                Core.TermApplication v0 ->  
                  let lhs = (Core.applicationFunction v0)
                  in  
                    let rhs = (Core.applicationArgument v0)
                    in  
                      let erhs = (rewrite [] recurse rhs)
                      in (rewrite (Lists.cons erhs args) recurse lhs)
                _ -> (afterRecursion (recurse t2))) t2))
    in (contractTerm (Rewriting.rewriteTerm (rewrite []) term))

-- | Recursively transform terms to eliminate partial application, e.g. 'add 42' becomes '\x.add 42 x'. Uses the Graph to look up types for arity calculation. Bare primitives and variables are NOT expanded; eliminations and partial applications are. This version properly tracks the Graph through nested scopes.
etaExpandTermNew :: (Graph.Graph -> Core.Term -> Core.Term)
etaExpandTermNew tx0 term0 =  
  let termArityWithContext = (\tx -> \term -> (\x -> case x of
          Core.TermAnnotated v0 -> (termArityWithContext tx (Core.annotatedTermBody v0))
          Core.TermApplication v0 -> (Math.sub (termArityWithContext tx (Core.applicationFunction v0)) 1)
          Core.TermFunction v0 -> ((\x -> case x of
            Core.FunctionElimination _ -> 1
            Core.FunctionLambda _ -> 0
            Core.FunctionPrimitive v1 -> (Maybes.maybe 0 Arity.typeSchemeArity (Maps.lookup v1 (Maps.fromList (Lists.map (\_gpt_p -> (Graph.primitiveName _gpt_p, (Graph.primitiveType _gpt_p))) (Maps.elems (Graph.graphPrimitives tx))))))) v0)
          Core.TermLet v0 -> (termArityWithContext (Schemas.extendGraphForLet (\_ -> \_ -> Nothing) tx v0) (Core.letBody v0))
          Core.TermTypeLambda v0 -> (termArityWithContext (Schemas.extendGraphForTypeLambda tx v0) (Core.typeLambdaBody v0))
          Core.TermTypeApplication v0 -> (termArityWithContext tx (Core.typeApplicationTermBody v0))
          Core.TermVariable v0 -> (Maybes.maybe 0 Arity.typeArity (Maybes.map Rewriting.typeSchemeToFType (Maps.lookup v0 (Graph.graphBoundTypes tx))))
          _ -> 0) term)
  in  
    let domainTypes = (\n -> \mt -> Logic.ifElse (Equality.lte n 0) [] (Maybes.maybe (Lists.map (\_ -> Nothing) (Math.range 1 n)) (\typ -> (\x -> case x of
            Core.TypeFunction v0 -> (Lists.cons (Just (Core.functionTypeDomain v0)) (domainTypes (Math.sub n 1) (Just (Core.functionTypeCodomain v0))))
            Core.TypeAnnotated v0 -> (domainTypes n (Just (Core.annotatedTypeBody v0)))
            Core.TypeApplication v0 -> (domainTypes n (Just (Core.applicationTypeFunction v0)))
            Core.TypeForall v0 -> (domainTypes n (Just (Core.forallTypeBody v0)))
            _ -> (Lists.map (\_ -> Nothing) (Math.range 1 n))) typ) mt))
    in  
      let peelFunctionDomains = (\mtyp -> \n -> Logic.ifElse (Equality.lte n 0) mtyp (Maybes.maybe Nothing (\typ -> (\x -> case x of
              Core.TypeFunction v0 -> (peelFunctionDomains (Just (Core.functionTypeCodomain v0)) (Math.sub n 1))
              Core.TypeAnnotated v0 -> (peelFunctionDomains (Just (Core.annotatedTypeBody v0)) n)
              Core.TypeApplication v0 -> (peelFunctionDomains (Just (Core.applicationTypeFunction v0)) n)
              Core.TypeForall v0 -> (peelFunctionDomains (Just (Core.forallTypeBody v0)) n)
              _ -> Nothing) typ) mtyp))
      in  
        let expand = (\alwaysPad -> \args -> \arity -> \headTyp -> \head ->  
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
                        let remainingType = (peelFunctionDomains headTyp numArgs)
                        in  
                          let domains = (domainTypes needed remainingType)
                          in  
                            let codomainType = (peelFunctionDomains remainingType needed)
                            in  
                              let fullyAppliedRaw = (Lists.foldl (\body -> \i ->  
                                      let vn = (Core.Name (Strings.cat2 "v" (Literals.showInt32 i)))
                                      in (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = body,
                                        Core.applicationArgument = (Core.TermVariable vn)}))) applied indices)
                              in  
                                let fullyApplied = (Maybes.maybe fullyAppliedRaw (\ct -> Core.TermAnnotated (Core.AnnotatedTerm {
                                        Core.annotatedTermBody = fullyAppliedRaw,
                                        Core.annotatedTermAnnotation = (Maps.singleton (Core.Name "type") (Core_.type_ ct))})) codomainType)
                                in  
                                  let indexedDomains = (Lists.zip indices domains)
                                  in (Lists.foldl (\body -> \idPair ->  
                                    let i = (Pairs.first idPair)
                                    in  
                                      let dom = (Pairs.second idPair)
                                      in  
                                        let vn = (Core.Name (Strings.cat2 "v" (Literals.showInt32 i)))
                                        in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                          Core.lambdaParameter = vn,
                                          Core.lambdaDomain = dom,
                                          Core.lambdaBody = body})))) fullyApplied (Lists.reverse indexedDomains))) applied))
        in  
          let rewriteWithArgs = (\args -> \tx -> \term ->  
                  let recurse = (\tx1 -> \term1 -> rewriteWithArgs [] tx1 term1)
                  in  
                    let termHeadType = (\tx2 -> \trm2 -> (\x -> case x of
                            Core.TermAnnotated v0 -> (termHeadType tx2 (Core.annotatedTermBody v0))
                            Core.TermFunction v0 -> ((\x -> case x of
                              Core.FunctionPrimitive v1 -> (Maybes.map (\ts2 -> Core.typeSchemeType ts2) (Maps.lookup v1 (Maps.fromList (Lists.map (\_gpt_p -> (Graph.primitiveName _gpt_p, (Graph.primitiveType _gpt_p))) (Maps.elems (Graph.graphPrimitives tx2))))))
                              _ -> Nothing) v0)
                            Core.TermLet v0 -> (termHeadType (Schemas.extendGraphForLet (\_ -> \_ -> Nothing) tx2 v0) (Core.letBody v0))
                            Core.TermTypeLambda v0 -> (termHeadType (Schemas.extendGraphForTypeLambda tx2 v0) (Core.typeLambdaBody v0))
                            Core.TermTypeApplication v0 -> (Maybes.bind (termHeadType tx2 (Core.typeApplicationTermBody v0)) (\htyp2 -> (\x -> case x of
                              Core.TypeForall v1 -> (Just (Rewriting.replaceFreeTypeVariable (Core.forallTypeParameter v1) (Core.typeApplicationTermType v0) (Core.forallTypeBody v1)))
                              _ -> (Just htyp2)) htyp2))
                            Core.TermVariable v0 -> (Maybes.map Rewriting.typeSchemeToFType (Maps.lookup v0 (Graph.graphBoundTypes tx2)))
                            _ -> Nothing) trm2)
                    in  
                      let afterRecursion = (\trm ->  
                              let arity = (termArityWithContext tx trm)
                              in  
                                let hType = (termHeadType tx trm)
                                in (expand False args arity hType trm))
                      in  
                        let forField = (\f -> Core.Field {
                                Core.fieldName = (Core.fieldName f),
                                Core.fieldTerm = (recurse tx (Core.fieldTerm f))})
                        in  
                          let forCaseBranch = (\f ->  
                                  let branchBody = (recurse tx (Core.fieldTerm f))
                                  in  
                                    let arty = (termArityWithContext tx branchBody)
                                    in  
                                      let branchHType = (termHeadType tx branchBody)
                                      in Core.Field {
                                        Core.fieldName = (Core.fieldName f),
                                        Core.fieldTerm = (expand True [] arty branchHType branchBody)})
                          in  
                            let forElimination = (\elm -> (\x -> case x of
                                    Core.EliminationRecord v0 -> (Core.EliminationRecord v0)
                                    Core.EliminationUnion v0 -> (Core.EliminationUnion (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                                      Core.caseStatementDefault = (Maybes.map (\t1 -> recurse tx t1) (Core.caseStatementDefault v0)),
                                      Core.caseStatementCases = (Lists.map forCaseBranch (Core.caseStatementCases v0))}))
                                    Core.EliminationWrap v0 -> (Core.EliminationWrap v0)) elm)
                            in  
                              let forMap = (\mp ->  
                                      let forPair = (\pr -> (recurse tx (Pairs.first pr), (recurse tx (Pairs.second pr))))
                                      in (Maps.fromList (Lists.map forPair (Maps.toList mp))))
                              in ((\x -> case x of
                                Core.TermAnnotated v0 -> (afterRecursion (Core.TermAnnotated (Core.AnnotatedTerm {
                                  Core.annotatedTermBody = (recurse tx (Core.annotatedTermBody v0)),
                                  Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})))
                                Core.TermApplication v0 ->  
                                  let rhs = (rewriteWithArgs [] tx (Core.applicationArgument v0))
                                  in (rewriteWithArgs (Lists.cons rhs args) tx (Core.applicationFunction v0))
                                Core.TermEither v0 -> (afterRecursion (Core.TermEither (Eithers.either (\l -> Left (recurse tx l)) (\r -> Right (recurse tx r)) v0)))
                                Core.TermFunction v0 -> ((\x -> case x of
                                  Core.FunctionElimination v1 ->  
                                    let padElim = ((\x -> case x of
                                            Core.EliminationRecord _ -> False
                                            Core.EliminationUnion _ -> True
                                            Core.EliminationWrap _ -> False) v1)
                                    in  
                                      let elimTerm = (Core.TermFunction (Core.FunctionElimination (forElimination v1)))
                                      in  
                                        let elimHeadType = ((\x -> case x of
                                                Core.EliminationUnion v2 -> (Just (Core.TypeFunction (Core.FunctionType {
                                                  Core.functionTypeDomain = (Core.TypeVariable (Core.caseStatementTypeName v2)),
                                                  Core.functionTypeCodomain = Core.TypeUnit})))
                                                _ -> Nothing) v1)
                                        in (expand padElim args 1 elimHeadType elimTerm)
                                  Core.FunctionLambda v1 ->  
                                    let tx1 = (Schemas.extendGraphForLambda tx v1)
                                    in  
                                      let body = (rewriteWithArgs [] tx1 (Core.lambdaBody v1))
                                      in  
                                        let result = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                                Core.lambdaParameter = (Core.lambdaParameter v1),
                                                Core.lambdaDomain = (Core.lambdaDomain v1),
                                                Core.lambdaBody = body})))
                                        in  
                                          let arty = (termArityWithContext tx result)
                                          in (expand False args arty Nothing result)
                                  Core.FunctionPrimitive v1 ->  
                                    let arty = (termArityWithContext tx term)
                                    in  
                                      let primType = (Maybes.map (\ts -> Core.typeSchemeType ts) (Maps.lookup v1 (Maps.fromList (Lists.map (\_gpt_p -> (Graph.primitiveName _gpt_p, (Graph.primitiveType _gpt_p))) (Maps.elems (Graph.graphPrimitives tx))))))
                                      in (expand False args arty primType term)) v0)
                                Core.TermLet v0 ->  
                                  let tx1 = (Schemas.extendGraphForLet (\_ -> \_ -> Nothing) tx v0)
                                  in  
                                    let mapBinding = (\b -> Core.Binding {
                                            Core.bindingName = (Core.bindingName b),
                                            Core.bindingTerm = (rewriteWithArgs [] tx1 (Core.bindingTerm b)),
                                            Core.bindingType = (Core.bindingType b)})
                                    in  
                                      let result = (Core.TermLet (Core.Let {
                                              Core.letBindings = (Lists.map mapBinding (Core.letBindings v0)),
                                              Core.letBody = (rewriteWithArgs [] tx1 (Core.letBody v0))}))
                                      in (afterRecursion result)
                                Core.TermList v0 -> (afterRecursion (Core.TermList (Lists.map (\el -> recurse tx el) v0)))
                                Core.TermLiteral v0 -> (Core.TermLiteral v0)
                                Core.TermMap v0 -> (afterRecursion (Core.TermMap (forMap v0)))
                                Core.TermMaybe v0 -> (afterRecursion (Core.TermMaybe (Maybes.map (\v -> recurse tx v) v0)))
                                Core.TermPair v0 -> (afterRecursion (Core.TermPair (recurse tx (Pairs.first v0), (recurse tx (Pairs.second v0)))))
                                Core.TermRecord v0 -> (afterRecursion (Core.TermRecord (Core.Record {
                                  Core.recordTypeName = (Core.recordTypeName v0),
                                  Core.recordFields = (Lists.map forField (Core.recordFields v0))})))
                                Core.TermSet v0 -> (afterRecursion (Core.TermSet (Sets.fromList (Lists.map (\el -> recurse tx el) (Sets.toList v0)))))
                                Core.TermTypeApplication v0 -> (afterRecursion (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (recurse tx (Core.typeApplicationTermBody v0)),
                                  Core.typeApplicationTermType = (Core.typeApplicationTermType v0)})))
                                Core.TermTypeLambda v0 ->  
                                  let tx1 = (Schemas.extendGraphForTypeLambda tx v0)
                                  in  
                                    let result = (Core.TermTypeLambda (Core.TypeLambda {
                                            Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
                                            Core.typeLambdaBody = (rewriteWithArgs [] tx1 (Core.typeLambdaBody v0))}))
                                    in (afterRecursion result)
                                Core.TermUnion v0 -> (afterRecursion (Core.TermUnion (Core.Injection {
                                  Core.injectionTypeName = (Core.injectionTypeName v0),
                                  Core.injectionField = (forField (Core.injectionField v0))})))
                                Core.TermUnit -> Core.TermUnit
                                Core.TermVariable v0 ->  
                                  let arty = (termArityWithContext tx term)
                                  in  
                                    let varType = (Maybes.map Rewriting.typeSchemeToFType (Maps.lookup v0 (Graph.graphBoundTypes tx)))
                                    in (expand False args arty varType term)
                                Core.TermWrap v0 -> (afterRecursion (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.wrappedTermTypeName v0),
                                  Core.wrappedTermBody = (recurse tx (Core.wrappedTermBody v0))})))) term))
          in (contractTerm (rewriteWithArgs [] tx0 term0))

-- | Calculate the arity for eta expansion Note: this is a "trusty" function which assumes the graph is well-formed, i.e. no dangling references.
etaExpansionArity :: (Graph.Graph -> Core.Term -> Int)
etaExpansionArity graph term = ((\x -> case x of
  Core.TermAnnotated v0 -> (etaExpansionArity graph (Core.annotatedTermBody v0))
  Core.TermApplication v0 -> (Math.sub (etaExpansionArity graph (Core.applicationFunction v0)) 1)
  Core.TermFunction v0 -> ((\x -> case x of
    Core.FunctionElimination _ -> 1
    Core.FunctionLambda _ -> 0
    Core.FunctionPrimitive v1 -> (Arity.primitiveArity (Maybes.fromJust (Lexical.lookupPrimitive graph v1)))) v0)
  Core.TermTypeLambda v0 -> (etaExpansionArity graph (Core.typeLambdaBody v0))
  Core.TermTypeApplication v0 -> (etaExpansionArity graph (Core.typeApplicationTermBody v0))
  Core.TermVariable v0 -> (Maybes.maybe 0 (\ts -> Arity.typeArity (Core.typeSchemeType ts)) (Maybes.bind (Lexical.lookupElement graph v0) (\b -> Core.bindingType b)))
  _ -> 0) term)

-- | Recursively transform arbitrary terms like 'add 42' into terms like '\x.add 42 x', eliminating partial application. Variable references are not expanded. This is useful for targets like Python with weaker support for currying than Hydra or Haskell. Note: this is a "trusty" function which assumes the graph is well-formed, i.e. no dangling references. It also assumes that type inference has already been performed. After eta expansion, type inference needs to be performed again, as new, untyped lambdas may have been added.
etaExpandTypedTerm :: (Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.OtherError) Core.Term)
etaExpandTypedTerm cx tx0 term0 =  
  let rewrite = (\topLevel -> \forced -> \typeArgs -> \recurse -> \tx -> \term ->  
          let rewriteSpine = (\term -> (\x -> case x of
                  Core.TermAnnotated v0 -> (Eithers.bind (rewriteSpine (Core.annotatedTermBody v0)) (\body ->  
                    let ann = (Core.annotatedTermAnnotation v0)
                    in (Right (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = body,
                      Core.annotatedTermAnnotation = ann})))))
                  Core.TermApplication v0 ->  
                    let l = (Logic.ifElse False [
                            Core.TypeLiteral Core.LiteralTypeString] [])
                    in (Eithers.bind (rewriteSpine (Core.applicationFunction v0)) (\lhs -> Eithers.bind (rewrite True False l recurse tx (Core.applicationArgument v0)) (\rhs -> Right (Core.TermApplication (Core.Application {
                      Core.applicationFunction = lhs,
                      Core.applicationArgument = rhs})))))
                  Core.TermTypeApplication v0 -> (Eithers.bind (rewriteSpine (Core.typeApplicationTermBody v0)) (\body ->  
                    let typ = (Core.typeApplicationTermType v0)
                    in (Right (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = body,
                      Core.typeApplicationTermType = typ})))))
                  _ -> (rewrite False False [] recurse tx term)) term)
          in  
            let arityOf = (\tx -> \term ->  
                    let dflt = (Eithers.map (\_tc -> Arity.typeArity (Pairs.first _tc)) (Checking.typeOf cx tx [] term))
                    in  
                      let forFunction = (\tx -> \f -> (\x -> case x of
                              Core.FunctionElimination _ -> (Right 1)
                              Core.FunctionLambda v0 ->  
                                let txl = (Schemas.extendGraphForLambda tx v0)
                                in (arityOf txl (Core.lambdaBody v0))
                              Core.FunctionPrimitive v0 -> (Eithers.map (\_ts -> Arity.typeSchemeArity _ts) (Lexical.requirePrimitiveType cx tx v0))) f)
                      in ((\x -> case x of
                        Core.TermAnnotated v0 -> (arityOf tx (Core.annotatedTermBody v0))
                        Core.TermFunction v0 -> (forFunction tx v0)
                        Core.TermLet v0 ->  
                          let txl = (Schemas.extendGraphForLet (\_ -> \_ -> Nothing) tx v0)
                          in (arityOf txl (Core.letBody v0))
                        Core.TermTypeApplication v0 -> (arityOf tx (Core.typeApplicationTermBody v0))
                        Core.TermTypeLambda v0 ->  
                          let txt = (Schemas.extendGraphForTypeLambda tx v0)
                          in (arityOf txt (Core.typeLambdaBody v0))
                        Core.TermVariable v0 -> (Maybes.maybe (Eithers.map (\_tc -> Arity.typeArity (Pairs.first _tc)) (Checking.typeOf cx tx [] (Core.TermVariable v0))) (\t -> Right (Arity.typeArity t)) (Maybes.map Rewriting.typeSchemeToFType (Maps.lookup v0 (Graph.graphBoundTypes tx))))
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
                      let forceExpansion = (\t -> Eithers.bind (Checking.typeOf cx tx [] t) (\typCx ->  
                              let arity = (Arity.typeArity (Pairs.first typCx))
                              in (Right (padn arity (unwind t)))))
                      in  
                        let recurseOrForce = (\term -> Logic.ifElse forced (forceExpansion term) (recurse tx (unwind term)))
                        in  
                          let forCase = (\f -> Eithers.bind (rewrite False True [] recurse tx (Core.fieldTerm f)) (\r -> Right (Core.Field {
                                  Core.fieldName = (Core.fieldName f),
                                  Core.fieldTerm = r})))
                          in  
                            let forCaseStatement = (\cs ->  
                                    let tname = (Core.caseStatementTypeName cs)
                                    in  
                                      let dflt = (Core.caseStatementDefault cs)
                                      in  
                                        let cases = (Core.caseStatementCases cs)
                                        in (Eithers.bind (Eithers.mapMaybe (rewrite False False [] recurse tx) dflt) (\rdflt -> Eithers.bind (Eithers.mapList forCase cases) (\rcases -> Right (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                          Core.caseStatementTypeName = tname,
                                          Core.caseStatementDefault = rdflt,
                                          Core.caseStatementCases = rcases}))))))))
                            in  
                              let forElimination = (\elm ->  
                                      let checkBase = (\elm -> (\x -> case x of
                                              Core.EliminationUnion v0 -> (forCaseStatement v0)
                                              _ -> (recurse tx term)) elm)
                                      in (Eithers.bind (Eithers.map unwind (checkBase elm)) (\base -> Right (Logic.ifElse (Logic.or topLevel forced) (padn 1 base) base))))
                              in ((\x -> case x of
                                Core.TermApplication v0 ->  
                                  let lhs = (Core.applicationFunction v0)
                                  in  
                                    let rhs = (Core.applicationArgument v0)
                                    in (Eithers.bind (rewrite True False [] recurse tx rhs) (\rhs2 -> Eithers.bind (arityOf tx lhs) (\lhsarity -> Eithers.bind (rewriteSpine lhs) (\lhs2 ->  
                                      let a2 = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = lhs2,
                                              Core.applicationArgument = rhs2}))
                                      in (Right (Logic.ifElse (Equality.gt lhsarity 1) (padn (Math.sub lhsarity 1) a2) a2))))))
                                Core.TermFunction v0 -> ((\x -> case x of
                                  Core.FunctionElimination v1 -> (forElimination v1)
                                  Core.FunctionLambda v1 ->  
                                    let txl = (Schemas.extendGraphForLambda tx v1)
                                    in (Eithers.map unwind (recurse txl term))
                                  _ -> (recurseOrForce term)) v0)
                                Core.TermLet v0 ->  
                                  let txlt = (Schemas.extendGraphForLet (\_ -> \_ -> Nothing) tx v0)
                                  in (recurse txlt term)
                                Core.TermTypeApplication v0 -> (rewrite topLevel forced (Lists.cons (Core.typeApplicationTermType v0) typeArgs) recurse tx (Core.typeApplicationTermBody v0))
                                Core.TermTypeLambda v0 ->  
                                  let txt = (Schemas.extendGraphForTypeLambda tx v0)
                                  in (recurse txt term)
                                _ -> (recurseOrForce term)) term))
  in (Rewriting.rewriteTermWithContextM (rewrite True False []) tx0 term0)

-- | A term evaluation function which is alternatively lazy or eager
reduceTerm :: (Context.Context -> Graph.Graph -> Bool -> Core.Term -> Either (Context.InContext Error.OtherError) Core.Term)
reduceTerm cx graph eager term =  
  let reduce = (\eager -> reduceTerm cx graph eager)
  in  
    let doRecurse = (\eager -> \term ->  
            let isNonLambda = (\f -> (\x -> case x of
                    Core.FunctionLambda _ -> False
                    _ -> True) f)
            in  
              let isNonLambdaTerm = ((\x -> case x of
                      Core.TermFunction v0 -> (isNonLambda v0)
                      Core.TermLet _ -> False
                      _ -> True) term)
              in (Logic.and eager isNonLambdaTerm))
    in  
      let reduceArg = (\eager -> \arg -> Logic.ifElse eager (Right arg) (reduce False arg))
      in  
        let applyToArguments = (\fun -> \args -> Logic.ifElse (Lists.null args) fun (applyToArguments (Core.TermApplication (Core.Application {
                Core.applicationFunction = fun,
                Core.applicationArgument = (Lists.head args)})) (Lists.tail args)))
        in  
          let mapErrorToOtherError = (\ic -> Context.InContext {
                  Context.inContextObject = (Error.OtherError ((\x -> case x of
                    Error.ErrorDecoding v0 -> (Error.unDecodingError v0)
                    Error.ErrorOther v0 -> (Error.unOtherError v0)
                    Error.ErrorUnification v0 -> (Error.unificationErrorMessage v0)) (Context.inContextObject ic))),
                  Context.inContextContext = (Context.inContextContext ic)})
          in  
            let applyElimination = (\elm -> \reducedArg -> (\x -> case x of
                    Core.EliminationRecord v0 -> (Eithers.bind (Core__.record cx (Core.projectionTypeName v0) graph (Rewriting.deannotateTerm reducedArg)) (\fields ->  
                      let matchingFields = (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.projectionField v0)) fields)
                      in (Logic.ifElse (Lists.null matchingFields) (Left (Context.InContext {
                        Context.inContextObject = (Error.OtherError (Strings.cat [
                          "no such field: ",
                          (Core.unName (Core.projectionField v0)),
                          " in ",
                          (Core.unName (Core.projectionTypeName v0)),
                          " record"])),
                        Context.inContextContext = cx})) (Right (Core.fieldTerm (Lists.head matchingFields))))))
                    Core.EliminationUnion v0 -> (Eithers.bind (Core__.injection cx (Core.caseStatementTypeName v0) graph reducedArg) (\field ->  
                      let matchingFields = (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.fieldName field)) (Core.caseStatementCases v0))
                      in (Logic.ifElse (Lists.null matchingFields) (Maybes.maybe (Left (Context.InContext {
                        Context.inContextObject = (Error.OtherError (Strings.cat [
                          "no such field ",
                          (Core.unName (Core.fieldName field)),
                          " in ",
                          (Core.unName (Core.caseStatementTypeName v0)),
                          " case statement"])),
                        Context.inContextContext = cx})) (\x -> Right x) (Core.caseStatementDefault v0)) (Right (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.fieldTerm (Lists.head matchingFields)),
                        Core.applicationArgument = (Core.fieldTerm field)}))))))
                    Core.EliminationWrap v0 -> (Core__.wrap cx v0 graph reducedArg)) elm)
            in  
              let applyIfNullary = (\eager -> \original -> \args ->  
                      let stripped = (Rewriting.deannotateTerm original)
                      in  
                        let forElimination = (\elm -> \args ->  
                                let arg = (Lists.head args)
                                in  
                                  let remainingArgs = (Lists.tail args)
                                  in (Eithers.bind (reduceArg eager (Rewriting.deannotateTerm arg)) (\reducedArg -> Eithers.bind (Eithers.bind (applyElimination elm reducedArg) (reduce eager)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs))))
                        in  
                          let forLambda = (\l -> \args ->  
                                  let param = (Core.lambdaParameter l)
                                  in  
                                    let body = (Core.lambdaBody l)
                                    in  
                                      let arg = (Lists.head args)
                                      in  
                                        let remainingArgs = (Lists.tail args)
                                        in (Eithers.bind (reduce eager (Rewriting.deannotateTerm arg)) (\reducedArg -> Eithers.bind (reduce eager (Rewriting.replaceFreeTermVariable param reducedArg body)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs))))
                          in  
                            let forPrimitive = (\prim -> \arity -> \args ->  
                                    let argList = (Lists.take arity args)
                                    in  
                                      let remainingArgs = (Lists.drop arity args)
                                      in (Eithers.bind (Eithers.mapList (reduceArg eager) argList) (\reducedArgs ->  
                                        let strippedArgs = (Lists.map Rewriting.deannotateTerm reducedArgs)
                                        in (Eithers.bind (Eithers.bimap mapErrorToOtherError (\x -> x) (Graph.primitiveImplementation prim cx graph strippedArgs)) (\primResult -> Eithers.bind (reduce eager primResult) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs))))))
                            in ((\x -> case x of
                              Core.TermApplication v0 -> (applyIfNullary eager (Core.applicationFunction v0) (Lists.cons (Core.applicationArgument v0) args))
                              Core.TermFunction v0 -> ((\x -> case x of
                                Core.FunctionElimination v1 -> (Logic.ifElse (Lists.null args) (Right original) (forElimination v1 args))
                                Core.FunctionLambda v1 -> (Logic.ifElse (Lists.null args) (Right original) (forLambda v1 args))
                                Core.FunctionPrimitive v1 -> (Eithers.bind (Lexical.requirePrimitive cx graph v1) (\prim ->  
                                  let arity = (Arity.primitiveArity prim)
                                  in (Logic.ifElse (Equality.gt arity (Lists.length args)) (Right (applyToArguments original args)) (forPrimitive prim arity args))))) v0)
                              Core.TermVariable v0 ->  
                                let mBinding = (Lexical.dereferenceElement graph v0)
                                in (Maybes.maybe (Right (applyToArguments original args)) (\binding -> applyIfNullary eager (Core.bindingTerm binding) args) mBinding)
                              Core.TermLet v0 ->  
                                let bindings = (Core.letBindings v0)
                                in  
                                  let body = (Core.letBody v0)
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
                                              in (Eithers.bind (reduce eager expandedBody) (\reducedBody -> applyIfNullary eager reducedBody args))
                              _ -> (Right (applyToArguments original args))) stripped))
              in  
                let mapping = (\recurse -> \mid -> Eithers.bind (Logic.ifElse (doRecurse eager mid) (recurse mid) (Right mid)) (\inner -> applyIfNullary eager inner []))
                in (Rewriting.rewriteTermM mapping term)

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: (Core.Term -> Bool)
termIsClosed term = (Sets.null (Rewriting.freeVariablesInTerm term))

-- | Whether a term has been fully reduced to a value
termIsValue :: (Core.Term -> Bool)
termIsValue term =  
  let forList = (\els -> Lists.foldl (\b -> \t -> Logic.and b (termIsValue t)) True els)
  in  
    let checkField = (\f -> termIsValue (Core.fieldTerm f))
    in  
      let checkFields = (\fields -> Lists.foldl (\b -> \f -> Logic.and b (checkField f)) True fields)
      in  
        let functionIsValue = (\f -> (\x -> case x of
                Core.FunctionElimination v0 -> ((\x -> case x of
                  Core.EliminationWrap _ -> True
                  Core.EliminationRecord _ -> True
                  Core.EliminationUnion v1 -> (Logic.and (checkFields (Core.caseStatementCases v1)) (Maybes.maybe True termIsValue (Core.caseStatementDefault v1)))) v0)
                Core.FunctionLambda v0 -> (termIsValue (Core.lambdaBody v0))
                Core.FunctionPrimitive _ -> True) f)
        in ((\x -> case x of
          Core.TermApplication _ -> False
          Core.TermEither v0 -> (Eithers.either (\l -> termIsValue l) (\r -> termIsValue r) v0)
          Core.TermLiteral _ -> True
          Core.TermFunction v0 -> (functionIsValue v0)
          Core.TermList v0 -> (forList v0)
          Core.TermMap v0 -> (Lists.foldl (\b -> \kv -> Logic.and b (Logic.and (termIsValue (Pairs.first kv)) (termIsValue (Pairs.second kv)))) True (Maps.toList v0))
          Core.TermMaybe v0 -> (Maybes.maybe True termIsValue v0)
          Core.TermRecord v0 -> (checkFields (Core.recordFields v0))
          Core.TermSet v0 -> (forList (Sets.toList v0))
          Core.TermUnion v0 -> (checkField (Core.injectionField v0))
          Core.TermUnit -> True
          Core.TermVariable _ -> False
          _ -> False) (Rewriting.deannotateTerm term))
