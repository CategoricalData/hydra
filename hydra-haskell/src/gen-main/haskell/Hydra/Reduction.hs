-- Note: this is an automatically generated file. Do not edit.

-- | Functions for reducing terms and types, i.e. performing computations.

module Hydra.Reduction where

import qualified Hydra.Accessors as Accessors
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
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
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
                        Core.TermVariable v1 -> (Maybes.maybe (Flows.fail (Strings.cat [
                          "unbound variable: ",
                          (Core.unName v1)])) (\t -> Flows.pure (Arity.typeArity t)) (Maps.lookup v1 (Typing.typeContextTypes tx)))
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

-- | Hoist case statements into bindings in the nearest enclosing let term. This is useful for targets such as Python which only support case statements (match) at the top level.
hoistCaseStatements :: (Typing.TypeContext -> Core.Term -> Core.Term)
hoistCaseStatements = (hoistSubterms (\path -> \term ->  
  let isUnionElim = ((\x -> case x of
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionElimination v2 -> ((\x -> case x of
              Core.EliminationUnion _ -> True
              _ -> False) v2)
            _ -> False) v1)
          _ -> False) term)
  in (Logic.ifElse (Logic.not isUnionElim) False ( 
    let innerPath = (Lists.tail path)
    in (Logic.ifElse (Lists.null innerPath) False ( 
      let allAppLhs = (Lists.foldl (\acc -> \accessor -> Logic.and acc ((\x -> case x of
              Accessors.TermAccessorApplicationFunction -> True
              _ -> False) accessor)) True innerPath)
      in (Logic.not allAppLhs)))))))

-- | Hoist subterms into bindings in the nearest enclosing let term, based on a predicate. The predicate receives the path (from the let body/binding root) and the term, and returns True if the term should be hoisted. When hoisting a term that contains free variables which are lambda-bound between the enclosing let and the current position, those variables are captured: the hoisted binding is wrapped in lambdas for those variables, and the reference is replaced with an application of those variables. Useful for targets that require certain constructs (e.g., case statements) at the top level.
hoistSubterms :: (([Accessors.TermAccessor] -> Core.Term -> Bool) -> Typing.TypeContext -> Core.Term -> Core.Term)
hoistSubterms shouldHoist cx0 term0 =  
  let outerRewrite = (\recurse -> \cx -> \counter -> \term ->  
          let dflt = (recurse counter term)
          in  
            let forLet = (\l ->  
                    let body = (Core.letBody l)
                    in  
                      let bindings = (Core.letBindings l)
                      in  
                        let parentLambdaVars = (Typing.typeContextLambdaVariables cx)
                        in  
                          let processSubterm = (\counterAndExtra -> \subterm ->  
                                  let counter0 = (Pairs.first counterAndExtra)
                                  in  
                                    let extraBindings0 = (Pairs.second counterAndExtra)
                                    in  
                                      let innerRewrite = (\recurse -> \path -> \acc -> \term ->  
                                              let counterExtra = (Pairs.first acc)
                                              in  
                                                let innerCx = (Pairs.second acc)
                                                in ((\x -> case x of
                                                  Core.TermLet v1 ->  
                                                    let nestedResult = (forLet v1)
                                                    in (((Pairs.first nestedResult, (Pairs.second counterExtra)), innerCx), (Pairs.second nestedResult))
                                                  _ ->  
                                                    let subCx = ((\x -> case x of
                                                            Core.TermFunction v1 -> ((\x -> case x of
                                                              Core.FunctionLambda v2 -> (Schemas.extendTypeContextForLambda innerCx v2)
                                                              _ -> innerCx) v1)
                                                            Core.TermLet v1 -> (Schemas.extendTypeContextForLet (\_ -> \_ -> Nothing) innerCx v1)
                                                            Core.TermTypeLambda v1 -> (Schemas.extendTypeContextForTypeLambda innerCx v1)
                                                            _ -> innerCx) term)
                                                    in  
                                                      let processed = (recurse path (counterExtra, subCx) term)
                                                      in  
                                                        let newAcc = (Pairs.first processed)
                                                        in  
                                                          let newCounterExtra = (Pairs.first newAcc)
                                                          in  
                                                            let processedTerm = (Pairs.second processed)
                                                            in (Logic.ifElse (shouldHoist path processedTerm) ( 
                                                              let counter = (Pairs.first newCounterExtra)
                                                              in  
                                                                let extraBindings = (Pairs.second newCounterExtra)
                                                                in  
                                                                  let bindingName = (Core.Name (Strings.cat2 "_hoist_" (Literals.showInt32 counter)))
                                                                  in  
                                                                    let currentLambdaVars = (Typing.typeContextLambdaVariables innerCx)
                                                                    in  
                                                                      let newLambdaVars = (Sets.difference currentLambdaVars parentLambdaVars)
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
                                                                              let newBinding = Core.Binding {
                                                                                      Core.bindingName = bindingName,
                                                                                      Core.bindingTerm = wrappedTerm,
                                                                                      Core.bindingType = Nothing}
                                                                              in  
                                                                                let reference = (Lists.foldl (\fn -> \varName -> Core.TermApplication (Core.Application {
                                                                                        Core.applicationFunction = fn,
                                                                                        Core.applicationArgument = (Core.TermVariable varName)})) (Core.TermVariable bindingName) capturedVars)
                                                                                in (((Math.add counter 1, (Lists.concat2 extraBindings [
                                                                                  newBinding])), innerCx), reference)) ((newCounterExtra, innerCx), processedTerm))) term))
                                      in  
                                        let result = (Rewriting.rewriteAndFoldTermWithPath innerRewrite ((counter0, extraBindings0), cx) subterm)
                                        in (Pairs.first (Pairs.first result), (Pairs.second result)))
                          in  
                            let pBody = (processSubterm (counter, []) body)
                            in  
                              let counterExtra1 = (Pairs.first pBody)
                              in  
                                let newBody = (Pairs.second pBody)
                                in  
                                  let foldBinding = (\acc -> \binding ->  
                                          let counterExtra = (Pairs.first acc)
                                          in  
                                            let newBindingsSoFar = (Pairs.second acc)
                                            in  
                                              let p = (processSubterm counterExtra (Core.bindingTerm binding))
                                              in  
                                                let newCounterExtra = (Pairs.first p)
                                                in  
                                                  let newTerm = (Pairs.second p)
                                                  in  
                                                    let newBinding = Core.Binding {
                                                            Core.bindingName = (Core.bindingName binding),
                                                            Core.bindingTerm = newTerm,
                                                            Core.bindingType = (Core.bindingType binding)}
                                                    in (newCounterExtra, (Lists.concat2 newBindingsSoFar [
                                                      newBinding])))
                                  in  
                                    let pBindings = (Lists.foldl foldBinding (counterExtra1, []) bindings)
                                    in  
                                      let counterExtra2 = (Pairs.first pBindings)
                                      in  
                                        let counter2 = (Pairs.first counterExtra2)
                                        in  
                                          let extraBindings = (Pairs.second counterExtra2)
                                          in  
                                            let newBindings = (Pairs.second pBindings)
                                            in  
                                              let allBindings = (Lists.concat2 newBindings extraBindings)
                                              in (counter2, (Core.TermLet (Core.Let {
                                                Core.letBindings = allBindings,
                                                Core.letBody = newBody}))))
            in ((\x -> case x of
              Core.TermLet v1 -> (forLet v1)
              _ -> dflt) term))
  in (Pairs.second (rewriteAndFoldTermWithTypeContext outerRewrite cx0 1 term0))

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
                      Core.unName (Core.projectionField v1),
                      " in ",
                      Core.unName (Core.projectionTypeName v1),
                      " record"])) (Flows.pure (Core.fieldTerm (Lists.head matchingFields))))))
                  Core.EliminationUnion v1 -> (Flows.bind (Core_.injection (Core.caseStatementTypeName v1) reducedArg) (\field ->  
                    let matchingFields = (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.fieldName field)) (Core.caseStatementCases v1))
                    in (Logic.ifElse (Lists.null matchingFields) (Maybes.maybe (Flows.fail (Strings.cat [
                      "no such field ",
                      Core.unName (Core.fieldName field),
                      " in ",
                      Core.unName (Core.caseStatementTypeName v1),
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
                  in ((Pairs.first fResult, cx1), (Pairs.second fResult)))
  in  
    let result = (Rewriting.rewriteAndFoldTerm wrapper (val0, cx0) term0)
    in (Pairs.first (Pairs.first result), (Pairs.second result))

rewriteAndFoldTermWithTypeContextAndPath :: ((((t0, (t1, Core.Term)) -> t2) -> [t0] -> Typing.TypeContext -> t1 -> Core.Term -> t2) -> Typing.TypeContext -> t1 -> Core.Term -> t2)
rewriteAndFoldTermWithTypeContextAndPath f cx0 val0 term0 =  
  let f2 = (\recurse -> \path -> \cx -> \val -> \term ->  
          let recurse1 = (\accessorValTerm ->  
                  let accessor = (Pairs.first accessorValTerm)
                  in  
                    let v = (Pairs.first (Pairs.second accessorValTerm))
                    in  
                      let t = (Pairs.second (Pairs.second accessorValTerm))
                      in  
                        let newPath = (Lists.concat2 path [
                                accessor])
                        in (recurse newPath cx v t))
          in ((\x -> case x of
            Core.TermFunction v1 -> ((\x -> case x of
              Core.FunctionLambda v2 ->  
                let cx1 = (Schemas.extendTypeContextForLambda cx v2)
                in  
                  let recurse2 = (\accessorValTerm ->  
                          let accessor = (Pairs.first accessorValTerm)
                          in  
                            let v = (Pairs.first (Pairs.second accessorValTerm))
                            in  
                              let t = (Pairs.second (Pairs.second accessorValTerm))
                              in  
                                let newPath = (Lists.concat2 path [
                                        accessor])
                                in (recurse newPath cx1 v t))
                  in (f recurse2 path cx1 val term)
              _ -> (f recurse1 path cx val term)) v1)
            Core.TermLet v1 ->  
              let cx1 = (Schemas.extendTypeContextForLet (\_ -> \_ -> Nothing) cx v1)
              in  
                let recurse2 = (\accessorValTerm ->  
                        let accessor = (Pairs.first accessorValTerm)
                        in  
                          let v = (Pairs.first (Pairs.second accessorValTerm))
                          in  
                            let t = (Pairs.second (Pairs.second accessorValTerm))
                            in  
                              let newPath = (Lists.concat2 path [
                                      accessor])
                              in (recurse newPath cx1 v t))
                in (f recurse2 path cx1 val term)
            Core.TermTypeLambda v1 ->  
              let cx1 = (Schemas.extendTypeContextForTypeLambda cx v1)
              in  
                let recurse2 = (\accessorValTerm ->  
                        let accessor = (Pairs.first accessorValTerm)
                        in  
                          let v = (Pairs.first (Pairs.second accessorValTerm))
                          in  
                            let t = (Pairs.second (Pairs.second accessorValTerm))
                            in  
                              let newPath = (Lists.concat2 path [
                                      accessor])
                              in (recurse newPath cx1 v t))
                in (f recurse2 path cx1 val term)
            _ -> (f recurse1 path cx val term)) term))
  in  
    let rewrite = (\path -> \cx -> \val -> \term -> f2 rewrite path cx val term)
    in (rewrite [] cx0 val0 term0)

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
