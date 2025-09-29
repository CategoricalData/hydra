-- | Functions for reducing terms and types, i.e. performing computations.

module Hydra.Reduction where

import qualified Hydra.Arity as Arity
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Alpha convert a variable in a term
alphaConvert :: (Core.Name -> Core.Name -> Core.Term -> Core.Term)
alphaConvert vold vnew term = (Rewriting.replaceFreeTermVariable vold (Core.TermVariable vnew) term)

betaReduceType :: (Core.Type -> Compute.Flow Graph.Graph Core.Type)
betaReduceType typ =  
  let reduceApp = (\app ->  
          let lhs = (Core.applicationTypeFunction app)
          in  
            let rhs = (Core.applicationTypeArgument app)
            in ((\x -> case x of
              Core.TypeAnnotated v1 -> (Flows.bind (reduceApp (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.annotatedTypeSubject v1),
                Core.applicationTypeArgument = rhs})) (\a -> Flows.pure (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeSubject = a,
                Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)}))))
              Core.TypeForall v1 -> (betaReduceType (Rewriting.replaceFreeTypeVariable (Core.forallTypeParameter v1) rhs (Core.forallTypeBody v1)))
              Core.TypeVariable v1 -> (Flows.bind (Schemas.requireType v1) (\t_ -> betaReduceType (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = t_,
                Core.applicationTypeArgument = rhs}))))) lhs))
  in  
    let mapExpr = (\recurse -> \t -> Flows.bind (recurse t) (\r -> (\x -> case x of
            Core.TypeApplication v1 -> (reduceApp v1)
            _ -> (Flows.pure r)) r))
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
                    Core.lambdaBody = (Core.annotatedTermSubject v1)}))
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
                            Core.applicationArgument = (Core.annotatedTermSubject v2)}))}))
                        Core.TermVariable v2 -> (Logic.ifElse (Logic.and (Equality.equal (Core.unName v) (Core.unName v2)) (Logic.not (Rewriting.isFreeVariableInTerm v lhs))) (etaReduceTerm lhs) noChange)
                        _ -> noChange) (etaReduceTerm rhs))
                  _ -> noChange) (etaReduceTerm body)))
    in ((\x -> case x of
      Core.TermAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermSubject = (etaReduceTerm (Core.annotatedTermSubject v1)),
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
              let t2 = (Rewriting.deannotateAndDetypeTerm t)
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
  Core.TermAnnotated v1 -> (etaExpansionArity graph (Core.annotatedTermSubject v1))
  Core.TermApplication v1 -> (Math.sub (etaExpansionArity graph (Core.applicationFunction v1)) 1)
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination _ -> 1
    Core.FunctionLambda _ -> 0
    Core.FunctionPrimitive v2 -> (Arity.primitiveArity (Optionals.fromJust (Lexical.lookupPrimitive graph v2)))) v1)
  Core.TermTypeLambda v1 -> (etaExpansionArity graph (Core.typeLambdaBody v1))
  Core.TermTypeApplication v1 -> (etaExpansionArity graph (Core.typedTermTerm v1))
  Core.TermVariable v1 -> (Optionals.maybe 0 (\ts -> Arity.typeArity (Core.typeSchemeType ts)) (Optionals.bind (Lexical.lookupElement graph v1) (\b -> Core.bindingType b)))
  _ -> 0) term)

-- | A term evaluation function which is alternatively lazy or eager
reduceTerm :: (Bool -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
reduceTerm eager term =  
  let reduce = (\eager -> reduceTerm eager)
  in  
    let doRecurse = (\eager -> \term -> Logic.and eager ((\x -> case x of
            Core.TermFunction v1 -> ((\x -> case x of
              Core.FunctionLambda _ -> False
              _ -> True) v1)
            _ -> True) term))
    in  
      let reduceArg = (\eager -> \arg -> Logic.ifElse eager (Flows.pure arg) (reduce False arg))
      in  
        let applyToArguments = (\fun -> \args -> Logic.ifElse (Lists.null args) fun (applyToArguments (Core.TermApplication (Core.Application {
                Core.applicationFunction = fun,
                Core.applicationArgument = (Lists.head args)})) (Lists.tail args)))
        in  
          let replaceFreeTypeVariable = (\toReplace -> \replacement -> \term ->  
                  let mapping = (\recurse -> \inner -> (\x -> case x of
                          Core.TermFunction v1 -> ((\x -> case x of
                            Core.FunctionLambda v2 -> (Logic.ifElse (Equality.equal (Core.lambdaParameter v2) toReplace) inner (recurse inner))
                            _ -> (recurse inner)) v1)
                          Core.TermVariable v1 -> (Logic.ifElse (Equality.equal v1 toReplace) replacement inner)
                          _ -> (recurse inner)) inner)
                  in (Rewriting.rewriteTerm mapping term))
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
                      in (Logic.ifElse (Lists.null matchingFields) (Optionals.maybe (Flows.fail (Strings.cat [
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
                      in ((\x -> case x of
                        Core.TermApplication v1 -> (applyIfNullary eager (Core.applicationFunction v1) (Lists.cons (Core.applicationArgument v1) args))
                        Core.TermFunction v1 -> ((\x -> case x of
                          Core.FunctionElimination v2 -> (Logic.ifElse (Lists.null args) (Flows.pure original) ( 
                            let arg = (Lists.head args)
                            in  
                              let remainingArgs = (Lists.tail args)
                              in (Flows.bind (reduceArg eager (Rewriting.deannotateTerm arg)) (\reducedArg -> Flows.bind (Flows.bind (applyElimination v2 reducedArg) (reduce eager)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs)))))
                          Core.FunctionLambda v2 -> (Logic.ifElse (Lists.null args) (Flows.pure original) ( 
                            let param = (Core.lambdaParameter v2)
                            in  
                              let body = (Core.lambdaBody v2)
                              in  
                                let arg = (Lists.head args)
                                in  
                                  let remainingArgs = (Lists.tail args)
                                  in (Flows.bind (reduce eager (Rewriting.deannotateTerm arg)) (\reducedArg -> Flows.bind (reduce eager (replaceFreeTypeVariable param reducedArg body)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs)))))
                          Core.FunctionPrimitive v2 -> (Flows.bind (Lexical.requirePrimitive v2) (\prim ->  
                            let arity = (Arity.primitiveArity prim)
                            in (Logic.ifElse (Equality.gt arity (Lists.length args)) (Flows.pure (applyToArguments original args)) ( 
                              let argList = (Lists.take arity args)
                              in  
                                let remainingArgs = (Lists.drop arity args)
                                in (Flows.bind (Flows.mapList (reduceArg eager) argList) (\reducedArgs -> Flows.bind (Flows.bind (Graph.primitiveImplementation prim reducedArgs) (reduce eager)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs)))))))) v1)
                        Core.TermVariable _ -> (Flows.pure (applyToArguments original args))
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
                  Core.EliminationUnion v2 -> (Logic.and (checkFields (Core.caseStatementCases v2)) (Optionals.maybe True (termIsValue g) (Core.caseStatementDefault v2)))) v1)
                Core.FunctionLambda v1 -> (termIsValue g (Core.lambdaBody v1))
                Core.FunctionPrimitive _ -> True) f)
        in ((\x -> case x of
          Core.TermApplication _ -> False
          Core.TermLiteral _ -> True
          Core.TermFunction v1 -> (functionIsValue v1)
          Core.TermList v1 -> (forList v1)
          Core.TermMap v1 -> (Lists.foldl (\b -> \kv -> Logic.and b (Logic.and (termIsValue g (fst kv)) (termIsValue g (snd kv)))) True (Maps.toList v1))
          Core.TermOptional v1 -> (Optionals.maybe True (termIsValue g) v1)
          Core.TermRecord v1 -> (checkFields (Core.recordFields v1))
          Core.TermSet v1 -> (forList (Sets.toList v1))
          Core.TermUnion v1 -> (checkField (Core.injectionField v1))
          Core.TermUnit -> True
          Core.TermVariable _ -> False
          _ -> False) (Rewriting.deannotateTerm term))
