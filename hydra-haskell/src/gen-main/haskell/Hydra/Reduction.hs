-- | Functions for reducing terms and types, i.e. performing computations.

module Hydra.Reduction where

import qualified Hydra.Arity as Arity
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Expect as Expect
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
import qualified Hydra.Strip as Strip
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Alpha convert a variable in a term
alphaConvert :: (Core.Name -> Core.Term -> Core.Term -> Core.Term)
alphaConvert vold tnew term =  
  let rewrite = (\recurse -> \t -> (\x -> case x of
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionLambda v2 ->  
              let v = (Core.lambdaParameter v2)
              in (Logic.ifElse (Equality.equalString (Core.unName v) (Core.unName vold)) t (recurse t))
            _ -> (recurse t)) v1)
          Core.TermVariable v1 -> (Logic.ifElse (Equality.equalString (Core.unName v1) (Core.unName vold)) tnew (Core.TermVariable v1))
          _ -> (recurse t)) t)
  in (Rewriting.rewriteTerm rewrite term)

betaReduceType :: (Core.Type -> Compute.Flow Graph.Graph Core.Type)
betaReduceType typ =  
  let mapExpr = (\recurse -> \t -> Flows.bind (recurse t) (\r -> (\x -> case x of
          Core.TypeApplication v1 -> (reduceApp v1)
          _ -> (Flows.pure r)) r)) 
      reduceApp = (\app ->  
              let lhs = (Core.applicationTypeFunction app) 
                  rhs = (Core.applicationTypeArgument app)
              in ((\x -> case x of
                Core.TypeAnnotated v1 -> (Flows.bind (reduceApp (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.annotatedTypeSubject v1),
                  Core.applicationTypeArgument = rhs})) (\a -> Flows.pure (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeSubject = a,
                  Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)}))))
                Core.TypeForall v1 -> (betaReduceType (Rewriting.replaceFreeName (Core.forallTypeParameter v1) rhs (Core.forallTypeBody v1)))
                Core.TypeVariable v1 -> (Flows.bind (Schemas.requireType v1) (\t_ -> betaReduceType (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = t_,
                  Core.applicationTypeArgument = rhs}))))) lhs))
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
                  rhs = (Core.applicationArgument v1)
              in ((\x -> case x of
                Core.TermFunction v2 -> ((\x -> case x of
                  Core.FunctionLambda v3 ->  
                    let v = (Core.lambdaParameter v3) 
                        body = (Core.lambdaBody v3)
                    in (Logic.ifElse (Rewriting.isFreeVariableInTerm v body) body (alphaConvert v rhs body))
                  _ -> rec) v2)
                _ -> rec) (Strip.fullyStripTerm lhs))
            _ -> rec) rec))
  in (Rewriting.rewriteTerm rewrite term)

countPrimitiveInvocations :: Bool
countPrimitiveInvocations = True

etaReduceTerm :: (Core.Term -> Core.Term)
etaReduceTerm term =  
  let noChange = term 
      reduceLambda = (\l ->  
              let v = (Core.lambdaParameter l) 
                  d = (Core.lambdaDomain l)
                  body = (Core.lambdaBody l)
              in ((\x -> case x of
                Core.TermAnnotated v1 -> (reduceLambda (Core.Lambda {
                  Core.lambdaParameter = v,
                  Core.lambdaDomain = d,
                  Core.lambdaBody = (Core.annotatedTermSubject v1)}))
                Core.TermApplication v1 ->  
                  let lhs = (Core.applicationFunction v1) 
                      rhs = (Core.applicationArgument v1)
                  in ((\x -> case x of
                    Core.TermAnnotated v2 -> (reduceLambda (Core.Lambda {
                      Core.lambdaParameter = v,
                      Core.lambdaDomain = d,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = lhs,
                        Core.applicationArgument = (Core.annotatedTermSubject v2)}))}))
                    Core.TermVariable v2 -> (Logic.ifElse (Logic.and (Equality.equalString (Core.unName v) (Core.unName v2)) (Logic.not (Rewriting.isFreeVariableInTerm v lhs))) (etaReduceTerm lhs) noChange)
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
expandLambdas :: (Graph.Graph -> Core.Term -> Core.Term)
expandLambdas graph term =  
  let expand = (\args -> \arity -> \t ->  
          let apps = (Lists.foldl (\lhs -> \arg -> Core.TermApplication (Core.Application {
                  Core.applicationFunction = lhs,
                  Core.applicationArgument = arg})) t args) 
              is = (Logic.ifElse (Equality.lteInt32 arity (Lists.length args)) [] (Math.rangeInt32 1 (Math.sub arity (Lists.length args))))
              pad = (\indices -> \t -> Logic.ifElse (Lists.null indices) t (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name (Strings.cat2 "v" (Literals.showInt32 (Lists.head indices)))),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (pad (Lists.tail indices) (Core.TermApplication (Core.Application {
                        Core.applicationFunction = t,
                        Core.applicationArgument = (Core.TermVariable (Core.Name (Strings.cat2 "v" (Literals.showInt32 (Lists.head indices)))))})))}))))
          in (pad is apps)) 
      rewrite = (\args -> \recurse -> \t ->  
              let afterRecursion = (\term -> expand args (expansionArity graph term) term)
              in ((\x -> case x of
                Core.TermApplication v1 ->  
                  let lhs = (Core.applicationFunction v1) 
                      rhs = (Core.applicationArgument v1)
                      erhs = (rewrite [] recurse rhs)
                  in (rewrite (Lists.cons erhs args) recurse lhs)
                _ -> (afterRecursion (recurse t))) t))
  in (contractTerm (Rewriting.rewriteTerm (rewrite []) term))

-- | Calculate the arity for lambda expansion
expansionArity :: (Graph.Graph -> Core.Term -> Int)
expansionArity graph term = ((\x -> case x of
  Core.TermApplication v1 -> (Math.sub (expansionArity graph (Core.applicationFunction v1)) 1)
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination _ -> 1
    Core.FunctionLambda _ -> 0
    Core.FunctionPrimitive v2 -> (Arity.primitiveArity (Optionals.fromJust (Lexical.lookupPrimitive graph v2)))) v1)
  Core.TermVariable v1 -> (Optionals.maybe 0 (\ts -> Arity.typeArity (Core.typeSchemeType ts)) (Optionals.bind (Lexical.lookupElement graph v1) (\el -> Graph.elementType el)))
  _ -> 0) (Strip.fullyStripTerm term))

reduceTerm :: (Bool -> M.Map t0 t1 -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
reduceTerm eager env term =  
  let reduce = (\eager -> reduceTerm eager Maps.empty) 
      doRecurse = (\eager -> \term -> Logic.and eager ((\x -> case x of
              Core.TermFunction v1 -> ((\x -> case x of
                Core.FunctionLambda _ -> False
                _ -> True) v1)
              _ -> True) term))
      reduceArg = (\eager -> \arg -> Logic.ifElse eager (Flows.pure arg) (reduce False arg))
      applyToArguments = (\fun -> \args -> Logic.ifElse (Lists.null args) fun (applyToArguments (Core.TermApplication (Core.Application {
              Core.applicationFunction = fun,
              Core.applicationArgument = (Lists.head args)})) (Lists.tail args)))
      replaceFreeName = (\toReplace -> \replacement -> \term ->  
              let mapping = (\recurse -> \inner -> (\x -> case x of
                      Core.TermFunction v1 -> ((\x -> case x of
                        Core.FunctionLambda v2 -> (Logic.ifElse (Equality.equal (Core.lambdaParameter v2) toReplace) inner (recurse inner))
                        _ -> (recurse inner)) v1)
                      Core.TermVariable v1 -> (Logic.ifElse (Equality.equal v1 toReplace) replacement inner)
                      _ -> (recurse inner)) inner)
              in (Rewriting.rewriteTerm mapping term))
      applyElimination = (\elm -> \reducedArg -> (\x -> case x of
              Core.EliminationRecord v1 -> (Flows.bind (Expect.record (Core.projectionTypeName v1) (Strip.stripTerm reducedArg)) (\fields ->  
                let matchingFields = (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.projectionField v1)) fields)
                in (Logic.ifElse (Lists.null matchingFields) (Flows.fail (Strings.cat [
                  "no such field: ",
                  Core.unName (Core.projectionField v1),
                  " in ",
                  Core.unName (Core.projectionTypeName v1),
                  " record"])) (Flows.pure (Core.fieldTerm (Lists.head matchingFields))))))
              Core.EliminationUnion v1 -> (Flows.bind (Expect.injection (Core.caseStatementTypeName v1) reducedArg) (\field ->  
                let matchingFields = (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.fieldName field)) (Core.caseStatementCases v1))
                in (Logic.ifElse (Lists.null matchingFields) (Optionals.maybe (Flows.fail (Strings.cat [
                  "no such field ",
                  Core.unName (Core.fieldName field),
                  " in ",
                  Core.unName (Core.caseStatementTypeName v1),
                  " case statement"])) Flows.pure (Core.caseStatementDefault v1)) (Flows.pure (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.fieldTerm (Lists.head matchingFields)),
                  Core.applicationArgument = (Core.fieldTerm field)}))))))
              Core.EliminationWrap v1 -> (Expect.wrap v1 reducedArg)) elm)
      applyIfNullary = (\eager -> \original -> \args ->  
              let stripped = (Strip.stripTerm original)
              in ((\x -> case x of
                Core.TermApplication v1 -> (applyIfNullary eager (Core.applicationFunction v1) (Lists.cons (Core.applicationArgument v1) args))
                Core.TermFunction v1 -> ((\x -> case x of
                  Core.FunctionElimination v2 -> (Logic.ifElse (Lists.null args) (Flows.pure original) ( 
                    let arg = (Lists.head args) 
                        remainingArgs = (Lists.tail args)
                    in (Flows.bind (reduceArg eager (Strip.stripTerm arg)) (\reducedArg -> Flows.bind (Flows.bind (applyElimination v2 reducedArg) (reduce eager)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs)))))
                  Core.FunctionLambda v2 -> (Logic.ifElse (Lists.null args) (Flows.pure original) ( 
                    let param = (Core.lambdaParameter v2) 
                        body = (Core.lambdaBody v2)
                        arg = (Lists.head args)
                        remainingArgs = (Lists.tail args)
                    in (Flows.bind (reduce eager (Strip.stripTerm arg)) (\reducedArg -> Flows.bind (reduce eager (replaceFreeName param reducedArg body)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs)))))
                  Core.FunctionPrimitive v2 -> (Flows.bind (Lexical.requirePrimitive v2) (\prim ->  
                    let arity = (Arity.primitiveArity prim)
                    in (Logic.ifElse (Equality.gtInt32 arity (Lists.length args)) (Flows.pure (applyToArguments original args)) ( 
                      let argList = (Lists.take arity args) 
                          remainingArgs = (Lists.drop arity args)
                      in (Flows.bind (Flows.mapList (reduceArg eager) argList) (\reducedArgs -> Flows.bind (Flows.bind (Graph.primitiveImplementation prim reducedArgs) (reduce eager)) (\reducedResult -> applyIfNullary eager reducedResult remainingArgs)))))))
                  _ -> (Flows.pure (applyToArguments original args))) v1)
                Core.TermVariable _ -> (Flows.pure (applyToArguments original args))
                _ -> (Flows.pure (applyToArguments original args))) stripped))
      mapping = (\recurse -> \mid -> Flows.bind (Logic.ifElse (doRecurse eager mid) (recurse mid) (Flows.pure mid)) (\inner -> applyIfNullary eager inner []))
  in (Rewriting.rewriteTermM mapping term)

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: (Core.Term -> Bool)
termIsClosed term = (Sets.null (Rewriting.freeVariablesInTerm term))

termIsValue :: (t0 -> Core.Term -> Bool)
termIsValue g term =  
  let forList = (\els -> Lists.foldl (\b -> \t -> Logic.and b (termIsValue g t)) True els) 
      checkField = (\f -> termIsValue g (Core.fieldTerm f))
      checkFields = (\fields -> Lists.foldl (\b -> \f -> Logic.and b (checkField f)) True fields)
      functionIsValue = (\f -> (\x -> case x of
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
    Core.TermVariable _ -> False
    _ -> False) (Strip.stripTerm term))
