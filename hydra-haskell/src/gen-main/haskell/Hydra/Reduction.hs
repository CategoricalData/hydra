-- | Functions for reducing terms and types, i.e. performing computations.

module Hydra.Reduction where

import qualified Hydra.Arity as Arity
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
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

countPrimitiveInvocations :: Bool
countPrimitiveInvocations = True

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
