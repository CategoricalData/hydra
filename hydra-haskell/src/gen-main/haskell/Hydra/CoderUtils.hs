-- Note: this is an automatically generated file. Do not edit.

-- | Common utilities for language coders, providing shared patterns for term decomposition and analysis.

module Hydra.CoderUtils where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Arity as Arity
import qualified Hydra.Checking as Checking
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Normalize a comment string for consistent output across coders
normalizeComment :: (String -> String)
normalizeComment s =  
  let stripped = (Formatting.stripLeadingAndTrailingWhitespace s)
  in (Logic.ifElse (Strings.null stripped) "" ( 
    let lastIdx = (Math.sub (Strings.length stripped) 1)
    in  
      let lastChar = (Strings.charAt lastIdx stripped)
      in (Logic.ifElse (Equality.equal lastChar 46) stripped (Strings.cat2 stripped "."))))

-- | Gather applications from a term, returning (args, baseTerm)
gatherApplications :: (Core.Term -> ([Core.Term], Core.Term))
gatherApplications term =  
  let go = (\args -> \t -> (\x -> case x of
          Core.TermApplication v1 ->  
            let lhs = (Core.applicationFunction v1)
            in  
              let rhs = (Core.applicationArgument v1)
              in (go (Lists.cons rhs args) lhs)
          _ -> (args, t)) (Rewriting.deannotateTerm t))
  in (go [] term)

-- | Gather term arguments, stripping type-level constructs
gatherArgs :: (Core.Term -> [Core.Term] -> (Core.Term, [Core.Term]))
gatherArgs term args = ((\x -> case x of
  Core.TermApplication v1 ->  
    let lhs = (Core.applicationFunction v1)
    in  
      let rhs = (Core.applicationArgument v1)
      in (gatherArgs lhs (Lists.cons rhs args))
  Core.TermTypeLambda v1 ->  
    let body = (Core.typeLambdaBody v1)
    in (gatherArgs body args)
  Core.TermTypeApplication v1 ->  
    let body = (Core.typeApplicationTermBody v1)
    in (gatherArgs body args)
  _ -> (term, args)) (Rewriting.deannotateTerm term))

-- | Gather term and type arguments from a term
gatherArgsWithTypeApps :: (Core.Term -> [Core.Term] -> [Core.Type] -> (Core.Term, ([Core.Term], [Core.Type])))
gatherArgsWithTypeApps term args tyArgs = ((\x -> case x of
  Core.TermApplication v1 ->  
    let lhs = (Core.applicationFunction v1)
    in  
      let rhs = (Core.applicationArgument v1)
      in (gatherArgsWithTypeApps lhs (Lists.cons rhs args) tyArgs)
  Core.TermTypeLambda v1 ->  
    let body = (Core.typeLambdaBody v1)
    in (gatherArgsWithTypeApps body args tyArgs)
  Core.TermTypeApplication v1 ->  
    let body = (Core.typeApplicationTermBody v1)
    in  
      let typ = (Core.typeApplicationTermType v1)
      in (gatherArgsWithTypeApps body args (Lists.cons typ tyArgs))
  _ -> (term, (args, tyArgs))) (Rewriting.deannotateTerm term))

-- | Check if a term can be encoded as a simple assignment
isSimpleAssignment :: (Core.Term -> Bool)
isSimpleAssignment term = ((\x -> case x of
  Core.TermAnnotated v1 -> (isSimpleAssignment (Core.annotatedTermBody v1))
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionLambda _ -> False
    _ -> True) v1)
  Core.TermLet _ -> False
  Core.TermTypeLambda _ -> False
  Core.TermTypeApplication v1 -> (isSimpleAssignment (Core.typeApplicationTermBody v1))
  _ ->  
    let baseTerm = (Pairs.first (gatherArgs term []))
    in ((\x -> case x of
      Core.TermFunction v1 -> ((\x -> case x of
        Core.FunctionElimination v2 -> ((\x -> case x of
          Core.EliminationUnion _ -> False
          _ -> True) v2)
        _ -> True) v1)
      _ -> True) baseTerm)) term)

-- | Check if a term needs to be treated as a function rather than a simple value
isComplexTerm :: (Typing.TypeContext -> Core.Term -> Bool)
isComplexTerm tc t = ((\x -> case x of
  Core.TermLet _ -> True
  Core.TermTypeApplication _ -> True
  Core.TermTypeLambda _ -> True
  Core.TermVariable v1 -> (isComplexVariable tc v1)
  _ -> (Lists.foldl (\b -> \sub -> Logic.or b (isComplexTerm tc sub)) False (Rewriting.subterms t))) t)

-- | Check if a variable is bound to a complex term
isComplexVariable :: (Typing.TypeContext -> Core.Name -> Bool)
isComplexVariable tc name =  
  let metaLookup = (Maps.lookup name (Typing.typeContextMetadata tc))
  in (Logic.ifElse (Maybes.isJust metaLookup) True (Logic.ifElse (Sets.member name (Typing.typeContextLambdaVariables tc)) True ( 
    let typeLookup = (Maps.lookup name (Typing.typeContextTypes tc))
    in (Logic.not (Maybes.isJust typeLookup)))))

-- | Check if a binding needs to be treated as a function
isComplexBinding :: (Typing.TypeContext -> Core.Binding -> Bool)
isComplexBinding tc b =  
  let term = (Core.bindingTerm b)
  in  
    let mts = (Core.bindingType b)
    in (Maybes.cases mts (isComplexTerm tc term) (\ts ->  
      let isPolymorphic = (Logic.not (Lists.null (Core.typeSchemeVariables ts)))
      in  
        let isNonNullary = (Equality.gt (Arity.typeArity (Core.typeSchemeType ts)) 0)
        in  
          let isComplex = (isComplexTerm tc term)
          in (Logic.or (Logic.or isPolymorphic isNonNullary) isComplex)))

-- | Extract comments/description from a Binding
commentsFromElement :: (Core.Binding -> Compute.Flow Graph.Graph (Maybe String))
commentsFromElement b = (Annotations.getTermDescription (Core.bindingTerm b))

-- | Extract comments/description from a FieldType
commentsFromFieldType :: (Core.FieldType -> Compute.Flow Graph.Graph (Maybe String))
commentsFromFieldType ft = (Annotations.getTypeDescription (Core.fieldTypeType ft))

tryTypeOf :: (String -> Typing.TypeContext -> Core.Term -> Compute.Flow t0 Core.Type)
tryTypeOf msg tc term = (Monads.withTrace msg (Checking.typeOf tc [] term))

-- | Produces metadata for a binding if it is complex
bindingMetadata :: (Typing.TypeContext -> Core.Binding -> Maybe Core.Term)
bindingMetadata tc b = (Logic.ifElse (isComplexBinding tc b) (Just (Core.TermLiteral (Core.LiteralBoolean True))) Nothing)

analyzeFunctionTermWith :: ((Typing.TypeContext -> Core.Binding -> Maybe Core.Term) -> (t0 -> Typing.TypeContext) -> (Typing.TypeContext -> t0 -> t0) -> t0 -> Core.Term -> Compute.Flow t1 (Typing.FunctionStructure t0))
analyzeFunctionTermWith forBinding getTC setTC env term =  
  let finish = (\fEnv -> \tparams -> \args -> \bindings -> \doms -> \tapps -> \body ->  
          let bodyWithTapps = (Lists.foldl (\trm -> \typ -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = trm,
                  Core.typeApplicationTermType = typ})) body tapps)
          in (Flows.bind (tryTypeOf "analyzeFunctionTermWith" (getTC fEnv) bodyWithTapps) (\typ -> Flows.pure (Typing.FunctionStructure {
            Typing.functionStructureTypeParams = (Lists.reverse tparams),
            Typing.functionStructureParams = (Lists.reverse args),
            Typing.functionStructureBindings = bindings,
            Typing.functionStructureBody = bodyWithTapps,
            Typing.functionStructureDomains = (Lists.reverse doms),
            Typing.functionStructureCodomain = (Just typ),
            Typing.functionStructureEnvironment = fEnv})))) 
      gather = (\argMode -> \gEnv -> \tparams -> \args -> \bindings -> \doms -> \tapps -> \t -> (\x -> case x of
              Core.TermFunction v1 -> ((\x -> case x of
                Core.FunctionLambda v2 -> (Logic.ifElse argMode ( 
                  let v = (Core.lambdaParameter v2)
                  in  
                    let dom = (Maybes.maybe (Core.TypeVariable (Core.Name "_")) (\x_ -> x_) (Core.lambdaDomain v2))
                    in  
                      let body = (Core.lambdaBody v2)
                      in  
                        let newEnv = (setTC (Schemas.extendTypeContextForLambda (getTC gEnv) v2) gEnv)
                        in (gather argMode newEnv tparams (Lists.cons v args) bindings (Lists.cons dom doms) tapps body)) (finish gEnv tparams args bindings doms tapps t))
                _ -> (finish gEnv tparams args bindings doms tapps t)) v1)
              Core.TermLet v1 ->  
                let newBindings = (Core.letBindings v1)
                in  
                  let body = (Core.letBody v1)
                  in  
                    let newEnv = (setTC (Schemas.extendTypeContextForLet forBinding (getTC gEnv) v1) gEnv)
                    in (gather False newEnv tparams args (Lists.concat2 bindings newBindings) doms tapps body)
              Core.TermTypeApplication v1 ->  
                let taBody = (Core.typeApplicationTermBody v1)
                in  
                  let typ = (Core.typeApplicationTermType v1)
                  in (gather argMode gEnv tparams args bindings doms (Lists.cons typ tapps) taBody)
              Core.TermTypeLambda v1 ->  
                let tvar = (Core.typeLambdaParameter v1)
                in  
                  let tlBody = (Core.typeLambdaBody v1)
                  in  
                    let newEnv = (setTC (Schemas.extendTypeContextForTypeLambda (getTC gEnv) v1) gEnv)
                    in (gather argMode newEnv (Lists.cons tvar tparams) args bindings doms tapps tlBody)
              _ -> (finish gEnv tparams args bindings doms tapps t)) (Rewriting.deannotateTerm t))
  in (gather True env [] [] [] [] [] term)

analyzeFunctionTermNoInferWith :: ((Typing.TypeContext -> Core.Binding -> Maybe Core.Term) -> (t0 -> Typing.TypeContext) -> (Typing.TypeContext -> t0 -> t0) -> t0 -> Core.Term -> Compute.Flow t1 (Typing.FunctionStructure t0))
analyzeFunctionTermNoInferWith forBinding getTC setTC env term =  
  let finish = (\fEnv -> \tparams -> \args -> \bindings -> \doms -> \tapps -> \body ->  
          let bodyWithTapps = (Lists.foldl (\trm -> \typ -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = trm,
                  Core.typeApplicationTermType = typ})) body tapps)
          in (Flows.pure (Typing.FunctionStructure {
            Typing.functionStructureTypeParams = (Lists.reverse tparams),
            Typing.functionStructureParams = (Lists.reverse args),
            Typing.functionStructureBindings = bindings,
            Typing.functionStructureBody = bodyWithTapps,
            Typing.functionStructureDomains = (Lists.reverse doms),
            Typing.functionStructureCodomain = Nothing,
            Typing.functionStructureEnvironment = fEnv}))) 
      gather = (\argMode -> \gEnv -> \tparams -> \args -> \bindings -> \doms -> \tapps -> \t -> (\x -> case x of
              Core.TermFunction v1 -> ((\x -> case x of
                Core.FunctionLambda v2 -> (Logic.ifElse argMode ( 
                  let v = (Core.lambdaParameter v2)
                  in  
                    let dom = (Maybes.maybe (Core.TypeVariable (Core.Name "_")) (\x_ -> x_) (Core.lambdaDomain v2))
                    in  
                      let body = (Core.lambdaBody v2)
                      in  
                        let newEnv = (setTC (Schemas.extendTypeContextForLambda (getTC gEnv) v2) gEnv)
                        in (gather argMode newEnv tparams (Lists.cons v args) bindings (Lists.cons dom doms) tapps body)) (finish gEnv tparams args bindings doms tapps t))
                _ -> (finish gEnv tparams args bindings doms tapps t)) v1)
              Core.TermLet v1 ->  
                let newBindings = (Core.letBindings v1)
                in  
                  let body = (Core.letBody v1)
                  in  
                    let newEnv = (setTC (Schemas.extendTypeContextForLet forBinding (getTC gEnv) v1) gEnv)
                    in (gather False newEnv tparams args (Lists.concat2 bindings newBindings) doms tapps body)
              Core.TermTypeApplication v1 ->  
                let taBody = (Core.typeApplicationTermBody v1)
                in  
                  let typ = (Core.typeApplicationTermType v1)
                  in (gather argMode gEnv tparams args bindings doms (Lists.cons typ tapps) taBody)
              Core.TermTypeLambda v1 ->  
                let tvar = (Core.typeLambdaParameter v1)
                in  
                  let tlBody = (Core.typeLambdaBody v1)
                  in  
                    let newEnv = (setTC (Schemas.extendTypeContextForTypeLambda (getTC gEnv) v1) gEnv)
                    in (gather argMode newEnv (Lists.cons tvar tparams) args bindings doms tapps tlBody)
              _ -> (finish gEnv tparams args bindings doms tapps t)) (Rewriting.deannotateTerm t))
  in (gather True env [] [] [] [] [] term)

analyzeFunctionTerm :: ((t0 -> Typing.TypeContext) -> (Typing.TypeContext -> t0 -> t0) -> t0 -> Core.Term -> Compute.Flow t1 (Typing.FunctionStructure t0))
analyzeFunctionTerm getTC setTC env term = (analyzeFunctionTermWith bindingMetadata getTC setTC env term)

analyzeFunctionTermInline :: ((t0 -> Typing.TypeContext) -> (Typing.TypeContext -> t0 -> t0) -> t0 -> Core.Term -> Compute.Flow t1 (Typing.FunctionStructure t0))
analyzeFunctionTermInline getTC setTC env term = (analyzeFunctionTermWith (\_ -> \_ -> Nothing) getTC setTC env term)

analyzeFunctionTermNoInfer :: ((t0 -> Typing.TypeContext) -> (Typing.TypeContext -> t0 -> t0) -> t0 -> Core.Term -> Compute.Flow t1 (Typing.FunctionStructure t0))
analyzeFunctionTermNoInfer getTC setTC env term = (analyzeFunctionTermNoInferWith bindingMetadata getTC setTC env term)

updateCoderMetadata :: ((t0 -> t1) -> (t2 -> t3 -> t0) -> (t0 -> t2) -> (t1 -> t3) -> Compute.Flow t0 ())
updateCoderMetadata getMeta makeCoder getGraph f = (Flows.bind Monads.getState (\st -> Monads.putState (makeCoder (getGraph st) (f (getMeta st)))))

withUpdatedCoderGraph :: ((t0 -> t1) -> (t0 -> t2) -> (t1 -> t2 -> t0) -> (t1 -> t1) -> Compute.Flow t0 t3 -> Compute.Flow t0 t3)
withUpdatedCoderGraph getGraph getMeta makeCoder f flow = (Flows.bind Monads.getState (\st -> Flows.bind (Monads.putState (makeCoder (f (getGraph st)) (getMeta st))) (\_ -> Flows.bind flow (\r -> Flows.bind Monads.getState (\st2 -> Flows.bind (Monads.putState (makeCoder (getGraph st) (getMeta st2))) (\_ -> Flows.pure r))))))

withGraphBindings :: ((t0 -> Graph.Graph) -> (Graph.Graph -> t1 -> t0) -> (t0 -> t1) -> [Core.Binding] -> Compute.Flow t0 t2 -> Compute.Flow t0 t2)
withGraphBindings getGraph makeCoder getMeta bindings flow = (withUpdatedCoderGraph getGraph getMeta makeCoder (Lexical.extendGraphWithBindings bindings) flow)

inCoderGraphContext :: ((t0 -> t1) -> (t0 -> t2) -> (t1 -> t2 -> t0) -> Compute.Flow t1 t3 -> Compute.Flow t0 t3)
inCoderGraphContext getGraph getMeta makeCoder graphFlow = (Flows.bind Monads.getState (\st -> Flows.bind (Monads.withState (getGraph st) (Flows.bind graphFlow (\ret -> Flows.bind Monads.getState (\g2 -> Flows.pure (ret, g2))))) (\result -> Flows.bind (Monads.putState (makeCoder (Pairs.second result) (getMeta st))) (\_ -> Flows.pure (Pairs.first result)))))
