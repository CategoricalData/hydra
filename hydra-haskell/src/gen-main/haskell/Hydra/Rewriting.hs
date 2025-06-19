-- | Utilities for type and term rewriting and analysis.

module Hydra.Rewriting where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Strip as Strip
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

foldOverTerm :: (Coders.TraversalOrder -> (t0 -> Core.Term -> t0) -> t0 -> Core.Term -> t0)
foldOverTerm order fld b0 term = ((\x -> case x of
  Coders.TraversalOrderPre -> (Lists.foldl (foldOverTerm order fld) (fld b0 term) (subterms term))
  Coders.TraversalOrderPost -> (fld (Lists.foldl (foldOverTerm order fld) b0 (subterms term)) term)) order)

foldOverType :: (Coders.TraversalOrder -> (t0 -> Core.Type -> t0) -> t0 -> Core.Type -> t0)
foldOverType order fld b0 typ = ((\x -> case x of
  Coders.TraversalOrderPre -> (Lists.foldl (foldOverType order fld) (fld b0 typ) (subtypes typ))
  Coders.TraversalOrderPost -> (fld (Lists.foldl (foldOverType order fld) b0 (subtypes typ)) typ)) order)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a term
freeVariablesInTerm :: (Core.Term -> S.Set Core.Name)
freeVariablesInTerm term =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInTerm t)) Sets.empty (subterms term))
  in ((\x -> case x of
    Core.TermFunction v1 -> ((\x -> case x of
      Core.FunctionLambda v2 -> (Sets.delete (Core.lambdaParameter v2) (freeVariablesInTerm (Core.lambdaBody v2)))
      _ -> dfltVars) v1)
    Core.TermVariable v1 -> (Sets.singleton v1)
    _ -> dfltVars) term)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a type
freeVariablesInType :: (Core.Type -> S.Set Core.Name)
freeVariablesInType typ =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInType t)) Sets.empty (subtypes typ))
  in ((\x -> case x of
    Core.TypeForall v1 -> (Sets.delete (Core.forallTypeParameter v1) (freeVariablesInType (Core.forallTypeBody v1)))
    Core.TypeVariable v1 -> (Sets.singleton v1)
    _ -> dfltVars) typ)

-- | Check whether a variable is free (not bound) in a term
isFreeVariableInTerm :: (Core.Name -> Core.Term -> Bool)
isFreeVariableInTerm v term = (Logic.not (Sets.member v (freeVariablesInTerm term)))

-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: (Core.Term -> Bool)
isLambda term = ((\x -> case x of
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionLambda _ -> True
    _ -> False) v1)
  Core.TermLet v1 -> (isLambda (Core.letEnvironment v1))
  _ -> False) (Strip.fullyStripTerm term))

-- | Apply a transformation to the first type beneath a chain of annotations
mapBeneathTypeAnnotations :: ((Core.Type -> Core.Type) -> Core.Type -> Core.Type)
mapBeneathTypeAnnotations f t = ((\x -> case x of
  Core.TypeAnnotated v1 -> (Core.TypeAnnotated (Core.AnnotatedType {
    Core.annotatedTypeSubject = (mapBeneathTypeAnnotations f (Core.annotatedTypeSubject v1)),
    Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)}))
  _ -> (f t)) t)

rewrite :: ((t1 -> t0) -> (t0 -> t1) -> t1)
rewrite fsub f =  
  let recurse = (f (fsub recurse))
  in recurse

rewriteTerm :: (((Core.Term -> Core.Term) -> Core.Term -> Core.Term) -> Core.Term -> Core.Term)
rewriteTerm f =  
  let fsub = (\recurse -> \term ->  
          let forElimination = (\elm -> (\x -> case x of
                  Core.EliminationProduct v1 -> (Core.EliminationProduct v1)
                  Core.EliminationRecord v1 -> (Core.EliminationRecord v1)
                  Core.EliminationUnion v1 -> (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.caseStatementTypeName v1),
                    Core.caseStatementDefault = (Optionals.map recurse (Core.caseStatementDefault v1)),
                    Core.caseStatementCases = (Lists.map forField (Core.caseStatementCases v1))}))
                  Core.EliminationWrap v1 -> (Core.EliminationWrap v1)) elm) 
              forField = (\f -> Core.Field {
                      Core.fieldName = (Core.fieldName f),
                      Core.fieldTerm = (recurse (Core.fieldTerm f))})
              forFunction = (\fun -> (\x -> case x of
                      Core.FunctionElimination v1 -> (Core.FunctionElimination (forElimination v1))
                      Core.FunctionLambda v1 -> (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.lambdaParameter v1),
                        Core.lambdaDomain = (Core.lambdaDomain v1),
                        Core.lambdaBody = (recurse (Core.lambdaBody v1))}))
                      Core.FunctionPrimitive v1 -> (Core.FunctionPrimitive v1)) fun)
              forLet = (\lt ->  
                      let mapBinding = (\b -> Core.LetBinding {
                              Core.letBindingName = (Core.letBindingName b),
                              Core.letBindingTerm = (recurse (Core.letBindingTerm b)),
                              Core.letBindingType = (Core.letBindingType b)})
                      in Core.Let {
                        Core.letBindings = (Lists.map mapBinding (Core.letBindings lt)),
                        Core.letEnvironment = (recurse (Core.letEnvironment lt))})
              forMap = (\m ->  
                      let forPair = (\p -> (recurse (fst p), (recurse (snd p))))
                      in (Maps.fromList (Lists.map forPair (Maps.toList m))))
          in ((\x -> case x of
            Core.TermAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermSubject = (recurse (Core.annotatedTermSubject v1)),
              Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))
            Core.TermApplication v1 -> (Core.TermApplication (Core.Application {
              Core.applicationFunction = (recurse (Core.applicationFunction v1)),
              Core.applicationArgument = (recurse (Core.applicationArgument v1))}))
            Core.TermFunction v1 -> (Core.TermFunction (forFunction v1))
            Core.TermLet v1 -> (Core.TermLet (forLet v1))
            Core.TermList v1 -> (Core.TermList (Lists.map recurse v1))
            Core.TermLiteral v1 -> (Core.TermLiteral v1)
            Core.TermMap v1 -> (Core.TermMap (forMap v1))
            Core.TermWrap v1 -> (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = (Core.wrappedTermTypeName v1),
              Core.wrappedTermObject = (recurse (Core.wrappedTermObject v1))}))
            Core.TermOptional v1 -> (Core.TermOptional (Optionals.map recurse v1))
            Core.TermProduct v1 -> (Core.TermProduct (Lists.map recurse v1))
            Core.TermRecord v1 -> (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.recordTypeName v1),
              Core.recordFields = (Lists.map forField (Core.recordFields v1))}))
            Core.TermSet v1 -> (Core.TermSet (Sets.fromList (Lists.map recurse (Sets.toList v1))))
            Core.TermSum v1 -> (Core.TermSum (Core.Sum {
              Core.sumIndex = (Core.sumIndex v1),
              Core.sumSize = (Core.sumSize v1),
              Core.sumTerm = (recurse (Core.sumTerm v1))}))
            Core.TermTypeAbstraction v1 -> (Core.TermTypeAbstraction (Core.TypeAbstraction {
              Core.typeAbstractionParameter = (Core.typeAbstractionParameter v1),
              Core.typeAbstractionBody = (recurse (Core.typeAbstractionBody v1))}))
            Core.TermTypeApplication v1 -> (Core.TermTypeApplication (Core.TypedTerm {
              Core.typedTermTerm = (recurse (Core.typedTermTerm v1)),
              Core.typedTermType = (Core.typedTermType v1)}))
            Core.TermTyped v1 -> (Core.TermTypeApplication (Core.TypedTerm {
              Core.typedTermTerm = (recurse (Core.typedTermTerm v1)),
              Core.typedTermType = (Core.typedTermType v1)}))
            Core.TermUnion v1 -> (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.injectionTypeName v1),
              Core.injectionField = (forField (Core.injectionField v1))}))
            Core.TermVariable v1 -> (Core.TermVariable v1)) term))
  in (rewrite fsub f)

rewriteTermM :: (((Core.Term -> Compute.Flow t0 Core.Term) -> Core.Term -> Compute.Flow t0 Core.Term) -> Core.Term -> Compute.Flow t0 Core.Term)
rewriteTermM f =  
  let fsub = (\recurse -> \term ->  
          let forField = (\f -> Flows.map (\t -> Core.Field {
                  Core.fieldName = (Core.fieldName f),
                  Core.fieldTerm = t}) (recurse (Core.fieldTerm f))) 
              forPair = (\kv ->  
                      let k = (fst kv) 
                          v = (snd kv)
                      in (Flows.bind (recurse k) (\km -> Flows.bind (recurse v) (\vm -> Flows.pure (km, vm)))))
              mapBinding = (\binding ->  
                      let k = (Core.letBindingName binding) 
                          v = (Core.letBindingTerm binding)
                          t = (Core.letBindingType binding)
                      in (Flows.bind (recurse v) (\v_ -> Flows.pure (Core.LetBinding {
                        Core.letBindingName = k,
                        Core.letBindingTerm = v_,
                        Core.letBindingType = t}))))
          in ((\x -> case x of
            Core.TermAnnotated v1 -> (Flows.bind (recurse (Core.annotatedTermSubject v1)) (\ex -> Flows.pure (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermSubject = ex,
              Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))))
            Core.TermApplication v1 -> (Flows.bind (recurse (Core.applicationFunction v1)) (\lhs -> Flows.bind (recurse (Core.applicationArgument v1)) (\rhs -> Flows.pure (Core.TermApplication (Core.Application {
              Core.applicationFunction = lhs,
              Core.applicationArgument = rhs})))))
            Core.TermFunction v1 -> (Flows.bind ((\x -> case x of
              Core.FunctionElimination v2 -> ((\x -> case x of
                Core.EliminationProduct v3 -> (Flows.pure (Core.FunctionElimination (Core.EliminationProduct v3)))
                Core.EliminationRecord v3 -> (Flows.pure (Core.FunctionElimination (Core.EliminationRecord v3)))
                Core.EliminationUnion v3 ->  
                  let n = (Core.caseStatementTypeName v3) 
                      def = (Core.caseStatementDefault v3)
                      cases = (Core.caseStatementCases v3)
                  in (Flows.bind (Optionals.maybe (Flows.pure Nothing) (\t -> Flows.map Optionals.pure (recurse t)) def) (\rdef -> Flows.map (\rcases -> Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = n,
                    Core.caseStatementDefault = rdef,
                    Core.caseStatementCases = rcases}))) (Flows.mapList forField cases)))
                Core.EliminationWrap v3 -> (Flows.pure (Core.FunctionElimination (Core.EliminationWrap v3)))) v2)
              Core.FunctionLambda v2 ->  
                let v = (Core.lambdaParameter v2) 
                    d = (Core.lambdaDomain v2)
                    body = (Core.lambdaBody v2)
                in (Flows.bind (recurse body) (\rbody -> Flows.pure (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = v,
                  Core.lambdaDomain = d,
                  Core.lambdaBody = rbody}))))
              Core.FunctionPrimitive v2 -> (Flows.pure (Core.FunctionPrimitive v2))) v1) (\rfun -> Flows.pure (Core.TermFunction rfun)))
            Core.TermLet v1 ->  
              let bindings = (Core.letBindings v1) 
                  env = (Core.letEnvironment v1)
              in (Flows.bind (Flows.mapList mapBinding bindings) (\rbindings -> Flows.bind (recurse env) (\renv -> Flows.pure (Core.TermLet (Core.Let {
                Core.letBindings = rbindings,
                Core.letEnvironment = renv})))))
            Core.TermList v1 -> (Flows.bind (Flows.mapList recurse v1) (\rels -> Flows.pure (Core.TermList rels)))
            Core.TermLiteral v1 -> (Flows.pure (Core.TermLiteral v1))
            Core.TermMap v1 -> (Flows.bind (Flows.mapList forPair (Maps.toList v1)) (\pairs -> Flows.pure (Core.TermMap (Maps.fromList pairs))))
            Core.TermOptional v1 -> (Flows.bind (Flows.traverseOptional recurse v1) (\rm -> Flows.pure (Core.TermOptional rm)))
            Core.TermProduct v1 -> (Flows.map (\rtuple -> Core.TermProduct rtuple) (Flows.mapList recurse v1))
            Core.TermRecord v1 ->  
              let n = (Core.recordTypeName v1) 
                  fields = (Core.recordFields v1)
              in (Flows.map (\rfields -> Core.TermRecord (Core.Record {
                Core.recordTypeName = n,
                Core.recordFields = rfields})) (Flows.mapList forField fields))
            Core.TermSet v1 -> (Flows.bind (Flows.mapList recurse (Sets.toList v1)) (\rlist -> Flows.pure (Core.TermSet (Sets.fromList rlist))))
            Core.TermSum v1 ->  
              let i = (Core.sumIndex v1) 
                  s = (Core.sumSize v1)
                  trm = (Core.sumTerm v1)
              in (Flows.bind (recurse trm) (\rtrm -> Flows.pure (Core.TermSum (Core.Sum {
                Core.sumIndex = i,
                Core.sumSize = s,
                Core.sumTerm = rtrm}))))
            Core.TermTyped v1 ->  
              let term1 = (Core.typedTermTerm v1) 
                  type2 = (Core.typedTermType v1)
              in (Flows.bind (recurse term1) (\rterm1 -> Flows.pure (Core.TermTyped (Core.TypedTerm {
                Core.typedTermTerm = rterm1,
                Core.typedTermType = type2}))))
            Core.TermUnion v1 ->  
              let n = (Core.injectionTypeName v1) 
                  field = (Core.injectionField v1)
              in (Flows.map (\rfield -> Core.TermUnion (Core.Injection {
                Core.injectionTypeName = n,
                Core.injectionField = rfield})) (forField field))
            Core.TermVariable v1 -> (Flows.pure (Core.TermVariable v1))
            Core.TermWrap v1 ->  
              let name = (Core.wrappedTermTypeName v1) 
                  t = (Core.wrappedTermObject v1)
              in (Flows.bind (recurse t) (\rt -> Flows.pure (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = name,
                Core.wrappedTermObject = rt}))))) term))
  in (rewrite fsub f)

rewriteType :: (((Core.Type -> Core.Type) -> Core.Type -> Core.Type) -> Core.Type -> Core.Type)
rewriteType f =  
  let fsub = (\recurse -> \typ ->  
          let forField = (\f -> Core.FieldType {
                  Core.fieldTypeName = (Core.fieldTypeName f),
                  Core.fieldTypeType = (recurse (Core.fieldTypeType f))})
          in ((\x -> case x of
            Core.TypeAnnotated v1 -> (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeSubject = (recurse (Core.annotatedTypeSubject v1)),
              Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)}))
            Core.TypeApplication v1 -> (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (recurse (Core.applicationTypeFunction v1)),
              Core.applicationTypeArgument = (recurse (Core.applicationTypeArgument v1))}))
            Core.TypeFunction v1 -> (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (recurse (Core.functionTypeDomain v1)),
              Core.functionTypeCodomain = (recurse (Core.functionTypeCodomain v1))}))
            Core.TypeForall v1 -> (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.forallTypeParameter v1),
              Core.forallTypeBody = (recurse (Core.forallTypeBody v1))}))
            Core.TypeList v1 -> (Core.TypeList (recurse v1))
            Core.TypeLiteral v1 -> (Core.TypeLiteral v1)
            Core.TypeMap v1 -> (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (recurse (Core.mapTypeKeys v1)),
              Core.mapTypeValues = (recurse (Core.mapTypeValues v1))}))
            Core.TypeOptional v1 -> (Core.TypeOptional (recurse v1))
            Core.TypeProduct v1 -> (Core.TypeProduct (Lists.map recurse v1))
            Core.TypeRecord v1 -> (Core.TypeRecord (Core.RowType {
              Core.rowTypeTypeName = (Core.rowTypeTypeName v1),
              Core.rowTypeFields = (Lists.map forField (Core.rowTypeFields v1))}))
            Core.TypeSet v1 -> (Core.TypeSet (recurse v1))
            Core.TypeSum v1 -> (Core.TypeSum (Lists.map recurse v1))
            Core.TypeUnion v1 -> (Core.TypeUnion (Core.RowType {
              Core.rowTypeTypeName = (Core.rowTypeTypeName v1),
              Core.rowTypeFields = (Lists.map forField (Core.rowTypeFields v1))}))
            Core.TypeVariable v1 -> (Core.TypeVariable v1)
            Core.TypeWrap v1 -> (Core.TypeWrap (Core.WrappedType {
              Core.wrappedTypeTypeName = (Core.wrappedTypeTypeName v1),
              Core.wrappedTypeObject = (recurse (Core.wrappedTypeObject v1))}))) typ))
  in (rewrite fsub f)

-- | Find the children of a given term
subterms :: (Core.Term -> [Core.Term])
subterms x = case x of
  Core.TermAnnotated v1 -> [
    Core.annotatedTermSubject v1]
  Core.TermApplication v1 -> [
    Core.applicationFunction v1,
    (Core.applicationArgument v1)]
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationUnion v3 -> (Lists.concat2 (Optionals.maybe [] (\t -> [
        t]) (Core.caseStatementDefault v3)) (Lists.map Core.fieldTerm (Core.caseStatementCases v3)))
      _ -> []) v2)
    Core.FunctionLambda v2 -> [
      Core.lambdaBody v2]
    _ -> []) v1)
  Core.TermLet v1 -> (Lists.cons (Core.letEnvironment v1) (Lists.map Core.letBindingTerm (Core.letBindings v1)))
  Core.TermList v1 -> v1
  Core.TermLiteral _ -> []
  Core.TermMap v1 -> (Lists.concat (Lists.map (\p -> [
    fst p,
    (snd p)]) (Maps.toList v1)))
  Core.TermOptional v1 -> (Optionals.maybe [] (\t -> [
    t]) v1)
  Core.TermProduct v1 -> v1
  Core.TermRecord v1 -> (Lists.map Core.fieldTerm (Core.recordFields v1))
  Core.TermSet v1 -> (Sets.toList v1)
  Core.TermSum v1 -> [
    Core.sumTerm v1]
  Core.TermTypeAbstraction v1 -> [
    Core.typeAbstractionBody v1]
  Core.TermTypeApplication v1 -> [
    Core.typedTermTerm v1]
  Core.TermTyped v1 -> [
    Core.typedTermTerm v1]
  Core.TermUnion v1 -> [
    Core.fieldTerm (Core.injectionField v1)]
  Core.TermVariable _ -> []
  Core.TermWrap v1 -> [
    Core.wrappedTermObject v1]

-- | Find the children of a given term
subtermsWithAccessors :: (Core.Term -> [(Mantle.TermAccessor, Core.Term)])
subtermsWithAccessors x = case x of
  Core.TermAnnotated v1 -> [
    (Mantle.TermAccessorAnnotatedSubject, (Core.annotatedTermSubject v1))]
  Core.TermApplication v1 -> [
    (Mantle.TermAccessorApplicationFunction, (Core.applicationFunction v1)),
    (Mantle.TermAccessorApplicationArgument, (Core.applicationArgument v1))]
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationUnion v3 -> (Lists.concat2 (Optionals.maybe [] (\t -> [
        (Mantle.TermAccessorUnionCasesDefault, t)]) (Core.caseStatementDefault v3)) (Lists.map (\f -> (Mantle.TermAccessorUnionCasesBranch (Core.fieldName f), (Core.fieldTerm f))) (Core.caseStatementCases v3)))
      _ -> []) v2)
    Core.FunctionLambda v2 -> [
      (Mantle.TermAccessorLambdaBody, (Core.lambdaBody v2))]
    _ -> []) v1)
  Core.TermLet v1 -> (Lists.cons (Mantle.TermAccessorLetEnvironment, (Core.letEnvironment v1)) (Lists.map (\b -> (Mantle.TermAccessorLetBinding (Core.letBindingName b), (Core.letBindingTerm b))) (Core.letBindings v1)))
  Core.TermList v1 -> (Lists.map (\e -> (Mantle.TermAccessorListElement 0, e)) v1)
  Core.TermLiteral _ -> []
  Core.TermMap v1 -> (Lists.concat (Lists.map (\p -> [
    (Mantle.TermAccessorMapKey 0, (fst p)),
    (Mantle.TermAccessorMapValue 0, (snd p))]) (Maps.toList v1)))
  Core.TermOptional v1 -> (Optionals.maybe [] (\t -> [
    (Mantle.TermAccessorOptionalTerm, t)]) v1)
  Core.TermProduct v1 -> (Lists.map (\e -> (Mantle.TermAccessorProductTerm 0, e)) v1)
  Core.TermRecord v1 -> (Lists.map (\f -> (Mantle.TermAccessorRecordField (Core.fieldName f), (Core.fieldTerm f))) (Core.recordFields v1))
  Core.TermSet v1 -> (Lists.map (\e -> (Mantle.TermAccessorListElement 0, e)) (Sets.toList v1))
  Core.TermSum v1 -> [
    (Mantle.TermAccessorSumTerm, (Core.sumTerm v1))]
  Core.TermTypeAbstraction v1 -> [
    (Mantle.TermAccessorTypeAbstractionBody, (Core.typeAbstractionBody v1))]
  Core.TermTypeApplication v1 -> [
    (Mantle.TermAccessorTypeApplicationTerm, (Core.typedTermTerm v1))]
  Core.TermTyped v1 -> [
    (Mantle.TermAccessorTypedTerm, (Core.typedTermTerm v1))]
  Core.TermUnion v1 -> [
    (Mantle.TermAccessorInjectionTerm, (Core.fieldTerm (Core.injectionField v1)))]
  Core.TermVariable _ -> []
  Core.TermWrap v1 -> [
    (Mantle.TermAccessorWrappedTerm, (Core.wrappedTermObject v1))]

-- | Find the children of a given type expression
subtypes :: (Core.Type -> [Core.Type])
subtypes x = case x of
  Core.TypeAnnotated v1 -> [
    Core.annotatedTypeSubject v1]
  Core.TypeApplication v1 -> [
    Core.applicationTypeFunction v1,
    (Core.applicationTypeArgument v1)]
  Core.TypeFunction v1 -> [
    Core.functionTypeDomain v1,
    (Core.functionTypeCodomain v1)]
  Core.TypeForall v1 -> [
    Core.forallTypeBody v1]
  Core.TypeList v1 -> [
    v1]
  Core.TypeLiteral _ -> []
  Core.TypeMap v1 -> [
    Core.mapTypeKeys v1,
    (Core.mapTypeValues v1)]
  Core.TypeOptional v1 -> [
    v1]
  Core.TypeProduct v1 -> v1
  Core.TypeRecord v1 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v1))
  Core.TypeSet v1 -> [
    v1]
  Core.TypeSum v1 -> v1
  Core.TypeUnion v1 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v1))
  Core.TypeVariable _ -> []
  Core.TypeWrap v1 -> [
    Core.wrappedTypeObject v1]

-- | Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that
termDependencyNames :: (Bool -> Bool -> Bool -> Core.Term -> S.Set Core.Name)
termDependencyNames withVars withPrims withNoms =  
  let addNames = (\names -> \term ->  
          let nominal = (\name -> Logic.ifElse withNoms (Sets.insert name names) names) 
              prim = (\name -> Logic.ifElse withPrims (Sets.insert name names) names)
              var = (\name -> Logic.ifElse withVars (Sets.insert name names) names)
          in ((\x -> case x of
            Core.TermFunction v1 -> ((\x -> case x of
              Core.FunctionPrimitive v2 -> (prim v2)
              Core.FunctionElimination v2 -> ((\x -> case x of
                Core.EliminationRecord v3 -> (nominal (Core.projectionTypeName v3))
                Core.EliminationUnion v3 -> (nominal (Core.caseStatementTypeName v3))
                Core.EliminationWrap v3 -> (nominal v3)
                _ -> names) v2)
              _ -> names) v1)
            Core.TermRecord v1 -> (nominal (Core.recordTypeName v1))
            Core.TermUnion v1 -> (nominal (Core.injectionTypeName v1))
            Core.TermVariable v1 -> (var v1)
            Core.TermWrap v1 -> (nominal (Core.wrappedTermTypeName v1))
            _ -> names) term))
  in (foldOverTerm Coders.TraversalOrderPre addNames Sets.empty)

typeDependencyNames :: (Bool -> Bool -> Core.Type -> S.Set Core.Name)
typeDependencyNames withSchema excludeUnit typ = (Logic.ifElse withSchema (Sets.union (freeVariablesInType typ) (typeNamesInType excludeUnit typ)) (freeVariablesInType typ))

typeNamesInType :: (Bool -> Core.Type -> S.Set Core.Name)
typeNamesInType excludeUnit =  
  let addNames = (\names -> \typ -> (\x -> case x of
          Core.TypeRecord v1 ->  
            let tname = (Core.rowTypeTypeName v1)
            in (Logic.ifElse (Logic.or (Logic.not excludeUnit) (Logic.not (Equality.equalString (Core.unName tname) (Core.unName (Core.Name "hydra.core.Unit"))))) (Sets.insert tname names) names)
          Core.TypeUnion v1 ->  
            let tname = (Core.rowTypeTypeName v1)
            in (Sets.insert tname names)
          Core.TypeWrap v1 ->  
            let tname = (Core.wrappedTypeTypeName v1)
            in (Sets.insert tname names)
          _ -> names) typ)
  in (foldOverType Coders.TraversalOrderPre addNames Sets.empty)
