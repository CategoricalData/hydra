-- | Utilities for type and term rewriting and analysis.

module Hydra.Rewriting where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Names as Names
import qualified Hydra.Sorting as Sorting
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Strip type annotations from the top levels of a term
deannotateAndDetypeTerm :: (Core.Term -> Core.Term)
deannotateAndDetypeTerm t = ((\x -> case x of
  Core.TermAnnotated v1 -> (deannotateAndDetypeTerm (Core.annotatedTermBody v1))
  Core.TermTypeApplication v1 -> (deannotateAndDetypeTerm (Core.typeApplicationTermBody v1))
  Core.TermTypeLambda v1 -> (deannotateAndDetypeTerm (Core.typeLambdaBody v1))
  _ -> t) t)

-- | Strip all annotations (including System F type annotations) from the top levels of a term
deannotateTerm :: (Core.Term -> Core.Term)
deannotateTerm t = ((\x -> case x of
  Core.TermAnnotated v1 -> (deannotateTerm (Core.annotatedTermBody v1))
  _ -> t) t)

-- | Strip all annotations from a term
deannotateType :: (Core.Type -> Core.Type)
deannotateType t = ((\x -> case x of
  Core.TypeAnnotated v1 -> (deannotateType (Core.annotatedTypeBody v1))
  _ -> t) t)

-- | Strip any top-level type lambdas from a type, extracting the (possibly nested) type body
deannotateTypeParameters :: (Core.Type -> Core.Type)
deannotateTypeParameters t = ((\x -> case x of
  Core.TypeForall v1 -> (deannotateTypeParameters (Core.forallTypeBody v1))
  _ -> t) (deannotateType t))

-- | Recursively strip all annotations from a type
deannotateTypeRecursive :: (Core.Type -> Core.Type)
deannotateTypeRecursive typ =  
  let strip = (\recurse -> \typ ->  
          let rewritten = (recurse typ)
          in ((\x -> case x of
            Core.TypeAnnotated v1 -> (Core.annotatedTypeBody v1)
            _ -> rewritten) rewritten))
  in (rewriteType strip typ)

-- | Recursively strip all annotations from a type scheme
deannotateTypeSchemeRecursive :: (Core.TypeScheme -> Core.TypeScheme)
deannotateTypeSchemeRecursive ts =  
  let vars = (Core.typeSchemeVariables ts)
  in  
    let typ = (Core.typeSchemeType ts)
    in Core.TypeScheme {
      Core.typeSchemeVariables = vars,
      Core.typeSchemeType = (deannotateTypeRecursive typ)}

-- | Strip System F type annotations from the top levels of a term, but leave application-specific annotations intact
detypeTerm :: (Core.Term -> Core.Term)
detypeTerm t = ((\x -> case x of
  Core.TermAnnotated v1 ->  
    let subj = (Core.annotatedTermBody v1)
    in  
      let ann = (Core.annotatedTermAnnotation v1)
      in (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (detypeTerm subj),
        Core.annotatedTermAnnotation = ann}))
  Core.TermTypeApplication v1 -> (deannotateAndDetypeTerm (Core.typeApplicationTermBody v1))
  Core.TermTypeLambda v1 -> (deannotateAndDetypeTerm (Core.typeLambdaBody v1))
  _ -> t) t)

-- | Flatten nested let expressions
flattenLetTerms :: (Core.Term -> Core.Term)
flattenLetTerms term =  
  let rewriteBinding = (\binding ->  
          let key0 = (Core.bindingName binding)
          in  
            let val0 = (Core.bindingTerm binding)
            in  
              let t = (Core.bindingType binding)
              in ((\x -> case x of
                Core.TermAnnotated v1 ->  
                  let val1 = (Core.annotatedTermBody v1)
                  in  
                    let ann = (Core.annotatedTermAnnotation v1)
                    in  
                      let recursive = (rewriteBinding (Core.Binding {
                              Core.bindingName = key0,
                              Core.bindingTerm = val1,
                              Core.bindingType = t}))
                      in  
                        let innerBinding = (fst recursive)
                        in  
                          let deps = (snd recursive)
                          in  
                            let val2 = (Core.bindingTerm innerBinding)
                            in (Core.Binding {
                              Core.bindingName = key0,
                              Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                                Core.annotatedTermBody = val2,
                                Core.annotatedTermAnnotation = ann})),
                              Core.bindingType = t}, deps)
                Core.TermLet v1 ->  
                  let bindings1 = (Core.letBindings v1)
                  in  
                    let body1 = (Core.letBody v1)
                    in  
                      let prefix = (Strings.cat2 (Core.unName key0) "_")
                      in  
                        let qualify = (\n -> Core.Name (Strings.cat2 prefix (Core.unName n)))
                        in  
                          let toSubstPair = (\b -> (Core.bindingName b, (qualify (Core.bindingName b))))
                          in  
                            let subst = (Maps.fromList (Lists.map toSubstPair bindings1))
                            in  
                              let replaceVars = (substituteVariables subst)
                              in  
                                let newBody = (replaceVars body1)
                                in  
                                  let newBinding = (\b -> Core.Binding {
                                          Core.bindingName = (qualify (Core.bindingName b)),
                                          Core.bindingTerm = (replaceVars (Core.bindingTerm b)),
                                          Core.bindingType = (Core.bindingType b)})
                                  in (Core.Binding {
                                    Core.bindingName = key0,
                                    Core.bindingTerm = newBody,
                                    Core.bindingType = t}, (Lists.map newBinding bindings1))
                _ -> (Core.Binding {
                  Core.bindingName = key0,
                  Core.bindingTerm = val0,
                  Core.bindingType = t}, [])) val0))
  in  
    let flatten = (\recurse -> \term ->  
            let rewritten = (recurse term)
            in ((\x -> case x of
              Core.TermLet v1 ->  
                let bindings = (Core.letBindings v1)
                in  
                  let body = (Core.letBody v1)
                  in  
                    let forResult = (\hr -> Lists.cons (fst hr) (snd hr))
                    in  
                      let newBindings = (Lists.concat (Lists.map (\arg_ -> forResult (rewriteBinding arg_)) bindings))
                      in (Core.TermLet (Core.Let {
                        Core.letBindings = newBindings,
                        Core.letBody = body}))
              _ -> rewritten) rewritten))
    in (rewriteTerm flatten term)

foldOverTerm :: (Coders.TraversalOrder -> (t0 -> Core.Term -> t0) -> t0 -> Core.Term -> t0)
foldOverTerm order fld b0 term = ((\x -> case x of
  Coders.TraversalOrderPre -> (Lists.foldl (foldOverTerm order fld) (fld b0 term) (subterms term))
  Coders.TraversalOrderPost -> (fld (Lists.foldl (foldOverTerm order fld) b0 (subterms term)) term)) order)

foldOverType :: (Coders.TraversalOrder -> (t0 -> Core.Type -> t0) -> t0 -> Core.Type -> t0)
foldOverType order fld b0 typ = ((\x -> case x of
  Coders.TraversalOrderPre -> (Lists.foldl (foldOverType order fld) (fld b0 typ) (subtypes typ))
  Coders.TraversalOrderPost -> (fld (Lists.foldl (foldOverType order fld) b0 (subtypes typ)) typ)) order)

-- | Get the set of free type variables in a term (including schema names, where they appear in type annotations). In this context, only the type schemes of let bindings can bind type variables; type lambdas do not.
freeTypeVariablesInTerm :: (Core.Term -> S.Set Core.Name)
freeTypeVariablesInTerm term0 =  
  let allOf = (\sets -> Lists.foldl Sets.union Sets.empty sets)
  in  
    let tryType = (\tvars -> \typ -> Sets.difference (freeVariablesInType typ) tvars)
    in  
      let getAll = (\vars -> \term ->  
              let recurse = (getAll vars)
              in  
                let dflt = (allOf (Lists.map recurse (subterms term)))
                in ((\x -> case x of
                  Core.TermFunction v1 -> ((\x -> case x of
                    Core.FunctionElimination v2 -> ((\x -> case x of
                      Core.EliminationProduct v3 -> (Maybes.maybe Sets.empty (\typs -> allOf (Lists.map (tryType vars) typs)) (Core.tupleProjectionDomain v3))
                      _ -> dflt) v2)
                    Core.FunctionLambda v2 ->  
                      let domt = (Maybes.maybe Sets.empty (tryType vars) (Core.lambdaDomain v2))
                      in (Sets.union domt (recurse (Core.lambdaBody v2)))
                    _ -> dflt) v1)
                  Core.TermLet v1 ->  
                    let forBinding = (\b ->  
                            let newVars = (Maybes.maybe vars (\ts -> Sets.union vars (Sets.fromList (Core.typeSchemeVariables ts))) (Core.bindingType b))
                            in (Sets.union (getAll newVars (Core.bindingTerm b)) (Maybes.maybe Sets.empty (\ts -> tryType newVars (Core.typeSchemeType ts)) (Core.bindingType b))))
                    in (Sets.union (allOf (Lists.map forBinding (Core.letBindings v1))) (recurse (Core.letBody v1)))
                  Core.TermTypeApplication v1 -> (Sets.union (tryType vars (Core.typeApplicationTermType v1)) (recurse (Core.typeApplicationTermBody v1)))
                  Core.TermTypeLambda v1 -> (Sets.union (tryType vars (Core.TypeVariable (Core.typeLambdaParameter v1))) (recurse (Core.typeLambdaBody v1)))
                  _ -> dflt) term))
      in (getAll Sets.empty term0)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a term
freeVariablesInTerm :: (Core.Term -> S.Set Core.Name)
freeVariablesInTerm term =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInTerm t)) Sets.empty (subterms term))
  in ((\x -> case x of
    Core.TermFunction v1 -> ((\x -> case x of
      Core.FunctionLambda v2 -> (Sets.delete (Core.lambdaParameter v2) (freeVariablesInTerm (Core.lambdaBody v2)))
      _ -> dfltVars) v1)
    Core.TermLet v1 -> (Sets.difference dfltVars (Sets.fromList (Lists.map Core.bindingName (Core.letBindings v1))))
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

-- | Find the free variables in a type in deterministic left-to-right order
freeVariablesInTypeOrdered :: (Core.Type -> [Core.Name])
freeVariablesInTypeOrdered typ =  
  let collectVars = (\boundVars -> \t -> (\x -> case x of
          Core.TypeVariable v1 -> (Logic.ifElse (Sets.member v1 boundVars) [] [
            v1])
          Core.TypeForall v1 -> (collectVars (Sets.insert (Core.forallTypeParameter v1) boundVars) (Core.forallTypeBody v1))
          _ -> (Lists.concat (Lists.map (collectVars boundVars) (subtypes t)))) t)
  in (Lists.nub (collectVars Sets.empty typ))

-- | Find free variables in a type scheme (simple version)
freeVariablesInTypeSchemeSimple :: (Core.TypeScheme -> S.Set Core.Name)
freeVariablesInTypeSchemeSimple ts =  
  let vars = (Core.typeSchemeVariables ts)
  in  
    let t = (Core.typeSchemeType ts)
    in (Sets.difference (freeVariablesInTypeSimple t) (Sets.fromList vars))

-- | Find free variables in a type scheme
freeVariablesInTypeScheme :: (Core.TypeScheme -> S.Set Core.Name)
freeVariablesInTypeScheme ts =  
  let vars = (Core.typeSchemeVariables ts)
  in  
    let t = (Core.typeSchemeType ts)
    in (Sets.difference (freeVariablesInType t) (Sets.fromList vars))

-- | Same as freeVariablesInType, but ignores the binding action of lambda types
freeVariablesInTypeSimple :: (Core.Type -> S.Set Core.Name)
freeVariablesInTypeSimple typ =  
  let helper = (\types -> \typ -> (\x -> case x of
          Core.TypeVariable v1 -> (Sets.insert v1 types)
          _ -> types) typ)
  in (foldOverType Coders.TraversalOrderPre helper Sets.empty typ)

inlineType :: (M.Map Core.Name Core.Type -> Core.Type -> Compute.Flow t0 Core.Type)
inlineType schema typ =  
  let f = (\recurse -> \typ ->  
          let afterRecurse = (\tr -> (\x -> case x of
                  Core.TypeVariable v1 -> (Maybes.maybe (Flows.fail (Strings.cat2 "No such type in schema: " (Core.unName v1))) (inlineType schema) (Maps.lookup v1 schema))
                  _ -> (Flows.pure tr)) tr)
          in (Flows.bind (recurse typ) (\tr -> afterRecurse tr)))
  in (rewriteTypeM f typ)

-- | Check whether a variable is free (not bound) in a term
isFreeVariableInTerm :: (Core.Name -> Core.Term -> Bool)
isFreeVariableInTerm v term = (Logic.not (Sets.member v (freeVariablesInTerm term)))

-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: (Core.Term -> Bool)
isLambda term = ((\x -> case x of
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionLambda _ -> True
    _ -> False) v1)
  Core.TermLet v1 -> (isLambda (Core.letBody v1))
  _ -> False) (deannotateTerm term))

-- | Rewrite terms like `let foo = bar in λx.baz` to `λx.let foo = bar in baz`, lifting lambda-bound variables above let-bound variables, recursively. This is helpful for targets such as Python.
liftLambdaAboveLet :: (Core.Term -> Core.Term)
liftLambdaAboveLet term0 =  
  let rewrite = (\recurse -> \term ->  
          let rewriteBinding = (\b -> Core.Binding {
                  Core.bindingName = (Core.bindingName b),
                  Core.bindingTerm = (rewrite recurse (Core.bindingTerm b)),
                  Core.bindingType = (Core.bindingType b)})
          in  
            let rewriteBindings = (\bs -> Lists.map rewriteBinding bs)
            in  
              let digForLambdas = (\original -> \cons -> \term -> (\x -> case x of
                      Core.TermAnnotated v1 -> (digForLambdas original (\t -> Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = (cons t),
                        Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)})) (Core.annotatedTermBody v1))
                      Core.TermFunction v1 -> ((\x -> case x of
                        Core.FunctionLambda v2 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.lambdaParameter v2),
                          Core.lambdaDomain = (Core.lambdaDomain v2),
                          Core.lambdaBody = (digForLambdas (cons (Core.lambdaBody v2)) (\t -> cons t) (Core.lambdaBody v2))})))
                        _ -> (recurse original)) v1)
                      Core.TermLet v1 -> (digForLambdas original (\t -> cons (Core.TermLet (Core.Let {
                        Core.letBindings = (rewriteBindings (Core.letBindings v1)),
                        Core.letBody = t}))) (Core.letBody v1))
                      _ -> (recurse original)) term)
              in ((\x -> case x of
                Core.TermLet v1 -> (digForLambdas term (\t -> Core.TermLet (Core.Let {
                  Core.letBindings = (rewriteBindings (Core.letBindings v1)),
                  Core.letBody = t})) (Core.letBody v1))
                _ -> (recurse term)) term))
  in (rewriteTerm rewrite term0)

-- | Apply a transformation to the first type beneath a chain of annotations
mapBeneathTypeAnnotations :: ((Core.Type -> Core.Type) -> Core.Type -> Core.Type)
mapBeneathTypeAnnotations f t = ((\x -> case x of
  Core.TypeAnnotated v1 -> (Core.TypeAnnotated (Core.AnnotatedType {
    Core.annotatedTypeBody = (mapBeneathTypeAnnotations f (Core.annotatedTypeBody v1)),
    Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)}))
  _ -> (f t)) t)

-- | Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ...
normalizeTypeVariablesInTerm :: (Core.Term -> Core.Term)
normalizeTypeVariablesInTerm term =  
  let replaceName = (\subst -> \v -> Maybes.fromMaybe v (Maps.lookup v subst))
  in  
    let substType = (\subst -> \typ ->  
            let rewrite = (\recurse -> \typ -> (\x -> case x of
                    Core.TypeVariable v1 -> (Core.TypeVariable (replaceName subst v1))
                    _ -> (recurse typ)) typ)
            in (rewriteType rewrite typ))
    in  
      let rewriteWithSubst = (\state -> \term0 ->  
              let sb = (fst state)
              in  
                let next = (snd state)
                in  
                  let subst = (fst sb)
                  in  
                    let boundVars = (snd sb)
                    in  
                      let rewrite = (\recurse -> \term -> (\x -> case x of
                              Core.TermFunction v1 -> ((\x -> case x of
                                Core.FunctionElimination v2 -> ((\x -> case x of
                                  Core.EliminationProduct v3 ->  
                                    let domain = (Core.tupleProjectionDomain v3)
                                    in (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                                      Core.tupleProjectionArity = (Core.tupleProjectionArity v3),
                                      Core.tupleProjectionIndex = (Core.tupleProjectionIndex v3),
                                      Core.tupleProjectionDomain = (Maybes.map (\types -> Lists.map (substType subst) types) domain)}))))
                                  _ -> (recurse term)) v2)
                                Core.FunctionLambda v2 ->  
                                  let domain = (Core.lambdaDomain v2)
                                  in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.lambdaParameter v2),
                                    Core.lambdaDomain = (Maybes.map (substType subst) domain),
                                    Core.lambdaBody = (rewriteWithSubst ((subst, boundVars), next) (Core.lambdaBody v2))})))
                                _ -> (recurse term)) v1)
                              Core.TermLet v1 ->  
                                let bindings0 = (Core.letBindings v1)
                                in  
                                  let body0 = (Core.letBody v1)
                                  in  
                                    let step = (\acc -> \bs ->  
                                            let b = (Lists.head bs)
                                            in  
                                              let tl = (Lists.tail bs)
                                              in  
                                                let noType =  
                                                        let newVal = (rewriteWithSubst ((subst, boundVars), next) (Core.bindingTerm b))
                                                        in  
                                                          let b1 = Core.Binding {
                                                                  Core.bindingName = (Core.bindingName b),
                                                                  Core.bindingTerm = newVal,
                                                                  Core.bindingType = Nothing}
                                                          in (step (Lists.cons b1 acc) tl)
                                                in  
                                                  let withType = (\ts ->  
                                                          let vars = (Core.typeSchemeVariables ts)
                                                          in  
                                                            let typ = (Core.typeSchemeType ts)
                                                            in  
                                                              let k = (Lists.length vars)
                                                              in  
                                                                let gen = (\i -> \rem -> \acc2 ->  
                                                                        let ti = (Core.Name (Strings.cat2 "t" (Literals.showInt32 (Math.add next i))))
                                                                        in (Logic.ifElse (Equality.equal rem 0) (Lists.reverse acc2) (gen (Math.add i 1) (Math.sub rem 1) (Lists.cons ti acc2))))
                                                                in  
                                                                  let newVars = (gen 0 k [])
                                                                  in  
                                                                    let newSubst = (Maps.union (Maps.fromList (Lists.zip vars newVars)) subst)
                                                                    in  
                                                                      let newBound = (Sets.union boundVars (Sets.fromList newVars))
                                                                      in  
                                                                        let newVal = (rewriteWithSubst ((newSubst, newBound), (Math.add next k)) (Core.bindingTerm b))
                                                                        in  
                                                                          let b1 = Core.Binding {
                                                                                  Core.bindingName = (Core.bindingName b),
                                                                                  Core.bindingTerm = newVal,
                                                                                  Core.bindingType = (Just (Core.TypeScheme {
                                                                                    Core.typeSchemeVariables = newVars,
                                                                                    Core.typeSchemeType = (substType newSubst typ)}))}
                                                                          in (step (Lists.cons b1 acc) tl))
                                                  in (Logic.ifElse (Lists.null bs) (Lists.reverse acc) (Maybes.maybe noType (\ts -> withType ts) (Core.bindingType b))))
                                    in  
                                      let bindings1 = (step [] bindings0)
                                      in (Core.TermLet (Core.Let {
                                        Core.letBindings = bindings1,
                                        Core.letBody = (rewriteWithSubst ((subst, boundVars), next) body0)}))
                              Core.TermTypeApplication v1 -> (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (rewriteWithSubst ((subst, boundVars), next) (Core.typeApplicationTermBody v1)),
                                Core.typeApplicationTermType = (substType subst (Core.typeApplicationTermType v1))}))
                              Core.TermTypeLambda v1 -> (Core.TermTypeLambda (Core.TypeLambda {
                                Core.typeLambdaParameter = (replaceName subst (Core.typeLambdaParameter v1)),
                                Core.typeLambdaBody = (rewriteWithSubst ((subst, boundVars), next) (Core.typeLambdaBody v1))}))
                              _ -> (recurse term)) term)
                      in (rewriteTerm rewrite term0))
      in (rewriteWithSubst ((Maps.empty, Sets.empty), 0) term)

-- | Recursively remove term annotations, including within subterms
removeTermAnnotations :: (Core.Term -> Core.Term)
removeTermAnnotations term =  
  let remove = (\recurse -> \term ->  
          let rewritten = (recurse term)
          in ((\x -> case x of
            Core.TermAnnotated v1 -> (Core.annotatedTermBody v1)
            _ -> rewritten) term))
  in (rewriteTerm remove term)

-- | Recursively remove type annotations, including within subtypes
removeTypeAnnotations :: (Core.Type -> Core.Type)
removeTypeAnnotations typ =  
  let remove = (\recurse -> \typ ->  
          let rewritten = (recurse typ)
          in ((\x -> case x of
            Core.TypeAnnotated v1 -> (Core.annotatedTypeBody v1)
            _ -> rewritten) rewritten))
  in (rewriteType remove typ)

-- | Strip type annotations from terms while preserving other annotations
removeTypesFromTerm :: (Core.Term -> Core.Term)
removeTypesFromTerm term =  
  let strip = (\recurse -> \term ->  
          let rewritten = (recurse term)
          in  
            let stripBinding = (\b -> Core.Binding {
                    Core.bindingName = (Core.bindingName b),
                    Core.bindingTerm = (Core.bindingTerm b),
                    Core.bindingType = Nothing})
            in ((\x -> case x of
              Core.TermFunction v1 -> ((\x -> case x of
                Core.FunctionElimination v2 -> ((\x -> case x of
                  Core.EliminationProduct v3 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                    Core.tupleProjectionArity = (Core.tupleProjectionArity v3),
                    Core.tupleProjectionIndex = (Core.tupleProjectionIndex v3),
                    Core.tupleProjectionDomain = Nothing}))))
                  _ -> (Core.TermFunction (Core.FunctionElimination v2))) v2)
                Core.FunctionLambda v2 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.lambdaParameter v2),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.lambdaBody v2)})))
                _ -> (Core.TermFunction v1)) v1)
              Core.TermLet v1 -> (Core.TermLet (Core.Let {
                Core.letBindings = (Lists.map stripBinding (Core.letBindings v1)),
                Core.letBody = (Core.letBody v1)}))
              Core.TermTypeApplication v1 -> (Core.typeApplicationTermBody v1)
              Core.TermTypeLambda v1 -> (Core.typeLambdaBody v1)
              _ -> rewritten) rewritten))
  in (rewriteTerm strip term)

-- | Replace a free variable in a term
replaceFreeTermVariable :: (Core.Name -> Core.Term -> Core.Term -> Core.Term)
replaceFreeTermVariable vold tnew term =  
  let rewrite = (\recurse -> \t -> (\x -> case x of
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionLambda v2 ->  
              let v = (Core.lambdaParameter v2)
              in (Logic.ifElse (Equality.equal v vold) t (recurse t))
            _ -> (recurse t)) v1)
          Core.TermVariable v1 -> (Logic.ifElse (Equality.equal v1 vold) tnew (Core.TermVariable v1))
          _ -> (recurse t)) t)
  in (rewriteTerm rewrite term)

-- | Replace free occurrences of a name in a type
replaceFreeTypeVariable :: (Core.Name -> Core.Type -> Core.Type -> Core.Type)
replaceFreeTypeVariable v rep typ =  
  let mapExpr = (\recurse -> \t -> (\x -> case x of
          Core.TypeForall v1 -> (Logic.ifElse (Equality.equal v (Core.forallTypeParameter v1)) t (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.forallTypeParameter v1),
            Core.forallTypeBody = (recurse (Core.forallTypeBody v1))})))
          Core.TypeVariable v1 -> (Logic.ifElse (Equality.equal v v1) rep t)
          _ -> (recurse t)) t)
  in (rewriteType mapExpr typ)

-- | Replace all occurrences of simple typedefs (type aliases) with the aliased types, recursively
replaceTypedefs :: (M.Map Core.Name Core.TypeScheme -> Core.Type -> Core.Type)
replaceTypedefs types typ0 =  
  let rewrite = (\recurse -> \typ ->  
          let dflt = (recurse typ)
          in ((\x -> case x of
            Core.TypeAnnotated v1 -> (rewrite recurse (Core.annotatedTypeBody v1))
            Core.TypeRecord _ -> typ
            Core.TypeUnion _ -> typ
            Core.TypeVariable v1 ->  
              let forMono = (\t -> (\x -> case x of
                      Core.TypeRecord _ -> dflt
                      Core.TypeUnion _ -> dflt
                      Core.TypeWrap _ -> dflt
                      _ -> (rewrite recurse t)) t)
              in  
                let forTypeScheme = (\ts ->  
                        let t = (Core.typeSchemeType ts)
                        in (Logic.ifElse (Lists.null (Core.typeSchemeVariables ts)) (forMono t) dflt))
                in (Maybes.maybe dflt (\ts -> forTypeScheme ts) (Maps.lookup v1 types))
            Core.TypeWrap _ -> typ
            _ -> dflt) typ))
  in (rewriteType rewrite typ0)

rewriteAndFoldTerm :: (((t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term))
rewriteAndFoldTerm f term0 =  
  let fsub = (\recurse -> \val0 -> \term0 ->  
          let forSingle = (\rec -> \cons -> \val -> \term ->  
                  let r = (rec val term)
                  in (fst r, (cons (snd r))))
          in  
            let forMany = (\rec -> \cons -> \val -> \els ->  
                    let rr = (Lists.foldl (\r -> \el ->  
                            let r2 = (rec (fst r) el)
                            in (fst r2, (Lists.cons (snd r2) (snd r)))) (val, []) els)
                    in (fst rr, (cons (Lists.reverse (snd rr)))))
            in  
              let forField = (\val -> \field ->  
                      let r = (recurse val (Core.fieldTerm field))
                      in (fst r, Core.Field {
                        Core.fieldName = (Core.fieldName field),
                        Core.fieldTerm = (snd r)}))
              in  
                let forFields = (forMany forField (\x -> x))
                in  
                  let forPair = (\val -> \kv ->  
                          let rk = (recurse val (fst kv))
                          in  
                            let rv = (recurse (fst rk) (snd kv))
                            in (fst rv, (snd rk, (snd rv))))
                  in  
                    let forBinding = (\val -> \binding ->  
                            let r = (recurse val (Core.bindingTerm binding))
                            in (fst r, Core.Binding {
                              Core.bindingName = (Core.bindingName binding),
                              Core.bindingTerm = (snd r),
                              Core.bindingType = (Core.bindingType binding)}))
                    in  
                      let forElimination = (\val -> \elm ->  
                              let r = ((\x -> case x of
                                      Core.EliminationUnion v1 ->  
                                        let rmd = (Maybes.map (recurse val) (Core.caseStatementDefault v1))
                                        in  
                                          let val1 = (Maybes.maybe val fst rmd)
                                          in  
                                            let rcases = (forFields val1 (Core.caseStatementCases v1))
                                            in (fst rcases, (Core.EliminationUnion (Core.CaseStatement {
                                              Core.caseStatementTypeName = (Core.caseStatementTypeName v1),
                                              Core.caseStatementDefault = (Maybes.map snd rmd),
                                              Core.caseStatementCases = (snd rcases)})))
                                      _ -> (val, elm)) elm)
                              in (fst r, (snd r)))
                      in  
                        let forFunction = (\val -> \fun -> (\x -> case x of
                                Core.FunctionElimination v1 ->  
                                  let r = (forElimination val v1)
                                  in (fst r, (Core.FunctionElimination (snd r)))
                                Core.FunctionLambda v1 ->  
                                  let r = (recurse val (Core.lambdaBody v1))
                                  in (fst r, (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.lambdaParameter v1),
                                    Core.lambdaDomain = (Core.lambdaDomain v1),
                                    Core.lambdaBody = (snd r)})))
                                _ -> (val, fun)) fun)
                        in  
                          let dflt = (val0, term0)
                          in ((\x -> case x of
                            Core.TermAnnotated v1 -> (forSingle recurse (\t -> Core.TermAnnotated (Core.AnnotatedTerm {
                              Core.annotatedTermBody = t,
                              Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)})) val0 (Core.annotatedTermBody v1))
                            Core.TermApplication v1 ->  
                              let rlhs = (recurse val0 (Core.applicationFunction v1))
                              in  
                                let rrhs = (recurse (fst rlhs) (Core.applicationArgument v1))
                                in (fst rrhs, (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (snd rlhs),
                                  Core.applicationArgument = (snd rrhs)})))
                            Core.TermFunction v1 -> (forSingle forFunction (\f -> Core.TermFunction f) val0 v1)
                            Core.TermLet v1 ->  
                              let renv = (recurse val0 (Core.letBody v1))
                              in (forMany forBinding (\bins -> Core.TermLet (Core.Let {
                                Core.letBindings = bins,
                                Core.letBody = (snd renv)})) (fst renv) (Core.letBindings v1))
                            Core.TermList v1 -> (forMany recurse (\x -> Core.TermList x) val0 v1)
                            Core.TermMap v1 -> (forMany forPair (\pairs -> Core.TermMap (Maps.fromList pairs)) val0 (Maps.toList v1))
                            Core.TermMaybe v1 -> (Maybes.maybe dflt (\t -> forSingle recurse (\t1 -> Core.TermMaybe (Just t1)) val0 t) v1)
                            Core.TermProduct v1 -> (forMany recurse (\x -> Core.TermProduct x) val0 v1)
                            Core.TermRecord v1 -> (forMany forField (\fields -> Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.recordTypeName v1),
                              Core.recordFields = fields})) val0 (Core.recordFields v1))
                            Core.TermSet v1 -> (forMany recurse (\e -> Core.TermSet (Sets.fromList e)) val0 (Sets.toList v1))
                            Core.TermSum v1 -> (forSingle recurse (\t -> Core.TermSum (Core.Sum {
                              Core.sumIndex = (Core.sumIndex v1),
                              Core.sumSize = (Core.sumSize v1),
                              Core.sumTerm = t})) val0 (Core.sumTerm v1))
                            Core.TermTypeApplication v1 -> (forSingle recurse (\t -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = t,
                              Core.typeApplicationTermType = (Core.typeApplicationTermType v1)})) val0 (Core.typeApplicationTermBody v1))
                            Core.TermTypeLambda v1 -> (forSingle recurse (\t -> Core.TermTypeLambda (Core.TypeLambda {
                              Core.typeLambdaParameter = (Core.typeLambdaParameter v1),
                              Core.typeLambdaBody = t})) val0 (Core.typeLambdaBody v1))
                            Core.TermUnion v1 -> (forSingle recurse (\t -> Core.TermUnion (Core.Injection {
                              Core.injectionTypeName = (Core.injectionTypeName v1),
                              Core.injectionField = Core.Field {
                                Core.fieldName = (Core.fieldName (Core.injectionField v1)),
                                Core.fieldTerm = t}})) val0 (Core.fieldTerm (Core.injectionField v1)))
                            Core.TermWrap v1 -> (forSingle recurse (\t -> Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.wrappedTermTypeName v1),
                              Core.wrappedTermBody = t})) val0 (Core.wrappedTermBody v1))
                            _ -> dflt) term0))
  in  
    let recurse = (f (fsub recurse))
    in (recurse term0)

rewriteAndFoldTermM :: (((t0 -> Core.Term -> Compute.Flow t1 (t0, Core.Term)) -> t0 -> Core.Term -> Compute.Flow t1 (t0, Core.Term)) -> t0 -> Core.Term -> Compute.Flow t1 (t0, Core.Term))
rewriteAndFoldTermM f term0 =  
  let fsub = (\recurse -> \val0 -> \term0 ->  
          let forSingle = (\rec -> \cons -> \val -> \term -> Flows.bind (rec val term) (\r -> Flows.pure (fst r, (cons (snd r)))))
          in  
            let forMany = (\rec -> \cons -> \val -> \els -> Flows.bind (Flows.foldl (\r -> \el -> Flows.bind (rec (fst r) el) (\r2 -> Flows.pure (fst r2, (Lists.cons (snd r2) (snd r))))) (val, []) els) (\rr -> Flows.pure (fst rr, (cons (Lists.reverse (snd rr))))))
            in  
              let forField = (\val -> \field -> Flows.bind (recurse val (Core.fieldTerm field)) (\r -> Flows.pure (fst r, Core.Field {
                      Core.fieldName = (Core.fieldName field),
                      Core.fieldTerm = (snd r)})))
              in  
                let forFields = (forMany forField (\x -> x))
                in  
                  let forPair = (\val -> \kv -> Flows.bind (recurse val (fst kv)) (\rk -> Flows.bind (recurse (fst rk) (snd kv)) (\rv -> Flows.pure (fst rv, (snd rk, (snd rv))))))
                  in  
                    let forBinding = (\val -> \binding -> Flows.bind (recurse val (Core.bindingTerm binding)) (\r -> Flows.pure (fst r, Core.Binding {
                            Core.bindingName = (Core.bindingName binding),
                            Core.bindingTerm = (snd r),
                            Core.bindingType = (Core.bindingType binding)})))
                    in  
                      let forElimination = (\val -> \elm ->  
                              let rw = (\elm -> (\x -> case x of
                                      Core.EliminationUnion v1 -> (Flows.bind (Maybes.maybe (Flows.pure Nothing) (\def -> Flows.map Maybes.pure (recurse val def)) (Core.caseStatementDefault v1)) (\rmd ->  
                                        let val1 = (Maybes.maybe val fst rmd)
                                        in (Flows.bind (forFields val1 (Core.caseStatementCases v1)) (\rcases -> Flows.pure (fst rcases, (Core.EliminationUnion (Core.CaseStatement {
                                          Core.caseStatementTypeName = (Core.caseStatementTypeName v1),
                                          Core.caseStatementDefault = (Maybes.map snd rmd),
                                          Core.caseStatementCases = (snd rcases)})))))))
                                      _ -> (Flows.pure (val, elm))) elm)
                              in (Flows.bind (rw elm) (\r -> Flows.pure (fst r, (snd r)))))
                      in  
                        let forFunction = (\val -> \fun -> (\x -> case x of
                                Core.FunctionElimination v1 -> (Flows.bind (forElimination val v1) (\r -> Flows.pure (fst r, (Core.FunctionElimination (snd r)))))
                                Core.FunctionLambda v1 -> (Flows.bind (recurse val (Core.lambdaBody v1)) (\r -> Flows.pure (fst r, (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.lambdaParameter v1),
                                  Core.lambdaDomain = (Core.lambdaDomain v1),
                                  Core.lambdaBody = (snd r)})))))
                                _ -> (Flows.pure (val, fun))) fun)
                        in  
                          let dflt = (Flows.pure (val0, term0))
                          in ((\x -> case x of
                            Core.TermAnnotated v1 -> (forSingle recurse (\t -> Core.TermAnnotated (Core.AnnotatedTerm {
                              Core.annotatedTermBody = t,
                              Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)})) val0 (Core.annotatedTermBody v1))
                            Core.TermApplication v1 -> (Flows.bind (recurse val0 (Core.applicationFunction v1)) (\rlhs -> Flows.bind (recurse (fst rlhs) (Core.applicationArgument v1)) (\rrhs -> Flows.pure (fst rrhs, (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (snd rlhs),
                              Core.applicationArgument = (snd rrhs)}))))))
                            Core.TermFunction v1 -> (forSingle forFunction (\f -> Core.TermFunction f) val0 v1)
                            Core.TermLet v1 -> (Flows.bind (recurse val0 (Core.letBody v1)) (\renv -> forMany forBinding (\bins -> Core.TermLet (Core.Let {
                              Core.letBindings = bins,
                              Core.letBody = (snd renv)})) (fst renv) (Core.letBindings v1)))
                            Core.TermList v1 -> (forMany recurse (\x -> Core.TermList x) val0 v1)
                            Core.TermMap v1 -> (forMany forPair (\pairs -> Core.TermMap (Maps.fromList pairs)) val0 (Maps.toList v1))
                            Core.TermMaybe v1 -> (Maybes.maybe dflt (\t -> forSingle recurse (\t1 -> Core.TermMaybe (Just t1)) val0 t) v1)
                            Core.TermProduct v1 -> (forMany recurse (\x -> Core.TermProduct x) val0 v1)
                            Core.TermRecord v1 -> (forMany forField (\fields -> Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.recordTypeName v1),
                              Core.recordFields = fields})) val0 (Core.recordFields v1))
                            Core.TermSet v1 -> (forMany recurse (\e -> Core.TermSet (Sets.fromList e)) val0 (Sets.toList v1))
                            Core.TermSum v1 -> (forSingle recurse (\t -> Core.TermSum (Core.Sum {
                              Core.sumIndex = (Core.sumIndex v1),
                              Core.sumSize = (Core.sumSize v1),
                              Core.sumTerm = t})) val0 (Core.sumTerm v1))
                            Core.TermTypeApplication v1 -> (forSingle recurse (\t -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = t,
                              Core.typeApplicationTermType = (Core.typeApplicationTermType v1)})) val0 (Core.typeApplicationTermBody v1))
                            Core.TermTypeLambda v1 -> (forSingle recurse (\t -> Core.TermTypeLambda (Core.TypeLambda {
                              Core.typeLambdaParameter = (Core.typeLambdaParameter v1),
                              Core.typeLambdaBody = t})) val0 (Core.typeLambdaBody v1))
                            Core.TermUnion v1 -> (forSingle recurse (\t -> Core.TermUnion (Core.Injection {
                              Core.injectionTypeName = (Core.injectionTypeName v1),
                              Core.injectionField = Core.Field {
                                Core.fieldName = (Core.fieldName (Core.injectionField v1)),
                                Core.fieldTerm = t}})) val0 (Core.fieldTerm (Core.injectionField v1)))
                            Core.TermWrap v1 -> (forSingle recurse (\t -> Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.wrappedTermTypeName v1),
                              Core.wrappedTermBody = t})) val0 (Core.wrappedTermBody v1))
                            _ -> dflt) term0))
  in  
    let recurse = (f (fsub recurse))
    in (recurse term0)

rewriteTerm :: (((Core.Term -> Core.Term) -> Core.Term -> Core.Term) -> Core.Term -> Core.Term)
rewriteTerm f term0 =  
  let fsub = (\recurse -> \term ->  
          let forField = (\f -> Core.Field {
                  Core.fieldName = (Core.fieldName f),
                  Core.fieldTerm = (recurse (Core.fieldTerm f))})
          in  
            let forElimination = (\elm -> (\x -> case x of
                    Core.EliminationProduct v1 -> (Core.EliminationProduct v1)
                    Core.EliminationRecord v1 -> (Core.EliminationRecord v1)
                    Core.EliminationUnion v1 -> (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.caseStatementTypeName v1),
                      Core.caseStatementDefault = (Maybes.map recurse (Core.caseStatementDefault v1)),
                      Core.caseStatementCases = (Lists.map forField (Core.caseStatementCases v1))}))
                    Core.EliminationWrap v1 -> (Core.EliminationWrap v1)) elm)
            in  
              let forFunction = (\fun -> (\x -> case x of
                      Core.FunctionElimination v1 -> (Core.FunctionElimination (forElimination v1))
                      Core.FunctionLambda v1 -> (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.lambdaParameter v1),
                        Core.lambdaDomain = (Core.lambdaDomain v1),
                        Core.lambdaBody = (recurse (Core.lambdaBody v1))}))
                      Core.FunctionPrimitive v1 -> (Core.FunctionPrimitive v1)) fun)
              in  
                let forLet = (\lt ->  
                        let mapBinding = (\b -> Core.Binding {
                                Core.bindingName = (Core.bindingName b),
                                Core.bindingTerm = (recurse (Core.bindingTerm b)),
                                Core.bindingType = (Core.bindingType b)})
                        in Core.Let {
                          Core.letBindings = (Lists.map mapBinding (Core.letBindings lt)),
                          Core.letBody = (recurse (Core.letBody lt))})
                in  
                  let forMap = (\m ->  
                          let forPair = (\p -> (recurse (fst p), (recurse (snd p))))
                          in (Maps.fromList (Lists.map forPair (Maps.toList m))))
                  in ((\x -> case x of
                    Core.TermAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (recurse (Core.annotatedTermBody v1)),
                      Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))
                    Core.TermApplication v1 -> (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (recurse (Core.applicationFunction v1)),
                      Core.applicationArgument = (recurse (Core.applicationArgument v1))}))
                    Core.TermFunction v1 -> (Core.TermFunction (forFunction v1))
                    Core.TermLet v1 -> (Core.TermLet (forLet v1))
                    Core.TermList v1 -> (Core.TermList (Lists.map recurse v1))
                    Core.TermLiteral v1 -> (Core.TermLiteral v1)
                    Core.TermMap v1 -> (Core.TermMap (forMap v1))
                    Core.TermMaybe v1 -> (Core.TermMaybe (Maybes.map recurse v1))
                    Core.TermProduct v1 -> (Core.TermProduct (Lists.map recurse v1))
                    Core.TermRecord v1 -> (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.recordTypeName v1),
                      Core.recordFields = (Lists.map forField (Core.recordFields v1))}))
                    Core.TermSet v1 -> (Core.TermSet (Sets.fromList (Lists.map recurse (Sets.toList v1))))
                    Core.TermSum v1 -> (Core.TermSum (Core.Sum {
                      Core.sumIndex = (Core.sumIndex v1),
                      Core.sumSize = (Core.sumSize v1),
                      Core.sumTerm = (recurse (Core.sumTerm v1))}))
                    Core.TermTypeApplication v1 -> (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (recurse (Core.typeApplicationTermBody v1)),
                      Core.typeApplicationTermType = (Core.typeApplicationTermType v1)}))
                    Core.TermTypeLambda v1 -> (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.typeLambdaParameter v1),
                      Core.typeLambdaBody = (recurse (Core.typeLambdaBody v1))}))
                    Core.TermUnion v1 -> (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = (Core.injectionTypeName v1),
                      Core.injectionField = (forField (Core.injectionField v1))}))
                    Core.TermUnit -> Core.TermUnit
                    Core.TermVariable v1 -> (Core.TermVariable v1)
                    Core.TermWrap v1 -> (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.wrappedTermTypeName v1),
                      Core.wrappedTermBody = (recurse (Core.wrappedTermBody v1))}))) term))
  in  
    let recurse = (f (fsub recurse))
    in (recurse term0)

rewriteTermM :: (((Core.Term -> Compute.Flow t0 Core.Term) -> Core.Term -> Compute.Flow t0 Core.Term) -> Core.Term -> Compute.Flow t0 Core.Term)
rewriteTermM f term0 =  
  let fsub = (\recurse -> \term ->  
          let forField = (\field -> Flows.bind (recurse (Core.fieldTerm field)) (\t -> Flows.pure (Core.Field {
                  Core.fieldName = (Core.fieldName field),
                  Core.fieldTerm = t})))
          in  
            let forPair = (\kv -> Flows.bind (recurse (fst kv)) (\k -> Flows.bind (recurse (snd kv)) (\v -> Flows.pure (k, v))))
            in  
              let mapBinding = (\b -> Flows.bind (recurse (Core.bindingTerm b)) (\v -> Flows.pure (Core.Binding {
                      Core.bindingName = (Core.bindingName b),
                      Core.bindingTerm = v,
                      Core.bindingType = (Core.bindingType b)})))
              in ((\x -> case x of
                Core.TermAnnotated v1 -> (Flows.bind (recurse (Core.annotatedTermBody v1)) (\ex -> Flows.pure (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = ex,
                  Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))))
                Core.TermApplication v1 -> (Flows.bind (recurse (Core.applicationFunction v1)) (\lhs -> Flows.bind (recurse (Core.applicationArgument v1)) (\rhs -> Flows.pure (Core.TermApplication (Core.Application {
                  Core.applicationFunction = lhs,
                  Core.applicationArgument = rhs})))))
                Core.TermFunction v1 ->  
                  let forElm = (\e -> (\x -> case x of
                          Core.EliminationProduct v2 -> (Flows.pure (Core.FunctionElimination (Core.EliminationProduct v2)))
                          Core.EliminationRecord v2 -> (Flows.pure (Core.FunctionElimination (Core.EliminationRecord v2)))
                          Core.EliminationUnion v2 ->  
                            let n = (Core.caseStatementTypeName v2)
                            in  
                              let def = (Core.caseStatementDefault v2)
                              in  
                                let cases = (Core.caseStatementCases v2)
                                in (Flows.bind (Maybes.maybe (Flows.pure Nothing) (\t -> Flows.map Maybes.pure (recurse t)) def) (\rdef -> Flows.map (\rcases -> Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                  Core.caseStatementTypeName = n,
                                  Core.caseStatementDefault = rdef,
                                  Core.caseStatementCases = rcases}))) (Flows.mapList forField cases)))
                          Core.EliminationWrap v2 -> (Flows.pure (Core.FunctionElimination (Core.EliminationWrap v2)))) e)
                  in  
                    let forFun = (\fun -> (\x -> case x of
                            Core.FunctionElimination v2 -> (forElm v2)
                            Core.FunctionLambda v2 ->  
                              let v = (Core.lambdaParameter v2)
                              in  
                                let d = (Core.lambdaDomain v2)
                                in  
                                  let body = (Core.lambdaBody v2)
                                  in (Flows.bind (recurse body) (\rbody -> Flows.pure (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = v,
                                    Core.lambdaDomain = d,
                                    Core.lambdaBody = rbody}))))
                            Core.FunctionPrimitive v2 -> (Flows.pure (Core.FunctionPrimitive v2))) fun)
                    in (Flows.bind (forFun v1) (\rfun -> Flows.pure (Core.TermFunction rfun)))
                Core.TermLet v1 ->  
                  let bindings = (Core.letBindings v1)
                  in  
                    let env = (Core.letBody v1)
                    in (Flows.bind (Flows.mapList mapBinding bindings) (\rbindings -> Flows.bind (recurse env) (\renv -> Flows.pure (Core.TermLet (Core.Let {
                      Core.letBindings = rbindings,
                      Core.letBody = renv})))))
                Core.TermList v1 -> (Flows.bind (Flows.mapList recurse v1) (\rels -> Flows.pure (Core.TermList rels)))
                Core.TermLiteral v1 -> (Flows.pure (Core.TermLiteral v1))
                Core.TermMap v1 -> (Flows.bind (Flows.mapList forPair (Maps.toList v1)) (\pairs -> Flows.pure (Core.TermMap (Maps.fromList pairs))))
                Core.TermMaybe v1 -> (Flows.bind (Flows.mapMaybe recurse v1) (\rm -> Flows.pure (Core.TermMaybe rm)))
                Core.TermProduct v1 -> (Flows.map (\rtuple -> Core.TermProduct rtuple) (Flows.mapList recurse v1))
                Core.TermRecord v1 ->  
                  let n = (Core.recordTypeName v1)
                  in  
                    let fields = (Core.recordFields v1)
                    in (Flows.map (\rfields -> Core.TermRecord (Core.Record {
                      Core.recordTypeName = n,
                      Core.recordFields = rfields})) (Flows.mapList forField fields))
                Core.TermSet v1 -> (Flows.bind (Flows.mapList recurse (Sets.toList v1)) (\rlist -> Flows.pure (Core.TermSet (Sets.fromList rlist))))
                Core.TermSum v1 ->  
                  let i = (Core.sumIndex v1)
                  in  
                    let s = (Core.sumSize v1)
                    in  
                      let trm = (Core.sumTerm v1)
                      in (Flows.bind (recurse trm) (\rtrm -> Flows.pure (Core.TermSum (Core.Sum {
                        Core.sumIndex = i,
                        Core.sumSize = s,
                        Core.sumTerm = rtrm}))))
                Core.TermTypeApplication v1 -> (Flows.bind (recurse (Core.typeApplicationTermBody v1)) (\t -> Flows.pure (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = t,
                  Core.typeApplicationTermType = (Core.typeApplicationTermType v1)}))))
                Core.TermTypeLambda v1 ->  
                  let v = (Core.typeLambdaParameter v1)
                  in  
                    let body = (Core.typeLambdaBody v1)
                    in (Flows.bind (recurse body) (\rbody -> Flows.pure (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = v,
                      Core.typeLambdaBody = rbody}))))
                Core.TermUnion v1 ->  
                  let n = (Core.injectionTypeName v1)
                  in  
                    let field = (Core.injectionField v1)
                    in (Flows.map (\rfield -> Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = n,
                      Core.injectionField = rfield})) (forField field))
                Core.TermUnit -> (Flows.pure Core.TermUnit)
                Core.TermVariable v1 -> (Flows.pure (Core.TermVariable v1))
                Core.TermWrap v1 ->  
                  let name = (Core.wrappedTermTypeName v1)
                  in  
                    let t = (Core.wrappedTermBody v1)
                    in (Flows.bind (recurse t) (\rt -> Flows.pure (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = name,
                      Core.wrappedTermBody = rt}))))) term))
  in  
    let recurse = (f (fsub recurse))
    in (recurse term0)

rewriteTermWithContext :: (((t0 -> Core.Term -> Core.Term) -> t0 -> Core.Term -> Core.Term) -> t0 -> Core.Term -> Core.Term)
rewriteTermWithContext f cx0 term0 =  
  let forSubterms = (\recurse0 -> \cx -> \term ->  
          let recurse = (recurse0 cx)
          in  
            let forField = (\field -> Core.Field {
                    Core.fieldName = (Core.fieldName field),
                    Core.fieldTerm = (recurse (Core.fieldTerm field))})
            in  
              let forElimination = (\elm -> (\x -> case x of
                      Core.EliminationProduct v1 -> (Core.EliminationProduct v1)
                      Core.EliminationRecord v1 -> (Core.EliminationRecord v1)
                      Core.EliminationUnion v1 -> (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.caseStatementTypeName v1),
                        Core.caseStatementDefault = (Maybes.map recurse (Core.caseStatementDefault v1)),
                        Core.caseStatementCases = (Lists.map forField (Core.caseStatementCases v1))}))
                      Core.EliminationWrap v1 -> (Core.EliminationWrap v1)) elm)
              in  
                let forFunction = (\fun -> (\x -> case x of
                        Core.FunctionElimination v1 -> (Core.FunctionElimination (forElimination v1))
                        Core.FunctionLambda v1 -> (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.lambdaParameter v1),
                          Core.lambdaDomain = (Core.lambdaDomain v1),
                          Core.lambdaBody = (recurse (Core.lambdaBody v1))}))
                        Core.FunctionPrimitive v1 -> (Core.FunctionPrimitive v1)) fun)
                in  
                  let forLet = (\lt ->  
                          let mapBinding = (\b -> Core.Binding {
                                  Core.bindingName = (Core.bindingName b),
                                  Core.bindingTerm = (recurse (Core.bindingTerm b)),
                                  Core.bindingType = (Core.bindingType b)})
                          in Core.Let {
                            Core.letBindings = (Lists.map mapBinding (Core.letBindings lt)),
                            Core.letBody = (recurse (Core.letBody lt))})
                  in  
                    let forMap = (\m ->  
                            let forPair = (\p -> (recurse (fst p), (recurse (snd p))))
                            in (Maps.fromList (Lists.map forPair (Maps.toList m))))
                    in ((\x -> case x of
                      Core.TermAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = (recurse (Core.annotatedTermBody v1)),
                        Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))
                      Core.TermApplication v1 -> (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (recurse (Core.applicationFunction v1)),
                        Core.applicationArgument = (recurse (Core.applicationArgument v1))}))
                      Core.TermFunction v1 -> (Core.TermFunction (forFunction v1))
                      Core.TermLet v1 -> (Core.TermLet (forLet v1))
                      Core.TermList v1 -> (Core.TermList (Lists.map recurse v1))
                      Core.TermLiteral v1 -> (Core.TermLiteral v1)
                      Core.TermMap v1 -> (Core.TermMap (forMap v1))
                      Core.TermMaybe v1 -> (Core.TermMaybe (Maybes.map recurse v1))
                      Core.TermProduct v1 -> (Core.TermProduct (Lists.map recurse v1))
                      Core.TermRecord v1 -> (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.recordTypeName v1),
                        Core.recordFields = (Lists.map forField (Core.recordFields v1))}))
                      Core.TermSet v1 -> (Core.TermSet (Sets.fromList (Lists.map recurse (Sets.toList v1))))
                      Core.TermSum v1 -> (Core.TermSum (Core.Sum {
                        Core.sumIndex = (Core.sumIndex v1),
                        Core.sumSize = (Core.sumSize v1),
                        Core.sumTerm = (recurse (Core.sumTerm v1))}))
                      Core.TermTypeApplication v1 -> (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (recurse (Core.typeApplicationTermBody v1)),
                        Core.typeApplicationTermType = (Core.typeApplicationTermType v1)}))
                      Core.TermTypeLambda v1 -> (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.typeLambdaParameter v1),
                        Core.typeLambdaBody = (recurse (Core.typeLambdaBody v1))}))
                      Core.TermUnion v1 -> (Core.TermUnion (Core.Injection {
                        Core.injectionTypeName = (Core.injectionTypeName v1),
                        Core.injectionField = (forField (Core.injectionField v1))}))
                      Core.TermUnit -> Core.TermUnit
                      Core.TermVariable v1 -> (Core.TermVariable v1)
                      Core.TermWrap v1 -> (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.wrappedTermTypeName v1),
                        Core.wrappedTermBody = (recurse (Core.wrappedTermBody v1))}))) term))
  in  
    let rewrite = (\cx -> \term -> f (forSubterms rewrite) cx term)
    in (rewrite cx0 term0)

rewriteTermWithContextM :: (((t0 -> Core.Term -> Compute.Flow t1 Core.Term) -> t0 -> Core.Term -> Compute.Flow t1 Core.Term) -> t0 -> Core.Term -> Compute.Flow t1 Core.Term)
rewriteTermWithContextM f cx0 term0 =  
  let forSubterms = (\recurse0 -> \cx -> \term ->  
          let recurse = (recurse0 cx)
          in  
            let forField = (\field -> Flows.bind (recurse (Core.fieldTerm field)) (\t -> Flows.pure (Core.Field {
                    Core.fieldName = (Core.fieldName field),
                    Core.fieldTerm = t})))
            in  
              let forPair = (\kv -> Flows.bind (recurse (fst kv)) (\k -> Flows.bind (recurse (snd kv)) (\v -> Flows.pure (k, v))))
              in  
                let forElimination = (\e -> (\x -> case x of
                        Core.EliminationProduct v1 -> (Flows.pure (Core.FunctionElimination (Core.EliminationProduct v1)))
                        Core.EliminationRecord v1 -> (Flows.pure (Core.FunctionElimination (Core.EliminationRecord v1)))
                        Core.EliminationUnion v1 ->  
                          let n = (Core.caseStatementTypeName v1)
                          in  
                            let def = (Core.caseStatementDefault v1)
                            in  
                              let cases = (Core.caseStatementCases v1)
                              in (Flows.bind (Maybes.maybe (Flows.pure Nothing) (\t -> Flows.map Maybes.pure (recurse t)) def) (\rdef -> Flows.map (\rcases -> Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                Core.caseStatementTypeName = n,
                                Core.caseStatementDefault = rdef,
                                Core.caseStatementCases = rcases}))) (Flows.mapList forField cases)))
                        Core.EliminationWrap v1 -> (Flows.pure (Core.FunctionElimination (Core.EliminationWrap v1)))) e)
                in  
                  let forFunction = (\fun -> (\x -> case x of
                          Core.FunctionElimination v1 -> (forElimination v1)
                          Core.FunctionLambda v1 ->  
                            let v = (Core.lambdaParameter v1)
                            in  
                              let d = (Core.lambdaDomain v1)
                              in  
                                let body = (Core.lambdaBody v1)
                                in (Flows.bind (recurse body) (\rbody -> Flows.pure (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = v,
                                  Core.lambdaDomain = d,
                                  Core.lambdaBody = rbody}))))
                          Core.FunctionPrimitive v1 -> (Flows.pure (Core.FunctionPrimitive v1))) fun)
                  in  
                    let mapBinding = (\b -> Flows.bind (recurse (Core.bindingTerm b)) (\v -> Flows.pure (Core.Binding {
                            Core.bindingName = (Core.bindingName b),
                            Core.bindingTerm = v,
                            Core.bindingType = (Core.bindingType b)})))
                    in ((\x -> case x of
                      Core.TermAnnotated v1 -> (Flows.bind (recurse (Core.annotatedTermBody v1)) (\ex -> Flows.pure (Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = ex,
                        Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))))
                      Core.TermApplication v1 -> (Flows.bind (recurse (Core.applicationFunction v1)) (\lhs -> Flows.bind (recurse (Core.applicationArgument v1)) (\rhs -> Flows.pure (Core.TermApplication (Core.Application {
                        Core.applicationFunction = lhs,
                        Core.applicationArgument = rhs})))))
                      Core.TermFunction v1 -> (Flows.bind (forFunction v1) (\rfun -> Flows.pure (Core.TermFunction rfun)))
                      Core.TermLet v1 ->  
                        let bindings = (Core.letBindings v1)
                        in  
                          let body = (Core.letBody v1)
                          in (Flows.bind (Flows.mapList mapBinding bindings) (\rbindings -> Flows.bind (recurse body) (\rbody -> Flows.pure (Core.TermLet (Core.Let {
                            Core.letBindings = rbindings,
                            Core.letBody = rbody})))))
                      Core.TermList v1 -> (Flows.bind (Flows.mapList recurse v1) (\rels -> Flows.pure (Core.TermList rels)))
                      Core.TermLiteral v1 -> (Flows.pure (Core.TermLiteral v1))
                      Core.TermMap v1 -> (Flows.bind (Flows.mapList forPair (Maps.toList v1)) (\pairs -> Flows.pure (Core.TermMap (Maps.fromList pairs))))
                      Core.TermMaybe v1 -> (Flows.bind (Flows.mapMaybe recurse v1) (\rm -> Flows.pure (Core.TermMaybe rm)))
                      Core.TermProduct v1 -> (Flows.map (\rtuple -> Core.TermProduct rtuple) (Flows.mapList recurse v1))
                      Core.TermRecord v1 ->  
                        let n = (Core.recordTypeName v1)
                        in  
                          let fields = (Core.recordFields v1)
                          in (Flows.map (\rfields -> Core.TermRecord (Core.Record {
                            Core.recordTypeName = n,
                            Core.recordFields = rfields})) (Flows.mapList forField fields))
                      Core.TermSet v1 -> (Flows.bind (Flows.mapList recurse (Sets.toList v1)) (\rlist -> Flows.pure (Core.TermSet (Sets.fromList rlist))))
                      Core.TermSum v1 ->  
                        let i = (Core.sumIndex v1)
                        in  
                          let s = (Core.sumSize v1)
                          in  
                            let trm = (Core.sumTerm v1)
                            in (Flows.bind (recurse trm) (\rtrm -> Flows.pure (Core.TermSum (Core.Sum {
                              Core.sumIndex = i,
                              Core.sumSize = s,
                              Core.sumTerm = rtrm}))))
                      Core.TermTypeApplication v1 -> (Flows.bind (recurse (Core.typeApplicationTermBody v1)) (\t -> Flows.pure (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = t,
                        Core.typeApplicationTermType = (Core.typeApplicationTermType v1)}))))
                      Core.TermTypeLambda v1 ->  
                        let v = (Core.typeLambdaParameter v1)
                        in  
                          let body = (Core.typeLambdaBody v1)
                          in (Flows.bind (recurse body) (\rbody -> Flows.pure (Core.TermTypeLambda (Core.TypeLambda {
                            Core.typeLambdaParameter = v,
                            Core.typeLambdaBody = rbody}))))
                      Core.TermUnion v1 ->  
                        let n = (Core.injectionTypeName v1)
                        in  
                          let field = (Core.injectionField v1)
                          in (Flows.map (\rfield -> Core.TermUnion (Core.Injection {
                            Core.injectionTypeName = n,
                            Core.injectionField = rfield})) (forField field))
                      Core.TermUnit -> (Flows.pure Core.TermUnit)
                      Core.TermVariable v1 -> (Flows.pure (Core.TermVariable v1))
                      Core.TermWrap v1 ->  
                        let name = (Core.wrappedTermTypeName v1)
                        in  
                          let t = (Core.wrappedTermBody v1)
                          in (Flows.bind (recurse t) (\rt -> Flows.pure (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = name,
                            Core.wrappedTermBody = rt}))))) term))
  in  
    let rewrite = (\cx -> \term -> f (forSubterms rewrite) cx term)
    in (rewrite cx0 term0)

rewriteType :: (((Core.Type -> Core.Type) -> Core.Type -> Core.Type) -> Core.Type -> Core.Type)
rewriteType f typ0 =  
  let fsub = (\recurse -> \typ ->  
          let forField = (\field -> Core.FieldType {
                  Core.fieldTypeName = (Core.fieldTypeName field),
                  Core.fieldTypeType = (recurse (Core.fieldTypeType field))})
          in ((\x -> case x of
            Core.TypeAnnotated v1 -> (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (recurse (Core.annotatedTypeBody v1)),
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
            Core.TypeMaybe v1 -> (Core.TypeMaybe (recurse v1))
            Core.TypeProduct v1 -> (Core.TypeProduct (Lists.map recurse v1))
            Core.TypeRecord v1 -> (Core.TypeRecord (Core.RowType {
              Core.rowTypeTypeName = (Core.rowTypeTypeName v1),
              Core.rowTypeFields = (Lists.map forField (Core.rowTypeFields v1))}))
            Core.TypeSet v1 -> (Core.TypeSet (recurse v1))
            Core.TypeSum v1 -> (Core.TypeSum (Lists.map recurse v1))
            Core.TypeUnion v1 -> (Core.TypeUnion (Core.RowType {
              Core.rowTypeTypeName = (Core.rowTypeTypeName v1),
              Core.rowTypeFields = (Lists.map forField (Core.rowTypeFields v1))}))
            Core.TypeUnit -> Core.TypeUnit
            Core.TypeVariable v1 -> (Core.TypeVariable v1)
            Core.TypeWrap v1 -> (Core.TypeWrap (Core.WrappedType {
              Core.wrappedTypeTypeName = (Core.wrappedTypeTypeName v1),
              Core.wrappedTypeBody = (recurse (Core.wrappedTypeBody v1))}))) typ))
  in  
    let recurse = (f (fsub recurse))
    in (recurse typ0)

rewriteTypeM :: (((Core.Type -> Compute.Flow t0 Core.Type) -> Core.Type -> Compute.Flow t0 Core.Type) -> Core.Type -> Compute.Flow t0 Core.Type)
rewriteTypeM f typ0 =  
  let fsub = (\recurse -> \typ -> (\x -> case x of
          Core.TypeAnnotated v1 -> (Flows.bind (recurse (Core.annotatedTypeBody v1)) (\t -> Flows.pure (Core.TypeAnnotated (Core.AnnotatedType {
            Core.annotatedTypeBody = t,
            Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)}))))
          Core.TypeApplication v1 -> (Flows.bind (recurse (Core.applicationTypeFunction v1)) (\lhs -> Flows.bind (recurse (Core.applicationTypeArgument v1)) (\rhs -> Flows.pure (Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = lhs,
            Core.applicationTypeArgument = rhs})))))
          Core.TypeFunction v1 -> (Flows.bind (recurse (Core.functionTypeDomain v1)) (\dom -> Flows.bind (recurse (Core.functionTypeCodomain v1)) (\cod -> Flows.pure (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = dom,
            Core.functionTypeCodomain = cod})))))
          Core.TypeForall v1 -> (Flows.bind (recurse (Core.forallTypeBody v1)) (\b -> Flows.pure (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.forallTypeParameter v1),
            Core.forallTypeBody = b}))))
          Core.TypeList v1 -> (Flows.bind (recurse v1) (\rt -> Flows.pure (Core.TypeList rt)))
          Core.TypeLiteral v1 -> (Flows.pure (Core.TypeLiteral v1))
          Core.TypeMap v1 -> (Flows.bind (recurse (Core.mapTypeKeys v1)) (\kt -> Flows.bind (recurse (Core.mapTypeValues v1)) (\vt -> Flows.pure (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = kt,
            Core.mapTypeValues = vt})))))
          Core.TypeMaybe v1 -> (Flows.bind (recurse v1) (\rt -> Flows.pure (Core.TypeMaybe rt)))
          Core.TypeProduct v1 -> (Flows.bind (Flows.mapList recurse v1) (\rtypes -> Flows.pure (Core.TypeProduct rtypes)))
          Core.TypeRecord v1 ->  
            let name = (Core.rowTypeTypeName v1)
            in  
              let fields = (Core.rowTypeFields v1)
              in  
                let forField = (\f -> Flows.bind (recurse (Core.fieldTypeType f)) (\t -> Flows.pure (Core.FieldType {
                        Core.fieldTypeName = (Core.fieldTypeName f),
                        Core.fieldTypeType = t})))
                in (Flows.bind (Flows.mapList forField fields) (\rfields -> Flows.pure (Core.TypeRecord (Core.RowType {
                  Core.rowTypeTypeName = name,
                  Core.rowTypeFields = rfields}))))
          Core.TypeSet v1 -> (Flows.bind (recurse v1) (\rt -> Flows.pure (Core.TypeSet rt)))
          Core.TypeSum v1 -> (Flows.bind (Flows.mapList recurse v1) (\rtypes -> Flows.pure (Core.TypeSum rtypes)))
          Core.TypeUnion v1 ->  
            let name = (Core.rowTypeTypeName v1)
            in  
              let fields = (Core.rowTypeFields v1)
              in  
                let forField = (\f -> Flows.bind (recurse (Core.fieldTypeType f)) (\t -> Flows.pure (Core.FieldType {
                        Core.fieldTypeName = (Core.fieldTypeName f),
                        Core.fieldTypeType = t})))
                in (Flows.bind (Flows.mapList forField fields) (\rfields -> Flows.pure (Core.TypeUnion (Core.RowType {
                  Core.rowTypeTypeName = name,
                  Core.rowTypeFields = rfields}))))
          Core.TypeUnit -> (Flows.pure Core.TypeUnit)
          Core.TypeVariable v1 -> (Flows.pure (Core.TypeVariable v1))
          Core.TypeWrap v1 -> (Flows.bind (recurse (Core.wrappedTypeBody v1)) (\t -> Flows.pure (Core.TypeWrap (Core.WrappedType {
            Core.wrappedTypeTypeName = (Core.wrappedTypeTypeName v1),
            Core.wrappedTypeBody = t}))))) typ)
  in  
    let recurse = (f (fsub recurse))
    in (recurse typ0)

-- | Simplify terms by applying beta reduction where possible
simplifyTerm :: (Core.Term -> Core.Term)
simplifyTerm term =  
  let simplify = (\recurse -> \term ->  
          let forRhs = (\rhs -> \var -> \body -> (\x -> case x of
                  Core.TermVariable v1 -> (simplifyTerm (substituteVariable var v1 body))
                  _ -> term) (deannotateTerm rhs))
          in  
            let forLhs = (\lhs -> \rhs ->  
                    let forFun = (\fun -> (\x -> case x of
                            Core.FunctionLambda v1 ->  
                              let var = (Core.lambdaParameter v1)
                              in  
                                let body = (Core.lambdaBody v1)
                                in (Logic.ifElse (Sets.member var (freeVariablesInTerm body)) (forRhs rhs var body) (simplifyTerm body))
                            _ -> term) fun)
                    in ((\x -> case x of
                      Core.TermFunction v1 -> (forFun v1)
                      _ -> term) (deannotateTerm lhs)))
            in  
              let forTerm = (\stripped -> (\x -> case x of
                      Core.TermApplication v1 ->  
                        let lhs = (Core.applicationFunction v1)
                        in  
                          let rhs = (Core.applicationArgument v1)
                          in (forLhs lhs rhs)
                      _ -> term) stripped)
              in  
                let stripped = (deannotateTerm term)
                in (recurse (forTerm stripped)))
  in (rewriteTerm simplify term)

-- | Substitute type variables in a type
substituteTypeVariables :: (M.Map Core.Name Core.Name -> Core.Type -> Core.Type)
substituteTypeVariables subst typ =  
  let replace = (\recurse -> \typ -> (\x -> case x of
          Core.TypeVariable v1 -> (Core.TypeVariable (Maybes.fromMaybe v1 (Maps.lookup v1 subst)))
          _ -> (recurse typ)) typ)
  in (rewriteType replace typ)

-- | Substitute one variable for another in a term
substituteVariable :: (Core.Name -> Core.Name -> Core.Term -> Core.Term)
substituteVariable from to term =  
  let replace = (\recurse -> \term -> (\x -> case x of
          Core.TermVariable v1 -> (Core.TermVariable (Logic.ifElse (Equality.equal v1 from) to v1))
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionLambda v2 -> (Logic.ifElse (Equality.equal (Core.lambdaParameter v2) from) term (recurse term))
            _ -> (recurse term)) v1)
          _ -> (recurse term)) term)
  in (rewriteTerm replace term)

-- | Substitute multiple variables in a term
substituteVariables :: (M.Map Core.Name Core.Name -> Core.Term -> Core.Term)
substituteVariables subst term =  
  let replace = (\recurse -> \term -> (\x -> case x of
          Core.TermVariable v1 -> (Core.TermVariable (Maybes.fromMaybe v1 (Maps.lookup v1 subst)))
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionLambda v2 -> (Maybes.maybe (recurse term) (\_ -> term) (Maps.lookup (Core.lambdaParameter v2) subst))
            _ -> (recurse term)) v1)
          _ -> (recurse term)) term)
  in (rewriteTerm replace term)

-- | Find the children of a given term
subterms :: (Core.Term -> [Core.Term])
subterms x = case x of
  Core.TermAnnotated v1 -> [
    Core.annotatedTermBody v1]
  Core.TermApplication v1 -> [
    Core.applicationFunction v1,
    (Core.applicationArgument v1)]
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationUnion v3 -> (Lists.concat2 (Maybes.maybe [] (\t -> [
        t]) (Core.caseStatementDefault v3)) (Lists.map Core.fieldTerm (Core.caseStatementCases v3)))
      _ -> []) v2)
    Core.FunctionLambda v2 -> [
      Core.lambdaBody v2]
    _ -> []) v1)
  Core.TermLet v1 -> (Lists.cons (Core.letBody v1) (Lists.map Core.bindingTerm (Core.letBindings v1)))
  Core.TermList v1 -> v1
  Core.TermLiteral _ -> []
  Core.TermMap v1 -> (Lists.concat (Lists.map (\p -> [
    fst p,
    (snd p)]) (Maps.toList v1)))
  Core.TermMaybe v1 -> (Maybes.maybe [] (\t -> [
    t]) v1)
  Core.TermProduct v1 -> v1
  Core.TermRecord v1 -> (Lists.map Core.fieldTerm (Core.recordFields v1))
  Core.TermSet v1 -> (Sets.toList v1)
  Core.TermSum v1 -> [
    Core.sumTerm v1]
  Core.TermTypeApplication v1 -> [
    Core.typeApplicationTermBody v1]
  Core.TermTypeLambda v1 -> [
    Core.typeLambdaBody v1]
  Core.TermUnion v1 -> [
    Core.fieldTerm (Core.injectionField v1)]
  Core.TermUnit -> []
  Core.TermVariable _ -> []
  Core.TermWrap v1 -> [
    Core.wrappedTermBody v1]

-- | Find the children of a given term
subtermsWithAccessors :: (Core.Term -> [(Accessors.TermAccessor, Core.Term)])
subtermsWithAccessors x = case x of
  Core.TermAnnotated v1 -> [
    (Accessors.TermAccessorAnnotatedBody, (Core.annotatedTermBody v1))]
  Core.TermApplication v1 -> [
    (Accessors.TermAccessorApplicationFunction, (Core.applicationFunction v1)),
    (Accessors.TermAccessorApplicationArgument, (Core.applicationArgument v1))]
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationUnion v3 -> (Lists.concat2 (Maybes.maybe [] (\t -> [
        (Accessors.TermAccessorUnionCasesDefault, t)]) (Core.caseStatementDefault v3)) (Lists.map (\f -> (Accessors.TermAccessorUnionCasesBranch (Core.fieldName f), (Core.fieldTerm f))) (Core.caseStatementCases v3)))
      _ -> []) v2)
    Core.FunctionLambda v2 -> [
      (Accessors.TermAccessorLambdaBody, (Core.lambdaBody v2))]
    _ -> []) v1)
  Core.TermLet v1 -> (Lists.cons (Accessors.TermAccessorLetBody, (Core.letBody v1)) (Lists.map (\b -> (Accessors.TermAccessorLetBinding (Core.bindingName b), (Core.bindingTerm b))) (Core.letBindings v1)))
  Core.TermList v1 -> (Lists.map (\e -> (Accessors.TermAccessorListElement 0, e)) v1)
  Core.TermLiteral _ -> []
  Core.TermMap v1 -> (Lists.concat (Lists.map (\p -> [
    (Accessors.TermAccessorMapKey 0, (fst p)),
    (Accessors.TermAccessorMapValue 0, (snd p))]) (Maps.toList v1)))
  Core.TermMaybe v1 -> (Maybes.maybe [] (\t -> [
    (Accessors.TermAccessorMaybeTerm, t)]) v1)
  Core.TermProduct v1 -> (Lists.map (\e -> (Accessors.TermAccessorProductTerm 0, e)) v1)
  Core.TermRecord v1 -> (Lists.map (\f -> (Accessors.TermAccessorRecordField (Core.fieldName f), (Core.fieldTerm f))) (Core.recordFields v1))
  Core.TermSet v1 -> (Lists.map (\e -> (Accessors.TermAccessorListElement 0, e)) (Sets.toList v1))
  Core.TermSum v1 -> [
    (Accessors.TermAccessorSumTerm, (Core.sumTerm v1))]
  Core.TermTypeApplication v1 -> [
    (Accessors.TermAccessorTypeApplicationTerm, (Core.typeApplicationTermBody v1))]
  Core.TermTypeLambda v1 -> [
    (Accessors.TermAccessorTypeLambdaBody, (Core.typeLambdaBody v1))]
  Core.TermUnion v1 -> [
    (Accessors.TermAccessorInjectionTerm, (Core.fieldTerm (Core.injectionField v1)))]
  Core.TermUnit -> []
  Core.TermVariable _ -> []
  Core.TermWrap v1 -> [
    (Accessors.TermAccessorWrappedTerm, (Core.wrappedTermBody v1))]

-- | Find the children of a given type expression
subtypes :: (Core.Type -> [Core.Type])
subtypes x = case x of
  Core.TypeAnnotated v1 -> [
    Core.annotatedTypeBody v1]
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
  Core.TypeMaybe v1 -> [
    v1]
  Core.TypeProduct v1 -> v1
  Core.TypeRecord v1 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v1))
  Core.TypeSet v1 -> [
    v1]
  Core.TypeSum v1 -> v1
  Core.TypeUnion v1 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v1))
  Core.TypeUnit -> []
  Core.TypeVariable _ -> []
  Core.TypeWrap v1 -> [
    Core.wrappedTypeBody v1]

-- | Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that
termDependencyNames :: (Bool -> Bool -> Bool -> Core.Term -> S.Set Core.Name)
termDependencyNames binds withPrims withNoms term0 =  
  let addNames = (\names -> \term ->  
          let nominal = (\name -> Logic.ifElse withNoms (Sets.insert name names) names)
          in  
            let prim = (\name -> Logic.ifElse withPrims (Sets.insert name names) names)
            in  
              let var = (\name -> Logic.ifElse binds (Sets.insert name names) names)
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
  in (foldOverTerm Coders.TraversalOrderPre addNames Sets.empty term0)

-- | Generate short names from a list of fully qualified names
toShortNames :: ([Core.Name] -> M.Map Core.Name Core.Name)
toShortNames original =  
  let addName = (\acc -> \name ->  
          let local = (Names.localNameOf name)
          in  
            let group = (Maybes.fromMaybe Sets.empty (Maps.lookup local acc))
            in (Maps.insert local (Sets.insert name group) acc))
  in  
    let groupNamesByLocal = (\names -> Lists.foldl addName Maps.empty names)
    in  
      let groups = (groupNamesByLocal original)
      in  
        let renameGroup = (\localNames ->  
                let local = (fst localNames)
                in  
                  let names = (snd localNames)
                  in  
                    let rangeFrom = (\start -> Lists.cons start (rangeFrom (Math.add start 1)))
                    in  
                      let rename = (\name -> \i -> (name, (Core.Name (Logic.ifElse (Equality.gt i 1) (Strings.cat2 local (Literals.showInt32 i)) local))))
                      in (Lists.zipWith rename (Sets.toList names) (rangeFrom 1)))
        in (Maps.fromList (Lists.concat (Lists.map renameGroup (Maps.toList groups))))

-- | Topological sort of connected components, in terms of dependencies between variable/term binding pairs
topologicalSortBindingMap :: (M.Map Core.Name Core.Term -> [[(Core.Name, Core.Term)]])
topologicalSortBindingMap bindingMap =  
  let bindings = (Maps.toList bindingMap)
  in  
    let keys = (Sets.fromList (Lists.map fst bindings))
    in  
      let hasTypeAnnotation = (\term -> (\x -> case x of
              Core.TermAnnotated v1 -> (hasTypeAnnotation (Core.annotatedTermBody v1))
              _ -> False) term)
      in  
        let depsOf = (\nameAndTerm ->  
                let name = (fst nameAndTerm)
                in  
                  let term = (snd nameAndTerm)
                  in (name, (Logic.ifElse (hasTypeAnnotation term) [] (Sets.toList (Sets.intersection keys (freeVariablesInTerm term))))))
        in  
          let toPair = (\name -> (name, (Maybes.fromMaybe (Core.TermLiteral (Core.LiteralString "Impossible!")) (Maps.lookup name bindingMap))))
          in (Lists.map (Lists.map toPair) (Sorting.topologicalSortComponents (Lists.map depsOf bindings)))

-- | Topological sort of elements based on their dependencies
topologicalSortBindings :: ([Core.Binding] -> Mantle.Either [[Core.Name]] [Core.Name])
topologicalSortBindings els =  
  let adjlist = (\e -> (Core.bindingName e, (Sets.toList (termDependencyNames False True True (Core.bindingTerm e)))))
  in (Sorting.topologicalSort (Lists.map adjlist els))

typeDependencyNames :: (Bool -> Core.Type -> S.Set Core.Name)
typeDependencyNames withSchema typ = (Logic.ifElse withSchema (Sets.union (freeVariablesInType typ) (typeNamesInType typ)) (freeVariablesInType typ))

typeNamesInType :: (Core.Type -> S.Set Core.Name)
typeNamesInType typ0 =  
  let addNames = (\names -> \typ -> (\x -> case x of
          Core.TypeRecord v1 ->  
            let tname = (Core.rowTypeTypeName v1)
            in (Sets.insert tname names)
          Core.TypeUnion v1 ->  
            let tname = (Core.rowTypeTypeName v1)
            in (Sets.insert tname names)
          Core.TypeWrap v1 ->  
            let tname = (Core.wrappedTypeTypeName v1)
            in (Sets.insert tname names)
          _ -> names) typ)
  in (foldOverType Coders.TraversalOrderPre addNames Sets.empty typ0)

-- | Unshadow lambda-bound variables in a term
unshadowVariables :: (Core.Term -> Core.Term)
unshadowVariables term =  
  let rewrite = (\recurse -> \m -> \term ->  
          let dflt = (recurse m term)
          in ((\x -> case x of
            Core.TermFunction v1 -> ((\x -> case x of
              Core.FunctionLambda v2 ->  
                let v = (Core.lambdaParameter v2)
                in  
                  let domain = (Core.lambdaDomain v2)
                  in  
                    let body = (Core.lambdaBody v2)
                    in (m, (Maybes.maybe (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = v,
                      Core.lambdaDomain = domain,
                      Core.lambdaBody = (snd (rewrite recurse (Maps.insert v 1 m) body))}))) (\i ->  
                      let i2 = (Math.add i 1)
                      in  
                        let v2 = (Core.Name (Strings.cat2 (Core.unName v) (Literals.showInt32 i2)))
                        in  
                          let m2 = (Maps.insert v i2 m)
                          in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = v2,
                            Core.lambdaDomain = domain,
                            Core.lambdaBody = (snd (rewrite recurse m2 body))})))) (Maps.lookup v m)))
              _ -> dflt) v1)
            Core.TermVariable v1 -> (m, (Core.TermVariable (Maybes.maybe v1 (\i -> Logic.ifElse (Equality.equal i 1) v1 (Core.Name (Strings.cat2 (Core.unName v1) (Literals.showInt32 i)))) (Maps.lookup v1 m))))
            _ -> dflt) term))
  in (snd (rewriteAndFoldTerm rewrite Maps.empty term))
