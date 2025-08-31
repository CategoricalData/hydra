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
import qualified Hydra.Lib.Optionals as Optionals
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
  Core.TermAnnotated v1 -> (deannotateAndDetypeTerm (Core.annotatedTermSubject v1))
  Core.TermTypeApplication v1 -> (deannotateAndDetypeTerm (Core.typedTermTerm v1))
  Core.TermTypeLambda v1 -> (deannotateAndDetypeTerm (Core.typeLambdaBody v1))
  _ -> t) t)

-- | Strip all annotations (including System F type annotations) from the top levels of a term
deannotateTerm :: (Core.Term -> Core.Term)
deannotateTerm t = ((\x -> case x of
  Core.TermAnnotated v1 -> (deannotateTerm (Core.annotatedTermSubject v1))
  _ -> t) t)

-- | Strip all annotations from a term
deannotateType :: (Core.Type -> Core.Type)
deannotateType t = ((\x -> case x of
  Core.TypeAnnotated v1 -> (deannotateType (Core.annotatedTypeSubject v1))
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
            Core.TypeAnnotated v1 -> (Core.annotatedTypeSubject v1)
            _ -> rewritten) rewritten))
  in (rewriteType strip typ)

-- | Recursively strip all annotations from a type scheme
deannotateTypeSchemeRecursive :: (Core.TypeScheme -> Core.TypeScheme)
deannotateTypeSchemeRecursive ts =  
  let vars = (Core.typeSchemeVariables ts) 
      typ = (Core.typeSchemeType ts)
  in Core.TypeScheme {
    Core.typeSchemeVariables = vars,
    Core.typeSchemeType = (deannotateTypeRecursive typ)}

-- | A variation of expandLambdas which also attaches type annotations when padding function terms
expandTypedLambdas :: (Core.Term -> Core.Term)
expandTypedLambdas term =  
  let toNaryFunType = (\typ ->  
          let helper = (\t -> (\x -> case x of
                  Core.TypeFunction v1 ->  
                    let dom0 = (Core.functionTypeDomain v1) 
                        cod0 = (Core.functionTypeCodomain v1)
                        recursive = (helper cod0)
                        doms = (fst recursive)
                        cod1 = (snd recursive)
                    in (Lists.cons dom0 doms, cod1)
                  _ -> ([], t)) t)
          in (helper (deannotateType typ))) 
      padTerm = (\i -> \doms -> \cod -> \term -> Logic.ifElse (Lists.null doms) term ( 
              let dom = (Lists.head doms) 
                  var = (Core.Name (Strings.cat2 "v" (Literals.showInt32 i)))
                  tailDoms = (Lists.tail doms)
                  toFunctionType = (\doms -> \cod -> Lists.foldl (\c -> \d -> Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = d,
                          Core.functionTypeCodomain = c})) cod doms)
              in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = var,
                Core.lambdaDomain = (Just dom),
                Core.lambdaBody = (padTerm (Math.add i 1) tailDoms cod (Core.TermApplication (Core.Application {
                  Core.applicationFunction = term,
                  Core.applicationArgument = (Core.TermVariable var)})))})))))
      expand = (\doms -> \cod -> \term -> (\x -> case x of
              Core.TermAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermSubject = (expand doms cod (Core.annotatedTermSubject v1)),
                Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))
              Core.TermApplication v1 ->  
                let lhs = (Core.applicationFunction v1) 
                    rhs = (Core.applicationArgument v1)
                in (rewriteTerm rewrite term)
              Core.TermFunction v1 -> ((\x -> case x of
                Core.FunctionLambda v2 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.lambdaParameter v2),
                  Core.lambdaDomain = (Core.lambdaDomain v2),
                  Core.lambdaBody = (expand (Lists.tail doms) cod (Core.lambdaBody v2))})))
                _ -> (padTerm 1 doms cod term)) v1)
              Core.TermLet v1 ->  
                let expandBinding = (\b -> Core.Binding {
                        Core.bindingName = (Core.bindingName b),
                        Core.bindingTerm = (expandTypedLambdas (Core.bindingTerm b)),
                        Core.bindingType = (Core.bindingType b)})
                in (Core.TermLet (Core.Let {
                  Core.letBindings = (Lists.map expandBinding (Core.letBindings v1)),
                  Core.letEnvironment = (expand doms cod (Core.letEnvironment v1))}))
              _ -> (rewriteTerm rewrite term)) term)
      rewrite = (\recurse -> \term -> recurse term)
  in (rewriteTerm rewrite term)

-- | Flatten nested let expressions
flattenLetTerms :: (Core.Term -> Core.Term)
flattenLetTerms term =  
  let rewriteBinding = (\binding ->  
          let key0 = (Core.bindingName binding) 
              val0 = (Core.bindingTerm binding)
              t = (Core.bindingType binding)
          in ((\x -> case x of
            Core.TermAnnotated v1 ->  
              let val1 = (Core.annotatedTermSubject v1) 
                  ann = (Core.annotatedTermAnnotation v1)
                  recursive = (rewriteBinding (Core.Binding {
                          Core.bindingName = key0,
                          Core.bindingTerm = val1,
                          Core.bindingType = t}))
                  innerBinding = (fst recursive)
                  deps = (snd recursive)
                  val2 = (Core.bindingTerm innerBinding)
              in (Core.Binding {
                Core.bindingName = key0,
                Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermSubject = val2,
                  Core.annotatedTermAnnotation = ann})),
                Core.bindingType = t}, deps)
            Core.TermLet v1 ->  
              let bindings1 = (Core.letBindings v1) 
                  body1 = (Core.letEnvironment v1)
                  prefix = (Strings.cat2 (Core.unName key0) "_")
                  qualify = (\n -> Core.Name (Strings.cat2 prefix (Core.unName n)))
                  toSubstPair = (\b -> (Core.bindingName b, (qualify (Core.bindingName b))))
                  subst = (Maps.fromList (Lists.map toSubstPair bindings1))
                  replaceVars = (substituteVariables subst)
                  newBody = (replaceVars body1)
                  newBinding = (\b -> Core.Binding {
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
      flatten = (\recurse -> \term ->  
              let rewritten = (recurse term)
              in ((\x -> case x of
                Core.TermLet v1 ->  
                  let bindings = (Core.letBindings v1) 
                      body = (Core.letEnvironment v1)
                      forResult = (\hr -> Lists.cons (fst hr) (snd hr))
                      newBindings = (Lists.concat (Lists.map (\arg_ -> forResult (rewriteBinding arg_)) bindings))
                  in (Core.TermLet (Core.Let {
                    Core.letBindings = newBindings,
                    Core.letEnvironment = body}))
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
      t = (Core.typeSchemeType ts)
  in (Sets.difference (freeVariablesInTypeSimple t) (Sets.fromList vars))

-- | Find free variables in a type scheme
freeVariablesInTypeScheme :: (Core.TypeScheme -> S.Set Core.Name)
freeVariablesInTypeScheme ts =  
  let vars = (Core.typeSchemeVariables ts) 
      t = (Core.typeSchemeType ts)
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
  let f = (\recurse -> \typ -> Flows.bind (recurse typ) (\tr -> (\x -> case x of
          Core.TypeVariable v1 -> (Optionals.maybe (Flows.fail (Strings.cat2 "No such type in schema: " (Core.unName v1))) (inlineType schema) (Maps.lookup v1 schema))
          _ -> (Flows.pure tr)) tr))
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
  Core.TermLet v1 -> (isLambda (Core.letEnvironment v1))
  _ -> False) (deannotateTerm term))

-- | Apply a transformation to the first type beneath a chain of annotations
mapBeneathTypeAnnotations :: ((Core.Type -> Core.Type) -> Core.Type -> Core.Type)
mapBeneathTypeAnnotations f t = ((\x -> case x of
  Core.TypeAnnotated v1 -> (Core.TypeAnnotated (Core.AnnotatedType {
    Core.annotatedTypeSubject = (mapBeneathTypeAnnotations f (Core.annotatedTypeSubject v1)),
    Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)}))
  _ -> (f t)) t)

-- | Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ...
normalizeTypeVariablesInTerm :: (Core.Term -> Core.Term)
normalizeTypeVariablesInTerm term =  
  let substType = (\subst -> \typ ->  
          let rewrite = (\recurse -> \typ -> (\x -> case x of
                  Core.TypeVariable v1 -> (Core.TypeVariable (replaceName subst v1))
                  _ -> (recurse typ)) typ)
          in (rewriteType rewrite typ)) 
      replaceName = (\subst -> \v -> Optionals.fromMaybe v (Maps.lookup v subst))
      rewriteWithSubst = (\substAndBound ->  
              let subst = (fst substAndBound) 
                  boundVars = (snd substAndBound)
                  rewrite = (\recurse -> \term -> (\x -> case x of
                          Core.TermFunction v1 -> ((\x -> case x of
                            Core.FunctionElimination v2 -> ((\x -> case x of
                              Core.EliminationProduct v3 ->  
                                let arity = (Core.tupleProjectionArity v3) 
                                    index = (Core.tupleProjectionIndex v3)
                                    domain = (Core.tupleProjectionDomain v3)
                                in (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                                  Core.tupleProjectionArity = arity,
                                  Core.tupleProjectionIndex = index,
                                  Core.tupleProjectionDomain = (Optionals.map (\types -> Lists.map (substType subst) types) domain)}))))
                              _ -> (recurse term)) v2)
                            Core.FunctionLambda v2 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.lambdaParameter v2),
                              Core.lambdaDomain = (Optionals.map (substType subst) (Core.lambdaDomain v2)),
                              Core.lambdaBody = (rewriteWithSubst (subst, boundVars) (Core.lambdaBody v2))})))
                            _ -> (recurse term)) v1)
                          Core.TermLet v1 ->  
                            let bindings = (Core.letBindings v1) 
                                env = (Core.letEnvironment v1)
                                rewriteBinding = (\b -> Optionals.maybe b (\ts ->  
                                        let vars = (Core.typeSchemeVariables ts) 
                                            typ = (Core.typeSchemeType ts)
                                            varsLen = (Lists.length vars)
                                            boundVarsLen = (Sets.size boundVars)
                                            normalVariables = (Lists.map (\i -> Core.Name (Strings.cat2 "t" (Literals.showInt32 i))) (Math.range 0 (Math.add varsLen boundVarsLen)))
                                            newVars = (Lists.take (Lists.length vars) (Lists.filter (\n -> Logic.not (Sets.member n boundVars)) normalVariables))
                                            newSubst = (Maps.union (Maps.fromList (Lists.zip vars newVars)) subst)
                                            newValue = (rewriteWithSubst (newSubst, (Sets.union boundVars (Sets.fromList newVars))) (Core.bindingTerm b))
                                        in Core.Binding {
                                          Core.bindingName = (Core.bindingName b),
                                          Core.bindingTerm = newValue,
                                          Core.bindingType = (Just (Core.TypeScheme {
                                            Core.typeSchemeVariables = newVars,
                                            Core.typeSchemeType = (substType newSubst typ)}))}) (Core.bindingType b))
                            in (Core.TermLet (Core.Let {
                              Core.letBindings = (Lists.map rewriteBinding bindings),
                              Core.letEnvironment = (rewriteWithSubst (subst, boundVars) env)}))
                          Core.TermTypeApplication v1 -> (Core.TermTypeApplication (Core.TypedTerm {
                            Core.typedTermTerm = (rewriteWithSubst (subst, boundVars) (Core.typedTermTerm v1)),
                            Core.typedTermType = (substType subst (Core.typedTermType v1))}))
                          Core.TermTypeLambda v1 -> (Core.TermTypeLambda (Core.TypeLambda {
                            Core.typeLambdaParameter = (replaceName subst (Core.typeLambdaParameter v1)),
                            Core.typeLambdaBody = (rewriteWithSubst (subst, boundVars) (Core.typeLambdaBody v1))}))
                          _ -> (recurse term)) term)
              in (rewriteTerm rewrite))
  in (rewriteWithSubst (Maps.empty, Sets.empty) term)

-- | Recursively remove term annotations, including within subterms
removeTermAnnotations :: (Core.Term -> Core.Term)
removeTermAnnotations term =  
  let remove = (\recurse -> \term ->  
          let rewritten = (recurse term)
          in ((\x -> case x of
            Core.TermAnnotated v1 -> (Core.annotatedTermSubject v1)
            _ -> rewritten) term))
  in (rewriteTerm remove term)

-- | Recursively remove type annotations, including within subtypes
removeTypeAnnotations :: (Core.Type -> Core.Type)
removeTypeAnnotations typ =  
  let remove = (\recurse -> \typ ->  
          let rewritten = (recurse typ)
          in ((\x -> case x of
            Core.TypeAnnotated v1 -> (Core.annotatedTypeSubject v1)
            _ -> rewritten) rewritten))
  in (rewriteType remove typ)

-- | Strip type annotations from terms while preserving other annotations
removeTypesFromTerm :: (Core.Term -> Core.Term)
removeTypesFromTerm term =  
  let strip = (\recurse -> \term ->  
          let rewritten = (recurse term) 
              stripBinding = (\b -> Core.Binding {
                      Core.bindingName = (Core.bindingName b),
                      Core.bindingTerm = (Core.bindingTerm b),
                      Core.bindingType = Nothing})
          in ((\x -> case x of
            Core.TermFunction v1 -> ((\x -> case x of
              Core.FunctionElimination v2 -> ((\x -> case x of
                Core.EliminationProduct v3 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                  Core.tupleProjectionArity = (Core.tupleProjectionIndex v3),
                  Core.tupleProjectionIndex = (Core.tupleProjectionArity v3),
                  Core.tupleProjectionDomain = Nothing}))))
                _ -> (Core.TermFunction (Core.FunctionElimination v2))) v2)
              Core.FunctionLambda v2 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.lambdaParameter v2),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.lambdaBody v2)})))
              _ -> (Core.TermFunction v1)) v1)
            Core.TermLet v1 -> (Core.TermLet (Core.Let {
              Core.letBindings = (Lists.map stripBinding (Core.letBindings v1)),
              Core.letEnvironment = (Core.letEnvironment v1)}))
            Core.TermTypeApplication v1 -> (Core.typedTermTerm v1)
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

rewrite :: ((t0 -> t1) -> (t1 -> t0) -> t0)
rewrite fsub f =  
  let recurse = (f (fsub recurse))
  in recurse

rewriteAndFoldTerm :: (((t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term))
rewriteAndFoldTerm f =  
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
                                        let rmd = (Optionals.map (recurse val) (Core.caseStatementDefault v1))
                                        in  
                                          let val1 = (Optionals.maybe val (\r -> fst r) rmd)
                                          in  
                                            let rcases = (forFields val1 (Core.caseStatementCases v1))
                                            in (fst rcases, (Core.EliminationUnion (Core.CaseStatement {
                                              Core.caseStatementTypeName = (Core.caseStatementTypeName v1),
                                              Core.caseStatementDefault = (Optionals.map snd rmd),
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
                              Core.annotatedTermSubject = t,
                              Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)})) val0 (Core.annotatedTermSubject v1))
                            Core.TermApplication v1 ->  
                              let rlhs = (recurse val0 (Core.applicationFunction v1))
                              in  
                                let rrhs = (recurse (fst rlhs) (Core.applicationArgument v1))
                                in (fst rrhs, (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (snd rlhs),
                                  Core.applicationArgument = (snd rrhs)})))
                            Core.TermFunction v1 -> (forSingle forFunction (\f -> Core.TermFunction f) val0 v1)
                            Core.TermLet v1 ->  
                              let renv = (recurse val0 (Core.letEnvironment v1))
                              in (forMany forBinding (\bins -> Core.TermLet (Core.Let {
                                Core.letBindings = bins,
                                Core.letEnvironment = (snd renv)})) (fst renv) (Core.letBindings v1))
                            Core.TermList v1 -> (forMany recurse (\x -> Core.TermList x) val0 v1)
                            Core.TermMap v1 -> (forMany forPair (\pairs -> Core.TermMap (Maps.fromList pairs)) val0 (Maps.toList v1))
                            Core.TermOptional v1 -> (Optionals.maybe dflt (\t -> forSingle recurse (\t1 -> Core.TermOptional (Just t1)) val0 t) v1)
                            Core.TermProduct v1 -> (forMany recurse (\x -> Core.TermProduct x) val0 v1)
                            Core.TermRecord v1 -> (forMany forField (\fields -> Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.recordTypeName v1),
                              Core.recordFields = fields})) val0 (Core.recordFields v1))
                            Core.TermSet v1 -> (forMany recurse (\e -> Core.TermSet (Sets.fromList e)) val0 (Sets.toList v1))
                            Core.TermSum v1 -> (forSingle recurse (\t -> Core.TermSum (Core.Sum {
                              Core.sumIndex = (Core.sumIndex v1),
                              Core.sumSize = (Core.sumSize v1),
                              Core.sumTerm = t})) val0 (Core.sumTerm v1))
                            Core.TermTypeApplication v1 -> (forSingle recurse (\t -> Core.TermTypeApplication (Core.TypedTerm {
                              Core.typedTermTerm = t,
                              Core.typedTermType = (Core.typedTermType v1)})) val0 (Core.typedTermTerm v1))
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
                              Core.wrappedTermObject = t})) val0 (Core.wrappedTermObject v1))
                            _ -> dflt) term0))
  in (rewrite fsub f)

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
                      let mapBinding = (\b -> Core.Binding {
                              Core.bindingName = (Core.bindingName b),
                              Core.bindingTerm = (recurse (Core.bindingTerm b)),
                              Core.bindingType = (Core.bindingType b)})
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
            Core.TermTypeApplication v1 -> (Core.TermTypeApplication (Core.TypedTerm {
              Core.typedTermTerm = (recurse (Core.typedTermTerm v1)),
              Core.typedTermType = (Core.typedTermType v1)}))
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
              Core.wrappedTermObject = (recurse (Core.wrappedTermObject v1))}))) term))
  in (rewrite fsub f)

rewriteTermM :: (((Core.Term -> Compute.Flow t0 Core.Term) -> Core.Term -> Compute.Flow t0 Core.Term) -> Core.Term -> Compute.Flow t0 Core.Term)
rewriteTermM f =  
  let fsub = (\recurse -> \term ->  
          let forField = (\field -> Flows.map (\t -> Core.Field {
                  Core.fieldName = (Core.fieldName field),
                  Core.fieldTerm = t}) (recurse (Core.fieldTerm field))) 
              forPair = (\kv ->  
                      let k = (fst kv) 
                          v = (snd kv)
                      in (Flows.bind (recurse k) (\km -> Flows.bind (recurse v) (\vm -> Flows.pure (km, vm)))))
              mapBinding = (\binding ->  
                      let k = (Core.bindingName binding) 
                          v = (Core.bindingTerm binding)
                          t = (Core.bindingType binding)
                      in (Flows.bind (recurse v) (\v_ -> Flows.pure (Core.Binding {
                        Core.bindingName = k,
                        Core.bindingTerm = v_,
                        Core.bindingType = t}))))
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
            Core.TermOptional v1 -> (Flows.bind (Flows.mapOptional recurse v1) (\rm -> Flows.pure (Core.TermOptional rm)))
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
            Core.TermTypeApplication v1 -> (Flows.bind (recurse (Core.typedTermTerm v1)) (\t -> Flows.pure (Core.TermTypeApplication (Core.TypedTerm {
              Core.typedTermTerm = t,
              Core.typedTermType = (Core.typedTermType v1)}))))
            Core.TermTypeLambda v1 ->  
              let v = (Core.typeLambdaParameter v1) 
                  body = (Core.typeLambdaBody v1)
              in (Flows.bind (recurse body) (\rbody -> Flows.pure (Core.TermTypeLambda (Core.TypeLambda {
                Core.typeLambdaParameter = v,
                Core.typeLambdaBody = rbody}))))
            Core.TermUnion v1 ->  
              let n = (Core.injectionTypeName v1) 
                  field = (Core.injectionField v1)
              in (Flows.map (\rfield -> Core.TermUnion (Core.Injection {
                Core.injectionTypeName = n,
                Core.injectionField = rfield})) (forField field))
            Core.TermUnit -> (Flows.pure Core.TermUnit)
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
          let forField = (\field -> Core.FieldType {
                  Core.fieldTypeName = (Core.fieldTypeName field),
                  Core.fieldTypeType = (recurse (Core.fieldTypeType field))})
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
            Core.TypeUnit -> Core.TypeUnit
            Core.TypeVariable v1 -> (Core.TypeVariable v1)
            Core.TypeWrap v1 -> (Core.TypeWrap (Core.WrappedType {
              Core.wrappedTypeTypeName = (Core.wrappedTypeTypeName v1),
              Core.wrappedTypeObject = (recurse (Core.wrappedTypeObject v1))}))) typ))
  in (rewrite fsub f)

rewriteTypeM :: (((Core.Type -> Compute.Flow t0 Core.Type) -> Core.Type -> Compute.Flow t0 Core.Type) -> Core.Type -> Compute.Flow t0 Core.Type)
rewriteTypeM =  
  let fsub = (\recurse -> \typ -> (\x -> case x of
          Core.TypeAnnotated v1 -> (Flows.bind (recurse (Core.annotatedTypeSubject v1)) (\t -> Flows.pure (Core.TypeAnnotated (Core.AnnotatedType {
            Core.annotatedTypeSubject = t,
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
          Core.TypeOptional v1 -> (Flows.bind (recurse v1) (\rt -> Flows.pure (Core.TypeOptional rt)))
          Core.TypeProduct v1 -> (Flows.bind (Flows.mapList recurse v1) (\rtypes -> Flows.pure (Core.TypeProduct rtypes)))
          Core.TypeRecord v1 ->  
            let name = (Core.rowTypeTypeName v1) 
                fields = (Core.rowTypeFields v1)
                forField = (\f -> Flows.bind (recurse (Core.fieldTypeType f)) (\t -> Flows.pure (Core.FieldType {
                        Core.fieldTypeName = (Core.fieldTypeName f),
                        Core.fieldTypeType = t})))
            in (Flows.bind (Flows.mapList forField fields) (\rfields -> Flows.pure (Core.TypeRecord (Core.RowType {
              Core.rowTypeTypeName = name,
              Core.rowTypeFields = rfields}))))
          Core.TypeSet v1 -> (Flows.bind (recurse v1) (\rt -> Flows.pure (Core.TypeSet rt)))
          Core.TypeSum v1 -> (Flows.bind (Flows.mapList recurse v1) (\rtypes -> Flows.pure (Core.TypeSum rtypes)))
          Core.TypeUnion v1 ->  
            let name = (Core.rowTypeTypeName v1) 
                fields = (Core.rowTypeFields v1)
                forField = (\f -> Flows.bind (recurse (Core.fieldTypeType f)) (\t -> Flows.pure (Core.FieldType {
                        Core.fieldTypeName = (Core.fieldTypeName f),
                        Core.fieldTypeType = t})))
            in (Flows.bind (Flows.mapList forField fields) (\rfields -> Flows.pure (Core.TypeUnion (Core.RowType {
              Core.rowTypeTypeName = name,
              Core.rowTypeFields = rfields}))))
          Core.TypeUnit -> (Flows.pure Core.TypeUnit)
          Core.TypeVariable v1 -> (Flows.pure (Core.TypeVariable v1))
          Core.TypeWrap v1 -> (Flows.bind (recurse (Core.wrappedTypeObject v1)) (\t -> Flows.pure (Core.TypeWrap (Core.WrappedType {
            Core.wrappedTypeTypeName = (Core.wrappedTypeTypeName v1),
            Core.wrappedTypeObject = t}))))) typ)
  in (\f -> rewrite fsub f)

-- | Simplify terms by applying beta reduction where possible
simplifyTerm :: (Core.Term -> Core.Term)
simplifyTerm term =  
  let simplify = (\recurse -> \term ->  
          let stripped = (deannotateTerm term)
          in (recurse ((\x -> case x of
            Core.TermApplication v1 ->  
              let lhs = (Core.applicationFunction v1) 
                  rhs = (Core.applicationArgument v1)
                  strippedLhs = (deannotateTerm lhs)
              in ((\x -> case x of
                Core.TermFunction v2 -> ((\x -> case x of
                  Core.FunctionLambda v3 ->  
                    let var = (Core.lambdaParameter v3) 
                        body = (Core.lambdaBody v3)
                    in (Logic.ifElse (Sets.member var (freeVariablesInTerm body)) ( 
                      let strippedRhs = (deannotateTerm rhs)
                      in ((\x -> case x of
                        Core.TermVariable v4 -> (simplifyTerm (substituteVariable var v4 body))
                        _ -> term) strippedRhs)) (simplifyTerm body))
                  _ -> term) v2)
                _ -> term) strippedLhs)
            _ -> term) stripped)))
  in (rewriteTerm simplify term)

-- | Substitute type variables in a type
substituteTypeVariables :: (M.Map Core.Name Core.Name -> Core.Type -> Core.Type)
substituteTypeVariables subst typ =  
  let replace = (\recurse -> \typ -> (\x -> case x of
          Core.TypeVariable v1 -> (Core.TypeVariable (Optionals.fromMaybe v1 (Maps.lookup v1 subst)))
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
          Core.TermVariable v1 -> (Core.TermVariable (Optionals.fromMaybe v1 (Maps.lookup v1 subst)))
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionLambda v2 -> (Optionals.maybe (recurse term) (\_ -> term) (Maps.lookup (Core.lambdaParameter v2) subst))
            _ -> (recurse term)) v1)
          _ -> (recurse term)) term)
  in (rewriteTerm replace term)

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
  Core.TermLet v1 -> (Lists.cons (Core.letEnvironment v1) (Lists.map Core.bindingTerm (Core.letBindings v1)))
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
  Core.TermTypeApplication v1 -> [
    Core.typedTermTerm v1]
  Core.TermTypeLambda v1 -> [
    Core.typeLambdaBody v1]
  Core.TermUnion v1 -> [
    Core.fieldTerm (Core.injectionField v1)]
  Core.TermUnit -> []
  Core.TermVariable _ -> []
  Core.TermWrap v1 -> [
    Core.wrappedTermObject v1]

-- | Find the children of a given term
subtermsWithAccessors :: (Core.Term -> [(Accessors.TermAccessor, Core.Term)])
subtermsWithAccessors x = case x of
  Core.TermAnnotated v1 -> [
    (Accessors.TermAccessorAnnotatedSubject, (Core.annotatedTermSubject v1))]
  Core.TermApplication v1 -> [
    (Accessors.TermAccessorApplicationFunction, (Core.applicationFunction v1)),
    (Accessors.TermAccessorApplicationArgument, (Core.applicationArgument v1))]
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationUnion v3 -> (Lists.concat2 (Optionals.maybe [] (\t -> [
        (Accessors.TermAccessorUnionCasesDefault, t)]) (Core.caseStatementDefault v3)) (Lists.map (\f -> (Accessors.TermAccessorUnionCasesBranch (Core.fieldName f), (Core.fieldTerm f))) (Core.caseStatementCases v3)))
      _ -> []) v2)
    Core.FunctionLambda v2 -> [
      (Accessors.TermAccessorLambdaBody, (Core.lambdaBody v2))]
    _ -> []) v1)
  Core.TermLet v1 -> (Lists.cons (Accessors.TermAccessorLetEnvironment, (Core.letEnvironment v1)) (Lists.map (\b -> (Accessors.TermAccessorLetBinding (Core.bindingName b), (Core.bindingTerm b))) (Core.letBindings v1)))
  Core.TermList v1 -> (Lists.map (\e -> (Accessors.TermAccessorListElement 0, e)) v1)
  Core.TermLiteral _ -> []
  Core.TermMap v1 -> (Lists.concat (Lists.map (\p -> [
    (Accessors.TermAccessorMapKey 0, (fst p)),
    (Accessors.TermAccessorMapValue 0, (snd p))]) (Maps.toList v1)))
  Core.TermOptional v1 -> (Optionals.maybe [] (\t -> [
    (Accessors.TermAccessorOptionalTerm, t)]) v1)
  Core.TermProduct v1 -> (Lists.map (\e -> (Accessors.TermAccessorProductTerm 0, e)) v1)
  Core.TermRecord v1 -> (Lists.map (\f -> (Accessors.TermAccessorRecordField (Core.fieldName f), (Core.fieldTerm f))) (Core.recordFields v1))
  Core.TermSet v1 -> (Lists.map (\e -> (Accessors.TermAccessorListElement 0, e)) (Sets.toList v1))
  Core.TermSum v1 -> [
    (Accessors.TermAccessorSumTerm, (Core.sumTerm v1))]
  Core.TermTypeApplication v1 -> [
    (Accessors.TermAccessorTypeApplicationTerm, (Core.typedTermTerm v1))]
  Core.TermTypeLambda v1 -> [
    (Accessors.TermAccessorTypeLambdaBody, (Core.typeLambdaBody v1))]
  Core.TermUnion v1 -> [
    (Accessors.TermAccessorInjectionTerm, (Core.fieldTerm (Core.injectionField v1)))]
  Core.TermUnit -> []
  Core.TermVariable _ -> []
  Core.TermWrap v1 -> [
    (Accessors.TermAccessorWrappedTerm, (Core.wrappedTermObject v1))]

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
  Core.TypeUnit -> []
  Core.TypeVariable _ -> []
  Core.TypeWrap v1 -> [
    Core.wrappedTypeObject v1]

-- | Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that
termDependencyNames :: (Bool -> Bool -> Bool -> Core.Term -> S.Set Core.Name)
termDependencyNames binds withPrims withNoms =  
  let addNames = (\names -> \term ->  
          let nominal = (\name -> Logic.ifElse withNoms (Sets.insert name names) names) 
              prim = (\name -> Logic.ifElse withPrims (Sets.insert name names) names)
              var = (\name -> Logic.ifElse binds (Sets.insert name names) names)
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

-- | Generate short names from a list of fully qualified names
toShortNames :: ([Core.Name] -> M.Map Core.Name Core.Name)
toShortNames original =  
  let groupNamesByLocal = (\names -> Lists.foldl addName Maps.empty names) 
      addName = (\acc -> \name ->  
              let local = (Names.localNameOf name) 
                  group = (Optionals.fromMaybe Sets.empty (Maps.lookup local acc))
              in (Maps.insert local (Sets.insert name group) acc))
      groups = (groupNamesByLocal original)
      renameGroup = (\localNames ->  
              let local = (fst localNames) 
                  names = (snd localNames)
                  rangeFrom = (\start -> Lists.cons start (rangeFrom (Math.add start 1)))
                  rename = (\name -> \i -> (name, (Core.Name (Logic.ifElse (Equality.gt i 1) (Strings.cat2 local (Literals.showInt32 i)) local))))
              in (Lists.zipWith rename (Sets.toList names) (rangeFrom 1)))
  in (Maps.fromList (Lists.concat (Lists.map renameGroup (Maps.toList groups))))

-- | Topological sort of connected components, in terms of dependencies between variable/term binding pairs
topologicalSortBindingMap :: (M.Map Core.Name Core.Term -> [[(Core.Name, Core.Term)]])
topologicalSortBindingMap bindingMap =  
  let bindings = (Maps.toList bindingMap) 
      keys = (Sets.fromList (Lists.map fst bindings))
      hasTypeAnnotation = (\term -> (\x -> case x of
              Core.TermAnnotated v1 -> (hasTypeAnnotation (Core.annotatedTermSubject v1))
              _ -> False) term)
      depsOf = (\nameAndTerm ->  
              let name = (fst nameAndTerm) 
                  term = (snd nameAndTerm)
              in (name, (Logic.ifElse (hasTypeAnnotation term) [] (Sets.toList (Sets.intersection keys (freeVariablesInTerm term))))))
      toPair = (\name -> (name, (Optionals.fromMaybe (Core.TermLiteral (Core.LiteralString "Impossible!")) (Maps.lookup name bindingMap))))
  in (Lists.map (Lists.map toPair) (Sorting.topologicalSortComponents (Lists.map depsOf bindings)))

-- | Topological sort of elements based on their dependencies
topologicalSortBindings :: ([Core.Binding] -> Mantle.Either [[Core.Name]] [Core.Name])
topologicalSortBindings els =  
  let adjlist = (\e -> (Core.bindingName e, (Sets.toList (termDependencyNames False True True (Core.bindingTerm e)))))
  in (Sorting.topologicalSort (Lists.map adjlist els))

typeDependencyNames :: (Bool -> Core.Type -> S.Set Core.Name)
typeDependencyNames withSchema typ = (Logic.ifElse withSchema (Sets.union (freeVariablesInType typ) (typeNamesInType typ)) (freeVariablesInType typ))

typeNamesInType :: (Core.Type -> S.Set Core.Name)
typeNamesInType = (foldOverType Coders.TraversalOrderPre addNames Sets.empty) 
  where 
    addNames = (\names -> \typ -> (\x -> case x of
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
