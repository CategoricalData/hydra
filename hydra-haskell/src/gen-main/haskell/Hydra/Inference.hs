-- | Type inference following Algorithm W, extended for nominal terms and types

module Hydra.Inference where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Formatting as Formatting
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
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Show.Mantle as Mantle_
import qualified Hydra.Show.Typing as Typing
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing_
import qualified Hydra.Unification as Unification
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

bindConstraints :: (Typing_.InferenceContext -> (Typing_.TypeSubst -> Compute.Flow t0 t1) -> [Typing_.TypeConstraint] -> Compute.Flow t0 t1)
bindConstraints cx f constraints = (Flows.bind (Unification.unifyTypeConstraints (Typing_.inferenceContextSchemaTypes cx) constraints) f)

checkSameType :: (String -> [Core.Type] -> Compute.Flow t0 Core.Type)
checkSameType desc types =  
  let h = (Lists.head types)
  in  
    let allEqual = (Lists.foldl (\b -> \t -> Logic.and b (Equality.equal t h)) True types)
    in (Logic.ifElse allEqual (Flows.pure h) (Flows.fail (Strings.cat [
      "unequal types ",
      Formatting.showList Core__.type_ types,
      " in ",
      desc])))

checkType :: (S.Set Core.Name -> Typing_.InferenceContext -> Core.Type -> Core.Term -> Compute.Flow t0 ())
checkType k g t e = (Logic.ifElse debugInference (Flows.bind (typeOfInternal g k (toFContext g) [] e) (\t0 -> Logic.ifElse (Equality.equal t0 t) (Flows.pure ()) (Flows.fail (Strings.cat [
  "type checking failed: expected ",
  Core__.type_ t,
  " but found ",
  (Core__.type_ t0)])))) (Flows.pure ()))

checkTypeVariables :: (Typing_.InferenceContext -> S.Set Core.Name -> Core.Type -> Compute.Flow t0 ())
checkTypeVariables cx tyvars typ = (Monads.withTrace (Strings.cat [
  "checking variables of: ",
  (Core__.type_ typ)]) ((\x -> case x of
  Core.TypeForall v1 -> (checkTypeVariables cx (Sets.insert (Core.forallTypeParameter v1) tyvars) (Core.forallTypeBody v1))
  Core.TypeVariable v1 -> (Logic.ifElse (Sets.member v1 tyvars) (Flows.pure ()) (Logic.ifElse (Maps.member v1 (Typing_.inferenceContextSchemaTypes cx)) (Flows.pure ()) (Flows.fail (Strings.cat [
    "unbound type variable \"",
    Core.unName v1,
    "\" in ",
    (Core__.type_ typ)]))))
  _ -> (Flows.bind (Flows.mapList (checkTypeVariables cx tyvars) (Rewriting.subtypes typ)) (\result -> Flows.pure ()))) typ))

-- | Disable type checking by default, for better performance
debugInference :: Bool
debugInference = True

-- | An empty inference context
emptyInferenceContext :: Typing_.InferenceContext
emptyInferenceContext = Typing_.InferenceContext {
  Typing_.inferenceContextSchemaTypes = (M.fromList []),
  Typing_.inferenceContextPrimitiveTypes = (M.fromList []),
  Typing_.inferenceContextDataTypes = (M.fromList []),
  Typing_.inferenceContextDebug = False}

-- | Add (term variable, type scheme) pairs to the typing environment
extendContext :: ([(Core.Name, Core.TypeScheme)] -> Typing_.InferenceContext -> Typing_.InferenceContext)
extendContext pairs cx = Typing_.InferenceContext {
  Typing_.inferenceContextSchemaTypes = (Typing_.inferenceContextSchemaTypes cx),
  Typing_.inferenceContextPrimitiveTypes = (Typing_.inferenceContextPrimitiveTypes cx),
  Typing_.inferenceContextDataTypes = (Maps.union (Maps.fromList pairs) (Typing_.inferenceContextDataTypes cx)),
  Typing_.inferenceContextDebug = (Typing_.inferenceContextDebug cx)}

forInferredTerm :: (Typing_.InferenceContext -> Core.Term -> String -> (Typing_.InferenceResult -> t0) -> Compute.Flow t1 t0)
forInferredTerm cx term desc f = (Flows.map f (inferTypeOfTerm cx term desc))

-- | Get all free variables in an inference context
freeVariablesInContext :: (Typing_.InferenceContext -> S.Set Core.Name)
freeVariablesInContext cx = (Lists.foldl Sets.union Sets.empty (Lists.map Rewriting.freeVariablesInTypeSchemeSimple (Maps.elems (Typing_.inferenceContextDataTypes cx))))

freshName :: (Compute.Flow t0 Core.Name)
freshName = (Flows.map normalTypeVariable (Annotations.nextCount key_vcount))

freshNames :: (Int -> Compute.Flow t0 [Core.Name])
freshNames n = (Flows.sequence (Lists.replicate n freshName))

freshVariableType :: (Compute.Flow t0 Core.Type)
freshVariableType = (Flows.map (\x -> Core.TypeVariable x) freshName)

-- | Generalize a type to a type scheme
generalize :: (Typing_.InferenceContext -> Core.Type -> Core.TypeScheme)
generalize cx typ =  
  let vars = (Lists.nub (Lists.filter (isUnbound cx) (Rewriting.freeVariablesInTypeOrdered typ)))
  in Core.TypeScheme {
    Core.typeSchemeVariables = vars,
    Core.typeSchemeType = typ}

graphToInferenceContext :: (Graph.Graph -> Compute.Flow t0 Typing_.InferenceContext)
graphToInferenceContext g0 =  
  let schema = (Optionals.fromMaybe g0 (Graph.graphSchema g0))
  in  
    let primTypes = (Maps.fromList (Lists.map (\p -> (Graph.primitiveName p, (Graph.primitiveType p))) (Maps.elems (Graph.graphPrimitives g0))))
    in  
      let varTypes = Maps.empty
      in (Flows.bind (Schemas.schemaGraphToTypingEnvironment schema) (\schemaTypes -> Flows.pure (Typing_.InferenceContext {
        Typing_.inferenceContextSchemaTypes = schemaTypes,
        Typing_.inferenceContextPrimitiveTypes = primTypes,
        Typing_.inferenceContextDataTypes = varTypes,
        Typing_.inferenceContextDebug = False})))

inferGraphTypes :: (Graph.Graph -> Compute.Flow t0 Graph.Graph)
inferGraphTypes g0 =  
  let fromLetTerm = (\l ->  
          let bindings = (Core.letBindings l)
          in  
            let env = (Core.letEnvironment l)
            in  
              let fromBinding = (\b -> (Core.bindingName b, b))
              in Graph.Graph {
                Graph.graphElements = (Maps.fromList (Lists.map fromBinding bindings)),
                Graph.graphEnvironment = Maps.empty,
                Graph.graphTypes = Maps.empty,
                Graph.graphBody = env,
                Graph.graphPrimitives = (Graph.graphPrimitives g0),
                Graph.graphSchema = (Graph.graphSchema g0)})
  in  
    let toLetTerm = (\g ->  
            let toBinding = (\el -> Core.Binding {
                    Core.bindingName = (Core.bindingName el),
                    Core.bindingTerm = (Core.bindingTerm el),
                    Core.bindingType = Nothing})
            in (Core.TermLet (Core.Let {
              Core.letBindings = (Lists.map toBinding (Maps.elems (Graph.graphElements g))),
              Core.letEnvironment = (Graph.graphBody g)})))
    in  
      let withResult = (\result ->  
              let term = (Typing_.inferenceResultTerm result)
              in  
                let ts = (Typing_.inferenceResultType result)
                in ((\x -> case x of
                  Core.TermLet v1 -> (Flows.pure (fromLetTerm v1))
                  Core.TermVariable _ -> (Flows.fail "Expected inferred graph as let term")) (Rewriting.normalizeTypeVariablesInTerm term)))
      in (Monads.withTrace "graph inference" (Flows.bind (graphToInferenceContext g0) (\cx -> Flows.bind (inferTypeOfTerm cx (toLetTerm g0) "graph term") withResult)))

-- | Infer the type of a term in graph context
inferInGraphContext :: (Core.Term -> Compute.Flow Graph.Graph Typing_.InferenceResult)
inferInGraphContext term = (Flows.bind Monads.getState (\g -> Flows.bind (graphToInferenceContext g) (\cx -> inferTypeOfTerm cx term "single term")))

inferMany :: (Typing_.InferenceContext -> [(Core.Term, String)] -> Compute.Flow t0 ([Core.Term], ([Core.Type], Typing_.TypeSubst)))
inferMany cx pairs = (Logic.ifElse (Lists.null pairs) (Flows.pure ([], ([], Substitution.idTypeSubst))) ( 
  let e = (fst (Lists.head pairs))
  in  
    let desc = (snd (Lists.head pairs))
    in  
      let tl = (Lists.tail pairs)
      in (Flows.bind (inferTypeOfTerm cx e desc) (\result1 ->  
        let e1 = (Typing_.inferenceResultTerm result1)
        in  
          let t1 = (Typing_.inferenceResultType result1)
          in  
            let s1 = (Typing_.inferenceResultSubst result1)
            in (Flows.bind (inferMany (Substitution.substInContext s1 cx) tl) (\result2 ->  
              let e2 = (fst result2)
              in  
                let t2 = (fst (snd result2))
                in  
                  let s2 = (snd (snd result2))
                  in (Flows.pure (Lists.cons (Substitution.substTypesInTerm s2 e1) e2, (Lists.cons (Substitution.substInType s2 t1) t2, (Substitution.composeTypeSubst s1 s2))))))))))

inferTypeOfAnnotatedTerm :: (Typing_.InferenceContext -> Core.AnnotatedTerm -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfAnnotatedTerm cx at =  
  let term = (Core.annotatedTermSubject at)
  in  
    let ann = (Core.annotatedTermAnnotation at)
    in (Flows.map (\result ->  
      let iterm = (Typing_.inferenceResultTerm result)
      in  
        let itype = (Typing_.inferenceResultType result)
        in  
          let isubst = (Typing_.inferenceResultSubst result)
          in Typing_.InferenceResult {
            Typing_.inferenceResultTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermSubject = iterm,
              Core.annotatedTermAnnotation = ann})),
            Typing_.inferenceResultType = itype,
            Typing_.inferenceResultSubst = isubst}) (inferTypeOfTerm cx term "annotated term"))

inferTypeOfApplication :: (Typing_.InferenceContext -> Core.Application -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfApplication cx app =  
  let e0 = (Core.applicationFunction app)
  in  
    let e1 = (Core.applicationArgument app)
    in (Flows.bind (inferTypeOfTerm cx e0 "lhs") (\lhsResult ->  
      let a = (Typing_.inferenceResultTerm lhsResult)
      in  
        let t0 = (Typing_.inferenceResultType lhsResult)
        in  
          let s0 = (Typing_.inferenceResultSubst lhsResult)
          in (Flows.bind (inferTypeOfTerm (Substitution.substInContext s0 cx) e1 "rhs") (\rhsResult ->  
            let b = (Typing_.inferenceResultTerm rhsResult)
            in  
              let t1 = (Typing_.inferenceResultType rhsResult)
              in  
                let s1 = (Typing_.inferenceResultSubst rhsResult)
                in (Flows.bind freshName (\v -> Flows.bind (Unification.unifyTypes (Typing_.inferenceContextSchemaTypes cx) (Substitution.substInType s1 t0) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = t1,
                  Core.functionTypeCodomain = (Core.TypeVariable v)})) "application lhs") (\s2 ->  
                  let rExpr = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Substitution.substTypesInTerm (Substitution.composeTypeSubst s1 s2) a),
                          Core.applicationArgument = (Substitution.substTypesInTerm s2 b)}))
                  in  
                    let rType = (Substitution.substInType s2 (Core.TypeVariable v))
                    in  
                      let rSubst = (Substitution.composeTypeSubstList [
                              s0,
                              s1,
                              s2])
                      in (Flows.pure (Typing_.InferenceResult {
                        Typing_.inferenceResultTerm = rExpr,
                        Typing_.inferenceResultType = rType,
                        Typing_.inferenceResultSubst = rSubst})))))))))

inferTypeOfCaseStatement :: (Typing_.InferenceContext -> Core.CaseStatement -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfCaseStatement cx caseStmt =  
  let tname = (Core.caseStatementTypeName caseStmt)
  in  
    let dflt = (Core.caseStatementDefault caseStmt)
    in  
      let cases = (Core.caseStatementCases caseStmt)
      in  
        let fnames = (Lists.map Core.fieldName cases)
        in (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
          let svars = (Core.typeSchemeVariables schemaType)
          in  
            let stype = (Core.typeSchemeType schemaType)
            in (Flows.bind (Core_.unionType tname stype) (\sfields -> Flows.bind (Flows.traverseOptional (\t -> inferTypeOfTerm cx t (Strings.cat [
              "case ",
              Core.unName tname,
              ".<default>"])) dflt) (\dfltResult -> Flows.bind (inferMany cx (Lists.map (\f -> (Core.fieldTerm f, (Strings.cat [
              "case ",
              Core.unName tname,
              ".",
              (Core.unName (Core.fieldName f))]))) cases)) (\caseResults ->  
              let iterms = (fst caseResults)
              in  
                let itypes = (fst (snd caseResults))
                in  
                  let isubst = (snd (snd caseResults))
                  in (Flows.bind freshName (\codv ->  
                    let cod = (Core.TypeVariable codv)
                    in  
                      let caseMap = (Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) sfields))
                      in  
                        let dfltConstraints = (Monads.optionalToList (Optionals.map (\r -> Typing_.TypeConstraint {
                                Typing_.typeConstraintLeft = cod,
                                Typing_.typeConstraintRight = (Typing_.inferenceResultType r),
                                Typing_.typeConstraintComment = "match default"}) dfltResult))
                        in  
                          let caseConstraints = (Optionals.cat (Lists.zipWith (\fname -> \itype -> Optionals.map (\ftype -> Typing_.TypeConstraint {
                                  Typing_.typeConstraintLeft = itype,
                                  Typing_.typeConstraintRight = (Core.TypeFunction (Core.FunctionType {
                                    Core.functionTypeDomain = ftype,
                                    Core.functionTypeCodomain = cod})),
                                  Typing_.typeConstraintComment = "case type"}) (Maps.lookup fname caseMap)) fnames itypes))
                          in (mapConstraints cx (\subst -> yield (Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypedTerm {
                            Core.typedTermTerm = t,
                            Core.typedTermType = (Core.TypeVariable v)})) (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = tname,
                            Core.caseStatementDefault = (Optionals.map Typing_.inferenceResultTerm dfltResult),
                            Core.caseStatementCases = (Lists.zipWith (\n -> \t -> Core.Field {
                              Core.fieldName = n,
                              Core.fieldTerm = t}) fnames iterms)})))) svars) (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)),
                            Core.functionTypeCodomain = cod})) (Substitution.composeTypeSubstList (Lists.concat [
                            Monads.optionalToList (Optionals.map Typing_.inferenceResultSubst dfltResult),
                            [
                              isubst,
                              subst]]))) (Lists.concat [
                            dfltConstraints,
                            caseConstraints]))))))))))

inferTypeOfCollection :: (Typing_.InferenceContext -> (Core.Type -> Core.Type) -> ([Core.Term] -> Core.Term) -> String -> [Core.Term] -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfCollection cx typCons trmCons desc els = (Flows.bind freshName (\var -> Logic.ifElse (Lists.null els) (Flows.pure (yield (Core.TermTypeApplication (Core.TypedTerm {
  Core.typedTermTerm = (trmCons []),
  Core.typedTermType = (Core.TypeVariable var)})) (typCons (Core.TypeVariable var)) Substitution.idTypeSubst)) (Flows.bind (inferMany cx (Lists.zip els (Lists.map (\i -> Strings.cat [
  "#",
  (Literals.showInt32 i)]) (Math.range 1 (Math.add (Lists.length els) 1))))) (\results ->  
  let terms = (fst results)
  in  
    let types = (fst (snd results))
    in  
      let subst1 = (snd (snd results))
      in  
        let constraints = (Lists.map (\t -> Typing_.TypeConstraint {
                Typing_.typeConstraintLeft = (Core.TypeVariable var),
                Typing_.typeConstraintRight = t,
                Typing_.typeConstraintComment = desc}) types)
        in (mapConstraints cx (\subst2 ->  
          let iterm = (trmCons terms)
          in  
            let itype = (typCons (Core.TypeVariable var))
            in  
              let isubst = (Substitution.composeTypeSubst subst1 subst2)
              in (yield iterm itype isubst)) constraints)))))

inferTypeOf :: (Typing_.InferenceContext -> Core.Term -> Compute.Flow t0 (Core.Term, Core.TypeScheme))
inferTypeOf cx term =  
  let letTerm = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "ignoredVariableName"),
              Core.bindingTerm = term,
              Core.bindingType = Nothing}],
          Core.letEnvironment = (Core.TermLiteral (Core.LiteralString "ignoredEnvironment"))}))
  in  
    let unifyAndSubst = (\result ->  
            let subst = (Typing_.inferenceResultSubst result)
            in (Flows.bind (Lexical.withEmptyGraph (Core_.letTerm (Rewriting.normalizeTypeVariablesInTerm (Typing_.inferenceResultTerm result)))) (\letResult ->  
              let bindings = (Core.letBindings letResult)
              in (Logic.ifElse (Equality.equal 1 (Lists.length bindings)) ( 
                let binding = (Lists.head bindings)
                in  
                  let term1 = (Core.bindingTerm binding)
                  in  
                    let mts = (Core.bindingType binding)
                    in (Optionals.maybe (Flows.fail "Expected a type scheme") (\ts -> Flows.pure (term1, ts)) mts)) (Flows.fail (Strings.cat [
                "Expected a single binding with a type scheme, but got: ",
                Literals.showInt32 (Lists.length bindings),
                " bindings"]))))))
    in (Flows.bind (inferTypeOfTerm cx letTerm "infer type of term") (\result -> unifyAndSubst result))

inferTypeOfElimination :: (Typing_.InferenceContext -> Core.Elimination -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfElimination cx elm = ((\x -> case x of
  Core.EliminationProduct v1 -> (inferTypeOfTupleProjection cx v1)
  Core.EliminationRecord v1 -> (inferTypeOfProjection cx v1)
  Core.EliminationUnion v1 -> (inferTypeOfCaseStatement cx v1)
  Core.EliminationWrap v1 -> (inferTypeOfUnwrap cx v1)) elm)

inferTypeOfFunction :: (Typing_.InferenceContext -> Core.Function -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfFunction cx f = ((\x -> case x of
  Core.FunctionElimination v1 -> (inferTypeOfElimination cx v1)
  Core.FunctionLambda v1 -> (inferTypeOfLambda cx v1)
  Core.FunctionPrimitive v1 -> (inferTypeOfPrimitive cx v1)) f)

inferTypeOfInjection :: (Typing_.InferenceContext -> Core.Injection -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfInjection cx injection =  
  let tname = (Core.injectionTypeName injection)
  in  
    let field = (Core.injectionField injection)
    in  
      let fname = (Core.fieldName field)
      in  
        let term = (Core.fieldTerm field)
        in (Flows.bind (inferTypeOfTerm cx term "injected term") (\result -> Flows.bind (requireSchemaType cx tname) (\schemaType ->  
          let svars = (Core.typeSchemeVariables schemaType)
          in  
            let stype = (Core.typeSchemeType schemaType)
            in  
              let iterm = (Typing_.inferenceResultTerm result)
              in  
                let ityp = (Typing_.inferenceResultType result)
                in  
                  let isubst = (Typing_.inferenceResultSubst result)
                  in (Flows.bind (Core_.unionType tname stype) (\sfields -> Flows.bind (Schemas.findFieldType fname sfields) (\ftyp -> mapConstraints cx (\subst -> yield (Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypedTerm {
                    Core.typedTermTerm = t,
                    Core.typedTermType = (Core.TypeVariable v)})) (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = tname,
                    Core.injectionField = Core.Field {
                      Core.fieldName = fname,
                      Core.fieldTerm = iterm}})) svars) (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)) (Substitution.composeTypeSubst isubst subst)) [
                    Typing_.TypeConstraint {
                      Typing_.typeConstraintLeft = ftyp,
                      Typing_.typeConstraintRight = ityp,
                      Typing_.typeConstraintComment = "schema type of injected field"}]))))))

inferTypeOfLambda :: (Typing_.InferenceContext -> Core.Lambda -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfLambda cx lambda =  
  let var = (Core.lambdaParameter lambda)
  in  
    let body = (Core.lambdaBody lambda)
    in (Flows.bind freshName (\vdom ->  
      let dom = (Core.TypeVariable vdom)
      in  
        let cx2 = (extendContext [
                (var, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = dom})] cx)
        in (Flows.bind (inferTypeOfTerm cx2 body "lambda body") (\result ->  
          let iterm = (Typing_.inferenceResultTerm result)
          in  
            let icod = (Typing_.inferenceResultType result)
            in  
              let isubst = (Typing_.inferenceResultSubst result)
              in  
                let rdom = (Substitution.substInType isubst dom)
                in  
                  let rterm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = var,
                          Core.lambdaDomain = (Just rdom),
                          Core.lambdaBody = iterm})))
                  in  
                    let rtype = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = rdom,
                            Core.functionTypeCodomain = icod}))
                    in  
                      let vars = (Sets.unions [
                              Rewriting.freeVariablesInType rdom,
                              Rewriting.freeVariablesInType icod,
                              (freeVariablesInContext (Substitution.substInContext isubst cx2))])
                      in  
                        let cx3 = (Substitution.substInContext isubst cx)
                        in (Flows.pure (Typing_.InferenceResult {
                          Typing_.inferenceResultTerm = rterm,
                          Typing_.inferenceResultType = rtype,
                          Typing_.inferenceResultSubst = isubst}))))))

inferTypeOfLetNormalized :: (Typing_.InferenceContext -> Core.Let -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfLetNormalized cx0 letTerm =  
  let bins0 = (Core.letBindings letTerm)
  in  
    let env0 = (Core.letEnvironment letTerm)
    in  
      let bnames = (Lists.map Core.bindingName bins0)
      in (Flows.bind (freshNames (Lists.length bins0)) (\bvars ->  
        let tbins0 = (Lists.map (\x -> Core.TypeVariable x) bvars)
        in  
          let cx1 = (extendContext (Lists.zip bnames (Lists.map (\t -> Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = t}) tbins0)) cx0)
          in (Flows.bind (inferTypesOfTemporaryBindings cx1 bins0) (\inferredResult ->  
            let bterms1 = (fst inferredResult)
            in  
              let tbins1 = (fst (snd inferredResult))
              in  
                let s1 = (snd (snd inferredResult))
                in (Flows.bind (Unification.unifyTypeLists (Typing_.inferenceContextSchemaTypes cx0) (Lists.map (Substitution.substInType s1) tbins0) tbins1 "temporary type bindings") (\s2 ->  
                  let g2 = (Substitution.substInContext (Substitution.composeTypeSubst s1 s2) cx0)
                  in  
                    let tsbins1 = (Lists.zip bnames (Lists.map (\t -> generalize g2 (Substitution.substInType s2 t)) tbins1))
                    in (Flows.bind (inferTypeOfTerm (extendContext tsbins1 g2) env0 "let environment") (\envResult ->  
                      let env1 = (Typing_.inferenceResultTerm envResult)
                      in  
                        let tenv = (Typing_.inferenceResultType envResult)
                        in  
                          let senv = (Typing_.inferenceResultSubst envResult)
                          in  
                            let st1 = (Typing_.TermSubst (Maps.fromList (Lists.map (\pair ->  
                                    let name = (fst pair)
                                    in  
                                      let ts = (snd pair)
                                      in (name, (Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypedTerm {
                                        Core.typedTermTerm = t,
                                        Core.typedTermType = (Core.TypeVariable v)})) (Core.TermVariable name) (Lists.reverse (Core.typeSchemeVariables ts))))) tsbins1)))
                            in  
                              let createBinding = (\bindingPair ->  
                                      let nameTsPair = (fst bindingPair)
                                      in  
                                        let term = (snd bindingPair)
                                        in  
                                          let name = (fst nameTsPair)
                                          in  
                                            let ts = (snd nameTsPair)
                                            in  
                                              let typeAbstractedTerm = (Lists.foldl (\b -> \v -> Core.TermTypeLambda (Core.TypeLambda {
                                                      Core.typeLambdaParameter = v,
                                                      Core.typeLambdaBody = b})) (Substitution.substituteInTerm st1 term) (Lists.reverse (Core.typeSchemeVariables ts)))
                                              in Core.Binding {
                                                Core.bindingName = name,
                                                Core.bindingTerm = (Substitution.substTypesInTerm (Substitution.composeTypeSubst senv s2) typeAbstractedTerm),
                                                Core.bindingType = (Just (Substitution.substInTypeScheme senv ts))})
                              in  
                                let bins1 = (Lists.map createBinding (Lists.zip tsbins1 bterms1))
                                in  
                                  let ret = Typing_.InferenceResult {
                                          Typing_.inferenceResultTerm = (Core.TermLet (Core.Let {
                                            Core.letBindings = bins1,
                                            Core.letEnvironment = env1})),
                                          Typing_.inferenceResultType = tenv,
                                          Typing_.inferenceResultSubst = (Substitution.composeTypeSubstList [
                                            s1,
                                            s2,
                                            senv])}
                                  in (Flows.pure ret)))))))))

inferTypeOfLet :: (Typing_.InferenceContext -> Core.Let -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfLet cx let0 =  
  let bindings0 = (Core.letBindings let0)
  in  
    let env0 = (Core.letEnvironment let0)
    in  
      let names = (Lists.map Core.bindingName bindings0)
      in  
        let nameSet = (Sets.fromList names)
        in  
          let toPair = (\binding ->  
                  let name = (Core.bindingName binding)
                  in  
                    let term = (Core.bindingTerm binding)
                    in (name, (Lists.filter (\n -> Sets.member n nameSet) (Sets.toList (Rewriting.freeVariablesInTerm term)))))
          in  
            let adjList = (Lists.map toPair bindings0)
            in  
              let groups = (Sorting.topologicalSortComponents adjList)
              in  
                let bindingMap = (Maps.fromList (Lists.zip names bindings0))
                in  
                  let createLet = (\e -> \group -> Core.TermLet (Core.Let {
                          Core.letBindings = (Optionals.cat (Lists.map (\n -> Maps.lookup n bindingMap) group)),
                          Core.letEnvironment = e}))
                  in  
                    let rewrittenLet = (Lists.foldl createLet env0 (Lists.reverse groups))
                    in  
                      let restoreLet = (\iterm ->  
                              let helper = (\level -> \bins -> \term -> Logic.ifElse (Equality.equal level 0) (bins, term) ((\x -> case x of
                                      Core.TermLet v1 ->  
                                        let bs = (Core.letBindings v1)
                                        in  
                                          let e = (Core.letEnvironment v1)
                                          in (helper (Math.sub level 1) (Lists.concat [
                                            bs,
                                            bins]) e)) term))
                              in  
                                let result = (helper (Lists.length groups) [] iterm)
                                in  
                                  let bindingList = (fst result)
                                  in  
                                    let e = (snd result)
                                    in  
                                      let bindingMap2 = (Maps.fromList (Lists.map (\b -> (Core.bindingName b, b)) bindingList))
                                      in (Core.TermLet (Core.Let {
                                        Core.letBindings = (Optionals.cat (Lists.map (\n -> Maps.lookup n bindingMap2) names)),
                                        Core.letEnvironment = e})))
                      in  
                        let rewriteResult = (\result ->  
                                let iterm = (Typing_.inferenceResultTerm result)
                                in  
                                  let itype = (Typing_.inferenceResultType result)
                                  in  
                                    let isubst = (Typing_.inferenceResultSubst result)
                                    in Typing_.InferenceResult {
                                      Typing_.inferenceResultTerm = (restoreLet iterm),
                                      Typing_.inferenceResultType = itype,
                                      Typing_.inferenceResultSubst = isubst})
                        in (Flows.map rewriteResult ((\x -> case x of
                          Core.TermLet v1 -> (inferTypeOfLetNormalized cx v1)
                          _ -> (inferTypeOfTerm cx rewrittenLet "empty let term")) rewrittenLet))

inferTypeOfList :: (Typing_.InferenceContext -> [Core.Term] -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfList cx = (inferTypeOfCollection cx (\x -> Core.TypeList x) (\x -> Core.TermList x) "list element")

inferTypeOfLiteral :: (t0 -> Core.Literal -> Compute.Flow t1 Typing_.InferenceResult)
inferTypeOfLiteral _ lit = (Flows.pure (Typing_.InferenceResult {
  Typing_.inferenceResultTerm = (Core.TermLiteral lit),
  Typing_.inferenceResultType = (Core.TypeLiteral (Variants.literalType lit)),
  Typing_.inferenceResultSubst = Substitution.idTypeSubst}))

inferTypeOfMap :: (Typing_.InferenceContext -> M.Map Core.Term Core.Term -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfMap cx m = (Flows.bind freshName (\kvar -> Flows.bind freshName (\vvar -> Logic.ifElse (Maps.null m) (Flows.pure (yield (Core.TermTypeApplication (Core.TypedTerm {
  Core.typedTermTerm = (Core.TermTypeApplication (Core.TypedTerm {
    Core.typedTermTerm = (Core.TermMap Maps.empty),
    Core.typedTermType = (Core.TypeVariable vvar)})),
  Core.typedTermType = (Core.TypeVariable kvar)})) (Core.TypeMap (Core.MapType {
  Core.mapTypeKeys = (Core.TypeVariable kvar),
  Core.mapTypeValues = (Core.TypeVariable vvar)})) Substitution.idTypeSubst)) (Flows.bind (inferMany cx (Lists.map (\k -> (k, "map key")) (Maps.keys m))) (\kresults ->  
  let kterms = (fst kresults)
  in  
    let ktypes = (fst (snd kresults))
    in  
      let ksubst = (snd (snd kresults))
      in (Flows.bind (inferMany cx (Lists.map (\v -> (v, "map value")) (Maps.elems m))) (\vresults ->  
        let vterms = (fst vresults)
        in  
          let vtypes = (fst (snd vresults))
          in  
            let vsubst = (snd (snd vresults))
            in  
              let kcons = (Lists.map (\t -> Typing_.TypeConstraint {
                      Typing_.typeConstraintLeft = (Core.TypeVariable kvar),
                      Typing_.typeConstraintRight = t,
                      Typing_.typeConstraintComment = "map key"}) ktypes)
              in  
                let vcons = (Lists.map (\t -> Typing_.TypeConstraint {
                        Typing_.typeConstraintLeft = (Core.TypeVariable vvar),
                        Typing_.typeConstraintRight = t,
                        Typing_.typeConstraintComment = "map value"}) vtypes)
                in (mapConstraints cx (\subst -> yield (Core.TermMap (Maps.fromList (Lists.zip kterms vterms))) (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable kvar),
                  Core.mapTypeValues = (Core.TypeVariable vvar)})) (Substitution.composeTypeSubstList [
                  ksubst,
                  vsubst,
                  subst])) (Lists.concat [
                  kcons,
                  vcons])))))))))

inferTypeOfOptional :: (Typing_.InferenceContext -> Maybe Core.Term -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfOptional cx m =  
  let trmCons = (\terms -> Logic.ifElse (Lists.null terms) (Core.TermOptional Nothing) (Core.TermOptional (Just (Lists.head terms))))
  in (inferTypeOfCollection cx (\x -> Core.TypeOptional x) trmCons "optional element" (Optionals.maybe [] Lists.singleton m))

inferTypeOfPrimitive :: (Typing_.InferenceContext -> Core.Name -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfPrimitive cx name = (Optionals.maybe (Flows.fail (Strings.cat2 "No such primitive: " (Core.unName name))) (\scheme -> Flows.bind (instantiateTypeScheme scheme) (\ts ->  
  let vars = (Core.typeSchemeVariables ts)
  in  
    let itype = (Core.typeSchemeType ts)
    in  
      let iterm = (Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypedTerm {
              Core.typedTermTerm = t,
              Core.typedTermType = (Core.TypeVariable v)})) (Core.TermFunction (Core.FunctionPrimitive name)) vars)
      in (yieldChecked cx vars iterm itype Substitution.idTypeSubst))) (Maps.lookup name (Typing_.inferenceContextPrimitiveTypes cx)))

inferTypeOfProduct :: (Typing_.InferenceContext -> [Core.Term] -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfProduct cx els = (Flows.map (\results ->  
  let iterms = (fst results)
  in  
    let itypes = (fst (snd results))
    in  
      let isubst = (snd (snd results))
      in (yield (Core.TermProduct iterms) (Core.TypeProduct itypes) isubst)) (inferMany cx (Lists.map (\e -> (e, "tuple element")) els)))

inferTypeOfProjection :: (Typing_.InferenceContext -> Core.Projection -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfProjection cx proj =  
  let tname = (Core.projectionTypeName proj)
  in  
    let fname = (Core.projectionField proj)
    in (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
      let svars = (Core.typeSchemeVariables schemaType)
      in  
        let stype = (Core.typeSchemeType schemaType)
        in (Flows.bind (Core_.recordType tname stype) (\sfields -> Flows.bind (Schemas.findFieldType fname sfields) (\ftyp -> Flows.pure (yield (Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypedTerm {
          Core.typedTermTerm = t,
          Core.typedTermType = (Core.TypeVariable v)})) (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = tname,
          Core.projectionField = fname})))) svars) (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)),
          Core.functionTypeCodomain = ftyp})) Substitution.idTypeSubst))))))

inferTypeOfRecord :: (Typing_.InferenceContext -> Core.Record -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfRecord cx record =  
  let tname = (Core.recordTypeName record)
  in  
    let fields = (Core.recordFields record)
    in  
      let fnames = (Lists.map Core.fieldName fields)
      in (Flows.bind (requireSchemaType cx tname) (\schemaType -> Flows.bind (inferMany cx (Lists.map (\f -> (Core.fieldTerm f, (Strings.cat2 "field " (Core.unName (Core.fieldName f))))) fields)) (\results ->  
        let svars = (Core.typeSchemeVariables schemaType)
        in  
          let stype = (Core.typeSchemeType schemaType)
          in  
            let iterms = (fst results)
            in  
              let itypes = (fst (snd results))
              in  
                let isubst = (snd (snd results))
                in  
                  let ityp = (Core.TypeRecord (Core.RowType {
                          Core.rowTypeTypeName = tname,
                          Core.rowTypeFields = (Lists.zipWith (\n -> \t -> Core.FieldType {
                            Core.fieldTypeName = n,
                            Core.fieldTypeType = t}) fnames itypes)}))
                  in (mapConstraints cx (\subst -> yield (Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypedTerm {
                    Core.typedTermTerm = t,
                    Core.typedTermType = (Core.TypeVariable v)})) (Core.TermRecord (Core.Record {
                    Core.recordTypeName = tname,
                    Core.recordFields = (Lists.zipWith (\n -> \t -> Core.Field {
                      Core.fieldName = n,
                      Core.fieldTerm = t}) fnames iterms)})) svars) (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)) (Substitution.composeTypeSubst isubst subst)) [
                    Typing_.TypeConstraint {
                      Typing_.typeConstraintLeft = stype,
                      Typing_.typeConstraintRight = ityp,
                      Typing_.typeConstraintComment = "schema type of record"}]))))

inferTypeOfSet :: (Typing_.InferenceContext -> S.Set Core.Term -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfSet cx s = (inferTypeOfCollection cx (\x -> Core.TypeSet x) (\terms -> Core.TermSet (Sets.fromList terms)) "set element" (Sets.toList s))

inferTypeOfSum :: (Typing_.InferenceContext -> Core.Sum -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfSum cx sum =  
  let i = (Core.sumIndex sum)
  in  
    let s = (Core.sumSize sum)
    in  
      let term = (Core.sumTerm sum)
      in (Flows.bind (inferTypeOfTerm cx term "sum term") (\result ->  
        let iterm = (Typing_.inferenceResultTerm result)
        in  
          let ityp = (Typing_.inferenceResultType result)
          in  
            let isubst = (Typing_.inferenceResultSubst result)
            in  
              let varOrTerm = (\t -> \j -> Logic.ifElse (Equality.equal i j) (Flows.pure (Mantle.EitherLeft t)) (Flows.map (\x -> Mantle.EitherRight x) freshName))
              in (Flows.bind (Flows.mapList (varOrTerm ityp) (Math.range 0 (Math.sub s 1))) (\vars ->  
                let toType = (\e -> (\x -> case x of
                        Mantle.EitherLeft v1 -> v1
                        Mantle.EitherRight v1 -> (Core.TypeVariable v1)) e)
                in (Flows.pure (yield (Core.TermSum (Core.Sum {
                  Core.sumIndex = i,
                  Core.sumSize = s,
                  Core.sumTerm = iterm})) (Core.TypeSum (Lists.map toType vars)) isubst))))))

inferTypeOfTerm :: (Typing_.InferenceContext -> Core.Term -> String -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfTerm cx term desc = (Monads.withTrace desc ((\x -> case x of
  Core.TermAnnotated v1 -> (inferTypeOfAnnotatedTerm cx v1)
  Core.TermApplication v1 -> (inferTypeOfApplication cx v1)
  Core.TermFunction v1 -> (inferTypeOfFunction cx v1)
  Core.TermLet v1 -> (inferTypeOfLet cx v1)
  Core.TermList v1 -> (inferTypeOfList cx v1)
  Core.TermLiteral v1 -> (inferTypeOfLiteral cx v1)
  Core.TermMap v1 -> (inferTypeOfMap cx v1)
  Core.TermOptional v1 -> (inferTypeOfOptional cx v1)
  Core.TermProduct v1 -> (inferTypeOfProduct cx v1)
  Core.TermRecord v1 -> (inferTypeOfRecord cx v1)
  Core.TermSet v1 -> (inferTypeOfSet cx v1)
  Core.TermSum v1 -> (inferTypeOfSum cx v1)
  Core.TermTypeLambda v1 -> (inferTypeOfTypeLambda cx v1)
  Core.TermTypeApplication v1 -> (inferTypeOfTypeApplication cx v1)
  Core.TermUnion v1 -> (inferTypeOfInjection cx v1)
  Core.TermUnit -> (Flows.pure inferTypeOfUnit)
  Core.TermVariable v1 -> (inferTypeOfVariable cx v1)
  Core.TermWrap v1 -> (inferTypeOfWrappedTerm cx v1)) term))

inferTypeOfTupleProjection :: (t0 -> Core.TupleProjection -> Compute.Flow t1 Typing_.InferenceResult)
inferTypeOfTupleProjection cx tp =  
  let arity = (Core.tupleProjectionArity tp)
  in  
    let idx = (Core.tupleProjectionIndex tp)
    in (Flows.bind (freshNames arity) (\vars ->  
      let types = (Lists.map (\x -> Core.TypeVariable x) vars)
      in  
        let cod = (Lists.at idx types)
        in (Flows.pure (yield (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
          Core.tupleProjectionArity = arity,
          Core.tupleProjectionIndex = idx,
          Core.tupleProjectionDomain = (Just types)})))) (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.TypeProduct types),
          Core.functionTypeCodomain = cod})) Substitution.idTypeSubst))))

inferTypeOfTypeLambda :: (Typing_.InferenceContext -> Core.TypeLambda -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfTypeLambda cx ta = (inferTypeOfTerm cx (Core.typeLambdaBody ta) "type abstraction")

inferTypeOfTypeApplication :: (Typing_.InferenceContext -> Core.TypedTerm -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfTypeApplication cx tt = (inferTypeOfTerm cx (Core.typedTermTerm tt) "type application term")

inferTypeOfUnwrap :: (Typing_.InferenceContext -> Core.Name -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfUnwrap cx tname = (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
  let svars = (Core.typeSchemeVariables schemaType)
  in  
    let stype = (Core.typeSchemeType schemaType)
    in (Flows.bind (Core_.wrappedType tname stype) (\wtyp -> Flows.pure (yield (Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypedTerm {
      Core.typedTermTerm = t,
      Core.typedTermType = (Core.TypeVariable v)})) (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap tname))) svars) (Core.TypeFunction (Core.FunctionType {
      Core.functionTypeDomain = (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)),
      Core.functionTypeCodomain = wtyp})) Substitution.idTypeSubst)))))

inferTypeOfVariable :: (Typing_.InferenceContext -> Core.Name -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfVariable cx name = (Optionals.maybe (Flows.fail (Strings.cat2 "Variable not bound to type: " (Core.unName name))) (\scheme -> Flows.bind (instantiateTypeScheme scheme) (\ts ->  
  let vars = (Core.typeSchemeVariables ts)
  in  
    let itype = (Core.typeSchemeType ts)
    in  
      let iterm = (Lists.foldl (\t -> \ty -> Core.TermTypeApplication (Core.TypedTerm {
              Core.typedTermTerm = t,
              Core.typedTermType = ty})) (Core.TermVariable name) (Lists.map (\x -> Core.TypeVariable x) vars))
      in (Flows.pure (Typing_.InferenceResult {
        Typing_.inferenceResultTerm = iterm,
        Typing_.inferenceResultType = itype,
        Typing_.inferenceResultSubst = Substitution.idTypeSubst})))) (Maps.lookup name (Typing_.inferenceContextDataTypes cx)))

inferTypeOfWrappedTerm :: (Typing_.InferenceContext -> Core.WrappedTerm -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfWrappedTerm cx wt =  
  let tname = (Core.wrappedTermTypeName wt)
  in  
    let term = (Core.wrappedTermObject wt)
    in (Flows.bind (requireSchemaType cx tname) (\schemaType -> Flows.bind (inferTypeOfTerm cx term "wrapped term") (\result ->  
      let svars = (Core.typeSchemeVariables schemaType)
      in  
        let stype = (Core.typeSchemeType schemaType)
        in  
          let iterm = (Typing_.inferenceResultTerm result)
          in  
            let ityp = (Typing_.inferenceResultType result)
            in  
              let isubst = (Typing_.inferenceResultSubst result)
              in (Flows.bind (freshNames (Lists.length svars)) (\freshVars ->  
                let subst = (Typing_.TypeSubst (Maps.fromList (Lists.zip svars (Lists.map (\x -> Core.TypeVariable x) freshVars))))
                in  
                  let stypInst = (Substitution.substInType subst stype)
                  in  
                    let nominalInst = (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) freshVars))
                    in  
                      let expected = (Core.TypeWrap (Core.WrappedType {
                              Core.wrappedTypeTypeName = tname,
                              Core.wrappedTypeObject = ityp}))
                      in  
                        let freeVars = (Sets.toList (Sets.unions [
                                Rewriting.freeVariablesInType ityp,
                                Rewriting.freeVariablesInTerm iterm,
                                (Sets.fromList freshVars)]))
                        in (bindConstraints cx (\subst2 -> yieldChecked cx freeVars (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = tname,
                          Core.wrappedTermObject = iterm})) nominalInst (Substitution.composeTypeSubst isubst subst2)) [
                          Typing_.TypeConstraint {
                            Typing_.typeConstraintLeft = stypInst,
                            Typing_.typeConstraintRight = expected,
                            Typing_.typeConstraintComment = "schema type of wrapper"}]))))))

inferTypesOfTemporaryBindings :: (Typing_.InferenceContext -> [Core.Binding] -> Compute.Flow t0 ([Core.Term], ([Core.Type], Typing_.TypeSubst)))
inferTypesOfTemporaryBindings cx bins = (Logic.ifElse (Lists.null bins) (Flows.pure ([], ([], Substitution.idTypeSubst))) ( 
  let binding = (Lists.head bins)
  in  
    let k = (Core.bindingName binding)
    in  
      let v = (Core.bindingTerm binding)
      in  
        let tl = (Lists.tail bins)
        in (Flows.bind (inferTypeOfTerm cx v (Strings.cat [
          "temporary let binding '",
          Core.unName k,
          "'"])) (\result1 ->  
          let j = (Typing_.inferenceResultTerm result1)
          in  
            let u_prime = (Typing_.inferenceResultType result1)
            in  
              let u = (Typing_.inferenceResultSubst result1)
              in (Flows.bind (inferTypesOfTemporaryBindings (Substitution.substInContext u cx) tl) (\result2 ->  
                let h = (fst result2)
                in  
                  let r_prime = (fst (snd result2))
                  in  
                    let r = (snd (snd result2))
                    in (Flows.pure (Lists.cons (Substitution.substTypesInTerm r j) h, (Lists.cons (Substitution.substInType r u_prime) r_prime, (Substitution.composeTypeSubst u r))))))))))

initialTypeContext :: (Graph.Graph -> Compute.Flow t0 Typing_.TypeContext)
initialTypeContext g =  
  let toPair = (\pair ->  
          let name = (fst pair)
          in  
            let el = (snd pair)
            in (Optionals.maybe (Flows.fail (Strings.cat [
              "untyped element: ",
              (Core.unName name)])) (\ts -> Flows.pure (name, (Schemas.typeSchemeToFType ts))) (Core.bindingType el)))
  in (Flows.bind (graphToInferenceContext g) (\ix -> Flows.bind (Flows.map Maps.fromList (Flows.mapList toPair (Maps.toList (Graph.graphElements g)))) (\types -> Flows.pure (Typing_.TypeContext {
    Typing_.typeContextTypes = types,
    Typing_.typeContextVariables = Sets.empty,
    Typing_.typeContextInferenceContext = ix}))))

instantiateTypeScheme :: (Core.TypeScheme -> Compute.Flow t0 Core.TypeScheme)
instantiateTypeScheme scheme =  
  let oldVars = (Core.typeSchemeVariables scheme)
  in (Flows.bind (freshNames (Lists.length oldVars)) (\newVars ->  
    let subst = (Typing_.TypeSubst (Maps.fromList (Lists.zip oldVars (Lists.map (\x -> Core.TypeVariable x) newVars))))
    in (Flows.pure (Core.TypeScheme {
      Core.typeSchemeVariables = newVars,
      Core.typeSchemeType = (Substitution.substInType subst (Core.typeSchemeType scheme))}))))

-- | Check if a variable is unbound in context
isUnbound :: (Typing_.InferenceContext -> Core.Name -> Bool)
isUnbound cx v = (Logic.and (Logic.not (Sets.member v (freeVariablesInContext cx))) (Logic.not (Maps.member v (Typing_.inferenceContextSchemaTypes cx))))

-- | Key for inference type variable count
key_vcount :: Core.Name
key_vcount = (Core.Name "inferenceTypeVariableCount")

mapConstraints :: (Typing_.InferenceContext -> (Typing_.TypeSubst -> t0) -> [Typing_.TypeConstraint] -> Compute.Flow t1 t0)
mapConstraints cx f constraints = (Flows.map f (Unification.unifyTypeConstraints (Typing_.inferenceContextSchemaTypes cx) constraints))

-- | Apply type arguments to a nominal type
nominalApplication :: (Core.Name -> [Core.Type] -> Core.Type)
nominalApplication tname args = (Lists.foldl (\t -> \a -> Core.TypeApplication (Core.ApplicationType {
  Core.applicationTypeFunction = t,
  Core.applicationTypeArgument = a})) (Core.TypeVariable tname) args)

-- | Type variable naming convention follows Haskell: t0, t1, etc.
normalTypeVariable :: (Int -> Core.Name)
normalTypeVariable i = (Core.Name (Strings.cat2 "t" (Literals.showInt32 i)))

requireSchemaType :: (Typing_.InferenceContext -> Core.Name -> Compute.Flow t0 Core.TypeScheme)
requireSchemaType cx tname = (Optionals.maybe (Flows.fail (Strings.cat2 "No such schema type: " (Core.unName tname))) (\ts -> instantiateTypeScheme (Rewriting.deannotateTypeSchemeRecursive ts)) (Maps.lookup tname (Typing_.inferenceContextSchemaTypes cx)))

-- | Show an inference result for debugging
showInferenceResult :: (Typing_.InferenceResult -> String)
showInferenceResult result =  
  let term = (Typing_.inferenceResultTerm result)
  in  
    let typ = (Typing_.inferenceResultType result)
    in  
      let subst = (Typing_.inferenceResultSubst result)
      in (Strings.cat [
        "{term=",
        Core__.term term,
        ", type=",
        Core__.type_ typ,
        ", subst=",
        Typing.typeSubst subst,
        "}"])

-- | Convert inference context to type context
toFContext :: (Typing_.InferenceContext -> M.Map Core.Name Core.Type)
toFContext cx = (Maps.map Schemas.typeSchemeToFType (Typing_.inferenceContextDataTypes cx))

typeOf :: (Typing_.TypeContext -> Core.Term -> Compute.Flow t0 Core.Type)
typeOf tcontext term = (typeOfInternal (Typing_.typeContextInferenceContext tcontext) (Typing_.typeContextVariables tcontext) (Typing_.typeContextTypes tcontext) [] term)

typeOfInternal :: (Typing_.InferenceContext -> S.Set Core.Name -> M.Map Core.Name Core.Type -> [Core.Type] -> Core.Term -> Compute.Flow t0 Core.Type)
typeOfInternal cx vars types apptypes term =  
  let checkApplied = (\e -> Logic.ifElse (Lists.null apptypes) e ( 
          let app = (\t -> \apptypes -> Logic.ifElse (Lists.null apptypes) (Flows.pure t) ((\x -> case x of
                  Core.TypeForall v1 ->  
                    let v = (Core.forallTypeParameter v1)
                    in  
                      let t2 = (Core.forallTypeBody v1)
                      in (app (Substitution.substInType (Typing_.TypeSubst (Maps.singleton v (Lists.head apptypes))) t2) (Lists.tail apptypes))
                  _ -> (Flows.fail (Strings.cat [
                    "not a forall type: ",
                    Core__.type_ t,
                    " in ",
                    (Core__.term term)]))) t))
          in (Flows.bind (typeOfInternal cx vars types [] term) (\t1 -> Flows.bind (checkTypeVariables cx vars t1) (\_ -> app t1 apptypes)))))
  in (Monads.withTrace (Strings.cat [
    "checking type of: ",
    Core__.term term,
    " (vars: ",
    Formatting.showList Core.unName (Sets.toList vars),
    ", types: ",
    Formatting.showList Core.unName (Maps.keys types),
    ")"]) ((\x -> case x of
    Core.TermAnnotated v1 -> (checkApplied ( 
      let term1 = (Core.annotatedTermSubject v1)
      in (typeOfInternal cx vars types apptypes term1)))
    Core.TermApplication v1 -> (checkApplied ( 
      let a = (Core.applicationFunction v1)
      in  
        let b = (Core.applicationArgument v1)
        in (Flows.bind (typeOfInternal cx vars types [] a) (\t1 -> Flows.bind (typeOfInternal cx vars types [] b) (\t2 -> Flows.bind (checkTypeVariables cx vars t1) (\_ -> Flows.bind (checkTypeVariables cx vars t2) (\_ -> (\x -> case x of
          Core.TypeFunction v2 ->  
            let p = (Core.functionTypeDomain v2)
            in  
              let q = (Core.functionTypeCodomain v2)
              in (Logic.ifElse (Equality.equal p t2) (Flows.pure q) (Flows.fail (Strings.cat [
                "expected ",
                Core__.type_ p,
                " in ",
                Core__.term term,
                " but found ",
                (Core__.type_ t2)])))
          _ -> (Flows.fail (Strings.cat [
            "left hand side of application ",
            Core__.term term,
            " is not a function type: ",
            (Core__.type_ t1)]))) t1)))))))
    Core.TermFunction v1 -> ((\x -> case x of
      Core.FunctionElimination v2 -> ((\x -> case x of
        Core.EliminationProduct v3 -> (checkApplied ( 
          let index = (Core.tupleProjectionIndex v3)
          in  
            let arity = (Core.tupleProjectionArity v3)
            in  
              let mtypes = (Core.tupleProjectionDomain v3)
              in (Optionals.maybe (Flows.fail (Strings.cat [
                "untyped tuple projection: ",
                (Core__.term term)])) (\types -> Flows.bind (Flows.mapList (checkTypeVariables cx vars) types) (\_ -> Flows.pure (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeProduct types),
                Core.functionTypeCodomain = (Lists.at index types)})))) mtypes)))
        Core.EliminationRecord v3 ->  
          let tname = (Core.projectionTypeName v3)
          in  
            let fname = (Core.projectionField v3)
            in (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
              let svars = (Core.typeSchemeVariables schemaType)
              in  
                let stype = (Core.typeSchemeType schemaType)
                in (Flows.bind (Core_.recordType tname stype) (\sfields -> Flows.bind (Schemas.findFieldType fname sfields) (\ftyp ->  
                  let subst = (Typing_.TypeSubst (Maps.fromList (Lists.zip svars apptypes)))
                  in  
                    let sftyp = (Substitution.substInType subst ftyp)
                    in (Flows.pure (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (nominalApplication tname apptypes),
                      Core.functionTypeCodomain = sftyp}))))))))
        Core.EliminationUnion v3 ->  
          let tname = (Core.caseStatementTypeName v3)
          in  
            let dflt = (Core.caseStatementDefault v3)
            in  
              let cases = (Core.caseStatementCases v3)
              in  
                let cterms = (Lists.map Core.fieldTerm cases)
                in (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
                  let svars = (Core.typeSchemeVariables schemaType)
                  in  
                    let stype = (Core.typeSchemeType schemaType)
                    in (Flows.bind (Core_.unionType tname stype) (\sfields -> Flows.bind (Flows.traverseOptional (\e -> typeOfInternal cx vars types [] e) dflt) (\tdflt -> Flows.bind (Flows.mapList (\e -> typeOfInternal cx vars types [] e) cterms) (\tcterms -> Flows.bind (Flows.mapList (\t -> Flows.map Core.functionTypeCodomain (Core_.functionType t)) tcterms) (\cods ->  
                      let ts = (Optionals.cat (Lists.cons tdflt (Lists.map Optionals.pure cods)))
                      in (Flows.bind (checkSameType "case branches" ts) (\cod ->  
                        let subst = (Typing_.TypeSubst (Maps.fromList (Lists.zip svars apptypes)))
                        in  
                          let scod = (Substitution.substInType subst cod)
                          in (Flows.pure (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (nominalApplication tname apptypes),
                            Core.functionTypeCodomain = scod}))))))))))))
        Core.EliminationWrap v3 -> (Flows.bind (requireSchemaType cx v3) (\schemaType ->  
          let svars = (Core.typeSchemeVariables schemaType)
          in  
            let stype = (Core.typeSchemeType schemaType)
            in (Flows.bind (Core_.wrappedType v3 stype) (\wrapped ->  
              let subst = (Typing_.TypeSubst (Maps.fromList (Lists.zip svars apptypes)))
              in  
                let swrapped = (Substitution.substInType subst wrapped)
                in (Flows.pure (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (nominalApplication v3 apptypes),
                  Core.functionTypeCodomain = swrapped})))))))) v2)
      Core.FunctionLambda v2 -> (checkApplied ( 
        let x = (Core.lambdaParameter v2)
        in  
          let mt = (Core.lambdaDomain v2)
          in  
            let e = (Core.lambdaBody v2)
            in (Optionals.maybe (Flows.fail (Strings.cat [
              "untyped lambda: ",
              (Core__.term term)])) (\t -> Flows.bind (checkTypeVariables cx vars t) (\_ -> Flows.bind (typeOfInternal cx vars (Maps.insert x t types) [] e) (\t1 -> Flows.bind (checkTypeVariables cx vars t1) (\_ -> Flows.pure (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = t,
              Core.functionTypeCodomain = t1})))))) mt)))
      Core.FunctionPrimitive v2 -> (checkApplied ( 
        let ts = (Optionals.maybe (Flows.fail (Strings.cat [
                "no such primitive: ",
                (Core.unName v2)])) Flows.pure (Maps.lookup v2 (Typing_.inferenceContextPrimitiveTypes cx)))
        in (Flows.map Schemas.typeSchemeToFType ts)))) v1)
    Core.TermLet v1 -> (checkApplied ( 
      let bs = (Core.letBindings v1)
      in  
        let env = (Core.letEnvironment v1)
        in  
          let bnames = (Lists.map Core.bindingName bs)
          in  
            let bterms = (Lists.map Core.bindingTerm bs)
            in  
              let btypeOf = (\b -> Optionals.maybe (Flows.fail (Strings.cat [
                      "untyped let binding in ",
                      (Core__.term term)])) (\ts -> Flows.pure (Schemas.typeSchemeToFType ts)) (Core.bindingType b))
              in (Flows.bind (Flows.mapList btypeOf bs) (\btypes ->  
                let types2 = (Maps.union (Maps.fromList (Lists.zip bnames btypes)) types)
                in (Flows.bind (Flows.mapList (typeOfInternal cx vars types2 []) bterms) (\typeofs -> Flows.bind (Flows.mapList (checkTypeVariables cx vars) btypes) (\_ -> Flows.bind (Flows.mapList (checkTypeVariables cx vars) typeofs) (\_ -> Logic.ifElse (Equality.equal typeofs btypes) (typeOfInternal cx vars types2 [] env) (Flows.fail (Strings.cat [
                  "binding types disagree: ",
                  Formatting.showList Core__.type_ btypes,
                  " and ",
                  Formatting.showList Core__.type_ typeofs,
                  " from terms: ",
                  (Formatting.showList Core__.term bterms)]))))))))))
    Core.TermList v1 -> (Logic.ifElse (Lists.null v1) (Logic.ifElse (Equality.equal (Lists.length apptypes) 1) (Flows.pure (Core.TypeList (Lists.head apptypes))) (Flows.fail "list type applied to more or less than one argument")) (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) v1) (\eltypes -> Flows.bind (checkSameType "list elements" eltypes) (\unifiedType -> Flows.bind (checkTypeVariables cx vars unifiedType) (\_ -> Flows.pure (Core.TypeList unifiedType))))))
    Core.TermLiteral v1 -> (checkApplied (Flows.pure (Core.TypeLiteral (Variants.literalType v1))))
    Core.TermMap v1 -> (Logic.ifElse (Maps.null v1) (Logic.ifElse (Equality.equal (Lists.length apptypes) 2) (Flows.pure (Core.TypeMap (Core.MapType {
      Core.mapTypeKeys = (Lists.at 1 apptypes),
      Core.mapTypeValues = (Lists.at 0 apptypes)}))) (Flows.fail "map type applied to more or less than two arguments")) (checkApplied ( 
      let pairs = (Maps.toList v1)
      in (Flows.bind (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) (Lists.map fst pairs)) (checkSameType "map keys")) (\kt -> Flows.bind (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) (Lists.map snd pairs)) (checkSameType "map values")) (\vt -> Flows.bind (checkTypeVariables cx vars kt) (\_ -> Flows.bind (checkTypeVariables cx vars vt) (\_ -> Flows.pure (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = kt,
        Core.mapTypeValues = vt}))))))))))
    Core.TermOptional v1 -> (Optionals.maybe (Logic.ifElse (Equality.equal (Lists.length apptypes) 1) (Flows.pure (Core.TypeOptional (Lists.head apptypes))) (Flows.fail "optional type applied to more or less than one argument")) (\term -> checkApplied (Flows.bind (typeOfInternal cx vars types [] term) (\termType -> Flows.bind (checkTypeVariables cx vars termType) (\_ -> Flows.pure (Core.TypeOptional termType))))) v1)
    Core.TermProduct v1 -> (checkApplied (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) v1) (\etypes -> Flows.bind (Flows.mapList (checkTypeVariables cx vars) etypes) (\_ -> Flows.pure (Core.TypeProduct etypes)))))
    Core.TermRecord v1 ->  
      let tname = (Core.recordTypeName v1)
      in  
        let fields = (Core.recordFields v1)
        in (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) (Lists.map Core.fieldTerm fields)) (\ftypes -> Flows.bind (Flows.mapList (checkTypeVariables cx vars) ftypes) (\_ -> typeOfNominal "record typeOf" cx tname (Core.TypeRecord (Core.RowType {
          Core.rowTypeTypeName = tname,
          Core.rowTypeFields = (Lists.zipWith (\n -> \t -> Core.FieldType {
            Core.fieldTypeName = n,
            Core.fieldTypeType = t}) (Lists.map Core.fieldName fields) ftypes)})))))
    Core.TermSet v1 -> (Logic.ifElse (Sets.null v1) (Logic.ifElse (Equality.equal (Lists.length apptypes) 1) (Flows.pure (Core.TypeSet (Lists.head apptypes))) (Flows.fail "set type applied to more or less than one argument")) (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) (Sets.toList v1)) (\eltypes -> Flows.bind (checkSameType "set elements" eltypes) (\unifiedType -> Flows.bind (checkTypeVariables cx vars unifiedType) (\_ -> Flows.pure (Core.TypeSet unifiedType))))))
    Core.TermTypeLambda v1 ->  
      let v = (Core.typeLambdaParameter v1)
      in  
        let e = (Core.typeLambdaBody v1)
        in (Flows.bind (typeOfInternal cx (Sets.insert v vars) types [] e) (\t1 -> Flows.bind (checkTypeVariables cx (Sets.insert v vars) t1) (\_ -> Flows.pure (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = v,
          Core.forallTypeBody = t1})))))
    Core.TermTypeApplication v1 ->  
      let e = (Core.typedTermTerm v1)
      in  
        let t = (Core.typedTermType v1)
        in (typeOfInternal cx vars types (Lists.cons t apptypes) e)
    Core.TermUnion v1 ->  
      let tname = (Core.injectionTypeName v1)
      in  
        let field = (Core.injectionField v1)
        in  
          let fname = (Core.fieldName field)
          in  
            let fterm = (Core.fieldTerm field)
            in (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
              let svars = (Core.typeSchemeVariables schemaType)
              in  
                let stype = (Core.typeSchemeType schemaType)
                in (Flows.bind (Core_.unionType tname stype) (\sfields -> Flows.bind (Schemas.findFieldType fname sfields) (\ftyp -> Flows.pure (nominalApplication tname apptypes))))))
    Core.TermUnit -> (checkApplied (Flows.pure Core.TypeUnit))
    Core.TermVariable v1 -> (checkApplied (Optionals.maybe (Flows.fail (Strings.cat [
      "unbound variable: ",
      (Core.unName v1)])) Flows.pure (Maps.lookup v1 types)))
    Core.TermWrap v1 ->  
      let tname = (Core.wrappedTermTypeName v1)
      in  
        let innerTerm = (Core.wrappedTermObject v1)
        in (Flows.bind (typeOfInternal cx vars types [] innerTerm) (\innerType -> Flows.bind (checkTypeVariables cx vars innerType) (\_ -> typeOfNominal "wrapper typeOf" cx tname (Core.TypeWrap (Core.WrappedType {
          Core.wrappedTypeTypeName = tname,
          Core.wrappedTypeObject = innerType})))))
    _ -> (Flows.fail (Strings.cat [
      "unsupported term variant in typeOf: ",
      (Mantle_.termVariant (Variants.termVariant term))]))) term))

typeOfNominal :: (String -> Typing_.InferenceContext -> Core.Name -> Core.Type -> Compute.Flow t0 Core.Type)
typeOfNominal desc cx tname expected =  
  let resolveType = (\subst -> \v -> Optionals.fromMaybe (Core.TypeVariable v) (Maps.lookup v subst))
  in (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
    let svars = (Core.typeSchemeVariables schemaType)
    in  
      let stype = (Core.typeSchemeType schemaType)
      in (Flows.bind (Unification.unifyTypes (Typing_.inferenceContextSchemaTypes cx) stype expected desc) (\substWrapper ->  
        let subst = (Typing_.unTypeSubst substWrapper)
        in  
          let tparams = (Lists.map (resolveType subst) svars)
          in (Flows.pure (nominalApplication tname tparams))))))

-- | The trivial inference result for the unit term
inferTypeOfUnit :: Typing_.InferenceResult
inferTypeOfUnit = Typing_.InferenceResult {
  Typing_.inferenceResultTerm = Core.TermUnit,
  Typing_.inferenceResultType = Core.TypeUnit,
  Typing_.inferenceResultSubst = Substitution.idTypeSubst}

-- | Create an inference result
yield :: (Core.Term -> Core.Type -> Typing_.TypeSubst -> Typing_.InferenceResult)
yield term typ subst = Typing_.InferenceResult {
  Typing_.inferenceResultTerm = (Substitution.substTypesInTerm subst term),
  Typing_.inferenceResultType = (Substitution.substInType subst typ),
  Typing_.inferenceResultSubst = subst}

yieldChecked :: (t0 -> t1 -> Core.Term -> Core.Type -> Typing_.TypeSubst -> Compute.Flow t2 Typing_.InferenceResult)
yieldChecked cx vars term typ subst =  
  let iterm = (Substitution.substTypesInTerm subst term)
  in  
    let itype = (Substitution.substInType subst typ)
    in (Flows.pure (Typing_.InferenceResult {
      Typing_.inferenceResultTerm = iterm,
      Typing_.inferenceResultType = itype,
      Typing_.inferenceResultSubst = subst}))

yieldDebug :: (t0 -> t1 -> Core.Term -> Core.Type -> Typing_.TypeSubst -> Compute.Flow t2 Typing_.InferenceResult)
yieldDebug cx debugId term typ subst =  
  let rterm = (Substitution.substTypesInTerm subst term)
  in  
    let rtyp = (Substitution.substInType subst typ)
    in (Flows.bind (Annotations.debugIf debugId (Strings.cat [
      "\n\tterm: ",
      Core__.term term,
      "\n\ttyp: ",
      Core__.type_ typ,
      "\n\tsubst: ",
      Typing.typeSubst subst,
      "\n\trterm: ",
      Core__.term rterm,
      "\n\trtyp: ",
      (Core__.type_ rtyp)])) (\result -> Flows.pure (Typing_.InferenceResult {
      Typing_.inferenceResultTerm = rterm,
      Typing_.inferenceResultType = rtyp,
      Typing_.inferenceResultSubst = subst})))
