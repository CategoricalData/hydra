-- Note: this is an automatically generated file. Do not edit.

-- | Type inference following Algorithm W, extended for nominal terms and types

module Hydra.Inference where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Checking as Checking
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Core as Core_
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
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Show.Typing as Typing
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing_
import qualified Hydra.Unification as Unification
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Unify type constraints and check the substitution
bindConstraints :: (Context.Context -> Graph.Graph -> [Typing_.TypeConstraint] -> Either (Context.InContext Error.Error) Typing_.TypeSubst)
bindConstraints flowCx cx constraints = (Eithers.bind (Eithers.bimap (\_ic -> Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unificationErrorMessage (Context.inContextObject _ic)))),
  Context.inContextContext = (Context.inContextContext _ic)}) (\_a -> _a) (Unification.unifyTypeConstraints flowCx (Graph.graphSchemaTypes cx) constraints)) (\s -> Eithers.bind (Checking.checkTypeSubst flowCx cx s) (\_ -> Right s)))

-- | Place unbound type variables appearing anywhere under a typed let binding in the type scheme of that binding. These variables may appear in the binding type scheme itself or in that of a subterm, in domain types attached to functions, and in type abstraction and type application terms. This process attempts to capture type variables which have escaped unification, e.g. due to unused code. However, unbound type variables not appearing beneath any typed let binding remain unbound.
bindUnboundTypeVariables :: (Graph.Graph -> Core.Term -> Core.Term)
bindUnboundTypeVariables cx term0 =  
  let svars = (Sets.fromList (Maps.keys (Graph.graphSchemaTypes cx)))
  in  
    let rewrite = (\recurse -> \term -> (\x -> case x of
            Core.TermLet v0 ->  
              let forBinding = (\b ->  
                      let bname = (Core.bindingName b)
                      in  
                        let bterm = (Core.bindingTerm b)
                        in (Maybes.maybe (Core.Binding {
                          Core.bindingName = bname,
                          Core.bindingTerm = (bindUnboundTypeVariables cx bterm),
                          Core.bindingType = Nothing}) (\ts ->  
                          let bvars = (Sets.fromList (Core.typeSchemeVariables ts))
                          in  
                            let unboundInType = (Rewriting.freeVariablesInType (Core.typeSchemeType ts))
                            in  
                              let unboundInTerm = (Rewriting.freeTypeVariablesInTerm bterm)
                              in  
                                let unbound = (Sets.toList (Sets.difference (Sets.union unboundInType unboundInTerm) (Sets.union svars bvars)))
                                in  
                                  let ts2 = Core.TypeScheme {
                                          Core.typeSchemeVariables = (Lists.concat2 (Core.typeSchemeVariables ts) unbound),
                                          Core.typeSchemeType = (Core.typeSchemeType ts),
                                          Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts)}
                                  in  
                                    let bterm2 = (Lists.foldl (\t -> \v -> Core.TermTypeLambda (Core.TypeLambda {
                                            Core.typeLambdaParameter = v,
                                            Core.typeLambdaBody = t})) bterm unbound)
                                    in Core.Binding {
                                      Core.bindingName = bname,
                                      Core.bindingTerm = bterm2,
                                      Core.bindingType = (Just ts2)}) (Core.bindingType b)))
              in (Core.TermLet (Core.Let {
                Core.letBindings = (Lists.map forBinding (Core.letBindings v0)),
                Core.letBody = (bindUnboundTypeVariables cx (Core.letBody v0))}))
            _ -> (recurse term)) term)
    in (Rewriting.rewriteTerm rewrite term0)

-- | Fold a list of type variables over a term to build a type application term
buildTypeApplicationTerm :: ([Core.Name] -> Core.Term -> Core.Term)
buildTypeApplicationTerm tvars body = (Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypeApplicationTerm {
  Core.typeApplicationTermBody = t,
  Core.typeApplicationTermType = (Core.TypeVariable v)})) body tvars)

-- | Add (term variable, type scheme) pairs to the graph's bound types
extendContext :: ([(Core.Name, Core.TypeScheme)] -> Graph.Graph -> Graph.Graph)
extendContext pairs cx = Graph.Graph {
  Graph.graphBoundTerms = (Graph.graphBoundTerms cx),
  Graph.graphBoundTypes = (Maps.union (Maps.fromList pairs) (Graph.graphBoundTypes cx)),
  Graph.graphClassConstraints = (Graph.graphClassConstraints cx),
  Graph.graphLambdaVariables = (Graph.graphLambdaVariables cx),
  Graph.graphMetadata = (Graph.graphMetadata cx),
  Graph.graphPrimitives = (Graph.graphPrimitives cx),
  Graph.graphSchemaTypes = (Graph.graphSchemaTypes cx),
  Graph.graphTypeVariables = (Graph.graphTypeVariables cx)}

-- | Finalize an inferred term by checking for unbound type variables, then normalizing type variables
finalizeInferredTerm :: (Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Term)
finalizeInferredTerm flowCx cx term =  
  let term2 = (bindUnboundTypeVariables cx term)
  in (Eithers.bind (Checking.checkForUnboundTypeVariables flowCx cx term2) (\_ -> Right (Rewriting.normalizeTypeVariablesInTerm term2)))

-- | Infer a term's type and map over the result
forInferredTerm :: (Context.Context -> Graph.Graph -> Core.Term -> String -> (Typing_.InferenceResult -> t0) -> Either (Context.InContext Error.Error) (t0, Context.Context))
forInferredTerm fcx cx term desc f = (Eithers.bind (inferTypeOfTerm fcx cx term desc) (\rp -> Right (f rp, (Typing_.inferenceResultContext rp))))

-- | Get all free variables in a graph's bound types
freeVariablesInContext :: (Graph.Graph -> S.Set Core.Name)
freeVariablesInContext cx = (Lists.foldl Sets.union Sets.empty (Lists.map Rewriting.freeVariablesInTypeSchemeSimple (Maps.elems (Graph.graphBoundTypes cx))))

-- | Generate a fresh type variable
freshVariableType :: (Context.Context -> (Core.Type, Context.Context))
freshVariableType cx =  
  let result = (Schemas.freshName cx)
  in  
    let name = (Pairs.first result)
    in  
      let cx2 = (Pairs.second result)
      in (Core.TypeVariable name, cx2)

-- | Generalize a type to a type scheme
generalize :: (Graph.Graph -> Core.Type -> Core.TypeScheme)
generalize cx typ =  
  let isTypeVarName = (\name ->  
          let parts = (Strings.splitOn "." (Core.unName name))
          in (Equality.lte (Lists.length parts) 1))
  in  
    let vars = (Lists.nub (Lists.filter (\v -> Logic.and (isUnbound cx v) (isTypeVarName v)) (Rewriting.freeVariablesInTypeOrdered typ)))
    in  
      let allConstraints = (Graph.graphClassConstraints cx)
      in  
        let relevantConstraints = (Maps.fromList (Maybes.cat (Lists.map (\v -> Maybes.map (\meta -> (v, meta)) (Maps.lookup v allConstraints)) vars)))
        in  
          let constraintsMaybe = (Logic.ifElse (Maps.null relevantConstraints) Nothing (Just relevantConstraints))
          in Core.TypeScheme {
            Core.typeSchemeVariables = vars,
            Core.typeSchemeType = typ,
            Core.typeSchemeConstraints = constraintsMaybe}

-- | Infer types for all elements in a graph, using the provided ordered bindings. Returns both the inferred graph and the ordered inferred bindings.
inferGraphTypes :: (Context.Context -> [Core.Binding] -> Graph.Graph -> Either (Context.InContext Error.Error) ((Graph.Graph, [Core.Binding]), Context.Context))
inferGraphTypes fcx0 bindings0 g0 =  
  let fcx = Context.Context {
          Context.contextTrace = (Lists.cons "graph inference" (Context.contextTrace fcx0)),
          Context.contextMessages = (Context.contextMessages fcx0),
          Context.contextOther = (Context.contextOther fcx0)}
  in  
    let let0 = Core.Let {
            Core.letBindings = bindings0,
            Core.letBody = Core.TermUnit}
    in  
      let fromLetTerm = (\l ->  
              let bindings = (Core.letBindings l)
              in  
                let prims = (Graph.graphPrimitives g0)
                in  
                  let schemaTypes = (Graph.graphSchemaTypes g0)
                  in  
                    let g = Graph.Graph {
                            Graph.graphBoundTerms = (Graph.graphBoundTerms (Lexical.buildGraph bindings Maps.empty prims)),
                            Graph.graphBoundTypes = (Graph.graphBoundTypes (Lexical.buildGraph bindings Maps.empty prims)),
                            Graph.graphClassConstraints = (Graph.graphClassConstraints (Lexical.buildGraph bindings Maps.empty prims)),
                            Graph.graphLambdaVariables = (Graph.graphLambdaVariables (Lexical.buildGraph bindings Maps.empty prims)),
                            Graph.graphMetadata = (Graph.graphMetadata (Lexical.buildGraph bindings Maps.empty prims)),
                            Graph.graphPrimitives = (Graph.graphPrimitives (Lexical.buildGraph bindings Maps.empty prims)),
                            Graph.graphSchemaTypes = schemaTypes,
                            Graph.graphTypeVariables = (Graph.graphTypeVariables (Lexical.buildGraph bindings Maps.empty prims))}
                    in (g, bindings))
      in (Eithers.bind (inferTypeOfTerm fcx g0 (Core.TermLet let0) "graph term") (\result ->  
        let fcx2 = (Typing_.inferenceResultContext result)
        in  
          let term = (Typing_.inferenceResultTerm result)
          in (Eithers.bind (finalizeInferredTerm fcx2 g0 term) (\finalized -> (\x -> case x of
            Core.TermLet v0 -> (Right (fromLetTerm v0, fcx2))
            Core.TermVariable _ -> (Left (Context.InContext {
              Context.inContextObject = (Error.ErrorOther (Error.OtherError "Expected inferred graph as let term")),
              Context.inContextContext = fcx2}))) finalized))))

-- | Infer the type of a term in a given inference context
inferInGraphContext :: (Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferInGraphContext fcx cx term = (inferTypeOfTerm fcx cx term "single term")

-- | Infer types for multiple terms, propagating class constraints from sub-expressions
inferMany :: (Context.Context -> Graph.Graph -> [(Core.Term, String)] -> Either (Context.InContext Error.Error) (([Core.Term], ([Core.Type], (Typing_.TypeSubst, (M.Map Core.Name Core.TypeVariableMetadata)))), Context.Context))
inferMany fcx cx pairs =  
  let dflt =  
          let e = (Pairs.first (Lists.head pairs))
          in  
            let desc = (Pairs.second (Lists.head pairs))
            in  
              let tl = (Lists.tail pairs)
              in (Eithers.bind (inferTypeOfTerm fcx cx e desc) (\result1 ->  
                let fcx2 = (Typing_.inferenceResultContext result1)
                in  
                  let e1 = (Typing_.inferenceResultTerm result1)
                  in  
                    let t1 = (Typing_.inferenceResultType result1)
                    in  
                      let s1 = (Typing_.inferenceResultSubst result1)
                      in  
                        let c1 = (Typing_.inferenceResultClassConstraints result1)
                        in (Eithers.bind (inferMany fcx2 (Substitution.substInContext s1 cx) tl) (\rp2 ->  
                          let result2 = (Pairs.first rp2)
                          in  
                            let fcx3 = (Pairs.second rp2)
                            in  
                              let e2 = (Pairs.first result2)
                              in  
                                let t2 = (Pairs.first (Pairs.second result2))
                                in  
                                  let s2 = (Pairs.first (Pairs.second (Pairs.second result2)))
                                  in  
                                    let c2 = (Pairs.second (Pairs.second (Pairs.second result2)))
                                    in  
                                      let c1Subst = (Substitution.substInClassConstraints s2 c1)
                                      in  
                                        let mergedConstraints = (mergeClassConstraints c1Subst c2)
                                        in (Right ((Lists.cons (Substitution.substTypesInTerm s2 e1) e2, (Lists.cons (Substitution.substInType s2 t1) t2, (Substitution.composeTypeSubst s1 s2, mergedConstraints))), fcx3))))))
  in (Logic.ifElse (Lists.null pairs) (Right (([], ([], (Substitution.idTypeSubst, Maps.empty))), fcx)) dflt)

-- | Map a possibly untyped term to a fully typed term and its type
inferTypeOf :: (Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) ((Core.Term, Core.TypeScheme), Context.Context))
inferTypeOf fcx cx term =  
  let letTerm = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "ignoredVariableName"),
              Core.bindingTerm = term,
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermLiteral (Core.LiteralString "ignoredBody"))}))
  in (Eithers.bind (inferTypeOfTerm fcx cx letTerm "infer type of term") (\result ->  
    let fcx2 = (Typing_.inferenceResultContext result)
    in (Eithers.bind (finalizeInferredTerm fcx2 cx (Typing_.inferenceResultTerm result)) (\finalized -> Eithers.bind (Core_.let_ fcx2 cx finalized) (\letResult ->  
      let bindings = (Core.letBindings letResult)
      in (Logic.ifElse (Equality.equal 1 (Lists.length bindings)) ( 
        let binding = (Lists.head bindings)
        in  
          let term1 = (Core.bindingTerm binding)
          in  
            let mts = (Core.bindingType binding)
            in (Maybes.maybe (Left (Context.InContext {
              Context.inContextObject = (Error.ErrorOther (Error.OtherError "Expected a type scheme")),
              Context.inContextContext = fcx2})) (\ts -> Right ((term1, ts), fcx2)) mts)) (Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
          "Expected a single binding with a type scheme, but got: ",
          (Literals.showInt32 (Lists.length bindings)),
          " bindings"]))),
        Context.inContextContext = fcx2}))))))))

-- | Infer the type of an annotated term (Either version)
inferTypeOfAnnotatedTerm :: (Context.Context -> Graph.Graph -> Core.AnnotatedTerm -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfAnnotatedTerm fcx cx at =  
  let term = (Core.annotatedTermBody at)
  in  
    let ann = (Core.annotatedTermAnnotation at)
    in (Eithers.bind (inferTypeOfTerm fcx cx term "annotated term") (\result ->  
      let fcx2 = (Typing_.inferenceResultContext result)
      in  
        let iterm = (Typing_.inferenceResultTerm result)
        in  
          let itype = (Typing_.inferenceResultType result)
          in  
            let isubst = (Typing_.inferenceResultSubst result)
            in  
              let iconstraints = (Typing_.inferenceResultClassConstraints result)
              in (Right (Typing_.InferenceResult {
                Typing_.inferenceResultTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = iterm,
                  Core.annotatedTermAnnotation = ann})),
                Typing_.inferenceResultType = itype,
                Typing_.inferenceResultSubst = isubst,
                Typing_.inferenceResultClassConstraints = iconstraints,
                Typing_.inferenceResultContext = fcx2}))))

-- | Infer the type of a function application (Either version)
inferTypeOfApplication :: (Context.Context -> Graph.Graph -> Core.Application -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfApplication fcx0 cx app =  
  let fcx = Context.Context {
          Context.contextTrace = (Lists.cons "application" (Context.contextTrace fcx0)),
          Context.contextMessages = (Context.contextMessages fcx0),
          Context.contextOther = (Context.contextOther fcx0)}
  in  
    let e0 = (Core.applicationFunction app)
    in  
      let e1 = (Core.applicationArgument app)
      in (Eithers.bind (inferTypeOfTerm fcx cx e0 "lhs") (\lhsResult ->  
        let fcx2 = (Typing_.inferenceResultContext lhsResult)
        in  
          let a = (Typing_.inferenceResultTerm lhsResult)
          in  
            let t0 = (Typing_.inferenceResultType lhsResult)
            in  
              let s0 = (Typing_.inferenceResultSubst lhsResult)
              in  
                let c0 = (Typing_.inferenceResultClassConstraints lhsResult)
                in (Eithers.bind (inferTypeOfTerm fcx2 (Substitution.substInContext s0 cx) e1 "rhs") (\rhsResult ->  
                  let fcx3 = (Typing_.inferenceResultContext rhsResult)
                  in  
                    let b = (Typing_.inferenceResultTerm rhsResult)
                    in  
                      let t1 = (Typing_.inferenceResultType rhsResult)
                      in  
                        let s1 = (Typing_.inferenceResultSubst rhsResult)
                        in  
                          let c1 = (Typing_.inferenceResultClassConstraints rhsResult)
                          in  
                            let vResult = (Schemas.freshName fcx3)
                            in  
                              let v = (Pairs.first vResult)
                              in  
                                let fcx4 = (Pairs.second vResult)
                                in (Eithers.bind (Eithers.bimap (\_ic -> Context.InContext {
                                  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unificationErrorMessage (Context.inContextObject _ic)))),
                                  Context.inContextContext = (Context.inContextContext _ic)}) (\_a -> _a) (Unification.unifyTypes fcx4 (Graph.graphSchemaTypes cx) (Substitution.substInType s1 t0) (Core.TypeFunction (Core.FunctionType {
                                  Core.functionTypeDomain = t1,
                                  Core.functionTypeCodomain = (Core.TypeVariable v)})) "application lhs")) (\s2 -> Eithers.bind (Checking.checkTypeSubst fcx4 cx s2) (\_ ->  
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
                                      in  
                                        let c0Subst = (Substitution.substInClassConstraints s2 (Substitution.substInClassConstraints s1 c0))
                                        in  
                                          let c1Subst = (Substitution.substInClassConstraints s2 c1)
                                          in  
                                            let rConstraints = (mergeClassConstraints c0Subst c1Subst)
                                            in (Right (Typing_.InferenceResult {
                                              Typing_.inferenceResultTerm = rExpr,
                                              Typing_.inferenceResultType = rType,
                                              Typing_.inferenceResultSubst = rSubst,
                                              Typing_.inferenceResultClassConstraints = rConstraints,
                                              Typing_.inferenceResultContext = fcx4})))))))))

-- | Infer the type of a case statement (Either version)
inferTypeOfCaseStatement :: (Context.Context -> Graph.Graph -> Core.CaseStatement -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfCaseStatement fcx cx caseStmt =  
  let tname = (Core.caseStatementTypeName caseStmt)
  in  
    let dflt = (Core.caseStatementDefault caseStmt)
    in  
      let cases = (Core.caseStatementCases caseStmt)
      in  
        let fnames = (Lists.map Core.fieldName cases)
        in (Eithers.bind (Schemas.requireSchemaType fcx (Graph.graphSchemaTypes cx) tname) (\stRp ->  
          let schemaType = (Pairs.first stRp)
          in  
            let fcx2 = (Pairs.second stRp)
            in  
              let svars = (Core.typeSchemeVariables schemaType)
              in  
                let stype = (Core.typeSchemeType schemaType)
                in (Eithers.bind (Core_.unionType fcx2 tname stype) (\sfields -> Eithers.bind (Eithers.mapMaybe (\t -> inferTypeOfTerm fcx2 cx t (Strings.cat [
                  "case ",
                  (Core.unName tname),
                  ".<default>"])) dflt) (\dfltRp ->  
                  let dfltResult = dfltRp
                  in  
                    let fcx3 = (Maybes.fromMaybe fcx2 (Maybes.map Typing_.inferenceResultContext dfltRp))
                    in (Eithers.bind (inferMany fcx3 cx (Lists.map (\f -> (Core.fieldTerm f, (Strings.cat [
                      "case ",
                      (Core.unName tname),
                      ".",
                      (Core.unName (Core.fieldName f))]))) cases)) (\caseRp ->  
                      let caseResults = (Pairs.first caseRp)
                      in  
                        let fcx4 = (Pairs.second caseRp)
                        in  
                          let iterms = (Pairs.first caseResults)
                          in  
                            let itypes = (Pairs.first (Pairs.second caseResults))
                            in  
                              let isubst = (Pairs.first (Pairs.second (Pairs.second caseResults)))
                              in  
                                let caseElemConstraints = (Pairs.second (Pairs.second (Pairs.second caseResults)))
                                in  
                                  let codvResult = (Schemas.freshName fcx4)
                                  in  
                                    let codv = (Pairs.first codvResult)
                                    in  
                                      let fcx5 = (Pairs.second codvResult)
                                      in  
                                        let cod = (Core.TypeVariable codv)
                                        in  
                                          let caseMap = (Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) sfields))
                                          in  
                                            let dfltConstraints = (Maybes.toList (Maybes.map (\r -> Typing_.TypeConstraint {
                                                    Typing_.typeConstraintLeft = cod,
                                                    Typing_.typeConstraintRight = (Substitution.substInType isubst (Typing_.inferenceResultType r)),
                                                    Typing_.typeConstraintComment = "match default"}) dfltResult))
                                            in  
                                              let caseConstraints = (Maybes.cat (Lists.zipWith (\fname -> \itype -> Maybes.map (\ftype -> Typing_.TypeConstraint {
                                                      Typing_.typeConstraintLeft = itype,
                                                      Typing_.typeConstraintRight = (Core.TypeFunction (Core.FunctionType {
                                                        Core.functionTypeDomain = ftype,
                                                        Core.functionTypeCodomain = cod})),
                                                      Typing_.typeConstraintComment = "case type"}) (Maps.lookup fname caseMap)) fnames itypes))
                                              in  
                                                let dfltClassConstraints = (Maybes.fromMaybe Maps.empty (Maybes.map Typing_.inferenceResultClassConstraints dfltResult))
                                                in  
                                                  let allElemConstraints = (mergeClassConstraints caseElemConstraints dfltClassConstraints)
                                                  in (Eithers.bind (mapConstraints fcx5 cx (\subst -> yieldWithConstraints fcx5 (buildTypeApplicationTerm svars (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                                    Core.caseStatementTypeName = tname,
                                                    Core.caseStatementDefault = (Maybes.map Typing_.inferenceResultTerm dfltResult),
                                                    Core.caseStatementCases = (Lists.zipWith (\n -> \t -> Core.Field {
                                                      Core.fieldName = n,
                                                      Core.fieldTerm = t}) fnames iterms)}))))) (Core.TypeFunction (Core.FunctionType {
                                                    Core.functionTypeDomain = (Schemas.nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)),
                                                    Core.functionTypeCodomain = cod})) (Substitution.composeTypeSubstList (Lists.concat [
                                                    Maybes.toList (Maybes.map Typing_.inferenceResultSubst dfltResult),
                                                    [
                                                      isubst,
                                                      subst]])) (Substitution.substInClassConstraints subst allElemConstraints)) (Lists.concat [
                                                    dfltConstraints,
                                                    caseConstraints])) (\mcResult -> Right mcResult)))))))))

-- | Infer the type of a collection. The classNames parameter specifies type classes (e.g. ordering) that the element type variable must satisfy.
inferTypeOfCollection :: (Context.Context -> Graph.Graph -> (Core.Type -> Core.Type) -> ([Core.Term] -> Core.Term) -> String -> S.Set Core.Name -> [Core.Term] -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfCollection fcx cx typCons trmCons desc classNames els =  
  let varResult = (Schemas.freshName fcx)
  in  
    let var = (Pairs.first varResult)
    in  
      let fcx2 = (Pairs.second varResult)
      in  
        let classConstraints = (Logic.ifElse (Sets.null classNames) Maps.empty (Maps.singleton var (Core.TypeVariableMetadata {
                Core.typeVariableMetadataClasses = classNames})))
        in (Logic.ifElse (Lists.null els) (Right (yieldWithConstraints fcx2 (buildTypeApplicationTerm [
          var] (trmCons [])) (typCons (Core.TypeVariable var)) Substitution.idTypeSubst classConstraints)) (Eithers.bind (inferMany fcx2 cx (Lists.zip els (Lists.map (\i -> Strings.cat [
          "#",
          (Literals.showInt32 i)]) (Math.range 1 (Math.add (Lists.length els) 1))))) (\resultsRp ->  
          let results = (Pairs.first resultsRp)
          in  
            let fcx3 = (Pairs.second resultsRp)
            in  
              let terms = (Pairs.first results)
              in  
                let types = (Pairs.first (Pairs.second results))
                in  
                  let subst1 = (Pairs.first (Pairs.second (Pairs.second results)))
                  in  
                    let elemConstraints = (Pairs.second (Pairs.second (Pairs.second results)))
                    in  
                      let constraints = (Lists.map (\t -> Typing_.TypeConstraint {
                              Typing_.typeConstraintLeft = (Core.TypeVariable var),
                              Typing_.typeConstraintRight = t,
                              Typing_.typeConstraintComment = desc}) types)
                      in  
                        let allConstraints = (mergeClassConstraints classConstraints elemConstraints)
                        in (Eithers.bind (mapConstraints fcx3 cx (\subst2 ->  
                          let iterm = (trmCons terms)
                          in  
                            let itype = (typCons (Core.TypeVariable var))
                            in  
                              let isubst = (Substitution.composeTypeSubst subst1 subst2)
                              in (yieldWithConstraints fcx3 iterm itype isubst (Substitution.substInClassConstraints subst2 allConstraints))) constraints) (\mcResult -> Right mcResult)))))

-- | Infer the type of an either value (Either version)
inferTypeOfEither :: (Context.Context -> Graph.Graph -> Either Core.Term Core.Term -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfEither fcx cx e = (Eithers.either (\l -> Eithers.bind (inferTypeOfTerm fcx cx l "either left value") (\r1 ->  
  let fcx2 = (Typing_.inferenceResultContext r1)
  in  
    let iterm = (Typing_.inferenceResultTerm r1)
    in  
      let leftType = (Typing_.inferenceResultType r1)
      in  
        let subst = (Typing_.inferenceResultSubst r1)
        in  
          let fvResult = (freshVariableType fcx2)
          in  
            let rightType = (Pairs.first fvResult)
            in  
              let fcx3 = (Pairs.second fvResult)
              in  
                let eitherTerm = (Core.TermEither (Left iterm))
                in  
                  let termWithLeftType = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = eitherTerm,
                          Core.typeApplicationTermType = leftType}))
                  in  
                    let termWithBothTypes = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = termWithLeftType,
                            Core.typeApplicationTermType = rightType}))
                    in  
                      let eitherType = (Core.TypeEither (Core.EitherType {
                              Core.eitherTypeLeft = leftType,
                              Core.eitherTypeRight = rightType}))
                      in (Right (yieldChecked fcx3 termWithBothTypes eitherType subst)))) (\r -> Eithers.bind (inferTypeOfTerm fcx cx r "either right value") (\r1 ->  
  let fcx2 = (Typing_.inferenceResultContext r1)
  in  
    let iterm = (Typing_.inferenceResultTerm r1)
    in  
      let rightType = (Typing_.inferenceResultType r1)
      in  
        let subst = (Typing_.inferenceResultSubst r1)
        in  
          let fvResult = (freshVariableType fcx2)
          in  
            let leftType = (Pairs.first fvResult)
            in  
              let fcx3 = (Pairs.second fvResult)
              in  
                let eitherTerm = (Core.TermEither (Right iterm))
                in  
                  let termWithLeftType = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = eitherTerm,
                          Core.typeApplicationTermType = leftType}))
                  in  
                    let termWithBothTypes = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = termWithLeftType,
                            Core.typeApplicationTermType = rightType}))
                    in  
                      let eitherType = (Core.TypeEither (Core.EitherType {
                              Core.eitherTypeLeft = leftType,
                              Core.eitherTypeRight = rightType}))
                      in (Right (yieldChecked fcx3 termWithBothTypes eitherType subst)))) e)

-- | Infer the type of an elimination (Either version)
inferTypeOfElimination :: (Context.Context -> Graph.Graph -> Core.Elimination -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfElimination fcx cx elm = ((\x -> case x of
  Core.EliminationRecord v0 -> (inferTypeOfProjection fcx cx v0)
  Core.EliminationUnion v0 -> (inferTypeOfCaseStatement fcx cx v0)
  Core.EliminationWrap v0 -> (inferTypeOfUnwrap fcx cx v0)) elm)

-- | Infer the type of a function (Either version)
inferTypeOfFunction :: (Context.Context -> Graph.Graph -> Core.Function -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfFunction fcx cx f = ((\x -> case x of
  Core.FunctionElimination v0 -> (inferTypeOfElimination fcx cx v0)
  Core.FunctionLambda v0 -> (inferTypeOfLambda fcx cx v0)
  Core.FunctionPrimitive v0 -> (inferTypeOfPrimitive fcx cx v0)) f)

-- | Infer the type of a union injection (Either version)
inferTypeOfInjection :: (Context.Context -> Graph.Graph -> Core.Injection -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfInjection fcx cx injection =  
  let tname = (Core.injectionTypeName injection)
  in  
    let field = (Core.injectionField injection)
    in  
      let fname = (Core.fieldName field)
      in  
        let term = (Core.fieldTerm field)
        in (Eithers.bind (inferTypeOfTerm fcx cx term "injected term") (\result ->  
          let fcx2 = (Typing_.inferenceResultContext result)
          in (Eithers.bind (Schemas.requireSchemaType fcx2 (Graph.graphSchemaTypes cx) tname) (\stRp ->  
            let schemaType = (Pairs.first stRp)
            in  
              let fcx3 = (Pairs.second stRp)
              in  
                let svars = (Core.typeSchemeVariables schemaType)
                in  
                  let stype = (Core.typeSchemeType schemaType)
                  in  
                    let iterm = (Typing_.inferenceResultTerm result)
                    in  
                      let ityp = (Typing_.inferenceResultType result)
                      in  
                        let isubst = (Typing_.inferenceResultSubst result)
                        in (Eithers.bind (Core_.unionType fcx3 tname stype) (\sfields -> Eithers.bind (Schemas.findFieldType fcx3 fname sfields) (\ftyp -> Eithers.bind (mapConstraints fcx3 cx (\subst -> yield fcx3 (buildTypeApplicationTerm svars (Core.TermUnion (Core.Injection {
                          Core.injectionTypeName = tname,
                          Core.injectionField = Core.Field {
                            Core.fieldName = fname,
                            Core.fieldTerm = iterm}}))) (Schemas.nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)) (Substitution.composeTypeSubst isubst subst)) [
                          Typing_.TypeConstraint {
                            Typing_.typeConstraintLeft = ftyp,
                            Typing_.typeConstraintRight = ityp,
                            Typing_.typeConstraintComment = "schema type of injected field"}]) (\mcResult -> Right mcResult))))))))

-- | Infer the type of a lambda function (Either version)
inferTypeOfLambda :: (Context.Context -> Graph.Graph -> Core.Lambda -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfLambda fcx cx lambda =  
  let var = (Core.lambdaParameter lambda)
  in  
    let body = (Core.lambdaBody lambda)
    in  
      let vdomResult = (Schemas.freshName fcx)
      in  
        let vdom = (Pairs.first vdomResult)
        in  
          let fcx2 = (Pairs.second vdomResult)
          in  
            let dom = (Core.TypeVariable vdom)
            in  
              let cx2 = (extendContext [
                      (var, Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = dom,
                        Core.typeSchemeConstraints = Nothing})] cx)
              in (Eithers.bind (inferTypeOfTerm fcx2 cx2 body "lambda body") (\result ->  
                let fcx3 = (Typing_.inferenceResultContext result)
                in  
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
                                      (Rewriting.freeVariablesInType icod),
                                      (freeVariablesInContext (Substitution.substInContext isubst cx2))])
                              in  
                                let cx3 = (Substitution.substInContext isubst cx)
                                in  
                                  let iconstraints = (Substitution.substInClassConstraints isubst (Typing_.inferenceResultClassConstraints result))
                                  in (Right (Typing_.InferenceResult {
                                    Typing_.inferenceResultTerm = rterm,
                                    Typing_.inferenceResultType = rtype,
                                    Typing_.inferenceResultSubst = isubst,
                                    Typing_.inferenceResultClassConstraints = iconstraints,
                                    Typing_.inferenceResultContext = fcx3}))))

-- | Normalize a let term before inferring its type (Either version)
inferTypeOfLet :: (Context.Context -> Graph.Graph -> Core.Let -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfLet fcx0 cx let0 =  
  let fcx = Context.Context {
          Context.contextTrace = (Lists.cons "let" (Context.contextTrace fcx0)),
          Context.contextMessages = (Context.contextMessages fcx0),
          Context.contextOther = (Context.contextOther fcx0)}
  in  
    let bindings0 = (Core.letBindings let0)
    in  
      let body0 = (Core.letBody let0)
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
                            Core.letBindings = (Maybes.cat (Lists.map (\n -> Maps.lookup n bindingMap) group)),
                            Core.letBody = e}))
                    in  
                      let rewrittenLet = (Lists.foldl createLet body0 (Lists.reverse groups))
                      in  
                        let restoreLet = (\iterm ->  
                                let helper = (\level -> \bins -> \term ->  
                                        let nonzero = (\term -> (\x -> case x of
                                                Core.TermLet v0 ->  
                                                  let bs = (Core.letBindings v0)
                                                  in  
                                                    let letBody = (Core.letBody v0)
                                                    in (helper (Math.sub level 1) (Lists.concat [
                                                      bs,
                                                      bins]) letBody)) term)
                                        in (Logic.ifElse (Equality.equal level 0) (bins, term) (nonzero term)))
                                in  
                                  let result = (helper (Lists.length groups) [] iterm)
                                  in  
                                    let bindingList = (Pairs.first result)
                                    in  
                                      let e = (Pairs.second result)
                                      in  
                                        let bindingMap2 = (Maps.fromList (Lists.map (\b -> (Core.bindingName b, b)) bindingList))
                                        in (Core.TermLet (Core.Let {
                                          Core.letBindings = (Maybes.cat (Lists.map (\n -> Maps.lookup n bindingMap2) names)),
                                          Core.letBody = e})))
                        in  
                          let rewriteResult = (\iresult ->  
                                  let fcxR = (Typing_.inferenceResultContext iresult)
                                  in  
                                    let iterm = (Typing_.inferenceResultTerm iresult)
                                    in  
                                      let itype = (Typing_.inferenceResultType iresult)
                                      in  
                                        let isubst = (Typing_.inferenceResultSubst iresult)
                                        in  
                                          let iconstraints = (Typing_.inferenceResultClassConstraints iresult)
                                          in Typing_.InferenceResult {
                                            Typing_.inferenceResultTerm = (restoreLet iterm),
                                            Typing_.inferenceResultType = itype,
                                            Typing_.inferenceResultSubst = isubst,
                                            Typing_.inferenceResultClassConstraints = iconstraints,
                                            Typing_.inferenceResultContext = fcxR})
                          in  
                            let res = ((\x -> case x of
                                    Core.TermLet v0 -> (inferTypeOfLetNormalized fcx cx v0)
                                    _ -> (inferTypeOfTerm fcx cx rewrittenLet "empty let term")) rewrittenLet)
                            in (Eithers.map rewriteResult res)

-- | Infer the type of a let (letrec) term which is already in a normal form (Either version)
inferTypeOfLetNormalized :: (Context.Context -> Graph.Graph -> Core.Let -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfLetNormalized fcx0 cx0 letTerm =  
  let fcx = Context.Context {
          Context.contextTrace = (Lists.cons "let-normalized" (Context.contextTrace fcx0)),
          Context.contextMessages = (Context.contextMessages fcx0),
          Context.contextOther = (Context.contextOther fcx0)}
  in  
    let bins0 = (Core.letBindings letTerm)
    in  
      let body0 = (Core.letBody letTerm)
      in  
        let bnames = (Lists.map Core.bindingName bins0)
        in  
          let bvarsResult = (Schemas.freshNames (Lists.length bins0) fcx)
          in  
            let bvars = (Pairs.first bvarsResult)
            in  
              let fcx2 = (Pairs.second bvarsResult)
              in  
                let tbins0 = (Lists.map (\x -> Core.TypeVariable x) bvars)
                in  
                  let cx1 = (extendContext (Lists.zip bnames (Lists.map (\t -> Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeType = t,
                          Core.typeSchemeConstraints = Nothing}) tbins0)) cx0)
                  in (Eithers.bind (inferTypesOfTemporaryBindings fcx2 cx1 bins0) (\irRp ->  
                    let inferredResult = (Pairs.first irRp)
                    in  
                      let fcx3 = (Pairs.second irRp)
                      in  
                        let bterms1 = (Pairs.first inferredResult)
                        in  
                          let tbins1 = (Pairs.first (Pairs.second inferredResult))
                          in  
                            let substAndConstraints = (Pairs.second (Pairs.second inferredResult))
                            in  
                              let s1 = (Pairs.first substAndConstraints)
                              in  
                                let inferredConstraints = (Pairs.second substAndConstraints)
                                in (Eithers.bind (Eithers.bimap (\_ic -> Context.InContext {
                                  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unificationErrorMessage (Context.inContextObject _ic)))),
                                  Context.inContextContext = (Context.inContextContext _ic)}) (\_a -> _a) (Unification.unifyTypeLists fcx3 (Graph.graphSchemaTypes cx0) (Lists.map (Substitution.substInType s1) tbins0) tbins1 "temporary type bindings")) (\s2 -> Eithers.bind (Checking.checkTypeSubst fcx3 cx0 s2) (\_ ->  
                                  let g2base = (Substitution.substInContext (Substitution.composeTypeSubst s1 s2) cx0)
                                  in  
                                    let constraintsWithS2 = (Substitution.substInClassConstraints s2 inferredConstraints)
                                    in  
                                      let composedSubst = (Substitution.composeTypeSubst s1 s2)
                                      in  
                                        let originalBindingConstraints = (Lists.foldl (\acc -> \b -> Maybes.maybe acc (\ts -> Maybes.maybe acc (\c -> mergeClassConstraints acc c) (Core.typeSchemeConstraints ts)) (Core.bindingType b)) Maps.empty bins0)
                                        in  
                                          let originalConstraintsSubst = (Substitution.substInClassConstraints composedSubst originalBindingConstraints)
                                          in  
                                            let allInferredConstraints = (mergeClassConstraints constraintsWithS2 originalConstraintsSubst)
                                            in  
                                              let mergedConstraints = (mergeClassConstraints (Graph.graphClassConstraints g2base) allInferredConstraints)
                                              in  
                                                let g2 = Graph.Graph {
                                                        Graph.graphBoundTerms = (Graph.graphBoundTerms g2base),
                                                        Graph.graphBoundTypes = (Graph.graphBoundTypes g2base),
                                                        Graph.graphClassConstraints = mergedConstraints,
                                                        Graph.graphLambdaVariables = (Graph.graphLambdaVariables g2base),
                                                        Graph.graphMetadata = (Graph.graphMetadata g2base),
                                                        Graph.graphPrimitives = (Graph.graphPrimitives g2base),
                                                        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g2base),
                                                        Graph.graphTypeVariables = (Graph.graphTypeVariables g2base)}
                                                in  
                                                  let bterms1Subst = (Lists.map (Substitution.substTypesInTerm s2) bterms1)
                                                  in  
                                                    let tsbins1 = (Lists.zip bnames (Lists.map (\t -> generalize g2 (Substitution.substInType s2 t)) tbins1))
                                                    in (Eithers.bind (inferTypeOfTerm fcx3 (extendContext tsbins1 g2) body0 "let body") (\bodyResult ->  
                                                      let fcx4 = (Typing_.inferenceResultContext bodyResult)
                                                      in  
                                                        let body1 = (Typing_.inferenceResultTerm bodyResult)
                                                        in  
                                                          let tbody = (Typing_.inferenceResultType bodyResult)
                                                          in  
                                                            let sbody = (Typing_.inferenceResultSubst bodyResult)
                                                            in  
                                                              let st1 = (Typing_.TermSubst (Maps.fromList (Lists.map (\pair ->  
                                                                      let name = (Pairs.first pair)
                                                                      in  
                                                                        let ts = (Pairs.second pair)
                                                                        in (name, (buildTypeApplicationTerm (Core.typeSchemeVariables ts) (Core.TermVariable name)))) tsbins1)))
                                                              in  
                                                                let createBinding = (\bindingPair ->  
                                                                        let nameTsPair = (Pairs.first bindingPair)
                                                                        in  
                                                                          let term = (Pairs.second bindingPair)
                                                                          in  
                                                                            let name = (Pairs.first nameTsPair)
                                                                            in  
                                                                              let ts = (Pairs.second nameTsPair)
                                                                              in  
                                                                                let finalTs = (Substitution.substInTypeScheme sbody ts)
                                                                                in  
                                                                                  let typeLambdaTerm = (Lists.foldl (\b -> \v -> Core.TermTypeLambda (Core.TypeLambda {
                                                                                          Core.typeLambdaParameter = v,
                                                                                          Core.typeLambdaBody = b})) (Substitution.substituteInTerm st1 term) (Lists.reverse (Core.typeSchemeVariables finalTs)))
                                                                                  in Core.Binding {
                                                                                    Core.bindingName = name,
                                                                                    Core.bindingTerm = (Substitution.substTypesInTerm (Substitution.composeTypeSubst sbody s2) typeLambdaTerm),
                                                                                    Core.bindingType = (Just finalTs)})
                                                                in  
                                                                  let bins1 = (Lists.map createBinding (Lists.zip tsbins1 bterms1Subst))
                                                                  in  
                                                                    let bodyConstraints = (Substitution.substInClassConstraints sbody (Typing_.inferenceResultClassConstraints bodyResult))
                                                                    in  
                                                                      let bindingConstraintsSubst = (Substitution.substInClassConstraints sbody constraintsWithS2)
                                                                      in  
                                                                        let allConstraints = (mergeClassConstraints bindingConstraintsSubst bodyConstraints)
                                                                        in (Right (Typing_.InferenceResult {
                                                                          Typing_.inferenceResultTerm = (Core.TermLet (Core.Let {
                                                                            Core.letBindings = bins1,
                                                                            Core.letBody = body1})),
                                                                          Typing_.inferenceResultType = tbody,
                                                                          Typing_.inferenceResultSubst = (Substitution.composeTypeSubstList [
                                                                            s1,
                                                                            s2,
                                                                            sbody]),
                                                                          Typing_.inferenceResultClassConstraints = allConstraints,
                                                                          Typing_.inferenceResultContext = fcx4})))))))))

-- | Infer the type of a list (Either version)
inferTypeOfList :: (Context.Context -> Graph.Graph -> [Core.Term] -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfList fcx cx = (inferTypeOfCollection fcx cx (\x -> Core.TypeList x) (\x -> Core.TermList x) "list element" Sets.empty)

-- | Infer the type of a literal
inferTypeOfLiteral :: (Context.Context -> Core.Literal -> Typing_.InferenceResult)
inferTypeOfLiteral fcx lit = Typing_.InferenceResult {
  Typing_.inferenceResultTerm = (Core.TermLiteral lit),
  Typing_.inferenceResultType = (Core.TypeLiteral (Reflect.literalType lit)),
  Typing_.inferenceResultSubst = Substitution.idTypeSubst,
  Typing_.inferenceResultClassConstraints = Maps.empty,
  Typing_.inferenceResultContext = fcx}

-- | Infer the type of a map (Either version)
inferTypeOfMap :: (Context.Context -> Graph.Graph -> M.Map Core.Term Core.Term -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfMap fcx cx m =  
  let kvarResult = (Schemas.freshName fcx)
  in  
    let kvar = (Pairs.first kvarResult)
    in  
      let fcx2 = (Pairs.second kvarResult)
      in  
        let vvarResult = (Schemas.freshName fcx2)
        in  
          let vvar = (Pairs.first vvarResult)
          in  
            let fcx3 = (Pairs.second vvarResult)
            in  
              let keyConstraints = (Maps.singleton kvar (Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (Sets.singleton (Core.Name "ordering"))}))
              in (Logic.ifElse (Maps.null m) (Right (yieldWithConstraints fcx3 (buildTypeApplicationTerm [
                kvar,
                vvar] (Core.TermMap Maps.empty)) (Core.TypeMap (Core.MapType {
                Core.mapTypeKeys = (Core.TypeVariable kvar),
                Core.mapTypeValues = (Core.TypeVariable vvar)})) Substitution.idTypeSubst keyConstraints)) (Eithers.bind (inferMany fcx3 cx (Lists.map (\k -> (k, "map key")) (Maps.keys m))) (\kRp ->  
                let kResults = (Pairs.first kRp)
                in  
                  let fcx4 = (Pairs.second kRp)
                  in  
                    let kterms = (Pairs.first kResults)
                    in  
                      let ktypes = (Pairs.first (Pairs.second kResults))
                      in  
                        let ksubst = (Pairs.first (Pairs.second (Pairs.second kResults)))
                        in  
                          let kElemConstraints = (Pairs.second (Pairs.second (Pairs.second kResults)))
                          in (Eithers.bind (inferMany fcx4 (Substitution.substInContext ksubst cx) (Lists.map (\v -> (v, "map value")) (Maps.elems m))) (\vRp ->  
                            let vResults = (Pairs.first vRp)
                            in  
                              let fcx5 = (Pairs.second vRp)
                              in  
                                let vterms = (Pairs.first vResults)
                                in  
                                  let vtypes = (Pairs.first (Pairs.second vResults))
                                  in  
                                    let vsubst = (Pairs.first (Pairs.second (Pairs.second vResults)))
                                    in  
                                      let vElemConstraints = (Pairs.second (Pairs.second (Pairs.second vResults)))
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
                                          in  
                                            let allMapConstraints = (mergeClassConstraints keyConstraints (mergeClassConstraints kElemConstraints vElemConstraints))
                                            in (Eithers.bind (mapConstraints fcx5 cx (\subst -> yieldWithConstraints fcx5 (Core.TermMap (Maps.fromList (Lists.zip kterms vterms))) (Core.TypeMap (Core.MapType {
                                              Core.mapTypeKeys = (Core.TypeVariable kvar),
                                              Core.mapTypeValues = (Core.TypeVariable vvar)})) (Substitution.composeTypeSubstList [
                                              ksubst,
                                              vsubst,
                                              subst]) (Substitution.substInClassConstraints subst allMapConstraints)) (Lists.concat [
                                              kcons,
                                              vcons])) (\mcResult -> Right mcResult)))))))

-- | Infer the type of an optional (Either version)
inferTypeOfOptional :: (Context.Context -> Graph.Graph -> Maybe Core.Term -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfOptional fcx cx m =  
  let trmCons = (\terms -> Logic.ifElse (Lists.null terms) (Core.TermMaybe Nothing) (Core.TermMaybe (Just (Lists.head terms))))
  in (inferTypeOfCollection fcx cx (\x -> Core.TypeMaybe x) trmCons "optional element" Sets.empty (Maybes.maybe [] Lists.singleton m))

-- | Infer the type of a pair (Either version)
inferTypeOfPair :: (Context.Context -> Graph.Graph -> (Core.Term, Core.Term) -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfPair fcx cx p = (Eithers.bind (inferMany fcx cx [
  (Pairs.first p, "pair first element"),
  (Pairs.second p, "pair second element")]) (\rp ->  
  let results = (Pairs.first rp)
  in  
    let fcx2 = (Pairs.second rp)
    in  
      let iterms = (Pairs.first results)
      in  
        let itypes = (Pairs.first (Pairs.second results))
        in  
          let isubst = (Pairs.first (Pairs.second (Pairs.second results)))
          in  
            let pairElemConstraints = (Pairs.second (Pairs.second (Pairs.second results)))
            in  
              let ifst = (Lists.head iterms)
              in  
                let isnd = (Lists.head (Lists.tail iterms))
                in  
                  let tyFst = (Lists.head itypes)
                  in  
                    let tySnd = (Lists.head (Lists.tail itypes))
                    in  
                      let pairTerm = (Core.TermPair (ifst, isnd))
                      in  
                        let termWithTypes = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = pairTerm,
                                  Core.typeApplicationTermType = tyFst})),
                                Core.typeApplicationTermType = tySnd}))
                        in (Right (yieldWithConstraints fcx2 termWithTypes (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = tyFst,
                          Core.pairTypeSecond = tySnd})) isubst pairElemConstraints))))

-- | Infer the type of a primitive function (Either version)
inferTypeOfPrimitive :: (Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfPrimitive fcx cx name = (Maybes.maybe (Left (Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "No such primitive: " (Core.unName name)))),
  Context.inContextContext = fcx})) (\scheme ->  
  let tsResult = (Schemas.instantiateTypeScheme fcx scheme)
  in  
    let ts = (Pairs.first tsResult)
    in  
      let fcx2 = (Pairs.second tsResult)
      in  
        let constraints = (Maybes.fromMaybe Maps.empty (Core.typeSchemeConstraints ts))
        in (Right (yieldCheckedWithConstraints fcx2 (buildTypeApplicationTerm (Core.typeSchemeVariables ts) (Core.TermFunction (Core.FunctionPrimitive name))) (Core.typeSchemeType ts) Substitution.idTypeSubst constraints))) (Maybes.map Graph.primitiveType (Maps.lookup name (Graph.graphPrimitives cx))))

-- | Infer the type of a record projection (Either version)
inferTypeOfProjection :: (Context.Context -> Graph.Graph -> Core.Projection -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfProjection fcx cx proj =  
  let tname = (Core.projectionTypeName proj)
  in  
    let fname = (Core.projectionField proj)
    in (Eithers.bind (Schemas.requireSchemaType fcx (Graph.graphSchemaTypes cx) tname) (\stRp ->  
      let schemaType = (Pairs.first stRp)
      in  
        let fcx2 = (Pairs.second stRp)
        in  
          let svars = (Core.typeSchemeVariables schemaType)
          in  
            let stype = (Core.typeSchemeType schemaType)
            in (Eithers.bind (Core_.recordType fcx2 tname stype) (\sfields -> Eithers.bind (Schemas.findFieldType fcx2 fname sfields) (\ftyp -> Right (yield fcx2 (buildTypeApplicationTerm svars (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = tname,
              Core.projectionField = fname}))))) (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Schemas.nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)),
              Core.functionTypeCodomain = ftyp})) Substitution.idTypeSubst))))))

-- | Infer the type of a record (Either version)
inferTypeOfRecord :: (Context.Context -> Graph.Graph -> Core.Record -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfRecord fcx cx record =  
  let tname = (Core.recordTypeName record)
  in  
    let fields = (Core.recordFields record)
    in  
      let fnames = (Lists.map Core.fieldName fields)
      in (Eithers.bind (Schemas.requireSchemaType fcx (Graph.graphSchemaTypes cx) tname) (\stRp ->  
        let schemaType = (Pairs.first stRp)
        in  
          let fcx2 = (Pairs.second stRp)
          in (Eithers.bind (inferMany fcx2 cx (Lists.map (\f -> (Core.fieldTerm f, (Strings.cat2 "field " (Core.unName (Core.fieldName f))))) fields)) (\rp ->  
            let results = (Pairs.first rp)
            in  
              let fcx3 = (Pairs.second rp)
              in  
                let svars = (Core.typeSchemeVariables schemaType)
                in  
                  let stype = (Core.typeSchemeType schemaType)
                  in  
                    let iterms = (Pairs.first results)
                    in  
                      let itypes = (Pairs.first (Pairs.second results))
                      in  
                        let isubst = (Pairs.first (Pairs.second (Pairs.second results)))
                        in  
                          let recElemConstraints = (Pairs.second (Pairs.second (Pairs.second results)))
                          in  
                            let ityp = (Core.TypeRecord (Core.RowType {
                                    Core.rowTypeTypeName = tname,
                                    Core.rowTypeFields = (Lists.zipWith (\n -> \t -> Core.FieldType {
                                      Core.fieldTypeName = n,
                                      Core.fieldTypeType = t}) fnames itypes)}))
                            in (Eithers.bind (mapConstraints fcx3 cx (\subst -> yieldWithConstraints fcx3 (buildTypeApplicationTerm svars (Core.TermRecord (Core.Record {
                              Core.recordTypeName = tname,
                              Core.recordFields = (Lists.zipWith (\n -> \t -> Core.Field {
                                Core.fieldName = n,
                                Core.fieldTerm = t}) fnames iterms)}))) (Schemas.nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)) (Substitution.composeTypeSubst isubst subst) (Substitution.substInClassConstraints subst recElemConstraints)) [
                              Typing_.TypeConstraint {
                                Typing_.typeConstraintLeft = stype,
                                Typing_.typeConstraintRight = ityp,
                                Typing_.typeConstraintComment = "schema type of record"}]) (\mcResult -> Right mcResult))))))

-- | Infer the type of a set (Either version)
inferTypeOfSet :: (Context.Context -> Graph.Graph -> S.Set Core.Term -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfSet fcx cx s = (inferTypeOfCollection fcx cx (\x -> Core.TypeSet x) (\terms -> Core.TermSet (Sets.fromList terms)) "set element" (Sets.singleton (Core.Name "ordering")) (Sets.toList s))

-- | Infer the type of a given term (Either version)
inferTypeOfTerm :: (Context.Context -> Graph.Graph -> Core.Term -> String -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfTerm fcx cx term desc =  
  let fcx2 = Context.Context {
          Context.contextTrace = (Lists.cons desc (Context.contextTrace fcx)),
          Context.contextMessages = (Context.contextMessages fcx),
          Context.contextOther = (Context.contextOther fcx)}
  in ((\x -> case x of
    Core.TermAnnotated v0 -> (inferTypeOfAnnotatedTerm fcx2 cx v0)
    Core.TermApplication v0 -> (inferTypeOfApplication fcx2 cx v0)
    Core.TermEither v0 -> (inferTypeOfEither fcx2 cx v0)
    Core.TermFunction v0 -> (inferTypeOfFunction fcx2 cx v0)
    Core.TermLet v0 -> (inferTypeOfLet fcx2 cx v0)
    Core.TermList v0 -> (inferTypeOfList fcx2 cx v0)
    Core.TermLiteral v0 -> (Right (inferTypeOfLiteral fcx2 v0))
    Core.TermMap v0 -> (inferTypeOfMap fcx2 cx v0)
    Core.TermMaybe v0 -> (inferTypeOfOptional fcx2 cx v0)
    Core.TermPair v0 -> (inferTypeOfPair fcx2 cx v0)
    Core.TermRecord v0 -> (inferTypeOfRecord fcx2 cx v0)
    Core.TermSet v0 -> (inferTypeOfSet fcx2 cx v0)
    Core.TermTypeApplication v0 -> (inferTypeOfTypeApplication fcx2 cx v0)
    Core.TermTypeLambda v0 -> (inferTypeOfTypeLambda fcx2 cx v0)
    Core.TermUnion v0 -> (inferTypeOfInjection fcx2 cx v0)
    Core.TermUnit -> (Right (inferTypeOfUnit fcx2))
    Core.TermVariable v0 -> (inferTypeOfVariable fcx2 cx v0)
    Core.TermWrap v0 -> (inferTypeOfWrappedTerm fcx2 cx v0)) term)

-- | Infer the type of a type abstraction (Either version)
inferTypeOfTypeLambda :: (Context.Context -> Graph.Graph -> Core.TypeLambda -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfTypeLambda fcx cx ta = (inferTypeOfTerm fcx cx (Core.typeLambdaBody ta) "type abstraction")

-- | Infer the type of a type application (Either version)
inferTypeOfTypeApplication :: (Context.Context -> Graph.Graph -> Core.TypeApplicationTerm -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfTypeApplication fcx cx tt = (inferTypeOfTerm fcx cx (Core.typeApplicationTermBody tt) "type application term")

-- | The trivial inference rule for the unit term
inferTypeOfUnit :: (Context.Context -> Typing_.InferenceResult)
inferTypeOfUnit fcx = Typing_.InferenceResult {
  Typing_.inferenceResultTerm = Core.TermUnit,
  Typing_.inferenceResultType = Core.TypeUnit,
  Typing_.inferenceResultSubst = Substitution.idTypeSubst,
  Typing_.inferenceResultClassConstraints = Maps.empty,
  Typing_.inferenceResultContext = fcx}

-- | Infer the type of an unwrap operation (Either version)
inferTypeOfUnwrap :: (Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfUnwrap fcx cx tname = (Eithers.bind (Schemas.requireSchemaType fcx (Graph.graphSchemaTypes cx) tname) (\stRp ->  
  let schemaType = (Pairs.first stRp)
  in  
    let fcx2 = (Pairs.second stRp)
    in  
      let svars = (Core.typeSchemeVariables schemaType)
      in  
        let stype = (Core.typeSchemeType schemaType)
        in (Eithers.bind (Core_.wrappedType fcx2 tname stype) (\wtyp -> Right (yield fcx2 (buildTypeApplicationTerm svars (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap tname)))) (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Schemas.nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)),
          Core.functionTypeCodomain = wtyp})) Substitution.idTypeSubst)))))

-- | Infer the type of a variable (Either version)
inferTypeOfVariable :: (Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfVariable fcx cx name = (Maybes.maybe (Left (Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Variable not bound to type: " (Core.unName name)))),
  Context.inContextContext = fcx})) (\scheme ->  
  let tsResult = (Schemas.instantiateTypeScheme fcx scheme)
  in  
    let ts = (Pairs.first tsResult)
    in  
      let fcx2 = (Pairs.second tsResult)
      in  
        let constraints = (Maybes.fromMaybe Maps.empty (Core.typeSchemeConstraints ts))
        in (Right (Typing_.InferenceResult {
          Typing_.inferenceResultTerm = (buildTypeApplicationTerm (Core.typeSchemeVariables ts) (Core.TermVariable name)),
          Typing_.inferenceResultType = (Core.typeSchemeType ts),
          Typing_.inferenceResultSubst = Substitution.idTypeSubst,
          Typing_.inferenceResultClassConstraints = constraints,
          Typing_.inferenceResultContext = fcx2}))) (Maps.lookup name (Graph.graphBoundTypes cx)))

-- | Infer the type of a wrapped term (Either version)
inferTypeOfWrappedTerm :: (Context.Context -> Graph.Graph -> Core.WrappedTerm -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
inferTypeOfWrappedTerm fcx cx wt =  
  let tname = (Core.wrappedTermTypeName wt)
  in  
    let term = (Core.wrappedTermBody wt)
    in (Eithers.bind (Schemas.requireSchemaType fcx (Graph.graphSchemaTypes cx) tname) (\stRp ->  
      let schemaType = (Pairs.first stRp)
      in  
        let fcx2 = (Pairs.second stRp)
        in (Eithers.bind (inferTypeOfTerm fcx2 cx term "wrapped term") (\result ->  
          let fcx3 = (Typing_.inferenceResultContext result)
          in  
            let svars = (Core.typeSchemeVariables schemaType)
            in  
              let stype = (Core.typeSchemeType schemaType)
              in  
                let iterm = (Typing_.inferenceResultTerm result)
                in  
                  let itype = (Typing_.inferenceResultType result)
                  in  
                    let isubst = (Typing_.inferenceResultSubst result)
                    in  
                      let ityp = (Core.TypeWrap (Core.WrappedType {
                              Core.wrappedTypeTypeName = tname,
                              Core.wrappedTypeBody = itype}))
                      in (Eithers.bind (mapConstraints fcx3 cx (\subst -> yield fcx3 (buildTypeApplicationTerm svars (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = tname,
                        Core.wrappedTermBody = iterm}))) (Schemas.nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)) (Substitution.composeTypeSubst isubst subst)) [
                        Typing_.TypeConstraint {
                          Typing_.typeConstraintLeft = stype,
                          Typing_.typeConstraintRight = ityp,
                          Typing_.typeConstraintComment = "schema type of wrapper"}]) (\mcResult -> Right mcResult))))))

-- | Infer types for temporary let bindings (Either version)
inferTypesOfTemporaryBindings :: (Context.Context -> Graph.Graph -> [Core.Binding] -> Either (Context.InContext Error.Error) (([Core.Term], ([Core.Type], (Typing_.TypeSubst, (M.Map Core.Name Core.TypeVariableMetadata)))), Context.Context))
inferTypesOfTemporaryBindings fcx cx bins =  
  let dflt =  
          let binding = (Lists.head bins)
          in  
            let k = (Core.bindingName binding)
            in  
              let v = (Core.bindingTerm binding)
              in  
                let tl = (Lists.tail bins)
                in (Eithers.bind (inferTypeOfTerm fcx cx v (Strings.cat [
                  "temporary let binding '",
                  (Core.unName k),
                  "'"])) (\result1 ->  
                  let fcx2 = (Typing_.inferenceResultContext result1)
                  in  
                    let j = (Typing_.inferenceResultTerm result1)
                    in  
                      let u_prime = (Typing_.inferenceResultType result1)
                      in  
                        let u = (Typing_.inferenceResultSubst result1)
                        in  
                          let c1Inferred = (Typing_.inferenceResultClassConstraints result1)
                          in (Eithers.bind (Maybes.maybe (Right Maps.empty) (\ts ->  
                            let tsResult = (Schemas.instantiateTypeScheme fcx2 ts)
                            in  
                              let instantiatedTs = (Pairs.first tsResult)
                              in  
                                let freshConstraints = (Maybes.fromMaybe Maps.empty (Core.typeSchemeConstraints instantiatedTs))
                                in (Eithers.bind (Eithers.bimap (\_ic -> Context.InContext {
                                  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unificationErrorMessage (Context.inContextObject _ic)))),
                                  Context.inContextContext = (Context.inContextContext _ic)}) (\_a -> _a) (Unification.unifyTypes fcx2 (Graph.graphSchemaTypes cx) (Core.typeSchemeType instantiatedTs) u_prime "original binding type")) (\unifySubst -> Right (Substitution.substInClassConstraints unifySubst freshConstraints)))) (Core.bindingType binding)) (\originalBindingConstraints ->  
                            let c1 = (mergeClassConstraints c1Inferred originalBindingConstraints)
                            in (Eithers.bind (inferTypesOfTemporaryBindings fcx2 (Substitution.substInContext u cx) tl) (\rp2 ->  
                              let result2 = (Pairs.first rp2)
                              in  
                                let fcx3 = (Pairs.second rp2)
                                in  
                                  let h = (Pairs.first result2)
                                  in  
                                    let r_prime = (Pairs.first (Pairs.second result2))
                                    in  
                                      let restPair = (Pairs.second (Pairs.second result2))
                                      in  
                                        let r = (Pairs.first restPair)
                                        in  
                                          let c2 = (Pairs.second restPair)
                                          in  
                                            let c1Subst = (Substitution.substInClassConstraints r c1)
                                            in  
                                              let mergedConstraints = (mergeClassConstraints c1Subst c2)
                                              in (Right ((Lists.cons (Substitution.substTypesInTerm r j) h, (Lists.cons (Substitution.substInType r u_prime) r_prime, (Substitution.composeTypeSubst u r, mergedConstraints))), fcx3))))))))
  in (Logic.ifElse (Lists.null bins) (Right (([], ([], (Substitution.idTypeSubst, Maps.empty))), fcx)) dflt)

-- | Check if a variable is unbound in context
isUnbound :: (Graph.Graph -> Core.Name -> Bool)
isUnbound cx v = (Logic.and (Logic.not (Sets.member v (freeVariablesInContext cx))) (Logic.not (Maps.member v (Graph.graphSchemaTypes cx))))

-- | Map over type constraints after unification
mapConstraints :: (Context.Context -> Graph.Graph -> (Typing_.TypeSubst -> t0) -> [Typing_.TypeConstraint] -> Either (Context.InContext Error.Error) t0)
mapConstraints flowCx cx f constraints = (Eithers.bind (Eithers.bimap (\_ic -> Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unificationErrorMessage (Context.inContextObject _ic)))),
  Context.inContextContext = (Context.inContextContext _ic)}) (\_a -> _a) (Unification.unifyTypeConstraints flowCx (Graph.graphSchemaTypes cx) constraints)) (\s -> Eithers.bind (Checking.checkTypeSubst flowCx cx s) (\_ -> Right (f s))))

-- | Merge two maps of class constraints. When both maps have constraints for the same variable, union the class sets.
mergeClassConstraints :: Ord t0 => (M.Map t0 Core.TypeVariableMetadata -> M.Map t0 Core.TypeVariableMetadata -> M.Map t0 Core.TypeVariableMetadata)
mergeClassConstraints m1 m2 = (Lists.foldl (\acc -> \pair ->  
  let k = (Pairs.first pair)
  in  
    let v = (Pairs.second pair)
    in (Maybes.maybe (Maps.insert k v acc) (\existing ->  
      let merged = Core.TypeVariableMetadata {
              Core.typeVariableMetadataClasses = (Sets.union (Core.typeVariableMetadataClasses existing) (Core.typeVariableMetadataClasses v))}
      in (Maps.insert k merged acc)) (Maps.lookup k acc))) m1 (Maps.toList m2))

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
        (Core__.term term),
        ", type=",
        (Core__.type_ typ),
        ", subst=",
        (Typing.typeSubst subst),
        "}"])

-- | Create an inference result with no class constraints
yield :: (Context.Context -> Core.Term -> Core.Type -> Typing_.TypeSubst -> Typing_.InferenceResult)
yield fcx term typ subst = Typing_.InferenceResult {
  Typing_.inferenceResultTerm = (Substitution.substTypesInTerm subst term),
  Typing_.inferenceResultType = (Substitution.substInType subst typ),
  Typing_.inferenceResultSubst = subst,
  Typing_.inferenceResultClassConstraints = Maps.empty,
  Typing_.inferenceResultContext = fcx}

-- | Create a checked inference result
yieldChecked :: (Context.Context -> Core.Term -> Core.Type -> Typing_.TypeSubst -> Typing_.InferenceResult)
yieldChecked fcx term typ subst =  
  let iterm = (Substitution.substTypesInTerm subst term)
  in  
    let itype = (Substitution.substInType subst typ)
    in Typing_.InferenceResult {
      Typing_.inferenceResultTerm = iterm,
      Typing_.inferenceResultType = itype,
      Typing_.inferenceResultSubst = subst,
      Typing_.inferenceResultClassConstraints = Maps.empty,
      Typing_.inferenceResultContext = fcx}

-- | Create a checked inference result with class constraints
yieldCheckedWithConstraints :: (Context.Context -> Core.Term -> Core.Type -> Typing_.TypeSubst -> M.Map Core.Name Core.TypeVariableMetadata -> Typing_.InferenceResult)
yieldCheckedWithConstraints fcx term typ subst constraints =  
  let iterm = (Substitution.substTypesInTerm subst term)
  in  
    let itype = (Substitution.substInType subst typ)
    in  
      let iconstraints = (Substitution.substInClassConstraints subst constraints)
      in Typing_.InferenceResult {
        Typing_.inferenceResultTerm = iterm,
        Typing_.inferenceResultType = itype,
        Typing_.inferenceResultSubst = subst,
        Typing_.inferenceResultClassConstraints = iconstraints,
        Typing_.inferenceResultContext = fcx}

-- | Create an inference result with debug output
yieldDebug :: (Context.Context -> t0 -> String -> Core.Term -> Core.Type -> Typing_.TypeSubst -> Either (Context.InContext Error.Error) Typing_.InferenceResult)
yieldDebug fcx cx debugId term typ subst =  
  let rterm = (Substitution.substTypesInTerm subst term)
  in  
    let rtyp = (Substitution.substInType subst typ)
    in (Eithers.bind (Annotations.debugIf fcx debugId (Strings.cat [
      "\n\tterm: ",
      (Core__.term term),
      "\n\ttyp: ",
      (Core__.type_ typ),
      "\n\tsubst: ",
      (Typing.typeSubst subst),
      "\n\trterm: ",
      (Core__.term rterm),
      "\n\trtyp: ",
      (Core__.type_ rtyp)])) (\result -> Right (Typing_.InferenceResult {
      Typing_.inferenceResultTerm = rterm,
      Typing_.inferenceResultType = rtyp,
      Typing_.inferenceResultSubst = subst,
      Typing_.inferenceResultClassConstraints = Maps.empty,
      Typing_.inferenceResultContext = fcx})))

-- | Create an inference result with class constraints
yieldWithConstraints :: (Context.Context -> Core.Term -> Core.Type -> Typing_.TypeSubst -> M.Map Core.Name Core.TypeVariableMetadata -> Typing_.InferenceResult)
yieldWithConstraints fcx term typ subst constraints = Typing_.InferenceResult {
  Typing_.inferenceResultTerm = (Substitution.substTypesInTerm subst term),
  Typing_.inferenceResultType = (Substitution.substInType subst typ),
  Typing_.inferenceResultSubst = subst,
  Typing_.inferenceResultClassConstraints = constraints,
  Typing_.inferenceResultContext = fcx}
