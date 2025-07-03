-- | Type inference following Algorithm W, extended for nominal terms and types

module Hydra.Inference where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
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
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Strip as Strip
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import qualified Hydra.Unification as Unification
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Disable type checking by default, for better performance
debugInference :: Bool
debugInference = True

-- | Key for inference type variable count
key_vcount :: Core.Name
key_vcount = (Core.Name "inferenceTypeVariableCount")

-- | Type variable naming convention follows Haskell: t0, t1, etc.
normalTypeVariable :: (Int -> Core.Name)
normalTypeVariable i = (Core.Name (Strings.cat2 "t" (Literals.showInt32 i)))

freshName :: (Compute.Flow t0 Core.Name)
freshName = (Flows.map normalTypeVariable (Annotations.nextCount key_vcount))

freshNames :: (Int -> Compute.Flow t0 [Core.Name])
freshNames n = (Flows.sequence (Lists.replicate n freshName))

freshVariableType :: (Compute.Flow t0 Core.Type)
freshVariableType = (Flows.map (\x -> Core.TypeVariable x) freshName)

typeOf :: (Typing.InferenceContext -> S.Set Core.Name -> M.Map Core.Name Core.Type -> Core.Term -> Compute.Flow t0 Core.Type)
typeOf cx vars types term = (Monads.withTrace (Strings.cat [
  "checking type of: ",
  Core__.term term,
  " (vars: ",
  Formatting.showList Core.unName (Sets.toList vars),
  ", types: ",
  Formatting.showList Core.unName (Maps.keys types),
  ")"]) ((\x -> case x of
  Core.TermAnnotated v1 ->  
    let term1 = (Core.annotatedTermSubject v1)
    in (typeOf cx vars types term1)
  Core.TermApplication v1 ->  
    let a = (Core.applicationFunction v1) 
        b = (Core.applicationArgument v1)
    in (Flows.bind (typeOf cx vars types a) (\t1 -> Flows.bind (typeOf cx vars types b) (\t2 -> Flows.bind (checkTypeVariables vars t1) (\_ -> Flows.bind (checkTypeVariables vars t2) (\_ -> (\x -> case x of
      Core.TypeFunction v2 ->  
        let p = (Core.functionTypeDomain v2) 
            q = (Core.functionTypeCodomain v2)
        in (Logic.ifElse (Equality.equal p t2) (Flows.pure q) (Flows.fail (Strings.cat [
          "expected ",
          Core__.type_ p,
          " in ",
          Core__.term term,
          " but found ",
          (Core__.type_ t2)])))
      Core.TypeVariable _ -> (Flows.fail (Strings.cat [
        "left hand side of application ",
        Core__.term term,
        " is not a function type: ",
        (Core__.type_ t1)]))) t1)))))
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationProduct v3 ->  
        let index = (Core.tupleProjectionIndex v3) 
            arity = (Core.tupleProjectionArity v3)
            mtypes = (Core.tupleProjectionDomain v3)
        in (Optionals.maybe (Flows.fail (Strings.cat [
          "untyped tuple projection: ",
          (Core__.term term)])) (\types -> Flows.bind (Flows.sequence (Lists.map (checkTypeVariables vars) types)) (\_ -> Flows.pure (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.TypeProduct types),
          Core.functionTypeCodomain = (Lists.at index types)})))) mtypes)) v2)
    Core.FunctionLambda v2 ->  
      let x = (Core.lambdaParameter v2) 
          mt = (Core.lambdaDomain v2)
          e = (Core.lambdaBody v2)
      in (Optionals.maybe (Flows.fail (Strings.cat [
        "untyped lambda: ",
        (Core__.term term)])) (\t -> Flows.bind (checkTypeVariables vars t) (\_ -> Flows.bind (typeOf cx vars (Maps.insert x t types) e) (\t1 -> Flows.bind (checkTypeVariables vars t1) (\_ -> Flows.pure (Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = t,
        Core.functionTypeCodomain = t1})))))) mt)
    Core.FunctionPrimitive v2 ->  
      let ts = (Optionals.maybe (Flows.fail (Strings.cat [
              "no such primitive: ",
              (Core.unName v2)])) Flows.pure (Maps.lookup v2 (Typing.inferenceContextPrimitiveTypes cx)))
      in (Flows.map typeSchemeToFType ts)) v1)
  Core.TermLet v1 ->  
    let es = (Core.letBindings v1) 
        e = (Core.letEnvironment v1)
        bnames = (Lists.map Core.letBindingName es)
        bterms = (Lists.map Core.letBindingTerm es)
        binType = (\b -> Optionals.maybe (Flows.fail (Strings.cat [
                "untyped let binding in ",
                (Core__.term term)])) (\ts -> Flows.pure (typeSchemeToFType ts)) (Core.letBindingType b))
    in (Flows.bind (Flows.mapList binType es) (\btypes ->  
      let types2 = (Maps.union (Maps.fromList (Lists.zip bnames btypes)) types)
      in (Flows.bind (Flows.mapList (\v -> typeOf cx vars types2 v) bterms) (\est -> Flows.bind (Flows.sequence (Lists.map (checkTypeVariables vars) est)) (\_ -> Flows.bind (Flows.sequence (Lists.map (checkTypeVariables vars) btypes)) (\_ -> Logic.ifElse (Equality.equal est btypes) (typeOf cx vars types2 e) (Flows.fail (Strings.cat [
        "binding types disagree: ",
        Formatting.showList Core__.type_ est,
        " and ",
        (Formatting.showList Core__.type_ btypes)]))))))))
  Core.TermList v1 -> (Logic.ifElse (Lists.null v1) (Flows.bind freshName (\t ->  
    let var = (Core.TypeVariable t)
    in (Flows.pure (Core.TypeForall (Core.ForallType {
      Core.forallTypeParameter = t,
      Core.forallTypeBody = (Core.TypeList var)}))))) ( 
    let x = (Lists.head v1) 
        xs = (Lists.tail v1)
    in (Flows.bind (Flows.bind (typeOf cx vars types x) instantiateFType) (\tx -> Flows.bind (Flows.sequence (Lists.map (\e -> Flows.bind (Flows.bind (typeOf cx vars types e) instantiateFType) (\t -> Unification.unifyTypes (Typing.inferenceContextSchemaTypes cx) t tx "type check over collection")) xs)) (\_ -> Flows.bind (checkTypeVariables vars tx) (\_ -> Flows.pure (Core.TypeList tx)))))))
  Core.TermLiteral v1 -> (Flows.pure (Core.TypeLiteral (Variants.literalType v1)))
  Core.TermMap v1 -> (Logic.ifElse (Maps.null v1) (Flows.pure (typeSchemeToFType (Core.TypeScheme {
    Core.typeSchemeVariables = [
      Core.Name "k",
      (Core.Name "v")],
    Core.typeSchemeType = (Core.TypeMap (Core.MapType {
      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
      Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}))) ( 
    let pairs = (Maps.toList v1)
    in (Flows.bind (Flows.bind (Flows.mapList (typeOf cx vars types) (Lists.map fst pairs)) (singleType "map keys")) (\kt -> Flows.bind (Flows.bind (Flows.mapList (typeOf cx vars types) (Lists.map snd pairs)) (singleType "map values")) (\vt -> Flows.bind (checkTypeVariables vars kt) (\_ -> Flows.bind (checkTypeVariables vars vt) (\_ -> Flows.pure (Core.TypeMap (Core.MapType {
      Core.mapTypeKeys = kt,
      Core.mapTypeValues = vt})))))))))
  Core.TermOptional v1 -> (typeOfCollection cx "optional" (\x -> Core.TypeOptional x) vars types (Optionals.maybe [] Lists.singleton v1))
  Core.TermProduct v1 -> (Flows.bind (Flows.mapList (typeOf cx vars types) v1) (\etypes -> Flows.bind (Flows.sequence (Lists.map (checkTypeVariables vars) etypes)) (\_ -> Flows.pure (Core.TypeProduct etypes))))
  Core.TermRecord v1 ->  
    let tname = (Core.recordTypeName v1) 
        fields = (Core.recordFields v1)
    in (Flows.bind (Flows.mapList (typeOf cx vars types) (Lists.map Core.fieldTerm fields)) (\ftypes -> Flows.bind (Flows.sequence (Lists.map (checkTypeVariables vars) ftypes)) (\_ -> typeOfNominal "record typeOf" cx tname (Core.TypeRecord (Core.RowType {
      Core.rowTypeTypeName = tname,
      Core.rowTypeFields = (Lists.zipWith (\n -> \t -> Core.FieldType {
        Core.fieldTypeName = n,
        Core.fieldTypeType = t}) (Lists.map Core.fieldName fields) ftypes)})))))
  Core.TermSet v1 -> (typeOfCollection cx "set" (\x -> Core.TypeSet x) vars types (Sets.toList v1))
  Core.TermTypeAbstraction v1 ->  
    let v = (Core.typeAbstractionParameter v1) 
        e = (Core.typeAbstractionBody v1)
    in (Flows.bind (typeOf cx (Sets.insert v vars) types e) (\t1 -> Flows.bind (checkTypeVariables (Sets.insert v vars) t1) (\_ -> Flows.pure (Core.TypeForall (Core.ForallType {
      Core.forallTypeParameter = v,
      Core.forallTypeBody = t1})))))
  Core.TermTypeApplication v1 ->  
    let e = (Core.typedTermTerm v1) 
        t = (Core.typedTermType v1)
    in (Flows.bind (typeOf cx vars types e) (\t1 -> Flows.bind (checkTypeVariables vars t1) (\_ -> (\x -> case x of
      Core.TypeForall v2 ->  
        let v = (Core.forallTypeParameter v2) 
            t2 = (Core.forallTypeBody v2)
        in (Flows.pure (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
          (v, t)])) t2))
      Core.TypeVariable _ -> (Flows.fail (Strings.cat [
        "not a forall type: ",
        Core__.type_ t1,
        " in ",
        (Core__.term term)]))) t1)))
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        term1 = (Core.fieldTerm field)
        fieldTypeOf = (\ftype -> \fname1 -> Logic.ifElse (Equality.equal fname1 fname) (Flows.pure ftype) (Flows.map (\x -> Core.TypeVariable x) freshName))
        resolveType = (\subst -> \v -> Optionals.fromMaybe (Core.TypeVariable v) (Maps.lookup v subst))
    in (Flows.bind (typeOf cx vars types term1) (\ftype -> Flows.bind (checkTypeVariables vars ftype) (\_ -> Flows.bind (requireSchemaType cx tname) (\schemaType ->  
      let svars = (Core.typeSchemeVariables schemaType) 
          styp = (Core.typeSchemeType schemaType)
      in (Flows.bind (Core_.unionType tname styp) (\sfields ->  
        let fnames = (Lists.map Core.fieldTypeName sfields)
        in (Flows.bind (Flows.mapList (fieldTypeOf ftype) fnames) (\ftypes ->  
          let expected = (Core.TypeUnion (Core.RowType {
                  Core.rowTypeTypeName = tname,
                  Core.rowTypeFields = (Lists.zipWith (\n -> \t -> Core.FieldType {
                    Core.fieldTypeName = n,
                    Core.fieldTypeType = t}) fnames ftypes)}))
          in (Flows.bind (Unification.unifyTypes (Typing.inferenceContextSchemaTypes cx) styp expected "union typeOf") (\substWrapper ->  
            let subst = (Typing.unTypeSubst substWrapper) 
                tparams = (Lists.map (resolveType subst) svars)
            in (Flows.pure (nominalApplication tname tparams))))))))))))
  Core.TermVariable v1 -> (Optionals.maybe (Flows.fail (Strings.cat [
    "unbound variable: ",
    (Core.unName v1)])) Flows.pure (Maps.lookup v1 types))
  Core.TermWrap v1 ->  
    let tname = (Core.wrappedTermTypeName v1) 
        innerTerm = (Core.wrappedTermObject v1)
    in (Flows.bind (typeOf cx vars types innerTerm) (\innerType -> Flows.bind (checkTypeVariables vars innerType) (\_ -> typeOfNominal "wrapper typeOf" cx tname (Core.TypeWrap (Core.WrappedType {
      Core.wrappedTypeTypeName = tname,
      Core.wrappedTypeObject = innerType})))))
  _ -> (Flows.fail (Strings.cat [
    "unsupported term variant in typeOf: ",
    (Core__.termVariant (Variants.termVariant term))]))) term))

typeOfCollection :: (Typing.InferenceContext -> String -> (Core.Type -> Core.Type) -> S.Set Core.Name -> M.Map Core.Name Core.Type -> [Core.Term] -> Compute.Flow t0 Core.Type)
typeOfCollection cx desc cons vars types els = (Logic.ifElse (Lists.null els) (Flows.pure (typeSchemeToFType (Core.TypeScheme {
  Core.typeSchemeVariables = [
    Core.Name "t"],
  Core.typeSchemeType = (cons (Core.TypeVariable (Core.Name "t")))}))) (Flows.bind (Flows.mapList (\el -> typeOf cx vars types el) els) (\etypes -> Flows.bind (singleType desc etypes) (\et -> Flows.bind (checkTypeVariables vars et) (\result -> Flows.pure (cons et))))))

typeOfNominal :: (String -> Typing.InferenceContext -> Core.Name -> Core.Type -> Compute.Flow t0 Core.Type)
typeOfNominal desc cx tname expected =  
  let resolveType = (\subst -> \v -> Optionals.fromMaybe (Core.TypeVariable v) (Maps.lookup v subst))
  in (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
    let svars = (Core.typeSchemeVariables schemaType) 
        styp = (Core.typeSchemeType schemaType)
    in (Flows.bind (Unification.unifyTypes (Typing.inferenceContextSchemaTypes cx) styp expected desc) (\substWrapper ->  
      let subst = (Typing.unTypeSubst substWrapper) 
          tparams = (Lists.map (resolveType subst) svars)
      in (Flows.pure (nominalApplication tname tparams))))))

singleType :: (String -> [Core.Type] -> Compute.Flow t0 Core.Type)
singleType desc types =  
  let h = (Lists.head types) 
      allEqual = (Lists.foldl (\b -> \t -> Logic.and b (Equality.equal t h)) True types)
  in (Logic.ifElse allEqual (Flows.pure h) (Flows.fail (Strings.cat [
    "unequal types ",
    Formatting.showList Core__.type_ types,
    " in ",
    desc])))

checkType :: (S.Set Core.Name -> Typing.InferenceContext -> Core.Type -> Core.Term -> Compute.Flow t0 ())
checkType k g t e = (Logic.ifElse debugInference (Flows.bind (typeOf g k (toFContext g) e) (\t0 -> Logic.ifElse (Equality.equal t0 t) (Flows.pure ()) (Flows.fail (Strings.cat [
  "type checking failed: expected ",
  Core__.type_ t,
  " but found ",
  (Core__.type_ t0)])))) (Flows.pure ()))

checkTypeVariables :: (S.Set Core.Name -> Core.Type -> Compute.Flow t0 ())
checkTypeVariables vars typ = ((\x -> case x of
  Core.TypeForall v1 -> (checkTypeVariables (Sets.insert (Core.forallTypeParameter v1) vars) (Core.forallTypeBody v1))
  Core.TypeVariable v1 -> (Logic.ifElse (Sets.member v1 vars) (Flows.pure ()) (Flows.fail (Strings.cat [
    "unbound type variable \"",
    Core.unName v1,
    "\" in ",
    (Core__.type_ typ)])))
  _ -> (Flows.bind (Flows.sequence (Lists.map (checkTypeVariables vars) (Rewriting.subtypes typ))) (\result -> Flows.pure ()))) typ)

-- | Helper to gather forall variables
gatherForall :: ([Core.Name] -> Core.Type -> Core.TypeScheme)
gatherForall vars typ = ((\x -> case x of
  Core.TypeForall v1 -> (gatherForall (Lists.cons (Core.forallTypeParameter v1) vars) (Core.forallTypeBody v1))
  Core.TypeVariable _ -> Core.TypeScheme {
    Core.typeSchemeVariables = (Lists.reverse vars),
    Core.typeSchemeType = typ}) (Strip.stripType typ))

-- | Convert a type scheme to a forall type
typeSchemeToFType :: (Core.TypeScheme -> Core.Type)
typeSchemeToFType ts =  
  let vars = (Core.typeSchemeVariables ts) 
      body = (Core.typeSchemeType ts)
  in (Lists.foldl (\t -> \v -> Core.TypeForall (Core.ForallType {
    Core.forallTypeParameter = v,
    Core.forallTypeBody = t})) body (Lists.reverse vars))

-- | Convert inference context to type context
toFContext :: (Typing.InferenceContext -> M.Map Core.Name Core.Type)
toFContext cx = (Maps.map typeSchemeToFType (Typing.inferenceContextDataTypes cx))

-- | Show an inference result for debugging
showInferenceResult :: (Typing.InferenceResult -> String)
showInferenceResult result =  
  let term = (Typing.inferenceResultTerm result) 
      typ = (Typing.inferenceResultType result)
      subst = (Typing.inferenceResultSubst result)
  in (Strings.cat [
    "{term=",
    Core__.term term,
    ", type=",
    Core__.type_ typ,
    ", subst=",
    Core__.typeSubst subst,
    "}"])

-- | An empty inference context
emptyInferenceContext :: Typing.InferenceContext
emptyInferenceContext = Typing.InferenceContext {
  Typing.inferenceContextSchemaTypes = (M.fromList []),
  Typing.inferenceContextPrimitiveTypes = (M.fromList []),
  Typing.inferenceContextDataTypes = (M.fromList []),
  Typing.inferenceContextDebug = False}

-- | Get all free variables in an inference context
freeVariablesInContext :: (Typing.InferenceContext -> S.Set Core.Name)
freeVariablesInContext cx = (Lists.foldl Sets.union Sets.empty (Lists.map Rewriting.freeVariablesInTypeSchemeSimple (Maps.elems (Typing.inferenceContextDataTypes cx))))

-- | Generalize a type to a type scheme
generalize :: (Typing.InferenceContext -> Core.Type -> Core.TypeScheme)
generalize cx typ =  
  let vars = (Lists.nub (Lists.filter (isUnbound cx) (Sets.toList (Rewriting.freeVariablesInType typ))))
  in Core.TypeScheme {
    Core.typeSchemeVariables = vars,
    Core.typeSchemeType = typ}

-- | Check if a variable is unbound in context
isUnbound :: (Typing.InferenceContext -> Core.Name -> Bool)
isUnbound cx v = (Logic.and (Logic.not (Sets.member v (freeVariablesInContext cx))) (Logic.not (Maps.member v (Typing.inferenceContextSchemaTypes cx))))

graphToInferenceContext :: (Graph.Graph -> Compute.Flow t0 Typing.InferenceContext)
graphToInferenceContext g0 =  
  let schema = (Optionals.fromMaybe g0 (Graph.graphSchema g0)) 
      primTypes = (Maps.fromList (Lists.map (\p -> (Graph.primitiveName p, (Graph.primitiveType p))) (Maps.elems (Graph.graphPrimitives g0))))
      varTypes = Maps.empty
  in (Flows.bind (Schemas.schemaGraphToTypingEnvironment schema) (\schemaTypes -> Flows.pure (Typing.InferenceContext {
    Typing.inferenceContextSchemaTypes = schemaTypes,
    Typing.inferenceContextPrimitiveTypes = primTypes,
    Typing.inferenceContextDataTypes = varTypes,
    Typing.inferenceContextDebug = False})))

-- | Infer the type of a term in graph context
inferInGraphContext :: (Core.Term -> Compute.Flow Graph.Graph Typing.InferenceResult)
inferInGraphContext term = (Flows.bind Errors.getState (\g -> Flows.bind (graphToInferenceContext g) (\cx -> inferTypeOfTerm cx term "single term")))

inferGraphTypes :: (Graph.Graph -> Compute.Flow t0 Graph.Graph)
inferGraphTypes g0 =  
  let fromLetTerm = (\l ->  
          let bindings = (Core.letBindings l) 
              env = (Core.letEnvironment l)
              fromBinding = (\b -> (Core.letBindingName b, Graph.Element {
                      Graph.elementName = (Core.letBindingName b),
                      Graph.elementTerm = (Core.letBindingTerm b),
                      Graph.elementType = (Core.letBindingType b)}))
          in Graph.Graph {
            Graph.graphElements = (Maps.fromList (Lists.map fromBinding bindings)),
            Graph.graphEnvironment = Maps.empty,
            Graph.graphTypes = Maps.empty,
            Graph.graphBody = env,
            Graph.graphPrimitives = (Graph.graphPrimitives g0),
            Graph.graphSchema = (Graph.graphSchema g0)}) 
      toLetTerm = (\g ->  
              let toBinding = (\el -> Core.LetBinding {
                      Core.letBindingName = (Graph.elementName el),
                      Core.letBindingTerm = (Graph.elementTerm el),
                      Core.letBindingType = Nothing})
              in (Core.TermLet (Core.Let {
                Core.letBindings = (Lists.map toBinding (Maps.elems (Graph.graphElements g))),
                Core.letEnvironment = (Graph.graphBody g)})))
      withResult = (\result ->  
              let term = (Typing.inferenceResultTerm result) 
                  ts = (Typing.inferenceResultType result)
              in ((\x -> case x of
                Core.TermLet v1 -> (Flows.pure (fromLetTerm v1))
                Core.TermVariable _ -> (Flows.fail "Expected inferred graph as let term")) (Rewriting.normalizeTypeVariablesInTerm term)))
  in (Monads.withTrace "graph inference" (Flows.bind (graphToInferenceContext g0) (\cx -> Flows.bind (inferTypeOfTerm cx (toLetTerm g0) "graph term") withResult)))

inferMany :: (Typing.InferenceContext -> [(Core.Term, String)] -> Compute.Flow t0 ([Core.Term], ([Core.Type], Typing.TypeSubst)))
inferMany cx pairs = (Logic.ifElse (Lists.null pairs) (Flows.pure ([], ([], Substitution.idTypeSubst))) ( 
  let e = (fst (Lists.head pairs)) 
      desc = (snd (Lists.head pairs))
      tl = (Lists.tail pairs)
  in (Flows.bind (inferTypeOfTerm cx e desc) (\result1 ->  
    let e1 = (Typing.inferenceResultTerm result1) 
        t1 = (Typing.inferenceResultType result1)
        s1 = (Typing.inferenceResultSubst result1)
    in (Flows.bind (inferMany (Substitution.substInContext s1 cx) tl) (\result2 ->  
      let e2 = (fst result2) 
          t2 = (fst (snd result2))
          s2 = (snd (snd result2))
      in (Flows.pure (Lists.cons (Substitution.substTypesInTerm s2 e1) e2, (Lists.cons (Substitution.substInType s2 t1) t2, (Substitution.composeTypeSubst s1 s2))))))))))

inferTypeOf :: (Typing.InferenceContext -> Core.Term -> Compute.Flow t0 (Core.Term, Core.TypeScheme))
inferTypeOf cx term =  
  let letTerm = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.LetBinding {
              Core.letBindingName = (Core.Name "ignoredVariableName"),
              Core.letBindingTerm = term,
              Core.letBindingType = Nothing}],
          Core.letEnvironment = (Core.TermLiteral (Core.LiteralString "ignoredEnvironment"))})) 
      unifyAndSubst = (\result ->  
              let subst = (Typing.inferenceResultSubst result)
              in (Flows.bind (Annotations.withEmptyGraph (Core_.letTerm (Rewriting.normalizeTypeVariablesInTerm (Typing.inferenceResultTerm result)))) (\letResult ->  
                let bindings = (Core.letBindings letResult)
                in (Logic.ifElse (Equality.equal 1 (Lists.length bindings)) ( 
                  let binding = (Lists.head bindings) 
                      term1 = (Core.letBindingTerm binding)
                      mts = (Core.letBindingType binding)
                  in (Optionals.maybe (Flows.fail "Expected a type scheme") (\ts -> Flows.pure (term1, ts)) mts)) (Flows.fail (Strings.cat [
                  "Expected a single binding with a type scheme, but got: ",
                  Literals.showInt32 (Lists.length bindings),
                  " bindings"]))))))
  in (Flows.bind (inferTypeOfTerm cx letTerm "infer type of term") (\result -> unifyAndSubst result))

inferTypeOfAnnotatedTerm :: (Typing.InferenceContext -> Core.AnnotatedTerm -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfAnnotatedTerm cx at =  
  let term = (Core.annotatedTermSubject at) 
      ann = (Core.annotatedTermAnnotation at)
  in (Flows.map (\result ->  
    let iterm = (Typing.inferenceResultTerm result) 
        itype = (Typing.inferenceResultType result)
        isubst = (Typing.inferenceResultSubst result)
    in Typing.InferenceResult {
      Typing.inferenceResultTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermSubject = iterm,
        Core.annotatedTermAnnotation = ann})),
      Typing.inferenceResultType = itype,
      Typing.inferenceResultSubst = isubst}) (inferTypeOfTerm cx term "annotated term"))

inferTypeOfApplication :: (Typing.InferenceContext -> Core.Application -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfApplication cx app =  
  let e0 = (Core.applicationFunction app) 
      e1 = (Core.applicationArgument app)
  in (Flows.bind (inferTypeOfTerm cx e0 "lhs") (\lhsResult ->  
    let a = (Typing.inferenceResultTerm lhsResult) 
        t0 = (Typing.inferenceResultType lhsResult)
        s0 = (Typing.inferenceResultSubst lhsResult)
    in (Flows.bind (inferTypeOfTerm (Substitution.substInContext s0 cx) e1 "rhs") (\rhsResult ->  
      let b = (Typing.inferenceResultTerm rhsResult) 
          t1 = (Typing.inferenceResultType rhsResult)
          s1 = (Typing.inferenceResultSubst rhsResult)
      in (Flows.bind freshName (\v -> Flows.bind (Unification.unifyTypes (Typing.inferenceContextSchemaTypes cx) (Substitution.substInType s1 t0) (Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = t1,
        Core.functionTypeCodomain = (Core.TypeVariable v)})) "application lhs") (\s2 ->  
        let rExpr = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Substitution.substTypesInTerm (Substitution.composeTypeSubst s1 s2) a),
                Core.applicationArgument = (Substitution.substTypesInTerm s2 b)})) 
            rType = (Substitution.substInType s2 (Core.TypeVariable v))
            rSubst = (Substitution.composeTypeSubstList [
                    s0,
                    s1,
                    s2])
        in (Flows.pure (Typing.InferenceResult {
          Typing.inferenceResultTerm = rExpr,
          Typing.inferenceResultType = rType,
          Typing.inferenceResultSubst = rSubst})))))))))

inferTypeOfCaseStatement :: (Typing.InferenceContext -> Core.CaseStatement -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfCaseStatement cx caseStmt =  
  let tname = (Core.caseStatementTypeName caseStmt) 
      dflt = (Core.caseStatementDefault caseStmt)
      cases = (Core.caseStatementCases caseStmt)
      fnames = (Lists.map Core.fieldName cases)
  in (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
    let svars = (Core.typeSchemeVariables schemaType) 
        styp = (Core.typeSchemeType schemaType)
    in (Flows.bind (Core_.unionType tname styp) (\sfields -> Flows.bind (Flows.traverseOptional (\t -> inferTypeOfTerm cx t (Strings.cat [
      "case ",
      Core.unName tname,
      ".<default>"])) dflt) (\dfltResult -> Flows.bind (inferMany cx (Lists.map (\f -> (Core.fieldTerm f, (Strings.cat [
      "case ",
      Core.unName tname,
      ".",
      (Core.unName (Core.fieldName f))]))) cases)) (\caseResults ->  
      let iterms = (fst caseResults) 
          itypes = (fst (snd caseResults))
          isubst = (snd (snd caseResults))
      in (Flows.bind freshName (\codv ->  
        let cod = (Core.TypeVariable codv) 
            caseMap = (Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) sfields))
            dfltConstraints = (Monads.optionalToList (Optionals.map (\r -> Typing.TypeConstraint {
                    Typing.typeConstraintLeft = cod,
                    Typing.typeConstraintRight = (Typing.inferenceResultType r),
                    Typing.typeConstraintComment = "match default"}) dfltResult))
            caseConstraints = (Optionals.cat (Lists.zipWith (\fname -> \itype -> Optionals.map (\ftype -> Typing.TypeConstraint {
                    Typing.typeConstraintLeft = itype,
                    Typing.typeConstraintRight = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = ftype,
                      Core.functionTypeCodomain = cod})),
                    Typing.typeConstraintComment = "case type"}) (Maps.lookup fname caseMap)) fnames itypes))
        in (mapConstraints cx (\subst -> yield (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
          Core.caseStatementTypeName = tname,
          Core.caseStatementDefault = (Optionals.map Typing.inferenceResultTerm dfltResult),
          Core.caseStatementCases = (Lists.zipWith (\n -> \t -> Core.Field {
            Core.fieldName = n,
            Core.fieldTerm = t}) fnames iterms)})))) (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)),
          Core.functionTypeCodomain = cod})) (Substitution.composeTypeSubstList (Lists.concat [
          Monads.optionalToList (Optionals.map Typing.inferenceResultSubst dfltResult),
          [
            isubst,
            subst]]))) (Lists.concat [
          dfltConstraints,
          caseConstraints]))))))))))

inferTypeOfCollection :: (Typing.InferenceContext -> (Core.Type -> Core.Type) -> ([Core.Term] -> Core.Term) -> String -> [Core.Term] -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfCollection cx typCons trmCons desc els = (Flows.bind freshName (\var -> Flows.bind (inferMany cx (Lists.zip els (Lists.map (\i -> Strings.cat [
  "#",
  (Literals.showInt32 i)]) (Math.rangeInt32 1 (Math.add (Lists.length els) 1))))) (\results ->  
  let terms = (fst results) 
      types = (fst (snd results))
      subst1 = (snd (snd results))
      constraints = (Lists.map (\t -> Typing.TypeConstraint {
              Typing.typeConstraintLeft = (Core.TypeVariable var),
              Typing.typeConstraintRight = t,
              Typing.typeConstraintComment = desc}) types)
  in (mapConstraints cx (\subst2 ->  
    let iterm = (trmCons terms) 
        itype = (typCons (Core.TypeVariable var))
        isubst = (Substitution.composeTypeSubst subst1 subst2)
    in (yield iterm itype isubst)) constraints))))

inferTypeOfElimination :: (Typing.InferenceContext -> Core.Elimination -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfElimination cx elm = ((\x -> case x of
  Core.EliminationProduct v1 -> (inferTypeOfTupleProjection cx v1)
  Core.EliminationRecord v1 -> (inferTypeOfProjection cx v1)
  Core.EliminationUnion v1 -> (inferTypeOfCaseStatement cx v1)
  Core.EliminationWrap v1 -> (inferTypeOfUnwrap cx v1)) elm)

inferTypeOfFunction :: (Typing.InferenceContext -> Core.Function -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfFunction cx f = ((\x -> case x of
  Core.FunctionElimination v1 -> (inferTypeOfElimination cx v1)
  Core.FunctionLambda v1 -> (inferTypeOfLambda cx v1)
  Core.FunctionPrimitive v1 -> (inferTypeOfPrimitive cx v1)) f)

inferTypeOfInjection :: (Typing.InferenceContext -> Core.Injection -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfInjection cx injection =  
  let tname = (Core.injectionTypeName injection) 
      field = (Core.injectionField injection)
      fname = (Core.fieldName field)
      term = (Core.fieldTerm field)
  in (Flows.bind (requireSchemaType cx tname) (\schemaType -> Flows.bind (inferTypeOfTerm cx term "injected term") (\result ->  
    let svars = (Core.typeSchemeVariables schemaType) 
        styp = (Core.typeSchemeType schemaType)
        iterm = (Typing.inferenceResultTerm result)
        ityp = (Typing.inferenceResultType result)
        isubst = (Typing.inferenceResultSubst result)
    in (Flows.bind (Core_.unionType tname styp) (\sfields -> Flows.bind (Schemas.findFieldType fname sfields) (\ftyp -> mapConstraints cx (\subst -> yield (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = tname,
      Core.injectionField = Core.Field {
        Core.fieldName = fname,
        Core.fieldTerm = iterm}})) (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)) (Substitution.composeTypeSubst isubst subst)) [
      Typing.TypeConstraint {
        Typing.typeConstraintLeft = ftyp,
        Typing.typeConstraintRight = ityp,
        Typing.typeConstraintComment = "schema type of injected field"}]))))))

inferTypeOfLambda :: (Typing.InferenceContext -> Core.Lambda -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfLambda cx lambda =  
  let var = (Core.lambdaParameter lambda) 
      body = (Core.lambdaBody lambda)
  in (Flows.bind freshName (\vdom ->  
    let dom = (Core.TypeVariable vdom) 
        cx2 = (extendContext [
                (var, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = dom})] cx)
    in (Flows.bind (inferTypeOfTerm cx2 body "lambda body") (\result ->  
      let iterm = (Typing.inferenceResultTerm result) 
          icod = (Typing.inferenceResultType result)
          isubst = (Typing.inferenceResultSubst result)
          rdom = (Substitution.substInType isubst dom)
          rterm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = var,
                  Core.lambdaDomain = (Just rdom),
                  Core.lambdaBody = iterm})))
          rtype = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = rdom,
                  Core.functionTypeCodomain = icod}))
          vars = (Sets.unions [
                  Rewriting.freeVariablesInType rdom,
                  Rewriting.freeVariablesInType icod,
                  (freeVariablesInContext (Substitution.substInContext isubst cx2))])
          cx3 = (Substitution.substInContext isubst cx)
      in (Flows.pure (Typing.InferenceResult {
        Typing.inferenceResultTerm = rterm,
        Typing.inferenceResultType = rtype,
        Typing.inferenceResultSubst = isubst}))))))

inferTypeOfLet :: (Typing.InferenceContext -> Core.Let -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfLet cx let0 =  
  let bindings0 = (Core.letBindings let0) 
      env0 = (Core.letEnvironment let0)
      names = (Lists.map Core.letBindingName bindings0)
      nameSet = (Sets.fromList names)
      toPair = (\binding ->  
              let name = (Core.letBindingName binding) 
                  term = (Core.letBindingTerm binding)
              in (name, (Lists.filter (\n -> Sets.member n nameSet) (Sets.toList (Rewriting.freeVariablesInTerm term)))))
      adjList = (Lists.map toPair bindings0)
      groups = (Sorting.topologicalSortComponents adjList)
      bindingMap = (Maps.fromList (Lists.zip names bindings0))
      createLet = (\e -> \group -> Core.TermLet (Core.Let {
              Core.letBindings = (Optionals.cat (Lists.map (\n -> Maps.lookup n bindingMap) group)),
              Core.letEnvironment = e}))
      rewrittenLet = (Lists.foldl createLet env0 (Lists.reverse groups))
      restoreLet = (\iterm ->  
              let helper = (\level -> \bins -> \term -> Logic.ifElse (Equality.equal level 0) (bins, term) ((\x -> case x of
                      Core.TermLet v1 ->  
                        let bs = (Core.letBindings v1) 
                            e = (Core.letEnvironment v1)
                        in (helper (Math.sub level 1) (Lists.concat [
                          bs,
                          bins]) e)) term)) 
                  result = (helper (Lists.length groups) [] iterm)
                  bindingList = (fst result)
                  e = (snd result)
                  bindingMap2 = (Maps.fromList (Lists.map (\b -> (Core.letBindingName b, b)) bindingList))
              in (Core.TermLet (Core.Let {
                Core.letBindings = (Optionals.cat (Lists.map (\n -> Maps.lookup n bindingMap2) names)),
                Core.letEnvironment = e})))
      rewriteResult = (\result ->  
              let iterm = (Typing.inferenceResultTerm result) 
                  itype = (Typing.inferenceResultType result)
                  isubst = (Typing.inferenceResultSubst result)
              in Typing.InferenceResult {
                Typing.inferenceResultTerm = (restoreLet iterm),
                Typing.inferenceResultType = itype,
                Typing.inferenceResultSubst = isubst})
  in (Flows.map rewriteResult ((\x -> case x of
    Core.TermLet v1 -> (inferTypeOfLetAfterNormalization cx v1)
    _ -> (inferTypeOfTerm cx rewrittenLet "empty let term")) rewrittenLet))

inferTypeOfLetAfterNormalization :: (Typing.InferenceContext -> Core.Let -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfLetAfterNormalization cx0 letTerm =  
  let bins0 = (Core.letBindings letTerm) 
      env0 = (Core.letEnvironment letTerm)
      bnames = (Lists.map Core.letBindingName bins0)
  in (Flows.bind (freshNames (Lists.length bins0)) (\bvars ->  
    let tbins0 = (Lists.map (\x -> Core.TypeVariable x) bvars) 
        cx1 = (extendContext (Lists.zip bnames (Lists.map (\t -> Core.TypeScheme {
                Core.typeSchemeVariables = [],
                Core.typeSchemeType = t}) tbins0)) cx0)
    in (Flows.bind (inferTypesOfTemporaryLetBindings cx1 bins0) (\inferredResult ->  
      let bterms1 = (fst inferredResult) 
          tbins1 = (fst (snd inferredResult))
          s1 = (snd (snd inferredResult))
      in (Flows.bind (Unification.unifyTypeLists (Typing.inferenceContextSchemaTypes cx0) (Lists.map (Substitution.substInType s1) tbins0) tbins1 "temporary type bindings") (\s2 ->  
        let g2 = (Substitution.substInContext (Substitution.composeTypeSubst s1 s2) cx0) 
            tsbins1 = (Lists.zip bnames (Lists.map (\t -> generalize g2 (Substitution.substInType s2 t)) tbins1))
        in (Flows.bind (inferTypeOfTerm (extendContext tsbins1 g2) env0 "let environment") (\envResult ->  
          let env1 = (Typing.inferenceResultTerm envResult) 
              tenv = (Typing.inferenceResultType envResult)
              senv = (Typing.inferenceResultSubst envResult)
              st1 = (Typing.TermSubst (Maps.fromList (Lists.map (\pair ->  
                      let name = (fst pair) 
                          ts = (snd pair)
                      in (name, (Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypedTerm {
                        Core.typedTermTerm = t,
                        Core.typedTermType = (Core.TypeVariable v)})) (Core.TermVariable name) (Core.typeSchemeVariables ts)))) tsbins1)))
              createBinding = (\bindingPair ->  
                      let nameTsPair = (fst bindingPair) 
                          term = (snd bindingPair)
                          name = (fst nameTsPair)
                          ts = (snd nameTsPair)
                          typeAbstractedTerm = (Lists.foldl (\b -> \v -> Core.TermTypeAbstraction (Core.TypeAbstraction {
                                  Core.typeAbstractionParameter = v,
                                  Core.typeAbstractionBody = b})) (Substitution.substituteInTerm st1 term) (Core.typeSchemeVariables ts))
                      in Core.LetBinding {
                        Core.letBindingName = name,
                        Core.letBindingTerm = (Substitution.substTypesInTerm (Substitution.composeTypeSubst senv s2) typeAbstractedTerm),
                        Core.letBindingType = (Just (Substitution.substInTypeScheme senv ts))})
              bins1 = (Lists.map createBinding (Lists.zip tsbins1 bterms1))
              ret = Typing.InferenceResult {
                      Typing.inferenceResultTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = bins1,
                        Core.letEnvironment = env1})),
                      Typing.inferenceResultType = tenv,
                      Typing.inferenceResultSubst = (Substitution.composeTypeSubstList [
                        s1,
                        s2,
                        senv])}
          in (Flows.pure ret)))))))))

inferTypeOfList :: (Typing.InferenceContext -> [Core.Term] -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfList cx = (inferTypeOfCollection cx (\x -> Core.TypeList x) (\x -> Core.TermList x) "list element")

inferTypeOfLiteral :: (t0 -> Core.Literal -> Compute.Flow t1 Typing.InferenceResult)
inferTypeOfLiteral _ lit = (Flows.pure (Typing.InferenceResult {
  Typing.inferenceResultTerm = (Core.TermLiteral lit),
  Typing.inferenceResultType = (Core.TypeLiteral (Variants.literalType lit)),
  Typing.inferenceResultSubst = Substitution.idTypeSubst}))

inferTypeOfMap :: (Typing.InferenceContext -> M.Map Core.Term Core.Term -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfMap cx m = (Flows.bind freshName (\kvar -> Flows.bind freshName (\vvar -> Logic.ifElse (Maps.null m) (Flows.pure (yield (Core.TermMap Maps.empty) (Core.TypeMap (Core.MapType {
  Core.mapTypeKeys = (Core.TypeVariable kvar),
  Core.mapTypeValues = (Core.TypeVariable vvar)})) Substitution.idTypeSubst)) (Flows.bind (inferMany cx (Lists.map (\k -> (k, "map key")) (Maps.keys m))) (\kresults ->  
  let kterms = (fst kresults) 
      ktypes = (fst (snd kresults))
      ksubst = (snd (snd kresults))
  in (Flows.bind (inferMany cx (Lists.map (\v -> (v, "map value")) (Maps.elems m))) (\vresults ->  
    let vterms = (fst vresults) 
        vtypes = (fst (snd vresults))
        vsubst = (snd (snd vresults))
        kcons = (Lists.map (\t -> Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable kvar),
                Typing.typeConstraintRight = t,
                Typing.typeConstraintComment = "map key"}) ktypes)
        vcons = (Lists.map (\t -> Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable vvar),
                Typing.typeConstraintRight = t,
                Typing.typeConstraintComment = "map value"}) vtypes)
    in (mapConstraints cx (\subst -> yield (Core.TermMap (Maps.fromList (Lists.zip kterms vterms))) (Core.TypeMap (Core.MapType {
      Core.mapTypeKeys = (Core.TypeVariable kvar),
      Core.mapTypeValues = (Core.TypeVariable vvar)})) (Substitution.composeTypeSubstList [
      ksubst,
      vsubst,
      subst])) (Lists.concat [
      kcons,
      vcons])))))))))

inferTypeOfOptional :: (Typing.InferenceContext -> Maybe Core.Term -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfOptional cx m =  
  let trmCons = (\terms -> Logic.ifElse (Lists.null terms) (Core.TermOptional Nothing) (Core.TermOptional (Just (Lists.head terms))))
  in (inferTypeOfCollection cx (\x -> Core.TypeOptional x) trmCons "optional element" (Optionals.maybe [] Lists.singleton m))

inferTypeOfPrimitive :: (Typing.InferenceContext -> Core.Name -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfPrimitive cx name = (Optionals.maybe (Flows.fail (Strings.cat2 "No such primitive: " (Core.unName name))) (\scheme -> Flows.bind (instantiateTypeScheme scheme) (\ts ->  
  let vars = (Core.typeSchemeVariables ts) 
      itype = (Core.typeSchemeType ts)
      iterm = (Lists.foldl (\t -> \v -> Core.TermTypeApplication (Core.TypedTerm {
              Core.typedTermTerm = t,
              Core.typedTermType = (Core.TypeVariable v)})) (Core.TermFunction (Core.FunctionPrimitive name)) vars)
  in (yieldChecked cx vars iterm itype Substitution.idTypeSubst))) (Maps.lookup name (Typing.inferenceContextPrimitiveTypes cx)))

inferTypeOfProduct :: (Typing.InferenceContext -> [Core.Term] -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfProduct cx els = (Flows.map (\results ->  
  let iterms = (fst results) 
      itypes = (fst (snd results))
      isubst = (snd (snd results))
  in (yield (Core.TermProduct iterms) (Core.TypeProduct itypes) isubst)) (inferMany cx (Lists.map (\e -> (e, "tuple element")) els)))

inferTypeOfProjection :: (Typing.InferenceContext -> Core.Projection -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfProjection cx proj =  
  let tname = (Core.projectionTypeName proj) 
      fname = (Core.projectionField proj)
  in (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
    let svars = (Core.typeSchemeVariables schemaType) 
        styp = (Core.typeSchemeType schemaType)
    in (Flows.bind (Core_.recordType tname styp) (\sfields -> Flows.bind (Schemas.findFieldType fname sfields) (\ftyp -> Flows.pure (yield (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
      Core.projectionTypeName = tname,
      Core.projectionField = fname})))) (Core.TypeFunction (Core.FunctionType {
      Core.functionTypeDomain = (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)),
      Core.functionTypeCodomain = ftyp})) Substitution.idTypeSubst))))))

inferTypeOfRecord :: (Typing.InferenceContext -> Core.Record -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfRecord cx record =  
  let tname = (Core.recordTypeName record) 
      fields = (Core.recordFields record)
      fnames = (Lists.map Core.fieldName fields)
  in (Flows.bind (requireSchemaType cx tname) (\schemaType -> Flows.bind (inferMany cx (Lists.map (\f -> (Core.fieldTerm f, (Strings.cat2 "field " (Core.unName (Core.fieldName f))))) fields)) (\results ->  
    let svars = (Core.typeSchemeVariables schemaType) 
        styp = (Core.typeSchemeType schemaType)
        iterms = (fst results)
        itypes = (fst (snd results))
        isubst = (snd (snd results))
        ityp = (Core.TypeRecord (Core.RowType {
                Core.rowTypeTypeName = tname,
                Core.rowTypeFields = (Lists.zipWith (\n -> \t -> Core.FieldType {
                  Core.fieldTypeName = n,
                  Core.fieldTypeType = t}) fnames itypes)}))
    in (mapConstraints cx (\subst -> yield (Core.TermRecord (Core.Record {
      Core.recordTypeName = tname,
      Core.recordFields = (Lists.zipWith (\n -> \t -> Core.Field {
        Core.fieldName = n,
        Core.fieldTerm = t}) fnames iterms)})) (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)) (Substitution.composeTypeSubst isubst subst)) [
      Typing.TypeConstraint {
        Typing.typeConstraintLeft = styp,
        Typing.typeConstraintRight = ityp,
        Typing.typeConstraintComment = "schema type of record"}]))))

inferTypeOfSet :: (Typing.InferenceContext -> S.Set Core.Term -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfSet cx s = (inferTypeOfCollection cx (\x -> Core.TypeSet x) (\terms -> Core.TermSet (Sets.fromList terms)) "set element" (Sets.toList s))

inferTypeOfSum :: (Typing.InferenceContext -> Core.Sum -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfSum cx sum =  
  let i = (Core.sumIndex sum) 
      s = (Core.sumSize sum)
      term = (Core.sumTerm sum)
  in (Flows.bind (inferTypeOfTerm cx term "sum term") (\result ->  
    let iterm = (Typing.inferenceResultTerm result) 
        ityp = (Typing.inferenceResultType result)
        isubst = (Typing.inferenceResultSubst result)
        varOrTerm = (\t -> \j -> Logic.ifElse (Equality.equalInt32 i j) (Flows.pure (Mantle.EitherLeft t)) (Flows.map (\x -> Mantle.EitherRight x) freshName))
    in (Flows.bind (Flows.sequence (Lists.map (varOrTerm ityp) (Math.rangeInt32 0 (Math.sub s 1)))) (\vars ->  
      let toType = (\e -> (\x -> case x of
              Mantle.EitherLeft v1 -> v1
              Mantle.EitherRight v1 -> (Core.TypeVariable v1)) e)
      in (Flows.pure (yield (Core.TermSum (Core.Sum {
        Core.sumIndex = i,
        Core.sumSize = s,
        Core.sumTerm = iterm})) (Core.TypeSum (Lists.map toType vars)) isubst))))))

inferTypeOfTerm :: (Typing.InferenceContext -> Core.Term -> String -> Compute.Flow t0 Typing.InferenceResult)
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
  Core.TermTypeAbstraction v1 -> (inferTypeOfTypeAbstraction cx v1)
  Core.TermTypeApplication v1 -> (inferTypeOfTypeApplication cx v1)
  Core.TermUnion v1 -> (inferTypeOfInjection cx v1)
  Core.TermVariable v1 -> (inferTypeOfVariable cx v1)
  Core.TermWrap v1 -> (inferTypeOfWrappedTerm cx v1)) term))

inferTypeOfTupleProjection :: (t0 -> Core.TupleProjection -> Compute.Flow t1 Typing.InferenceResult)
inferTypeOfTupleProjection _ tp =  
  let arity = (Core.tupleProjectionArity tp) 
      idx = (Core.tupleProjectionIndex tp)
  in (forVars arity (\vars ->  
    let types = (Lists.map (\x -> Core.TypeVariable x) vars) 
        cod = (Lists.at idx types)
    in (yield (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
      Core.tupleProjectionArity = arity,
      Core.tupleProjectionIndex = idx,
      Core.tupleProjectionDomain = (Just types)})))) (Core.TypeFunction (Core.FunctionType {
      Core.functionTypeDomain = (Core.TypeProduct types),
      Core.functionTypeCodomain = cod})) Substitution.idTypeSubst)))

inferTypeOfTypeAbstraction :: (Typing.InferenceContext -> Core.TypeAbstraction -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfTypeAbstraction cx ta = (inferTypeOfTerm cx (Core.typeAbstractionBody ta) "type abstraction")

inferTypeOfTypeApplication :: (Typing.InferenceContext -> Core.TypedTerm -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfTypeApplication cx tt = (inferTypeOfTerm cx (Core.typedTermTerm tt) "type application term")

inferTypeOfUnwrap :: (Typing.InferenceContext -> Core.Name -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfUnwrap cx tname = (Flows.bind (requireSchemaType cx tname) (\schemaType ->  
  let svars = (Core.typeSchemeVariables schemaType) 
      styp = (Core.typeSchemeType schemaType)
  in (Flows.bind (Core_.wrappedType tname styp) (\wtyp -> Flows.pure (yield (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap tname))) (Core.TypeFunction (Core.FunctionType {
    Core.functionTypeDomain = (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) svars)),
    Core.functionTypeCodomain = wtyp})) Substitution.idTypeSubst)))))

inferTypeOfVariable :: (Typing.InferenceContext -> Core.Name -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfVariable cx name = (Optionals.maybe (Flows.fail (Strings.cat2 "Variable not bound to type: " (Core.unName name))) (\scheme -> Flows.bind (instantiateTypeScheme scheme) (\ts ->  
  let vars = (Core.typeSchemeVariables ts) 
      itype = (Core.typeSchemeType ts)
      iterm = (Lists.foldl (\t -> \ty -> Core.TermTypeApplication (Core.TypedTerm {
              Core.typedTermTerm = t,
              Core.typedTermType = ty})) (Core.TermVariable name) (Lists.map (\x -> Core.TypeVariable x) vars))
  in (Flows.pure (Typing.InferenceResult {
    Typing.inferenceResultTerm = iterm,
    Typing.inferenceResultType = itype,
    Typing.inferenceResultSubst = Substitution.idTypeSubst})))) (Maps.lookup name (Typing.inferenceContextDataTypes cx)))

inferTypeOfWrappedTerm :: (Typing.InferenceContext -> Core.WrappedTerm -> Compute.Flow t0 Typing.InferenceResult)
inferTypeOfWrappedTerm cx wt =  
  let tname = (Core.wrappedTermTypeName wt) 
      term = (Core.wrappedTermObject wt)
  in (Flows.bind (requireSchemaType cx tname) (\schemaType -> Flows.bind (inferTypeOfTerm cx term "wrapped term") (\result ->  
    let svars = (Core.typeSchemeVariables schemaType) 
        styp = (Core.typeSchemeType schemaType)
        iterm = (Typing.inferenceResultTerm result)
        ityp = (Typing.inferenceResultType result)
        isubst = (Typing.inferenceResultSubst result)
    in (Flows.bind (freshNames (Lists.length svars)) (\freshVars ->  
      let subst = (Typing.TypeSubst (Maps.fromList (Lists.zip svars (Lists.map (\x -> Core.TypeVariable x) freshVars)))) 
          stypInst = (Substitution.substInType subst styp)
          nominalInst = (nominalApplication tname (Lists.map (\x -> Core.TypeVariable x) freshVars))
          expected = (Core.TypeWrap (Core.WrappedType {
                  Core.wrappedTypeTypeName = tname,
                  Core.wrappedTypeObject = ityp}))
          freeVars = (Sets.toList (Sets.unions [
                  Rewriting.freeVariablesInType ityp,
                  Rewriting.freeVariablesInTerm iterm,
                  (Sets.fromList freshVars)]))
      in (bindConstraints cx (\subst2 -> yieldChecked cx freeVars (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = tname,
        Core.wrappedTermObject = iterm})) nominalInst (Substitution.composeTypeSubst isubst subst2)) [
        Typing.TypeConstraint {
          Typing.typeConstraintLeft = stypInst,
          Typing.typeConstraintRight = expected,
          Typing.typeConstraintComment = "schema type of wrapper"}]))))))

inferTypesOfTemporaryLetBindings :: (Typing.InferenceContext -> [Core.LetBinding] -> Compute.Flow t0 ([Core.Term], ([Core.Type], Typing.TypeSubst)))
inferTypesOfTemporaryLetBindings cx bins = (Logic.ifElse (Lists.null bins) (Flows.pure ([], ([], Substitution.idTypeSubst))) ( 
  let binding = (Lists.head bins) 
      k = (Core.letBindingName binding)
      v = (Core.letBindingTerm binding)
      tl = (Lists.tail bins)
  in (Flows.bind (inferTypeOfTerm cx v (Strings.cat [
    "temporary let binding '",
    Core.unName k,
    "'"])) (\result1 ->  
    let j = (Typing.inferenceResultTerm result1) 
        u_prime = (Typing.inferenceResultType result1)
        u = (Typing.inferenceResultSubst result1)
    in (Flows.bind (inferTypesOfTemporaryLetBindings (Substitution.substInContext u cx) tl) (\result2 ->  
      let h = (fst result2) 
          r_prime = (fst (snd result2))
          r = (snd (snd result2))
      in (Flows.pure (Lists.cons (Substitution.substTypesInTerm r j) h, (Lists.cons (Substitution.substInType r u_prime) r_prime, (Substitution.composeTypeSubst u r))))))))))

bindConstraints :: (Typing.InferenceContext -> (Typing.TypeSubst -> Compute.Flow t1 t0) -> [Typing.TypeConstraint] -> Compute.Flow t1 t0)
bindConstraints cx f constraints = (Flows.bind (Unification.unifyTypeConstraints (Typing.inferenceContextSchemaTypes cx) constraints) f)

forInferredTerm :: (Typing.InferenceContext -> Core.Term -> String -> (Typing.InferenceResult -> t0) -> Compute.Flow t1 t0)
forInferredTerm cx term desc f = (Flows.map f (inferTypeOfTerm cx term desc))

forVar :: ((Core.Name -> t0) -> Compute.Flow t1 t0)
forVar f = (Flows.map f freshName)

forVars :: (Int -> ([Core.Name] -> t0) -> Compute.Flow t1 t0)
forVars n f = (Flows.map f (freshNames n))

-- | Convert a forall type to a type scheme
fTypeToTypeScheme :: (Core.Type -> Core.TypeScheme)
fTypeToTypeScheme typ = (gatherForall [] typ)

instantiateFType :: (Core.Type -> Compute.Flow t0 Core.Type)
instantiateFType typ = (Flows.bind (instantiateTypeScheme (fTypeToTypeScheme typ)) (\ts -> Flows.pure (Core.typeSchemeType ts)))

instantiateTypeScheme :: (Core.TypeScheme -> Compute.Flow t0 Core.TypeScheme)
instantiateTypeScheme scheme =  
  let oldVars = (Core.typeSchemeVariables scheme)
  in (Flows.bind (freshNames (Lists.length oldVars)) (\newVars ->  
    let subst = (Typing.TypeSubst (Maps.fromList (Lists.zip oldVars (Lists.map (\x -> Core.TypeVariable x) newVars))))
    in (Flows.pure (Core.TypeScheme {
      Core.typeSchemeVariables = newVars,
      Core.typeSchemeType = (Substitution.substInType subst (Core.typeSchemeType scheme))}))))

mapConstraints :: (Typing.InferenceContext -> (Typing.TypeSubst -> t0) -> [Typing.TypeConstraint] -> Compute.Flow t1 t0)
mapConstraints cx f constraints = (Flows.map f (Unification.unifyTypeConstraints (Typing.inferenceContextSchemaTypes cx) constraints))

-- | Apply type arguments to a nominal type
nominalApplication :: (Core.Name -> [Core.Type] -> Core.Type)
nominalApplication tname args = (Lists.foldl (\t -> \a -> Core.TypeApplication (Core.ApplicationType {
  Core.applicationTypeFunction = t,
  Core.applicationTypeArgument = a})) (Core.TypeVariable tname) args)

requireSchemaType :: (Typing.InferenceContext -> Core.Name -> Compute.Flow t0 Core.TypeScheme)
requireSchemaType cx tname = (Optionals.maybe (Flows.fail (Strings.cat2 "No such schema type: " (Core.unName tname))) (\ts -> instantiateTypeScheme (Rewriting.stripTypeSchemeRecursive ts)) (Maps.lookup tname (Typing.inferenceContextSchemaTypes cx)))

-- | Add (term variable, type scheme) pairs to the typing environment
extendContext :: ([(Core.Name, Core.TypeScheme)] -> Typing.InferenceContext -> Typing.InferenceContext)
extendContext pairs cx = Typing.InferenceContext {
  Typing.inferenceContextSchemaTypes = (Typing.inferenceContextSchemaTypes cx),
  Typing.inferenceContextPrimitiveTypes = (Typing.inferenceContextPrimitiveTypes cx),
  Typing.inferenceContextDataTypes = (Maps.union (Maps.fromList pairs) (Typing.inferenceContextDataTypes cx)),
  Typing.inferenceContextDebug = (Typing.inferenceContextDebug cx)}

-- | Create an inference result
yield :: (Core.Term -> Core.Type -> Typing.TypeSubst -> Typing.InferenceResult)
yield term typ subst = Typing.InferenceResult {
  Typing.inferenceResultTerm = (Substitution.substTypesInTerm subst term),
  Typing.inferenceResultType = (Substitution.substInType subst typ),
  Typing.inferenceResultSubst = subst}

yieldChecked :: (t0 -> t1 -> Core.Term -> Core.Type -> Typing.TypeSubst -> Compute.Flow t2 Typing.InferenceResult)
yieldChecked cx vars term typ subst =  
  let iterm = (Substitution.substTypesInTerm subst term) 
      itype = (Substitution.substInType subst typ)
  in (Flows.pure (Typing.InferenceResult {
    Typing.inferenceResultTerm = iterm,
    Typing.inferenceResultType = itype,
    Typing.inferenceResultSubst = subst}))

yieldDebug :: (t0 -> t1 -> Core.Term -> Core.Type -> Typing.TypeSubst -> Compute.Flow t2 Typing.InferenceResult)
yieldDebug cx debugId term typ subst =  
  let rterm = (Substitution.substTypesInTerm subst term) 
      rtyp = (Substitution.substInType subst typ)
  in (Flows.bind (Annotations.debugIf debugId (Strings.cat [
    "\n\tterm: ",
    Core__.term term,
    "\n\ttyp: ",
    Core__.type_ typ,
    "\n\tsubst: ",
    Core__.typeSubst subst,
    "\n\trterm: ",
    Core__.term rterm,
    "\n\trtyp: ",
    (Core__.type_ rtyp)])) (\result -> Flows.pure (Typing.InferenceResult {
    Typing.inferenceResultTerm = rterm,
    Typing.inferenceResultType = rtyp,
    Typing.inferenceResultSubst = subst})))
