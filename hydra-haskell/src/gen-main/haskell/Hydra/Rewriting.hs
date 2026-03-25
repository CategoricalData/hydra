-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for type and term rewriting and analysis.

module Hydra.Rewriting where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
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
import qualified Hydra.Names as Names
import qualified Hydra.Sorting as Sorting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Apply a term-level function inside any leading type lambdas
applyInsideTypeLambdasAndAnnotations :: (Core.Term -> Core.Term) -> Core.Term -> Core.Term
applyInsideTypeLambdasAndAnnotations f term0 =
    case term0 of
      Core.TermAnnotated v0 -> Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (applyInsideTypeLambdasAndAnnotations f (Core.annotatedTermBody v0)),
        Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})
      Core.TermTypeLambda v0 -> Core.TermTypeLambda (Core.TypeLambda {
        Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
        Core.typeLambdaBody = (applyInsideTypeLambdasAndAnnotations f (Core.typeLambdaBody v0))})
      _ -> f term0

-- | Strip type annotations from the top levels of a term
deannotateAndDetypeTerm :: Core.Term -> Core.Term
deannotateAndDetypeTerm t =
    case t of
      Core.TermAnnotated v0 -> deannotateAndDetypeTerm (Core.annotatedTermBody v0)
      Core.TermTypeApplication v0 -> deannotateAndDetypeTerm (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> deannotateAndDetypeTerm (Core.typeLambdaBody v0)
      _ -> t

-- | Strip all annotations (including System F type annotations) from the top levels of a term
deannotateTerm :: Core.Term -> Core.Term
deannotateTerm t =
    case t of
      Core.TermAnnotated v0 -> deannotateTerm (Core.annotatedTermBody v0)
      _ -> t

-- | Strip all annotations from a term
deannotateType :: Core.Type -> Core.Type
deannotateType t =
    case t of
      Core.TypeAnnotated v0 -> deannotateType (Core.annotatedTypeBody v0)
      _ -> t

-- | Strip any top-level type lambdas from a type, extracting the (possibly nested) type body
deannotateTypeParameters :: Core.Type -> Core.Type
deannotateTypeParameters t =
    case (deannotateType t) of
      Core.TypeForall v0 -> deannotateTypeParameters (Core.forallTypeBody v0)
      _ -> t

-- | Recursively strip all annotations from a type
deannotateTypeRecursive :: Core.Type -> Core.Type
deannotateTypeRecursive typ =

      let strip =
              \recurse -> \typ ->
                let rewritten = recurse typ
                in case rewritten of
                  Core.TypeAnnotated v0 -> Core.annotatedTypeBody v0
                  _ -> rewritten
      in (rewriteType strip typ)

-- | Recursively strip all annotations from a type scheme
deannotateTypeSchemeRecursive :: Core.TypeScheme -> Core.TypeScheme
deannotateTypeSchemeRecursive ts =

      let vars = Core.typeSchemeVariables ts
          typ = Core.typeSchemeType ts
          constraints = Core.typeSchemeConstraints ts
      in Core.TypeScheme {
        Core.typeSchemeVariables = vars,
        Core.typeSchemeType = (deannotateTypeRecursive typ),
        Core.typeSchemeConstraints = constraints}

-- | Strip System F type annotations from the top levels of a term, but leave application-specific annotations intact
detypeTerm :: Core.Term -> Core.Term
detypeTerm t =
    case t of
      Core.TermAnnotated v0 ->
        let subj = Core.annotatedTermBody v0
            ann = Core.annotatedTermAnnotation v0
        in (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (detypeTerm subj),
          Core.annotatedTermAnnotation = ann}))
      Core.TermTypeApplication v0 -> deannotateAndDetypeTerm (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> deannotateAndDetypeTerm (Core.typeLambdaBody v0)
      _ -> t

-- | Extend a graph by descending into a lambda body
extendGraphForLambda :: Graph.Graph -> Core.Lambda -> Graph.Graph
extendGraphForLambda g lam =

      let var = Core.lambdaParameter lam
      in Graph.Graph {
        Graph.graphBoundTerms = (Graph.graphBoundTerms g),
        Graph.graphBoundTypes = (Maybes.maybe (Graph.graphBoundTypes g) (\dom -> Maps.insert var (fTypeToTypeScheme dom) (Graph.graphBoundTypes g)) (Core.lambdaDomain lam)),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Sets.insert var (Graph.graphLambdaVariables g)),
        Graph.graphMetadata = (Maps.delete var (Graph.graphMetadata g)),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Graph.graphTypeVariables g)}

-- | Extend a graph by descending into a let body
extendGraphForLet :: (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> Graph.Graph -> Core.Let -> Graph.Graph
extendGraphForLet forBinding g letrec =

      let bindings = Core.letBindings letrec
          g2 = extendGraphWithBindings bindings g
      in Graph.Graph {
        Graph.graphBoundTerms = (Maps.union (Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) bindings)) (Graph.graphBoundTerms g)),
        Graph.graphBoundTypes = (Maps.union (Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingType b)) bindings))) (Graph.graphBoundTypes g)),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Lists.foldl (\s -> \b -> Sets.delete (Core.bindingName b) s) (Graph.graphLambdaVariables g) bindings),
        Graph.graphMetadata = (Graph.graphMetadata (Lists.foldl (\gAcc -> \b ->
          let m = Graph.graphMetadata gAcc
              newMeta = Maybes.maybe (Maps.delete (Core.bindingName b) m) (\t -> Maps.insert (Core.bindingName b) t m) (forBinding gAcc b)
          in Graph.Graph {
            Graph.graphBoundTerms = (Graph.graphBoundTerms gAcc),
            Graph.graphBoundTypes = (Graph.graphBoundTypes gAcc),
            Graph.graphClassConstraints = (Graph.graphClassConstraints gAcc),
            Graph.graphLambdaVariables = (Graph.graphLambdaVariables gAcc),
            Graph.graphMetadata = newMeta,
            Graph.graphPrimitives = (Graph.graphPrimitives gAcc),
            Graph.graphSchemaTypes = (Graph.graphSchemaTypes gAcc),
            Graph.graphTypeVariables = (Graph.graphTypeVariables gAcc)}) g2 bindings)),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Graph.graphTypeVariables g)}

-- | Extend a graph by descending into a type lambda body
extendGraphForTypeLambda :: Graph.Graph -> Core.TypeLambda -> Graph.Graph
extendGraphForTypeLambda g tlam =

      let name = Core.typeLambdaParameter tlam
      in Graph.Graph {
        Graph.graphBoundTerms = (Graph.graphBoundTerms g),
        Graph.graphBoundTypes = (Graph.graphBoundTypes g),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
        Graph.graphMetadata = (Graph.graphMetadata g),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Sets.insert name (Graph.graphTypeVariables g))}

-- | Add bindings to an existing graph
extendGraphWithBindings :: [Core.Binding] -> Graph.Graph -> Graph.Graph
extendGraphWithBindings bindings g =

      let newTerms = Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) bindings)
          newTypes =
                  Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingType b)) bindings))
      in Graph.Graph {
        Graph.graphBoundTerms = (Maps.union newTerms (Graph.graphBoundTerms g)),
        Graph.graphBoundTypes = (Maps.union newTypes (Graph.graphBoundTypes g)),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
        Graph.graphMetadata = (Graph.graphMetadata g),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Graph.graphTypeVariables g)}

-- | Convert a forall type to a type scheme
fTypeToTypeScheme :: Core.Type -> Core.TypeScheme
fTypeToTypeScheme typ =

      let gatherForall =
              \vars -> \typ -> case (deannotateType typ) of
                Core.TypeForall v0 -> gatherForall (Lists.cons (Core.forallTypeParameter v0) vars) (Core.forallTypeBody v0)
                _ -> Core.TypeScheme {
                  Core.typeSchemeVariables = (Lists.reverse vars),
                  Core.typeSchemeType = typ,
                  Core.typeSchemeConstraints = Nothing}
      in (gatherForall [] typ)

-- | Flatten nested let expressions
flattenLetTerms :: Core.Term -> Core.Term
flattenLetTerms term =

      let rewriteBinding =
              \binding ->
                let key0 = Core.bindingName binding
                    val0 = Core.bindingTerm binding
                    t = Core.bindingType binding
                in case val0 of
                  Core.TermAnnotated v0 ->
                    let val1 = Core.annotatedTermBody v0
                        ann = Core.annotatedTermAnnotation v0
                        recursive =
                                rewriteBinding (Core.Binding {
                                  Core.bindingName = key0,
                                  Core.bindingTerm = val1,
                                  Core.bindingType = t})
                        innerBinding = Pairs.first recursive
                        deps = Pairs.second recursive
                        val2 = Core.bindingTerm innerBinding
                    in (Core.Binding {
                      Core.bindingName = key0,
                      Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = val2,
                        Core.annotatedTermAnnotation = ann})),
                      Core.bindingType = t}, deps)
                  Core.TermLet v0 ->
                    let bindings1 = Core.letBindings v0
                        body1 = Core.letBody v0
                        prefix = Strings.cat2 (Core.unName key0) "_"
                        qualify = \n -> Core.Name (Strings.cat2 prefix (Core.unName n))
                        toSubstPair = \b -> (Core.bindingName b, (qualify (Core.bindingName b)))
                        subst = Maps.fromList (Lists.map toSubstPair bindings1)
                        replaceVars = substituteVariables subst
                        newBody = replaceVars body1
                        newBinding =
                                \b -> Core.Binding {
                                  Core.bindingName = (qualify (Core.bindingName b)),
                                  Core.bindingTerm = (replaceVars (Core.bindingTerm b)),
                                  Core.bindingType = (Core.bindingType b)}
                    in (Core.Binding {
                      Core.bindingName = key0,
                      Core.bindingTerm = newBody,
                      Core.bindingType = t}, (Lists.map newBinding bindings1))
                  _ -> (Core.Binding {
                    Core.bindingName = key0,
                    Core.bindingTerm = val0,
                    Core.bindingType = t}, [])
          flattenBodyLet =
                  \bindings -> \body -> case body of
                    Core.TermLet v0 ->
                      let innerBindings = Core.letBindings v0
                          innerBody = Core.letBody v0
                      in (flattenBodyLet (Lists.concat2 bindings innerBindings) innerBody)
                    _ -> (Lists.concat2 [] bindings, body)
          flatten =
                  \recurse -> \term ->
                    let rewritten = recurse term
                    in case rewritten of
                      Core.TermLet v0 ->
                        let bindings = Core.letBindings v0
                            body = Core.letBody v0
                            forResult = \hr -> Lists.concat2 (Pairs.second hr) (Lists.pure (Pairs.first hr))
                            flattenedBindings = Lists.concat (Lists.map (\arg_ -> forResult (rewriteBinding arg_)) bindings)
                            merged = flattenBodyLet flattenedBindings body
                            newBindings = Pairs.first merged
                            newBody = Pairs.second merged
                        in (Core.TermLet (Core.Let {
                          Core.letBindings = newBindings,
                          Core.letBody = newBody}))
                      _ -> rewritten
      in (rewriteTerm flatten term)

-- | Fold over a term, traversing its subterms in the specified order
foldOverTerm :: Coders.TraversalOrder -> (t0 -> Core.Term -> t0) -> t0 -> Core.Term -> t0
foldOverTerm order fld b0 term =
    case order of
      Coders.TraversalOrderPre -> Lists.foldl (foldOverTerm order fld) (fld b0 term) (subterms term)
      Coders.TraversalOrderPost -> fld (Lists.foldl (foldOverTerm order fld) b0 (subterms term)) term

-- | Fold over a type, traversing its subtypes in the specified order
foldOverType :: Coders.TraversalOrder -> (t0 -> Core.Type -> t0) -> t0 -> Core.Type -> t0
foldOverType order fld b0 typ =
    case order of
      Coders.TraversalOrderPre -> Lists.foldl (foldOverType order fld) (fld b0 typ) (subtypes typ)
      Coders.TraversalOrderPost -> fld (Lists.foldl (foldOverType order fld) b0 (subtypes typ)) typ

-- | Fold over a term to produce a value, with both Graph and accessor path tracked. Like rewriteAndFoldTermWithGraphAndPath, but only folds without rewriting. The Graph is automatically updated when descending into lambdas, lets, and type lambdas.
foldTermWithGraphAndPath :: ((t0 -> Core.Term -> t0) -> [Accessors.TermAccessor] -> Graph.Graph -> t0 -> Core.Term -> t0) -> Graph.Graph -> t0 -> Core.Term -> t0
foldTermWithGraphAndPath f cx0 val0 term0 =

      let wrapper =
              \recurse -> \path -> \cx -> \val -> \term ->
                let recurseForUser =
                        \valIn -> \subterm ->
                          let r = recurse valIn subterm
                          in (Pairs.first r)
                in (f recurseForUser path cx val term, term)
          result = rewriteAndFoldTermWithGraphAndPath wrapper cx0 val0 term0
      in (Pairs.first result)

-- | Get the set of free type variables in a term (including schema names, where they appear in type annotations). In this context, only the type schemes of let bindings can bind type variables; type lambdas do not.
freeTypeVariablesInTerm :: Core.Term -> S.Set Core.Name
freeTypeVariablesInTerm term0 =

      let allOf = \sets -> Lists.foldl Sets.union Sets.empty sets
          tryType = \tvars -> \typ -> Sets.difference (freeVariablesInType typ) tvars
          getAll =
                  \vars -> \term ->
                    let recurse = getAll vars
                        dflt = allOf (Lists.map recurse (subterms term))
                    in case term of
                      Core.TermFunction v0 -> case v0 of
                        Core.FunctionElimination _ -> dflt
                        Core.FunctionLambda v1 ->
                          let domt = Maybes.maybe Sets.empty (tryType vars) (Core.lambdaDomain v1)
                          in (Sets.union domt (recurse (Core.lambdaBody v1)))
                        _ -> dflt
                      Core.TermLet v0 ->
                        let forBinding =
                                \b ->
                                  let newVars = Maybes.maybe vars (\ts -> Sets.union vars (Sets.fromList (Core.typeSchemeVariables ts))) (Core.bindingType b)
                                  in (Sets.union (getAll newVars (Core.bindingTerm b)) (Maybes.maybe Sets.empty (\ts -> tryType newVars (Core.typeSchemeType ts)) (Core.bindingType b)))
                        in (Sets.union (allOf (Lists.map forBinding (Core.letBindings v0))) (recurse (Core.letBody v0)))
                      Core.TermTypeApplication v0 -> Sets.union (tryType vars (Core.typeApplicationTermType v0)) (recurse (Core.typeApplicationTermBody v0))
                      Core.TermTypeLambda v0 -> Sets.union (tryType vars (Core.TypeVariable (Core.typeLambdaParameter v0))) (recurse (Core.typeLambdaBody v0))
                      _ -> dflt
      in (getAll Sets.empty term0)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a term
freeVariablesInTerm :: Core.Term -> S.Set Core.Name
freeVariablesInTerm term =

      let dfltVars = \_ -> Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInTerm t)) Sets.empty (subterms term)
      in case term of
        Core.TermFunction v0 -> case v0 of
          Core.FunctionLambda v1 -> Sets.delete (Core.lambdaParameter v1) (freeVariablesInTerm (Core.lambdaBody v1))
          _ -> dfltVars ()
        Core.TermLet v0 -> Sets.difference (dfltVars ()) (Sets.fromList (Lists.map Core.bindingName (Core.letBindings v0)))
        Core.TermVariable v0 -> Sets.singleton v0
        _ -> dfltVars ()

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a type
freeVariablesInType :: Core.Type -> S.Set Core.Name
freeVariablesInType typ =

      let dfltVars = Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInType t)) Sets.empty (subtypes typ)
      in case typ of
        Core.TypeForall v0 -> Sets.delete (Core.forallTypeParameter v0) (freeVariablesInType (Core.forallTypeBody v0))
        Core.TypeVariable v0 -> Sets.singleton v0
        _ -> dfltVars

-- | Find the free variables in a type in deterministic left-to-right order
freeVariablesInTypeOrdered :: Core.Type -> [Core.Name]
freeVariablesInTypeOrdered typ =

      let collectVars =
              \boundVars -> \t -> case t of
                Core.TypeVariable v0 -> Logic.ifElse (Sets.member v0 boundVars) [] [
                  v0]
                Core.TypeForall v0 -> collectVars (Sets.insert (Core.forallTypeParameter v0) boundVars) (Core.forallTypeBody v0)
                _ -> Lists.concat (Lists.map (collectVars boundVars) (subtypes t))
      in (Lists.nub (collectVars Sets.empty typ))

-- | Find free variables in a type scheme
freeVariablesInTypeScheme :: Core.TypeScheme -> S.Set Core.Name
freeVariablesInTypeScheme ts =

      let vars = Core.typeSchemeVariables ts
          t = Core.typeSchemeType ts
      in (Sets.difference (freeVariablesInType t) (Sets.fromList vars))

-- | Find free variables in a type scheme (simple version)
freeVariablesInTypeSchemeSimple :: Core.TypeScheme -> S.Set Core.Name
freeVariablesInTypeSchemeSimple ts =

      let vars = Core.typeSchemeVariables ts
          t = Core.typeSchemeType ts
      in (Sets.difference (freeVariablesInTypeSimple t) (Sets.fromList vars))

-- | Same as freeVariablesInType, but ignores the binding action of lambda types
freeVariablesInTypeSimple :: Core.Type -> S.Set Core.Name
freeVariablesInTypeSimple typ =

      let helper =
              \types -> \typ -> case typ of
                Core.TypeVariable v0 -> Sets.insert v0 types
                _ -> types
      in (foldOverType Coders.TraversalOrderPre helper Sets.empty typ)

-- | Inline all type variables in a type using the provided schema (Either version). Note: this function is only appropriate for nonrecursive type definitions
inlineType :: M.Map Core.Name Core.Type -> Core.Type -> Either String Core.Type
inlineType schema typ =

      let f =
              \recurse -> \typ ->
                let afterRecurse =
                        \tr -> case tr of
                          Core.TypeVariable v0 -> Maybes.maybe (Left (Strings.cat2 "No such type in schema: " (Core.unName v0))) (inlineType schema) (Maps.lookup v0 schema)
                          _ -> Right tr
                in (Eithers.bind (recurse typ) (\tr -> afterRecurse tr))
      in (rewriteTypeM f typ)

-- | Check whether a variable is free (not bound) in a term
isFreeVariableInTerm :: Core.Name -> Core.Term -> Bool
isFreeVariableInTerm v term = Logic.not (Sets.member v (freeVariablesInTerm term))

-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: Core.Term -> Bool
isLambda term =
    case (deannotateTerm term) of
      Core.TermFunction v0 -> case v0 of
        Core.FunctionLambda _ -> True
        _ -> False
      Core.TermLet v0 -> isLambda (Core.letBody v0)
      _ -> False

-- | Rewrite terms like `let foo = bar in λx.baz` to `λx.let foo = bar in baz`, lifting lambda-bound variables above let-bound variables, recursively. This is helpful for targets such as Python.
liftLambdaAboveLet :: Core.Term -> Core.Term
liftLambdaAboveLet term0 =

      let rewrite =
              \recurse -> \term ->
                let rewriteBinding =
                        \b -> Core.Binding {
                          Core.bindingName = (Core.bindingName b),
                          Core.bindingTerm = (rewrite recurse (Core.bindingTerm b)),
                          Core.bindingType = (Core.bindingType b)}
                    rewriteBindings = \bs -> Lists.map rewriteBinding bs
                    digForLambdas =
                            \original -> \cons -> \term -> case term of
                              Core.TermAnnotated v0 -> digForLambdas original (\t -> Core.TermAnnotated (Core.AnnotatedTerm {
                                Core.annotatedTermBody = (cons t),
                                Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})) (Core.annotatedTermBody v0)
                              Core.TermFunction v0 -> case v0 of
                                Core.FunctionLambda v1 -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.lambdaParameter v1),
                                  Core.lambdaDomain = (Core.lambdaDomain v1),
                                  Core.lambdaBody = (digForLambdas (cons (Core.lambdaBody v1)) (\t -> cons t) (Core.lambdaBody v1))}))
                                _ -> recurse original
                              Core.TermLet v0 -> digForLambdas original (\t -> cons (Core.TermLet (Core.Let {
                                Core.letBindings = (rewriteBindings (Core.letBindings v0)),
                                Core.letBody = t}))) (Core.letBody v0)
                              _ -> recurse original
                in case term of
                  Core.TermLet v0 -> digForLambdas term (\t -> Core.TermLet (Core.Let {
                    Core.letBindings = (rewriteBindings (Core.letBindings v0)),
                    Core.letBody = t})) (Core.letBody v0)
                  _ -> recurse term
      in (rewriteTerm rewrite term0)

-- | Apply a transformation to the first type beneath a chain of annotations
mapBeneathTypeAnnotations :: (Core.Type -> Core.Type) -> Core.Type -> Core.Type
mapBeneathTypeAnnotations f t =
    case t of
      Core.TypeAnnotated v0 -> Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (mapBeneathTypeAnnotations f (Core.annotatedTypeBody v0)),
        Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v0)})
      _ -> f t

-- | Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ...
normalizeTypeVariablesInTerm :: Core.Term -> Core.Term
normalizeTypeVariablesInTerm term =

      let replaceName = \subst -> \v -> Maybes.fromMaybe v (Maps.lookup v subst)
          substType =
                  \subst -> \typ ->
                    let rewrite =
                            \recurse -> \typ -> case typ of
                              Core.TypeVariable v0 -> Core.TypeVariable (replaceName subst v0)
                              _ -> recurse typ
                    in (rewriteType rewrite typ)
          rewriteWithSubst =
                  \state -> \term0 ->
                    let sb = Pairs.first state
                        next = Pairs.second state
                        subst = Pairs.first sb
                        boundVars = Pairs.second sb
                        rewrite =
                                \recurse -> \term -> case term of
                                  Core.TermFunction v0 -> case v0 of
                                    Core.FunctionElimination _ -> recurse term
                                    Core.FunctionLambda v1 ->
                                      let domain = Core.lambdaDomain v1
                                      in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.lambdaParameter v1),
                                        Core.lambdaDomain = (Maybes.map (substType subst) domain),
                                        Core.lambdaBody = (rewriteWithSubst ((subst, boundVars), next) (Core.lambdaBody v1))})))
                                    _ -> recurse term
                                  Core.TermLet v0 ->
                                    let bindings0 = Core.letBindings v0
                                        body0 = Core.letBody v0
                                        step =
                                                \acc -> \bs -> Logic.ifElse (Lists.null bs) (Lists.reverse acc) (
                                                  let b = Lists.head bs
                                                      tl = Lists.tail bs
                                                      noType =

                                                                let newVal = rewriteWithSubst ((subst, boundVars), next) (Core.bindingTerm b)
                                                                    b1 =
                                                                            Core.Binding {
                                                                              Core.bindingName = (Core.bindingName b),
                                                                              Core.bindingTerm = newVal,
                                                                              Core.bindingType = Nothing}
                                                                in (step (Lists.cons b1 acc) tl)
                                                      withType =
                                                              \ts ->
                                                                let vars = Core.typeSchemeVariables ts
                                                                    typ = Core.typeSchemeType ts
                                                                    k = Lists.length vars
                                                                    gen =
                                                                            \i -> \rem -> \acc2 ->
                                                                              let ti = Core.Name (Strings.cat2 "t" (Literals.showInt32 (Math.add next i)))
                                                                              in (Logic.ifElse (Equality.equal rem 0) (Lists.reverse acc2) (gen (Math.add i 1) (Math.sub rem 1) (Lists.cons ti acc2)))
                                                                    newVars = gen 0 k []
                                                                    newSubst = Maps.union (Maps.fromList (Lists.zip vars newVars)) subst
                                                                    newBound = Sets.union boundVars (Sets.fromList newVars)
                                                                    newVal = rewriteWithSubst ((newSubst, newBound), (Math.add next k)) (Core.bindingTerm b)
                                                                    renameConstraintKeys =
                                                                            \constraintMap -> Maps.fromList (Lists.map (\p ->
                                                                              let oldName = Pairs.first p
                                                                                  meta = Pairs.second p
                                                                                  newName = Maybes.fromMaybe oldName (Maps.lookup oldName newSubst)
                                                                              in (newName, meta)) (Maps.toList constraintMap))
                                                                    oldConstraints = Core.typeSchemeConstraints ts
                                                                    newConstraints = Maybes.map renameConstraintKeys oldConstraints
                                                                    b1 =
                                                                            Core.Binding {
                                                                              Core.bindingName = (Core.bindingName b),
                                                                              Core.bindingTerm = newVal,
                                                                              Core.bindingType = (Just (Core.TypeScheme {
                                                                                Core.typeSchemeVariables = newVars,
                                                                                Core.typeSchemeType = (substType newSubst typ),
                                                                                Core.typeSchemeConstraints = newConstraints}))}
                                                                in (step (Lists.cons b1 acc) tl)
                                                  in (Maybes.maybe noType (\ts -> withType ts) (Core.bindingType b)))
                                        bindings1 = step [] bindings0
                                    in (Core.TermLet (Core.Let {
                                      Core.letBindings = bindings1,
                                      Core.letBody = (rewriteWithSubst ((subst, boundVars), next) body0)}))
                                  Core.TermTypeApplication v0 -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (rewriteWithSubst ((subst, boundVars), next) (Core.typeApplicationTermBody v0)),
                                    Core.typeApplicationTermType = (substType subst (Core.typeApplicationTermType v0))})
                                  Core.TermTypeLambda v0 -> Core.TermTypeLambda (Core.TypeLambda {
                                    Core.typeLambdaParameter = (replaceName subst (Core.typeLambdaParameter v0)),
                                    Core.typeLambdaBody = (rewriteWithSubst ((subst, boundVars), next) (Core.typeLambdaBody v0))})
                                  _ -> recurse term
                    in (rewriteTerm rewrite term0)
      in (rewriteWithSubst ((Maps.empty, Sets.empty), 0) term)

-- | Given a let expression, remove any unused bindings. The resulting expression is still a let, even if has no remaining bindings
pruneLet :: Core.Let -> Core.Let
pruneLet l =

      let bindingMap = Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) (Core.letBindings l))
          rootName = Core.Name "[[[root]]]"
          adj =
                  \n -> Sets.intersection (Sets.fromList (Maps.keys bindingMap)) (freeVariablesInTerm (Logic.ifElse (Equality.equal n rootName) (Core.letBody l) (Maybes.fromJust (Maps.lookup n bindingMap))))
          reachable = Sorting.findReachableNodes adj rootName
          prunedBindings = Lists.filter (\b -> Sets.member (Core.bindingName b) reachable) (Core.letBindings l)
      in Core.Let {
        Core.letBindings = prunedBindings,
        Core.letBody = (Core.letBody l)}

-- | Recursively remove term annotations, including within subterms
removeTermAnnotations :: Core.Term -> Core.Term
removeTermAnnotations term =

      let remove =
              \recurse -> \term ->
                let rewritten = recurse term
                in case term of
                  Core.TermAnnotated v0 -> Core.annotatedTermBody v0
                  _ -> rewritten
      in (rewriteTerm remove term)

-- | Recursively remove type annotations, including within subtypes
removeTypeAnnotations :: Core.Type -> Core.Type
removeTypeAnnotations typ =

      let remove =
              \recurse -> \typ ->
                let rewritten = recurse typ
                in case rewritten of
                  Core.TypeAnnotated v0 -> Core.annotatedTypeBody v0
                  _ -> rewritten
      in (rewriteType remove typ)

-- | Strip type annotations (TypeLambda, TypeApplication, binding type schemes) from terms while preserving lambda domain types and other annotations
removeTypeAnnotationsFromTerm :: Core.Term -> Core.Term
removeTypeAnnotationsFromTerm term =

      let strip =
              \recurse -> \term ->
                let rewritten = recurse term
                    stripBinding =
                            \b -> Core.Binding {
                              Core.bindingName = (Core.bindingName b),
                              Core.bindingTerm = (Core.bindingTerm b),
                              Core.bindingType = Nothing}
                in case rewritten of
                  Core.TermLet v0 -> Core.TermLet (Core.Let {
                    Core.letBindings = (Lists.map stripBinding (Core.letBindings v0)),
                    Core.letBody = (Core.letBody v0)})
                  Core.TermTypeApplication v0 -> Core.typeApplicationTermBody v0
                  Core.TermTypeLambda v0 -> Core.typeLambdaBody v0
                  _ -> rewritten
      in (rewriteTerm strip term)

-- | Strip type annotations from terms while preserving other annotations
removeTypesFromTerm :: Core.Term -> Core.Term
removeTypesFromTerm term =

      let strip =
              \recurse -> \term ->
                let rewritten = recurse term
                    stripBinding =
                            \b -> Core.Binding {
                              Core.bindingName = (Core.bindingName b),
                              Core.bindingTerm = (Core.bindingTerm b),
                              Core.bindingType = Nothing}
                in case rewritten of
                  Core.TermFunction v0 -> case v0 of
                    Core.FunctionElimination v1 -> Core.TermFunction (Core.FunctionElimination v1)
                    Core.FunctionLambda v1 -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.lambdaParameter v1),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.lambdaBody v1)}))
                    _ -> Core.TermFunction v0
                  Core.TermLet v0 -> Core.TermLet (Core.Let {
                    Core.letBindings = (Lists.map stripBinding (Core.letBindings v0)),
                    Core.letBody = (Core.letBody v0)})
                  Core.TermTypeApplication v0 -> Core.typeApplicationTermBody v0
                  Core.TermTypeLambda v0 -> Core.typeLambdaBody v0
                  _ -> rewritten
      in (rewriteTerm strip term)

-- | Replace a free variable in a term
replaceFreeTermVariable :: Core.Name -> Core.Term -> Core.Term -> Core.Term
replaceFreeTermVariable vold tnew term =

      let rewrite =
              \recurse -> \t -> case t of
                Core.TermFunction v0 -> case v0 of
                  Core.FunctionLambda v1 ->
                    let v = Core.lambdaParameter v1
                    in (Logic.ifElse (Equality.equal v vold) t (recurse t))
                  _ -> recurse t
                Core.TermVariable v0 -> Logic.ifElse (Equality.equal v0 vold) tnew (Core.TermVariable v0)
                _ -> recurse t
      in (rewriteTerm rewrite term)

-- | Replace free occurrences of a name in a type
replaceFreeTypeVariable :: Core.Name -> Core.Type -> Core.Type -> Core.Type
replaceFreeTypeVariable v rep typ =

      let mapExpr =
              \recurse -> \t -> case t of
                Core.TypeForall v0 -> Logic.ifElse (Equality.equal v (Core.forallTypeParameter v0)) t (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.forallTypeParameter v0),
                  Core.forallTypeBody = (recurse (Core.forallTypeBody v0))}))
                Core.TypeVariable v0 -> Logic.ifElse (Equality.equal v v0) rep t
                _ -> recurse t
      in (rewriteType mapExpr typ)

-- | Replace all occurrences of simple typedefs (type aliases) with the aliased types, recursively
replaceTypedefs :: M.Map Core.Name Core.TypeScheme -> Core.Type -> Core.Type
replaceTypedefs types typ0 =

      let rewrite =
              \recurse -> \typ -> case typ of
                Core.TypeAnnotated v0 -> Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (rewrite recurse (Core.annotatedTypeBody v0)),
                  Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v0)})
                Core.TypeRecord _ -> typ
                Core.TypeUnion _ -> typ
                Core.TypeVariable v0 ->
                  let forMono =
                          \t -> case t of
                            Core.TypeRecord _ -> typ
                            Core.TypeUnion _ -> typ
                            Core.TypeWrap _ -> typ
                            _ -> rewrite recurse t
                      forTypeScheme =
                              \ts ->
                                let t = Core.typeSchemeType ts
                                in (Logic.ifElse (Lists.null (Core.typeSchemeVariables ts)) (forMono t) typ)
                  in (Maybes.maybe typ (\ts -> forTypeScheme ts) (Maps.lookup v0 types))
                Core.TypeWrap _ -> typ
                _ -> recurse typ
      in (rewriteType rewrite typ0)

-- | Rewrite a term, and at the same time, fold a function over it, accumulating a value
rewriteAndFoldTerm :: ((t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term)
rewriteAndFoldTerm f term0 =

      let fsub =
              \recurse -> \val0 -> \term0 ->
                let forSingle =
                        \rec -> \cons -> \val -> \term ->
                          let r = rec val term
                          in (Pairs.first r, (cons (Pairs.second r)))
                    forMany =
                            \rec -> \cons -> \val -> \els ->
                              let rr =
                                      Lists.foldl (\r -> \el ->
                                        let r2 = rec (Pairs.first r) el
                                        in (Pairs.first r2, (Lists.cons (Pairs.second r2) (Pairs.second r)))) (val, []) els
                              in (Pairs.first rr, (cons (Lists.reverse (Pairs.second rr))))
                    forField =
                            \val -> \field ->
                              let r = recurse val (Core.fieldTerm field)
                              in (Pairs.first r, Core.Field {
                                Core.fieldName = (Core.fieldName field),
                                Core.fieldTerm = (Pairs.second r)})
                    forFields = forMany forField (\x -> x)
                    forPair =
                            \val -> \kv ->
                              let rk = recurse val (Pairs.first kv)
                                  rv = recurse (Pairs.first rk) (Pairs.second kv)
                              in (Pairs.first rv, (Pairs.second rk, (Pairs.second rv)))
                    forBinding =
                            \val -> \binding ->
                              let r = recurse val (Core.bindingTerm binding)
                              in (Pairs.first r, Core.Binding {
                                Core.bindingName = (Core.bindingName binding),
                                Core.bindingTerm = (Pairs.second r),
                                Core.bindingType = (Core.bindingType binding)})
                    forElimination =
                            \val -> \elm ->
                              let r =
                                      case elm of
                                        Core.EliminationUnion v0 ->
                                          let rmd = Maybes.map (recurse val) (Core.caseStatementDefault v0)
                                              val1 = Maybes.maybe val Pairs.first rmd
                                              rcases = forFields val1 (Core.caseStatementCases v0)
                                          in (Pairs.first rcases, (Core.EliminationUnion (Core.CaseStatement {
                                            Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                                            Core.caseStatementDefault = (Maybes.map Pairs.second rmd),
                                            Core.caseStatementCases = (Pairs.second rcases)})))
                                        _ -> (val, elm)
                              in (Pairs.first r, (Pairs.second r))
                    forFunction =
                            \val -> \fun -> case fun of
                              Core.FunctionElimination v0 ->
                                let re = forElimination val v0
                                in (Pairs.first re, (Core.FunctionElimination (Pairs.second re)))
                              Core.FunctionLambda v0 ->
                                let rl = recurse val (Core.lambdaBody v0)
                                in (Pairs.first rl, (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.lambdaParameter v0),
                                  Core.lambdaDomain = (Core.lambdaDomain v0),
                                  Core.lambdaBody = (Pairs.second rl)})))
                              _ -> (val, fun)
                    dflt = (val0, term0)
                in case term0 of
                  Core.TermAnnotated v0 -> forSingle recurse (\t -> Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = t,
                    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})) val0 (Core.annotatedTermBody v0)
                  Core.TermApplication v0 ->
                    let rlhs = recurse val0 (Core.applicationFunction v0)
                        rrhs = recurse (Pairs.first rlhs) (Core.applicationArgument v0)
                    in (Pairs.first rrhs, (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Pairs.second rlhs),
                      Core.applicationArgument = (Pairs.second rrhs)})))
                  Core.TermEither v0 -> Eithers.either (\l ->
                    let rl = recurse val0 l
                    in (Pairs.first rl, (Core.TermEither (Left (Pairs.second rl))))) (\r ->
                    let rr = recurse val0 r
                    in (Pairs.first rr, (Core.TermEither (Right (Pairs.second rr))))) v0
                  Core.TermFunction v0 -> forSingle forFunction (\f -> Core.TermFunction f) val0 v0
                  Core.TermLet v0 ->
                    let renv = recurse val0 (Core.letBody v0)
                    in (forMany forBinding (\bins -> Core.TermLet (Core.Let {
                      Core.letBindings = bins,
                      Core.letBody = (Pairs.second renv)})) (Pairs.first renv) (Core.letBindings v0))
                  Core.TermList v0 -> forMany recurse (\x -> Core.TermList x) val0 v0
                  Core.TermMap v0 -> forMany forPair (\pairs -> Core.TermMap (Maps.fromList pairs)) val0 (Maps.toList v0)
                  Core.TermMaybe v0 -> Maybes.maybe dflt (\t -> forSingle recurse (\t1 -> Core.TermMaybe (Just t1)) val0 t) v0
                  Core.TermPair v0 ->
                    let rf = recurse val0 (Pairs.first v0)
                        rs = recurse (Pairs.first rf) (Pairs.second v0)
                    in (Pairs.first rs, (Core.TermPair (Pairs.second rf, (Pairs.second rs))))
                  Core.TermRecord v0 -> forMany forField (\fields -> Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.recordTypeName v0),
                    Core.recordFields = fields})) val0 (Core.recordFields v0)
                  Core.TermSet v0 -> forMany recurse (\e -> Core.TermSet (Sets.fromList e)) val0 (Sets.toList v0)
                  Core.TermTypeApplication v0 -> forSingle recurse (\t -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = t,
                    Core.typeApplicationTermType = (Core.typeApplicationTermType v0)})) val0 (Core.typeApplicationTermBody v0)
                  Core.TermTypeLambda v0 -> forSingle recurse (\t -> Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
                    Core.typeLambdaBody = t})) val0 (Core.typeLambdaBody v0)
                  Core.TermUnion v0 -> forSingle recurse (\t -> Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.injectionTypeName v0),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.fieldName (Core.injectionField v0)),
                      Core.fieldTerm = t}})) val0 (Core.fieldTerm (Core.injectionField v0))
                  Core.TermWrap v0 -> forSingle recurse (\t -> Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.wrappedTermTypeName v0),
                    Core.wrappedTermBody = t})) val0 (Core.wrappedTermBody v0)
                  _ -> dflt
          recurse = f (fsub recurse)
      in (recurse term0)

-- | Rewrite a term while folding to produce a value, with Graph updated as we descend into subterms. Combines the features of rewriteAndFoldTerm and rewriteTermWithGraph. The user function f receives a recurse function that handles subterm traversal and Graph management.
rewriteAndFoldTermWithGraph :: ((t0 -> Core.Term -> (t0, Core.Term)) -> Graph.Graph -> t0 -> Core.Term -> (t0, Core.Term)) -> Graph.Graph -> t0 -> Core.Term -> (t0, Core.Term)
rewriteAndFoldTermWithGraph f cx0 val0 term0 =

      let wrapper =
              \lowLevelRecurse -> \valAndCx -> \term ->
                let val = Pairs.first valAndCx
                    cx = Pairs.second valAndCx
                    cx1 =
                            case term of
                              Core.TermFunction v0 -> case v0 of
                                Core.FunctionLambda v1 -> extendGraphForLambda cx v1
                                _ -> cx
                              Core.TermLet v0 -> extendGraphForLet (\_ -> \_ -> Nothing) cx v0
                              Core.TermTypeLambda v0 -> extendGraphForTypeLambda cx v0
                              _ -> cx
                    recurseForUser =
                            \newVal -> \subterm ->
                              let result = lowLevelRecurse (newVal, cx1) subterm
                              in (Pairs.first (Pairs.first result), (Pairs.second result))
                    fResult = f recurseForUser cx1 val term
                in ((Pairs.first fResult, cx), (Pairs.second fResult))
          result = rewriteAndFoldTerm wrapper (val0, cx0) term0
      in (Pairs.first (Pairs.first result), (Pairs.second result))

-- | Rewrite a term while folding to produce a value, with both Graph and accessor path tracked. The path is a list of TermAccessors representing the position from the root to the current term. Combines the features of rewriteAndFoldTermWithPath and Graph tracking. The Graph is automatically updated when descending into lambdas, lets, and type lambdas.
rewriteAndFoldTermWithGraphAndPath :: ((t0 -> Core.Term -> (t0, Core.Term)) -> [Accessors.TermAccessor] -> Graph.Graph -> t0 -> Core.Term -> (t0, Core.Term)) -> Graph.Graph -> t0 -> Core.Term -> (t0, Core.Term)
rewriteAndFoldTermWithGraphAndPath f cx0 val0 term0 =

      let wrapper =
              \recurse -> \path -> \cxAndVal -> \term ->
                let cx = Pairs.first cxAndVal
                    val = Pairs.second cxAndVal
                    cx1 =
                            case term of
                              Core.TermFunction v0 -> case v0 of
                                Core.FunctionLambda v1 -> extendGraphForLambda cx v1
                                _ -> cx
                              Core.TermLet v0 -> extendGraphForLet (\_ -> \_ -> Nothing) cx v0
                              Core.TermTypeLambda v0 -> extendGraphForTypeLambda cx v0
                              _ -> cx
                    recurseForUser =
                            \valIn -> \termIn ->
                              let result = recurse path (cx1, valIn) termIn
                              in (Pairs.second (Pairs.first result), (Pairs.second result))
                    fResult = f recurseForUser path cx1 val term
                in ((cx, (Pairs.first fResult)), (Pairs.second fResult))
          result = rewriteAndFoldTermWithPath wrapper (cx0, val0) term0
      in (Pairs.second (Pairs.first result), (Pairs.second result))

-- | Rewrite a term with path tracking, and fold a function over it, accumulating a value. The path is a list of TermAccessors from root to current position.
rewriteAndFoldTermWithPath :: (([Accessors.TermAccessor] -> t0 -> Core.Term -> (t0, Core.Term)) -> [Accessors.TermAccessor] -> t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term)
rewriteAndFoldTermWithPath f term0 =

      let fsub =
              \recurse -> \path -> \val0 -> \term0 ->
                let forSingleWithAccessor =
                        \rec -> \cons -> \accessor -> \val -> \term ->
                          let r = rec (Lists.concat2 path [
                                accessor]) val term
                          in (Pairs.first r, (cons (Pairs.second r)))
                    forManyWithAccessors =
                            \rec -> \cons -> \val -> \accessorTermPairs ->
                              let rr =
                                      Lists.foldl (\r -> \atp ->
                                        let r2 = rec (Lists.concat2 path [
                                              Pairs.first atp]) (Pairs.first r) (Pairs.second atp)
                                        in (Pairs.first r2, (Lists.cons (Pairs.second r2) (Pairs.second r)))) (val, []) accessorTermPairs
                              in (Pairs.first rr, (cons (Lists.reverse (Pairs.second rr))))
                    forFieldWithAccessor =
                            \mkAccessor -> \val -> \field ->
                              let r = recurse (Lists.concat2 path [
                                    mkAccessor (Core.fieldName field)]) val (Core.fieldTerm field)
                              in (Pairs.first r, Core.Field {
                                Core.fieldName = (Core.fieldName field),
                                Core.fieldTerm = (Pairs.second r)})
                    forFieldsWithAccessor =
                            \mkAccessor -> forManyWithAccessors (\path1 -> \val1 -> \field1 -> forFieldWithAccessor mkAccessor val1 field1) (\x -> x)
                    forPairWithAccessors =
                            \keyAccessor -> \valAccessor -> \val -> \kv ->
                              let rk = recurse (Lists.concat2 path [
                                    keyAccessor]) val (Pairs.first kv)
                                  rv = recurse (Lists.concat2 path [
                                        valAccessor]) (Pairs.first rk) (Pairs.second kv)
                              in (Pairs.first rv, (Pairs.second rk, (Pairs.second rv)))
                    forBindingWithAccessor =
                            \val -> \binding ->
                              let r =
                                      recurse (Lists.concat2 path [
                                        Accessors.TermAccessorLetBinding (Core.bindingName binding)]) val (Core.bindingTerm binding)
                              in (Pairs.first r, Core.Binding {
                                Core.bindingName = (Core.bindingName binding),
                                Core.bindingTerm = (Pairs.second r),
                                Core.bindingType = (Core.bindingType binding)})
                    forElimination =
                            \val -> \elm ->
                              let r =
                                      case elm of
                                        Core.EliminationUnion v0 ->
                                          let rmd =
                                                  Maybes.map (\def -> recurse (Lists.concat2 path [
                                                    Accessors.TermAccessorUnionCasesDefault]) val def) (Core.caseStatementDefault v0)
                                              val1 = Maybes.maybe val Pairs.first rmd
                                              rcases =
                                                      forManyWithAccessors recurse (\x -> x) val1 (Lists.map (\f -> (Accessors.TermAccessorUnionCasesBranch (Core.fieldName f), (Core.fieldTerm f))) (Core.caseStatementCases v0))
                                          in (Pairs.first rcases, (Core.EliminationUnion (Core.CaseStatement {
                                            Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                                            Core.caseStatementDefault = (Maybes.map Pairs.second rmd),
                                            Core.caseStatementCases = (Lists.map (\ft -> Core.Field {
                                              Core.fieldName = (Pairs.first ft),
                                              Core.fieldTerm = (Pairs.second ft)}) (Lists.zip (Lists.map Core.fieldName (Core.caseStatementCases v0)) (Pairs.second rcases)))})))
                                        _ -> (val, elm)
                              in (Pairs.first r, (Pairs.second r))
                    forFunction =
                            \val -> \fun -> case fun of
                              Core.FunctionElimination v0 ->
                                let re = forElimination val v0
                                in (Pairs.first re, (Core.FunctionElimination (Pairs.second re)))
                              Core.FunctionLambda v0 ->
                                let rl = recurse (Lists.concat2 path [
                                      Accessors.TermAccessorLambdaBody]) val (Core.lambdaBody v0)
                                in (Pairs.first rl, (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.lambdaParameter v0),
                                  Core.lambdaDomain = (Core.lambdaDomain v0),
                                  Core.lambdaBody = (Pairs.second rl)})))
                              _ -> (val, fun)
                    dflt = (val0, term0)
                in case term0 of
                  Core.TermAnnotated v0 -> forSingleWithAccessor recurse (\t -> Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = t,
                    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})) Accessors.TermAccessorAnnotatedBody val0 (Core.annotatedTermBody v0)
                  Core.TermApplication v0 ->
                    let rlhs = recurse (Lists.concat2 path [
                          Accessors.TermAccessorApplicationFunction]) val0 (Core.applicationFunction v0)
                        rrhs =
                                recurse (Lists.concat2 path [
                                  Accessors.TermAccessorApplicationArgument]) (Pairs.first rlhs) (Core.applicationArgument v0)
                    in (Pairs.first rrhs, (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Pairs.second rlhs),
                      Core.applicationArgument = (Pairs.second rrhs)})))
                  Core.TermEither v0 -> Eithers.either (\l ->
                    let rl = recurse (Lists.concat2 path [
                          Accessors.TermAccessorSumTerm]) val0 l
                    in (Pairs.first rl, (Core.TermEither (Left (Pairs.second rl))))) (\r ->
                    let rr = recurse (Lists.concat2 path [
                          Accessors.TermAccessorSumTerm]) val0 r
                    in (Pairs.first rr, (Core.TermEither (Right (Pairs.second rr))))) v0
                  Core.TermFunction v0 ->
                    let rf = forFunction val0 v0
                    in (Pairs.first rf, (Core.TermFunction (Pairs.second rf)))
                  Core.TermLet v0 ->
                    let renv = recurse (Lists.concat2 path [
                          Accessors.TermAccessorLetBody]) val0 (Core.letBody v0)
                        rbindings =
                                Lists.foldl (\r -> \binding ->
                                  let rb = forBindingWithAccessor (Pairs.first r) binding
                                  in (Pairs.first rb, (Lists.cons (Pairs.second rb) (Pairs.second r)))) (Pairs.first renv, []) (Core.letBindings v0)
                    in (Pairs.first rbindings, (Core.TermLet (Core.Let {
                      Core.letBindings = (Lists.reverse (Pairs.second rbindings)),
                      Core.letBody = (Pairs.second renv)})))
                  Core.TermList v0 ->
                    let idx = 0
                        rr =
                                Lists.foldl (\r -> \el ->
                                  let r2 = recurse (Lists.concat2 path [
                                        Accessors.TermAccessorListElement (Pairs.first r)]) (Pairs.first (Pairs.second r)) el
                                  in (Math.add (Pairs.first r) 1, (Pairs.first r2, (Lists.cons (Pairs.second r2) (Pairs.second (Pairs.second r)))))) (idx, (val0, [])) v0
                    in (Pairs.first (Pairs.second rr), (Core.TermList (Lists.reverse (Pairs.second (Pairs.second rr)))))
                  Core.TermMap v0 ->
                    let idx = 0
                        rr =
                                Lists.foldl (\r -> \kv ->
                                  let rk =
                                          recurse (Lists.concat2 path [
                                            Accessors.TermAccessorMapKey (Pairs.first r)]) (Pairs.first (Pairs.second r)) (Pairs.first kv)
                                      rv = recurse (Lists.concat2 path [
                                            Accessors.TermAccessorMapValue (Pairs.first r)]) (Pairs.first rk) (Pairs.second kv)
                                  in (Math.add (Pairs.first r) 1, (Pairs.first rv, (Lists.cons (Pairs.second rk, (Pairs.second rv)) (Pairs.second (Pairs.second r)))))) (idx, (val0, [])) (Maps.toList v0)
                    in (Pairs.first (Pairs.second rr), (Core.TermMap (Maps.fromList (Lists.reverse (Pairs.second (Pairs.second rr))))))
                  Core.TermMaybe v0 -> Maybes.maybe dflt (\t -> forSingleWithAccessor recurse (\t1 -> Core.TermMaybe (Just t1)) Accessors.TermAccessorMaybeTerm val0 t) v0
                  Core.TermPair v0 ->
                    let rf = recurse (Lists.concat2 path [
                          Accessors.TermAccessorProductTerm 0]) val0 (Pairs.first v0)
                        rs = recurse (Lists.concat2 path [
                              Accessors.TermAccessorProductTerm 1]) (Pairs.first rf) (Pairs.second v0)
                    in (Pairs.first rs, (Core.TermPair (Pairs.second rf, (Pairs.second rs))))
                  Core.TermRecord v0 ->
                    let rfields =
                            forManyWithAccessors recurse (\x -> x) val0 (Lists.map (\f -> (Accessors.TermAccessorRecordField (Core.fieldName f), (Core.fieldTerm f))) (Core.recordFields v0))
                    in (Pairs.first rfields, (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.recordTypeName v0),
                      Core.recordFields = (Lists.map (\ft -> Core.Field {
                        Core.fieldName = (Pairs.first ft),
                        Core.fieldTerm = (Pairs.second ft)}) (Lists.zip (Lists.map Core.fieldName (Core.recordFields v0)) (Pairs.second rfields)))})))
                  Core.TermSet v0 ->
                    let idx = 0
                        rr =
                                Lists.foldl (\r -> \el ->
                                  let r2 = recurse (Lists.concat2 path [
                                        Accessors.TermAccessorSetElement (Pairs.first r)]) (Pairs.first (Pairs.second r)) el
                                  in (Math.add (Pairs.first r) 1, (Pairs.first r2, (Lists.cons (Pairs.second r2) (Pairs.second (Pairs.second r)))))) (idx, (val0, [])) (Sets.toList v0)
                    in (Pairs.first (Pairs.second rr), (Core.TermSet (Sets.fromList (Lists.reverse (Pairs.second (Pairs.second rr))))))
                  Core.TermTypeApplication v0 -> forSingleWithAccessor recurse (\t -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = t,
                    Core.typeApplicationTermType = (Core.typeApplicationTermType v0)})) Accessors.TermAccessorTypeApplicationTerm val0 (Core.typeApplicationTermBody v0)
                  Core.TermTypeLambda v0 -> forSingleWithAccessor recurse (\t -> Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
                    Core.typeLambdaBody = t})) Accessors.TermAccessorTypeLambdaBody val0 (Core.typeLambdaBody v0)
                  Core.TermUnion v0 -> forSingleWithAccessor recurse (\t -> Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.injectionTypeName v0),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.fieldName (Core.injectionField v0)),
                      Core.fieldTerm = t}})) Accessors.TermAccessorInjectionTerm val0 (Core.fieldTerm (Core.injectionField v0))
                  Core.TermWrap v0 -> forSingleWithAccessor recurse (\t -> Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.wrappedTermTypeName v0),
                    Core.wrappedTermBody = t})) Accessors.TermAccessorWrappedTerm val0 (Core.wrappedTermBody v0)
                  _ -> dflt
          recurse = f (fsub recurse)
      in (recurse [] term0)

rewriteTerm :: ((Core.Term -> Core.Term) -> Core.Term -> Core.Term) -> Core.Term -> Core.Term
rewriteTerm f term0 =

      let fsub =
              \recurse -> \term ->
                let forField =
                        \f -> Core.Field {
                          Core.fieldName = (Core.fieldName f),
                          Core.fieldTerm = (recurse (Core.fieldTerm f))}
                    forElimination =
                            \elm -> case elm of
                              Core.EliminationRecord v0 -> Core.EliminationRecord v0
                              Core.EliminationUnion v0 -> Core.EliminationUnion (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                                Core.caseStatementDefault = (Maybes.map recurse (Core.caseStatementDefault v0)),
                                Core.caseStatementCases = (Lists.map forField (Core.caseStatementCases v0))})
                              Core.EliminationWrap v0 -> Core.EliminationWrap v0
                    forFunction =
                            \fun -> case fun of
                              Core.FunctionElimination v0 -> Core.FunctionElimination (forElimination v0)
                              Core.FunctionLambda v0 -> Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.lambdaParameter v0),
                                Core.lambdaDomain = (Core.lambdaDomain v0),
                                Core.lambdaBody = (recurse (Core.lambdaBody v0))})
                              Core.FunctionPrimitive v0 -> Core.FunctionPrimitive v0
                    forLet =
                            \lt ->
                              let mapBinding =
                                      \b -> Core.Binding {
                                        Core.bindingName = (Core.bindingName b),
                                        Core.bindingTerm = (recurse (Core.bindingTerm b)),
                                        Core.bindingType = (Core.bindingType b)}
                              in Core.Let {
                                Core.letBindings = (Lists.map mapBinding (Core.letBindings lt)),
                                Core.letBody = (recurse (Core.letBody lt))}
                    forMap =
                            \m ->
                              let forPair = \p -> (recurse (Pairs.first p), (recurse (Pairs.second p)))
                              in (Maps.fromList (Lists.map forPair (Maps.toList m)))
                in case term of
                  Core.TermAnnotated v0 -> Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (recurse (Core.annotatedTermBody v0)),
                    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})
                  Core.TermApplication v0 -> Core.TermApplication (Core.Application {
                    Core.applicationFunction = (recurse (Core.applicationFunction v0)),
                    Core.applicationArgument = (recurse (Core.applicationArgument v0))})
                  Core.TermEither v0 -> Core.TermEither (Eithers.either (\l -> Left (recurse l)) (\r -> Right (recurse r)) v0)
                  Core.TermFunction v0 -> Core.TermFunction (forFunction v0)
                  Core.TermLet v0 -> Core.TermLet (forLet v0)
                  Core.TermList v0 -> Core.TermList (Lists.map recurse v0)
                  Core.TermLiteral v0 -> Core.TermLiteral v0
                  Core.TermMap v0 -> Core.TermMap (forMap v0)
                  Core.TermMaybe v0 -> Core.TermMaybe (Maybes.map recurse v0)
                  Core.TermPair v0 -> Core.TermPair (recurse (Pairs.first v0), (recurse (Pairs.second v0)))
                  Core.TermRecord v0 -> Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.recordTypeName v0),
                    Core.recordFields = (Lists.map forField (Core.recordFields v0))})
                  Core.TermSet v0 -> Core.TermSet (Sets.fromList (Lists.map recurse (Sets.toList v0)))
                  Core.TermTypeApplication v0 -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (recurse (Core.typeApplicationTermBody v0)),
                    Core.typeApplicationTermType = (Core.typeApplicationTermType v0)})
                  Core.TermTypeLambda v0 -> Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
                    Core.typeLambdaBody = (recurse (Core.typeLambdaBody v0))})
                  Core.TermUnion v0 -> Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.injectionTypeName v0),
                    Core.injectionField = (forField (Core.injectionField v0))})
                  Core.TermUnit -> Core.TermUnit
                  Core.TermVariable v0 -> Core.TermVariable v0
                  Core.TermWrap v0 -> Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.wrappedTermTypeName v0),
                    Core.wrappedTermBody = (recurse (Core.wrappedTermBody v0))})
          recurse = f (fsub recurse)
      in (recurse term0)

-- | Either-based term rewriting with custom transformation function
rewriteTermM :: ((Core.Term -> Either t0 Core.Term) -> Core.Term -> Either t0 Core.Term) -> Core.Term -> Either t0 Core.Term
rewriteTermM f term0 =

      let fsub =
              \recurse -> \term ->
                let forField =
                        \field -> Eithers.bind (recurse (Core.fieldTerm field)) (\t -> Right (Core.Field {
                          Core.fieldName = (Core.fieldName field),
                          Core.fieldTerm = t}))
                    forPair =
                            \kv -> Eithers.bind (recurse (Pairs.first kv)) (\k -> Eithers.bind (recurse (Pairs.second kv)) (\v -> Right (k, v)))
                    mapBinding =
                            \b -> Eithers.bind (recurse (Core.bindingTerm b)) (\v -> Right (Core.Binding {
                              Core.bindingName = (Core.bindingName b),
                              Core.bindingTerm = v,
                              Core.bindingType = (Core.bindingType b)}))
                in case term of
                  Core.TermAnnotated v0 -> Eithers.bind (recurse (Core.annotatedTermBody v0)) (\ex -> Right (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = ex,
                    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})))
                  Core.TermApplication v0 -> Eithers.bind (recurse (Core.applicationFunction v0)) (\lhs -> Eithers.bind (recurse (Core.applicationArgument v0)) (\rhs -> Right (Core.TermApplication (Core.Application {
                    Core.applicationFunction = lhs,
                    Core.applicationArgument = rhs}))))
                  Core.TermEither v0 -> Eithers.bind (Eithers.either (\l -> Eithers.map (\x -> Left x) (recurse l)) (\r -> Eithers.map (\x -> Right x) (recurse r)) v0) (\re -> Right (Core.TermEither re))
                  Core.TermFunction v0 ->
                    let forElm =
                            \e -> case e of
                              Core.EliminationRecord v1 -> Right (Core.FunctionElimination (Core.EliminationRecord v1))
                              Core.EliminationUnion v1 ->
                                let n = Core.caseStatementTypeName v1
                                    def = Core.caseStatementDefault v1
                                    cases = Core.caseStatementCases v1
                                in (Eithers.bind (Maybes.maybe (Right Nothing) (\t -> Eithers.map Maybes.pure (recurse t)) def) (\rdef -> Eithers.map (\rcases -> Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                  Core.caseStatementTypeName = n,
                                  Core.caseStatementDefault = rdef,
                                  Core.caseStatementCases = rcases}))) (Eithers.mapList forField cases)))
                              Core.EliminationWrap v1 -> Right (Core.FunctionElimination (Core.EliminationWrap v1))
                        forFun =
                                \fun -> case fun of
                                  Core.FunctionElimination v1 -> forElm v1
                                  Core.FunctionLambda v1 ->
                                    let v = Core.lambdaParameter v1
                                        d = Core.lambdaDomain v1
                                        body = Core.lambdaBody v1
                                    in (Eithers.bind (recurse body) (\rbody -> Right (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = v,
                                      Core.lambdaDomain = d,
                                      Core.lambdaBody = rbody}))))
                                  Core.FunctionPrimitive v1 -> Right (Core.FunctionPrimitive v1)
                    in (Eithers.bind (forFun v0) (\rfun -> Right (Core.TermFunction rfun)))
                  Core.TermLet v0 ->
                    let bindings = Core.letBindings v0
                        env = Core.letBody v0
                    in (Eithers.bind (Eithers.mapList mapBinding bindings) (\rbindings -> Eithers.bind (recurse env) (\renv -> Right (Core.TermLet (Core.Let {
                      Core.letBindings = rbindings,
                      Core.letBody = renv})))))
                  Core.TermList v0 -> Eithers.bind (Eithers.mapList recurse v0) (\rels -> Right (Core.TermList rels))
                  Core.TermLiteral v0 -> Right (Core.TermLiteral v0)
                  Core.TermMap v0 -> Eithers.bind (Eithers.mapList forPair (Maps.toList v0)) (\pairs -> Right (Core.TermMap (Maps.fromList pairs)))
                  Core.TermMaybe v0 -> Eithers.bind (Eithers.mapMaybe recurse v0) (\rm -> Right (Core.TermMaybe rm))
                  Core.TermPair v0 -> Eithers.bind (recurse (Pairs.first v0)) (\rf -> Eithers.bind (recurse (Pairs.second v0)) (\rs -> Right (Core.TermPair (rf, rs))))
                  Core.TermRecord v0 ->
                    let n = Core.recordTypeName v0
                        fields = Core.recordFields v0
                    in (Eithers.map (\rfields -> Core.TermRecord (Core.Record {
                      Core.recordTypeName = n,
                      Core.recordFields = rfields})) (Eithers.mapList forField fields))
                  Core.TermSet v0 -> Eithers.bind (Eithers.mapList recurse (Sets.toList v0)) (\rlist -> Right (Core.TermSet (Sets.fromList rlist)))
                  Core.TermTypeApplication v0 -> Eithers.bind (recurse (Core.typeApplicationTermBody v0)) (\t -> Right (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = t,
                    Core.typeApplicationTermType = (Core.typeApplicationTermType v0)})))
                  Core.TermTypeLambda v0 ->
                    let v = Core.typeLambdaParameter v0
                        body = Core.typeLambdaBody v0
                    in (Eithers.bind (recurse body) (\rbody -> Right (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = v,
                      Core.typeLambdaBody = rbody}))))
                  Core.TermUnion v0 ->
                    let n = Core.injectionTypeName v0
                        field = Core.injectionField v0
                    in (Eithers.map (\rfield -> Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = n,
                      Core.injectionField = rfield})) (forField field))
                  Core.TermUnit -> Right Core.TermUnit
                  Core.TermVariable v0 -> Right (Core.TermVariable v0)
                  Core.TermWrap v0 ->
                    let name = Core.wrappedTermTypeName v0
                        t = Core.wrappedTermBody v0
                    in (Eithers.bind (recurse t) (\rt -> Right (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = name,
                      Core.wrappedTermBody = rt}))))
          recurse = f (fsub recurse)
      in (recurse term0)

-- | A variant of rewriteTerm which allows a context (e.g. a TypeContext) to be passed down to all subterms during rewriting
rewriteTermWithContext :: ((t0 -> Core.Term -> Core.Term) -> t0 -> Core.Term -> Core.Term) -> t0 -> Core.Term -> Core.Term
rewriteTermWithContext f cx0 term0 =

      let forSubterms =
              \recurse0 -> \cx -> \term ->
                let recurse = recurse0 cx
                    forField =
                            \field -> Core.Field {
                              Core.fieldName = (Core.fieldName field),
                              Core.fieldTerm = (recurse (Core.fieldTerm field))}
                    forElimination =
                            \elm -> case elm of
                              Core.EliminationRecord v0 -> Core.EliminationRecord v0
                              Core.EliminationUnion v0 -> Core.EliminationUnion (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                                Core.caseStatementDefault = (Maybes.map recurse (Core.caseStatementDefault v0)),
                                Core.caseStatementCases = (Lists.map forField (Core.caseStatementCases v0))})
                              Core.EliminationWrap v0 -> Core.EliminationWrap v0
                    forFunction =
                            \fun -> case fun of
                              Core.FunctionElimination v0 -> Core.FunctionElimination (forElimination v0)
                              Core.FunctionLambda v0 -> Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.lambdaParameter v0),
                                Core.lambdaDomain = (Core.lambdaDomain v0),
                                Core.lambdaBody = (recurse (Core.lambdaBody v0))})
                              Core.FunctionPrimitive v0 -> Core.FunctionPrimitive v0
                    forLet =
                            \lt ->
                              let mapBinding =
                                      \b -> Core.Binding {
                                        Core.bindingName = (Core.bindingName b),
                                        Core.bindingTerm = (recurse (Core.bindingTerm b)),
                                        Core.bindingType = (Core.bindingType b)}
                              in Core.Let {
                                Core.letBindings = (Lists.map mapBinding (Core.letBindings lt)),
                                Core.letBody = (recurse (Core.letBody lt))}
                    forMap =
                            \m ->
                              let forPair = \p -> (recurse (Pairs.first p), (recurse (Pairs.second p)))
                              in (Maps.fromList (Lists.map forPair (Maps.toList m)))
                in case term of
                  Core.TermAnnotated v0 -> Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (recurse (Core.annotatedTermBody v0)),
                    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})
                  Core.TermApplication v0 -> Core.TermApplication (Core.Application {
                    Core.applicationFunction = (recurse (Core.applicationFunction v0)),
                    Core.applicationArgument = (recurse (Core.applicationArgument v0))})
                  Core.TermEither v0 -> Core.TermEither (Eithers.either (\l -> Left (recurse l)) (\r -> Right (recurse r)) v0)
                  Core.TermFunction v0 -> Core.TermFunction (forFunction v0)
                  Core.TermLet v0 -> Core.TermLet (forLet v0)
                  Core.TermList v0 -> Core.TermList (Lists.map recurse v0)
                  Core.TermLiteral v0 -> Core.TermLiteral v0
                  Core.TermMap v0 -> Core.TermMap (forMap v0)
                  Core.TermMaybe v0 -> Core.TermMaybe (Maybes.map recurse v0)
                  Core.TermPair v0 -> Core.TermPair (recurse (Pairs.first v0), (recurse (Pairs.second v0)))
                  Core.TermRecord v0 -> Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.recordTypeName v0),
                    Core.recordFields = (Lists.map forField (Core.recordFields v0))})
                  Core.TermSet v0 -> Core.TermSet (Sets.fromList (Lists.map recurse (Sets.toList v0)))
                  Core.TermTypeApplication v0 -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (recurse (Core.typeApplicationTermBody v0)),
                    Core.typeApplicationTermType = (Core.typeApplicationTermType v0)})
                  Core.TermTypeLambda v0 -> Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
                    Core.typeLambdaBody = (recurse (Core.typeLambdaBody v0))})
                  Core.TermUnion v0 -> Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.injectionTypeName v0),
                    Core.injectionField = (forField (Core.injectionField v0))})
                  Core.TermUnit -> Core.TermUnit
                  Core.TermVariable v0 -> Core.TermVariable v0
                  Core.TermWrap v0 -> Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.wrappedTermTypeName v0),
                    Core.wrappedTermBody = (recurse (Core.wrappedTermBody v0))})
          rewrite = \cx -> \term -> f (forSubterms rewrite) cx term
      in (rewrite cx0 term0)

-- | Either-based variant of rewriteTermWithContextM which allows a context (e.g. a TypeContext) to be passed down to all subterms during rewriting
rewriteTermWithContextM :: ((t0 -> Core.Term -> Either t1 Core.Term) -> t0 -> Core.Term -> Either t1 Core.Term) -> t0 -> Core.Term -> Either t1 Core.Term
rewriteTermWithContextM f cx0 term0 =

      let forSubterms =
              \recurse0 -> \cx -> \term ->
                let recurse = recurse0 cx
                    forField =
                            \field -> Eithers.bind (recurse (Core.fieldTerm field)) (\t -> Right (Core.Field {
                              Core.fieldName = (Core.fieldName field),
                              Core.fieldTerm = t}))
                    forPair =
                            \kv -> Eithers.bind (recurse (Pairs.first kv)) (\k -> Eithers.bind (recurse (Pairs.second kv)) (\v -> Right (k, v)))
                    forElimination =
                            \e -> case e of
                              Core.EliminationRecord v0 -> Right (Core.FunctionElimination (Core.EliminationRecord v0))
                              Core.EliminationUnion v0 ->
                                let n = Core.caseStatementTypeName v0
                                    def = Core.caseStatementDefault v0
                                    cases = Core.caseStatementCases v0
                                in (Eithers.bind (Maybes.maybe (Right Nothing) (\t -> Eithers.map Maybes.pure (recurse t)) def) (\rdef -> Eithers.map (\rcases -> Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                  Core.caseStatementTypeName = n,
                                  Core.caseStatementDefault = rdef,
                                  Core.caseStatementCases = rcases}))) (Eithers.mapList forField cases)))
                              Core.EliminationWrap v0 -> Right (Core.FunctionElimination (Core.EliminationWrap v0))
                    forFunction =
                            \fun -> case fun of
                              Core.FunctionElimination v0 -> forElimination v0
                              Core.FunctionLambda v0 ->
                                let v = Core.lambdaParameter v0
                                    d = Core.lambdaDomain v0
                                    body = Core.lambdaBody v0
                                in (Eithers.bind (recurse body) (\rbody -> Right (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = v,
                                  Core.lambdaDomain = d,
                                  Core.lambdaBody = rbody}))))
                              Core.FunctionPrimitive v0 -> Right (Core.FunctionPrimitive v0)
                    mapBinding =
                            \b -> Eithers.bind (recurse (Core.bindingTerm b)) (\v -> Right (Core.Binding {
                              Core.bindingName = (Core.bindingName b),
                              Core.bindingTerm = v,
                              Core.bindingType = (Core.bindingType b)}))
                in case term of
                  Core.TermAnnotated v0 -> Eithers.bind (recurse (Core.annotatedTermBody v0)) (\ex -> Right (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = ex,
                    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})))
                  Core.TermApplication v0 -> Eithers.bind (recurse (Core.applicationFunction v0)) (\lhs -> Eithers.bind (recurse (Core.applicationArgument v0)) (\rhs -> Right (Core.TermApplication (Core.Application {
                    Core.applicationFunction = lhs,
                    Core.applicationArgument = rhs}))))
                  Core.TermEither v0 -> Eithers.bind (Eithers.either (\l -> Eithers.map (\x -> Left x) (recurse l)) (\r -> Eithers.map (\x -> Right x) (recurse r)) v0) (\re -> Right (Core.TermEither re))
                  Core.TermFunction v0 -> Eithers.bind (forFunction v0) (\rfun -> Right (Core.TermFunction rfun))
                  Core.TermLet v0 ->
                    let bindings = Core.letBindings v0
                        body = Core.letBody v0
                    in (Eithers.bind (Eithers.mapList mapBinding bindings) (\rbindings -> Eithers.bind (recurse body) (\rbody -> Right (Core.TermLet (Core.Let {
                      Core.letBindings = rbindings,
                      Core.letBody = rbody})))))
                  Core.TermList v0 -> Eithers.bind (Eithers.mapList recurse v0) (\rels -> Right (Core.TermList rels))
                  Core.TermLiteral v0 -> Right (Core.TermLiteral v0)
                  Core.TermMap v0 -> Eithers.bind (Eithers.mapList forPair (Maps.toList v0)) (\pairs -> Right (Core.TermMap (Maps.fromList pairs)))
                  Core.TermMaybe v0 -> Eithers.bind (Eithers.mapMaybe recurse v0) (\rm -> Right (Core.TermMaybe rm))
                  Core.TermPair v0 -> Eithers.bind (recurse (Pairs.first v0)) (\rfirst -> Eithers.bind (recurse (Pairs.second v0)) (\rsecond -> Right (Core.TermPair (rfirst, rsecond))))
                  Core.TermRecord v0 ->
                    let n = Core.recordTypeName v0
                        fields = Core.recordFields v0
                    in (Eithers.map (\rfields -> Core.TermRecord (Core.Record {
                      Core.recordTypeName = n,
                      Core.recordFields = rfields})) (Eithers.mapList forField fields))
                  Core.TermSet v0 -> Eithers.bind (Eithers.mapList recurse (Sets.toList v0)) (\rlist -> Right (Core.TermSet (Sets.fromList rlist)))
                  Core.TermTypeApplication v0 -> Eithers.bind (recurse (Core.typeApplicationTermBody v0)) (\t -> Right (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = t,
                    Core.typeApplicationTermType = (Core.typeApplicationTermType v0)})))
                  Core.TermTypeLambda v0 ->
                    let v = Core.typeLambdaParameter v0
                        body = Core.typeLambdaBody v0
                    in (Eithers.bind (recurse body) (\rbody -> Right (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = v,
                      Core.typeLambdaBody = rbody}))))
                  Core.TermUnion v0 ->
                    let n = Core.injectionTypeName v0
                        field = Core.injectionField v0
                    in (Eithers.map (\rfield -> Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = n,
                      Core.injectionField = rfield})) (forField field))
                  Core.TermUnit -> Right Core.TermUnit
                  Core.TermVariable v0 -> Right (Core.TermVariable v0)
                  Core.TermWrap v0 ->
                    let name = Core.wrappedTermTypeName v0
                        t = Core.wrappedTermBody v0
                    in (Eithers.bind (recurse t) (\rt -> Right (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = name,
                      Core.wrappedTermBody = rt}))))
          rewrite = \cx -> \term -> f (forSubterms rewrite) cx term
      in (rewrite cx0 term0)

-- | Rewrite a term with the help of a Graph which is updated as we descend into subterms
rewriteTermWithGraph :: ((Core.Term -> t0) -> Graph.Graph -> Core.Term -> t0) -> Graph.Graph -> Core.Term -> t0
rewriteTermWithGraph f cx0 term0 =

      let f2 =
              \recurse -> \cx -> \term ->
                let recurse1 = \term -> recurse cx term
                in case term of
                  Core.TermFunction v0 -> case v0 of
                    Core.FunctionLambda v1 ->
                      let cx1 = extendGraphForLambda cx v1
                          recurse2 = \term -> recurse cx1 term
                      in (f recurse2 cx1 term)
                    _ -> f recurse1 cx term
                  Core.TermLet v0 ->
                    let cx1 = extendGraphForLet (\_ -> \_ -> Nothing) cx v0
                        recurse2 = \term -> recurse cx1 term
                    in (f recurse2 cx1 term)
                  Core.TermTypeLambda v0 ->
                    let cx1 = extendGraphForTypeLambda cx v0
                        recurse2 = \term -> recurse cx1 term
                    in (f recurse2 cx1 term)
                  _ -> f recurse1 cx term
          rewrite = \cx -> \term -> f2 rewrite cx term
      in (rewrite cx0 term0)

rewriteType :: ((Core.Type -> Core.Type) -> Core.Type -> Core.Type) -> Core.Type -> Core.Type
rewriteType f typ0 =

      let fsub =
              \recurse -> \typ ->
                let forField =
                        \field -> Core.FieldType {
                          Core.fieldTypeName = (Core.fieldTypeName field),
                          Core.fieldTypeType = (recurse (Core.fieldTypeType field))}
                in case typ of
                  Core.TypeAnnotated v0 -> Core.TypeAnnotated (Core.AnnotatedType {
                    Core.annotatedTypeBody = (recurse (Core.annotatedTypeBody v0)),
                    Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v0)})
                  Core.TypeApplication v0 -> Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (recurse (Core.applicationTypeFunction v0)),
                    Core.applicationTypeArgument = (recurse (Core.applicationTypeArgument v0))})
                  Core.TypeEither v0 -> Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (recurse (Core.eitherTypeLeft v0)),
                    Core.eitherTypeRight = (recurse (Core.eitherTypeRight v0))})
                  Core.TypePair v0 -> Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (recurse (Core.pairTypeFirst v0)),
                    Core.pairTypeSecond = (recurse (Core.pairTypeSecond v0))})
                  Core.TypeFunction v0 -> Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (recurse (Core.functionTypeDomain v0)),
                    Core.functionTypeCodomain = (recurse (Core.functionTypeCodomain v0))})
                  Core.TypeForall v0 -> Core.TypeForall (Core.ForallType {
                    Core.forallTypeParameter = (Core.forallTypeParameter v0),
                    Core.forallTypeBody = (recurse (Core.forallTypeBody v0))})
                  Core.TypeList v0 -> Core.TypeList (recurse v0)
                  Core.TypeLiteral v0 -> Core.TypeLiteral v0
                  Core.TypeMap v0 -> Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (recurse (Core.mapTypeKeys v0)),
                    Core.mapTypeValues = (recurse (Core.mapTypeValues v0))})
                  Core.TypeMaybe v0 -> Core.TypeMaybe (recurse v0)
                  Core.TypeRecord v0 -> Core.TypeRecord (Lists.map forField v0)
                  Core.TypeSet v0 -> Core.TypeSet (recurse v0)
                  Core.TypeUnion v0 -> Core.TypeUnion (Lists.map forField v0)
                  Core.TypeUnit -> Core.TypeUnit
                  Core.TypeVariable v0 -> Core.TypeVariable v0
                  Core.TypeVoid -> Core.TypeVoid
                  Core.TypeWrap v0 -> Core.TypeWrap (recurse v0)
          recurse = f (fsub recurse)
      in (recurse typ0)

-- | Either-based type rewriting
rewriteTypeM :: ((Core.Type -> Either t0 Core.Type) -> Core.Type -> Either t0 Core.Type) -> Core.Type -> Either t0 Core.Type
rewriteTypeM f typ0 =

      let fsub =
              \recurse -> \typ -> case typ of
                Core.TypeAnnotated v0 -> Eithers.bind (recurse (Core.annotatedTypeBody v0)) (\t -> Right (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = t,
                  Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v0)})))
                Core.TypeApplication v0 -> Eithers.bind (recurse (Core.applicationTypeFunction v0)) (\lhs -> Eithers.bind (recurse (Core.applicationTypeArgument v0)) (\rhs -> Right (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = lhs,
                  Core.applicationTypeArgument = rhs}))))
                Core.TypeEither v0 -> Eithers.bind (recurse (Core.eitherTypeLeft v0)) (\left -> Eithers.bind (recurse (Core.eitherTypeRight v0)) (\right -> Right (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = left,
                  Core.eitherTypeRight = right}))))
                Core.TypePair v0 -> Eithers.bind (recurse (Core.pairTypeFirst v0)) (\pairFirst -> Eithers.bind (recurse (Core.pairTypeSecond v0)) (\pairSecond -> Right (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = pairFirst,
                  Core.pairTypeSecond = pairSecond}))))
                Core.TypeFunction v0 -> Eithers.bind (recurse (Core.functionTypeDomain v0)) (\dom -> Eithers.bind (recurse (Core.functionTypeCodomain v0)) (\cod -> Right (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = dom,
                  Core.functionTypeCodomain = cod}))))
                Core.TypeForall v0 -> Eithers.bind (recurse (Core.forallTypeBody v0)) (\b -> Right (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.forallTypeParameter v0),
                  Core.forallTypeBody = b})))
                Core.TypeList v0 -> Eithers.bind (recurse v0) (\rt -> Right (Core.TypeList rt))
                Core.TypeLiteral v0 -> Right (Core.TypeLiteral v0)
                Core.TypeMap v0 -> Eithers.bind (recurse (Core.mapTypeKeys v0)) (\kt -> Eithers.bind (recurse (Core.mapTypeValues v0)) (\vt -> Right (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = kt,
                  Core.mapTypeValues = vt}))))
                Core.TypeMaybe v0 -> Eithers.bind (recurse v0) (\rt -> Right (Core.TypeMaybe rt))
                Core.TypeRecord v0 ->
                  let forField =
                          \f -> Eithers.bind (recurse (Core.fieldTypeType f)) (\t -> Right (Core.FieldType {
                            Core.fieldTypeName = (Core.fieldTypeName f),
                            Core.fieldTypeType = t}))
                  in (Eithers.bind (Eithers.mapList forField v0) (\rfields -> Right (Core.TypeRecord rfields)))
                Core.TypeSet v0 -> Eithers.bind (recurse v0) (\rt -> Right (Core.TypeSet rt))
                Core.TypeUnion v0 ->
                  let forField =
                          \f -> Eithers.bind (recurse (Core.fieldTypeType f)) (\t -> Right (Core.FieldType {
                            Core.fieldTypeName = (Core.fieldTypeName f),
                            Core.fieldTypeType = t}))
                  in (Eithers.bind (Eithers.mapList forField v0) (\rfields -> Right (Core.TypeUnion rfields)))
                Core.TypeUnit -> Right Core.TypeUnit
                Core.TypeVariable v0 -> Right (Core.TypeVariable v0)
                Core.TypeVoid -> Right Core.TypeVoid
                Core.TypeWrap v0 -> Eithers.bind (recurse v0) (\t -> Right (Core.TypeWrap t))
          recurse = f (fsub recurse)
      in (recurse typ0)

-- | Simplify terms by applying beta reduction where possible
simplifyTerm :: Core.Term -> Core.Term
simplifyTerm term =

      let simplify =
              \recurse -> \term ->
                let forRhs =
                        \rhs -> \var -> \body -> case (deannotateTerm rhs) of
                          Core.TermVariable v0 -> simplifyTerm (substituteVariable var v0 body)
                          _ -> term
                    forLhs =
                            \lhs -> \rhs ->
                              let forFun =
                                      \fun -> case fun of
                                        Core.FunctionLambda v0 ->
                                          let var = Core.lambdaParameter v0
                                              body = Core.lambdaBody v0
                                          in (Logic.ifElse (Sets.member var (freeVariablesInTerm body)) (forRhs rhs var body) (simplifyTerm body))
                                        _ -> term
                              in case (deannotateTerm lhs) of
                                Core.TermFunction v0 -> forFun v0
                                _ -> term
                    forTerm =
                            \stripped -> case stripped of
                              Core.TermApplication v0 ->
                                let lhs = Core.applicationFunction v0
                                    rhs = Core.applicationArgument v0
                                in (forLhs lhs rhs)
                              _ -> term
                    stripped = deannotateTerm term
                in (recurse (forTerm stripped))
      in (rewriteTerm simplify term)

-- | Strip outer type lambda wrappers from a term, preserving type application wrappers and annotations
stripTypeLambdas :: Core.Term -> Core.Term
stripTypeLambdas t =
    case t of
      Core.TermAnnotated v0 ->
        let subj = Core.annotatedTermBody v0
            ann = Core.annotatedTermAnnotation v0
        in (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (stripTypeLambdas subj),
          Core.annotatedTermAnnotation = ann}))
      Core.TermTypeLambda v0 -> stripTypeLambdas (Core.typeLambdaBody v0)
      _ -> t

-- | Substitute type variables in a type
substituteTypeVariables :: M.Map Core.Name Core.Name -> Core.Type -> Core.Type
substituteTypeVariables subst typ =

      let replace =
              \recurse -> \typ -> case typ of
                Core.TypeVariable v0 -> Core.TypeVariable (Maybes.fromMaybe v0 (Maps.lookup v0 subst))
                _ -> recurse typ
      in (rewriteType replace typ)

-- | Substitute type variables throughout a term, including in type annotations, type applications, lambda domains, and type schemes
substituteTypeVariablesInTerm :: M.Map Core.Name Core.Name -> Core.Term -> Core.Term
substituteTypeVariablesInTerm subst term =

      let st = substituteTypeVariables subst
          stOpt = \mt -> Maybes.map st mt
          stScheme =
                  \ts -> Core.TypeScheme {
                    Core.typeSchemeVariables = (Core.typeSchemeVariables ts),
                    Core.typeSchemeType = (st (Core.typeSchemeType ts)),
                    Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts)}
          stSchemeOpt = \mts -> Maybes.map stScheme mts
          replace =
                  \recurse -> \t -> case t of
                    Core.TermFunction v0 -> case v0 of
                      Core.FunctionLambda v1 -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.lambdaParameter v1),
                        Core.lambdaDomain = (stOpt (Core.lambdaDomain v1)),
                        Core.lambdaBody = (recurse (Core.lambdaBody v1))}))
                      _ -> recurse t
                    Core.TermLet v0 ->
                      let mapBinding =
                              \b -> Core.Binding {
                                Core.bindingName = (Core.bindingName b),
                                Core.bindingTerm = (recurse (Core.bindingTerm b)),
                                Core.bindingType = (stSchemeOpt (Core.bindingType b))}
                      in (Core.TermLet (Core.Let {
                        Core.letBindings = (Lists.map mapBinding (Core.letBindings v0)),
                        Core.letBody = (recurse (Core.letBody v0))}))
                    Core.TermTypeApplication v0 -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (recurse (Core.typeApplicationTermBody v0)),
                      Core.typeApplicationTermType = (st (Core.typeApplicationTermType v0))})
                    Core.TermTypeLambda v0 -> Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Maybes.fromMaybe (Core.typeLambdaParameter v0) (Maps.lookup (Core.typeLambdaParameter v0) subst)),
                      Core.typeLambdaBody = (recurse (Core.typeLambdaBody v0))})
                    Core.TermAnnotated v0 -> Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (recurse (Core.annotatedTermBody v0)),
                      Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})
                    _ -> recurse t
      in (rewriteTerm replace term)

-- | Substitute one variable for another in a term
substituteVariable :: Core.Name -> Core.Name -> Core.Term -> Core.Term
substituteVariable from to term =

      let replace =
              \recurse -> \term -> case term of
                Core.TermVariable v0 -> Core.TermVariable (Logic.ifElse (Equality.equal v0 from) to v0)
                Core.TermFunction v0 -> case v0 of
                  Core.FunctionLambda v1 -> Logic.ifElse (Equality.equal (Core.lambdaParameter v1) from) term (recurse term)
                  _ -> recurse term
                _ -> recurse term
      in (rewriteTerm replace term)

-- | Substitute multiple variables in a term
substituteVariables :: M.Map Core.Name Core.Name -> Core.Term -> Core.Term
substituteVariables subst term =

      let replace =
              \recurse -> \term -> case term of
                Core.TermVariable v0 -> Core.TermVariable (Maybes.fromMaybe v0 (Maps.lookup v0 subst))
                Core.TermFunction v0 -> case v0 of
                  Core.FunctionLambda v1 -> Maybes.maybe (recurse term) (\_ -> term) (Maps.lookup (Core.lambdaParameter v1) subst)
                  _ -> recurse term
                _ -> recurse term
      in (rewriteTerm replace term)

-- | Find the children of a given term
subterms :: Core.Term -> [Core.Term]
subterms x =
    case x of
      Core.TermAnnotated v0 -> [
        Core.annotatedTermBody v0]
      Core.TermApplication v0 -> [
        Core.applicationFunction v0,
        (Core.applicationArgument v0)]
      Core.TermEither v0 -> Eithers.either (\l -> [
        l]) (\r -> [
        r]) v0
      Core.TermFunction v0 -> case v0 of
        Core.FunctionElimination v1 -> case v1 of
          Core.EliminationUnion v2 -> Lists.concat2 (Maybes.maybe [] (\t -> [
            t]) (Core.caseStatementDefault v2)) (Lists.map Core.fieldTerm (Core.caseStatementCases v2))
          _ -> []
        Core.FunctionLambda v1 -> [
          Core.lambdaBody v1]
        _ -> []
      Core.TermLet v0 -> Lists.cons (Core.letBody v0) (Lists.map Core.bindingTerm (Core.letBindings v0))
      Core.TermList v0 -> v0
      Core.TermLiteral _ -> []
      Core.TermMap v0 -> Lists.concat (Lists.map (\p -> [
        Pairs.first p,
        (Pairs.second p)]) (Maps.toList v0))
      Core.TermMaybe v0 -> Maybes.maybe [] (\t -> [
        t]) v0
      Core.TermPair v0 -> [
        Pairs.first v0,
        (Pairs.second v0)]
      Core.TermRecord v0 -> Lists.map Core.fieldTerm (Core.recordFields v0)
      Core.TermSet v0 -> Sets.toList v0
      Core.TermTypeApplication v0 -> [
        Core.typeApplicationTermBody v0]
      Core.TermTypeLambda v0 -> [
        Core.typeLambdaBody v0]
      Core.TermUnion v0 -> [
        Core.fieldTerm (Core.injectionField v0)]
      Core.TermUnit -> []
      Core.TermVariable _ -> []
      Core.TermWrap v0 -> [
        Core.wrappedTermBody v0]

-- | Find the children of a given term
subtermsWithAccessors :: Core.Term -> [(Accessors.TermAccessor, Core.Term)]
subtermsWithAccessors x =
    case x of
      Core.TermAnnotated v0 -> [
        (Accessors.TermAccessorAnnotatedBody, (Core.annotatedTermBody v0))]
      Core.TermApplication v0 -> [
        (Accessors.TermAccessorApplicationFunction, (Core.applicationFunction v0)),
        (Accessors.TermAccessorApplicationArgument, (Core.applicationArgument v0))]
      Core.TermEither _ -> []
      Core.TermFunction v0 -> case v0 of
        Core.FunctionElimination v1 -> case v1 of
          Core.EliminationUnion v2 -> Lists.concat2 (Maybes.maybe [] (\t -> [
            (Accessors.TermAccessorUnionCasesDefault, t)]) (Core.caseStatementDefault v2)) (Lists.map (\f -> (Accessors.TermAccessorUnionCasesBranch (Core.fieldName f), (Core.fieldTerm f))) (Core.caseStatementCases v2))
          _ -> []
        Core.FunctionLambda v1 -> [
          (Accessors.TermAccessorLambdaBody, (Core.lambdaBody v1))]
        _ -> []
      Core.TermLet v0 -> Lists.cons (Accessors.TermAccessorLetBody, (Core.letBody v0)) (Lists.map (\b -> (Accessors.TermAccessorLetBinding (Core.bindingName b), (Core.bindingTerm b))) (Core.letBindings v0))
      Core.TermList v0 -> Lists.map (\e -> (Accessors.TermAccessorListElement 0, e)) v0
      Core.TermLiteral _ -> []
      Core.TermMap v0 -> Lists.concat (Lists.map (\p -> [
        (Accessors.TermAccessorMapKey 0, (Pairs.first p)),
        (Accessors.TermAccessorMapValue 0, (Pairs.second p))]) (Maps.toList v0))
      Core.TermMaybe v0 -> Maybes.maybe [] (\t -> [
        (Accessors.TermAccessorMaybeTerm, t)]) v0
      Core.TermPair _ -> []
      Core.TermRecord v0 -> Lists.map (\f -> (Accessors.TermAccessorRecordField (Core.fieldName f), (Core.fieldTerm f))) (Core.recordFields v0)
      Core.TermSet v0 -> Lists.map (\e -> (Accessors.TermAccessorListElement 0, e)) (Sets.toList v0)
      Core.TermTypeApplication v0 -> [
        (Accessors.TermAccessorTypeApplicationTerm, (Core.typeApplicationTermBody v0))]
      Core.TermTypeLambda v0 -> [
        (Accessors.TermAccessorTypeLambdaBody, (Core.typeLambdaBody v0))]
      Core.TermUnion v0 -> [
        (Accessors.TermAccessorInjectionTerm, (Core.fieldTerm (Core.injectionField v0)))]
      Core.TermUnit -> []
      Core.TermVariable _ -> []
      Core.TermWrap v0 -> [
        (Accessors.TermAccessorWrappedTerm, (Core.wrappedTermBody v0))]

-- | Find the children of a given type expression
subtypes :: Core.Type -> [Core.Type]
subtypes x =
    case x of
      Core.TypeAnnotated v0 -> [
        Core.annotatedTypeBody v0]
      Core.TypeApplication v0 -> [
        Core.applicationTypeFunction v0,
        (Core.applicationTypeArgument v0)]
      Core.TypeEither v0 -> [
        Core.eitherTypeLeft v0,
        (Core.eitherTypeRight v0)]
      Core.TypePair v0 -> [
        Core.pairTypeFirst v0,
        (Core.pairTypeSecond v0)]
      Core.TypeFunction v0 -> [
        Core.functionTypeDomain v0,
        (Core.functionTypeCodomain v0)]
      Core.TypeForall v0 -> [
        Core.forallTypeBody v0]
      Core.TypeList v0 -> [
        v0]
      Core.TypeLiteral _ -> []
      Core.TypeMap v0 -> [
        Core.mapTypeKeys v0,
        (Core.mapTypeValues v0)]
      Core.TypeMaybe v0 -> [
        v0]
      Core.TypeRecord v0 -> Lists.map Core.fieldTypeType v0
      Core.TypeSet v0 -> [
        v0]
      Core.TypeUnion v0 -> Lists.map Core.fieldTypeType v0
      Core.TypeUnit -> []
      Core.TypeVariable _ -> []
      Core.TypeVoid -> []
      Core.TypeWrap v0 -> [
        v0]

-- | Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that
termDependencyNames :: Bool -> Bool -> Bool -> Core.Term -> S.Set Core.Name
termDependencyNames binds withPrims withNoms term0 =

      let addNames =
              \names -> \term ->
                let nominal = \name -> Logic.ifElse withNoms (Sets.insert name names) names
                    prim = \name -> Logic.ifElse withPrims (Sets.insert name names) names
                    var = \name -> Logic.ifElse binds (Sets.insert name names) names
                in case term of
                  Core.TermFunction v0 -> case v0 of
                    Core.FunctionPrimitive v1 -> prim v1
                    Core.FunctionElimination v1 -> case v1 of
                      Core.EliminationRecord v2 -> nominal (Core.projectionTypeName v2)
                      Core.EliminationUnion v2 -> nominal (Core.caseStatementTypeName v2)
                      Core.EliminationWrap v2 -> nominal v2
                    _ -> names
                  Core.TermRecord v0 -> nominal (Core.recordTypeName v0)
                  Core.TermUnion v0 -> nominal (Core.injectionTypeName v0)
                  Core.TermVariable v0 -> var v0
                  Core.TermWrap v0 -> nominal (Core.wrappedTermTypeName v0)
                  _ -> names
      in (foldOverTerm Coders.TraversalOrderPre addNames Sets.empty term0)

-- | Generate short names from a list of fully qualified names
toShortNames :: [Core.Name] -> M.Map Core.Name Core.Name
toShortNames original =

      let addName =
              \acc -> \name ->
                let local = Names.localNameOf name
                    group = Maybes.fromMaybe Sets.empty (Maps.lookup local acc)
                in (Maps.insert local (Sets.insert name group) acc)
          groupNamesByLocal = \names -> Lists.foldl addName Maps.empty names
          groups = groupNamesByLocal original
          renameGroup =
                  \localNames ->
                    let local = Pairs.first localNames
                        names = Pairs.second localNames
                        rangeFrom = \start -> Lists.cons start (rangeFrom (Math.add start 1))
                        rename =
                                \name -> \i -> (name, (Core.Name (Logic.ifElse (Equality.gt i 1) (Strings.cat2 local (Literals.showInt32 i)) local)))
                    in (Lists.zipWith rename (Sets.toList names) (rangeFrom 1))
      in (Maps.fromList (Lists.concat (Lists.map renameGroup (Maps.toList groups))))

-- | Topological sort of connected components, in terms of dependencies between variable/term binding pairs
topologicalSortBindingMap :: M.Map Core.Name Core.Term -> [[(Core.Name, Core.Term)]]
topologicalSortBindingMap bindingMap =

      let bindings = Maps.toList bindingMap
          keys = Sets.fromList (Lists.map Pairs.first bindings)
          hasTypeAnnotation =
                  \term -> case term of
                    Core.TermAnnotated v0 -> hasTypeAnnotation (Core.annotatedTermBody v0)
                    _ -> False
          depsOf =
                  \nameAndTerm ->
                    let name = Pairs.first nameAndTerm
                        term = Pairs.second nameAndTerm
                    in (name, (Logic.ifElse (hasTypeAnnotation term) [] (Sets.toList (Sets.intersection keys (freeVariablesInTerm term)))))
          toPair =
                  \name -> (name, (Maybes.fromMaybe (Core.TermLiteral (Core.LiteralString "Impossible!")) (Maps.lookup name bindingMap)))
      in (Lists.map (Lists.map toPair) (Sorting.topologicalSortComponents (Lists.map depsOf bindings)))

-- | Topological sort of elements based on their dependencies
topologicalSortBindings :: [Core.Binding] -> Either [[Core.Name]] [Core.Name]
topologicalSortBindings els =

      let adjlist = \e -> (Core.bindingName e, (Sets.toList (termDependencyNames False True True (Core.bindingTerm e))))
      in (Sorting.topologicalSort (Lists.map adjlist els))

typeDependencyNames :: Bool -> Core.Type -> S.Set Core.Name
typeDependencyNames withSchema typ =
    Logic.ifElse withSchema (Sets.union (freeVariablesInType typ) (typeNamesInType typ)) (freeVariablesInType typ)

typeNamesInType :: Ord t0 => (Core.Type -> S.Set t0)
typeNamesInType typ0 =

      let addNames = \names -> \typ -> names
      in (foldOverType Coders.TraversalOrderPre addNames Sets.empty typ0)

-- | Convert a type scheme to a forall type
typeSchemeToFType :: Core.TypeScheme -> Core.Type
typeSchemeToFType ts =

      let vars = Core.typeSchemeVariables ts
          body = Core.typeSchemeType ts
      in (Lists.foldl (\t -> \v -> Core.TypeForall (Core.ForallType {
        Core.forallTypeParameter = v,
        Core.forallTypeBody = t})) body (Lists.reverse vars))

-- | Rename all shadowed variables (both lambda parameters and let-bound variables that shadow lambda parameters) in a term.
unshadowVariables :: Core.Term -> Core.Term
unshadowVariables term0 =

      let freshName =
              \base -> \i -> \m ->
                let candidate = Core.Name (Strings.cat2 (Core.unName base) (Literals.showInt32 i))
                in (Logic.ifElse (Maps.member candidate m) (freshName base (Math.add i 1) m) candidate)
          f =
                  \recurse -> \m -> \term -> case term of
                    Core.TermFunction v0 -> case v0 of
                      Core.FunctionLambda v1 ->
                        let v = Core.lambdaParameter v1
                            domain = Core.lambdaDomain v1
                            body = Core.lambdaBody v1
                        in (Logic.ifElse (Maps.member v m) (
                          let v2 = freshName v 2 m
                              m2 = Maps.insert v v2 (Maps.insert v2 v2 m)
                          in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = v2,
                            Core.lambdaDomain = domain,
                            Core.lambdaBody = (f recurse m2 body)})))) (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = v,
                          Core.lambdaDomain = domain,
                          Core.lambdaBody = (f recurse (Maps.insert v v m) body)}))))
                      _ -> recurse m term
                    Core.TermLet v0 ->
                      let m2 =
                              Lists.foldl (\acc -> \b ->
                                let bname = Core.bindingName b
                                in (Logic.ifElse (Maps.member bname acc) acc (Maps.insert bname bname acc))) m (Core.letBindings v0)
                      in (recurse m2 term)
                    Core.TermVariable v0 -> Core.TermVariable (Maybes.maybe v0 (\renamed -> renamed) (Maps.lookup v0 m))
                    _ -> recurse m term
      in (rewriteTermWithContext f Maps.empty term0)
