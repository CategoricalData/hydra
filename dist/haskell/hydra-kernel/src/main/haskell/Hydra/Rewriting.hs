-- Note: this is an automatically generated file. Do not edit.

-- | Core rewrite and fold combinators for terms and types

module Hydra.Rewriting where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Paths as Paths
import qualified Hydra.Scoping as Scoping
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

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
foldTermWithGraphAndPath :: ((t0 -> Core.Term -> t0) -> [Paths.SubtermStep] -> Graph.Graph -> t0 -> Core.Term -> t0) -> Graph.Graph -> t0 -> Core.Term -> t0
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

-- | Apply a transformation to the first type beneath a chain of annotations
mapBeneathTypeAnnotations :: (Core.Type -> Core.Type) -> Core.Type -> Core.Type
mapBeneathTypeAnnotations f t =
    case t of
      Core.TypeAnnotated v0 -> Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (mapBeneathTypeAnnotations f (Core.annotatedTypeBody v0)),
        Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v0)})
      _ -> f t

-- | Rewrite a term, and at the same time, fold a function over it, accumulating a value
rewriteAndFoldTerm :: ((t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term)
rewriteAndFoldTerm f term0 =

      let fsub =
              \recurse -> \val0 -> \term02 ->
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
                    dflt = (val0, term02)
                in case term02 of
                  Core.TermAnnotated v0 -> forSingle recurse (\t -> Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = t,
                    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})) val0 (Core.annotatedTermBody v0)
                  Core.TermApplication v0 ->
                    let rlhs = recurse val0 (Core.applicationFunction v0)
                        rrhs = recurse (Pairs.first rlhs) (Core.applicationArgument v0)
                    in (Pairs.first rrhs, (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Pairs.second rlhs),
                      Core.applicationArgument = (Pairs.second rrhs)})))
                  Core.TermCases v0 ->
                    let rmd = Maybes.map (recurse val0) (Core.caseStatementDefault v0)
                        val1 = Maybes.maybe val0 Pairs.first rmd
                        rcases = forFields val1 (Core.caseStatementCases v0)
                    in (Pairs.first rcases, (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                      Core.caseStatementDefault = (Maybes.map Pairs.second rmd),
                      Core.caseStatementCases = (Pairs.second rcases)})))
                  Core.TermEither v0 -> Eithers.either (\l ->
                    let rl = recurse val0 l
                    in (Pairs.first rl, (Core.TermEither (Left (Pairs.second rl))))) (\r ->
                    let rr = recurse val0 r
                    in (Pairs.first rr, (Core.TermEither (Right (Pairs.second rr))))) v0
                  Core.TermLambda v0 ->
                    let rl = recurse val0 (Core.lambdaBody v0)
                    in (Pairs.first rl, (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.lambdaParameter v0),
                      Core.lambdaDomain = (Core.lambdaDomain v0),
                      Core.lambdaBody = (Pairs.second rl)})))
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
                  Core.TermProject v0 -> (val0, (Core.TermProject v0))
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
                  Core.TermInject v0 -> forSingle recurse (\t -> Core.TermInject (Core.Injection {
                    Core.injectionTypeName = (Core.injectionTypeName v0),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.fieldName (Core.injectionField v0)),
                      Core.fieldTerm = t}})) val0 (Core.fieldTerm (Core.injectionField v0))
                  Core.TermUnwrap v0 -> (val0, (Core.TermUnwrap v0))
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
                              Core.TermLambda v0 -> Scoping.extendGraphForLambda cx v0
                              Core.TermLet v0 -> Scoping.extendGraphForLet (\_ -> \_2 -> Nothing) cx v0
                              Core.TermTypeLambda v0 -> Scoping.extendGraphForTypeLambda cx v0
                              _ -> cx
                    recurseForUser =
                            \newVal -> \subterm ->
                              let result = lowLevelRecurse (newVal, cx1) subterm
                              in (Pairs.first (Pairs.first result), (Pairs.second result))
                    fResult = f recurseForUser cx1 val term
                in ((Pairs.first fResult, cx), (Pairs.second fResult))
          result = rewriteAndFoldTerm wrapper (val0, cx0) term0
      in (Pairs.first (Pairs.first result), (Pairs.second result))

-- | Rewrite a term while folding to produce a value, with both Graph and accessor path tracked. The path is a list of SubtermSteps representing the position from the root to the current term. Combines the features of rewriteAndFoldTermWithPath and Graph tracking. The Graph is automatically updated when descending into lambdas, lets, and type lambdas.
rewriteAndFoldTermWithGraphAndPath :: ((t0 -> Core.Term -> (t0, Core.Term)) -> [Paths.SubtermStep] -> Graph.Graph -> t0 -> Core.Term -> (t0, Core.Term)) -> Graph.Graph -> t0 -> Core.Term -> (t0, Core.Term)
rewriteAndFoldTermWithGraphAndPath f cx0 val0 term0 =

      let wrapper =
              \recurse -> \path -> \cxAndVal -> \term ->
                let cx = Pairs.first cxAndVal
                    val = Pairs.second cxAndVal
                    cx1 =
                            case term of
                              Core.TermLambda v0 -> Scoping.extendGraphForLambda cx v0
                              Core.TermLet v0 -> Scoping.extendGraphForLet (\_ -> \_2 -> Nothing) cx v0
                              Core.TermTypeLambda v0 -> Scoping.extendGraphForTypeLambda cx v0
                              _ -> cx
                    recurseForUser =
                            \valIn -> \termIn ->
                              let result = recurse path (cx1, valIn) termIn
                              in (Pairs.second (Pairs.first result), (Pairs.second result))
                    fResult = f recurseForUser path cx1 val term
                in ((cx, (Pairs.first fResult)), (Pairs.second fResult))
          result = rewriteAndFoldTermWithPath wrapper (cx0, val0) term0
      in (Pairs.second (Pairs.first result), (Pairs.second result))

-- | Rewrite a term with path tracking, and fold a function over it, accumulating a value. The path is a list of SubtermSteps from root to current position.
rewriteAndFoldTermWithPath :: (([Paths.SubtermStep] -> t0 -> Core.Term -> (t0, Core.Term)) -> [Paths.SubtermStep] -> t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term)
rewriteAndFoldTermWithPath f term0 =

      let fsub =
              \recurse -> \path -> \val0 -> \term02 ->
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
                              let r = recurse (Lists.concat2 path [
                                    Paths.SubtermStepLetBinding (Core.bindingName binding)]) val (Core.bindingTerm binding)
                              in (Pairs.first r, Core.Binding {
                                Core.bindingName = (Core.bindingName binding),
                                Core.bindingTerm = (Pairs.second r),
                                Core.bindingType = (Core.bindingType binding)})
                    dflt = (val0, term02)
                in case term02 of
                  Core.TermAnnotated v0 -> forSingleWithAccessor recurse (\t -> Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = t,
                    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})) Paths.SubtermStepAnnotatedBody val0 (Core.annotatedTermBody v0)
                  Core.TermApplication v0 ->
                    let rlhs = recurse (Lists.concat2 path [
                          Paths.SubtermStepApplicationFunction]) val0 (Core.applicationFunction v0)
                        rrhs = recurse (Lists.concat2 path [
                              Paths.SubtermStepApplicationArgument]) (Pairs.first rlhs) (Core.applicationArgument v0)
                    in (Pairs.first rrhs, (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Pairs.second rlhs),
                      Core.applicationArgument = (Pairs.second rrhs)})))
                  Core.TermCases v0 ->
                    let rmd =
                            Maybes.map (\def -> recurse (Lists.concat2 path [
                              Paths.SubtermStepUnionCasesDefault]) val0 def) (Core.caseStatementDefault v0)
                        val1 = Maybes.maybe val0 Pairs.first rmd
                        rcases =
                                forManyWithAccessors recurse (\x -> x) val1 (Lists.map (\f2 -> (Paths.SubtermStepUnionCasesBranch (Core.fieldName f2), (Core.fieldTerm f2))) (Core.caseStatementCases v0))
                    in (Pairs.first rcases, (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                      Core.caseStatementDefault = (Maybes.map Pairs.second rmd),
                      Core.caseStatementCases = (Lists.map (\ft -> Core.Field {
                        Core.fieldName = (Pairs.first ft),
                        Core.fieldTerm = (Pairs.second ft)}) (Lists.zip (Lists.map Core.fieldName (Core.caseStatementCases v0)) (Pairs.second rcases)))})))
                  Core.TermEither v0 -> Eithers.either (\l ->
                    let rl = recurse (Lists.concat2 path [
                          Paths.SubtermStepSumTerm]) val0 l
                    in (Pairs.first rl, (Core.TermEither (Left (Pairs.second rl))))) (\r ->
                    let rr = recurse (Lists.concat2 path [
                          Paths.SubtermStepSumTerm]) val0 r
                    in (Pairs.first rr, (Core.TermEither (Right (Pairs.second rr))))) v0
                  Core.TermLambda v0 ->
                    let rl = recurse (Lists.concat2 path [
                          Paths.SubtermStepLambdaBody]) val0 (Core.lambdaBody v0)
                    in (Pairs.first rl, (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.lambdaParameter v0),
                      Core.lambdaDomain = (Core.lambdaDomain v0),
                      Core.lambdaBody = (Pairs.second rl)})))
                  Core.TermLet v0 ->
                    let renv = recurse (Lists.concat2 path [
                          Paths.SubtermStepLetBody]) val0 (Core.letBody v0)
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
                                        Paths.SubtermStepListElement (Pairs.first r)]) (Pairs.first (Pairs.second r)) el
                                  in (Math.add (Pairs.first r) 1, (Pairs.first r2, (Lists.cons (Pairs.second r2) (Pairs.second (Pairs.second r)))))) (idx, (val0, [])) v0
                    in (Pairs.first (Pairs.second rr), (Core.TermList (Lists.reverse (Pairs.second (Pairs.second rr)))))
                  Core.TermMap v0 ->
                    let idx = 0
                        rr =
                                Lists.foldl (\r -> \kv ->
                                  let rk = recurse (Lists.concat2 path [
                                        Paths.SubtermStepMapKey (Pairs.first r)]) (Pairs.first (Pairs.second r)) (Pairs.first kv)
                                      rv = recurse (Lists.concat2 path [
                                            Paths.SubtermStepMapValue (Pairs.first r)]) (Pairs.first rk) (Pairs.second kv)
                                  in (Math.add (Pairs.first r) 1, (Pairs.first rv, (Lists.cons (Pairs.second rk, (Pairs.second rv)) (Pairs.second (Pairs.second r)))))) (idx, (val0, [])) (Maps.toList v0)
                    in (Pairs.first (Pairs.second rr), (Core.TermMap (Maps.fromList (Lists.reverse (Pairs.second (Pairs.second rr))))))
                  Core.TermMaybe v0 -> Maybes.maybe dflt (\t -> forSingleWithAccessor recurse (\t1 -> Core.TermMaybe (Just t1)) Paths.SubtermStepMaybeTerm val0 t) v0
                  Core.TermPair v0 ->
                    let rf = recurse (Lists.concat2 path [
                          Paths.SubtermStepProductTerm 0]) val0 (Pairs.first v0)
                        rs = recurse (Lists.concat2 path [
                              Paths.SubtermStepProductTerm 1]) (Pairs.first rf) (Pairs.second v0)
                    in (Pairs.first rs, (Core.TermPair (Pairs.second rf, (Pairs.second rs))))
                  Core.TermProject v0 -> (val0, (Core.TermProject v0))
                  Core.TermRecord v0 ->
                    let rfields =
                            forManyWithAccessors recurse (\x -> x) val0 (Lists.map (\f2 -> (Paths.SubtermStepRecordField (Core.fieldName f2), (Core.fieldTerm f2))) (Core.recordFields v0))
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
                                        Paths.SubtermStepSetElement (Pairs.first r)]) (Pairs.first (Pairs.second r)) el
                                  in (Math.add (Pairs.first r) 1, (Pairs.first r2, (Lists.cons (Pairs.second r2) (Pairs.second (Pairs.second r)))))) (idx, (val0, [])) (Sets.toList v0)
                    in (Pairs.first (Pairs.second rr), (Core.TermSet (Sets.fromList (Lists.reverse (Pairs.second (Pairs.second rr))))))
                  Core.TermTypeApplication v0 -> forSingleWithAccessor recurse (\t -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = t,
                    Core.typeApplicationTermType = (Core.typeApplicationTermType v0)})) Paths.SubtermStepTypeApplicationTerm val0 (Core.typeApplicationTermBody v0)
                  Core.TermTypeLambda v0 -> forSingleWithAccessor recurse (\t -> Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
                    Core.typeLambdaBody = t})) Paths.SubtermStepTypeLambdaBody val0 (Core.typeLambdaBody v0)
                  Core.TermInject v0 -> forSingleWithAccessor recurse (\t -> Core.TermInject (Core.Injection {
                    Core.injectionTypeName = (Core.injectionTypeName v0),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.fieldName (Core.injectionField v0)),
                      Core.fieldTerm = t}})) Paths.SubtermStepInjectionTerm val0 (Core.fieldTerm (Core.injectionField v0))
                  Core.TermUnwrap v0 -> (val0, (Core.TermUnwrap v0))
                  Core.TermWrap v0 -> forSingleWithAccessor recurse (\t -> Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.wrappedTermTypeName v0),
                    Core.wrappedTermBody = t})) Paths.SubtermStepWrappedTerm val0 (Core.wrappedTermBody v0)
                  _ -> dflt
          recurse = f (fsub recurse)
      in (recurse [] term0)

rewriteTerm :: ((Core.Term -> Core.Term) -> Core.Term -> Core.Term) -> Core.Term -> Core.Term
rewriteTerm f term0 =

      let fsub =
              \recurse -> \term ->
                let forField =
                        \f2 -> Core.Field {
                          Core.fieldName = (Core.fieldName f2),
                          Core.fieldTerm = (recurse (Core.fieldTerm f2))}
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
                  Core.TermCases v0 -> Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                    Core.caseStatementDefault = (Maybes.map recurse (Core.caseStatementDefault v0)),
                    Core.caseStatementCases = (Lists.map forField (Core.caseStatementCases v0))})
                  Core.TermEither v0 -> Core.TermEither (Eithers.either (\l -> Left (recurse l)) (\r -> Right (recurse r)) v0)
                  Core.TermLambda v0 -> Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.lambdaParameter v0),
                    Core.lambdaDomain = (Core.lambdaDomain v0),
                    Core.lambdaBody = (recurse (Core.lambdaBody v0))})
                  Core.TermLet v0 -> Core.TermLet (forLet v0)
                  Core.TermList v0 -> Core.TermList (Lists.map recurse v0)
                  Core.TermLiteral v0 -> Core.TermLiteral v0
                  Core.TermMap v0 -> Core.TermMap (forMap v0)
                  Core.TermMaybe v0 -> Core.TermMaybe (Maybes.map recurse v0)
                  Core.TermPair v0 -> Core.TermPair (recurse (Pairs.first v0), (recurse (Pairs.second v0)))
                  Core.TermProject v0 -> Core.TermProject v0
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
                  Core.TermInject v0 -> Core.TermInject (Core.Injection {
                    Core.injectionTypeName = (Core.injectionTypeName v0),
                    Core.injectionField = (forField (Core.injectionField v0))})
                  Core.TermUnit -> Core.TermUnit
                  Core.TermUnwrap v0 -> Core.TermUnwrap v0
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
                  Core.TermCases v0 ->
                    let n = Core.caseStatementTypeName v0
                        def = Core.caseStatementDefault v0
                        csCases = Core.caseStatementCases v0
                    in (Eithers.bind (Maybes.maybe (Right Nothing) (\t -> Eithers.map Maybes.pure (recurse t)) def) (\rdef -> Eithers.map (\rcases -> Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = n,
                      Core.caseStatementDefault = rdef,
                      Core.caseStatementCases = rcases})) (Eithers.mapList forField csCases)))
                  Core.TermEither v0 -> Eithers.bind (Eithers.either (\l -> Eithers.map (\x -> Left x) (recurse l)) (\r -> Eithers.map (\x -> Right x) (recurse r)) v0) (\re -> Right (Core.TermEither re))
                  Core.TermLambda v0 ->
                    let v = Core.lambdaParameter v0
                        d = Core.lambdaDomain v0
                        body = Core.lambdaBody v0
                    in (Eithers.bind (recurse body) (\rbody -> Right (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = v,
                      Core.lambdaDomain = d,
                      Core.lambdaBody = rbody}))))
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
                  Core.TermProject v0 -> Right (Core.TermProject v0)
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
                  Core.TermInject v0 ->
                    let n = Core.injectionTypeName v0
                        field = Core.injectionField v0
                    in (Eithers.map (\rfield -> Core.TermInject (Core.Injection {
                      Core.injectionTypeName = n,
                      Core.injectionField = rfield})) (forField field))
                  Core.TermUnit -> Right Core.TermUnit
                  Core.TermUnwrap v0 -> Right (Core.TermUnwrap v0)
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
                  Core.TermCases v0 -> Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                    Core.caseStatementDefault = (Maybes.map recurse (Core.caseStatementDefault v0)),
                    Core.caseStatementCases = (Lists.map forField (Core.caseStatementCases v0))})
                  Core.TermEither v0 -> Core.TermEither (Eithers.either (\l -> Left (recurse l)) (\r -> Right (recurse r)) v0)
                  Core.TermLambda v0 -> Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.lambdaParameter v0),
                    Core.lambdaDomain = (Core.lambdaDomain v0),
                    Core.lambdaBody = (recurse (Core.lambdaBody v0))})
                  Core.TermLet v0 -> Core.TermLet (forLet v0)
                  Core.TermList v0 -> Core.TermList (Lists.map recurse v0)
                  Core.TermLiteral v0 -> Core.TermLiteral v0
                  Core.TermMap v0 -> Core.TermMap (forMap v0)
                  Core.TermMaybe v0 -> Core.TermMaybe (Maybes.map recurse v0)
                  Core.TermPair v0 -> Core.TermPair (recurse (Pairs.first v0), (recurse (Pairs.second v0)))
                  Core.TermProject v0 -> Core.TermProject v0
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
                  Core.TermInject v0 -> Core.TermInject (Core.Injection {
                    Core.injectionTypeName = (Core.injectionTypeName v0),
                    Core.injectionField = (forField (Core.injectionField v0))})
                  Core.TermUnit -> Core.TermUnit
                  Core.TermUnwrap v0 -> Core.TermUnwrap v0
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
                  Core.TermCases v0 ->
                    let n = Core.caseStatementTypeName v0
                        def = Core.caseStatementDefault v0
                        csCases = Core.caseStatementCases v0
                    in (Eithers.bind (Maybes.maybe (Right Nothing) (\t -> Eithers.map Maybes.pure (recurse t)) def) (\rdef -> Eithers.map (\rcases -> Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = n,
                      Core.caseStatementDefault = rdef,
                      Core.caseStatementCases = rcases})) (Eithers.mapList forField csCases)))
                  Core.TermEither v0 -> Eithers.bind (Eithers.either (\l -> Eithers.map (\x -> Left x) (recurse l)) (\r -> Eithers.map (\x -> Right x) (recurse r)) v0) (\re -> Right (Core.TermEither re))
                  Core.TermLambda v0 ->
                    let v = Core.lambdaParameter v0
                        d = Core.lambdaDomain v0
                        body = Core.lambdaBody v0
                    in (Eithers.bind (recurse body) (\rbody -> Right (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = v,
                      Core.lambdaDomain = d,
                      Core.lambdaBody = rbody}))))
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
                  Core.TermProject v0 -> Right (Core.TermProject v0)
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
                  Core.TermInject v0 ->
                    let n = Core.injectionTypeName v0
                        field = Core.injectionField v0
                    in (Eithers.map (\rfield -> Core.TermInject (Core.Injection {
                      Core.injectionTypeName = n,
                      Core.injectionField = rfield})) (forField field))
                  Core.TermUnit -> Right Core.TermUnit
                  Core.TermUnwrap v0 -> Right (Core.TermUnwrap v0)
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
                let recurse1 = \term2 -> recurse cx term2
                in case term of
                  Core.TermLambda v0 ->
                    let cx1 = Scoping.extendGraphForLambda cx v0
                        recurse2 = \term2 -> recurse cx1 term2
                    in (f recurse2 cx1 term)
                  Core.TermLet v0 ->
                    let cx1 = Scoping.extendGraphForLet (\_ -> \_2 -> Nothing) cx v0
                        recurse2 = \term2 -> recurse cx1 term2
                    in (f recurse2 cx1 term)
                  Core.TermTypeLambda v0 ->
                    let cx1 = Scoping.extendGraphForTypeLambda cx v0
                        recurse2 = \term2 -> recurse cx1 term2
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
                          \f2 -> Eithers.bind (recurse (Core.fieldTypeType f2)) (\t -> Right (Core.FieldType {
                            Core.fieldTypeName = (Core.fieldTypeName f2),
                            Core.fieldTypeType = t}))
                  in (Eithers.bind (Eithers.mapList forField v0) (\rfields -> Right (Core.TypeRecord rfields)))
                Core.TypeSet v0 -> Eithers.bind (recurse v0) (\rt -> Right (Core.TypeSet rt))
                Core.TypeUnion v0 ->
                  let forField =
                          \f2 -> Eithers.bind (recurse (Core.fieldTypeType f2)) (\t -> Right (Core.FieldType {
                            Core.fieldTypeName = (Core.fieldTypeName f2),
                            Core.fieldTypeType = t}))
                  in (Eithers.bind (Eithers.mapList forField v0) (\rfields -> Right (Core.TypeUnion rfields)))
                Core.TypeUnit -> Right Core.TypeUnit
                Core.TypeVariable v0 -> Right (Core.TypeVariable v0)
                Core.TypeVoid -> Right Core.TypeVoid
                Core.TypeWrap v0 -> Eithers.bind (recurse v0) (\t -> Right (Core.TypeWrap t))
          recurse = f (fsub recurse)
      in (recurse typ0)

-- | Find the children of a given term
subterms :: Core.Term -> [Core.Term]
subterms x =
    case x of
      Core.TermAnnotated v0 -> [
        Core.annotatedTermBody v0]
      Core.TermApplication v0 -> [
        Core.applicationFunction v0,
        (Core.applicationArgument v0)]
      Core.TermCases v0 -> Lists.concat2 (Maybes.maybe [] (\t -> [
        t]) (Core.caseStatementDefault v0)) (Lists.map Core.fieldTerm (Core.caseStatementCases v0))
      Core.TermEither v0 -> Eithers.either (\l -> [
        l]) (\r -> [
        r]) v0
      Core.TermLambda v0 -> [
        Core.lambdaBody v0]
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
      Core.TermProject _ -> []
      Core.TermRecord v0 -> Lists.map Core.fieldTerm (Core.recordFields v0)
      Core.TermSet v0 -> Sets.toList v0
      Core.TermTypeApplication v0 -> [
        Core.typeApplicationTermBody v0]
      Core.TermTypeLambda v0 -> [
        Core.typeLambdaBody v0]
      Core.TermInject v0 -> [
        Core.fieldTerm (Core.injectionField v0)]
      Core.TermUnit -> []
      Core.TermUnwrap _ -> []
      Core.TermVariable _ -> []
      Core.TermWrap v0 -> [
        Core.wrappedTermBody v0]

-- | Find the children of a given term
subtermsWithSteps :: Core.Term -> [(Paths.SubtermStep, Core.Term)]
subtermsWithSteps x =
    case x of
      Core.TermAnnotated v0 -> [
        (Paths.SubtermStepAnnotatedBody, (Core.annotatedTermBody v0))]
      Core.TermApplication v0 -> [
        (Paths.SubtermStepApplicationFunction, (Core.applicationFunction v0)),
        (Paths.SubtermStepApplicationArgument, (Core.applicationArgument v0))]
      Core.TermCases v0 -> Lists.concat2 (Maybes.maybe [] (\t -> [
        (Paths.SubtermStepUnionCasesDefault, t)]) (Core.caseStatementDefault v0)) (Lists.map (\f -> (Paths.SubtermStepUnionCasesBranch (Core.fieldName f), (Core.fieldTerm f))) (Core.caseStatementCases v0))
      Core.TermEither _ -> []
      Core.TermLambda v0 -> [
        (Paths.SubtermStepLambdaBody, (Core.lambdaBody v0))]
      Core.TermLet v0 -> Lists.cons (Paths.SubtermStepLetBody, (Core.letBody v0)) (Lists.map (\b -> (Paths.SubtermStepLetBinding (Core.bindingName b), (Core.bindingTerm b))) (Core.letBindings v0))
      Core.TermList v0 -> Lists.map (\e -> (Paths.SubtermStepListElement 0, e)) v0
      Core.TermLiteral _ -> []
      Core.TermMap v0 -> Lists.concat (Lists.map (\p -> [
        (Paths.SubtermStepMapKey 0, (Pairs.first p)),
        (Paths.SubtermStepMapValue 0, (Pairs.second p))]) (Maps.toList v0))
      Core.TermMaybe v0 -> Maybes.maybe [] (\t -> [
        (Paths.SubtermStepMaybeTerm, t)]) v0
      Core.TermPair _ -> []
      Core.TermProject _ -> []
      Core.TermRecord v0 -> Lists.map (\f -> (Paths.SubtermStepRecordField (Core.fieldName f), (Core.fieldTerm f))) (Core.recordFields v0)
      Core.TermSet v0 -> Lists.map (\e -> (Paths.SubtermStepListElement 0, e)) (Sets.toList v0)
      Core.TermTypeApplication v0 -> [
        (Paths.SubtermStepTypeApplicationTerm, (Core.typeApplicationTermBody v0))]
      Core.TermTypeLambda v0 -> [
        (Paths.SubtermStepTypeLambdaBody, (Core.typeLambdaBody v0))]
      Core.TermInject v0 -> [
        (Paths.SubtermStepInjectionTerm, (Core.fieldTerm (Core.injectionField v0)))]
      Core.TermUnit -> []
      Core.TermUnwrap _ -> []
      Core.TermVariable _ -> []
      Core.TermWrap v0 -> [
        (Paths.SubtermStepWrappedTerm, (Core.wrappedTermBody v0))]

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
