-- Note: this is an automatically generated file. Do not edit.
-- | Dependency extraction, binding sort, and let normalization

module Hydra.Dependencies where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
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
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Strip as Strip
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | Get definitions with their dependencies
definitionsWithDependencies :: t0 -> Graph.Graph -> [Core.Binding] -> Either Errors.Error [Core.Binding]
definitionsWithDependencies cx graph original =

      let depNames = \el -> Sets.toList (termDependencyNames True False False (Core.bindingTerm el))
          allDepNames = Lists.nub (Lists.concat2 (Lists.map Core.bindingName original) (Lists.concat (Lists.map depNames original)))
      in (Eithers.mapList (\name -> Lexical.requireBinding graph name) allDepNames)
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
                        replaceVars = Variables.substituteVariables subst
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
                  \recurse -> \term2 ->
                    let rewritten = recurse term2
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
      in (Rewriting.rewriteTerm flatten term)
-- | Inline all type variables in a type using the provided schema (Either version). Note: this function is only appropriate for nonrecursive type definitions
inlineType :: M.Map Core.Name Core.Type -> Core.Type -> Either Errors.Error Core.Type
inlineType schema typ =

      let f =
              \recurse -> \typ2 ->
                let afterRecurse =
                        \tr -> case tr of
                          Core.TypeVariable v0 -> Maybes.maybe (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "No such type in schema: " (Core.unName v0))))) (inlineType schema) (Maps.lookup v0 schema)
                          _ -> Right tr
                in (Eithers.bind (recurse typ2) (\tr -> afterRecurse tr))
      in (Rewriting.rewriteTypeM f typ)
-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: Core.Term -> Bool
isLambda term =
    case (Strip.deannotateTerm term) of
      Core.TermLambda _ -> True
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
                            \original -> \cons -> \term2 -> case term2 of
                              Core.TermAnnotated v0 -> digForLambdas original (\t -> Core.TermAnnotated (Core.AnnotatedTerm {
                                Core.annotatedTermBody = (cons t),
                                Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})) (Core.annotatedTermBody v0)
                              Core.TermLambda v0 -> Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.lambdaParameter v0),
                                Core.lambdaDomain = (Core.lambdaDomain v0),
                                Core.lambdaBody = (digForLambdas (cons (Core.lambdaBody v0)) (\t -> cons t) (Core.lambdaBody v0))})
                              Core.TermLet v0 -> digForLambdas original (\t -> cons (Core.TermLet (Core.Let {
                                Core.letBindings = (rewriteBindings (Core.letBindings v0)),
                                Core.letBody = t}))) (Core.letBody v0)
                              _ -> recurse original
                in case term of
                  Core.TermLet v0 -> digForLambdas term (\t -> Core.TermLet (Core.Let {
                    Core.letBindings = (rewriteBindings (Core.letBindings v0)),
                    Core.letBody = t})) (Core.letBody v0)
                  _ -> recurse term
      in (Rewriting.rewriteTerm rewrite term0)
-- | Given a let expression, remove any unused bindings. The resulting expression is still a let, even if has no remaining bindings
pruneLet :: Core.Let -> Core.Let
pruneLet l =

      let bindingMap = Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) (Core.letBindings l))
          rootName = Core.Name "[[[root]]]"
          adj =
                  \n -> Sets.intersection (Sets.fromList (Maps.keys bindingMap)) (Variables.freeVariablesInTerm (Logic.ifElse (Equality.equal n rootName) (Core.letBody l) (Maybes.fromMaybe Core.TermUnit (Maps.lookup n bindingMap))))
          reachable = Sorting.findReachableNodes adj rootName
          prunedBindings = Lists.filter (\b -> Sets.member (Core.bindingName b) reachable) (Core.letBindings l)
      in Core.Let {
        Core.letBindings = prunedBindings,
        Core.letBody = (Core.letBody l)}
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
      in (Rewriting.rewriteType rewrite typ0)
-- | Simplify terms by applying beta reduction where possible
simplifyTerm :: Core.Term -> Core.Term
simplifyTerm term =

      let simplify =
              \recurse -> \term2 ->
                let forRhs =
                        \rhs -> \var -> \body -> case (Strip.deannotateTerm rhs) of
                          Core.TermVariable v0 -> simplifyTerm (Variables.substituteVariable var v0 body)
                          _ -> term2
                    forLhs =
                            \lhs -> \rhs -> case (Strip.deannotateTerm lhs) of
                              Core.TermLambda v0 ->
                                let var = Core.lambdaParameter v0
                                    body = Core.lambdaBody v0
                                in (Logic.ifElse (Sets.member var (Variables.freeVariablesInTerm body)) (forRhs rhs var body) (simplifyTerm body))
                              _ -> term2
                    forTerm =
                            \stripped -> case stripped of
                              Core.TermApplication v0 ->
                                let lhs = Core.applicationFunction v0
                                    rhs = Core.applicationArgument v0
                                in (forLhs lhs rhs)
                              _ -> term2
                    stripped = Strip.deannotateTerm term2
                in (recurse (forTerm stripped))
      in (Rewriting.rewriteTerm simplify term)
-- | Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that
termDependencyNames :: Bool -> Bool -> Bool -> Core.Term -> S.Set Core.Name
termDependencyNames binds withPrims withNoms term0 =

      let addNames =
              \names -> \term ->
                let nominal = \name -> Logic.ifElse withNoms (Sets.insert name names) names
                    prim = \name -> Logic.ifElse withPrims (Sets.insert name names) names
                    var = \name -> Logic.ifElse binds (Sets.insert name names) names
                in case term of
                  Core.TermCases v0 -> nominal (Core.caseStatementTypeName v0)
                  Core.TermProject v0 -> nominal (Core.projectionTypeName v0)
                  Core.TermUnwrap v0 -> nominal v0
                  Core.TermRecord v0 -> nominal (Core.recordTypeName v0)
                  Core.TermInject v0 -> nominal (Core.injectionTypeName v0)
                  Core.TermVariable v0 -> var v0
                  Core.TermWrap v0 -> nominal (Core.wrappedTermTypeName v0)
                  _ -> names
      in (Rewriting.foldOverTerm Coders.TraversalOrderPre addNames Sets.empty term0)
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
                    in (name, (Logic.ifElse (hasTypeAnnotation term) [] (Sets.toList (Sets.intersection keys (Variables.freeVariablesInTerm term)))))
          toPair =
                  \name -> (name, (Maybes.fromMaybe (Core.TermLiteral (Core.LiteralString "Impossible!")) (Maps.lookup name bindingMap)))
      in (Lists.map (Lists.map toPair) (Sorting.topologicalSortComponents (Lists.map depsOf bindings)))
-- | Topological sort of elements based on their dependencies
topologicalSortBindings :: [Core.Binding] -> Either [[Core.Name]] [Core.Name]
topologicalSortBindings els =

      let adjlist = \e -> (Core.bindingName e, (Sets.toList (termDependencyNames False True True (Core.bindingTerm e))))
      in (Sorting.topologicalSort (Lists.map adjlist els))
-- | Topologically sort type definitions by dependencies
topologicalSortTypeDefinitions :: [Packaging.TypeDefinition] -> [[Packaging.TypeDefinition]]
topologicalSortTypeDefinitions defs =

      let toPair =
              \def -> (Packaging.typeDefinitionName def, (Sets.toList (typeDependencyNames False (Core.typeSchemeType (Packaging.typeDefinitionType def)))))
          nameToDef = Maps.fromList (Lists.map (\d -> (Packaging.typeDefinitionName d, d)) defs)
          sorted = Sorting.topologicalSortComponents (Lists.map toPair defs)
      in (Lists.map (\names -> Maybes.cat (Lists.map (\n -> Maps.lookup n nameToDef) names)) sorted)
typeDependencyNames :: Bool -> Core.Type -> S.Set Core.Name
typeDependencyNames withSchema typ =
    Logic.ifElse withSchema (Sets.union (Variables.freeVariablesInType typ) (typeNamesInType typ)) (Variables.freeVariablesInType typ)
typeNamesInType :: Ord t0 => (Core.Type -> S.Set t0)
typeNamesInType typ0 =

      let addNames = \names -> \typ -> names
      in (Rewriting.foldOverType Coders.TraversalOrderPre addNames Sets.empty typ0)
