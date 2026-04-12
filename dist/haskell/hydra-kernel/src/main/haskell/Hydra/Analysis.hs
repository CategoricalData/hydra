-- Note: this is an automatically generated file. Do not edit.

-- | Module dependency namespace analysis

module Hydra.Analysis where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Checking as Checking
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Encode.Core as Core__
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Strip as Strip
import qualified Hydra.Typing as Typing
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Set as S

-- | Add names to existing namespaces mapping
addNamesToNamespaces :: (Packaging.Namespace -> t0) -> S.Set Core.Name -> Packaging.Namespaces t0 -> Packaging.Namespaces t0
addNamesToNamespaces encodeNamespace names ns0 =

      let nss = Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList names)))
          toPair = \ns -> (ns, (encodeNamespace ns))
      in Packaging.Namespaces {
        Packaging.namespacesFocus = (Packaging.namespacesFocus ns0),
        Packaging.namespacesMapping = (Maps.union (Packaging.namespacesMapping ns0) (Maps.fromList (Lists.map toPair (Sets.toList nss))))}

-- | Analyze a function term, collecting lambdas, type lambdas, lets, and type applications
analyzeFunctionTerm :: Context.Context -> (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t0) -> t0 -> Core.Term -> Either t1 (Typing.FunctionStructure t0)
analyzeFunctionTerm cx getTC setTC env term =
    analyzeFunctionTermWith cx (\g -> \b -> Logic.ifElse (Predicates.isComplexBinding g b) (Just (Core.TermLiteral (Core.LiteralBoolean True))) Nothing) getTC setTC env term

-- | Analyze a function term with configurable binding metadata
analyzeFunctionTermWith :: Context.Context -> (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t0) -> t0 -> Core.Term -> Either t1 (Typing.FunctionStructure t0)
analyzeFunctionTermWith cx forBinding getTC setTC env term =
    analyzeFunctionTermWith_gather cx forBinding getTC setTC True env [] [] [] [] [] term

analyzeFunctionTermWith_finish :: Context.Context -> (t0 -> Graph.Graph) -> t0 -> [Core.Name] -> [Core.Name] -> [Core.Binding] -> [Core.Type] -> [Core.Type] -> Core.Term -> Either t1 (Typing.FunctionStructure t0)
analyzeFunctionTermWith_finish cx getTC fEnv tparams args bindings doms tapps body =

      let bodyWithTapps =
              Lists.foldl (\trm -> \typ -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = trm,
                Core.typeApplicationTermType = typ})) body tapps
          mcod = Eithers.either (\_ -> Nothing) (\c -> Just c) (Checking.typeOfTerm cx (getTC fEnv) bodyWithTapps)
      in (Right (Typing.FunctionStructure {
        Typing.functionStructureTypeParams = (Lists.reverse tparams),
        Typing.functionStructureParams = (Lists.reverse args),
        Typing.functionStructureBindings = bindings,
        Typing.functionStructureBody = bodyWithTapps,
        Typing.functionStructureDomains = (Lists.reverse doms),
        Typing.functionStructureCodomain = mcod,
        Typing.functionStructureEnvironment = fEnv}))

analyzeFunctionTermWith_gather :: Context.Context -> (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t0) -> Bool -> t0 -> [Core.Name] -> [Core.Name] -> [Core.Binding] -> [Core.Type] -> [Core.Type] -> Core.Term -> Either t1 (Typing.FunctionStructure t0)
analyzeFunctionTermWith_gather cx forBinding getTC setTC argMode gEnv tparams args bindings doms tapps t =
    case (Strip.deannotateTerm t) of
      Core.TermFunction v0 -> case v0 of
        Core.FunctionLambda v1 -> Logic.ifElse argMode (
          let v = Core.lambdaParameter v1
              dom = Maybes.maybe (Core.TypeVariable (Core.Name "_")) (\x_ -> x_) (Core.lambdaDomain v1)
              body = Core.lambdaBody v1
              newEnv = setTC (Scoping.extendGraphForLambda (getTC gEnv) v1) gEnv
          in (analyzeFunctionTermWith_gather cx forBinding getTC setTC argMode newEnv tparams (Lists.cons v args) bindings (Lists.cons dom doms) tapps body)) (analyzeFunctionTermWith_finish cx getTC gEnv tparams args bindings doms tapps t)
        _ -> analyzeFunctionTermWith_finish cx getTC gEnv tparams args bindings doms tapps t
      Core.TermLet v0 ->
        let newBindings = Core.letBindings v0
            body = Core.letBody v0
            newEnv = setTC (Scoping.extendGraphForLet forBinding (getTC gEnv) v0) gEnv
        in (analyzeFunctionTermWith_gather cx forBinding getTC setTC False newEnv tparams args (Lists.concat2 bindings newBindings) doms tapps body)
      Core.TermTypeApplication v0 ->
        let taBody = Core.typeApplicationTermBody v0
            typ = Core.typeApplicationTermType v0
        in (analyzeFunctionTermWith_gather cx forBinding getTC setTC argMode gEnv tparams args bindings doms (Lists.cons typ tapps) taBody)
      Core.TermTypeLambda v0 ->
        let tvar = Core.typeLambdaParameter v0
            tlBody = Core.typeLambdaBody v0
            newEnv = setTC (Scoping.extendGraphForTypeLambda (getTC gEnv) v0) gEnv
        in (analyzeFunctionTermWith_gather cx forBinding getTC setTC argMode newEnv (Lists.cons tvar tparams) args bindings doms tapps tlBody)
      _ -> analyzeFunctionTermWith_finish cx getTC gEnv tparams args bindings doms tapps t

-- | Get dependency namespaces from definitions
definitionDependencyNamespaces :: [Packaging.Definition] -> S.Set Packaging.Namespace
definitionDependencyNamespaces defs =

      let defNames =
              \def -> case def of
                Packaging.DefinitionType v0 -> Dependencies.typeDependencyNames True (Core.typeSchemeType (Packaging.typeDefinitionType v0))
                Packaging.DefinitionTerm v0 -> Dependencies.termDependencyNames True True True (Packaging.termDefinitionTerm v0)
          allNames = Sets.unions (Lists.map defNames defs)
      in (Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList allNames))))

-- | Find dependency namespaces in all of a set of terms (Either version)
dependencyNamespaces :: t0 -> Graph.Graph -> Bool -> Bool -> Bool -> Bool -> [Core.Binding] -> Either Errors.Error (S.Set Packaging.Namespace)
dependencyNamespaces cx graph binds withPrims withNoms withSchema els =

      let depNames =
              \el ->
                let term = Core.bindingTerm el
                    deannotatedTerm = Strip.deannotateTerm term
                    dataNames = Dependencies.termDependencyNames binds withPrims withNoms term
                    schemaNames =
                            Logic.ifElse withSchema (Maybes.maybe Sets.empty (\ts -> Dependencies.typeDependencyNames True (Core.typeSchemeType ts)) (Core.bindingType el)) Sets.empty
                in (Logic.ifElse (Predicates.isEncodedType deannotatedTerm) (Eithers.map (\typ -> Sets.unions [
                  dataNames,
                  schemaNames,
                  (Dependencies.typeDependencyNames True typ)]) (Eithers.bimap (\_e -> Errors.ErrorDecoding _e) (\_a -> _a) (Core_.type_ graph term))) (Logic.ifElse (Predicates.isEncodedTerm deannotatedTerm) (Eithers.map (\decodedTerm -> Sets.unions [
                  dataNames,
                  schemaNames,
                  (Dependencies.termDependencyNames binds withPrims withNoms decodedTerm)]) (Eithers.bimap (\_e -> Errors.ErrorDecoding _e) (\_a -> _a) (Core_.term graph term))) (Right (Sets.unions [
                  dataNames,
                  schemaNames]))))
      in (Eithers.map (\namesList -> Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList (Sets.unions namesList))))) (Eithers.mapList depNames els))

-- | Gather applications from a term, returning (args, baseTerm)
gatherApplications :: Core.Term -> ([Core.Term], Core.Term)
gatherApplications term =

      let go =
              \args -> \t -> case (Strip.deannotateTerm t) of
                Core.TermApplication v0 ->
                  let lhs = Core.applicationFunction v0
                      rhs = Core.applicationArgument v0
                  in (go (Lists.cons rhs args) lhs)
                _ -> (args, t)
      in (go [] term)

-- | Gather term arguments, stripping type-level constructs
gatherArgs :: Core.Term -> [Core.Term] -> (Core.Term, [Core.Term])
gatherArgs term args =
    case (Strip.deannotateTerm term) of
      Core.TermApplication v0 ->
        let lhs = Core.applicationFunction v0
            rhs = Core.applicationArgument v0
        in (gatherArgs lhs (Lists.cons rhs args))
      Core.TermTypeLambda v0 ->
        let body = Core.typeLambdaBody v0
        in (gatherArgs body args)
      Core.TermTypeApplication v0 ->
        let body = Core.typeApplicationTermBody v0
        in (gatherArgs body args)
      _ -> (term, args)

-- | Gather term and type arguments from a term
gatherArgsWithTypeApps :: Core.Term -> [Core.Term] -> [Core.Type] -> (Core.Term, ([Core.Term], [Core.Type]))
gatherArgsWithTypeApps term args tyArgs =
    case (Strip.deannotateTerm term) of
      Core.TermApplication v0 ->
        let lhs = Core.applicationFunction v0
            rhs = Core.applicationArgument v0
        in (gatherArgsWithTypeApps lhs (Lists.cons rhs args) tyArgs)
      Core.TermTypeLambda v0 ->
        let body = Core.typeLambdaBody v0
        in (gatherArgsWithTypeApps body args tyArgs)
      Core.TermTypeApplication v0 ->
        let body = Core.typeApplicationTermBody v0
            typ = Core.typeApplicationTermType v0
        in (gatherArgsWithTypeApps body args (Lists.cons typ tyArgs))
      _ -> (term, (args, tyArgs))

-- | Check if a term body is self-tail-recursive with respect to a function name
isSelfTailRecursive :: Core.Name -> Core.Term -> Bool
isSelfTailRecursive funcName body =

      let callsSelf = Logic.not (Variables.isFreeVariableInTerm funcName body)
      in (Logic.ifElse callsSelf (isTailRecursiveInTailPosition funcName body) False)

-- | Check if a term can be encoded as a simple assignment
isSimpleAssignment :: Core.Term -> Bool
isSimpleAssignment term =
    case term of
      Core.TermAnnotated v0 -> isSimpleAssignment (Core.annotatedTermBody v0)
      Core.TermFunction v0 -> case v0 of
        Core.FunctionLambda _ -> False
        _ -> True
      Core.TermLet _ -> False
      Core.TermTypeLambda _ -> False
      Core.TermTypeApplication v0 -> isSimpleAssignment (Core.typeApplicationTermBody v0)
      _ ->
        let baseTerm = Pairs.first (gatherArgs term [])
        in case baseTerm of
          Core.TermFunction v0 -> case v0 of
            Core.FunctionElimination v1 -> case v1 of
              Core.EliminationUnion _ -> False
              _ -> True
            _ -> True
          _ -> True

-- | Check that all self-references are in tail position
isTailRecursiveInTailPosition :: Core.Name -> Core.Term -> Bool
isTailRecursiveInTailPosition funcName term =

      let stripped = Strip.deannotateAndDetypeTerm term
      in case stripped of
        Core.TermApplication _ ->
          let gathered = gatherApplications stripped
              gatherArgs = Pairs.first gathered
              gatherFun = Pairs.second gathered
              strippedFun = Strip.deannotateAndDetypeTerm gatherFun
          in case strippedFun of
            Core.TermVariable v1 -> Logic.ifElse (Equality.equal v1 funcName) (
              let argsNoFunc = Lists.foldl (\ok -> \arg -> Logic.and ok (Variables.isFreeVariableInTerm funcName arg)) True gatherArgs
                  argsNoLambda =
                          Lists.foldl (\ok -> \arg -> Logic.and ok (Logic.not (Rewriting.foldOverTerm Coders.TraversalOrderPre (\found -> \t -> Logic.or found (case t of
                            Core.TermFunction v2 -> case v2 of
                              Core.FunctionLambda v3 ->
                                let ignore = Core.lambdaBody v3
                                in True
                              _ -> False
                            _ -> False)) False arg))) True gatherArgs
              in (Logic.and argsNoFunc argsNoLambda)) (Variables.isFreeVariableInTerm funcName term)
            Core.TermFunction v1 -> case v1 of
              Core.FunctionElimination v2 -> case v2 of
                Core.EliminationUnion v3 ->
                  let cases_ = Core.caseStatementCases v3
                      dflt = Core.caseStatementDefault v3
                      branchesOk =
                              Lists.foldl (\ok -> \field -> Logic.and ok (isTailRecursiveInTailPosition funcName (Core.fieldTerm field))) True cases_
                      dfltOk = Maybes.maybe True (\d -> isTailRecursiveInTailPosition funcName d) dflt
                      argsOk = Lists.foldl (\ok -> \arg -> Logic.and ok (Variables.isFreeVariableInTerm funcName arg)) True gatherArgs
                  in (Logic.and (Logic.and branchesOk dfltOk) argsOk)
                _ -> Variables.isFreeVariableInTerm funcName term
              _ -> Variables.isFreeVariableInTerm funcName term
            _ -> Variables.isFreeVariableInTerm funcName term
        Core.TermFunction v0 -> case v0 of
          Core.FunctionLambda v1 -> isTailRecursiveInTailPosition funcName (Core.lambdaBody v1)
          _ -> Variables.isFreeVariableInTerm funcName term
        Core.TermLet v0 ->
          let bindingsOk =
                  Lists.foldl (\ok -> \b -> Logic.and ok (Variables.isFreeVariableInTerm funcName (Core.bindingTerm b))) True (Core.letBindings v0)
          in (Logic.and bindingsOk (isTailRecursiveInTailPosition funcName (Core.letBody v0)))
        _ -> Variables.isFreeVariableInTerm funcName term

-- | Check whether a module contains any binary literal values
moduleContainsBinaryLiterals :: Packaging.Module -> Bool
moduleContainsBinaryLiterals mod =

      let checkTerm =
              \found -> \term -> Logic.or found (case term of
                Core.TermLiteral v0 -> case v0 of
                  Core.LiteralBinary _ -> True
                  _ -> False
                _ -> False)
          termContainsBinary = \term -> Rewriting.foldOverTerm Coders.TraversalOrderPre checkTerm False term
          defTerms =
                  Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Packaging.termDefinitionTerm v0)
                    _ -> Nothing) (Packaging.moduleDefinitions mod))
      in (Lists.foldl (\acc -> \t -> Logic.or acc (termContainsBinary t)) False defTerms)

-- | Find dependency namespaces in all elements of a module, excluding the module's own namespace (Either version)
moduleDependencyNamespaces :: t0 -> Graph.Graph -> Bool -> Bool -> Bool -> Bool -> Packaging.Module -> Either Errors.Error (S.Set Packaging.Namespace)
moduleDependencyNamespaces cx graph binds withPrims withNoms withSchema mod =

      let allBindings =
              Maybes.cat (Lists.map (\d -> case d of
                Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
                  let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                      dataTerm =
                              Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                Core.annotatedTermBody = (Core__.type_ typ),
                                Core.annotatedTermAnnotation = (Maps.fromList [
                                  (Constants.key_type, schemaTerm)])}))
                  in Core.Binding {
                    Core.bindingName = name,
                    Core.bindingTerm = dataTerm,
                    Core.bindingType = (Just (Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                      Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeType (Packaging.typeDefinitionType v0)))
                Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                  Core.bindingName = (Packaging.termDefinitionName v0),
                  Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                  Core.bindingType = (Packaging.termDefinitionType v0)})
                _ -> Nothing) (Packaging.moduleDefinitions mod))
      in (Eithers.map (\deps -> Sets.delete (Packaging.moduleNamespace mod) deps) (dependencyNamespaces cx graph binds withPrims withNoms withSchema allBindings))

-- | Create namespaces mapping for definitions
namespacesForDefinitions :: (Packaging.Namespace -> t0) -> Packaging.Namespace -> [Packaging.Definition] -> Packaging.Namespaces t0
namespacesForDefinitions encodeNamespace focusNs defs =

      let nss = Sets.delete focusNs (definitionDependencyNamespaces defs)
          toPair = \ns -> (ns, (encodeNamespace ns))
      in Packaging.Namespaces {
        Packaging.namespacesFocus = (toPair focusNs),
        Packaging.namespacesMapping = (Maps.fromList (Lists.map toPair (Sets.toList nss)))}
