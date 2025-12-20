
module Hydra.Sources.Kernel.Terms.Inference where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  bindConstraints, bindUnboundTypeVariables, buildTypeApplicationTerm, emptyInferenceContext,
  extendContext, finalizeInferredTerm, forInferredTerm, freeVariablesInContext, freshVariableType,
  generalize, inferGraphTypes, inferInGraphContext, inferMany, inferTypeOf, inferTypeOfAnnotatedTerm,
  inferTypeOfApplication, inferTypeOfCaseStatement, inferTypeOfCollection, inferTypeOfEither,
  inferTypeOfElimination, inferTypeOfFunction, inferTypeOfInjection, inferTypeOfLambda, inferTypeOfLet,
  inferTypeOfLetNormalized, inferTypeOfList, inferTypeOfLiteral, inferTypeOfMap, inferTypeOfOptional,
  inferTypeOfPair, inferTypeOfPrimitive, inferTypeOfProjection, inferTypeOfRecord, inferTypeOfSet,
  inferTypeOfTerm, inferTypeOfTypeLambda, inferTypeOfTypeApplication, inferTypeOfUnit, inferTypeOfUnwrap,
  inferTypeOfVariable, inferTypeOfWrappedTerm, inferTypesOfTemporaryBindings, initialTypeContext,
  isUnbound, mapConstraints, showInferenceResult, yield, yieldChecked, yieldDebug)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Annotations  as Annotations
import qualified Hydra.Sources.Kernel.Terms.Checking     as Checking
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads       as Monads
import qualified Hydra.Sources.Kernel.Terms.Reflect      as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting    as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas      as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core    as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Typing  as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting      as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution
import qualified Hydra.Sources.Kernel.Terms.Unification  as Unification


ns :: Namespace
ns = Namespace "hydra.inference"

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, Checking.ns, ExtractCore.ns, Lexical.ns, Monads.ns, Reflect.ns,
      Rewriting.ns, Schemas.ns, ShowCore.ns, ShowTyping.ns, Sorting.ns, Substitution.ns,
      Unification.ns]
    kernelTypesNamespaces $
    Just "Type inference following Algorithm W, extended for nominal terms and types"
  where
    elements = [
      toBinding bindConstraints,
      toBinding bindUnboundTypeVariables,
      toBinding buildTypeApplicationTerm,
      toBinding emptyInferenceContext,
      toBinding extendContext,
      toBinding finalizeInferredTerm,
      toBinding forInferredTerm,
      toBinding freeVariablesInContext,
      toBinding freshVariableType,
      toBinding generalize,
      toBinding inferGraphTypes,
      toBinding inferInGraphContext,
      toBinding inferMany,
      toBinding inferTypeOfAnnotatedTerm,
      toBinding inferTypeOfApplication,
      toBinding inferTypeOfCaseStatement,
      toBinding inferTypeOfCollection,
      toBinding inferTypeOf,
      toBinding inferTypeOfEither,
      toBinding inferTypeOfElimination,
      toBinding inferTypeOfFunction,
      toBinding inferTypeOfInjection,
      toBinding inferTypeOfLambda,
      toBinding inferTypeOfLetNormalized,
      toBinding inferTypeOfLet,
      toBinding inferTypeOfList,
      toBinding inferTypeOfLiteral,
      toBinding inferTypeOfMap,
      toBinding inferTypeOfOptional,
      toBinding inferTypeOfPair,
      toBinding inferTypeOfPrimitive,
      toBinding inferTypeOfProjection,
      toBinding inferTypeOfRecord,
      toBinding inferTypeOfSet,
      toBinding inferTypeOfTerm,
      toBinding inferTypeOfTypeLambda,
      toBinding inferTypeOfTypeApplication,
      toBinding inferTypeOfUnit,
      toBinding inferTypeOfUnwrap,
      toBinding inferTypeOfVariable,
      toBinding inferTypeOfWrappedTerm,
      toBinding inferTypesOfTemporaryBindings,
      toBinding initialTypeContext,
      toBinding isUnbound,
      toBinding mapConstraints,
      toBinding showInferenceResult,
      toBinding yield,
      toBinding yieldChecked,
      toBinding yieldDebug]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

--

bindConstraints :: TBinding (InferenceContext -> (TypeSubst -> Flow s a) -> [TypeConstraint] -> Flow s a)
bindConstraints = define "bindConstraints" $
  doc "Bind type constraints and continue with substitution" $
  "cx" ~> "f" ~> "constraints" ~>
  "s" <<~ Unification.unifyTypeConstraints @@ Typing.inferenceContextSchemaTypes (var "cx") @@ var "constraints" $
  exec (Checking.checkTypeSubst @@ var "cx" @@ var "s") $
  var "f" @@ var "s"

bindUnboundTypeVariables :: TBinding (InferenceContext -> Term -> Term)
bindUnboundTypeVariables = define "bindUnboundTypeVariables" $
  doc ("Place unbound type variables appearing anywhere under a typed let binding in the type scheme of that binding."
    <> " These variables may appear in the binding type scheme itself or in that of a subterm,"
    <> " in domain types attached to functions, and in type abstraction and type application terms."
    <> " This process attempts to capture type variables which have escaped unification, e.g. due to unused code."
    <> " However, unbound type variables not appearing beneath any typed let binding remain unbound.") $
  "cx" ~> "term0" ~>
  "svars" <~ Sets.fromList (Maps.keys $ Typing.inferenceContextSchemaTypes $ var "cx") $
  "rewrite" <~ ("recurse" ~> "term" ~> cases _Term (var "term")
    (Just $ var "recurse" @@ var "term") [
    _Term_let>>: "l" ~>
      "forBinding" <~ ("b" ~>
        "bname" <~ (Core.bindingName $ var "b") $
        "bterm" <~ (Core.bindingTerm $ var "b") $
        optCases (Core.bindingType $ var "b")
          (Core.binding (var "bname") (bindUnboundTypeVariables @@ var "cx" @@ var "bterm") nothing)
          ("ts" ~>
            "bvars" <~ Sets.fromList (Core.typeSchemeVariables $ var "ts") $
            "unboundInType" <~ Rewriting.freeVariablesInType @@ (Core.typeSchemeType $ var "ts") $
            "unboundInTerm" <~ Rewriting.freeTypeVariablesInTerm @@ var "bterm" $
            "unbound" <~ Sets.toList (Sets.difference
              (Sets.union (var "unboundInType") (var "unboundInTerm"))
              (Sets.union (var "svars") (var "bvars"))) $
            "ts2" <~ Core.typeScheme
              (Lists.concat2
                (Core.typeSchemeVariables $ var "ts")
                (var "unbound"))
              (Core.typeSchemeType $ var "ts") $
            "bterm2" <~ Lists.foldl
              ("t" ~> "v" ~> Core.termTypeLambda
                (Core.typeLambda (var "v") (var "t")))
              (var "bterm")
              (var "unbound") $
            Core.binding (var "bname") (var "bterm2") (just $ var "ts2"))) $
      Core.termLet $ Core.let_
        (Lists.map (var "forBinding") (Core.letBindings $ var "l"))
        (bindUnboundTypeVariables @@ var "cx" @@ (Core.letBody $ var "l"))]) $
  Rewriting.rewriteTerm @@ var "rewrite" @@ var "term0"

buildTypeApplicationTerm :: TBinding ([Name] -> Term -> Term)
buildTypeApplicationTerm = define "buildTypeApplicationTerm" $
  doc "Fold a list of type variables over a term to build a type application term" $
  "tvars" ~> "body" ~> Lists.foldl
    ("t" ~> "v" ~> Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeVariable (var "v")))
    (var "body")
    (var "tvars")

emptyInferenceContext :: TBinding InferenceContext
emptyInferenceContext = define "emptyInferenceContext" $
  doc "An empty inference context" $
  Typing.inferenceContext
    (Phantoms.map M.empty)
    (Phantoms.map M.empty)
    (Phantoms.map M.empty)
    false

extendContext :: TBinding ([(Name, TypeScheme)] -> InferenceContext -> InferenceContext)
extendContext = define "extendContext" $
  doc "Add (term variable, type scheme) pairs to the typing environment" $
  "pairs" ~> "cx" ~>
  Typing.inferenceContextWithDataTypes (var "cx") $ Maps.union
    (Maps.fromList $ var "pairs")
    (Typing.inferenceContextDataTypes $ var "cx")

finalizeInferredTerm :: TBinding (InferenceContext -> Term -> Flow s Term)
finalizeInferredTerm = define "finalizeInferredTerm" $
  doc "Finalize an inferred term by checking for unbound type variables, then normalizing type variables" $
  "cx" ~> "term" ~>
  -- TODO: restore these (but make sure type checking tests pass)
  "term2" <~ bindUnboundTypeVariables @@ var "cx" @@ var "term" $
  exec (Checking.checkForUnboundTypeVariables @@ var "cx" @@ var "term2") $
--  "term2" <~ var "term" $
  produce $ Rewriting.normalizeTypeVariablesInTerm @@ var "term2"

forInferredTerm :: TBinding (InferenceContext -> Term -> String -> (InferenceResult -> a) -> Flow s a)
forInferredTerm = define "forInferredTerm" $
  doc "Infer a term's type and map over the result" $
  "cx" ~> "term" ~> "desc" ~> "f" ~>
  Flows.map (var "f") $ inferTypeOfTerm @@ var "cx" @@ var "term" @@ var "desc"

freeVariablesInContext :: TBinding (InferenceContext -> S.Set Name)
freeVariablesInContext = define "freeVariablesInContext" $
  doc "Get all free variables in an inference context" $
  "cx" ~>
    Lists.foldl (binaryFunction Sets.union) Sets.empty $
      Lists.map (Rewriting.freeVariablesInTypeSchemeSimple) $
        Maps.elems $ Typing.inferenceContextDataTypes $ var "cx"

freshVariableType :: TBinding (Flow s Type)
freshVariableType = define "freshVariableType" $
  doc "Generate a fresh type variable" $
  Flows.map (unaryFunction Core.typeVariable) (Schemas.freshName)

generalize :: TBinding (InferenceContext -> Type -> TypeScheme)
generalize = define "generalize" $
  doc "Generalize a type to a type scheme" $
  "cx" ~> "typ" ~>
  "vars" <~ Lists.nub (Lists.filter (isUnbound @@ var "cx") $
     Rewriting.freeVariablesInTypeOrdered @@ var "typ") $
  Core.typeScheme (var "vars") (var "typ")

inferGraphTypes :: TBinding (Graph -> Flow s Graph)
inferGraphTypes = define "inferGraphTypes" $
  doc "Infer types for all elements in a graph" $
  "g0" ~>
  "fromLetTerm" <~ ("l" ~>
    "bindings" <~ Core.letBindings (var "l") $
    "body" <~ Core.letBody (var "l") $
    "fromBinding" <~ ("b" ~> pair
      (Core.bindingName $ var "b")
      (var "b")) $
    Graph.graph
      (Maps.fromList $ Lists.map (var "fromBinding") (var "bindings"))
      (Maps.empty)
      (Maps.empty)
      (var "body")
      (Graph.graphPrimitives $ var "g0")
      (Graph.graphSchema $ var "g0")) $
  "toLetTerm" <~ ("g" ~>
    "toBinding" <~ ("el" ~> Core.binding
      (Core.bindingName $ var "el")
      (Core.bindingTerm $ var "el")
      nothing) $
    Core.termLet $ Core.let_
      (Lists.map (var "toBinding") $ Maps.elems $ Graph.graphElements $ var "g")
      (Graph.graphBody $ var "g")) $
  "forFinal" <~ ("finalized" ~> cases _Term (var "finalized")
    Nothing [
    _Term_let>>: "l" ~> Flows.pure $ var "fromLetTerm" @@ var "l",
    _Term_variable>>: constant $ Flows.fail $ (string "Expected inferred graph as let term")]) $
  trace (string "graph inference") $
  "cx" <<~ Schemas.graphToInferenceContext @@ var "g0" $
  "result" <<~ inferTypeOfTerm @@ var "cx" @@ (var "toLetTerm" @@ var "g0") @@ (string "graph term") $
  "term" <~ Typing.inferenceResultTerm (var "result") $
  "ts" <~ Typing.inferenceResultType (var "result") $
  "finalized" <<~ finalizeInferredTerm @@ var "cx" @@ var "term" $
  var "forFinal" @@ var "finalized"

-- Note: this operation is expensive, as it creates a new typing environment for each individual term
inferInGraphContext :: TBinding (Term -> Flow Graph InferenceResult)
inferInGraphContext = define "inferInGraphContext" $
  doc "Infer the type of a term in graph context" $
  "term" ~>
  "g" <<~ Monads.getState $
  "cx" <<~ Schemas.graphToInferenceContext @@ var "g" $
  inferTypeOfTerm @@ var "cx" @@ var "term" @@ (string "single term")

inferMany :: TBinding (InferenceContext -> [(Term, String)] -> Flow s ([Term], [Type], TypeSubst))
inferMany = define "inferMany" $
  doc "Infer types for multiple terms" $
  "cx" ~> "pairs" ~>
  "dflt" <~ (
    "e" <~ Pairs.first (Lists.head $ var "pairs") $
    "desc" <~ Pairs.second (Lists.head $ var "pairs") $
    "tl" <~ Lists.tail (var "pairs") $
    "result1" <<~ inferTypeOfTerm @@ var "cx" @@ var "e" @@ var "desc" $
    "e1" <~ Typing.inferenceResultTerm (var "result1") $
    "t1" <~ Typing.inferenceResultType (var "result1") $
    "s1" <~ Typing.inferenceResultSubst (var "result1") $
    "result2" <<~ inferMany @@ (Substitution.substInContext @@ var "s1" @@ var "cx") @@ var "tl" $
    "e2" <~ Pairs.first (var "result2") $
    "t2" <~ Pairs.first (Pairs.second $ var "result2") $
    "s2" <~ Pairs.second (Pairs.second $ var "result2") $
    produce $ pair
      (Lists.cons (Substitution.substTypesInTerm @@ var "s2" @@ var "e1") (var "e2"))
      (pair
        (Lists.cons (Substitution.substInType @@ var "s2" @@ var "t1") (var "t2"))
        (Substitution.composeTypeSubst @@ var "s1" @@ var "s2"))) $
  Logic.ifElse (Lists.null $ var "pairs")
    (Flows.pure $ pair (list ([] :: [TTerm Term])) $ pair (list ([] :: [TTerm Type])) (Substitution.idTypeSubst))
    (var "dflt")

inferTypeOf :: TBinding (InferenceContext -> Term -> Flow s (Term, TypeScheme))
inferTypeOf = define "inferTypeOf" $
  doc "Infer the type of a term and return a type scheme" $
  "cx" ~> "term" ~>
  -- Top-level let term which allows us to easily extract an inferred type scheme
  "letTerm" <~ Core.termLet (Core.let_
    (list [Core.binding (Core.name $ (string "ignoredVariableName")) (var "term") nothing])
    (MetaTerms.string "ignoredBody")) $
  "forBindings" <~ ("bindings" ~>
    "binding" <~ Lists.head (var "bindings") $
    "term1" <~ Core.bindingTerm (var "binding") $
    "mts" <~ Core.bindingType (var "binding") $
    Maybes.maybe
      (Flows.fail $ (string "Expected a type scheme"))
      ("ts" ~> Flows.pure $ pair (var "term1") (var "ts"))
      (var "mts")) $
  "unifyAndSubst" <~ ("result" ~>
    "subst" <~ Typing.inferenceResultSubst (var "result") $
    "finalized" <<~ finalizeInferredTerm @@ var "cx" @@ Typing.inferenceResultTerm (var "result") $
    "letResult" <<~ Lexical.withEmptyGraph @@ (ExtractCore.let_ @@ var "finalized") $
    "bindings" <~ Core.letBindings (var "letResult") $
    Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "bindings"))
      (var "forBindings" @@ var "bindings")
      (Flows.fail $ Strings.cat $ list [
        (string "Expected a single binding with a type scheme, but got: "),
        Literals.showInt32 $ Lists.length $ var "bindings",
        (string " bindings")])) $
  "result" <<~ inferTypeOfTerm @@ var "cx" @@ var "letTerm" @@ (string "infer type of term") $
  var "unifyAndSubst" @@ var "result"

inferTypeOfAnnotatedTerm :: TBinding (InferenceContext -> AnnotatedTerm -> Flow s InferenceResult)
inferTypeOfAnnotatedTerm = define "inferTypeOfAnnotatedTerm" $
  doc "Infer the type of an annotated term" $
  "cx" ~> "at" ~>
  "term" <~ Core.annotatedTermBody (var "at") $
  "ann" <~ Core.annotatedTermAnnotation (var "at") $
  "result" <<~ inferTypeOfTerm @@ var "cx" @@ var "term" @@ (string "annotated term") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "itype" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  produce $ Typing.inferenceResult
    (Core.termAnnotated $ Core.annotatedTerm (var "iterm") (var "ann"))
    (var "itype")
    (var "isubst")

inferTypeOfApplication :: TBinding (InferenceContext -> Application -> Flow s InferenceResult)
inferTypeOfApplication = define "inferTypeOfApplication" $
  doc "Infer the type of a function application" $
  "cx" ~> "app" ~>
  "e0" <~ Core.applicationFunction (var "app") $
  "e1" <~ Core.applicationArgument (var "app") $
  "lhsResult" <<~ inferTypeOfTerm @@ var "cx" @@ var "e0" @@ (string "lhs") $
  "a" <~ Typing.inferenceResultTerm (var "lhsResult") $
  "t0" <~ Typing.inferenceResultType (var "lhsResult") $
  "s0" <~ Typing.inferenceResultSubst (var "lhsResult") $
  "rhsResult" <<~ inferTypeOfTerm
    @@ (Substitution.substInContext @@ var "s0" @@ var "cx")
    @@ var "e1"
    @@ (string "rhs") $
  "b" <~ Typing.inferenceResultTerm (var "rhsResult") $
  "t1" <~ Typing.inferenceResultType (var "rhsResult") $
  "s1" <~ Typing.inferenceResultSubst (var "rhsResult") $
  "v" <<~ Schemas.freshName $
  "s2" <<~ Unification.unifyTypes
    @@ (Typing.inferenceContextSchemaTypes $ var "cx")
    @@ (Substitution.substInType @@ var "s1" @@ var "t0")
    @@ (Core.typeFunction $ Core.functionType (var "t1") (Core.typeVariable $ var "v"))
    @@ (string "application lhs") $
  exec (Checking.checkTypeSubst @@ var "cx" @@ var "s2") $
  "rExpr" <~ Core.termApplication (Core.application
    (Substitution.substTypesInTerm @@ (Substitution.composeTypeSubst @@ var "s1" @@ var "s2") @@ var "a")
    (Substitution.substTypesInTerm @@ var "s2" @@ var "b")) $
  "rType" <~ Substitution.substInType @@ var "s2" @@ Core.typeVariable (var "v") $
  "rSubst" <~ Substitution.composeTypeSubstList @@ list [var "s0", var "s1", var "s2"] $
  produce $ Typing.inferenceResult (var "rExpr") (var "rType") (var "rSubst")

inferTypeOfCaseStatement :: TBinding (InferenceContext -> CaseStatement -> Flow s InferenceResult)
inferTypeOfCaseStatement = define "inferTypeOfCaseStatement" $
  doc "Infer the type of a case statement" $
  "cx" ~> "caseStmt" ~>
  "tname" <~ Core.caseStatementTypeName (var "caseStmt") $
  "dflt" <~ Core.caseStatementDefault (var "caseStmt") $
  "cases" <~ Core.caseStatementCases (var "caseStmt") $
  "fnames" <~ Lists.map (unaryFunction Core.fieldName) (var "cases") $
  "schemaType" <<~ Schemas.requireSchemaType @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ExtractCore.unionType @@ var "tname" @@ var "stype" $
  "dfltResult" <<~ Flows.mapMaybe ("t" ~> inferTypeOfTerm @@ var "cx" @@ var "t" @@
    (Strings.cat $ list [(string "case "), Core.unName $ var "tname", (string ".<default>")])) (var "dflt") $
  "caseResults" <<~ inferMany @@ var "cx" @@ Lists.map
    ("f" ~> pair (Core.fieldTerm $ var "f")
      (Strings.cat $ list [(string "case "), Core.unName $ var "tname", (string "."), Core.unName $ Core.fieldName $ var "f"]))
    (var "cases") $
  "iterms" <~ Pairs.first (var "caseResults") $
  "itypes" <~ Pairs.first (Pairs.second $ var "caseResults") $
  "isubst" <~ Pairs.second (Pairs.second $ var "caseResults") $
  "codv" <<~ Schemas.freshName $
  "cod" <~ Core.typeVariable (var "codv") $
  "caseMap" <~ Maps.fromList (Lists.map
    ("ft" ~> pair (Core.fieldTypeName $ var "ft") (Core.fieldTypeType $ var "ft"))
    (var "sfields")) $
  "dfltConstraints" <~ Monads.maybeToList @@ (Maybes.map
    ("r" ~> Typing.typeConstraint (var "cod") (Typing.inferenceResultType $ var "r") (string "match default"))
    (var "dfltResult")) $
  "caseConstraints" <~ Maybes.cat (Lists.zipWith
    ("fname" ~> "itype" ~> Maybes.map
      ("ftype" ~> Typing.typeConstraint
        (var "itype")
        (Core.typeFunction $ Core.functionType (var "ftype") (var "cod"))
        (string "case type"))
      (Maps.lookup (var "fname") (var "caseMap")))
    (var "fnames") (var "itypes")) $
  mapConstraints
    @@ var "cx"
    @@ ("subst" ~> yield
      @@ (buildTypeApplicationTerm @@ var "svars"
          @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationUnion $
            Core.caseStatement (var "tname") (Maybes.map (unaryFunction Typing.inferenceResultTerm) $ var "dfltResult") $
            Lists.zipWith ("n" ~> "t" ~> Core.field (var "n") (var "t")) (var "fnames") (var "iterms")))
      @@ (Core.typeFunction $ Core.functionType
          (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
          (var "cod"))
      @@ (Substitution.composeTypeSubstList
        @@ (Lists.concat $ list [
          Monads.maybeToList @@ (Maybes.map (unaryFunction Typing.inferenceResultSubst) (var "dfltResult")),
          list [var "isubst", var "subst"]])))
    @@ (Lists.concat $ list [var "dfltConstraints", var "caseConstraints"])

inferTypeOfCollection :: TBinding (InferenceContext -> (Type -> Type) -> ([Term] -> Term) -> String -> [Term] -> Flow s InferenceResult)
inferTypeOfCollection = define "inferTypeOfCollection" $
  doc "Infer the type of a collection" $
  "cx" ~> "typCons" ~> "trmCons" ~> "desc" ~> "els" ~>
  "var" <<~ Schemas.freshName $
  Logic.ifElse (Lists.null $ var "els")
    -- Empty collection: add type application for the fresh element type variable
    (Flows.pure $ yield
      @@ (buildTypeApplicationTerm
        @@ list [var "var"]
        @@ (var "trmCons" @@ list ([] :: [TTerm Term])))
      @@ (var "typCons" @@ (Core.typeVariable $ var "var"))
      @@ (Substitution.idTypeSubst))
    -- General case: non-empty collection
    ("results" <<~ inferMany @@ var "cx" @@
      (Lists.zip (var "els") $ Lists.map ("i" ~> Strings.cat $ list [(string "#"), Literals.showInt32 $ var "i"]) $
        Math.range (int32 1) (Math.add (Lists.length $ var "els") (int32 1))) $
    "terms" <~ Pairs.first (var "results") $
    "types" <~ Pairs.first (Pairs.second $ var "results") $
    "subst1" <~ Pairs.second (Pairs.second $ var "results") $
    "constraints" <~ Lists.map ("t" ~> Typing.typeConstraint (Core.typeVariable $ var "var") (var "t") (var "desc")) (var "types") $
    mapConstraints @@ var "cx" @@
      ("subst2" ~>
        "iterm" <~ var "trmCons" @@ var "terms" $
        "itype" <~ var "typCons" @@ (Core.typeVariable $ var "var") $
        "isubst" <~ Substitution.composeTypeSubst @@ var "subst1" @@ var "subst2" $
        yield @@ var "iterm" @@ var "itype" @@ var "isubst") @@
      var "constraints")

inferTypeOfEither :: TBinding (InferenceContext -> Prelude.Either Term Term -> Flow s InferenceResult)
inferTypeOfEither = define "inferTypeOfEither" $
  doc "Infer the type of an either value" $
  "cx" ~> "e" ~>
  Eithers.either_
    ("l" ~>
      "r1" <<~ (inferTypeOfTerm @@ var "cx" @@ var "l" @@ (string "either left value")) $
      "iterm" <~ Typing.inferenceResultTerm (var "r1") $
      "leftType" <~ Typing.inferenceResultType (var "r1") $
      "subst" <~ Typing.inferenceResultSubst (var "r1") $
      "rightType" <<~ freshVariableType $
      "eitherTerm" <~ (Core.termEither $ left $ var "iterm") $
      "termWithLeftType" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "eitherTerm") (var "leftType")) $
      "termWithBothTypes" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "termWithLeftType") (var "rightType")) $
      "eitherType" <~ (Core.typeEither $ Core.eitherType (var "leftType") (var "rightType")) $
      yieldChecked @@ var "termWithBothTypes" @@ var "eitherType" @@ var "subst")
    ("r" ~>
      "r1" <<~ (inferTypeOfTerm @@ var "cx" @@ var "r" @@ (string "either right value")) $
      "iterm" <~ Typing.inferenceResultTerm (var "r1") $
      "rightType" <~ Typing.inferenceResultType (var "r1") $
      "subst" <~ Typing.inferenceResultSubst (var "r1") $
      "leftType" <<~ freshVariableType $
      "eitherTerm" <~ (Core.termEither $ right $ var "iterm") $
      "termWithLeftType" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "eitherTerm") (var "leftType")) $
      "termWithBothTypes" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "termWithLeftType") (var "rightType")) $
      "eitherType" <~ (Core.typeEither $ Core.eitherType (var "leftType") (var "rightType")) $
      yieldChecked @@ var "termWithBothTypes" @@ var "eitherType" @@ var "subst")
    (var "e")

inferTypeOfElimination :: TBinding (InferenceContext -> Elimination -> Flow s InferenceResult)
inferTypeOfElimination = define "inferTypeOfElimination" $
  doc "Infer the type of an elimination" $
  "cx" ~> "elm" ~>
  cases _Elimination (var "elm") Nothing [
    _Elimination_record>>: "p" ~> inferTypeOfProjection @@ var "cx" @@ var "p",
    _Elimination_union>>: "c" ~> inferTypeOfCaseStatement @@ var "cx" @@ var "c",
    _Elimination_wrap>>: "tname" ~> inferTypeOfUnwrap @@ var "cx" @@ var "tname"]

inferTypeOfFunction :: TBinding (InferenceContext -> Function -> Flow s InferenceResult)
inferTypeOfFunction = define "inferTypeOfFunction" $
  doc "Infer the type of a function" $
  "cx" ~> "f" ~>
  cases _Function (var "f") Nothing [
    _Function_elimination>>: "elm" ~> inferTypeOfElimination @@ var "cx" @@ var "elm",
    _Function_lambda>>: "l" ~> inferTypeOfLambda @@ var "cx" @@ var "l",
    _Function_primitive>>: "name" ~> inferTypeOfPrimitive @@ var "cx" @@ var "name"]

inferTypeOfInjection :: TBinding (InferenceContext -> Injection -> Flow s InferenceResult)
inferTypeOfInjection = define "inferTypeOfInjection" $
  doc "Infer the type of a union injection" $
  "cx" ~> "injection" ~>
  "tname" <~ Core.injectionTypeName (var "injection") $
  "field" <~ Core.injectionField (var "injection") $
  "fname" <~ Core.fieldName (var "field") $
  "term" <~ Core.fieldTerm (var "field") $
  "result" <<~ inferTypeOfTerm @@ var "cx" @@ var "term" @@ (string "injected term") $
  "schemaType" <<~ Schemas.requireSchemaType @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "ityp" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "sfields" <<~ ExtractCore.unionType @@ var "tname" @@ var "stype" $
  "ftyp" <<~ Schemas.findFieldType @@ var "fname" @@ var "sfields" $
  mapConstraints @@ var "cx" @@
    ("subst" ~> yield
      @@ (buildTypeApplicationTerm @@ var "svars"
        @@ (Core.termUnion $ Core.injection (var "tname") $ Core.field (var "fname") (var "iterm")))
      @@ (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      @@ (Substitution.composeTypeSubst @@ var "isubst" @@ var "subst")) @@
    list [Typing.typeConstraint (var "ftyp") (var "ityp") (string "schema type of injected field")]

inferTypeOfLambda :: TBinding (InferenceContext -> Lambda -> Flow s InferenceResult)
inferTypeOfLambda = define "inferTypeOfLambda" $
  doc "Infer the type of a lambda function" $
  "cx" ~> "lambda" ~>
  "var" <~ Core.lambdaParameter (var "lambda") $
  "body" <~ Core.lambdaBody (var "lambda") $
  "vdom" <<~ Schemas.freshName $
  "dom" <~ Core.typeVariable (var "vdom") $
  "cx2" <~ (extendContext @@ list [pair (var "var") (Core.typeScheme (list ([] :: [TTerm Name])) (var "dom"))] @@ var "cx") $
  "result" <<~ inferTypeOfTerm @@ var "cx2" @@ var "body" @@ (string "lambda body") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "icod" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "rdom" <~ Substitution.substInType @@ var "isubst" @@ var "dom" $
  "rterm" <~ Core.termFunction (Core.functionLambda $ Core.lambda (var "var") (just $ var "rdom") (var "iterm")) $
  "rtype" <~ Core.typeFunction (Core.functionType (var "rdom") (var "icod")) $
  "vars" <~ (Sets.unions $ list [
    Rewriting.freeVariablesInType @@ var "rdom",
    Rewriting.freeVariablesInType @@ var "icod",
    freeVariablesInContext @@ (Substitution.substInContext @@ var "isubst" @@ var "cx2")]) $
  "cx3" <~ Substitution.substInContext @@ var "isubst" @@ var "cx" $
  produce $ Typing.inferenceResult (var "rterm") (var "rtype") (var "isubst")

-- | Normalize a let term before inferring its type.
inferTypeOfLet :: TBinding (InferenceContext -> Let -> Flow s InferenceResult)
inferTypeOfLet = define "inferTypeOfLet" $
  doc "Normalize a let term before inferring its type" $
  "cx" ~> "let0" ~>
  "bindings0" <~ Core.letBindings (var "let0") $
  "body0" <~ Core.letBody (var "let0") $
  "names" <~ Lists.map (unaryFunction Core.bindingName) (var "bindings0") $
  "nameSet" <~ Sets.fromList (var "names") $
  "toPair" <~ ("binding" ~>
    "name" <~ Core.bindingName (var "binding") $
    "term" <~ Core.bindingTerm (var "binding") $
    pair (var "name") $ Lists.filter ("n" ~> Sets.member (var "n") (var "nameSet")) $
      Sets.toList $ Rewriting.freeVariablesInTerm @@ var "term") $
  "adjList" <~ Lists.map (var "toPair") (var "bindings0") $
  "groups" <~ Sorting.topologicalSortComponents @@ var "adjList" $
  "bindingMap" <~ Maps.fromList (Lists.zip (var "names") (var "bindings0")) $
  "createLet" <~ ("e" ~> "group" ~> Core.termLet $ Core.let_
    (Maybes.cat $ Lists.map ("n" ~> Maps.lookup (var "n") (var "bindingMap")) (var "group"))
    (var "e")) $
  -- Note: this rewritten let term will yield success in all cases of dependencies among letrec bindings *except*
  --       in cases of polymorphic recursion. In those cases, type hints will be needed (#162).
  "rewrittenLet" <~ Lists.foldl (var "createLet") (var "body0") (Lists.reverse $ var "groups") $
  "restoreLet" <~ ("iterm" ~>
    "helper" <~ ("level" ~> "bins" ~> "term" ~>
      "nonzero" <~ ("term" ~> cases _Term (var "term") Nothing [
        _Term_let>>: "l" ~>
          "bs" <~ Core.letBindings (var "l") $
          "e" <~ Core.letBody (var "l") $
          var "helper" @@
            (Math.sub (var "level") (int32 1)) @@
            (Lists.concat $ list [var "bs", var "bins"]) @@
            (var "e")]) $
      Logic.ifElse (Equality.equal (var "level") (int32 0))
        (pair (var "bins") (var "term"))
        (var "nonzero" @@ var "term")) $
    "result" <~ var "helper" @@ (Lists.length $ var "groups") @@ list ([] :: [TTerm Binding]) @@ var "iterm" $
    "bindingList" <~ Pairs.first (var "result") $
    "e" <~ Pairs.second (var "result") $
    "bindingMap2" <~ Maps.fromList (Lists.map ("b" ~> pair (Core.bindingName $ var "b") (var "b")) (var "bindingList")) $
    Core.termLet $ Core.let_
      (Maybes.cat $ Lists.map ("n" ~> Maps.lookup (var "n") (var "bindingMap2")) (var "names"))
      (var "e")) $
  "rewriteResult" <~ ("result" ~>
    "iterm" <~ Typing.inferenceResultTerm (var "result") $
    "itype" <~ Typing.inferenceResultType (var "result") $
    "isubst" <~ Typing.inferenceResultSubst (var "result") $
    Typing.inferenceResult (var "restoreLet" @@ var "iterm") (var "itype") (var "isubst")) $
  "res" <~ (cases _Term (var "rewrittenLet")
     (Just $ inferTypeOfTerm @@ var "cx" @@ var "rewrittenLet" @@ (string "empty let term")) [
     _Term_let>>: "l" ~> inferTypeOfLetNormalized @@ var "cx" @@ var "l"]) $
  Flows.map (var "rewriteResult") (var "res")

inferTypeOfLetNormalized :: TBinding (InferenceContext -> Let -> Flow s InferenceResult)
inferTypeOfLetNormalized = define "inferTypeOfLetNormalized" $
  doc "Infer the type of a let (letrec) term which is already in a normal form" $
  "cx0" ~> "letTerm" ~>
  
  -- Extract the bindings and body from the let term
  "bins0" <~ Core.letBindings (var "letTerm") $
  "body0" <~ Core.letBody (var "letTerm") $
  "bnames" <~ Lists.map (unaryFunction Core.bindingName) (var "bins0") $
  
  -- Phase 1: Create fresh temporary type variables for each binding
  -- These act as placeholders during the initial inference pass
  "bvars" <<~ Schemas.freshNames @@ (Lists.length $ var "bins0") $
  "tbins0" <~ Lists.map (unaryFunction Core.typeVariable) (var "bvars") $
  
  -- Extend the context with temporary monomorphic type schemes
  -- This allows recursive references during inference
  "cx1" <~ (extendContext
    @@ (Lists.zip (var "bnames") $ Lists.map ("t" ~> Core.typeScheme (list ([] :: [TTerm Name])) (var "t")) (var "tbins0"))
    @@ (var "cx0")) $
  
  -- Phase 2: Infer the actual types of all binding terms
  -- This returns: (inferred terms, inferred types, substitution s1)
  "inferredResult" <<~ inferTypesOfTemporaryBindings @@ var "cx1" @@ var "bins0" $
  "bterms1" <~ Pairs.first (var "inferredResult") $  -- Terms with type applications embedded
  "tbins1" <~ Pairs.first (Pairs.second $ var "inferredResult") $  -- Actual inferred types
  "s1" <~ Pairs.second (Pairs.second $ var "inferredResult") $  -- Substitution from inference
  
  -- Phase 3: Unify temporary types with actual inferred types
  -- This ensures the temporary placeholders match what we actually inferred
  -- Returns substitution s2 that maps temporary variables to their actual types
  "s2" <<~ Unification.unifyTypeLists @@
    (Typing.inferenceContextSchemaTypes $ var "cx0") @@
    (Lists.map (Substitution.substInType @@ var "s1") (var "tbins0")) @@
    (var "tbins1") @@
    (string "temporary type bindings") $
  exec (Checking.checkTypeSubst @@ var "cx0" @@ var "s2") $

  -- Apply the composed substitution to the original context
  -- This context will be used for generalization
  "g2" <~ (Substitution.substInContext @@
    (Substitution.composeTypeSubst @@ var "s1" @@ var "s2") @@
    (var "cx0")) $

  -- Apply s2 to the terms as well as the types
  -- This ensures type applications in the terms use the same variables as the types
  "bterms1Subst" <~ Lists.map (Substitution.substTypesInTerm @@ var "s2") (var "bterms1") $

  -- Phase 4: Generalize the inferred types into type schemes
  "tsbins1" <~ (Lists.zip (var "bnames") $
    Lists.map ("t" ~> generalize @@ var "g2" @@
      (Substitution.substInType @@ var "s2" @@ var "t")) (var "tbins1")) $

  -- Phase 5: Infer the type of the let body
  -- The body can now reference the bindings with their generalized type schemes
  "bodyResult" <<~ inferTypeOfTerm @@
    (extendContext @@ var "tsbins1" @@ var "g2") @@
    (var "body0") @@
    (string "let body") $
  "body1" <~ Typing.inferenceResultTerm (var "bodyResult") $
  "tbody" <~ Typing.inferenceResultType (var "bodyResult") $
  "sbody" <~ Typing.inferenceResultSubst (var "bodyResult") $
  
  -- Phase 6: Create term substitution for polymorphic instantiation
  -- For each binding, create a mapping from its name to a term that applies
  -- all the type variables from its type scheme
  -- e.g., foo becomes foo!<t0><t1> if its scheme is ∀t0,t1. ...
  "st1" <~ (Typing.termSubst (Maps.fromList $
    Lists.map
      ("pair" ~>
        "name" <~ Pairs.first (var "pair") $
        "ts" <~ Pairs.second (var "pair") $
        pair
          (var "name") $
          (buildTypeApplicationTerm
            @@ (Core.typeSchemeVariables $ var "ts")
            @@ (Core.termVariable $ var "name")))
      (var "tsbins1"))) $

  -- Phase 7: Create the final bindings with type lambdas
  "createBinding" <~ ("bindingPair" ~>
    "nameTsPair" <~ Pairs.first (var "bindingPair") $
    "term" <~ Pairs.second (var "bindingPair") $
    "name" <~ Pairs.first (var "nameTsPair") $
    "ts" <~ Pairs.second (var "nameTsPair") $

    -- First, substitute polymorphic references in the term
    -- Then wrap in type lambdas for each variable in the type scheme.
    -- Note: this is the only place -- at the top level of bound terms -- where inference creates type lambda terms.
    "typeLambdaTerm" <~ Lists.foldl
      ("b" ~> "v" ~> Core.termTypeLambda $ Core.typeLambda (var "v") (var "b"))
      (Substitution.substituteInTerm @@ var "st1" @@ var "term")
      (Lists.reverse $ Core.typeSchemeVariables $ var "ts") $

    -- Apply remaining substitutions (senv and s2) to the wrapped term
    Core.binding (var "name")
      (Substitution.substTypesInTerm @@
        (Substitution.composeTypeSubst @@ var "sbody" @@ var "s2") @@
        (var "typeLambdaTerm"))
      (just $ Substitution.substInTypeScheme @@ var "sbody" @@ var "ts")) $
  
  "bins1" <~ (Lists.map (var "createBinding") $
    Lists.zip (var "tsbins1") (var "bterms1Subst")) $

  -- Return the final let term with properly typed bindings
  "ret" <~ (Typing.inferenceResult
    (Core.termLet $ Core.let_ (var "bins1") (var "body1"))
    (var "tbody")
    (Substitution.composeTypeSubstList @@ list [var "s1", var "s2", var "sbody"])) $
  produce $ var "ret"

inferTypeOfList :: TBinding (InferenceContext -> [Term] -> Flow s InferenceResult)
inferTypeOfList = define "inferTypeOfList" $
  doc "Infer the type of a list" $
  "cx" ~> inferTypeOfCollection
    @@ var "cx"
    @@ (unaryFunction Core.typeList)
    @@ (unaryFunction Core.termList)
    @@ (string "list element")

inferTypeOfLiteral :: TBinding (InferenceContext -> Literal -> Flow s InferenceResult)
inferTypeOfLiteral = define "inferTypeOfLiteral" $
  doc "Infer the type of a literal" $
  "_" ~> "lit" ~>
  produce $ Typing.inferenceResult
    (Core.termLiteral $ var "lit")
    (Core.typeLiteral $ Reflect.literalType @@ var "lit")
    (Substitution.idTypeSubst)

inferTypeOfMap :: TBinding (InferenceContext -> M.Map Term Term -> Flow s InferenceResult)
inferTypeOfMap = define "inferTypeOfMap" $
  doc "Infer the type of a map" $
  "cx" ~> "m" ~>
  "kvar" <<~ Schemas.freshName $
  "vvar" <<~ Schemas.freshName $
  Logic.ifElse (Maps.null $ var "m")
    -- Empty map: add type applications for both key and value type variables
    (Flows.pure $ yield
      @@ (buildTypeApplicationTerm
        @@ list [var "kvar", var "vvar"]
        @@ (Core.termMap Maps.empty))
      @@ (Core.typeMap $ Core.mapType (Core.typeVariable $ var "kvar") (Core.typeVariable $ var "vvar"))
      @@ Substitution.idTypeSubst)
    -- Non-empty map: infer and unify key and value types
    ("kresults" <<~ inferMany @@ var "cx" @@
      (Lists.map ("k" ~> pair (var "k") (string "map key")) $ Maps.keys $ var "m") $
    "kterms" <~ Pairs.first (var "kresults") $
    "ktypes" <~ Pairs.first (Pairs.second $ var "kresults") $
    "ksubst" <~ Pairs.second (Pairs.second $ var "kresults") $
    "vresults" <<~ inferMany
      @@ var "cx"
      @@ (Lists.map ("v" ~> pair (var "v") (string "map value")) $ Maps.elems $ var "m") $
    "vterms" <~ Pairs.first (var "vresults") $
    "vtypes" <~ Pairs.first (Pairs.second $ var "vresults") $
    "vsubst" <~ Pairs.second (Pairs.second $ var "vresults") $
    "kcons" <~ Lists.map ("t" ~> Typing.typeConstraint (Core.typeVariable $ var "kvar") (var "t") (string "map key")) (var "ktypes") $
    "vcons" <~ Lists.map ("t" ~> Typing.typeConstraint (Core.typeVariable $ var "vvar") (var "t") (string "map value")) (var "vtypes") $
    mapConstraints @@ var "cx" @@
      ("subst" ~> yield
        @@ (Core.termMap $ Maps.fromList $ Lists.zip (var "kterms") (var "vterms"))
        @@ (Core.typeMap $ Core.mapType (Core.typeVariable $ var "kvar") (Core.typeVariable $ var "vvar"))
        @@ (Substitution.composeTypeSubstList @@ list [var "ksubst", var "vsubst", var "subst"])) @@
      (Lists.concat $ list [var "kcons", var "vcons"]))

inferTypeOfOptional :: TBinding (InferenceContext -> Maybe Term -> Flow s InferenceResult)
inferTypeOfOptional = define "inferTypeOfOptional" $
  doc "Infer the type of an optional" $
  "cx" ~> "m" ~>
  "trmCons" <~ ("terms" ~> Logic.ifElse (Lists.null $ var "terms")
    (Core.termMaybe nothing)
    (Core.termMaybe $ just $ Lists.head $ var "terms")) $
  inferTypeOfCollection
    @@ var "cx"
    @@ (unaryFunction Core.typeMaybe)
    @@ var "trmCons"
    @@ (string "optional element")
    @@ (Maybes.maybe (list ([] :: [TTerm Term])) (unaryFunction Lists.singleton) $ var "m")

inferTypeOfPair :: TBinding (InferenceContext -> (Term, Term) -> Flow s InferenceResult)
inferTypeOfPair = define "inferTypeOfPair" $
  doc "Infer the type of a pair" $
  "cx" ~> "p" ~>
  Flows.map
    ( "results" ~>
      "iterms" <~ Pairs.first (var "results") $
      "itypes" <~ Pairs.first (Pairs.second $ var "results") $
      "isubst" <~ Pairs.second (Pairs.second $ var "results") $
      "ifst" <~ Lists.head (var "iterms") $
      "isnd" <~ Lists.head (Lists.tail $ var "iterms") $
      "tyFst" <~ Lists.head (var "itypes") $
      "tySnd" <~ Lists.head (Lists.tail $ var "itypes") $
      "pairTerm" <~ (Core.termPair $ pair (var "ifst") (var "isnd")) $
      "termWithTypes" <~ (Core.termTypeApplication $ Core.typeApplicationTerm
        (Core.termTypeApplication $ Core.typeApplicationTerm (var "pairTerm") (var "tyFst"))
        (var "tySnd")) $
      yield
        @@ var "termWithTypes"
        @@ (Core.typePair $ Core.pairType (var "tyFst") (var "tySnd"))
        @@ var "isubst")
    (inferMany @@ var "cx" @@ list [
      pair (Pairs.first $ var "p") (string "pair first element"),
      pair (Pairs.second $ var "p") (string "pair second element")])






--inferTypeOfProduct :: TBinding (InferenceContext -> [Term] -> Flow s InferenceResult)
--inferTypeOfProduct = define "inferTypeOfProduct" $
--  doc "Infer the type of a product (tuple)" $
--  "cx" ~> "els" ~>
--  Flows.map
--    ( "results" ~>
--      "iterms" <~ Pairs.first (var "results") $
--      "itypes" <~ Pairs.first (Pairs.second $ var "results") $
--      "isubst" <~ Pairs.second (Pairs.second $ var "results") $
--      yield @@ (Core.termProduct $ var "iterms") @@ (Core.typeProduct $ var "itypes") @@ var "isubst")
--    (inferMany @@ var "cx" @@ (Lists.map ("e" ~> pair (var "e") (string "tuple element")) $ var "els"))










inferTypeOfPrimitive :: TBinding (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfPrimitive = define "inferTypeOfPrimitive" $
  doc "Infer the type of a primitive function" $
  "cx" ~> "name" ~>
  Maybes.maybe
    (Flows.fail $ Strings.cat2 (string "No such primitive: ") (Core.unName $ var "name"))
    ("scheme" ~>
      "ts" <<~ Schemas.instantiateTypeScheme @@ var "scheme" $
      yieldChecked
        @@ (buildTypeApplicationTerm
          @@ Core.typeSchemeVariables (var "ts")
          @@ (Core.termFunction $ Core.functionPrimitive $ var "name"))
        @@ Core.typeSchemeType (var "ts")
        @@ Substitution.idTypeSubst)
    (Maps.lookup (var "name") (Typing.inferenceContextPrimitiveTypes $ var "cx"))

inferTypeOfProjection :: TBinding (InferenceContext -> Projection -> Flow s InferenceResult)
inferTypeOfProjection = define "inferTypeOfProjection" $
  doc "Infer the type of a record projection" $
  "cx" ~> "proj" ~>
  "tname" <~ Core.projectionTypeName (var "proj") $
  "fname" <~ Core.projectionField (var "proj") $
  "schemaType" <<~ Schemas.requireSchemaType @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ExtractCore.recordType @@ var "tname" @@ var "stype" $
  "ftyp" <<~ Schemas.findFieldType @@ var "fname" @@ var "sfields" $
  Flows.pure $ yield
    @@ (buildTypeApplicationTerm @@ var "svars"
      @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $ Core.projection (var "tname") (var "fname")))
    @@ (Core.typeFunction $ Core.functionType
      (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      (var "ftyp"))
    @@ Substitution.idTypeSubst

inferTypeOfRecord :: TBinding (InferenceContext -> Record -> Flow s InferenceResult)
inferTypeOfRecord = define "inferTypeOfRecord" $
  doc "Infer the type of a record" $
  "cx" ~> "record" ~>
  "tname" <~ Core.recordTypeName (var "record") $
  "fields" <~ Core.recordFields (var "record") $
  "fnames" <~ Lists.map (unaryFunction Core.fieldName) (var "fields") $
  "schemaType" <<~ Schemas.requireSchemaType @@ var "cx" @@ var "tname" $
  "results" <<~ inferMany @@ var "cx" @@ Lists.map
    ("f" ~> pair
      (Core.fieldTerm $ var "f")
      (Strings.cat2 (string "field ") (Core.unName $ Core.fieldName $ var "f")))
    (var "fields") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterms" <~ Pairs.first (var "results") $
  "itypes" <~ Pairs.first (Pairs.second $ var "results") $
  "isubst" <~ Pairs.second (Pairs.second $ var "results") $
  "ityp" <~ Core.typeRecord (Core.rowType (var "tname") $
      Lists.zipWith ("n" ~> "t" ~> Core.fieldType (var "n") (var "t")) (var "fnames") (var "itypes")) $
  mapConstraints @@ var "cx" @@
    ("subst" ~> yield
      @@ (buildTypeApplicationTerm @@ var "svars" @@
        (Core.termRecord $ Core.record (var "tname") $ Lists.zipWith
          ("n" ~> "t" ~> Core.field (var "n") (var "t"))
          (var "fnames")
          (var "iterms")))
      @@ (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      @@ (Substitution.composeTypeSubst @@ var "isubst" @@ var "subst")) @@
    list [Typing.typeConstraint (var "stype") (var "ityp") (string "schema type of record")]

inferTypeOfSet :: TBinding (InferenceContext -> S.Set Term -> Flow s InferenceResult)
inferTypeOfSet = define "inferTypeOfSet" $
  doc "Infer the type of a set" $
  "cx" ~>
  "s" ~>
  inferTypeOfCollection
    @@ var "cx"
    @@ (unaryFunction Core.typeSet)
    @@ ("terms" ~> Core.termSet $ Sets.fromList $ var "terms")
    @@ (string "set element")
    @@ (Sets.toList $ var "s")

inferTypeOfTerm :: TBinding (InferenceContext -> Term -> String -> Flow s InferenceResult)
inferTypeOfTerm = define "inferTypeOfTerm" $
  doc "Infer the type of a given term" $
  "cx" ~> "term" ~> "desc" ~>
  "matchTerm" <~ (cases _Term (var "term") Nothing [
    _Term_annotated>>: "a" ~> inferTypeOfAnnotatedTerm @@ var "cx" @@ var "a",
    _Term_application>>: "a" ~> inferTypeOfApplication @@ var "cx" @@ var "a",
    _Term_either>>: "e" ~> inferTypeOfEither @@ var "cx" @@ var "e",
    _Term_function>>: "f" ~> inferTypeOfFunction @@ var "cx" @@ var "f",
    _Term_let>>: "l" ~> inferTypeOfLet @@ var "cx" @@ var "l",
    _Term_list>>: "els" ~> inferTypeOfList @@ var "cx" @@ var "els",
    _Term_literal>>: "l" ~> inferTypeOfLiteral @@ var "cx" @@ var "l",
    _Term_map>>: "m" ~> inferTypeOfMap @@ var "cx" @@ var "m",
    _Term_maybe>>: "m" ~> inferTypeOfOptional @@ var "cx" @@ var "m",
    _Term_pair>>: "p" ~> inferTypeOfPair @@ var "cx" @@ var "p",
    _Term_record>>: "r" ~> inferTypeOfRecord @@ var "cx" @@ var "r",
    _Term_set>>: "s" ~> inferTypeOfSet @@ var "cx" @@ var "s",
    _Term_typeApplication>>: "tt" ~> inferTypeOfTypeApplication @@ var "cx" @@ var "tt",
    _Term_typeLambda>>: "ta" ~> inferTypeOfTypeLambda @@ var "cx" @@ var "ta",
    _Term_union>>: "i" ~> inferTypeOfInjection @@ var "cx" @@ var "i",
    _Term_unit>>: constant $ produce $ inferTypeOfUnit,
    _Term_variable>>: "name" ~> inferTypeOfVariable @@ var "cx" @@ var "name",
    _Term_wrap>>: "w" ~> inferTypeOfWrappedTerm @@ var "cx" @@ var "w"]) $
  trace (var "desc") (var "matchTerm")

inferTypeOfTypeLambda :: TBinding (InferenceContext -> TypeLambda -> Flow s InferenceResult)
inferTypeOfTypeLambda = define "inferTypeOfTypeLambda" $
  doc "Infer the type of a type abstraction; just pass through to the lambda body." $
  "cx" ~> "ta" ~>
  inferTypeOfTerm @@ var "cx" @@ (Core.typeLambdaBody $ var "ta") @@ (string "type abstraction")

inferTypeOfTypeApplication :: TBinding (InferenceContext -> TypeApplicationTerm -> Flow s InferenceResult)
inferTypeOfTypeApplication = define "inferTypeOfTypeApplication" $
  doc "Infer the type of a type application; just pass through to the inner term." $
  "cx" ~> "tt" ~>
  inferTypeOfTerm @@ var "cx" @@ (Core.typeApplicationTermBody $ var "tt") @@ (string "type application term")

inferTypeOfUnit :: TBinding InferenceResult
inferTypeOfUnit = define "inferTypeOfUnit" $
  doc "The trivial inference rule for the unit term" $
  Typing.inferenceResult
    (Core.termUnit)
    (Core.typeUnit)
    (Substitution.idTypeSubst)

inferTypeOfUnwrap :: TBinding (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfUnwrap = define "inferTypeOfUnwrap" $
  doc "Infer the type of an unwrap operation" $
  "cx" ~> "tname" ~>
  "schemaType" <<~ Schemas.requireSchemaType @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "wtyp" <<~ ExtractCore.wrappedType @@ var "tname" @@ var "stype" $
  Flows.pure $ yield
    @@ (buildTypeApplicationTerm @@ var "svars"
      @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationWrap $ var "tname"))
    @@ (Core.typeFunction $ Core.functionType
      (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      (var "wtyp"))
    @@ Substitution.idTypeSubst

inferTypeOfVariable :: TBinding (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfVariable = define "inferTypeOfVariable" $
  doc "Infer the type of a variable" $
  "cx" ~> "name" ~>
  Maybes.maybe
    (Flows.fail $ Strings.cat2 (string "Variable not bound to type: ") (Core.unName $ var "name"))
    ("scheme" ~>
      "ts" <<~ Schemas.instantiateTypeScheme @@ var "scheme" $
      produce $ Typing.inferenceResult
        (buildTypeApplicationTerm
          @@ Core.typeSchemeVariables (var "ts")
          @@ Core.termVariable (var "name"))
        (Core.typeSchemeType $ var "ts")
        (Substitution.idTypeSubst))
    (Maps.lookup (var "name") (Typing.inferenceContextDataTypes $ var "cx"))

inferTypeOfWrappedTerm :: TBinding (InferenceContext -> WrappedTerm -> Flow s InferenceResult)
inferTypeOfWrappedTerm = define "inferTypeOfWrappedTerm" $
  doc "Infer the type of a wrapped term" $
  "cx" ~> "wt" ~>
  "tname" <~ Core.wrappedTermTypeName (var "wt") $
  "term" <~ Core.wrappedTermBody (var "wt") $
  "schemaType" <<~ Schemas.requireSchemaType @@ var "cx" @@ var "tname" $
  "result" <<~ inferTypeOfTerm @@ var "cx" @@ var "term" @@ (string "wrapped term") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "itype" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "ityp" <~ Core.typeWrap (Core.wrappedType (var "tname") (var "itype")) $
  mapConstraints @@ var "cx"
    @@ ("subst" ~> yield
      @@ (buildTypeApplicationTerm @@ var "svars" @@ (Core.termWrap $ Core.wrappedTerm (var "tname") (var "iterm")))
      @@ (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      @@ (Substitution.composeTypeSubst @@ var "isubst" @@ var "subst"))
    @@ list [Typing.typeConstraint (var "stype") (var "ityp") (string "schema type of wrapper")]

inferTypesOfTemporaryBindings :: TBinding (InferenceContext -> [Binding] -> Flow s ([Term], ([Type], TypeSubst)))
inferTypesOfTemporaryBindings = define "inferTypesOfTemporaryBindings" $
  doc "Infer types for temporary let bindings" $
  "cx" ~> "bins" ~>
  "dflt" <~ (
    "binding" <~ Lists.head (var "bins") $
    "k" <~ Core.bindingName (var "binding") $
    "v" <~ Core.bindingTerm (var "binding") $
    "tl" <~ Lists.tail (var "bins") $
    "result1" <<~ inferTypeOfTerm @@ var "cx" @@ var "v" @@
      (Strings.cat $ list [
        (string "temporary let binding '"),
        Core.unName $ var "k",
        (string "'")]) $
    "j" <~ Typing.inferenceResultTerm (var "result1") $
    "u_prime" <~ Typing.inferenceResultType (var "result1") $
    "u" <~ Typing.inferenceResultSubst (var "result1") $
    "result2" <<~ inferTypesOfTemporaryBindings @@
      (Substitution.substInContext @@ var "u" @@ var "cx") @@
      var "tl" $
    "h" <~ Pairs.first (var "result2") $
    "r_prime" <~ Pairs.first (Pairs.second $ var "result2") $
    "r" <~ Pairs.second (Pairs.second $ var "result2") $
    Flows.pure $ pair
      (Lists.cons (Substitution.substTypesInTerm @@ var "r" @@ var "j") (var "h"))
      (pair
        (Lists.cons (Substitution.substInType @@ var "r" @@ var "u_prime") (var "r_prime"))
        (Substitution.composeTypeSubst @@ var "u" @@ var "r"))) $
  Logic.ifElse (Lists.null $ var "bins")
    (Flows.pure $ pair (list ([] :: [TTerm Term])) (pair (list ([] :: [TTerm Type])) (Substitution.idTypeSubst)))
    (var "dflt")

initialTypeContext :: TBinding (Graph -> Flow s TypeContext)
initialTypeContext = define "initialTypeContext" $
  doc "Create an initial type context from a graph" $
  "g" ~>
  "toPair" <~ ("pair" ~>
    "name" <~ Pairs.first (var "pair") $
    "el"  <~ Pairs.second (var "pair") $
    optCases (Core.bindingType $ var "el")
      (Flows.fail $ (string "untyped element: ") ++ Core.unName (var "name"))
      ("ts" ~> produce $ pair (var "name") (Schemas.typeSchemeToFType @@ var "ts"))) $
  "ix" <<~ Schemas.graphToInferenceContext @@ var "g" $
  "types" <<~ Flows.map
    (unaryFunction Maps.fromList)
    (Flows.mapList (var "toPair") (Maps.toList $ Graph.graphElements $ var "g")) $
  produce $ Typing.typeContext (var "types") Maps.empty Sets.empty Sets.empty (var "ix")

isUnbound :: TBinding (InferenceContext -> Name -> Bool)
isUnbound = define "isUnbound" $
  doc "Check if a variable is unbound in context" $
  "cx" ~> "v" ~>
  Logic.and
    (Logic.not $ Sets.member (var "v") $ freeVariablesInContext @@ var "cx")
    (Logic.not $ Maps.member (var "v") $ Typing.inferenceContextSchemaTypes $ var "cx")

mapConstraints :: TBinding (InferenceContext -> (TypeSubst -> a) -> [TypeConstraint] -> Flow s a)
mapConstraints = define "mapConstraints" $
  doc "Map over type constraints after unification" $
  "cx" ~> "f" ~> "constraints" ~>
  "s" <<~ Unification.unifyTypeConstraints @@ (Typing.inferenceContextSchemaTypes $ var "cx") @@ var "constraints" $
  exec (Checking.checkTypeSubst @@ var "cx" @@ var "s") $
  produce (var "f" @@ var "s")

showInferenceResult :: TBinding (InferenceResult -> String)
showInferenceResult = define "showInferenceResult" $
  doc "Show an inference result for debugging" $
  "result" ~>
  "term" <~ Typing.inferenceResultTerm (var "result") $
  "typ" <~ Typing.inferenceResultType (var "result") $
  "subst" <~ Typing.inferenceResultSubst (var "result") $
  Strings.cat $ list [
    (string "{term="),
    ShowCore.term @@ var "term",
    (string ", type="),
    ShowCore.type_ @@ var "typ",
    (string ", subst="),
    ShowTyping.typeSubst @@ var "subst",
    (string "}")]

yield :: TBinding (Term -> Type -> TypeSubst -> InferenceResult)
yield = define "yield" $
  doc "Create an inference result" $
  "term" ~> "typ" ~> "subst" ~>
  Typing.inferenceResult
    (Substitution.substTypesInTerm @@ var "subst" @@ var "term")
    (Substitution.substInType @@ var "subst" @@ var "typ")
    (var "subst")

-- TODO: pass context and variables, and actually check types
yieldChecked :: TBinding (Term -> Type -> TypeSubst -> Flow s InferenceResult)
yieldChecked = define "yieldChecked" $
  doc "Create a checked inference result" $
  "term" ~> "typ" ~> "subst" ~>
  "iterm" <~ Substitution.substTypesInTerm @@ var "subst" @@ var "term" $
  "itype" <~ Substitution.substInType @@ var "subst" @@ var "typ" $
  produce $ Typing.inferenceResult (var "iterm") (var "itype") (var "subst")

yieldDebug :: TBinding (InferenceContext -> String -> Term -> Type -> TypeSubst -> Flow s InferenceResult)
yieldDebug = define "yieldDebug" $
  doc "Create an inference result with debug output" $
  "cx" ~> "debugId" ~> "term" ~> "typ" ~> "subst" ~>
  "rterm" <~ Substitution.substTypesInTerm @@ var "subst" @@ var "term" $
  "rtyp" <~ Substitution.substInType @@ var "subst" @@ var "typ" $
  "result" <<~ Annotations.debugIf @@ var "debugId" @@
    (Strings.cat $ list [
      (string "\n\tterm: "),  ShowCore.term @@ var "term",
      (string "\n\ttyp: "),   ShowCore.type_ @@ var "typ",
      (string "\n\tsubst: "), ShowTyping.typeSubst @@ var "subst",
      (string "\n\trterm: "), ShowCore.term @@ var "rterm",
      (string "\n\trtyp: "),  ShowCore.type_ @@ var "rtyp"]) $
  produce $ Typing.inferenceResult (var "rterm") (var "rtyp") (var "subst")
