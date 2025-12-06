{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Inference where

-- Standard imports for kernel terms modules
import Hydra.Kernel
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


module_ :: Module
module_ = Module (Namespace "hydra.inference") elements
    [Annotations.module_, Checking.module_, ExtractCore.module_, Lexical.module_, Monads.module_, Reflect.module_,
      Rewriting.module_, Schemas.module_, ShowCore.module_, ShowTyping.module_, Sorting.module_, Substitution.module_,
      Unification.module_]
    kernelTypesModules $
    Just "Type inference following Algorithm W, extended for nominal terms and types"
  where
    elements = [
      el bindConstraintsDef,
      el bindUnboundTypeVariablesDef,
      el buildTypeApplicationTermDef,
      el emptyInferenceContextDef,
      el extendContextDef,
      el finalizeInferredTermDef,
      el forInferredTermDef,
      el freeVariablesInContextDef,
      el freshVariableTypeDef,
      el generalizeDef,
      el inferGraphTypesDef,
      el inferInGraphContextDef,
      el inferManyDef,
      el inferTypeOfAnnotatedTermDef,
      el inferTypeOfApplicationDef,
      el inferTypeOfCaseStatementDef,
      el inferTypeOfCollectionDef,
      el inferTypeOfDef,
      el inferTypeOfEitherDef,
      el inferTypeOfEliminationDef,
      el inferTypeOfFunctionDef,
      el inferTypeOfInjectionDef,
      el inferTypeOfLambdaDef,
      el inferTypeOfLetNormalizedDef,
      el inferTypeOfLetDef,
      el inferTypeOfListDef,
      el inferTypeOfLiteralDef,
      el inferTypeOfMapDef,
      el inferTypeOfOptionalDef,
      el inferTypeOfPairDef,
      el inferTypeOfPrimitiveDef,
      el inferTypeOfProjectionDef,
      el inferTypeOfRecordDef,
      el inferTypeOfSetDef,
      el inferTypeOfTermDef,
      el inferTypeOfTypeLambdaDef,
      el inferTypeOfTypeApplicationDef,
      el inferTypeOfUnitDef,
      el inferTypeOfUnwrapDef,
      el inferTypeOfVariableDef,
      el inferTypeOfWrappedTermDef,
      el inferTypesOfTemporaryBindingsDef,
      el initialTypeContextDef,
      el isUnboundDef,
      el mapConstraintsDef,
      el showInferenceResultDef,
      el yieldDef,
      el yieldCheckedDef,
      el yieldDebugDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

--

bindConstraintsDef :: TBinding (InferenceContext -> (TypeSubst -> Flow s a) -> [TypeConstraint] -> Flow s a)
bindConstraintsDef = define "bindConstraints" $
  doc "Bind type constraints and continue with substitution" $
  "cx" ~> "f" ~> "constraints" ~>
  "s" <<~ ref Unification.unifyTypeConstraintsDef @@ Typing.inferenceContextSchemaTypes (var "cx") @@ var "constraints" $
  exec (ref Checking.checkTypeSubstDef @@ var "cx" @@ var "s") $
  var "f" @@ var "s"

bindUnboundTypeVariablesDef :: TBinding (InferenceContext -> Term -> Term)
bindUnboundTypeVariablesDef = define "bindUnboundTypeVariables" $
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
          (Core.binding (var "bname") (ref bindUnboundTypeVariablesDef @@ var "cx" @@ var "bterm") nothing)
          ("ts" ~>
            "bvars" <~ Sets.fromList (Core.typeSchemeVariables $ var "ts") $
            "unboundInType" <~ ref Rewriting.freeVariablesInTypeDef @@ (Core.typeSchemeType $ var "ts") $
            "unboundInTerm" <~ ref Rewriting.freeTypeVariablesInTermDef @@ var "bterm" $
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
        (ref bindUnboundTypeVariablesDef @@ var "cx" @@ (Core.letBody $ var "l"))]) $
  ref Rewriting.rewriteTermDef @@ var "rewrite" @@ var "term0"

buildTypeApplicationTermDef :: TBinding ([Name] -> Term -> Term)
buildTypeApplicationTermDef = define "buildTypeApplicationTerm" $
  doc "Fold a list of type variables over a term to build a type application term" $
  "tvars" ~> "body" ~> Lists.foldl
    ("t" ~> "v" ~> Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeVariable (var "v")))
    (var "body")
    (var "tvars")

emptyInferenceContextDef :: TBinding InferenceContext
emptyInferenceContextDef = define "emptyInferenceContext" $
  doc "An empty inference context" $
  Typing.inferenceContext
    (Phantoms.map M.empty)
    (Phantoms.map M.empty)
    (Phantoms.map M.empty)
    false

extendContextDef :: TBinding ([(Name, TypeScheme)] -> InferenceContext -> InferenceContext)
extendContextDef = define "extendContext" $
  doc "Add (term variable, type scheme) pairs to the typing environment" $
  "pairs" ~> "cx" ~>
  Typing.inferenceContextWithDataTypes (var "cx") $ Maps.union
    (Maps.fromList $ var "pairs")
    (Typing.inferenceContextDataTypes $ var "cx")

finalizeInferredTermDef :: TBinding (InferenceContext -> Term -> Flow s Term)
finalizeInferredTermDef = define "finalizeInferredTerm" $
  doc "Finalize an inferred term by checking for unbound type variables, then normalizing type variables" $
  "cx" ~> "term" ~>
  -- TODO: restore these (but make sure type checking tests pass)
  "term2" <~ ref bindUnboundTypeVariablesDef @@ var "cx" @@ var "term" $
  exec (ref Checking.checkForUnboundTypeVariablesDef @@ var "cx" @@ var "term2") $
--  "term2" <~ var "term" $
  produce $ ref Rewriting.normalizeTypeVariablesInTermDef @@ var "term2"

forInferredTermDef :: TBinding (InferenceContext -> Term -> String -> (InferenceResult -> a) -> Flow s a)
forInferredTermDef = define "forInferredTerm" $
  doc "Infer a term's type and map over the result" $
  "cx" ~> "term" ~> "desc" ~> "f" ~>
  Flows.map (var "f") $ ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ var "desc"

freeVariablesInContextDef :: TBinding (InferenceContext -> S.Set Name)
freeVariablesInContextDef = define "freeVariablesInContext" $
  doc "Get all free variables in an inference context" $
  "cx" ~>
    Lists.foldl (binaryFunction Sets.union) Sets.empty $
      Lists.map (ref Rewriting.freeVariablesInTypeSchemeSimpleDef) $
        Maps.elems $ Typing.inferenceContextDataTypes $ var "cx"

freshVariableTypeDef :: TBinding (Flow s Type)
freshVariableTypeDef = define "freshVariableType" $
  doc "Generate a fresh type variable" $
  Flows.map (unaryFunction Core.typeVariable) (ref Schemas.freshNameDef)

generalizeDef :: TBinding (InferenceContext -> Type -> TypeScheme)
generalizeDef = define "generalize" $
  doc "Generalize a type to a type scheme" $
  "cx" ~> "typ" ~>
  "vars" <~ Lists.nub (Lists.filter (ref isUnboundDef @@ var "cx") $
     ref Rewriting.freeVariablesInTypeOrderedDef @@ var "typ") $
  Core.typeScheme (var "vars") (var "typ")

inferGraphTypesDef :: TBinding (Graph -> Flow s Graph)
inferGraphTypesDef = define "inferGraphTypes" $
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
    _Term_variable>>: constant $ Flows.fail $ string "Expected inferred graph as let term"]) $
  trace "graph inference" $
  "cx" <<~ ref Schemas.graphToInferenceContextDef @@ var "g0" $
  "result" <<~ ref inferTypeOfTermDef @@ var "cx" @@ (var "toLetTerm" @@ var "g0") @@ string "graph term" $
  "term" <~ Typing.inferenceResultTerm (var "result") $
  "ts" <~ Typing.inferenceResultType (var "result") $
  "finalized" <<~ ref finalizeInferredTermDef @@ var "cx" @@ var "term" $
  var "forFinal" @@ var "finalized"

-- Note: this operation is expensive, as it creates a new typing environment for each individual term
inferInGraphContextDef :: TBinding (Term -> Flow Graph InferenceResult)
inferInGraphContextDef = define "inferInGraphContext" $
  doc "Infer the type of a term in graph context" $
  "term" ~>
  "g" <<~ ref Monads.getStateDef $
  "cx" <<~ ref Schemas.graphToInferenceContextDef @@ var "g" $
  ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "single term"

inferManyDef :: TBinding (InferenceContext -> [(Term, String)] -> Flow s ([Term], [Type], TypeSubst))
inferManyDef = define "inferMany" $
  doc "Infer types for multiple terms" $
  "cx" ~> "pairs" ~>
  "dflt" <~ (
    "e" <~ Pairs.first (Lists.head $ var "pairs") $
    "desc" <~ Pairs.second (Lists.head $ var "pairs") $
    "tl" <~ Lists.tail (var "pairs") $
    "result1" <<~ ref inferTypeOfTermDef @@ var "cx" @@ var "e" @@ var "desc" $
    "e1" <~ Typing.inferenceResultTerm (var "result1") $
    "t1" <~ Typing.inferenceResultType (var "result1") $
    "s1" <~ Typing.inferenceResultSubst (var "result1") $
    "result2" <<~ ref inferManyDef @@ (ref Substitution.substInContextDef @@ var "s1" @@ var "cx") @@ var "tl" $
    "e2" <~ Pairs.first (var "result2") $
    "t2" <~ Pairs.first (Pairs.second $ var "result2") $
    "s2" <~ Pairs.second (Pairs.second $ var "result2") $
    produce $ pair
      (Lists.cons (ref Substitution.substTypesInTermDef @@ var "s2" @@ var "e1") (var "e2"))
      (pair
        (Lists.cons (ref Substitution.substInTypeDef @@ var "s2" @@ var "t1") (var "t2"))
        (ref Substitution.composeTypeSubstDef @@ var "s1" @@ var "s2"))) $
  Logic.ifElse (Lists.null $ var "pairs")
    (Flows.pure $ pair (list []) $ pair (list []) (ref Substitution.idTypeSubstDef))
    (var "dflt")

inferTypeOfDef :: TBinding (InferenceContext -> Term -> Flow s (Term, TypeScheme))
inferTypeOfDef = define "inferTypeOf" $
  doc "Infer the type of a term and return a type scheme" $
  "cx" ~> "term" ~>
  -- Top-level let term which allows us to easily extract an inferred type scheme
  "letTerm" <~ Core.termLet (Core.let_
    (list [Core.binding (Core.name $ string "ignoredVariableName") (var "term") nothing])
    (MetaTerms.string "ignoredBody")) $
  "forBindings" <~ ("bindings" ~>
    "binding" <~ Lists.head (var "bindings") $
    "term1" <~ Core.bindingTerm (var "binding") $
    "mts" <~ Core.bindingType (var "binding") $
    Maybes.maybe
      (Flows.fail $ string "Expected a type scheme")
      ("ts" ~> Flows.pure $ pair (var "term1") (var "ts"))
      (var "mts")) $
  "unifyAndSubst" <~ ("result" ~>
    "subst" <~ Typing.inferenceResultSubst (var "result") $
    "finalized" <<~ ref finalizeInferredTermDef @@ var "cx" @@ Typing.inferenceResultTerm (var "result") $
    "letResult" <<~ ref Lexical.withEmptyGraphDef @@ (ref ExtractCore.letTermDef @@ var "finalized") $
    "bindings" <~ Core.letBindings (var "letResult") $
    Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "bindings"))
      (var "forBindings" @@ var "bindings")
      (Flows.fail $ Strings.cat $ list [
        string "Expected a single binding with a type scheme, but got: ",
        Literals.showInt32 $ Lists.length $ var "bindings",
        string " bindings"])) $
  "result" <<~ ref inferTypeOfTermDef @@ var "cx" @@ var "letTerm" @@ string "infer type of term" $
  var "unifyAndSubst" @@ var "result"

inferTypeOfAnnotatedTermDef :: TBinding (InferenceContext -> AnnotatedTerm -> Flow s InferenceResult)
inferTypeOfAnnotatedTermDef = define "inferTypeOfAnnotatedTerm" $
  doc "Infer the type of an annotated term" $
  "cx" ~> "at" ~>
  "term" <~ Core.annotatedTermBody (var "at") $
  "ann" <~ Core.annotatedTermAnnotation (var "at") $
  "result" <<~ ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "annotated term" $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "itype" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  produce $ Typing.inferenceResult
    (Core.termAnnotated $ Core.annotatedTerm (var "iterm") (var "ann"))
    (var "itype")
    (var "isubst")

inferTypeOfApplicationDef :: TBinding (InferenceContext -> Application -> Flow s InferenceResult)
inferTypeOfApplicationDef = define "inferTypeOfApplication" $
  doc "Infer the type of a function application" $
  "cx" ~> "app" ~>
  "e0" <~ Core.applicationFunction (var "app") $
  "e1" <~ Core.applicationArgument (var "app") $
  "lhsResult" <<~ ref inferTypeOfTermDef @@ var "cx" @@ var "e0" @@ string "lhs" $
  "a" <~ Typing.inferenceResultTerm (var "lhsResult") $
  "t0" <~ Typing.inferenceResultType (var "lhsResult") $
  "s0" <~ Typing.inferenceResultSubst (var "lhsResult") $
  "rhsResult" <<~ ref inferTypeOfTermDef
    @@ (ref Substitution.substInContextDef @@ var "s0" @@ var "cx")
    @@ var "e1"
    @@ string "rhs" $
  "b" <~ Typing.inferenceResultTerm (var "rhsResult") $
  "t1" <~ Typing.inferenceResultType (var "rhsResult") $
  "s1" <~ Typing.inferenceResultSubst (var "rhsResult") $
  "v" <<~ ref Schemas.freshNameDef $
  "s2" <<~ ref Unification.unifyTypesDef
    @@ (Typing.inferenceContextSchemaTypes $ var "cx")
    @@ (ref Substitution.substInTypeDef @@ var "s1" @@ var "t0")
    @@ (Core.typeFunction $ Core.functionType (var "t1") (Core.typeVariable $ var "v"))
    @@ string "application lhs" $
  exec (ref Checking.checkTypeSubstDef @@ var "cx" @@ var "s2") $
  "rExpr" <~ Core.termApplication (Core.application
    (ref Substitution.substTypesInTermDef @@ (ref Substitution.composeTypeSubstDef @@ var "s1" @@ var "s2") @@ var "a")
    (ref Substitution.substTypesInTermDef @@ var "s2" @@ var "b")) $
  "rType" <~ ref Substitution.substInTypeDef @@ var "s2" @@ Core.typeVariable (var "v") $
  "rSubst" <~ ref Substitution.composeTypeSubstListDef @@ list [var "s0", var "s1", var "s2"] $
  produce $ Typing.inferenceResult (var "rExpr") (var "rType") (var "rSubst")

inferTypeOfCaseStatementDef :: TBinding (InferenceContext -> CaseStatement -> Flow s InferenceResult)
inferTypeOfCaseStatementDef = define "inferTypeOfCaseStatement" $
  doc "Infer the type of a case statement" $
  "cx" ~> "caseStmt" ~>
  "tname" <~ Core.caseStatementTypeName (var "caseStmt") $
  "dflt" <~ Core.caseStatementDefault (var "caseStmt") $
  "cases" <~ Core.caseStatementCases (var "caseStmt") $
  "fnames" <~ Lists.map (unaryFunction Core.fieldName) (var "cases") $
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ref ExtractCore.unionTypeDef @@ var "tname" @@ var "stype" $
  "dfltResult" <<~ Flows.mapMaybe ("t" ~> ref inferTypeOfTermDef @@ var "cx" @@ var "t" @@
    (Strings.cat $ list [string "case ", Core.unName $ var "tname", string ".<default>"])) (var "dflt") $
  "caseResults" <<~ ref inferManyDef @@ var "cx" @@ Lists.map
    ("f" ~> pair (Core.fieldTerm $ var "f")
      (Strings.cat $ list [string "case ", Core.unName $ var "tname", string ".", Core.unName $ Core.fieldName $ var "f"]))
    (var "cases") $
  "iterms" <~ Pairs.first (var "caseResults") $
  "itypes" <~ Pairs.first (Pairs.second $ var "caseResults") $
  "isubst" <~ Pairs.second (Pairs.second $ var "caseResults") $
  "codv" <<~ ref Schemas.freshNameDef $
  "cod" <~ Core.typeVariable (var "codv") $
  "caseMap" <~ Maps.fromList (Lists.map
    ("ft" ~> pair (Core.fieldTypeName $ var "ft") (Core.fieldTypeType $ var "ft"))
    (var "sfields")) $
  "dfltConstraints" <~ ref Monads.maybeToListDef @@ (Maybes.map
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
  ref mapConstraintsDef
    @@ var "cx"
    @@ ("subst" ~> ref yieldDef
      @@ (ref buildTypeApplicationTermDef @@ var "svars"
          @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationUnion $
            Core.caseStatement (var "tname") (Maybes.map (unaryFunction Typing.inferenceResultTerm) $ var "dfltResult") $
            Lists.zipWith ("n" ~> "t" ~> Core.field (var "n") (var "t")) (var "fnames") (var "iterms")))
      @@ (Core.typeFunction $ Core.functionType
          (ref Schemas.nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
          (var "cod"))
      @@ (ref Substitution.composeTypeSubstListDef
        @@ (Lists.concat $ list [
          ref Monads.maybeToListDef @@ (Maybes.map (unaryFunction Typing.inferenceResultSubst) (var "dfltResult")),
          list [var "isubst", var "subst"]])))
    @@ (Lists.concat $ list [var "dfltConstraints", var "caseConstraints"])

inferTypeOfCollectionDef :: TBinding (InferenceContext -> (Type -> Type) -> ([Term] -> Term) -> String -> [Term] -> Flow s InferenceResult)
inferTypeOfCollectionDef = define "inferTypeOfCollection" $
  doc "Infer the type of a collection" $
  "cx" ~> "typCons" ~> "trmCons" ~> "desc" ~> "els" ~>
  "var" <<~ ref Schemas.freshNameDef $
  Logic.ifElse (Lists.null $ var "els")
    -- Empty collection: add type application for the fresh element type variable
    (Flows.pure $ ref yieldDef
      @@ (ref buildTypeApplicationTermDef
        @@ list [var "var"]
        @@ (var "trmCons" @@ list []))
      @@ (var "typCons" @@ (Core.typeVariable $ var "var"))
      @@ (ref Substitution.idTypeSubstDef))
    -- General case: non-empty collection
    ("results" <<~ ref inferManyDef @@ var "cx" @@
      (Lists.zip (var "els") $ Lists.map ("i" ~> Strings.cat $ list [string "#", Literals.showInt32 $ var "i"]) $
        Math.range (int32 1) (Math.add (Lists.length $ var "els") (int32 1))) $
    "terms" <~ Pairs.first (var "results") $
    "types" <~ Pairs.first (Pairs.second $ var "results") $
    "subst1" <~ Pairs.second (Pairs.second $ var "results") $
    "constraints" <~ Lists.map ("t" ~> Typing.typeConstraint (Core.typeVariable $ var "var") (var "t") (var "desc")) (var "types") $
    ref mapConstraintsDef @@ var "cx" @@
      ("subst2" ~>
        "iterm" <~ var "trmCons" @@ var "terms" $
        "itype" <~ var "typCons" @@ (Core.typeVariable $ var "var") $
        "isubst" <~ ref Substitution.composeTypeSubstDef @@ var "subst1" @@ var "subst2" $
        ref yieldDef @@ var "iterm" @@ var "itype" @@ var "isubst") @@
      var "constraints")

inferTypeOfEitherDef :: TBinding (InferenceContext -> Prelude.Either Term Term -> Flow s InferenceResult)
inferTypeOfEitherDef = define "inferTypeOfEither" $
  doc "Infer the type of an either value" $
  "cx" ~> "e" ~>
  Eithers.either_
    ("l" ~>
      "r1" <<~ (ref inferTypeOfTermDef @@ var "cx" @@ var "l" @@ string "either left value") $
      "iterm" <~ Typing.inferenceResultTerm (var "r1") $
      "leftType" <~ Typing.inferenceResultType (var "r1") $
      "subst" <~ Typing.inferenceResultSubst (var "r1") $
      "rightType" <<~ ref freshVariableTypeDef $
      "eitherTerm" <~ (Core.termEither $ left $ var "iterm") $
      "termWithLeftType" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "eitherTerm") (var "leftType")) $
      "termWithBothTypes" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "termWithLeftType") (var "rightType")) $
      "eitherType" <~ (Core.typeEither $ Core.eitherType (var "leftType") (var "rightType")) $
      ref yieldCheckedDef @@ var "termWithBothTypes" @@ var "eitherType" @@ var "subst")
    ("r" ~>
      "r1" <<~ (ref inferTypeOfTermDef @@ var "cx" @@ var "r" @@ string "either right value") $
      "iterm" <~ Typing.inferenceResultTerm (var "r1") $
      "rightType" <~ Typing.inferenceResultType (var "r1") $
      "subst" <~ Typing.inferenceResultSubst (var "r1") $
      "leftType" <<~ ref freshVariableTypeDef $
      "eitherTerm" <~ (Core.termEither $ right $ var "iterm") $
      "termWithLeftType" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "eitherTerm") (var "leftType")) $
      "termWithBothTypes" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "termWithLeftType") (var "rightType")) $
      "eitherType" <~ (Core.typeEither $ Core.eitherType (var "leftType") (var "rightType")) $
      ref yieldCheckedDef @@ var "termWithBothTypes" @@ var "eitherType" @@ var "subst")
    (var "e")

inferTypeOfEliminationDef :: TBinding (InferenceContext -> Elimination -> Flow s InferenceResult)
inferTypeOfEliminationDef = define "inferTypeOfElimination" $
  doc "Infer the type of an elimination" $
  "cx" ~> "elm" ~>
  cases _Elimination (var "elm") Nothing [
    _Elimination_record>>: "p" ~> ref inferTypeOfProjectionDef @@ var "cx" @@ var "p",
    _Elimination_union>>: "c" ~> ref inferTypeOfCaseStatementDef @@ var "cx" @@ var "c",
    _Elimination_wrap>>: "tname" ~> ref inferTypeOfUnwrapDef @@ var "cx" @@ var "tname"]

inferTypeOfFunctionDef :: TBinding (InferenceContext -> Function -> Flow s InferenceResult)
inferTypeOfFunctionDef = define "inferTypeOfFunction" $
  doc "Infer the type of a function" $
  "cx" ~> "f" ~>
  cases _Function (var "f") Nothing [
    _Function_elimination>>: "elm" ~> ref inferTypeOfEliminationDef @@ var "cx" @@ var "elm",
    _Function_lambda>>: "l" ~> ref inferTypeOfLambdaDef @@ var "cx" @@ var "l",
    _Function_primitive>>: "name" ~> ref inferTypeOfPrimitiveDef @@ var "cx" @@ var "name"]

inferTypeOfInjectionDef :: TBinding (InferenceContext -> Injection -> Flow s InferenceResult)
inferTypeOfInjectionDef = define "inferTypeOfInjection" $
  doc "Infer the type of a union injection" $
  "cx" ~> "injection" ~>
  "tname" <~ Core.injectionTypeName (var "injection") $
  "field" <~ Core.injectionField (var "injection") $
  "fname" <~ Core.fieldName (var "field") $
  "term" <~ Core.fieldTerm (var "field") $
  "result" <<~ ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "injected term" $
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "ityp" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "sfields" <<~ ref ExtractCore.unionTypeDef @@ var "tname" @@ var "stype" $
  "ftyp" <<~ ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields" $
  ref mapConstraintsDef @@ var "cx" @@
    ("subst" ~> ref yieldDef
      @@ (ref buildTypeApplicationTermDef @@ var "svars"
        @@ (Core.termUnion $ Core.injection (var "tname") $ Core.field (var "fname") (var "iterm")))
      @@ (ref Schemas.nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      @@ (ref Substitution.composeTypeSubstDef @@ var "isubst" @@ var "subst")) @@
    list [Typing.typeConstraint (var "ftyp") (var "ityp") (string "schema type of injected field")]

inferTypeOfLambdaDef :: TBinding (InferenceContext -> Lambda -> Flow s InferenceResult)
inferTypeOfLambdaDef = define "inferTypeOfLambda" $
  doc "Infer the type of a lambda function" $
  "cx" ~> "lambda" ~>
  "var" <~ Core.lambdaParameter (var "lambda") $
  "body" <~ Core.lambdaBody (var "lambda") $
  "vdom" <<~ ref Schemas.freshNameDef $
  "dom" <~ Core.typeVariable (var "vdom") $
  "cx2" <~ (ref extendContextDef @@ list [pair (var "var") (Core.typeScheme (list []) (var "dom"))] @@ var "cx") $
  "result" <<~ ref inferTypeOfTermDef @@ var "cx2" @@ var "body" @@ string "lambda body" $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "icod" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "rdom" <~ ref Substitution.substInTypeDef @@ var "isubst" @@ var "dom" $
  "rterm" <~ Core.termFunction (Core.functionLambda $ Core.lambda (var "var") (just $ var "rdom") (var "iterm")) $
  "rtype" <~ Core.typeFunction (Core.functionType (var "rdom") (var "icod")) $
  "vars" <~ (Sets.unions $ list [
    ref Rewriting.freeVariablesInTypeDef @@ var "rdom",
    ref Rewriting.freeVariablesInTypeDef @@ var "icod",
    ref freeVariablesInContextDef @@ (ref Substitution.substInContextDef @@ var "isubst" @@ var "cx2")]) $
  "cx3" <~ ref Substitution.substInContextDef @@ var "isubst" @@ var "cx" $
  produce $ Typing.inferenceResult (var "rterm") (var "rtype") (var "isubst")

-- | Normalize a let term before inferring its type.
inferTypeOfLetDef :: TBinding (InferenceContext -> Let -> Flow s InferenceResult)
inferTypeOfLetDef = define "inferTypeOfLet" $
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
      Sets.toList $ ref Rewriting.freeVariablesInTermDef @@ var "term") $
  "adjList" <~ Lists.map (var "toPair") (var "bindings0") $
  "groups" <~ ref Sorting.topologicalSortComponentsDef @@ var "adjList" $
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
    "result" <~ var "helper" @@ (Lists.length $ var "groups") @@ list [] @@ var "iterm" $
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
     (Just $ ref inferTypeOfTermDef @@ var "cx" @@ var "rewrittenLet" @@ string "empty let term") [
     _Term_let>>: "l" ~> ref inferTypeOfLetNormalizedDef @@ var "cx" @@ var "l"]) $
  Flows.map (var "rewriteResult") (var "res")

inferTypeOfLetNormalizedDef :: TBinding (InferenceContext -> Let -> Flow s InferenceResult)
inferTypeOfLetNormalizedDef = define "inferTypeOfLetNormalized" $
  doc "Infer the type of a let (letrec) term which is already in a normal form" $
  "cx0" ~> "letTerm" ~>
  
  -- Extract the bindings and body from the let term
  "bins0" <~ Core.letBindings (var "letTerm") $
  "body0" <~ Core.letBody (var "letTerm") $
  "bnames" <~ Lists.map (unaryFunction Core.bindingName) (var "bins0") $
  
  -- Phase 1: Create fresh temporary type variables for each binding
  -- These act as placeholders during the initial inference pass
  "bvars" <<~ ref Schemas.freshNamesDef @@ (Lists.length $ var "bins0") $
  "tbins0" <~ Lists.map (unaryFunction Core.typeVariable) (var "bvars") $
  
  -- Extend the context with temporary monomorphic type schemes
  -- This allows recursive references during inference
  "cx1" <~ (ref extendContextDef
    @@ (Lists.zip (var "bnames") $ Lists.map ("t" ~> Core.typeScheme (list []) (var "t")) (var "tbins0"))
    @@ (var "cx0")) $
  
  -- Phase 2: Infer the actual types of all binding terms
  -- This returns: (inferred terms, inferred types, substitution s1)
  "inferredResult" <<~ ref inferTypesOfTemporaryBindingsDef @@ var "cx1" @@ var "bins0" $
  "bterms1" <~ Pairs.first (var "inferredResult") $  -- Terms with type applications embedded
  "tbins1" <~ Pairs.first (Pairs.second $ var "inferredResult") $  -- Actual inferred types
  "s1" <~ Pairs.second (Pairs.second $ var "inferredResult") $  -- Substitution from inference
  
  -- Phase 3: Unify temporary types with actual inferred types
  -- This ensures the temporary placeholders match what we actually inferred
  -- Returns substitution s2 that maps temporary variables to their actual types
  "s2" <<~ ref Unification.unifyTypeListsDef @@
    (Typing.inferenceContextSchemaTypes $ var "cx0") @@
    (Lists.map (ref Substitution.substInTypeDef @@ var "s1") (var "tbins0")) @@
    (var "tbins1") @@
    (string "temporary type bindings") $
  exec (ref Checking.checkTypeSubstDef @@ var "cx0" @@ var "s2") $

  -- Apply the composed substitution to the original context
  -- This context will be used for generalization
  "g2" <~ (ref Substitution.substInContextDef @@
    (ref Substitution.composeTypeSubstDef @@ var "s1" @@ var "s2") @@
    (var "cx0")) $

  -- Apply s2 to the terms as well as the types
  -- This ensures type applications in the terms use the same variables as the types
  "bterms1Subst" <~ Lists.map (ref Substitution.substTypesInTermDef @@ var "s2") (var "bterms1") $

  -- Phase 4: Generalize the inferred types into type schemes
  "tsbins1" <~ (Lists.zip (var "bnames") $
    Lists.map ("t" ~> ref generalizeDef @@ var "g2" @@
      (ref Substitution.substInTypeDef @@ var "s2" @@ var "t")) (var "tbins1")) $

  -- Phase 5: Infer the type of the let body
  -- The body can now reference the bindings with their generalized type schemes
  "bodyResult" <<~ ref inferTypeOfTermDef @@
    (ref extendContextDef @@ var "tsbins1" @@ var "g2") @@
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
          (ref buildTypeApplicationTermDef
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
      (ref Substitution.substituteInTermDef @@ var "st1" @@ var "term")
      (Lists.reverse $ Core.typeSchemeVariables $ var "ts") $

    -- Apply remaining substitutions (senv and s2) to the wrapped term
    Core.binding (var "name")
      (ref Substitution.substTypesInTermDef @@
        (ref Substitution.composeTypeSubstDef @@ var "sbody" @@ var "s2") @@
        (var "typeLambdaTerm"))
      (just $ ref Substitution.substInTypeSchemeDef @@ var "sbody" @@ var "ts")) $
  
  "bins1" <~ (Lists.map (var "createBinding") $
    Lists.zip (var "tsbins1") (var "bterms1Subst")) $

  -- Return the final let term with properly typed bindings
  "ret" <~ (Typing.inferenceResult
    (Core.termLet $ Core.let_ (var "bins1") (var "body1"))
    (var "tbody")
    (ref Substitution.composeTypeSubstListDef @@ list [var "s1", var "s2", var "sbody"])) $
  produce $ var "ret"

inferTypeOfListDef :: TBinding (InferenceContext -> [Term] -> Flow s InferenceResult)
inferTypeOfListDef = define "inferTypeOfList" $
  doc "Infer the type of a list" $
  "cx" ~> ref inferTypeOfCollectionDef
    @@ var "cx"
    @@ (unaryFunction Core.typeList)
    @@ (unaryFunction Core.termList)
    @@ string "list element"

inferTypeOfLiteralDef :: TBinding (InferenceContext -> Literal -> Flow s InferenceResult)
inferTypeOfLiteralDef = define "inferTypeOfLiteral" $
  doc "Infer the type of a literal" $
  "_" ~> "lit" ~>
  produce $ Typing.inferenceResult
    (Core.termLiteral $ var "lit")
    (Core.typeLiteral $ ref Reflect.literalTypeDef @@ var "lit")
    (ref Substitution.idTypeSubstDef)

inferTypeOfMapDef :: TBinding (InferenceContext -> M.Map Term Term -> Flow s InferenceResult)
inferTypeOfMapDef = define "inferTypeOfMap" $
  doc "Infer the type of a map" $
  "cx" ~> "m" ~>
  "kvar" <<~ ref Schemas.freshNameDef $
  "vvar" <<~ ref Schemas.freshNameDef $
  Logic.ifElse (Maps.null $ var "m")
    -- Empty map: add type applications for both key and value type variables
    (Flows.pure $ ref yieldDef
      @@ (ref buildTypeApplicationTermDef
        @@ list [var "kvar", var "vvar"]
        @@ (Core.termMap Maps.empty))
      @@ (Core.typeMap $ Core.mapType (Core.typeVariable $ var "kvar") (Core.typeVariable $ var "vvar"))
      @@ ref Substitution.idTypeSubstDef)
    -- Non-empty map: infer and unify key and value types
    ("kresults" <<~ ref inferManyDef @@ var "cx" @@
      (Lists.map ("k" ~> pair (var "k") (string "map key")) $ Maps.keys $ var "m") $
    "kterms" <~ Pairs.first (var "kresults") $
    "ktypes" <~ Pairs.first (Pairs.second $ var "kresults") $
    "ksubst" <~ Pairs.second (Pairs.second $ var "kresults") $
    "vresults" <<~ ref inferManyDef
      @@ var "cx"
      @@ (Lists.map ("v" ~> pair (var "v") (string "map value")) $ Maps.elems $ var "m") $
    "vterms" <~ Pairs.first (var "vresults") $
    "vtypes" <~ Pairs.first (Pairs.second $ var "vresults") $
    "vsubst" <~ Pairs.second (Pairs.second $ var "vresults") $
    "kcons" <~ Lists.map ("t" ~> Typing.typeConstraint (Core.typeVariable $ var "kvar") (var "t") (string "map key")) (var "ktypes") $
    "vcons" <~ Lists.map ("t" ~> Typing.typeConstraint (Core.typeVariable $ var "vvar") (var "t") (string "map value")) (var "vtypes") $
    ref mapConstraintsDef @@ var "cx" @@
      ("subst" ~> ref yieldDef
        @@ (Core.termMap $ Maps.fromList $ Lists.zip (var "kterms") (var "vterms"))
        @@ (Core.typeMap $ Core.mapType (Core.typeVariable $ var "kvar") (Core.typeVariable $ var "vvar"))
        @@ (ref Substitution.composeTypeSubstListDef @@ list [var "ksubst", var "vsubst", var "subst"])) @@
      (Lists.concat $ list [var "kcons", var "vcons"]))

inferTypeOfOptionalDef :: TBinding (InferenceContext -> Maybe Term -> Flow s InferenceResult)
inferTypeOfOptionalDef = define "inferTypeOfOptional" $
  doc "Infer the type of an optional" $
  "cx" ~> "m" ~>
  "trmCons" <~ ("terms" ~> Logic.ifElse (Lists.null $ var "terms")
    (Core.termMaybe nothing)
    (Core.termMaybe $ just $ Lists.head $ var "terms")) $
  ref inferTypeOfCollectionDef
    @@ var "cx"
    @@ (unaryFunction Core.typeMaybe)
    @@ var "trmCons"
    @@ string "optional element"
    @@ (Maybes.maybe (list []) (unaryFunction Lists.singleton) $ var "m")

inferTypeOfPairDef :: TBinding (InferenceContext -> (Term, Term) -> Flow s InferenceResult)
inferTypeOfPairDef = define "inferTypeOfPair" $
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
      ref yieldDef
        @@ var "termWithTypes"
        @@ (Core.typePair $ Core.pairType (var "tyFst") (var "tySnd"))
        @@ var "isubst")
    (ref inferManyDef @@ var "cx" @@ list [
      pair (Pairs.first $ var "p") (string "pair first element"),
      pair (Pairs.second $ var "p") (string "pair second element")])






--inferTypeOfProductDef :: TBinding (InferenceContext -> [Term] -> Flow s InferenceResult)
--inferTypeOfProductDef = define "inferTypeOfProduct" $
--  doc "Infer the type of a product (tuple)" $
--  "cx" ~> "els" ~>
--  Flows.map
--    ( "results" ~>
--      "iterms" <~ Pairs.first (var "results") $
--      "itypes" <~ Pairs.first (Pairs.second $ var "results") $
--      "isubst" <~ Pairs.second (Pairs.second $ var "results") $
--      ref yieldDef @@ (Core.termProduct $ var "iterms") @@ (Core.typeProduct $ var "itypes") @@ var "isubst")
--    (ref inferManyDef @@ var "cx" @@ (Lists.map ("e" ~> pair (var "e") (string "tuple element")) $ var "els"))










inferTypeOfPrimitiveDef :: TBinding (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfPrimitiveDef = define "inferTypeOfPrimitive" $
  doc "Infer the type of a primitive function" $
  "cx" ~> "name" ~>
  Maybes.maybe
    (Flows.fail $ Strings.cat2 (string "No such primitive: ") (Core.unName $ var "name"))
    ("scheme" ~>
      "ts" <<~ ref Schemas.instantiateTypeSchemeDef @@ var "scheme" $
      ref yieldCheckedDef
        @@ (ref buildTypeApplicationTermDef
          @@ Core.typeSchemeVariables (var "ts")
          @@ (Core.termFunction $ Core.functionPrimitive $ var "name"))
        @@ Core.typeSchemeType (var "ts")
        @@ ref Substitution.idTypeSubstDef)
    (Maps.lookup (var "name") (Typing.inferenceContextPrimitiveTypes $ var "cx"))

inferTypeOfProjectionDef :: TBinding (InferenceContext -> Projection -> Flow s InferenceResult)
inferTypeOfProjectionDef = define "inferTypeOfProjection" $
  doc "Infer the type of a record projection" $
  "cx" ~> "proj" ~>
  "tname" <~ Core.projectionTypeName (var "proj") $
  "fname" <~ Core.projectionField (var "proj") $
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ref ExtractCore.recordTypeDef @@ var "tname" @@ var "stype" $
  "ftyp" <<~ ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields" $
  Flows.pure $ ref yieldDef
    @@ (ref buildTypeApplicationTermDef @@ var "svars"
      @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $ Core.projection (var "tname") (var "fname")))
    @@ (Core.typeFunction $ Core.functionType
      (ref Schemas.nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      (var "ftyp"))
    @@ ref Substitution.idTypeSubstDef

inferTypeOfRecordDef :: TBinding (InferenceContext -> Record -> Flow s InferenceResult)
inferTypeOfRecordDef = define "inferTypeOfRecord" $
  doc "Infer the type of a record" $
  "cx" ~> "record" ~>
  "tname" <~ Core.recordTypeName (var "record") $
  "fields" <~ Core.recordFields (var "record") $
  "fnames" <~ Lists.map (unaryFunction Core.fieldName) (var "fields") $
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "results" <<~ ref inferManyDef @@ var "cx" @@ Lists.map
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
  ref mapConstraintsDef @@ var "cx" @@
    ("subst" ~> ref yieldDef
      @@ (ref buildTypeApplicationTermDef @@ var "svars" @@
        (Core.termRecord $ Core.record (var "tname") $ Lists.zipWith
          ("n" ~> "t" ~> Core.field (var "n") (var "t"))
          (var "fnames")
          (var "iterms")))
      @@ (ref Schemas.nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      @@ (ref Substitution.composeTypeSubstDef @@ var "isubst" @@ var "subst")) @@
    list [Typing.typeConstraint (var "stype") (var "ityp") (string "schema type of record")]

inferTypeOfSetDef :: TBinding (InferenceContext -> S.Set Term -> Flow s InferenceResult)
inferTypeOfSetDef = define "inferTypeOfSet" $
  doc "Infer the type of a set" $
  "cx" ~>
  "s" ~>
  ref inferTypeOfCollectionDef
    @@ var "cx"
    @@ (unaryFunction Core.typeSet)
    @@ ("terms" ~> Core.termSet $ Sets.fromList $ var "terms")
    @@ string "set element"
    @@ (Sets.toList $ var "s")

inferTypeOfTermDef :: TBinding (InferenceContext -> Term -> String -> Flow s InferenceResult)
inferTypeOfTermDef = define "inferTypeOfTerm" $
  doc "Infer the type of a given term" $
  "cx" ~> "term" ~> "desc" ~>
  "matchTerm" <~ (cases _Term (var "term") Nothing [
    _Term_annotated>>: "a" ~> ref inferTypeOfAnnotatedTermDef @@ var "cx" @@ var "a",
    _Term_application>>: "a" ~> ref inferTypeOfApplicationDef @@ var "cx" @@ var "a",
    _Term_either>>: "e" ~> ref inferTypeOfEitherDef @@ var "cx" @@ var "e",
    _Term_function>>: "f" ~> ref inferTypeOfFunctionDef @@ var "cx" @@ var "f",
    _Term_let>>: "l" ~> ref inferTypeOfLetDef @@ var "cx" @@ var "l",
    _Term_list>>: "els" ~> ref inferTypeOfListDef @@ var "cx" @@ var "els",
    _Term_literal>>: "l" ~> ref inferTypeOfLiteralDef @@ var "cx" @@ var "l",
    _Term_map>>: "m" ~> ref inferTypeOfMapDef @@ var "cx" @@ var "m",
    _Term_maybe>>: "m" ~> ref inferTypeOfOptionalDef @@ var "cx" @@ var "m",
    _Term_pair>>: "p" ~> ref inferTypeOfPairDef @@ var "cx" @@ var "p",
    _Term_record>>: "r" ~> ref inferTypeOfRecordDef @@ var "cx" @@ var "r",
    _Term_set>>: "s" ~> ref inferTypeOfSetDef @@ var "cx" @@ var "s",
    _Term_typeApplication>>: "tt" ~> ref inferTypeOfTypeApplicationDef @@ var "cx" @@ var "tt",
    _Term_typeLambda>>: "ta" ~> ref inferTypeOfTypeLambdaDef @@ var "cx" @@ var "ta",
    _Term_union>>: "i" ~> ref inferTypeOfInjectionDef @@ var "cx" @@ var "i",
    _Term_unit>>: constant $ produce $ ref inferTypeOfUnitDef,
    _Term_variable>>: "name" ~> ref inferTypeOfVariableDef @@ var "cx" @@ var "name",
    _Term_wrap>>: "w" ~> ref inferTypeOfWrappedTermDef @@ var "cx" @@ var "w"]) $
  trace (var "desc") (var "matchTerm")

inferTypeOfTypeLambdaDef :: TBinding (InferenceContext -> TypeLambda -> Flow s InferenceResult)
inferTypeOfTypeLambdaDef = define "inferTypeOfTypeLambda" $
  doc "Infer the type of a type abstraction; just pass through to the lambda body." $
  "cx" ~> "ta" ~>
  ref inferTypeOfTermDef @@ var "cx" @@ (Core.typeLambdaBody $ var "ta") @@ string "type abstraction"

inferTypeOfTypeApplicationDef :: TBinding (InferenceContext -> TypeApplicationTerm -> Flow s InferenceResult)
inferTypeOfTypeApplicationDef = define "inferTypeOfTypeApplication" $
  doc "Infer the type of a type application; just pass through to the inner term." $
  "cx" ~> "tt" ~>
  ref inferTypeOfTermDef @@ var "cx" @@ (Core.typeApplicationTermBody $ var "tt") @@ string "type application term"

inferTypeOfUnitDef :: TBinding InferenceResult
inferTypeOfUnitDef = define "inferTypeOfUnit" $
  doc "The trivial inference rule for the unit term" $
  Typing.inferenceResult
    (Core.termUnit)
    (Core.typeUnit)
    (ref Substitution.idTypeSubstDef)

inferTypeOfUnwrapDef :: TBinding (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfUnwrapDef = define "inferTypeOfUnwrap" $
  doc "Infer the type of an unwrap operation" $
  "cx" ~> "tname" ~>
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "wtyp" <<~ ref ExtractCore.wrappedTypeDef @@ var "tname" @@ var "stype" $
  Flows.pure $ ref yieldDef
    @@ (ref buildTypeApplicationTermDef @@ var "svars"
      @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationWrap $ var "tname"))
    @@ (Core.typeFunction $ Core.functionType
      (ref Schemas.nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      (var "wtyp"))
    @@ ref Substitution.idTypeSubstDef

inferTypeOfVariableDef :: TBinding (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfVariableDef = define "inferTypeOfVariable" $
  doc "Infer the type of a variable" $
  "cx" ~> "name" ~>
  Maybes.maybe
    (Flows.fail $ Strings.cat2 (string "Variable not bound to type: ") (Core.unName $ var "name"))
    ("scheme" ~>
      "ts" <<~ ref Schemas.instantiateTypeSchemeDef @@ var "scheme" $
      produce $ Typing.inferenceResult
        (ref buildTypeApplicationTermDef
          @@ Core.typeSchemeVariables (var "ts")
          @@ Core.termVariable (var "name"))
        (Core.typeSchemeType $ var "ts")
        (ref Substitution.idTypeSubstDef))
    (Maps.lookup (var "name") (Typing.inferenceContextDataTypes $ var "cx"))

inferTypeOfWrappedTermDef :: TBinding (InferenceContext -> WrappedTerm -> Flow s InferenceResult)
inferTypeOfWrappedTermDef = define "inferTypeOfWrappedTerm" $
  doc "Infer the type of a wrapped term" $
  "cx" ~> "wt" ~>
  "tname" <~ Core.wrappedTermTypeName (var "wt") $
  "term" <~ Core.wrappedTermBody (var "wt") $
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "result" <<~ ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "wrapped term" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "itype" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "ityp" <~ Core.typeWrap (Core.wrappedType (var "tname") (var "itype")) $
  ref mapConstraintsDef @@ var "cx"
    @@ ("subst" ~> ref yieldDef
      @@ (ref buildTypeApplicationTermDef @@ var "svars" @@ (Core.termWrap $ Core.wrappedTerm (var "tname") (var "iterm")))
      @@ (ref Schemas.nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      @@ (ref Substitution.composeTypeSubstDef @@ var "isubst" @@ var "subst"))
    @@ list [Typing.typeConstraint (var "stype") (var "ityp") (string "schema type of wrapper")]

inferTypesOfTemporaryBindingsDef :: TBinding (InferenceContext -> [Binding] -> Flow s ([Term], ([Type], TypeSubst)))
inferTypesOfTemporaryBindingsDef = define "inferTypesOfTemporaryBindings" $
  doc "Infer types for temporary let bindings" $
  "cx" ~> "bins" ~>
  "dflt" <~ (
    "binding" <~ Lists.head (var "bins") $
    "k" <~ Core.bindingName (var "binding") $
    "v" <~ Core.bindingTerm (var "binding") $
    "tl" <~ Lists.tail (var "bins") $
    "result1" <<~ ref inferTypeOfTermDef @@ var "cx" @@ var "v" @@
      (Strings.cat $ list [
        string "temporary let binding '",
        Core.unName $ var "k",
        string "'"]) $
    "j" <~ Typing.inferenceResultTerm (var "result1") $
    "u_prime" <~ Typing.inferenceResultType (var "result1") $
    "u" <~ Typing.inferenceResultSubst (var "result1") $
    "result2" <<~ ref inferTypesOfTemporaryBindingsDef @@
      (ref Substitution.substInContextDef @@ var "u" @@ var "cx") @@
      var "tl" $
    "h" <~ Pairs.first (var "result2") $
    "r_prime" <~ Pairs.first (Pairs.second $ var "result2") $
    "r" <~ Pairs.second (Pairs.second $ var "result2") $
    Flows.pure $ pair
      (Lists.cons (ref Substitution.substTypesInTermDef @@ var "r" @@ var "j") (var "h"))
      (pair
        (Lists.cons (ref Substitution.substInTypeDef @@ var "r" @@ var "u_prime") (var "r_prime"))
        (ref Substitution.composeTypeSubstDef @@ var "u" @@ var "r"))) $
  Logic.ifElse (Lists.null $ var "bins")
    (Flows.pure $ pair (list []) (pair (list []) (ref Substitution.idTypeSubstDef)))
    (var "dflt")

initialTypeContextDef :: TBinding (Graph -> Flow s TypeContext)
initialTypeContextDef = define "initialTypeContext" $
  doc "Create an initial type context from a graph" $
  "g" ~>
  "toPair" <~ ("pair" ~>
    "name" <~ Pairs.first (var "pair") $
    "el"  <~ Pairs.second (var "pair") $
    optCases (Core.bindingType $ var "el")
      (Flows.fail $ "untyped element: " ++ Core.unName (var "name"))
      ("ts" ~> produce $ pair (var "name") (ref Schemas.typeSchemeToFTypeDef @@ var "ts"))) $
  "ix" <<~ ref Schemas.graphToInferenceContextDef @@ var "g" $
  "types" <<~ Flows.map
    (unaryFunction Maps.fromList)
    (Flows.mapList (var "toPair") (Maps.toList $ Graph.graphElements $ var "g")) $
  produce $ Typing.typeContext (var "types") Sets.empty (var "ix")

isUnboundDef :: TBinding (InferenceContext -> Name -> Bool)
isUnboundDef = define "isUnbound" $
  doc "Check if a variable is unbound in context" $
  "cx" ~> "v" ~>
  Logic.and
    (Logic.not $ Sets.member (var "v") $ ref freeVariablesInContextDef @@ var "cx")
    (Logic.not $ Maps.member (var "v") $ Typing.inferenceContextSchemaTypes $ var "cx")

mapConstraintsDef :: TBinding (InferenceContext -> (TypeSubst -> a) -> [TypeConstraint] -> Flow s a)
mapConstraintsDef = define "mapConstraints" $
  doc "Map over type constraints after unification" $
  "cx" ~> "f" ~> "constraints" ~>
  "s" <<~ ref Unification.unifyTypeConstraintsDef @@ (Typing.inferenceContextSchemaTypes $ var "cx") @@ var "constraints" $
  exec (ref Checking.checkTypeSubstDef @@ var "cx" @@ var "s") $
  produce (var "f" @@ var "s")

showInferenceResultDef :: TBinding (InferenceResult -> String)
showInferenceResultDef = define "showInferenceResult" $
  doc "Show an inference result for debugging" $
  "result" ~>
  "term" <~ Typing.inferenceResultTerm (var "result") $
  "typ" <~ Typing.inferenceResultType (var "result") $
  "subst" <~ Typing.inferenceResultSubst (var "result") $
  Strings.cat $ list [
    string "{term=",
    ref ShowCore.termDef @@ var "term",
    string ", type=",
    ref ShowCore.typeDef @@ var "typ",
    string ", subst=",
    ref ShowTyping.typeSubstDef @@ var "subst",
    string "}"]

yieldDef :: TBinding (Term -> Type -> TypeSubst -> InferenceResult)
yieldDef = define "yield" $
  doc "Create an inference result" $
  "term" ~> "typ" ~> "subst" ~>
  Typing.inferenceResult
    (ref Substitution.substTypesInTermDef @@ var "subst" @@ var "term")
    (ref Substitution.substInTypeDef @@ var "subst" @@ var "typ")
    (var "subst")

-- TODO: pass context and variables, and actually check types
yieldCheckedDef :: TBinding (Term -> Type -> TypeSubst -> Flow s InferenceResult)
yieldCheckedDef = define "yieldChecked" $
  doc "Create a checked inference result" $
  "term" ~> "typ" ~> "subst" ~>
  "iterm" <~ ref Substitution.substTypesInTermDef @@ var "subst" @@ var "term" $
  "itype" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "typ" $
  produce $ Typing.inferenceResult (var "iterm") (var "itype") (var "subst")

yieldDebugDef :: TBinding (InferenceContext -> String -> Term -> Type -> TypeSubst -> Flow s InferenceResult)
yieldDebugDef = define "yieldDebug" $
  doc "Create an inference result with debug output" $
  "cx" ~> "debugId" ~> "term" ~> "typ" ~> "subst" ~>
  "rterm" <~ ref Substitution.substTypesInTermDef @@ var "subst" @@ var "term" $
  "rtyp" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "typ" $
  "result" <<~ ref Annotations.debugIfDef @@ var "debugId" @@
    (Strings.cat $ list [
      string "\n\tterm: ",  ref ShowCore.termDef @@ var "term",
      string "\n\ttyp: ",   ref ShowCore.typeDef @@ var "typ",
      string "\n\tsubst: ", ref ShowTyping.typeSubstDef @@ var "subst",
      string "\n\trterm: ", ref ShowCore.termDef @@ var "rterm",
      string "\n\trtyp: ",  ref ShowCore.typeDef @@ var "rtyp"]) $
  produce $ Typing.inferenceResult (var "rterm") (var "rtyp") (var "subst")
