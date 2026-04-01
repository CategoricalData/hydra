
module Hydra.Sources.Kernel.Terms.Inference where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  bindConstraints, bindUnboundTypeVariables, buildTypeApplicationTerm,
  extendContext, finalizeInferredTerm,
  forInferredTerm, freeVariablesInContext, freshVariableType,
  generalize, inferGraphTypes, inferInGraphContext, inferMany,
  inferTypeOf, inferTypeOfAnnotatedTerm, inferTypeOfApplication,
  inferTypeOfCaseStatement, inferTypeOfCollection,
  inferTypeOfEither, inferTypeOfElimination,
  inferTypeOfFunction, inferTypeOfInjection,
  inferTypeOfLambda, inferTypeOfLet, inferTypeOfLetNormalized,
  inferTypeOfList, inferTypeOfLiteral,
  inferTypeOfMap, inferTypeOfOptional,
  inferTypeOfPair, inferTypeOfPrimitive,
  inferTypeOfProjection, inferTypeOfRecord,
  inferTypeOfSet, inferTypeOfTerm,
  inferTypeOfTypeLambda, inferTypeOfTypeApplication,
  inferTypeOfUnit, inferTypeOfUnwrap, inferTypeOfVariable,
  inferTypeOfWrappedTerm, inferTypesOfTemporaryBindings,
  isUnbound, mapConstraints, mergeClassConstraints, showInferenceResult, yield, yieldChecked,
  yieldCheckedWithConstraints, yieldDebug, yieldWithConstraints)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Annotations  as Annotations
import qualified Hydra.Sources.Kernel.Terms.Checking     as Checking
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Reflect      as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting    as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas      as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core    as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Errors  as ShowError
import qualified Hydra.Sources.Kernel.Terms.Show.Typing  as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting      as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution
import qualified Hydra.Sources.Kernel.Terms.Unification  as Unification


ns :: Namespace
ns = Namespace "hydra.inference"

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, Checking.ns, ExtractCore.ns, Lexical.ns, Reflect.ns,
      Rewriting.ns, Schemas.ns, ShowCore.ns, ShowError.ns, ShowTyping.ns, Sorting.ns, Substitution.ns,
      Unification.ns]
    kernelTypesNamespaces $
    Just "Type inference following Algorithm W, extended for nominal terms and types"
  where
    elements = [
      toDefinition bindConstraints,
      toDefinition bindUnboundTypeVariables,
      toDefinition buildTypeApplicationTerm,
      toDefinition extendContext,
      toDefinition finalizeInferredTerm,
      toDefinition forInferredTerm,
      toDefinition freeVariablesInContext,
      toDefinition freshVariableType,
      toDefinition generalize,
      toDefinition inferGraphTypes,
      toDefinition inferInGraphContext,
      toDefinition inferMany,
      toDefinition inferTypeOf,
      toDefinition inferTypeOfAnnotatedTerm,
      toDefinition inferTypeOfApplication,
      toDefinition inferTypeOfCaseStatement,
      toDefinition inferTypeOfCollection,
      toDefinition inferTypeOfEither,
      toDefinition inferTypeOfElimination,
      toDefinition inferTypeOfFunction,
      toDefinition inferTypeOfInjection,
      toDefinition inferTypeOfLambda,
      toDefinition inferTypeOfLet,
      toDefinition inferTypeOfLetNormalized,
      toDefinition inferTypeOfList,
      toDefinition inferTypeOfLiteral,
      toDefinition inferTypeOfMap,
      toDefinition inferTypeOfOptional,
      toDefinition inferTypeOfPair,
      toDefinition inferTypeOfPrimitive,
      toDefinition inferTypeOfProjection,
      toDefinition inferTypeOfRecord,
      toDefinition inferTypeOfSet,
      toDefinition inferTypeOfTerm,
      toDefinition inferTypeOfTypeLambda,
      toDefinition inferTypeOfTypeApplication,
      toDefinition inferTypeOfUnit,
      toDefinition inferTypeOfUnwrap,
      toDefinition inferTypeOfVariable,
      toDefinition inferTypeOfWrappedTerm,
      toDefinition inferTypesOfTemporaryBindings,
      toDefinition isUnbound,
      toDefinition mapConstraints,
      toDefinition mergeClassConstraints,
      toDefinition showInferenceResult,
      toDefinition yield,
      toDefinition yieldChecked,
      toDefinition yieldCheckedWithConstraints,
      toDefinition yieldDebug,
      toDefinition yieldWithConstraints]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- | Bridge helper: format InContext UnificationError as a string
formatUnifError :: TTerm (InContext UnificationError -> String)
formatUnifError = "ic" ~> Error.unificationErrorMessage (Ctx.inContextObject (var "ic"))

-- | Bridge helper: format InContext Error as a string
formatError :: TTerm (InContext Error -> String)
formatError = "ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")

--

bindConstraints :: TTermDefinition (Context -> Graph -> [TypeConstraint] -> Either (InContext Error) TypeSubst)
bindConstraints = define "bindConstraints" $
  doc "Unify type constraints and check the substitution" $
  "flowCx" ~> "cx" ~> "constraints" ~>
  "s" <<~ Eithers.bimap
    ("_ic" ~> Ctx.inContext (Error.errorOther $ Error.otherError (Error.unificationErrorMessage (Ctx.inContextObject (var "_ic")))) (Ctx.inContextContext (var "_ic")))
    ("_a" ~> var "_a")
    (Unification.unifyTypeConstraints @@ var "flowCx" @@ (Graph.graphSchemaTypes $ var "cx") @@ var "constraints") $
  Eithers.bind (Checking.checkTypeSubst @@ var "flowCx" @@ var "cx" @@ var "s") (
    "_" ~> right (var "s"))

bindUnboundTypeVariables :: TTermDefinition (Graph -> Term -> Term)
bindUnboundTypeVariables = define "bindUnboundTypeVariables" $
  doc ("Place unbound type variables appearing anywhere under a typed let binding in the type scheme of that binding."
    <> " These variables may appear in the binding type scheme itself or in that of a subterm,"
    <> " in domain types attached to functions, and in type abstraction and type application terms."
    <> " This process attempts to capture type variables which have escaped unification, e.g. due to unused code."
    <> " However, unbound type variables not appearing beneath any typed let binding remain unbound.") $
  "cx" ~> "term0" ~>
  "svars" <~ Sets.fromList (Maps.keys $ Graph.graphSchemaTypes $ var "cx") $
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
              (Core.typeSchemeType $ var "ts")
              (Core.typeSchemeConstraints $ var "ts") $
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

buildTypeApplicationTerm :: TTermDefinition ([Name] -> Term -> Term)
buildTypeApplicationTerm = define "buildTypeApplicationTerm" $
  doc "Fold a list of type variables over a term to build a type application term" $
  "tvars" ~> "body" ~> Lists.foldl
    ("t" ~> "v" ~> Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeVariable (var "v")))
    (var "body")
    (var "tvars")

extendContext :: TTermDefinition ([(Name, TypeScheme)] -> Graph -> Graph)
extendContext = define "extendContext" $
  doc "Add (term variable, type scheme) pairs to the graph's bound types" $
  "pairs" ~> "cx" ~>
  Graph.graphWithBoundTypes (var "cx") $ Maps.union
    (Maps.fromList $ var "pairs")
    (Graph.graphBoundTypes $ var "cx")

finalizeInferredTerm :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Term)
finalizeInferredTerm = define "finalizeInferredTerm" $
  doc "Finalize an inferred term by checking for unbound type variables, then normalizing type variables" $
  "flowCx" ~> "cx" ~> "term" ~>
  "term2" <~ bindUnboundTypeVariables @@ var "cx" @@ var "term" $
  "_" <<~ Checking.checkForUnboundTypeVariables @@ var "flowCx" @@ var "cx" @@ var "term2" $
  right (Rewriting.normalizeTypeVariablesInTerm @@ var "term2")

forInferredTerm :: TTermDefinition (Context -> Graph -> Term -> String -> (InferenceResult -> a) -> Prelude.Either (InContext Error) (a, Context))
forInferredTerm = define "forInferredTerm" $
  doc "Infer a term's type and map over the result" $
  "fcx" ~> "cx" ~> "term" ~> "desc" ~> "f" ~>
  "rp" <<~ inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "term" @@ var "desc" $
  right $ pair (var "f" @@ var "rp") (Typing.inferenceResultContext (var "rp"))

freeVariablesInContext :: TTermDefinition (Graph -> S.Set Name)
freeVariablesInContext = define "freeVariablesInContext" $
  doc "Get all free variables in a graph's bound types" $
  "cx" ~>
    Lists.foldl (binaryFunction Sets.union) Sets.empty $
      Lists.map (Rewriting.freeVariablesInTypeSchemeSimple) $
        Maps.elems $ Graph.graphBoundTypes $ var "cx"

freshVariableType :: TTermDefinition (Context -> (Type, Context))
freshVariableType = define "freshVariableType" $
  doc "Generate a fresh type variable" $
  "cx" ~>
  "result" <~ Schemas.freshName @@ var "cx" $
  "name" <~ Pairs.first (var "result") $
  "cx2" <~ Pairs.second (var "result") $
  pair (Core.typeVariable (var "name")) (var "cx2")

mergeClassConstraints :: TTermDefinition (M.Map Name TypeVariableMetadata -> M.Map Name TypeVariableMetadata -> M.Map Name TypeVariableMetadata)
mergeClassConstraints = define "mergeClassConstraints" $
  doc "Merge two maps of class constraints. When both maps have constraints for the same variable, union the class sets." $
  "m1" ~> "m2" ~>
  Lists.foldl
    ("acc" ~> "pair" ~>
      "k" <~ Pairs.first (var "pair") $
      "v" <~ Pairs.second (var "pair") $
      Maybes.maybe
        (Maps.insert (var "k") (var "v") (var "acc"))
        ("existing" ~>
          "merged" <~ Core.typeVariableMetadata (Sets.union (Core.typeVariableMetadataClasses $ var "existing") (Core.typeVariableMetadataClasses $ var "v")) $
          Maps.insert (var "k") (var "merged") (var "acc"))
        (Maps.lookup (var "k") (var "acc")))
    (var "m1")
    (Maps.toList $ var "m2")

generalize :: TTermDefinition (Graph -> Type -> TypeScheme)
generalize = define "generalize" $
  doc "Generalize a type to a type scheme" $
  "cx" ~> "typ" ~>
  -- IMPORTANT: freeVariablesInTypeOrdered returns ALL names from Type_variable positions,
  -- including qualified type names (like hydra.core.Lambda) which are NOT actual type variables.
  -- We must filter these out to avoid quantifying over type names.
  "isTypeVarName" <~ ("name" ~>
    "parts" <~ Strings.splitOn (string ".") (Core.unName $ var "name") $
    Equality.lte (Lists.length $ var "parts") (int32 1)) $
  "vars" <~ Lists.nub (Lists.filter ("v" ~> Logic.and (isUnbound @@ var "cx" @@ var "v") (var "isTypeVarName" @@ var "v")) $
     Rewriting.freeVariablesInTypeOrdered @@ var "typ") $
  -- Extract constraints for the generalized variables from the context
  "allConstraints" <~ Graph.graphClassConstraints (var "cx") $
  "relevantConstraints" <~ Maps.fromList (Maybes.cat $ Lists.map
    ("v" ~> Maybes.map ("meta" ~> pair (var "v") (var "meta")) $ Maps.lookup (var "v") (var "allConstraints"))
    (var "vars")) $
  -- Only include constraints if there are any
  "constraintsMaybe" <~ Logic.ifElse (Maps.null $ var "relevantConstraints") Phantoms.nothing (just $ var "relevantConstraints") $
  Core.typeScheme (var "vars") (var "typ") (var "constraintsMaybe")

inferGraphTypes :: TTermDefinition (Context -> [Binding] -> Graph -> Prelude.Either (InContext Error) ((Graph, [Binding]), Context))
inferGraphTypes = define "inferGraphTypes" $
  doc ("Infer types for all elements in a graph, using the provided ordered bindings."
    <> " Returns both the inferred graph and the ordered inferred bindings.") $
  "fcx0" ~> "bindings0" ~> "g0" ~>
  "fcx" <~ Ctx.pushTrace (string "graph inference") (var "fcx0") $
  "let0" <~ Core.let_ (var "bindings0") Core.termUnit $
  -- Reconstruct a (Graph, [Binding]) from an inferred Let term, preserving binding order
  "fromLetTerm" <~ ("l" ~>
    "bindings" <~ Core.letBindings (var "l") $
    "prims" <~ Graph.graphPrimitives (var "g0") $
    "schemaTypes" <~ Graph.graphSchemaTypes (var "g0") $
    "rawG" <~ (Lexical.buildGraph @@ var "bindings" @@ Maps.empty @@ var "prims") $
    "g" <~ Graph.graphWithSchemaTypes (var "rawG") (var "schemaTypes") $
    pair (var "g") (var "bindings")) $
  "result" <<~ inferTypeOfTerm @@ var "fcx" @@ var "g0" @@ (Core.termLet $ var "let0") @@ (string "graph term") $
  "fcx2" <~ Typing.inferenceResultContext (var "result") $
  "term" <~ Typing.inferenceResultTerm (var "result") $
  "finalized" <<~ finalizeInferredTerm @@ var "fcx2" @@ var "g0" @@ var "term" $
  cases _Term (var "finalized")
    Nothing [
    _Term_let>>: "l" ~> right $ pair (var "fromLetTerm" @@ var "l") (var "fcx2"),
    _Term_variable>>: constant $ Ctx.failInContext (Error.errorOther $ Error.otherError (string "Expected inferred graph as let term")) (var "fcx2")]

inferInGraphContext :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) InferenceResult)
inferInGraphContext = define "inferInGraphContext" $
  doc "Infer the type of a term in a given inference context" $
  "fcx" ~> "cx" ~> "term" ~>
  inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "term" @@ (string "single term")


inferTypeOf :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) ((Term, TypeScheme), Context))
inferTypeOf = define "inferTypeOf" $
  doc "Map a possibly untyped term to a fully typed term and its type" $
  "fcx" ~> "cx" ~> "term" ~>
  -- Top-level let term which allows us to easily extract an inferred type scheme
  "letTerm" <~ Core.termLet (Core.let_
    (list [Core.binding (Core.name $ (string "ignoredVariableName")) (var "term") nothing])
    (MetaTerms.string "ignoredBody")) $
  "result" <<~ inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "letTerm" @@ (string "infer type of term") $
  "fcx2" <~ Typing.inferenceResultContext (var "result") $
  "finalized" <<~ finalizeInferredTerm @@ var "fcx2" @@ var "cx" @@ Typing.inferenceResultTerm (var "result") $
  "letResult" <<~ ExtractCore.let_ @@ var "fcx2" @@ var "cx" @@ var "finalized" $
  "bindings" <~ Core.letBindings (var "letResult") $
  Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "bindings"))
    ("binding" <~ Lists.head (var "bindings") $
     "term1" <~ Core.bindingTerm (var "binding") $
     "mts" <~ Core.bindingType (var "binding") $
     Maybes.maybe
       (Ctx.failInContext (Error.errorOther $ Error.otherError (string "Expected a type scheme")) (var "fcx2"))
       ("ts" ~> right $ pair (pair (var "term1") (var "ts")) (var "fcx2"))
       (var "mts"))
    (Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [
        (string "Expected a single binding with a type scheme, but got: "),
        Literals.showInt32 $ Lists.length $ var "bindings",
        (string " bindings")])) (var "fcx2"))


inferTypeOfLiteral :: TTermDefinition (Context -> Literal -> InferenceResult)
inferTypeOfLiteral = define "inferTypeOfLiteral" $
  doc "Infer the type of a literal" $
  "fcx" ~> "lit" ~>
  Typing.inferenceResult
    (Core.termLiteral $ var "lit")
    (Core.typeLiteral $ Reflect.literalType @@ var "lit")
    (asTerm Substitution.idTypeSubst)
    Maps.empty
    (var "fcx")


inferTypeOfUnit :: TTermDefinition (Context -> InferenceResult)
inferTypeOfUnit = define "inferTypeOfUnit" $
  doc "The trivial inference rule for the unit term" $
  "fcx" ~>
  Typing.inferenceResult
    (Core.termUnit)
    (Core.typeUnit)
    (asTerm Substitution.idTypeSubst)
    Maps.empty
    (var "fcx")

isUnbound :: TTermDefinition (Graph -> Name -> Bool)
isUnbound = define "isUnbound" $
  doc "Check if a variable is unbound in context" $
  "cx" ~> "v" ~>
  Logic.and
    (Logic.not $ Sets.member (var "v") $ freeVariablesInContext @@ var "cx")
    (Logic.not $ Maps.member (var "v") $ Graph.graphSchemaTypes $ var "cx")

mapConstraints :: TTermDefinition (Context -> Graph -> (TypeSubst -> a) -> [TypeConstraint] -> Either (InContext Error) a)
mapConstraints = define "mapConstraints" $
  doc "Map over type constraints after unification" $
  "flowCx" ~> "cx" ~> "f" ~> "constraints" ~>
  "s" <<~ Eithers.bimap
    ("_ic" ~> Ctx.inContext (Error.errorOther $ Error.otherError (Error.unificationErrorMessage (Ctx.inContextObject (var "_ic")))) (Ctx.inContextContext (var "_ic")))
    ("_a" ~> var "_a")
    (Unification.unifyTypeConstraints @@ var "flowCx" @@ (Graph.graphSchemaTypes $ var "cx") @@ var "constraints") $
  Eithers.bind (Checking.checkTypeSubst @@ var "flowCx" @@ var "cx" @@ var "s") (
    "_" ~> right (var "f" @@ var "s"))

showInferenceResult :: TTermDefinition (InferenceResult -> String)
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

yield :: TTermDefinition (Context -> Term -> Type -> TypeSubst -> InferenceResult)
yield = define "yield" $
  doc "Create an inference result with no class constraints" $
  "fcx" ~> "term" ~> "typ" ~> "subst" ~>
  Typing.inferenceResult
    (Substitution.substTypesInTerm @@ var "subst" @@ var "term")
    (Substitution.substInType @@ var "subst" @@ var "typ")
    (var "subst")
    Maps.empty
    (var "fcx")

yieldWithConstraints :: TTermDefinition (Context -> Term -> Type -> TypeSubst -> M.Map Name TypeVariableMetadata -> InferenceResult)
yieldWithConstraints = define "yieldWithConstraints" $
  doc "Create an inference result with class constraints" $
  "fcx" ~> "term" ~> "typ" ~> "subst" ~> "constraints" ~>
  Typing.inferenceResult
    (Substitution.substTypesInTerm @@ var "subst" @@ var "term")
    (Substitution.substInType @@ var "subst" @@ var "typ")
    (var "subst")
    (var "constraints")
    (var "fcx")

-- TODO: pass context and variables, and actually check types
yieldChecked :: TTermDefinition (Context -> Term -> Type -> TypeSubst -> InferenceResult)
yieldChecked = define "yieldChecked" $
  doc "Create a checked inference result" $
  "fcx" ~> "term" ~> "typ" ~> "subst" ~>
  "iterm" <~ Substitution.substTypesInTerm @@ var "subst" @@ var "term" $
  "itype" <~ Substitution.substInType @@ var "subst" @@ var "typ" $
  Typing.inferenceResult (var "iterm") (var "itype") (var "subst") Maps.empty (var "fcx")

yieldCheckedWithConstraints :: TTermDefinition (Context -> Term -> Type -> TypeSubst -> M.Map Name TypeVariableMetadata -> InferenceResult)
yieldCheckedWithConstraints = define "yieldCheckedWithConstraints" $
  doc "Create a checked inference result with class constraints" $
  "fcx" ~> "term" ~> "typ" ~> "subst" ~> "constraints" ~>
  "iterm" <~ Substitution.substTypesInTerm @@ var "subst" @@ var "term" $
  "itype" <~ Substitution.substInType @@ var "subst" @@ var "typ" $
  -- Apply the substitution to constraint keys as well, so they track the final variable names
  "iconstraints" <~ Substitution.substInClassConstraints @@ var "subst" @@ var "constraints" $
  Typing.inferenceResult (var "iterm") (var "itype") (var "subst") (var "iconstraints") (var "fcx")

-- ============================================================================
-- Inference functions: Either (InContext Error) InferenceResult
-- All take "fcx" (Context for errors/tracing) and "cx" (Graph for type context)
-- and return InferenceResult (which now contains the updated Context).
-- ============================================================================

inferTypeOfAnnotatedTerm :: TTermDefinition (Context -> Graph -> AnnotatedTerm -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfAnnotatedTerm = define "inferTypeOfAnnotatedTerm" $
  doc "Infer the type of an annotated term (Either version)" $
  "fcx" ~> "cx" ~> "at" ~>
  "term" <~ Core.annotatedTermBody (var "at") $
  "ann" <~ Core.annotatedTermAnnotation (var "at") $
  "result" <<~ inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "term" @@ (string "annotated term") $
  "fcx2" <~ Typing.inferenceResultContext (var "result") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "itype" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "iconstraints" <~ Typing.inferenceResultClassConstraints (var "result") $
  right $ Typing.inferenceResult
    (Core.termAnnotated $ Core.annotatedTerm (var "iterm") (var "ann"))
    (var "itype")
    (var "isubst")
    (var "iconstraints")
    (var "fcx2")

inferTypeOfApplication :: TTermDefinition (Context -> Graph -> Application -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfApplication = define "inferTypeOfApplication" $
  doc "Infer the type of a function application (Either version)" $
  "fcx0" ~> "cx" ~> "app" ~>
  "fcx" <~ Ctx.pushTrace (string "application") (var "fcx0") $
  "e0" <~ Core.applicationFunction (var "app") $
  "e1" <~ Core.applicationArgument (var "app") $
  "lhsResult" <<~ inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "e0" @@ (string "lhs") $
  "fcx2" <~ Typing.inferenceResultContext (var "lhsResult") $
  "a" <~ Typing.inferenceResultTerm (var "lhsResult") $
  "t0" <~ Typing.inferenceResultType (var "lhsResult") $
  "s0" <~ Typing.inferenceResultSubst (var "lhsResult") $
  "c0" <~ Typing.inferenceResultClassConstraints (var "lhsResult") $
  "rhsResult" <<~ inferTypeOfTerm @@ var "fcx2"
    @@ (Substitution.substInContext @@ var "s0" @@ var "cx")
    @@ var "e1"
    @@ (string "rhs") $
  "fcx3" <~ Typing.inferenceResultContext (var "rhsResult") $
  "b" <~ Typing.inferenceResultTerm (var "rhsResult") $
  "t1" <~ Typing.inferenceResultType (var "rhsResult") $
  "s1" <~ Typing.inferenceResultSubst (var "rhsResult") $
  "c1" <~ Typing.inferenceResultClassConstraints (var "rhsResult") $
  "vResult" <~ Schemas.freshName @@ var "fcx3" $
  "v" <~ Pairs.first (var "vResult") $
  "fcx4" <~ Pairs.second (var "vResult") $
  "s2" <<~ Eithers.bimap
    ("_ic" ~> Ctx.inContext (Error.errorOther $ Error.otherError (Error.unificationErrorMessage (Ctx.inContextObject (var "_ic")))) (Ctx.inContextContext (var "_ic")))
    ("_a" ~> var "_a")
    (Unification.unifyTypes
    @@ var "fcx4"
    @@ (Graph.graphSchemaTypes $ var "cx")
    @@ (Substitution.substInType @@ var "s1" @@ var "t0")
    @@ (Core.typeFunction $ Core.functionType (var "t1") (Core.typeVariable $ var "v"))
    @@ (string "application lhs")) $
  "_" <<~ Checking.checkTypeSubst @@ var "fcx4" @@ var "cx" @@ var "s2" $
  "rExpr" <~ Core.termApplication (Core.application
    (Substitution.substTypesInTerm @@ (Substitution.composeTypeSubst @@ var "s1" @@ var "s2") @@ var "a")
    (Substitution.substTypesInTerm @@ var "s2" @@ var "b")) $
  "rType" <~ Substitution.substInType @@ var "s2" @@ Core.typeVariable (var "v") $
  "rSubst" <~ Substitution.composeTypeSubstList @@ list [var "s0", var "s1", var "s2"] $
  "c0Subst" <~ Substitution.substInClassConstraints @@ var "s2" @@ (Substitution.substInClassConstraints @@ var "s1" @@ var "c0") $
  "c1Subst" <~ Substitution.substInClassConstraints @@ var "s2" @@ var "c1" $
  "rConstraints" <~ mergeClassConstraints @@ var "c0Subst" @@ var "c1Subst" $
  right $ Typing.inferenceResult (var "rExpr") (var "rType") (var "rSubst") (var "rConstraints") (var "fcx4")

inferTypeOfCaseStatement :: TTermDefinition (Context -> Graph -> CaseStatement -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfCaseStatement = define "inferTypeOfCaseStatement" $
  doc "Infer the type of a case statement (Either version)" $
  "fcx" ~> "cx" ~> "caseStmt" ~>
  "tname" <~ Core.caseStatementTypeName (var "caseStmt") $
  "dflt" <~ Core.caseStatementDefault (var "caseStmt") $
  "cases" <~ Core.caseStatementCases (var "caseStmt") $
  "fnames" <~ Lists.map (unaryFunction Core.fieldName) (var "cases") $
  "stRp" <<~ Schemas.requireSchemaType @@ var "fcx" @@ (Graph.graphSchemaTypes $ var "cx") @@ var "tname" $
  "schemaType" <~ Pairs.first (var "stRp") $
  "fcx2" <~ Pairs.second (var "stRp") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ExtractCore.unionType @@ var "fcx2" @@ var "tname" @@ var "stype" $
  "dfltRp" <<~ Eithers.mapMaybe ("t" ~> inferTypeOfTerm @@ var "fcx2" @@ var "cx" @@ var "t" @@
    (Strings.cat $ list [(string "case "), Core.unName $ var "tname", (string ".<default>")])) (var "dflt") $
  -- dfltRp :: Maybe InferenceResult (context is inside the result)
  "dfltResult" <~ var "dfltRp" $
  "fcx3" <~ Maybes.fromMaybe (var "fcx2") (Maybes.map (unaryFunction Typing.inferenceResultContext) (var "dfltRp")) $
  "caseRp" <<~ inferMany @@ var "fcx3" @@ var "cx" @@ Lists.map
    ("f" ~> pair (Core.fieldTerm $ var "f")
      (Strings.cat $ list [(string "case "), Core.unName $ var "tname", (string "."), Core.unName $ Core.fieldName $ var "f"]))
    (var "cases") $
  "caseResults" <~ Pairs.first (var "caseRp") $
  "fcx4" <~ Pairs.second (var "caseRp") $
  "iterms" <~ Pairs.first (var "caseResults") $
  "itypes" <~ Pairs.first (Pairs.second $ var "caseResults") $
  "isubst" <~ Pairs.first (Pairs.second $ Pairs.second $ var "caseResults") $
  "caseElemConstraints" <~ Pairs.second (Pairs.second $ Pairs.second $ var "caseResults") $
  "codvResult" <~ Schemas.freshName @@ var "fcx4" $
  "codv" <~ Pairs.first (var "codvResult") $
  "fcx5" <~ Pairs.second (var "codvResult") $
  "cod" <~ Core.typeVariable (var "codv") $
  "caseMap" <~ Maps.fromList (Lists.map
    ("ft" ~> pair (Core.fieldTypeName $ var "ft") (Core.fieldTypeType $ var "ft"))
    (var "sfields")) $
  "dfltConstraints" <~ Maybes.toList (Maybes.map
    ("r" ~> Typing.typeConstraint (var "cod")
      (Substitution.substInType @@ var "isubst" @@ (Typing.inferenceResultType $ var "r"))
      (string "match default"))
    (var "dfltResult")) $
  "caseConstraints" <~ Maybes.cat (Lists.zipWith
    ("fname" ~> "itype" ~> Maybes.map
      ("ftype" ~> Typing.typeConstraint
        (var "itype")
        (Core.typeFunction $ Core.functionType (var "ftype") (var "cod"))
        (string "case type"))
      (Maps.lookup (var "fname") (var "caseMap")))
    (var "fnames") (var "itypes")) $
  "dfltClassConstraints" <~ Maybes.fromMaybe Maps.empty (Maybes.map (unaryFunction Typing.inferenceResultClassConstraints) (var "dfltResult")) $
  "allElemConstraints" <~ mergeClassConstraints @@ var "caseElemConstraints" @@ var "dfltClassConstraints" $
  "mcResult" <<~ mapConstraints @@ var "fcx5" @@ var "cx"
    @@ ("subst" ~> yieldWithConstraints
      @@ var "fcx5"
      @@ (buildTypeApplicationTerm @@ var "svars"
          @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationUnion $
            Core.caseStatement (var "tname") (Maybes.map (unaryFunction Typing.inferenceResultTerm) $ var "dfltResult") $
            Lists.zipWith ("n" ~> "t" ~> Core.field (var "n") (var "t")) (var "fnames") (var "iterms")))
      @@ (Core.typeFunction $ Core.functionType
          (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
          (var "cod"))
      @@ (Substitution.composeTypeSubstList
        @@ (Lists.concat $ list [
          Maybes.toList (Maybes.map (unaryFunction Typing.inferenceResultSubst) (var "dfltResult")),
          list [var "isubst", var "subst"]]))
      @@ (Substitution.substInClassConstraints @@ var "subst" @@ var "allElemConstraints"))
    @@ (Lists.concat $ list [var "dfltConstraints", var "caseConstraints"]) $
  right (var "mcResult")

inferTypeOfCollection :: TTermDefinition (Context -> Graph -> (Type -> Type) -> ([Term] -> Term) -> String -> S.Set Name -> [Term] -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfCollection = define "inferTypeOfCollection" $
  doc "Infer the type of a collection. The classNames parameter specifies type classes (e.g. ordering) that the element type variable must satisfy." $
  "fcx" ~> "cx" ~> "typCons" ~> "trmCons" ~> "desc" ~> "classNames" ~> "els" ~>
  "varResult" <~ Schemas.freshName @@ var "fcx" $
  "var" <~ Pairs.first (var "varResult") $
  "fcx2" <~ Pairs.second (var "varResult") $
  "classConstraints" <~ Logic.ifElse (Sets.null $ var "classNames")
    Maps.empty
    (Maps.singleton (var "var") (Core.typeVariableMetadata $ var "classNames")) $
  Logic.ifElse (Lists.null $ var "els")
    (right (yieldWithConstraints
      @@ var "fcx2"
      @@ (buildTypeApplicationTerm
        @@ list [var "var"]
        @@ (var "trmCons" @@ list ([] :: [TTerm Term])))
      @@ (var "typCons" @@ (Core.typeVariable $ var "var"))
      @@ (Substitution.idTypeSubst)
      @@ var "classConstraints"))
    ("resultsRp" <<~ inferMany @@ var "fcx2" @@ var "cx" @@
      (Lists.zip (var "els") $ Lists.map ("i" ~> Strings.cat $ list [(string "#"), Literals.showInt32 $ var "i"]) $
        Math.range (int32 1) (Math.add (Lists.length $ var "els") (int32 1))) $
    "results" <~ Pairs.first (var "resultsRp") $
    "fcx3" <~ Pairs.second (var "resultsRp") $
    "terms" <~ Pairs.first (var "results") $
    "types" <~ Pairs.first (Pairs.second $ var "results") $
    "subst1" <~ Pairs.first (Pairs.second $ Pairs.second $ var "results") $
    "elemConstraints" <~ Pairs.second (Pairs.second $ Pairs.second $ var "results") $
    "constraints" <~ Lists.map ("t" ~> Typing.typeConstraint (Core.typeVariable $ var "var") (var "t") (var "desc")) (var "types") $
    "allConstraints" <~ mergeClassConstraints @@ var "classConstraints" @@ var "elemConstraints" $
    "mcResult" <<~ mapConstraints @@ var "fcx3" @@ var "cx" @@
      ("subst2" ~>
        "iterm" <~ var "trmCons" @@ var "terms" $
        "itype" <~ var "typCons" @@ (Core.typeVariable $ var "var") $
        "isubst" <~ Substitution.composeTypeSubst @@ var "subst1" @@ var "subst2" $
        yieldWithConstraints @@ var "fcx3" @@ var "iterm" @@ var "itype" @@ var "isubst" @@ (Substitution.substInClassConstraints @@ var "subst2" @@ var "allConstraints")) @@
      var "constraints" $
    right (var "mcResult"))

inferTypeOfEither :: TTermDefinition (Context -> Graph -> Prelude.Either Term Term -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfEither = define "inferTypeOfEither" $
  doc "Infer the type of an either value (Either version)" $
  "fcx" ~> "cx" ~> "e" ~>
  Eithers.either_
    ("l" ~>
      "r1" <<~ inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "l" @@ (string "either left value") $
      "fcx2" <~ Typing.inferenceResultContext (var "r1") $
      "iterm" <~ Typing.inferenceResultTerm (var "r1") $
      "leftType" <~ Typing.inferenceResultType (var "r1") $
      "subst" <~ Typing.inferenceResultSubst (var "r1") $
      "fvResult" <~ freshVariableType @@ var "fcx2" $
      "rightType" <~ Pairs.first (var "fvResult") $
      "fcx3" <~ Pairs.second (var "fvResult") $
      "eitherTerm" <~ (Core.termEither $ left $ var "iterm") $
      "termWithLeftType" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "eitherTerm") (var "leftType")) $
      "termWithBothTypes" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "termWithLeftType") (var "rightType")) $
      "eitherType" <~ (Core.typeEither $ Core.eitherType (var "leftType") (var "rightType")) $
      right (yieldChecked @@ var "fcx3" @@ var "termWithBothTypes" @@ var "eitherType" @@ var "subst"))
    ("r" ~>
      "r1" <<~ inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "r" @@ (string "either right value") $
      "fcx2" <~ Typing.inferenceResultContext (var "r1") $
      "iterm" <~ Typing.inferenceResultTerm (var "r1") $
      "rightType" <~ Typing.inferenceResultType (var "r1") $
      "subst" <~ Typing.inferenceResultSubst (var "r1") $
      "fvResult" <~ freshVariableType @@ var "fcx2" $
      "leftType" <~ Pairs.first (var "fvResult") $
      "fcx3" <~ Pairs.second (var "fvResult") $
      "eitherTerm" <~ (Core.termEither $ right $ var "iterm") $
      "termWithLeftType" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "eitherTerm") (var "leftType")) $
      "termWithBothTypes" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "termWithLeftType") (var "rightType")) $
      "eitherType" <~ (Core.typeEither $ Core.eitherType (var "leftType") (var "rightType")) $
      right (yieldChecked @@ var "fcx3" @@ var "termWithBothTypes" @@ var "eitherType" @@ var "subst"))
    (var "e")

inferTypeOfElimination :: TTermDefinition (Context -> Graph -> Elimination -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfElimination = define "inferTypeOfElimination" $
  doc "Infer the type of an elimination (Either version)" $
  "fcx" ~> "cx" ~> "elm" ~>
  cases _Elimination (var "elm") Nothing [
    _Elimination_record>>: "p" ~> inferTypeOfProjection @@ var "fcx" @@ var "cx" @@ var "p",
    _Elimination_union>>: "c" ~> inferTypeOfCaseStatement @@ var "fcx" @@ var "cx" @@ var "c",
    _Elimination_wrap>>: "tname" ~> inferTypeOfUnwrap @@ var "fcx" @@ var "cx" @@ var "tname"]

inferTypeOfFunction :: TTermDefinition (Context -> Graph -> Function -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfFunction = define "inferTypeOfFunction" $
  doc "Infer the type of a function (Either version)" $
  "fcx" ~> "cx" ~> "f" ~>
  cases _Function (var "f") Nothing [
    _Function_elimination>>: "elm" ~> inferTypeOfElimination @@ var "fcx" @@ var "cx" @@ var "elm",
    _Function_lambda>>: "l" ~> inferTypeOfLambda @@ var "fcx" @@ var "cx" @@ var "l",
    _Function_primitive>>: "name" ~> inferTypeOfPrimitive @@ var "fcx" @@ var "cx" @@ var "name"]

inferTypeOfInjection :: TTermDefinition (Context -> Graph -> Injection -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfInjection = define "inferTypeOfInjection" $
  doc "Infer the type of a union injection (Either version)" $
  "fcx" ~> "cx" ~> "injection" ~>
  "tname" <~ Core.injectionTypeName (var "injection") $
  "field" <~ Core.injectionField (var "injection") $
  "fname" <~ Core.fieldName (var "field") $
  "term" <~ Core.fieldTerm (var "field") $
  "result" <<~ inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "term" @@ (string "injected term") $
  "fcx2" <~ Typing.inferenceResultContext (var "result") $
  "stRp" <<~ Schemas.requireSchemaType @@ var "fcx2" @@ (Graph.graphSchemaTypes $ var "cx") @@ var "tname" $
  "schemaType" <~ Pairs.first (var "stRp") $
  "fcx3" <~ Pairs.second (var "stRp") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "ityp" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "sfields" <<~ ExtractCore.unionType @@ var "fcx3" @@ var "tname" @@ var "stype" $
  "ftyp" <<~ Schemas.findFieldType @@ var "fcx3" @@ var "fname" @@ var "sfields" $
  "mcResult" <<~ mapConstraints @@ var "fcx3" @@ var "cx" @@
    ("subst" ~> yield
      @@ var "fcx3"
      @@ (buildTypeApplicationTerm @@ var "svars"
        @@ (Core.termUnion $ Core.injection (var "tname") $ Core.field (var "fname") (var "iterm")))
      @@ (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      @@ (Substitution.composeTypeSubst @@ var "isubst" @@ var "subst")) @@
    list [Typing.typeConstraint (var "ftyp") (var "ityp") (string "schema type of injected field")] $
  right (var "mcResult")

inferTypeOfLambda :: TTermDefinition (Context -> Graph -> Lambda -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfLambda = define "inferTypeOfLambda" $
  doc "Infer the type of a lambda function (Either version)" $
  "fcx" ~> "cx" ~> "lambda" ~>
  "var" <~ Core.lambdaParameter (var "lambda") $
  "body" <~ Core.lambdaBody (var "lambda") $
  "vdomResult" <~ Schemas.freshName @@ var "fcx" $
  "vdom" <~ Pairs.first (var "vdomResult") $
  "fcx2" <~ Pairs.second (var "vdomResult") $
  "dom" <~ Core.typeVariable (var "vdom") $
  "cx2" <~ (extendContext @@ list [pair (var "var") (Core.typeScheme (list ([] :: [TTerm Name])) (var "dom") Phantoms.nothing)] @@ var "cx") $
  "result" <<~ inferTypeOfTerm @@ var "fcx2" @@ var "cx2" @@ var "body" @@ (string "lambda body") $
  "fcx3" <~ Typing.inferenceResultContext (var "result") $
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
  "iconstraints" <~ Substitution.substInClassConstraints @@ var "isubst" @@ (Typing.inferenceResultClassConstraints $ var "result") $
  right (Typing.inferenceResult (var "rterm") (var "rtype") (var "isubst") (var "iconstraints") (var "fcx3"))

inferTypeOfList :: TTermDefinition (Context -> Graph -> [Term] -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfList = define "inferTypeOfList" $
  doc "Infer the type of a list (Either version)" $
  "fcx" ~> "cx" ~> inferTypeOfCollection
    @@ var "fcx"
    @@ var "cx"
    @@ (unaryFunction Core.typeList)
    @@ (unaryFunction Core.termList)
    @@ (string "list element")
    @@ (Sets.empty :: TTerm (S.Set Name))

inferTypeOfMap :: TTermDefinition (Context -> Graph -> M.Map Term Term -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfMap = define "inferTypeOfMap" $
  doc "Infer the type of a map (Either version)" $
  "fcx" ~> "cx" ~> "m" ~>
  "kvarResult" <~ Schemas.freshName @@ var "fcx" $
  "kvar" <~ Pairs.first (var "kvarResult") $
  "fcx2" <~ Pairs.second (var "kvarResult") $
  "vvarResult" <~ Schemas.freshName @@ var "fcx2" $
  "vvar" <~ Pairs.first (var "vvarResult") $
  "fcx3" <~ Pairs.second (var "vvarResult") $
  "keyConstraints" <~ Maps.singleton (var "kvar") (Core.typeVariableMetadata $ Sets.singleton $ Core.nameLift _TypeClass_ordering) $
  Logic.ifElse (Maps.null $ var "m")
    (right (yieldWithConstraints
      @@ var "fcx3"
      @@ (buildTypeApplicationTerm
        @@ list [var "kvar", var "vvar"]
        @@ (Core.termMap Maps.empty))
      @@ (Core.typeMap $ Core.mapType (Core.typeVariable $ var "kvar") (Core.typeVariable $ var "vvar"))
      @@ Substitution.idTypeSubst
      @@ var "keyConstraints"))
    ("kRp" <<~ inferMany @@ var "fcx3" @@ var "cx" @@
      (Lists.map ("k" ~> pair (var "k") (string "map key")) $ Maps.keys $ var "m") $
    "kResults" <~ Pairs.first (var "kRp") $
    "fcx4" <~ Pairs.second (var "kRp") $
    "kterms" <~ Pairs.first (var "kResults") $
    "ktypes" <~ Pairs.first (Pairs.second $ var "kResults") $
    "ksubst" <~ Pairs.first (Pairs.second $ Pairs.second $ var "kResults") $
    "kElemConstraints" <~ Pairs.second (Pairs.second $ Pairs.second $ var "kResults") $
    "vRp" <<~ inferMany @@ var "fcx4" @@ (Substitution.substInContext @@ var "ksubst" @@ var "cx") @@
      (Lists.map ("v" ~> pair (var "v") (string "map value")) $ Maps.elems $ var "m") $
    "vResults" <~ Pairs.first (var "vRp") $
    "fcx5" <~ Pairs.second (var "vRp") $
    "vterms" <~ Pairs.first (var "vResults") $
    "vtypes" <~ Pairs.first (Pairs.second $ var "vResults") $
    "vsubst" <~ Pairs.first (Pairs.second $ Pairs.second $ var "vResults") $
    "vElemConstraints" <~ Pairs.second (Pairs.second $ Pairs.second $ var "vResults") $
    "kcons" <~ Lists.map ("t" ~> Typing.typeConstraint (Core.typeVariable $ var "kvar") (var "t") (string "map key")) (var "ktypes") $
    "vcons" <~ Lists.map ("t" ~> Typing.typeConstraint (Core.typeVariable $ var "vvar") (var "t") (string "map value")) (var "vtypes") $
    "allMapConstraints" <~ mergeClassConstraints @@ var "keyConstraints" @@ (mergeClassConstraints @@ var "kElemConstraints" @@ var "vElemConstraints") $
    "mcResult" <<~ mapConstraints @@ var "fcx5" @@ var "cx" @@
      ("subst" ~> yieldWithConstraints
        @@ var "fcx5"
        @@ (Core.termMap $ Maps.fromList $ Lists.zip (var "kterms") (var "vterms"))
        @@ (Core.typeMap $ Core.mapType (Core.typeVariable $ var "kvar") (Core.typeVariable $ var "vvar"))
        @@ (Substitution.composeTypeSubstList @@ list [var "ksubst", var "vsubst", var "subst"])
        @@ (Substitution.substInClassConstraints @@ var "subst" @@ var "allMapConstraints")) @@
      (Lists.concat $ list [var "kcons", var "vcons"]) $
    right (var "mcResult"))

inferTypeOfOptional :: TTermDefinition (Context -> Graph -> Maybe Term -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfOptional = define "inferTypeOfOptional" $
  doc "Infer the type of an optional (Either version)" $
  "fcx" ~> "cx" ~> "m" ~>
  "trmCons" <~ ("terms" ~> Logic.ifElse (Lists.null $ var "terms")
    (Core.termMaybe nothing)
    (Core.termMaybe $ just $ Lists.head $ var "terms")) $
  inferTypeOfCollection
    @@ var "fcx"
    @@ var "cx"
    @@ (unaryFunction Core.typeMaybe)
    @@ var "trmCons"
    @@ (string "optional element")
    @@ (Sets.empty :: TTerm (S.Set Name))
    @@ (Maybes.maybe (list ([] :: [TTerm Term])) (unaryFunction Lists.singleton) $ var "m")

inferTypeOfPair :: TTermDefinition (Context -> Graph -> (Term, Term) -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfPair = define "inferTypeOfPair" $
  doc "Infer the type of a pair (Either version)" $
  "fcx" ~> "cx" ~> "p" ~>
  "rp" <<~ inferMany @@ var "fcx" @@ var "cx" @@ list [
    pair (Pairs.first $ var "p") (string "pair first element"),
    pair (Pairs.second $ var "p") (string "pair second element")] $
  "results" <~ Pairs.first (var "rp") $
  "fcx2" <~ Pairs.second (var "rp") $
  "iterms" <~ Pairs.first (var "results") $
  "itypes" <~ Pairs.first (Pairs.second $ var "results") $
  "isubst" <~ Pairs.first (Pairs.second $ Pairs.second $ var "results") $
  "pairElemConstraints" <~ Pairs.second (Pairs.second $ Pairs.second $ var "results") $
  "ifst" <~ Lists.head (var "iterms") $
  "isnd" <~ Lists.head (Lists.tail $ var "iterms") $
  "tyFst" <~ Lists.head (var "itypes") $
  "tySnd" <~ Lists.head (Lists.tail $ var "itypes") $
  "pairTerm" <~ (Core.termPair $ pair (var "ifst") (var "isnd")) $
  "termWithTypes" <~ (Core.termTypeApplication $ Core.typeApplicationTerm
    (Core.termTypeApplication $ Core.typeApplicationTerm (var "pairTerm") (var "tyFst"))
    (var "tySnd")) $
  right (yieldWithConstraints
    @@ var "fcx2"
    @@ var "termWithTypes"
    @@ (Core.typePair $ Core.pairType (var "tyFst") (var "tySnd"))
    @@ var "isubst"
    @@ var "pairElemConstraints")

inferTypeOfPrimitive :: TTermDefinition (Context -> Graph -> Name -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfPrimitive = define "inferTypeOfPrimitive" $
  doc "Infer the type of a primitive function (Either version)" $
  "fcx" ~> "cx" ~> "name" ~>
  Maybes.maybe
    (Ctx.failInContext
      (Error.errorOther $ Error.otherError $ Strings.cat2 (string "No such primitive: ") (Core.unName $ var "name"))
      (var "fcx"))
    ("scheme" ~>
      "tsResult" <~ Schemas.instantiateTypeScheme @@ var "fcx" @@ var "scheme" $
      "ts" <~ Pairs.first (var "tsResult") $
      "fcx2" <~ Pairs.second (var "tsResult") $
      "constraints" <~ Maybes.fromMaybe Maps.empty (Core.typeSchemeConstraints $ var "ts") $
      right (yieldCheckedWithConstraints
        @@ var "fcx2"
        @@ (buildTypeApplicationTerm
          @@ Core.typeSchemeVariables (var "ts")
          @@ (Core.termFunction $ Core.functionPrimitive $ var "name"))
        @@ Core.typeSchemeType (var "ts")
        @@ Substitution.idTypeSubst
        @@ var "constraints"))
    (Maybes.map (unaryFunction Graph.primitiveType) $ Maps.lookup (var "name") (Graph.graphPrimitives $ var "cx"))

inferTypeOfProjection :: TTermDefinition (Context -> Graph -> Projection -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfProjection = define "inferTypeOfProjection" $
  doc "Infer the type of a record projection (Either version)" $
  "fcx" ~> "cx" ~> "proj" ~>
  "tname" <~ Core.projectionTypeName (var "proj") $
  "fname" <~ Core.projectionField (var "proj") $
  "stRp" <<~ Schemas.requireSchemaType @@ var "fcx" @@ (Graph.graphSchemaTypes $ var "cx") @@ var "tname" $
  "schemaType" <~ Pairs.first (var "stRp") $
  "fcx2" <~ Pairs.second (var "stRp") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ExtractCore.recordType @@ var "fcx2" @@ var "tname" @@ var "stype" $
  "ftyp" <<~ Schemas.findFieldType @@ var "fcx2" @@ var "fname" @@ var "sfields" $
  right (yield
    @@ var "fcx2"
    @@ (buildTypeApplicationTerm @@ var "svars"
      @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $ Core.projection (var "tname") (var "fname")))
    @@ (Core.typeFunction $ Core.functionType
      (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      (var "ftyp"))
    @@ Substitution.idTypeSubst)

inferTypeOfRecord :: TTermDefinition (Context -> Graph -> Record -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfRecord = define "inferTypeOfRecord" $
  doc "Infer the type of a record (Either version)" $
  "fcx" ~> "cx" ~> "record" ~>
  "tname" <~ Core.recordTypeName (var "record") $
  "fields" <~ Core.recordFields (var "record") $
  "fnames" <~ Lists.map (unaryFunction Core.fieldName) (var "fields") $
  "stRp" <<~ Schemas.requireSchemaType @@ var "fcx" @@ (Graph.graphSchemaTypes $ var "cx") @@ var "tname" $
  "schemaType" <~ Pairs.first (var "stRp") $
  "fcx2" <~ Pairs.second (var "stRp") $
  "rp" <<~ inferMany @@ var "fcx2" @@ var "cx" @@ Lists.map
    ("f" ~> pair
      (Core.fieldTerm $ var "f")
      (Strings.cat2 (string "field ") (Core.unName $ Core.fieldName $ var "f")))
    (var "fields") $
  "results" <~ Pairs.first (var "rp") $
  "fcx3" <~ Pairs.second (var "rp") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterms" <~ Pairs.first (var "results") $
  "itypes" <~ Pairs.first (Pairs.second $ var "results") $
  "isubst" <~ Pairs.first (Pairs.second $ Pairs.second $ var "results") $
  "recElemConstraints" <~ Pairs.second (Pairs.second $ Pairs.second $ var "results") $
  "ityp" <~ Core.typeRecord (
      Lists.zipWith ("n" ~> "t" ~> Core.fieldType (var "n") (var "t")) (var "fnames") (var "itypes")) $
  "mcResult" <<~ mapConstraints @@ var "fcx3" @@ var "cx" @@
    ("subst" ~> yieldWithConstraints
      @@ var "fcx3"
      @@ (buildTypeApplicationTerm @@ var "svars" @@
        (Core.termRecord $ Core.record (var "tname") $ Lists.zipWith
          ("n" ~> "t" ~> Core.field (var "n") (var "t"))
          (var "fnames")
          (var "iterms")))
      @@ (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      @@ (Substitution.composeTypeSubst @@ var "isubst" @@ var "subst")
      @@ (Substitution.substInClassConstraints @@ var "subst" @@ var "recElemConstraints")) @@
    list [Typing.typeConstraint (var "stype") (var "ityp") (string "schema type of record")] $
  right (var "mcResult")

inferTypeOfSet :: TTermDefinition (Context -> Graph -> S.Set Term -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfSet = define "inferTypeOfSet" $
  doc "Infer the type of a set (Either version)" $
  "fcx" ~> "cx" ~>
  "s" ~>
  inferTypeOfCollection
    @@ var "fcx"
    @@ var "cx"
    @@ (unaryFunction Core.typeSet)
    @@ ("terms" ~> Core.termSet $ Sets.fromList $ var "terms")
    @@ (string "set element")
    @@ (Sets.singleton $ Core.nameLift _TypeClass_ordering)
    @@ (Sets.toList $ var "s")

inferTypeOfTerm :: TTermDefinition (Context -> Graph -> Term -> String -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfTerm = define "inferTypeOfTerm" $
  doc "Infer the type of a given term (Either version)" $
  "fcx" ~> "cx" ~> "term" ~> "desc" ~>
  "fcx2" <~ Ctx.pushTrace (var "desc") (var "fcx") $
  cases _Term (var "term") Nothing [
    _Term_annotated>>: "a" ~> inferTypeOfAnnotatedTerm @@ var "fcx2" @@ var "cx" @@ var "a",
    _Term_application>>: "a" ~> inferTypeOfApplication @@ var "fcx2" @@ var "cx" @@ var "a",
    _Term_either>>: "e" ~> inferTypeOfEither @@ var "fcx2" @@ var "cx" @@ var "e",
    _Term_function>>: "f" ~> inferTypeOfFunction @@ var "fcx2" @@ var "cx" @@ var "f",
    _Term_let>>: "l" ~> inferTypeOfLet @@ var "fcx2" @@ var "cx" @@ var "l",
    _Term_list>>: "els" ~> inferTypeOfList @@ var "fcx2" @@ var "cx" @@ var "els",
    _Term_literal>>: "l" ~> right (inferTypeOfLiteral @@ var "fcx2" @@ var "l"),
    _Term_map>>: "m" ~> inferTypeOfMap @@ var "fcx2" @@ var "cx" @@ var "m",
    _Term_maybe>>: "m" ~> inferTypeOfOptional @@ var "fcx2" @@ var "cx" @@ var "m",
    _Term_pair>>: "p" ~> inferTypeOfPair @@ var "fcx2" @@ var "cx" @@ var "p",
    _Term_record>>: "r" ~> inferTypeOfRecord @@ var "fcx2" @@ var "cx" @@ var "r",
    _Term_set>>: "s" ~> inferTypeOfSet @@ var "fcx2" @@ var "cx" @@ var "s",
    _Term_typeApplication>>: "tt" ~> inferTypeOfTypeApplication @@ var "fcx2" @@ var "cx" @@ var "tt",
    _Term_typeLambda>>: "ta" ~> inferTypeOfTypeLambda @@ var "fcx2" @@ var "cx" @@ var "ta",
    _Term_union>>: "i" ~> inferTypeOfInjection @@ var "fcx2" @@ var "cx" @@ var "i",
    _Term_unit>>: constant $ right (inferTypeOfUnit @@ var "fcx2"),
    _Term_variable>>: "name" ~> inferTypeOfVariable @@ var "fcx2" @@ var "cx" @@ var "name",
    _Term_wrap>>: "w" ~> inferTypeOfWrappedTerm @@ var "fcx2" @@ var "cx" @@ var "w"]

inferTypeOfTypeLambda :: TTermDefinition (Context -> Graph -> TypeLambda -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfTypeLambda = define "inferTypeOfTypeLambda" $
  doc "Infer the type of a type abstraction (Either version)" $
  "fcx" ~> "cx" ~> "ta" ~>
  inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ (Core.typeLambdaBody $ var "ta") @@ (string "type abstraction")

inferTypeOfTypeApplication :: TTermDefinition (Context -> Graph -> TypeApplicationTerm -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfTypeApplication = define "inferTypeOfTypeApplication" $
  doc "Infer the type of a type application (Either version)" $
  "fcx" ~> "cx" ~> "tt" ~>
  inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ (Core.typeApplicationTermBody $ var "tt") @@ (string "type application term")

inferTypeOfUnwrap :: TTermDefinition (Context -> Graph -> Name -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfUnwrap = define "inferTypeOfUnwrap" $
  doc "Infer the type of an unwrap operation (Either version)" $
  "fcx" ~> "cx" ~> "tname" ~>
  "stRp" <<~ Schemas.requireSchemaType @@ var "fcx" @@ (Graph.graphSchemaTypes $ var "cx") @@ var "tname" $
  "schemaType" <~ Pairs.first (var "stRp") $
  "fcx2" <~ Pairs.second (var "stRp") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "wtyp" <<~ ExtractCore.wrappedType @@ var "fcx2" @@ var "tname" @@ var "stype" $
  right (yield
    @@ var "fcx2"
    @@ (buildTypeApplicationTerm @@ var "svars"
      @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationWrap $ var "tname"))
    @@ (Core.typeFunction $ Core.functionType
      (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      (var "wtyp"))
    @@ Substitution.idTypeSubst)

inferTypeOfVariable :: TTermDefinition (Context -> Graph -> Name -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfVariable = define "inferTypeOfVariable" $
  doc "Infer the type of a variable (Either version)" $
  "fcx" ~> "cx" ~> "name" ~>
  Maybes.maybe
    (Ctx.failInContext
      (Error.errorOther $ Error.otherError $ Strings.cat2 (string "Variable not bound to type: ") (Core.unName $ var "name"))
      (var "fcx"))
    ("scheme" ~>
      "tsResult" <~ Schemas.instantiateTypeScheme @@ var "fcx" @@ var "scheme" $
      "ts" <~ Pairs.first (var "tsResult") $
      "fcx2" <~ Pairs.second (var "tsResult") $
      "constraints" <~ Maybes.fromMaybe Maps.empty (Core.typeSchemeConstraints $ var "ts") $
      right (Typing.inferenceResult
        (buildTypeApplicationTerm
          @@ Core.typeSchemeVariables (var "ts")
          @@ Core.termVariable (var "name"))
        (Core.typeSchemeType $ var "ts")
        (asTerm Substitution.idTypeSubst)
        (var "constraints")
        (var "fcx2")))
    (Maps.lookup (var "name") (Graph.graphBoundTypes $ var "cx"))

inferTypeOfWrappedTerm :: TTermDefinition (Context -> Graph -> WrappedTerm -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfWrappedTerm = define "inferTypeOfWrappedTerm" $
  doc "Infer the type of a wrapped term (Either version)" $
  "fcx" ~> "cx" ~> "wt" ~>
  "tname" <~ Core.wrappedTermTypeName (var "wt") $
  "term" <~ Core.wrappedTermBody (var "wt") $
  "stRp" <<~ Schemas.requireSchemaType @@ var "fcx" @@ (Graph.graphSchemaTypes $ var "cx") @@ var "tname" $
  "schemaType" <~ Pairs.first (var "stRp") $
  "fcx2" <~ Pairs.second (var "stRp") $
  "result" <<~ inferTypeOfTerm @@ var "fcx2" @@ var "cx" @@ var "term" @@ (string "wrapped term") $
  "fcx3" <~ Typing.inferenceResultContext (var "result") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "itype" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "ityp" <~ Core.typeWrap (var "itype") $
  "mcResult" <<~ mapConstraints @@ var "fcx3" @@ var "cx"
    @@ ("subst" ~> yield
      @@ var "fcx3"
      @@ (buildTypeApplicationTerm @@ var "svars" @@ (Core.termWrap $ Core.wrappedTerm (var "tname") (var "iterm")))
      @@ (Schemas.nominalApplication @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      @@ (Substitution.composeTypeSubst @@ var "isubst" @@ var "subst"))
    @@ list [Typing.typeConstraint (var "stype") (var "ityp") (string "schema type of wrapper")] $
  right (var "mcResult")

inferTypeOfLet :: TTermDefinition (Context -> Graph -> Let -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfLet = define "inferTypeOfLet" $
  doc "Normalize a let term before inferring its type (Either version)" $
  "fcx0" ~> "cx" ~> "let0" ~>
  "fcx" <~ Ctx.pushTrace (string "let") (var "fcx0") $
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
  "rewrittenLet" <~ Lists.foldl (var "createLet") (var "body0") (Lists.reverse $ var "groups") $
  "restoreLet" <~ ("iterm" ~>
    "helper" <~ ("level" ~> "bins" ~> "term" ~>
      "nonzero" <~ ("term" ~> cases _Term (var "term") Nothing [
        _Term_let>>: "l" ~>
          "bs" <~ Core.letBindings (var "l") $
          "letBody" <~ Core.letBody (var "l") $
          var "helper" @@
            (Math.sub (var "level") (int32 1)) @@
            (Lists.concat $ list [var "bs", var "bins"]) @@
            (var "letBody")]) $
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
  "rewriteResult" <~ ("iresult" ~>
    "fcxR" <~ Typing.inferenceResultContext (var "iresult") $
    "iterm" <~ Typing.inferenceResultTerm (var "iresult") $
    "itype" <~ Typing.inferenceResultType (var "iresult") $
    "isubst" <~ Typing.inferenceResultSubst (var "iresult") $
    "iconstraints" <~ Typing.inferenceResultClassConstraints (var "iresult") $
    Typing.inferenceResult (var "restoreLet" @@ var "iterm") (var "itype") (var "isubst") (var "iconstraints") (var "fcxR")) $
  "res" <~ (cases _Term (var "rewrittenLet")
     (Just $ inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "rewrittenLet" @@ (string "empty let term")) [
     _Term_let>>: "l" ~> inferTypeOfLetNormalized @@ var "fcx" @@ var "cx" @@ var "l"]) $
  Eithers.map (var "rewriteResult") (var "res")

inferTypeOfLetNormalized :: TTermDefinition (Context -> Graph -> Let -> Prelude.Either (InContext Error) InferenceResult)
inferTypeOfLetNormalized = define "inferTypeOfLetNormalized" $
  doc "Infer the type of a let (letrec) term which is already in a normal form (Either version)" $
  "fcx0" ~> "cx0" ~> "letTerm" ~>
  "fcx" <~ Ctx.pushTrace (string "let-normalized") (var "fcx0") $
  "bins0" <~ Core.letBindings (var "letTerm") $
  "body0" <~ Core.letBody (var "letTerm") $
  "bnames" <~ Lists.map (unaryFunction Core.bindingName) (var "bins0") $

  -- Phase 1: Create fresh temporary type variables
  "bvarsResult" <~ Schemas.freshNames @@ (Lists.length $ var "bins0") @@ var "fcx" $
  "bvars" <~ Pairs.first (var "bvarsResult") $
  "fcx2" <~ Pairs.second (var "bvarsResult") $
  "tbins0" <~ Lists.map (unaryFunction Core.typeVariable) (var "bvars") $

  "cx1" <~ (extendContext
    @@ (Lists.zip (var "bnames") $ Lists.map ("t" ~> Core.typeScheme (list ([] :: [TTerm Name])) (var "t") Phantoms.nothing) (var "tbins0"))
    @@ (var "cx0")) $

  -- Phase 2: Infer actual types
  "irRp" <<~ inferTypesOfTemporaryBindings @@ var "fcx2" @@ var "cx1" @@ var "bins0" $
  "inferredResult" <~ Pairs.first (var "irRp") $
  "fcx3" <~ Pairs.second (var "irRp") $
  "bterms1" <~ Pairs.first (var "inferredResult") $
  "tbins1" <~ Pairs.first (Pairs.second $ var "inferredResult") $
  "substAndConstraints" <~ Pairs.second (Pairs.second $ var "inferredResult") $
  "s1" <~ Pairs.first (var "substAndConstraints") $
  "inferredConstraints" <~ Pairs.second (var "substAndConstraints") $

  -- Phase 3: Unify temporary types with actual inferred types
  "s2" <<~ Eithers.bimap
    ("_ic" ~> Ctx.inContext (Error.errorOther $ Error.otherError (Error.unificationErrorMessage (Ctx.inContextObject (var "_ic")))) (Ctx.inContextContext (var "_ic")))
    ("_a" ~> var "_a")
    (Unification.unifyTypeLists
    @@ var "fcx3"
    @@ (Graph.graphSchemaTypes $ var "cx0")
    @@ (Lists.map (Substitution.substInType @@ var "s1") (var "tbins0"))
    @@ (var "tbins1")
    @@ (string "temporary type bindings")) $
  "_" <<~ Checking.checkTypeSubst @@ var "fcx3" @@ var "cx0" @@ var "s2" $

  "g2base" <~ (Substitution.substInContext @@
    (Substitution.composeTypeSubst @@ var "s1" @@ var "s2") @@
    (var "cx0")) $
  "constraintsWithS2" <~ Substitution.substInClassConstraints @@ var "s2" @@ var "inferredConstraints" $

  "composedSubst" <~ Substitution.composeTypeSubst @@ var "s1" @@ var "s2" $
  "originalBindingConstraints" <~ Lists.foldl
    ("acc" ~> "b" ~>
      Maybes.maybe
        (var "acc")
        ("ts" ~> Maybes.maybe
          (var "acc")
          ("c" ~> mergeClassConstraints @@ var "acc" @@ var "c")
          (Core.typeSchemeConstraints $ var "ts"))
        (Core.bindingType $ var "b"))
    Maps.empty
    (var "bins0") $
  "originalConstraintsSubst" <~ Substitution.substInClassConstraints @@ var "composedSubst" @@ var "originalBindingConstraints" $

  "allInferredConstraints" <~ mergeClassConstraints @@ var "constraintsWithS2" @@ var "originalConstraintsSubst" $
  "mergedConstraints" <~ mergeClassConstraints @@ (Graph.graphClassConstraints $ var "g2base") @@ var "allInferredConstraints" $
  "g2" <~ Graph.graphWithClassConstraints (var "g2base") (var "mergedConstraints") $

  "bterms1Subst" <~ Lists.map (Substitution.substTypesInTerm @@ var "s2") (var "bterms1") $

  -- Phase 4: Generalize
  "tsbins1" <~ (Lists.zip (var "bnames") $
    Lists.map ("t" ~> generalize @@ var "g2" @@
      (Substitution.substInType @@ var "s2" @@ var "t")) (var "tbins1")) $

  -- Phase 5: Infer body type
  "bodyResult" <<~ inferTypeOfTerm @@ var "fcx3" @@
    (extendContext @@ var "tsbins1" @@ var "g2") @@
    (var "body0") @@
    (string "let body") $
  "fcx4" <~ Typing.inferenceResultContext (var "bodyResult") $
  "body1" <~ Typing.inferenceResultTerm (var "bodyResult") $
  "tbody" <~ Typing.inferenceResultType (var "bodyResult") $
  "sbody" <~ Typing.inferenceResultSubst (var "bodyResult") $

  -- Phase 6: Create term substitution for polymorphic instantiation
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

  -- Phase 7: Create final bindings with type lambdas
  "createBinding" <~ ("bindingPair" ~>
    "nameTsPair" <~ Pairs.first (var "bindingPair") $
    "term" <~ Pairs.second (var "bindingPair") $
    "name" <~ Pairs.first (var "nameTsPair") $
    "ts" <~ Pairs.second (var "nameTsPair") $
    "finalTs" <~ Substitution.substInTypeScheme @@ var "sbody" @@ var "ts" $
    "typeLambdaTerm" <~ Lists.foldl
      ("b" ~> "v" ~> Core.termTypeLambda $ Core.typeLambda (var "v") (var "b"))
      (Substitution.substituteInTerm @@ var "st1" @@ var "term")
      (Lists.reverse $ Core.typeSchemeVariables $ var "finalTs") $
    Core.binding (var "name")
      (Substitution.substTypesInTerm @@
        (Substitution.composeTypeSubst @@ var "sbody" @@ var "s2") @@
        (var "typeLambdaTerm"))
      (just $ var "finalTs")) $

  "bins1" <~ (Lists.map (var "createBinding") $
    Lists.zip (var "tsbins1") (var "bterms1Subst")) $

  "bodyConstraints" <~ Substitution.substInClassConstraints @@ var "sbody" @@ (Typing.inferenceResultClassConstraints $ var "bodyResult") $
  "bindingConstraintsSubst" <~ Substitution.substInClassConstraints @@ var "sbody" @@ var "constraintsWithS2" $
  "allConstraints" <~ mergeClassConstraints @@ var "bindingConstraintsSubst" @@ var "bodyConstraints" $

  right (Typing.inferenceResult
    (Core.termLet $ Core.let_ (var "bins1") (var "body1"))
    (var "tbody")
    (Substitution.composeTypeSubstList @@ list [var "s1", var "s2", var "sbody"])
    (var "allConstraints")
    (var "fcx4"))

inferTypesOfTemporaryBindings :: TTermDefinition (Context -> Graph -> [Binding] -> Prelude.Either (InContext Error) (([Term], ([Type], (TypeSubst, M.Map Name TypeVariableMetadata))), Context))
inferTypesOfTemporaryBindings = define "inferTypesOfTemporaryBindings" $
  doc "Infer types for temporary let bindings (Either version)" $
  "fcx" ~> "cx" ~> "bins" ~>
  Logic.ifElse (Lists.null $ var "bins")
    (right $ pair (pair (list ([] :: [TTerm Term])) (pair (list ([] :: [TTerm Type])) (pair (Substitution.idTypeSubst) Maps.empty))) (var "fcx"))
    ("dflt" <~ (
    "binding" <~ Lists.head (var "bins") $
    "k" <~ Core.bindingName (var "binding") $
    "v" <~ Core.bindingTerm (var "binding") $
    "tl" <~ Lists.tail (var "bins") $
    "result1" <<~ inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "v" @@
      (Strings.cat $ list [
        (string "temporary let binding '"),
        Core.unName $ var "k",
        (string "'")]) $
    "fcx2" <~ Typing.inferenceResultContext (var "result1") $
    "j" <~ Typing.inferenceResultTerm (var "result1") $
    "u_prime" <~ Typing.inferenceResultType (var "result1") $
    "u" <~ Typing.inferenceResultSubst (var "result1") $
    "c1Inferred" <~ Typing.inferenceResultClassConstraints (var "result1") $

    -- Extract constraints from the original binding's TypeScheme
    "originalBindingConstraints" <<~ Maybes.maybe
      (right Maps.empty)
      ("ts" ~>
        "tsResult" <~ Schemas.instantiateTypeScheme @@ var "fcx2" @@ var "ts" $
        "instantiatedTs" <~ Pairs.first (var "tsResult") $
        "freshConstraints" <~ Maybes.fromMaybe Maps.empty (Core.typeSchemeConstraints $ var "instantiatedTs") $
        -- Unify the instantiated type with the inferred type to map fresh variables
        "unifySubst" <<~ Eithers.bimap
          ("_ic" ~> Ctx.inContext (Error.errorOther $ Error.otherError (Error.unificationErrorMessage (Ctx.inContextObject (var "_ic")))) (Ctx.inContextContext (var "_ic")))
          ("_a" ~> var "_a")
          (Unification.unifyTypes
            @@ var "fcx2"
            @@ (Graph.graphSchemaTypes $ var "cx")
            @@ (Core.typeSchemeType $ var "instantiatedTs")
            @@ var "u_prime"
            @@ string "original binding type") $
        right (Substitution.substInClassConstraints @@ var "unifySubst" @@ var "freshConstraints"))
      (Core.bindingType $ var "binding") $

    "c1" <~ mergeClassConstraints @@ var "c1Inferred" @@ var "originalBindingConstraints" $

    "rp2" <<~ inferTypesOfTemporaryBindings @@ var "fcx2" @@
      (Substitution.substInContext @@ var "u" @@ var "cx") @@
      var "tl" $
    "result2" <~ Pairs.first (var "rp2") $
    "fcx3" <~ Pairs.second (var "rp2") $
    "h" <~ Pairs.first (var "result2") $
    "r_prime" <~ Pairs.first (Pairs.second $ var "result2") $
    "restPair" <~ Pairs.second (Pairs.second $ var "result2") $
    "r" <~ Pairs.first (var "restPair") $
    "c2" <~ Pairs.second (var "restPair") $
    "c1Subst" <~ Substitution.substInClassConstraints @@ var "r" @@ var "c1" $
    "mergedConstraints" <~ mergeClassConstraints @@ var "c1Subst" @@ var "c2" $
    right $ pair (pair
      (Lists.cons (Substitution.substTypesInTerm @@ var "r" @@ var "j") (var "h"))
      (pair
        (Lists.cons (Substitution.substInType @@ var "r" @@ var "u_prime") (var "r_prime"))
        (pair (Substitution.composeTypeSubst @@ var "u" @@ var "r") (var "mergedConstraints")))) (var "fcx3")) $
  var "dflt")

inferMany :: TTermDefinition (Context -> Graph -> [(Term, String)] -> Prelude.Either (InContext Error) (([Term], ([Type], (TypeSubst, M.Map Name TypeVariableMetadata))), Context))
inferMany = define "inferMany" $
  doc "Infer types for multiple terms, propagating class constraints from sub-expressions" $
  "fcx" ~> "cx" ~> "pairs" ~>
  Logic.ifElse (Lists.null $ var "pairs")
    (right $ pair (pair (list ([] :: [TTerm Term])) $ pair (list ([] :: [TTerm Type])) (pair Substitution.idTypeSubst Maps.empty)) (var "fcx"))
    ("dflt" <~ (
    "e" <~ Pairs.first (Lists.head $ var "pairs") $
    "desc" <~ Pairs.second (Lists.head $ var "pairs") $
    "tl" <~ Lists.tail (var "pairs") $
    "result1" <<~ inferTypeOfTerm @@ var "fcx" @@ var "cx" @@ var "e" @@ var "desc" $
    "fcx2" <~ Typing.inferenceResultContext (var "result1") $
    "e1" <~ Typing.inferenceResultTerm (var "result1") $
    "t1" <~ Typing.inferenceResultType (var "result1") $
    "s1" <~ Typing.inferenceResultSubst (var "result1") $
    "c1" <~ Typing.inferenceResultClassConstraints (var "result1") $
    "rp2" <<~ inferMany @@ var "fcx2" @@ (Substitution.substInContext @@ var "s1" @@ var "cx") @@ var "tl" $
    "result2" <~ Pairs.first (var "rp2") $
    "fcx3" <~ Pairs.second (var "rp2") $
    "e2" <~ Pairs.first (var "result2") $
    "t2" <~ Pairs.first (Pairs.second $ var "result2") $
    "s2" <~ Pairs.first (Pairs.second $ Pairs.second $ var "result2") $
    "c2" <~ Pairs.second (Pairs.second $ Pairs.second $ var "result2") $
    "c1Subst" <~ Substitution.substInClassConstraints @@ var "s2" @@ var "c1" $
    "mergedConstraints" <~ mergeClassConstraints @@ var "c1Subst" @@ var "c2" $
    right $ pair (pair
      (Lists.cons (Substitution.substTypesInTerm @@ var "s2" @@ var "e1") (var "e2"))
      (pair
        (Lists.cons (Substitution.substInType @@ var "s2" @@ var "t1") (var "t2"))
        (pair (Substitution.composeTypeSubst @@ var "s1" @@ var "s2") (var "mergedConstraints")))) (var "fcx3")) $
  var "dflt")

yieldDebug :: TTermDefinition (Context -> Graph -> String -> Term -> Type -> TypeSubst -> Prelude.Either (InContext Error) InferenceResult)
yieldDebug = define "yieldDebug" $
  doc "Create an inference result with debug output" $
  "fcx" ~> "cx" ~> "debugId" ~> "term" ~> "typ" ~> "subst" ~>
  "rterm" <~ Substitution.substTypesInTerm @@ var "subst" @@ var "term" $
  "rtyp" <~ Substitution.substInType @@ var "subst" @@ var "typ" $
  "result" <<~ Annotations.debugIf @@ var "fcx" @@ var "debugId" @@
    (Strings.cat $ list [
      (string "\n\tterm: "),  ShowCore.term @@ var "term",
      (string "\n\ttyp: "),   ShowCore.type_ @@ var "typ",
      (string "\n\tsubst: "), ShowTyping.typeSubst @@ var "subst",
      (string "\n\trterm: "), ShowCore.term @@ var "rterm",
      (string "\n\trtyp: "),  ShowCore.type_ @@ var "rtyp"]) $
  right (Typing.inferenceResult (var "rterm") (var "rtyp") (var "subst") Maps.empty (var "fcx"))
