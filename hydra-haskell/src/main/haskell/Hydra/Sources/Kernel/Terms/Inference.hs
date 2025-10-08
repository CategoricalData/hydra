{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Inference where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Mantle as ShowMantle
import qualified Hydra.Sources.Kernel.Terms.Show.Typing as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting as Sorting
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution
import qualified Hydra.Sources.Kernel.Terms.Unification as Unification
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.inference") elements
    [Annotations.module_, Lexical.module_, Schemas.module_, Unification.module_,
      ShowCore.module_, ShowGraph.module_, ShowMantle.module_, ShowTyping.module_]
    kernelTypesModules $
    Just "Type inference following Algorithm W, extended for nominal terms and types"
  where
    elements = [
      el applyTypesDef,
      el bindConstraintsDef,
      el bindUnboundTypeVariablesDef,
      el checkForUnboundTypeVariablesDef,
      el checkSameTypeDef,
      el checkTypeDef,
      el checkTypeSubstDef, -- TODO: use this
      el checkTypeVariablesDef,
      el debugInferenceDef,
      el emptyInferenceContextDef,
      el extendContextDef,
      el finalizeInferredTermDef,
      el forInferredTermDef,
      el freeVariablesInContextDef,
      el freshNameDef,
      el freshNamesDef,
      el freshVariableTypeDef,
      el generalizeDef,
      el graphToInferenceContextDef,
      el inferGraphTypesDef,
      el inferInGraphContextDef,
      el inferManyDef,
      el inferTypeOfAnnotatedTermDef,
      el inferTypeOfApplicationDef,
      el inferTypeOfCaseStatementDef,
      el inferTypeOfCollectionDef,
      el inferTypeOfDef,
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
      el inferTypeOfPrimitiveDef,
      el inferTypeOfProductDef,
      el inferTypeOfProjectionDef,
      el inferTypeOfRecordDef,
      el inferTypeOfSetDef,
      el inferTypeOfSumDef,
      el inferTypeOfTermDef,
      el inferTypeOfTupleProjectionDef,
      el inferTypeOfTypeLambdaDef,
      el inferTypeOfTypeApplicationDef,
      el inferTypeOfUnwrapDef,
      el inferTypeOfVariableDef,
      el inferTypeOfWrappedTermDef,
      el inferTypesOfTemporaryBindingsDef,
      el initialTypeContextDef,
      el instantiateTypeSchemeDef,
      el isUnboundDef,
      el key_vcountDef,
      el mapConstraintsDef,
      el nominalApplicationDef,
      el normalTypeVariableDef,
      el requireSchemaTypeDef,
      el showInferenceResultDef,
      el toFContextDef,
      el typeOfDef,
      el typeOfInternalDef,
      el typeOfNominalDef,
      el typeOfUnitDef,
      el unboundTypeVariablesInTermDef,
      el yieldDef,
      el yieldCheckedDef,
      el yieldDebugDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

--

applyTypesDef :: TBinding (Term -> [Name] -> Term)
applyTypesDef = define "applyTypes" $
  doc "Apply type variables to a term by wrapping it with type applications" $
  "term" ~> "vars" ~>
  Lists.foldl
    ("t" ~> "v" ~> Core.termTypeApplication $ Core.typedTerm (var "t") (Core.typeVariable $ var "v"))
    (var "term")
    (var "vars")

bindConstraintsDef :: TBinding (InferenceContext -> (TypeSubst -> Flow s a) -> [TypeConstraint] -> Flow s a)
bindConstraintsDef = define "bindConstraints" $
  doc "Bind type constraints and continue with substitution" $
  "cx" ~> "f" ~> "constraints" ~>
  "s" <<~ ref Unification.unifyTypeConstraintsDef @@ Typing.inferenceContextSchemaTypes (var "cx") @@ var "constraints" $
  exec (ref checkTypeSubstDef @@ var "cx" @@ var "s") $
  var "f" @@ var "s"

bindUnboundTypeVariablesDef :: TBinding (InferenceContext -> Term -> Term)
bindUnboundTypeVariablesDef = define "bindUnboundTypeVariables" $
  doc ("Place unbound type variables appearing anywhere under a typed let binding in the type scheme of that binding."
    <> " These variables may appear in the binding type scheme itself or in that of a subterm,"
    <> " in domain types attached to functions, and in type abstraction and type application terms."
    <> " This process attempts to capture type variables which have escaped unification, e.g. due to unused code."
    <> " However, unbound type variables not appearing beneath any typed let binding remain unbound.") $
  "cx" ~> "term0" ~>
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
            "unboundInTerm" <~ ref unboundTypeVariablesInTermDef @@ var "cx" @@ var "bterm" $
            "unbound" <~ Sets.difference (Sets.union (var "unboundInType") (var "unboundInTerm")) (var "bvars") $
            "ts2" <~ Core.typeScheme
              (Lists.concat2
                (Core.typeSchemeVariables $ var "ts")
                (Sets.toList $ var "unbound"))
              (Core.typeSchemeType $ var "ts") $
            Core.binding (var "bname") (var "bterm") (just $ var "ts2"))) $
      Core.termLet $ Core.let_
        (Lists.map (var "forBinding") (Core.letBindings $ var "l"))
        (ref bindUnboundTypeVariablesDef @@ var "cx" @@ (Core.letEnvironment $ var "l"))]) $
  ref Rewriting.rewriteTermDef @@ var "rewrite" @@ var "term0"

checkForUnboundTypeVariablesDef :: TBinding (InferenceContext -> Term -> Flow s ())
checkForUnboundTypeVariablesDef = define "checkForUnboundTypeVariables" $
  doc "Check that a term has no unbound type variables" $
  "cx" ~> "term0" ~>
  "svars" <~ Sets.fromList (Maps.keys $ Typing.inferenceContextSchemaTypes $ var "cx") $
  "checkRecursive" <~ ("vars" ~> "trace" ~> "lbinding" ~> "term" ~>
    "recurse" <~ var "checkRecursive" @@ var "vars" @@ var "trace" @@ var "lbinding" $
    "dflt" <~ (
      exec (Flows.mapList (var "recurse") (ref Rewriting.subtermsDef @@ var "term")) $
      produce unit) $
    "check" <~ ("typ" ~>
      "freevars" <~ ref Rewriting.freeVariablesInTypeDef @@ var "typ" $
      "badvars" <~ Sets.difference (Sets.difference (var "freevars") (var "vars")) (var "svars") $
      Logic.ifElse (Sets.null $ var "badvars")
        (produce unit)
        (Flows.fail $ "unbound type variables: {"
          ++ (Strings.intercalate ", " (Lists.map (unaryFunction Core.unName) $ Sets.toList $ var "badvars"))
          ++ "} in type " ++ (ref ShowCore.typeDef @@ var "typ") ++ " at path: " ++ (Strings.intercalate " >> " (Lists.reverse $ var "trace"))
          ++ (optCases (var "lbinding")
            ("none")
            ("binding" ~>
                 ". bound term = " ++ (ref ShowCore.termDef @@ (Core.bindingTerm $ var "binding"))
              ++ ". bound type = " ++ (optCases (Core.bindingType $ var "binding") ("none") (ref ShowCore.typeSchemeDef))))
--          ++ ". full term = " ++ (ref ShowCore.termDef @@ var "term0")
--          ++ ". vars = {" ++ (Strings.intercalate ", " (Lists.map (unaryFunction Core.unName) $ Sets.toList $ var "vars")) ++ "}"
          )) $
    "checkOptional" <~ ("m" ~>
      exec (Flows.mapOptional (var "check") (var "m")) $
      produce unit) $
    "checkOptionalList" <~ ("ml" ~>
      exec (Flows.mapOptional ("l" ~> Flows.mapList (var "check") (var "l")) (var "ml")) $
      produce unit) $
    cases _Term (var "term")
      (Just $ var "dflt") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "dflt") [
        _Function_elimination>>: "e" ~> cases _Elimination (var "e")
          (Just $ var "dflt") [
          _Elimination_product>>: "tp" ~> var "checkOptionalList" @@ (Core.tupleProjectionDomain $ var "tp")],
        _Function_lambda>>: "l" ~>
          exec (var "checkOptional" @@ (Core.lambdaDomain $ var "l")) $
          var "recurse" @@ (Core.lambdaBody $ var "l")],
      _Term_let>>: "l" ~>
        "forBinding" <~ ("b" ~>
          "bterm" <~ Core.bindingTerm (var "b") $
          "newVars" <~ optCases (Core.bindingType $ var "b")
             (var "vars")
             ("ts" ~> Sets.union (var "vars") (Sets.fromList $ Core.typeSchemeVariables $ var "ts")) $
          "newTrace" <~ Lists.cons (Core.unName $ Core.bindingName $ var "b") (var "trace") $
          var "checkRecursive" @@ var "newVars" @@ var "newTrace" @@ (just $ var "b") @@ var "bterm") $
        exec (Flows.mapList (var "forBinding") $ Core.letBindings $ var "l") $
        var "recurse" @@ (Core.letEnvironment $ var "l"),
      _Term_typeApplication>>: "tt" ~>
        exec (var "check" @@ (Core.typedTermType $ var "tt")) $
        var "recurse" @@ (Core.typedTermTerm $ var "tt"),
      _Term_typeLambda>>: "tl" ~>
        exec (var "check" @@ (Core.typeVariable $ Core.typeLambdaParameter $ var "tl")) $
        var "recurse" @@ (Core.typeLambdaBody $ var "tl")]) $
  var "checkRecursive" @@ Sets.empty @@ list ["top level"] @@ nothing @@ var "term0"

checkSameTypeDef :: TBinding (String -> [Type] -> Flow s Type)
checkSameTypeDef = define "checkSameType" $
  doc "Ensure all types in a list are equal and return the common type" $
  "desc" ~> "types" ~>
  "h" <~ Lists.head (var "types") $
  "allEqual" <~ Lists.foldl
    ("b" ~> "t" ~> Logic.and (var "b") (Equality.equal (var "t") (var "h")))
    true
    (var "types") $
  Logic.ifElse (var "allEqual")
    (Flows.pure $ var "h")
    (Flows.fail $ Strings.cat $ list [
      string "unequal types ",
      (ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "types"),
      string " in ",
      var "desc"])

checkTypeDef :: TBinding (S.Set Name -> InferenceContext -> Type -> Term -> Flow s ())
checkTypeDef = define "checkType" $
  doc "Check that a term has the expected type" $
  "k" ~> "g" ~> "t" ~> "e" ~>
  Logic.ifElse (ref debugInferenceDef)
    ("t0" <<~ ref typeOfInternalDef @@ var "g" @@ var "k" @@ (ref toFContextDef @@ var "g") @@ list [] @@ var "e" $
      Logic.ifElse (Equality.equal (var "t0") (var "t"))
        (Flows.pure unit)
        (Flows.fail $ Strings.cat $ list [
          string "type checking failed: expected ",
          ref ShowCore.typeDef @@ var "t",
          string " but found ",
          ref ShowCore.typeDef @@ var "t0"]))
    (Flows.pure unit)

checkTypeSubstDef :: TBinding (InferenceContext -> TypeSubst -> Flow s TypeSubst)
checkTypeSubstDef = define "checkTypeSubst" $
  doc ("Sanity-check a type substitution arising from unification. Specifically, check that schema types have not been"
    <> " inappropriately unified with type variables inferred from terms.") $
  "cx" ~> "subst" ~>
  "s" <~ Typing.unTypeSubst (var "subst") $
  "vars" <~ Sets.fromList (Maps.keys $ var "s") $
  "suspectVars" <~ Sets.intersection (var "vars") (Sets.fromList $ Maps.keys $ Typing.inferenceContextSchemaTypes $ var "cx") $
  "isTypeAlias" <~ ("ts" ~> cases _Type (ref Rewriting.deannotateTypeDef @@ (Core.typeSchemeType $ var "ts"))
    (Just true) [
    _Type_record>>: constant true,
    _Type_union>>: constant true,
    _Type_wrap>>: constant true]) $
  "badVars" <~ Sets.fromList (Lists.filter
    ("v" ~> Optionals.maybe false ("t" ~> Logic.not (var "isTypeAlias" @@ var "t")) $
      ref Lexical.dereferenceSchemaTypeDef @@ var "v" @@ (Typing.inferenceContextSchemaTypes $ var "cx"))
    (Sets.toList $ var "suspectVars")) $
  "badPairs" <~ Lists.filter ("p" ~> Sets.member (first $ var "p") (var "badVars")) (Maps.toList $ var "s") $
  "printPair" <~ ("p" ~> (Core.unName $ first $ var "p") ++ " --> " ++ (ref ShowCore.typeDef @@ second (var "p"))) $
  Logic.ifElse (Sets.null $ var "badVars")
    (produce $ var "subst")
    (Flows.fail $ "Schema type(s) incorrectly unified: {"
      ++ (Strings.intercalate ", " (Lists.map (var "printPair") (var "badPairs")))
      ++ "}")

-- W: wfTy
checkTypeVariablesDef :: TBinding (InferenceContext -> S.Set Name -> Type -> Flow s ())
checkTypeVariablesDef = define "checkTypeVariables" $
  doc "Check that all type variables in a type are bound" $
  "cx" ~> "tyvars" ~> "typ" ~>
  trace (Strings.cat $ list [
    string "checking variables of: ",
    ref ShowCore.typeDef @@ var "typ"]) $
  cases _Type (var "typ")
    (Just $
      "result" <<~ Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "tyvars")
        (ref Rewriting.subtypesDef @@ var "typ") $
      Flows.pure unit) [
    _Type_forall>>: "ft" ~> ref checkTypeVariablesDef
      @@ var "cx"
      @@ (Sets.insert (Core.forallTypeParameter $ var "ft") (var "tyvars"))
      @@ (Core.forallTypeBody $ var "ft"),
    _Type_variable>>: "v" ~> Logic.ifElse (Sets.member (var "v") (var "tyvars"))
      (Flows.pure unit)
      (Logic.ifElse (Maps.member (var "v") (Typing.inferenceContextSchemaTypes (var "cx")))
        (Flows.pure unit)
        (Flows.fail $ Strings.cat $ list [
          string "unbound type variable \"",
          Core.unName $ var "v",
          string "\" in ",
          ref ShowCore.typeDef @@ var "typ"]))]

debugInferenceDef :: TBinding Bool
debugInferenceDef = define "debugInference" $
  doc "Disable type checking by default, for better performance" $
  true

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
  -- TODO: restore
--  "term2" <~ ref bindUnboundTypeVariablesDef @@ var "cx" @@ var "term" $
--  exec (ref checkForUnboundTypeVariablesDef @@ var "cx" @@ var "term2") $
  "term2" <~ "term" $
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

freshNameDef :: TBinding (Flow s Name)
freshNameDef = define "freshName" $
  doc "Generate a fresh type variable name" $
  Flows.map (ref normalTypeVariableDef) (ref Annotations.nextCountDef @@ ref key_vcountDef)

freshNamesDef :: TBinding (Int -> Flow s [Name])
freshNamesDef = define "freshNames" $
  doc "Generate multiple fresh type variable names" $
  "n" ~> Flows.sequence $ Lists.replicate (var "n") (ref freshNameDef)

freshVariableTypeDef :: TBinding (Flow s Type)
freshVariableTypeDef = define "freshVariableType" $
  doc "Generate a fresh type variable" $
  Flows.map (unaryFunction Core.typeVariable) (ref freshNameDef)

generalizeDef :: TBinding (InferenceContext -> Type -> TypeScheme)
generalizeDef = define "generalize" $
  doc "Generalize a type to a type scheme" $
  "cx" ~> "typ" ~>
  "vars" <~ Lists.nub (Lists.filter (ref isUnboundDef @@ var "cx") $
     ref Rewriting.freeVariablesInTypeOrderedDef @@ var "typ") $
  Core.typeScheme (var "vars") (var "typ")

graphToInferenceContextDef :: TBinding (Graph -> Flow s InferenceContext)
graphToInferenceContextDef = define "graphToInferenceContext" $
  doc "Convert a graph to an inference context" $
  "g0" ~>
  "schema" <~ Optionals.fromMaybe (var "g0") (Graph.graphSchema $ var "g0") $
  "primTypes" <~ Maps.fromList (Lists.map
    ("p" ~> pair (Graph.primitiveName $ var "p") (Graph.primitiveType $ var "p"))
    (Maps.elems $ Graph.graphPrimitives $ var "g0")) $
  "varTypes" <~ Maps.empty $
  "schemaTypes" <<~ ref Schemas.schemaGraphToTypingEnvironmentDef @@ var "schema" $
  produce $ Typing.inferenceContext (var "schemaTypes") (var "primTypes") (var "varTypes") false

inferGraphTypesDef :: TBinding (Graph -> Flow s Graph)
inferGraphTypesDef = define "inferGraphTypes" $
  doc "Infer types for all elements in a graph" $
  "g0" ~>
  "fromLetTerm" <~ ("l" ~>
    "bindings" <~ Core.letBindings (var "l") $
    "body" <~ Core.letEnvironment (var "l") $
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
  trace "graph inference" $
  "cx" <<~ ref graphToInferenceContextDef @@ var "g0" $
  "result" <<~ ref inferTypeOfTermDef @@ var "cx" @@ (var "toLetTerm" @@ var "g0") @@ string "graph term" $
  "term" <~ Typing.inferenceResultTerm (var "result") $
  "ts" <~ Typing.inferenceResultType (var "result") $
  "finalized" <<~ ref finalizeInferredTermDef @@ var "cx" @@ var "term" $
  cases _Term (var "finalized")
    Nothing [
    _Term_let>>: "l" ~> Flows.pure $ var "fromLetTerm" @@ var "l",
    _Term_variable>>: constant $ Flows.fail $ string "Expected inferred graph as let term"]

-- Note: this operation is expensive, as it creates a new typing environment for each individual term
inferInGraphContextDef :: TBinding (Term -> Flow Graph InferenceResult)
inferInGraphContextDef = define "inferInGraphContext" $
  doc "Infer the type of a term in graph context" $
  "term" ~>
  "g" <<~ ref Monads.getStateDef $
  "cx" <<~ ref graphToInferenceContextDef @@ var "g" $
  ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "single term"

inferManyDef :: TBinding (InferenceContext -> [(Term, String)] -> Flow s ([Term], [Type], TypeSubst))
inferManyDef = define "inferMany" $
  doc "Infer types for multiple terms" $
  "cx" ~> "pairs" ~>
  Logic.ifElse (Lists.null $ var "pairs")
    (Flows.pure $ pair (list []) $ pair (list []) (ref Substitution.idTypeSubstDef))
    ("e" <~ first (Lists.head $ var "pairs") $
     "desc" <~ second (Lists.head $ var "pairs") $
     "tl" <~ Lists.tail (var "pairs") $
     "result1" <<~ ref inferTypeOfTermDef @@ var "cx" @@ var "e" @@ var "desc" $
     "e1" <~ Typing.inferenceResultTerm (var "result1") $
     "t1" <~ Typing.inferenceResultType (var "result1") $
     "s1" <~ Typing.inferenceResultSubst (var "result1") $
     "result2" <<~ ref inferManyDef @@ (ref Substitution.substInContextDef @@ var "s1" @@ var "cx") @@ var "tl" $
     "e2" <~ first (var "result2") $
     "t2" <~ first (second $ var "result2") $
     "s2" <~ second (second $ var "result2") $
     produce $ pair
       (Lists.cons (ref Substitution.substTypesInTermDef @@ var "s2" @@ var "e1") (var "e2"))
       (pair
         (Lists.cons (ref Substitution.substInTypeDef @@ var "s2" @@ var "t1") (var "t2"))
         (ref Substitution.composeTypeSubstDef @@ var "s1" @@ var "s2")))

inferTypeOfDef :: TBinding (InferenceContext -> Term -> Flow s (Term, TypeScheme))
inferTypeOfDef = define "inferTypeOf" $
  doc "Infer the type of a term and return a type scheme" $
  "cx" ~> "term" ~>
  -- Top-level let term which allows us to easily extract an inferred type scheme
  "letTerm" <~ Core.termLet (Core.let_
    (list [Core.binding (Core.name $ string "ignoredVariableName") (var "term") nothing])
    (TTerms.string "ignoredBody")) $
  "unifyAndSubst" <~ ("result" ~>
    "subst" <~ Typing.inferenceResultSubst (var "result") $
    "finalized" <<~ ref finalizeInferredTermDef @@ var "cx" @@ Typing.inferenceResultTerm (var "result") $
    "letResult" <<~ ref Lexical.withEmptyGraphDef @@ (ref ExtractCore.letTermDef @@ var "finalized") $
    "bindings" <~ Core.letBindings (var "letResult") $
    Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "bindings"))
      ("binding" <~ Lists.head (var "bindings") $
       "term1" <~ Core.bindingTerm (var "binding") $
       "mts" <~ Core.bindingType (var "binding") $
       Optionals.maybe
         (Flows.fail $ string "Expected a type scheme")
         ("ts" ~> Flows.pure $ pair (var "term1") (var "ts"))
         (var "mts"))
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
  "term" <~ Core.annotatedTermSubject (var "at") $
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
  "v" <<~ ref freshNameDef $
  "s2" <<~ ref Unification.unifyTypesDef
    @@ (Typing.inferenceContextSchemaTypes $ var "cx")
    @@ (ref Substitution.substInTypeDef @@ var "s1" @@ var "t0")
    @@ (Core.typeFunction $ Core.functionType (var "t1") (Core.typeVariable $ var "v"))
    @@ string "application lhs" $
  exec (ref checkTypeSubstDef @@ var "cx" @@ var "s2") $
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
  "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ref ExtractCore.unionTypeDef @@ var "tname" @@ var "stype" $
  "dfltResult" <<~ Flows.mapOptional ("t" ~> ref inferTypeOfTermDef @@ var "cx" @@ var "t" @@
    (Strings.cat $ list [string "case ", Core.unName $ var "tname", string ".<default>"])) (var "dflt") $
  "caseResults" <<~ ref inferManyDef @@ var "cx" @@ Lists.map
    ("f" ~> pair (Core.fieldTerm $ var "f")
      (Strings.cat $ list [string "case ", Core.unName $ var "tname", string ".", Core.unName $ Core.fieldName $ var "f"]))
    (var "cases") $
  "iterms" <~ first (var "caseResults") $
  "itypes" <~ first (second $ var "caseResults") $
  "isubst" <~ second (second $ var "caseResults") $
  "codv" <<~ ref freshNameDef $
  "cod" <~ Core.typeVariable (var "codv") $
  "caseMap" <~ Maps.fromList (Lists.map
    ("ft" ~> pair (Core.fieldTypeName $ var "ft") (Core.fieldTypeType $ var "ft"))
    (var "sfields")) $
  "dfltConstraints" <~ ref Monads.optionalToListDef @@ (Optionals.map
    ("r" ~> Typing.typeConstraint (var "cod") (Typing.inferenceResultType $ var "r") (string "match default"))
    (var "dfltResult")) $
  "caseConstraints" <~ Optionals.cat (Lists.zipWith
    ("fname" ~> "itype" ~> Optionals.map
      ("ftype" ~> Typing.typeConstraint
        (var "itype")
        (Core.typeFunction $ Core.functionType (var "ftype") (var "cod"))
        (string "case type"))
      (Maps.lookup (var "fname") (var "caseMap")))
    (var "fnames") (var "itypes")) $
  ref mapConstraintsDef
    @@ var "cx"
    @@ ("subst" ~> ref yieldDef
      @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationUnion $
            Core.caseStatement (var "tname") (Optionals.map (unaryFunction Typing.inferenceResultTerm) $ var "dfltResult") $
            Lists.zipWith ("n" ~> "t" ~> Core.field (var "n") (var "t")) (var "fnames") (var "iterms"))
      @@ (Core.typeFunction $ Core.functionType
          (ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
          (var "cod"))
      @@ (ref Substitution.composeTypeSubstListDef
        @@ (Lists.concat $ list [
          ref Monads.optionalToListDef @@ (Optionals.map (unaryFunction Typing.inferenceResultSubst) (var "dfltResult")),
          list [var "isubst", var "subst"]])))
    @@ (Lists.concat $ list [var "dfltConstraints", var "caseConstraints"])

inferTypeOfCollectionDef :: TBinding (InferenceContext -> (Type -> Type) -> ([Term] -> Term) -> String -> [Term] -> Flow s InferenceResult)
inferTypeOfCollectionDef = define "inferTypeOfCollection" $
  doc "Infer the type of a collection" $
  "cx" ~> "typCons" ~> "trmCons" ~> "desc" ~> "els" ~>
  "var" <<~ ref freshNameDef $
  Logic.ifElse (Lists.null $ var "els")
    -- Empty collection: add type application for the fresh element type variable
    (Flows.pure $ ref yieldDef
      @@ (ref applyTypesDef @@ (var "trmCons" @@ list []) @@ list [var "var"])
      @@ (var "typCons" @@ (Core.typeVariable $ var "var"))
      @@ (ref Substitution.idTypeSubstDef))
    -- General case: non-empty collection
    ("results" <<~ ref inferManyDef @@ var "cx" @@
      (Lists.zip (var "els") $ Lists.map ("i" ~> Strings.cat $ list [string "#", Literals.showInt32 $ var "i"]) $
        Math.range (int32 1) (Math.add (Lists.length $ var "els") (int32 1))) $
    "terms" <~ first (var "results") $
    "types" <~ first (second $ var "results") $
    "subst1" <~ second (second $ var "results") $
    "constraints" <~ Lists.map ("t" ~> Typing.typeConstraint (Core.typeVariable $ var "var") (var "t") (var "desc")) (var "types") $
    ref mapConstraintsDef @@ var "cx" @@
      ("subst2" ~>
        "iterm" <~ var "trmCons" @@ var "terms" $
        "itype" <~ var "typCons" @@ (Core.typeVariable $ var "var") $
        "isubst" <~ ref Substitution.composeTypeSubstDef @@ var "subst1" @@ var "subst2" $
        ref yieldDef @@ var "iterm" @@ var "itype" @@ var "isubst") @@
      var "constraints")

inferTypeOfEliminationDef :: TBinding (InferenceContext -> Elimination -> Flow s InferenceResult)
inferTypeOfEliminationDef = define "inferTypeOfElimination" $
  doc "Infer the type of an elimination" $
  "cx" ~> "elm" ~>
  cases _Elimination (var "elm") Nothing [
    _Elimination_product>>: "tp" ~> ref inferTypeOfTupleProjectionDef @@ var "cx" @@ var "tp",
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
  "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "ityp" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "sfields" <<~ ref ExtractCore.unionTypeDef @@ var "tname" @@ var "stype" $
  "ftyp" <<~ ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields" $
  ref mapConstraintsDef @@ var "cx" @@
    ("subst" ~> ref yieldDef
      @@ (Core.termUnion $ Core.injection (var "tname") $ Core.field (var "fname") (var "iterm"))
      @@ (ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      @@ (ref Substitution.composeTypeSubstDef @@ var "isubst" @@ var "subst")) @@
    list [Typing.typeConstraint (var "ftyp") (var "ityp") (string "schema type of injected field")]

inferTypeOfLambdaDef :: TBinding (InferenceContext -> Lambda -> Flow s InferenceResult)
inferTypeOfLambdaDef = define "inferTypeOfLambda" $
  doc "Infer the type of a lambda function" $
  "cx" ~> "lambda" ~>
  "var" <~ Core.lambdaParameter (var "lambda") $
  "body" <~ Core.lambdaBody (var "lambda") $
  "vdom" <<~ ref freshNameDef $
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
  "body0" <~ Core.letEnvironment (var "let0") $
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
    (Optionals.cat $ Lists.map ("n" ~> Maps.lookup (var "n") (var "bindingMap")) (var "group"))
    (var "e")) $
  -- Note: this rewritten let term will yield success in all cases of dependencies among letrec bindings *except*
  --       in cases of polymorphic recursion. In those cases, type hints will be needed (#162).
  "rewrittenLet" <~ Lists.foldl (var "createLet") (var "body0") (Lists.reverse $ var "groups") $
  "restoreLet" <~ ("iterm" ~>
    "helper" <~ ("level" ~> "bins" ~> "term" ~>
      Logic.ifElse (Equality.equal (var "level") (int32 0))
        (pair (var "bins") (var "term"))
        (cases _Term (var "term") Nothing [
          _Term_let>>: "l" ~>
            "bs" <~ Core.letBindings (var "l") $
            "e" <~ Core.letEnvironment (var "l") $
            var "helper" @@
              (Math.sub (var "level") (int32 1)) @@
              (Lists.concat $ list [var "bs", var "bins"]) @@
              (var "e")])) $
    "result" <~ var "helper" @@ (Lists.length $ var "groups") @@ list [] @@ var "iterm" $
    "bindingList" <~ first (var "result") $
    "e" <~ second (var "result") $
    "bindingMap2" <~ Maps.fromList (Lists.map ("b" ~> pair (Core.bindingName $ var "b") (var "b")) (var "bindingList")) $
    Core.termLet $ Core.let_
      (Optionals.cat $ Lists.map ("n" ~> Maps.lookup (var "n") (var "bindingMap2")) (var "names"))
      (var "e")) $
  "rewriteResult" <~ ("result" ~>
    "iterm" <~ Typing.inferenceResultTerm (var "result") $
    "itype" <~ Typing.inferenceResultType (var "result") $
    "isubst" <~ Typing.inferenceResultSubst (var "result") $
    Typing.inferenceResult (var "restoreLet" @@ var "iterm") (var "itype") (var "isubst")) $
  Flows.map (var "rewriteResult") $
    cases _Term (var "rewrittenLet")
      (Just $ ref inferTypeOfTermDef @@ var "cx" @@ var "rewrittenLet" @@ string "empty let term") [
      _Term_let>>: "l" ~> ref inferTypeOfLetNormalizedDef @@ var "cx" @@ var "l"]

inferTypeOfLetNormalizedDef :: TBinding (InferenceContext -> Let -> Flow s InferenceResult)
inferTypeOfLetNormalizedDef = define "inferTypeOfLetNormalized" $
  doc "Infer the type of a let (letrec) term which is already in a normal form" $
  "cx0" ~> "letTerm" ~>
  
  -- Extract the bindings and body from the let term
  "bins0" <~ Core.letBindings (var "letTerm") $
  "body0" <~ Core.letEnvironment (var "letTerm") $
  "bnames" <~ Lists.map (unaryFunction Core.bindingName) (var "bins0") $
  
  -- Phase 1: Create fresh temporary type variables for each binding
  -- These act as placeholders during the initial inference pass
  "bvars" <<~ ref freshNamesDef @@ (Lists.length $ var "bins0") $
  "tbins0" <~ Lists.map (unaryFunction Core.typeVariable) (var "bvars") $
  
  -- Extend the context with temporary monomorphic type schemes
  -- This allows recursive references during inference
  "cx1" <~ (ref extendContextDef
    @@ (Lists.zip (var "bnames") $ Lists.map ("t" ~> Core.typeScheme (list []) (var "t")) (var "tbins0"))
    @@ (var "cx0")) $
  
  -- Phase 2: Infer the actual types of all binding terms
  -- This returns: (inferred terms, inferred types, substitution s1)
  "inferredResult" <<~ ref inferTypesOfTemporaryBindingsDef @@ var "cx1" @@ var "bins0" $
  "bterms1" <~ first (var "inferredResult") $  -- Terms with type applications embedded
  "tbins1" <~ first (second $ var "inferredResult") $  -- Actual inferred types
  "s1" <~ second (second $ var "inferredResult") $  -- Substitution from inference
  
  -- Phase 3: Unify temporary types with actual inferred types
  -- This ensures the temporary placeholders match what we actually inferred
  -- Returns substitution s2 that maps temporary variables to their actual types
  "s2" <<~ ref Unification.unifyTypeListsDef @@
    (Typing.inferenceContextSchemaTypes $ var "cx0") @@
    (Lists.map (ref Substitution.substInTypeDef @@ var "s1") (var "tbins0")) @@
    (var "tbins1") @@
    (string "temporary type bindings") $
  exec (ref checkTypeSubstDef @@ var "cx0" @@ var "s2") $

  -- Apply the composed substitution to the original context
  -- This context will be used for generalization
  "g2" <~ (ref Substitution.substInContextDef @@
    (ref Substitution.composeTypeSubstDef @@ var "s1" @@ var "s2") @@
    (var "cx0")) $

  -- Apply s2 to the terms as well as the types
  -- This ensures type applications in the terms use the same variables as the types
  "bterms1Subst" <~ Lists.map (ref Substitution.substTypesInTermDef @@ var "s2") (var "bterms1") $

  -- Phase 4: Generalize the inferred types into type schemes
--  "tsbins1" <~ (Lists.zip (var "bnames") $
--    Lists.zipWith
--      ("term" ~> "t" ~> ref generalizeDef @@ var "g2" @@ var "term" @@
--        (ref Substitution.substInTypeDef @@ var "s2" @@ var "t"))
--      (var "bterms1Subst")
--      (var "tbins1")) $
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
        "name" <~ first (var "pair") $
        "ts" <~ second (var "pair") $
        pair
          (var "name") $
          (ref applyTypesDef @@ (Core.termVariable $ var "name") @@ (Core.typeSchemeVariables $ var "ts")))
      (var "tsbins1"))) $
  
  -- Phase 7: Create the final bindings with type lambdas
  "createBinding" <~ ("bindingPair" ~>
    "nameTsPair" <~ first (var "bindingPair") $
    "term" <~ second (var "bindingPair") $
    "name" <~ first (var "nameTsPair") $
    "ts" <~ second (var "nameTsPair") $
    
    -- First, substitute polymorphic references in the term
    -- Then wrap in type lambdas for each variable in the type scheme
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
    (Core.typeLiteral $ ref Variants.literalTypeDef @@ var "lit")
    (ref Substitution.idTypeSubstDef)

inferTypeOfMapDef :: TBinding (InferenceContext -> M.Map Term Term -> Flow s InferenceResult)
inferTypeOfMapDef = define "inferTypeOfMap" $
  doc "Infer the type of a map" $
  "cx" ~> "m" ~>
  "kvar" <<~ ref freshNameDef $
  "vvar" <<~ ref freshNameDef $
  Logic.ifElse (Maps.null $ var "m")
    -- Empty map: add type applications for both key and value type variables
    (Flows.pure $ ref yieldDef
      @@ (ref applyTypesDef @@ (Core.termMap Maps.empty) @@ list [var "kvar", var "vvar"])
      @@ (Core.typeMap $ Core.mapType (Core.typeVariable $ var "kvar") (Core.typeVariable $ var "vvar"))
      @@ ref Substitution.idTypeSubstDef)
    -- Non-empty map: infer and unify key and value types
    ("kresults" <<~ ref inferManyDef @@ var "cx" @@
      (Lists.map ("k" ~> pair (var "k") (string "map key")) $ Maps.keys $ var "m") $
    "kterms" <~ first (var "kresults") $
    "ktypes" <~ first (second $ var "kresults") $
    "ksubst" <~ second (second $ var "kresults") $
    "vresults" <<~ ref inferManyDef
      @@ var "cx"
      @@ (Lists.map ("v" ~> pair (var "v") (string "map value")) $ Maps.elems $ var "m") $
    "vterms" <~ first (var "vresults") $
    "vtypes" <~ first (second $ var "vresults") $
    "vsubst" <~ second (second $ var "vresults") $
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
    (Core.termOptional nothing)
    (Core.termOptional $ just $ Lists.head $ var "terms")) $
  ref inferTypeOfCollectionDef
    @@ var "cx"
    @@ (unaryFunction Core.typeOptional)
    @@ var "trmCons"
    @@ string "optional element"
    @@ (Optionals.maybe (list []) (unaryFunction Lists.singleton) $ var "m")

inferTypeOfPrimitiveDef :: TBinding (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfPrimitiveDef = define "inferTypeOfPrimitive" $
  doc "Infer the type of a primitive function" $
  "cx" ~> "name" ~>
  Optionals.maybe
    (Flows.fail $ Strings.cat2 (string "No such primitive: ") (Core.unName $ var "name"))
    ("scheme" ~>
      "ts" <<~ ref instantiateTypeSchemeDef @@ var "scheme" $
      ref yieldCheckedDef @@
        (ref applyTypesDef @@
           (Core.termFunction $ Core.functionPrimitive $ var "name") @@
           Core.typeSchemeVariables (var "ts")) @@
        Core.typeSchemeType (var "ts") @@
        ref Substitution.idTypeSubstDef)
    (Maps.lookup (var "name") (Typing.inferenceContextPrimitiveTypes $ var "cx"))

inferTypeOfProductDef :: TBinding (InferenceContext -> [Term] -> Flow s InferenceResult)
inferTypeOfProductDef = define "inferTypeOfProduct" $
  doc "Infer the type of a product (tuple)" $
  "cx" ~> "els" ~>
  Flows.map
    ( "results" ~>
      "iterms" <~ first (var "results") $
      "itypes" <~ first (second $ var "results") $
      "isubst" <~ second (second $ var "results") $
      ref yieldDef @@ (Core.termProduct $ var "iterms") @@ (Core.typeProduct $ var "itypes") @@ var "isubst")
    (ref inferManyDef @@ var "cx" @@ (Lists.map ("e" ~> pair (var "e") (string "tuple element")) $ var "els"))

inferTypeOfProjectionDef :: TBinding (InferenceContext -> Projection -> Flow s InferenceResult)
inferTypeOfProjectionDef = define "inferTypeOfProjection" $
  doc "Infer the type of a record projection" $
  "cx" ~> "proj" ~>
  "tname" <~ Core.projectionTypeName (var "proj") $
  "fname" <~ Core.projectionField (var "proj") $
  "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ref ExtractCore.recordTypeDef @@ var "tname" @@ var "stype" $
  "ftyp" <<~ ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields" $
  Flows.pure $ ref yieldDef
    @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $ Core.projection (var "tname") (var "fname"))
    @@ (Core.typeFunction $ Core.functionType
      (ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      (var "ftyp"))
    @@ ref Substitution.idTypeSubstDef

inferTypeOfRecordDef :: TBinding (InferenceContext -> Record -> Flow s InferenceResult)
inferTypeOfRecordDef = define "inferTypeOfRecord" $
  doc "Infer the type of a record" $
  "cx" ~> "record" ~>
  "tname" <~ Core.recordTypeName (var "record") $
  "fields" <~ Core.recordFields (var "record") $
  "fnames" <~ Lists.map (unaryFunction Core.fieldName) (var "fields") $
  "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "results" <<~ ref inferManyDef @@ var "cx" @@ Lists.map
    ("f" ~> pair
      (Core.fieldTerm $ var "f")
      (Strings.cat2 (string "field ") (Core.unName $ Core.fieldName $ var "f")))
    (var "fields") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterms" <~ first (var "results") $
  "itypes" <~ first (second $ var "results") $
  "isubst" <~ second (second $ var "results") $
  "ityp" <~ Core.typeRecord (Core.rowType (var "tname") $
      Lists.zipWith ("n" ~> "t" ~> Core.fieldType (var "n") (var "t")) (var "fnames") (var "itypes")) $
  ref mapConstraintsDef @@ var "cx" @@
    ("subst" ~> ref yieldDef
      @@ (Core.termRecord $ Core.record (var "tname") $ Lists.zipWith
              ("n" ~> "t" ~> Core.field (var "n") (var "t"))
              (var "fnames")
              (var "iterms"))
      @@ (ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
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

inferTypeOfSumDef :: TBinding (InferenceContext -> Sum -> Flow s InferenceResult)
inferTypeOfSumDef = define "inferTypeOfSum" $
  doc "Infer the type of a sum type" $
  "cx" ~>
  "sum" ~>
  "i" <~ Core.sumIndex (var "sum") $
  "s" <~ Core.sumSize (var "sum") $
  "term" <~ Core.sumTerm (var "sum") $
  "result" <<~ ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "sum term" $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "ityp" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "varOrTerm" <~ ("t" ~> "j" ~> Logic.ifElse (Equality.equal (var "i") (var "j"))
    (Flows.pure $ Mantle.eitherLeft $ var "t")
    (Flows.map (unaryFunction Mantle.eitherRight) $ ref freshNameDef)) $
  "vars" <<~ Flows.mapList (var "varOrTerm" @@ var "ityp") (Math.range (int32 0) (Math.sub (var "s") (int32 1))) $
  "toType" <~ ("e" ~> cases _Either (var "e")
    Nothing [
    _Either_left>>: "t" ~> var "t",
    _Either_right>>: "v" ~> Core.typeVariable $ var "v"]) $
  produce $ ref yieldDef
    @@ Core.termSum (Core.sum (var "i") (var "s") (var "iterm"))
    @@ Core.typeSum (Lists.map (var "toType") (var "vars"))
    @@ var "isubst"

inferTypeOfTermDef :: TBinding (InferenceContext -> Term -> String -> Flow s InferenceResult)
inferTypeOfTermDef = define "inferTypeOfTerm" $
  doc "Infer the type of a given term" $
  "cx" ~> "term" ~> "desc" ~>
  trace (var "desc") $
  cases _Term (var "term") Nothing [
    _Term_annotated>>: "a" ~> ref inferTypeOfAnnotatedTermDef @@ var "cx" @@ var "a",
    _Term_application>>: "a" ~> ref inferTypeOfApplicationDef @@ var "cx" @@ var "a",
    _Term_function>>: "f" ~> ref inferTypeOfFunctionDef @@ var "cx" @@ var "f",
    _Term_let>>: "l" ~> ref inferTypeOfLetDef @@ var "cx" @@ var "l",
    _Term_list>>: "els" ~> ref inferTypeOfListDef @@ var "cx" @@ var "els",
    _Term_literal>>: "l" ~> ref inferTypeOfLiteralDef @@ var "cx" @@ var "l",
    _Term_map>>: "m" ~> ref inferTypeOfMapDef @@ var "cx" @@ var "m",
    _Term_optional>>: "m" ~> ref inferTypeOfOptionalDef @@ var "cx" @@ var "m",
    _Term_product>>: "els" ~> ref inferTypeOfProductDef @@ var "cx" @@ var "els",
    _Term_record>>: "r" ~> ref inferTypeOfRecordDef @@ var "cx" @@ var "r",
    _Term_set>>: "s" ~> ref inferTypeOfSetDef @@ var "cx" @@ var "s",
    _Term_sum>>: "s" ~> ref inferTypeOfSumDef @@ var "cx" @@ var "s",
    _Term_typeLambda>>: "ta" ~> ref inferTypeOfTypeLambdaDef @@ var "cx" @@ var "ta",
    _Term_typeApplication>>: "tt" ~> ref inferTypeOfTypeApplicationDef @@ var "cx" @@ var "tt",
    _Term_union>>: "i" ~> ref inferTypeOfInjectionDef @@ var "cx" @@ var "i",
    _Term_unit>>: constant $ Flows.pure $ ref typeOfUnitDef,
    _Term_variable>>: "name" ~> ref inferTypeOfVariableDef @@ var "cx" @@ var "name",
    _Term_wrap>>: "w" ~> ref inferTypeOfWrappedTermDef @@ var "cx" @@ var "w"]

inferTypeOfTupleProjectionDef :: TBinding (InferenceContext -> TupleProjection -> Flow s InferenceResult)
inferTypeOfTupleProjectionDef = define "inferTypeOfTupleProjection" $
  doc "Infer the type of a tuple projection" $
  "cx" ~> "tp" ~>
  "arity" <~ Core.tupleProjectionArity (var "tp") $
  "idx" <~ Core.tupleProjectionIndex (var "tp") $
  "vars" <<~ ref freshNamesDef @@ var "arity" $
  "types" <~ Lists.map (unaryFunction Core.typeVariable) (var "vars") $
  "cod" <~ Lists.at (var "idx") (var "types") $
  Flows.pure $ ref yieldDef
    @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationProduct $
        Core.tupleProjection (var "arity") (var "idx") (just $ var "types"))
    @@ (Core.typeFunction $ Core.functionType (Core.typeProduct $ var "types") (var "cod"))
    @@ (ref Substitution.idTypeSubstDef)

inferTypeOfTypeLambdaDef :: TBinding (InferenceContext -> TypeLambda -> Flow s InferenceResult)
inferTypeOfTypeLambdaDef = define "inferTypeOfTypeLambda" $
  doc "Infer the type of a type abstraction; just pass through to the lambda body." $
  "cx" ~> "ta" ~>
  ref inferTypeOfTermDef @@ var "cx" @@ (Core.typeLambdaBody $ var "ta") @@ string "type abstraction"

inferTypeOfTypeApplicationDef :: TBinding (InferenceContext -> TypedTerm -> Flow s InferenceResult)
inferTypeOfTypeApplicationDef = define "inferTypeOfTypeApplication" $
  doc "Infer the type of a type application; just pass through to the inner term." $
  "cx" ~> "tt" ~>
  ref inferTypeOfTermDef @@ var "cx" @@ (Core.typedTermTerm $ var "tt") @@ string "type application term"

inferTypeOfUnwrapDef :: TBinding (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfUnwrapDef = define "inferTypeOfUnwrap" $
  doc "Infer the type of an unwrap operation" $
  "cx" ~> "tname" ~>
  "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "wtyp" <<~ ref ExtractCore.wrappedTypeDef @@ var "tname" @@ var "stype" $
  Flows.pure $ ref yieldDef
    @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationWrap $ var "tname")
    @@ (Core.typeFunction $ Core.functionType
      (ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
      (var "wtyp"))
    @@ ref Substitution.idTypeSubstDef

inferTypeOfVariableDef :: TBinding (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfVariableDef = define "inferTypeOfVariable" $
  doc "Infer the type of a variable" $
  "cx" ~> "name" ~>
  Optionals.maybe
    (Flows.fail $ Strings.cat2 (string "Variable not bound to type: ") (Core.unName $ var "name"))
    ("scheme" ~>
      "ts" <<~ ref instantiateTypeSchemeDef @@ var "scheme" $
      produce $ Typing.inferenceResult
        (ref applyTypesDef @@
          (Core.termVariable $ var "name") @@
          Core.typeSchemeVariables (var "ts"))
        (Core.typeSchemeType $ var "ts")
        (ref Substitution.idTypeSubstDef))
    (Maps.lookup (var "name") (Typing.inferenceContextDataTypes $ var "cx"))

inferTypeOfWrappedTermDef :: TBinding (InferenceContext -> WrappedTerm -> Flow s InferenceResult)
inferTypeOfWrappedTermDef = define "inferTypeOfWrappedTerm" $
  doc "Infer the type of a wrapped term" $
  "cx" ~> "wt" ~>
  "tname" <~ Core.wrappedTermTypeName (var "wt") $
  "term" <~ Core.wrappedTermObject (var "wt") $
  "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "result" <<~ ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "wrapped term" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "iterm" <~ Typing.inferenceResultTerm (var "result") $
  "ityp" <~ Typing.inferenceResultType (var "result") $
  "isubst" <~ Typing.inferenceResultSubst (var "result") $
  "freshVars" <<~ ref freshNamesDef @@ Lists.length (var "svars") $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (Lists.map (unaryFunction Core.typeVariable) $ var "freshVars")) $
  "stypInst" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "stype" $
  "nominalInst" <~ ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "freshVars") $
  "expected" <~ Core.typeWrap (Core.wrappedType (var "tname") (var "ityp")) $
  "freeVars" <~ Sets.toList (Sets.unions $ list [
    ref Rewriting.freeVariablesInTypeDef @@ var "ityp",
    ref Rewriting.freeVariablesInTermDef @@ var "iterm",
    Sets.fromList (var "freshVars")]) $
  ref bindConstraintsDef @@ var "cx" @@
    ("subst2" ~> ref yieldCheckedDef @@
      (Core.termWrap $ Core.wrappedTerm (var "tname") (var "iterm")) @@
      var "nominalInst" @@
      (ref Substitution.composeTypeSubstDef @@ var "isubst" @@ var "subst2")) @@
    list [Typing.typeConstraint (var "stypInst") (var "expected") (string "schema type of wrapper")]

inferTypesOfTemporaryBindingsDef :: TBinding (InferenceContext -> [Binding] -> Flow s ([Term], ([Type], TypeSubst)))
inferTypesOfTemporaryBindingsDef = define "inferTypesOfTemporaryBindings" $
  doc "Infer types for temporary let bindings" $
  "cx" ~> "bins" ~>
  Logic.ifElse (Lists.null $ var "bins")
    (Flows.pure $ pair (list []) (pair (list []) (ref Substitution.idTypeSubstDef))) $
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
  "h" <~ first (var "result2") $
  "r_prime" <~ first (second $ var "result2") $
  "r" <~ second (second $ var "result2") $
  Flows.pure $ pair
    (Lists.cons (ref Substitution.substTypesInTermDef @@ var "r" @@ var "j") (var "h"))
    (pair
      (Lists.cons (ref Substitution.substInTypeDef @@ var "r" @@ var "u_prime") (var "r_prime"))
      (ref Substitution.composeTypeSubstDef @@ var "u" @@ var "r"))

initialTypeContextDef :: TBinding (Graph -> Flow s TypeContext)
initialTypeContextDef = define "initialTypeContext" $
  doc "Create an initial type context from a graph" $
  "g" ~>
  "toPair" <~ ("pair" ~>
    "name" <~ first (var "pair") $
    "el"  <~ second (var "pair") $
    optCases (Core.bindingType $ var "el")
      (Flows.fail $ "untyped element: " ++ Core.unName (var "name"))
      ("ts" ~> produce $ pair (var "name") (ref Schemas.typeSchemeToFTypeDef @@ var "ts"))) $
  "ix" <<~ ref graphToInferenceContextDef @@ var "g" $
  "types" <<~ Flows.map
    (unaryFunction Maps.fromList)
    (Flows.mapList (var "toPair") (Maps.toList $ Graph.graphElements $ var "g")) $
  produce $ Typing.typeContext (var "types") Sets.empty (var "ix")

-- W: inst
instantiateTypeSchemeDef :: TBinding (TypeScheme -> Flow s TypeScheme)
instantiateTypeSchemeDef = define "instantiateTypeScheme" $
  doc "Instantiate a type scheme with fresh variables" $
  "scheme" ~>
  "oldVars" <~ Core.typeSchemeVariables (var "scheme") $
  "newVars" <<~ ref freshNamesDef @@ Lists.length (var "oldVars") $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "oldVars") (Lists.map (unaryFunction Core.typeVariable) $ var "newVars")) $
  produce $ Core.typeScheme (var "newVars") $
    ref Substitution.substInTypeDef @@ var "subst" @@ Core.typeSchemeType (var "scheme")

isUnboundDef :: TBinding (InferenceContext -> Name -> Bool)
isUnboundDef = define "isUnbound" $
  doc "Check if a variable is unbound in context" $
  "cx" ~> "v" ~>
  Logic.and
    (Logic.not $ Sets.member (var "v") $ ref freeVariablesInContextDef @@ var "cx")
    (Logic.not $ Maps.member (var "v") $ Typing.inferenceContextSchemaTypes $ var "cx")

key_vcountDef :: TBinding Name
key_vcountDef = define "key_vcount" $
  doc "Key for inference type variable count" $
  Core.name $ string "inferenceTypeVariableCount"

mapConstraintsDef :: TBinding (InferenceContext -> (TypeSubst -> a) -> [TypeConstraint] -> Flow s a)
mapConstraintsDef = define "mapConstraints" $
  doc "Map over type constraints after unification" $
  "cx" ~> "f" ~> "constraints" ~>
  "s" <<~ ref Unification.unifyTypeConstraintsDef @@ (Typing.inferenceContextSchemaTypes $ var "cx") @@ var "constraints" $
  exec (ref checkTypeSubstDef @@ var "cx" @@ var "s") $
  produce (var "f" @@ var "s")

nominalApplicationDef :: TBinding (Name -> [Type] -> Type)
nominalApplicationDef = define "nominalApplication" $
  doc "Apply type arguments to a nominal type" $
  "tname" ~> "args" ~>
  Lists.foldl
    ("t" ~> "a" ~> Core.typeApplication $ Core.applicationType (var "t") (var "a"))
    (Core.typeVariable $ var "tname")
    (var "args")

normalTypeVariableDef :: TBinding (Int -> Name)
normalTypeVariableDef = define "normalTypeVariable" $
  doc "Type variable naming convention follows Haskell: t0, t1, etc." $
  "i" ~> Core.name (Strings.cat2 (string "t") (Literals.showInt32 $ var "i"))

requireSchemaTypeDef :: TBinding (InferenceContext -> Name -> Flow s TypeScheme)
requireSchemaTypeDef = define "requireSchemaType" $
  doc "Look up a schema type in the context and instantiate it" $
  "cx" ~> "tname" ~>
  Optionals.maybe
    (Flows.fail $ Strings.cat2 (string "No such schema type: ") (Core.unName $ var "tname"))
    -- TODO: the deannotation is probably superfluous
    ("ts" ~> ref instantiateTypeSchemeDef @@ (ref Rewriting.deannotateTypeSchemeRecursiveDef @@ var "ts"))
    (Maps.lookup (var "tname") (Typing.inferenceContextSchemaTypes $ var "cx"))

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

toFContextDef :: TBinding (InferenceContext -> M.Map Name Type)
toFContextDef = define "toFContext" $
  doc "Convert inference context to type context" $
  "cx" ~> Maps.map (ref Schemas.typeSchemeToFTypeDef) $ Typing.inferenceContextDataTypes $ var "cx"

typeOfDef :: TBinding (TypeContext -> Term -> Flow s Type)
typeOfDef = define "typeOf" $
  doc "Given a type context, reconstruct the type of a System F term" $
  "tcontext" ~> "term" ~>
  ref typeOfInternalDef @@
    Typing.typeContextInferenceContext (var "tcontext") @@
    Typing.typeContextVariables (var "tcontext") @@
    Typing.typeContextTypes (var "tcontext") @@
    (list []) @@
    var "term"

-- W: typeOf
typeOfInternalDef :: TBinding (InferenceContext -> S.Set Name -> M.Map Name Type -> [Type] -> Term -> Flow s Type)
typeOfInternalDef = define "typeOfInternal" $
  doc "Given internal context, reconstruct the type of a System F term" $
  "cx" ~> "vars" ~> "types" ~> "apptypes" ~> "term" ~>
  "checkApplied" <~ ("e" ~>  Logic.ifElse (Lists.null $ var "apptypes")
    (var "e")
    ( "app" <~ ("t" ~> "apptypes" ~>
        Logic.ifElse (Lists.null $ var "apptypes")
          (Flows.pure $ var "t")
          (cases _Type (var "t")
            (Just $ Flows.fail $ Strings.cat $ list [
              string "not a forall type: ",
              ref ShowCore.typeDef @@ var "t",
              string " in ",
              ref ShowCore.termDef @@ var "term"]) [
            _Type_forall>>: "ft" ~>
              "v" <~ Core.forallTypeParameter (var "ft") $
              "t2" <~ Core.forallTypeBody (var "ft") $
              var "app"
                @@ (ref Substitution.substInTypeDef @@
                  (Typing.typeSubst $ Maps.singleton (var "v") (Lists.head $ var "apptypes")) @@
                  (var "t2"))
                @@ (Lists.tail $ var "apptypes")])) $
      "t1" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "term" $
      exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "t1") $
      var "app" @@ var "t1" @@ var "apptypes")) $

  trace (Strings.cat $ list [
    string "checking type of: ",
    ref ShowCore.termDef @@ var "term",
    string " (vars: ",
    ref Formatting.showListDef @@ unaryFunction Core.unName @@ (Sets.toList $ var "vars"),
    string ", apptypes: ",
    ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "apptypes",
    string ", types: ",
    ref Formatting.showListDef @@ unaryFunction Core.unName @@ (Maps.keys $ var "types"),
    string ")"]) $

  cases _Term (var "term")
    (Just $ Flows.fail $ Strings.cat $ list [
      "unsupported term variant in typeOf: ",
      ref ShowMantle.termVariantDef @@ (ref Variants.termVariantDef @@ var "term")]) [

    _Term_annotated>>: "at" ~> var "checkApplied" @@ (
      "term1" <~ Core.annotatedTermSubject (var "at") $
      ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "apptypes" @@ var "term1"),

    _Term_application>>: "app" ~> var "checkApplied" @@ (
      "a" <~ Core.applicationFunction (var "app") $
      "b" <~ Core.applicationArgument (var "app") $
      "t1" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "a" $
      "t2" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "b" $
      exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "t1") $
      exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "t2") $
      cases _Type (var "t1")
        (Just $ Flows.fail $ Strings.cat $ list [
          "left hand side of application ",
          ref ShowCore.termDef @@ var "term",
          " is not a function type: ",
          ref ShowCore.typeDef @@ var "t1"]) [
        _Type_function>>: "ft" ~>
          "p" <~ Core.functionTypeDomain (var "ft") $
          "q" <~ Core.functionTypeCodomain (var "ft") $
          Logic.ifElse (Equality.equal (var "p") (var "t2"))
            (Flows.pure $ var "q")
            (Flows.fail $ Strings.cat $ list [
              "expected ",
              ref ShowCore.typeDef @@ var "p",
              " in ",
              ref ShowCore.termDef @@ var "term",
              " but found ",
              ref ShowCore.typeDef @@ var "t2"])]),

    _Term_function>>: "f" ~>
      cases _Function (var "f") Nothing [
        _Function_elimination>>: "elm" ~>
          cases _Elimination (var "elm") Nothing [
            _Elimination_product>>: "tp" ~> var "checkApplied" @@ (
              "index" <~ Core.tupleProjectionIndex (var "tp") $
              "arity" <~ Core.tupleProjectionArity (var "tp") $
              "mtypes" <~ Core.tupleProjectionDomain (var "tp") $
              Optionals.maybe
                (Flows.fail $ Strings.cat $ list [
                  "untyped tuple projection: ",
                  ref ShowCore.termDef @@ var "term"])
                ( "types" ~>
                  exec (Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "vars") (var "types")) $
                  Flows.pure $ Core.typeFunction $ Core.functionType
                    (Core.typeProduct $ var "types")
                    (Lists.at (var "index") (var "types")))
                (var "mtypes")),

            _Elimination_record>>: "p" ~>
              "tname" <~ Core.projectionTypeName (var "p") $
              "fname" <~ Core.projectionField (var "p") $
              "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
              "svars" <~ Core.typeSchemeVariables (var "schemaType") $
              "stype" <~ Core.typeSchemeType (var "schemaType") $
              "sfields" <<~ ref ExtractCore.recordTypeDef @@ var "tname" @@ var "stype" $
              "ftyp" <<~ ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields" $
              "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "apptypes")) $
              "sftyp" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "ftyp" $
              Flows.pure $ Core.typeFunction $ Core.functionType
                (ref nominalApplicationDef @@ var "tname"  @@ var "apptypes")
                (var "sftyp"),

            _Elimination_union>>: "cs" ~>
              "tname" <~ Core.caseStatementTypeName (var "cs") $
              "dflt" <~ Core.caseStatementDefault (var "cs") $
              "cases" <~ Core.caseStatementCases (var "cs") $
              "cterms" <~ Lists.map (unaryFunction Core.fieldTerm) (var "cases") $
              "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
              "svars" <~ Core.typeSchemeVariables (var "schemaType") $
              "stype" <~ Core.typeSchemeType (var "schemaType") $
              "sfields" <<~ ref ExtractCore.unionTypeDef @@ var "tname" @@ var "stype" $
              "tdflt" <<~ Flows.mapOptional ("e" ~> ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "e") (var "dflt") $
              "tcterms" <<~ Flows.mapList ("e" ~> ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "e") (var "cterms") $
              "cods" <<~ Flows.mapList ("t" ~> Flows.map (unaryFunction Core.functionTypeCodomain) $ ref ExtractCore.functionTypeDef @@ var "t") (var "tcterms") $
              "ts" <~ Optionals.cat (Lists.cons (var "tdflt") $ Lists.map (unaryFunction Optionals.pure) (var "cods")) $
              "cod" <<~ ref checkSameTypeDef @@ string "case branches" @@ var "ts" $
              "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "apptypes")) $
              "scod" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "cod" $
              Flows.pure $ Core.typeFunction $ Core.functionType
                (ref nominalApplicationDef @@ var "tname"  @@ var "apptypes")
                (var "scod"),

            _Elimination_wrap>>: "tname" ~>
              "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
              "svars" <~ Core.typeSchemeVariables (var "schemaType") $
              "stype" <~ Core.typeSchemeType (var "schemaType") $
              "wrapped" <<~ ref ExtractCore.wrappedTypeDef @@ var "tname" @@ var "stype" $
              "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "apptypes")) $
              "swrapped" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "wrapped" $
              produce $ TTypes.function
                (ref nominalApplicationDef @@ var "tname" @@ var "apptypes")
                (var "swrapped")],

        _Function_lambda>>: "l" ~> var "checkApplied" @@ (
          "x" <~ Core.lambdaParameter (var "l") $
          "mt" <~ Core.lambdaDomain (var "l") $
          "e" <~ Core.lambdaBody (var "l") $
          Optionals.maybe
            (Flows.fail $ Strings.cat $ list [
              string "untyped lambda: ",
              ref ShowCore.termDef @@ var "term"])
            ("t" ~>
              exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "t") $
              "t1" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@
                (Maps.insert (var "x") (var "t") (var "types")) @@ list [] @@ var "e" $
              exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "t1") $
              Flows.pure $ Core.typeFunction $ Core.functionType (var "t") (var "t1"))
            (var "mt")),
        _Function_primitive>>: "name" ~> var "checkApplied" @@ (
          -- Note: no instantiation
          "ts" <~ Optionals.maybe
            (Flows.fail $ Strings.cat $ list [
              string "no such primitive: ",
              Core.unName $ var "name"])
            (unaryFunction Flows.pure)
            (Maps.lookup (var "name") (Typing.inferenceContextPrimitiveTypes $ var "cx")) $
          Flows.map (ref Schemas.typeSchemeToFTypeDef) (var "ts"))],

    _Term_let>>: "letTerm" ~> var "checkApplied" @@ (
      "bs" <~ Core.letBindings (var "letTerm") $
      "body" <~ Core.letEnvironment (var "letTerm") $
      "bnames" <~ Lists.map (unaryFunction Core.bindingName) (var "bs") $
      "bterms" <~ Lists.map (unaryFunction Core.bindingTerm) (var "bs") $
      "btypeOf" <~ ("b" ~>
        Optionals.maybe
          (Flows.fail $ Strings.cat $ list [
            string "untyped let binding in ",
            ref ShowCore.termDef @@ var "term"])
          ("ts" ~> Flows.pure $ ref Schemas.typeSchemeToFTypeDef @@ var "ts")
          (Core.bindingType $ var "b")) $
      "btypes" <<~ Flows.mapList (var "btypeOf") (var "bs") $
      "types2" <~ Maps.union (Maps.fromList $ Lists.zip (var "bnames") (var "btypes")) (var "types") $
      "typeofs" <<~ Flows.mapList (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types2" @@ list []) (var "bterms") $
      exec (Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "vars") (var "btypes")) $
      exec (Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "vars") (var "typeofs")) $
      Logic.ifElse (Equality.equal (var "typeofs") (var "btypes"))
        (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types2" @@ list [] @@ var "body")
        (Flows.fail $ Strings.cat $ list [
          string "binding types disagree: ",
          ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "btypes",
          string " and ",
          ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "typeofs",
          string " from terms: ",
          ref Formatting.showListDef @@ ref ShowCore.termDef @@ var "bterms"])),

    _Term_list>>: "els" ~>
      Logic.ifElse (Lists.null $ var "els")
        -- Empty list is polymorphic
        (Logic.ifElse (Equality.equal (Lists.length $ var "apptypes") (int32 1))
          (Flows.pure $ Core.typeList $ Lists.head $ var "apptypes")
          (Flows.fail $ "list type applied to more or less than one argument"))
        -- Nonempty list must have elements of the same type
        ( "eltypes" <<~ Flows.mapList
           (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [])
           (var "els") $
           "unifiedType" <<~ ref checkSameTypeDef @@ string "list elements" @@ var "eltypes" $
           -- Verify the unified type is well-formed in the current scope
           exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "unifiedType") $
           Flows.pure $ Core.typeList $ var "unifiedType"),

    _Term_literal>>: "lit" ~> var "checkApplied" @@ (
      Flows.pure $ Core.typeLiteral $ ref Variants.literalTypeDef @@ var "lit"),

    _Term_map>>: "m" ~>
      Logic.ifElse (Maps.null $ var "m")
        -- Empty map is polymorphic
        (Logic.ifElse (Equality.equal (Lists.length $ var "apptypes") (int32 2))
          (Flows.pure $ Core.typeMap $ Core.mapType
            (Lists.at (int32 1) $ var "apptypes")
            (Lists.at (int32 0) $ var "apptypes"))
          (Flows.fail $ "map type applied to more or less than two arguments"))
        -- Nonempty map must have keys and values of the same type
        (var "checkApplied" @@ (
          "pairs" <~ Maps.toList (var "m") $
          "kt" <<~ Flows.bind
            (Flows.mapList (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list []) $
              Lists.map (unaryFunction first) (var "pairs"))
            (ref checkSameTypeDef @@ string "map keys") $
          "vt" <<~ Flows.bind
            (Flows.mapList (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list []) $
              Lists.map (unaryFunction second) (var "pairs"))
            (ref checkSameTypeDef @@ string "map values") $
          exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "kt") $
          exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "vt") $
          Flows.pure $ Core.typeMap $ Core.mapType (var "kt") (var "vt"))),

    _Term_optional>>: "mt" ~>
      Optionals.maybe
        -- Nothing case is polymorphic
        ("n" <~ Lists.length (var "apptypes") $
          Logic.ifElse (Equality.equal (var "n") (int32 1))
            (Flows.pure $ Core.typeOptional $ Lists.head $ var "apptypes")
            (Flows.fail $ "optional type applied to " ++ Literals.showInt32 (var "n") ++ " argument(s). Expected 1."))
        -- Just case: infer type of the contained term
        ("term" ~> var "checkApplied" @@ (
          "termType" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "term" $
           exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "termType") $
           Flows.pure $ Core.typeOptional $ var "termType"))
        (var "mt"),

    _Term_product>>: "tuple" ~> var "checkApplied" @@ (
      "etypes" <<~ Flows.mapList (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list []) (var "tuple") $
      exec (Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "vars") (var "etypes")) $
      Flows.pure $ Core.typeProduct $ var "etypes"),

    _Term_record>>: "record" ~>
      "tname" <~ Core.recordTypeName (var "record") $
      "fields" <~ Core.recordFields (var "record") $
      "ftypes" <<~ Flows.mapList
        (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [])
        (Lists.map (unaryFunction Core.fieldTerm) (var "fields")) $
      exec (Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "vars") (var "ftypes")) $
      ref typeOfNominalDef
        @@ string "record typeOf"
        @@ var "cx"
        @@ var "tname"
        @@ (Core.typeRecord $ Core.rowType (var "tname") $ Lists.zipWith
          ("n" ~>
           "t" ~>
           Core.fieldType (var "n") (var "t"))
          (Lists.map (unaryFunction Core.fieldName) (var "fields"))
          (var "ftypes")),

    _Term_set>>: "els" ~>
      Logic.ifElse (Sets.null $ var "els")
        -- Empty set is polymorphic
        (Logic.ifElse (Equality.equal (Lists.length $ var "apptypes") (int32 1))
          (Flows.pure $ Core.typeSet $ Lists.head $ var "apptypes")
          (Flows.fail $ "set type applied to more or less than one argument"))
        -- Nonempty set must have elements of the same type
        ( "eltypes" <<~ Flows.mapList
           (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [])
           (Sets.toList $ var "els") $
           "unifiedType" <<~ ref checkSameTypeDef @@ string "set elements" @@ var "eltypes" $
           -- Verify the unified type is well-formed in the current scope
           exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "unifiedType") $
           Flows.pure $ Core.typeSet $ var "unifiedType"),

    -- TermSum (Sum idx size term1) is ignored for now. See https://github.com/CategoricalData/hydra/issues/134.

    _Term_typeLambda>>: "ta" ~>
      "v" <~ Core.typeLambdaParameter (var "ta") $
      "e" <~ Core.typeLambdaBody (var "ta") $
      "t1" <<~ ref typeOfInternalDef @@ var "cx" @@ (Sets.insert (var "v") (var "vars")) @@ var "types" @@ list [] @@ var "e" $
      exec (ref checkTypeVariablesDef @@ var "cx" @@ (Sets.insert (var "v") (var "vars")) @@ var "t1") $
      Flows.pure $ Core.typeForall $ Core.forallType (var "v") (var "t1"),

    _Term_typeApplication>>: "tyapp" ~>
      "e" <~ Core.typedTermTerm (var "tyapp") $
      "t" <~ Core.typedTermType (var "tyapp") $
      ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ Lists.cons (var "t") (var "apptypes") @@ var "e",

    _Term_union>>: "injection" ~>
      "tname" <~ Core.injectionTypeName (var "injection") $

      -- Note: the following are only for sanity checking, and are not used in the typeOf result.
      "field" <~ Core.injectionField (var "injection") $
      "fname" <~ Core.fieldName (var "field") $
      "fterm" <~ Core.fieldTerm (var "field") $
      "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
      "svars" <~ Core.typeSchemeVariables (var "schemaType") $
      "stype" <~ Core.typeSchemeType (var "schemaType") $
      "sfields" <<~ ref ExtractCore.unionTypeDef @@ var "tname" @@ var "stype" $
      "ftyp" <<~ ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields" $

      Flows.pure $ ref nominalApplicationDef @@ var "tname"  @@ var "apptypes",

    _Term_unit>>: constant $ var "checkApplied" @@ (Flows.pure Core.typeUnit),

    _Term_variable>>: "name" ~> var "checkApplied" @@ (Optionals.maybe
      (Flows.fail $ Strings.cat $ list [
        string "unbound variable: ",
        Core.unName $ var "name"])
      (unaryFunction Flows.pure)
      (Maps.lookup (var "name") (var "types"))),

    _Term_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTermTypeName (var "wt") $
      "innerTerm" <~ Core.wrappedTermObject (var "wt") $
      "innerType" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "innerTerm" $
      exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "innerType") $
      ref typeOfNominalDef @@ string "wrapper typeOf" @@ var "cx" @@ var "tname" @@
        (Core.typeWrap $ Core.wrappedType (var "tname") (var "innerType"))]

typeOfNominalDef :: TBinding (String -> InferenceContext -> Name -> Type -> Flow s Type)
typeOfNominalDef = define "typeOfNominal" $
  doc "Infer the type of a nominal type" $
  "desc" ~> "cx" ~> "tname" ~> "expected" ~>
  "resolveType" <~ ("subst" ~> "v" ~>
    Optionals.fromMaybe (Core.typeVariable $ var "v") (Maps.lookup (var "v") (var "subst"))) $
  "schemaType" <<~ ref requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "substWrapper" <<~ ref Unification.unifyTypesDef @@
    (Typing.inferenceContextSchemaTypes $ var "cx") @@
    var "stype" @@
    var "expected" @@
    var "desc" $
  exec (ref checkTypeSubstDef @@ var "cx" @@ var "substWrapper") $
  "subst" <~ Typing.unTypeSubst (var "substWrapper") $
  "tparams" <~ Lists.map (var "resolveType" @@ var "subst") (var "svars") $
  Flows.pure $ ref nominalApplicationDef @@ var "tname" @@ var "tparams"

typeOfUnitDef :: TBinding InferenceResult
typeOfUnitDef = define "inferTypeOfUnit" $
  doc "The trivial inference result for the unit term" $
  Typing.inferenceResult
    (Core.termUnit)
    (Core.typeUnit)
    (ref Substitution.idTypeSubstDef)

unboundTypeVariablesInTermDef :: TBinding (InferenceContext -> Term -> S.Set Name)
unboundTypeVariablesInTermDef = define "unboundTypeVariablesInTerm" $
  doc ("Get the set of unbound type variables in a term."
    <> " In this context, only the type schemes of let bindings can bind type variables; type lambdas do not.") $
  "cx" ~> "term0" ~>
  "allOf" <~ ("sets" ~> Lists.foldl (binaryFunction Sets.union) Sets.empty $ var "sets") $
  "tryType" <~ ("tvars" ~> "typ" ~> Sets.difference (ref Rewriting.freeVariablesInTypeDef @@ var "typ") (var "tvars")) $
  "getAll" <~ ("vars" ~> "term" ~>
    "recurse" <~ var "getAll" @@ var "vars" $
    "dflt" <~ (var "allOf" @@ Lists.map (var "recurse") (ref Rewriting.subtermsDef @@ var "term")) $
    cases _Term (var "term")
      (Just $ var "dflt") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "dflt") [
        _Function_elimination>>: "e" ~> cases _Elimination (var "e")
          (Just $ var "dflt") [
          _Elimination_product>>: "tp" ~> optCases (Core.tupleProjectionDomain $ var "tp")
            (Sets.empty)
            ("typs" ~> var "allOf" @@ (Lists.map (var "tryType" @@ var "vars") $ var "typs"))],
        _Function_lambda>>: "l" ~>
          "domt" <~ optCases (Core.lambdaDomain $ var "l") (Sets.empty) (var "tryType" @@ var "vars") $
          Sets.union (var "domt") (var "recurse" @@ (Core.lambdaBody $ var "l"))],
      _Term_let>>: "l" ~>
        "forBinding" <~ ("b" ~>
          "newVars" <~ optCases (Core.bindingType $ var "b")
             (var "vars")
             ("ts" ~> Sets.union (var "vars") (Sets.fromList $ Core.typeSchemeVariables $ var "ts")) $
          Sets.union
            (var "getAll" @@ var "newVars" @@ (Core.bindingTerm $ var "b"))
            (optCases (Core.bindingType $ var "b")
              Sets.empty
              ("ts" ~> var "tryType" @@ var "newVars" @@ (Core.typeSchemeType $ var "ts")))) $
        Sets.union
          (var "allOf" @@ Lists.map (var "forBinding") (Core.letBindings $ var "l"))
          (var "recurse" @@ (Core.letEnvironment $ var "l")),
      _Term_typeApplication>>: "tt" ~>
        Sets.union
          (var "tryType" @@ var "vars" @@ (Core.typedTermType $ var "tt"))
          (var "recurse" @@ (Core.typedTermTerm $ var "tt")),
      _Term_typeLambda>>: "tl" ~>
        Sets.union
          -- The type variable introduced by a type lambda is considered unbound unless it is also introduced in an
          -- enclosing let binding, as all type lambda terms are in Hydra.
          (var "tryType" @@ var "vars" @@ (Core.typeVariable $ Core.typeLambdaParameter $ var "tl"))
          (var "recurse" @@ (Core.typeLambdaBody $ var "tl"))]) $
  "svars" <~ Sets.fromList (Maps.keys $ Typing.inferenceContextSchemaTypes $ var "cx") $
  var "getAll" @@ var "svars" @@ var "term0"

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
