
module Hydra.Sources.Kernel.Terms.Rewriting where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  deannotateAndDetypeTerm,
  deannotateTerm,
  deannotateType,
  deannotateTypeParameters,
  deannotateTypeRecursive,
  deannotateTypeSchemeRecursive,
  detypeTerm,
  flattenLetTerms,
  foldOverTerm,
  foldOverType,
  freeTypeVariablesInTerm,
  freeVariablesInTerm,
  freeVariablesInType,
  freeVariablesInTypeOrdered,
  freeVariablesInTypeSimple,
  freeVariablesInTypeScheme,
  freeVariablesInTypeSchemeSimple,
  inlineType,
  isFreeVariableInTerm,
  isLambda,
  liftLambdaAboveLet,
  mapBeneathTypeAnnotations,
  normalizeTypeVariablesInTerm,
  pruneLet,
  removeTermAnnotations,
  removeTypeAnnotations,
  removeTypesFromTerm,
  replaceFreeTermVariable,
  replaceFreeTypeVariable,
  replaceTypedefs,
  rewriteAndFoldTerm,
  rewriteAndFoldTermM,
  rewriteTerm,
  rewriteTermM,
  rewriteTermWithContext,
  rewriteTermWithContextM,
  rewriteType,
  rewriteTypeM,
  simplifyTerm,
  substituteTypeVariables,
  substituteVariable,
  substituteVariables,
  subterms,
  subtermsWithAccessors,
  subtypes,
  termDependencyNames,
  toShortNames,
  topologicalSortBindingMap,
  topologicalSortBindings,
  typeDependencyNames,
  typeNamesInType,
  unshadowVariables)
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

import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Sorting as Sorting


ns :: Namespace
ns = Namespace "hydra.rewriting"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Names.module_, Sorting.module_]
    kernelTypesModules $
    Just ("Utilities for type and term rewriting and analysis.")
  where
   elements = [
     toBinding deannotateAndDetypeTerm,
     toBinding deannotateTerm,
     toBinding deannotateType,
     toBinding deannotateTypeParameters,
     toBinding deannotateTypeRecursive,
     toBinding deannotateTypeSchemeRecursive,
     toBinding detypeTerm,
     toBinding flattenLetTerms,
     toBinding foldOverTerm,
     toBinding foldOverType,
     toBinding freeTypeVariablesInTerm,
     toBinding freeVariablesInTerm,
--     toBinding freeVariablesInTermOpt,
     toBinding freeVariablesInType,
     toBinding freeVariablesInTypeOrdered,
     toBinding freeVariablesInTypeSchemeSimple,
     toBinding freeVariablesInTypeScheme,
     toBinding freeVariablesInTypeSimple,
     toBinding inlineType,
     toBinding isFreeVariableInTerm,
     toBinding isLambda,
     toBinding liftLambdaAboveLet,
     toBinding mapBeneathTypeAnnotations,
     toBinding normalizeTypeVariablesInTerm,
     toBinding pruneLet,
     toBinding removeTermAnnotations,
     toBinding removeTypeAnnotations,
     toBinding removeTypesFromTerm,
     toBinding replaceFreeTermVariable,
     toBinding replaceFreeTypeVariable,
     toBinding replaceTypedefs,
--     toBinding rewrite,
     toBinding rewriteAndFoldTerm,
     toBinding rewriteAndFoldTermM,
     toBinding rewriteTerm,
     toBinding rewriteTermM,
     toBinding rewriteTermWithContext,
     toBinding rewriteTermWithContextM,
     toBinding rewriteType,
     toBinding rewriteTypeM,
     toBinding simplifyTerm,
     toBinding substituteTypeVariables,
     toBinding substituteVariable,
     toBinding substituteVariables,
     toBinding subterms,
     toBinding subtermsWithAccessors,
     toBinding subtypes,
     toBinding termDependencyNames,
     toBinding toShortNames,
     toBinding topologicalSortBindingMap,
     toBinding topologicalSortBindings,
     toBinding typeDependencyNames,
     toBinding typeNamesInType,
     toBinding unshadowVariables]

deannotateAndDetypeTerm :: TBinding (Term -> Term)
deannotateAndDetypeTerm = define "deannotateAndDetypeTerm" $
  doc "Strip type annotations from the top levels of a term" $
  "t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~> deannotateAndDetypeTerm @@ (Core.annotatedTermBody $ var "at"),
    _Term_typeApplication>>: "tt" ~> deannotateAndDetypeTerm @@ (Core.typeApplicationTermBody $ var "tt"),
    _Term_typeLambda>>: "ta" ~> deannotateAndDetypeTerm @@ (Core.typeLambdaBody $ var "ta")]

deannotateTerm :: TBinding (Term -> Term)
deannotateTerm = define "deannotateTerm" $
  doc "Strip all annotations (including System F type annotations) from the top levels of a term" $
  "t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~> deannotateTerm @@ (Core.annotatedTermBody $ var "at")]

deannotateType :: TBinding (Type -> Type)
deannotateType = define "deannotateType" $
  doc "Strip all annotations from a term" $
  "t" ~> cases _Type (var "t")
    (Just $ var "t") [
    _Type_annotated>>: deannotateType <.> (project _AnnotatedType _AnnotatedType_body)]

deannotateTypeParameters :: TBinding (Type -> Type)
deannotateTypeParameters = define "deannotateTypeParameters" $
  doc "Strip any top-level type lambdas from a type, extracting the (possibly nested) type body" $
  "t" ~> cases _Type (deannotateType @@ var "t")
    (Just $ var "t") [
    _Type_forall>>: "lt" ~> deannotateTypeParameters @@ (project _ForallType _ForallType_body @@ var "lt")]

deannotateTypeRecursive :: TBinding (Type -> Type)
deannotateTypeRecursive = define "deannotateTypeRecursive" $
  doc "Recursively strip all annotations from a type" $
  "typ" ~>
  "strip" <~ ("recurse" ~> "typ" ~>
    "rewritten" <~ var "recurse" @@ var "typ" $
    cases _Type (var "rewritten")
      (Just $ var "rewritten") [
      _Type_annotated>>: "at" ~> Core.annotatedTypeBody $ var "at"]) $
  rewriteType @@ var "strip" @@ var "typ"

deannotateTypeSchemeRecursive :: TBinding (TypeScheme -> TypeScheme)
deannotateTypeSchemeRecursive = define "deannotateTypeSchemeRecursive" $
  doc "Recursively strip all annotations from a type scheme" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "typ" <~ Core.typeSchemeType (var "ts") $
  Core.typeScheme (var "vars") (deannotateTypeRecursive @@ var "typ")

detypeTerm :: TBinding (Term -> Term)
detypeTerm = define "detypeTerm" $
  doc "Strip System F type annotations from the top levels of a term, but leave application-specific annotations intact" $
  "t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~>
       "subj" <~ Core.annotatedTermBody (var "at") $
       "ann" <~ Core.annotatedTermAnnotation (var "at") $
       Core.termAnnotated $ Core.annotatedTerm (detypeTerm @@ var "subj") (var "ann"),
    _Term_typeApplication>>: "tt" ~> deannotateAndDetypeTerm @@ (Core.typeApplicationTermBody $ var "tt"),
    _Term_typeLambda>>: "ta" ~> deannotateAndDetypeTerm @@ (Core.typeLambdaBody $ var "ta")]

flattenLetTerms :: TBinding (Term -> Term)
flattenLetTerms = define "flattenLetTerms" $
  doc "Flatten nested let expressions" $
  "term" ~>
  "rewriteBinding" <~ ("binding" ~>
    "key0" <~ Core.bindingName (var "binding") $
    "val0" <~ Core.bindingTerm (var "binding") $
    "t" <~ Core.bindingType (var "binding") $
    cases _Term (var "val0")
      (Just $ pair (Core.binding (var "key0") (var "val0") (var "t")) (list ([] :: [TTerm Binding]))) [
      _Term_annotated>>: "at" ~>
        "val1" <~ Core.annotatedTermBody (var "at") $
        "ann" <~ Core.annotatedTermAnnotation (var "at") $
        "recursive" <~ var "rewriteBinding" @@ (Core.binding (var "key0") (var "val1") (var "t")) $
        "innerBinding" <~ Pairs.first (var "recursive") $
        "deps" <~ Pairs.second (var "recursive") $
        "val2" <~ Core.bindingTerm (var "innerBinding") $
        pair
          (Core.binding (var "key0") (Core.termAnnotated $ Core.annotatedTerm (var "val2") (var "ann")) (var "t"))
          (var "deps"),
      _Term_let>>: "innerLet" ~>
        "bindings1" <~ Core.letBindings (var "innerLet") $
        "body1" <~ Core.letBody (var "innerLet") $
        "prefix" <~ Strings.cat2 (unwrap _Name @@ var "key0") (string "_") $
        "qualify" <~ ("n" ~> Core.name $ Strings.cat2 (var "prefix") (unwrap _Name @@ var "n")) $
        "toSubstPair" <~ ("b" ~> pair (Core.bindingName $ var "b") (var "qualify" @@ (Core.bindingName $ var "b"))) $
        "subst" <~ Maps.fromList (Lists.map (var "toSubstPair") (var "bindings1")) $
        "replaceVars" <~ substituteVariables @@ var "subst" $
        "newBody" <~ var "replaceVars" @@ var "body1" $
        "newBinding" <~ ("b" ~> Core.binding
          (var "qualify" @@ (Core.bindingName $ var "b"))
          (var "replaceVars" @@ (Core.bindingTerm $ var "b"))
          (Core.bindingType $ var "b")) $
        pair
          (Core.binding (var "key0") (var "newBody") (var "t"))
          (Lists.map (var "newBinding") (var "bindings1"))]) $
  "flatten" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    cases _Term (var "rewritten")
      (Just $ var "rewritten") [
      _Term_let>>: "lt" ~>
        "bindings" <~ Core.letBindings (var "lt") $
        "body" <~ Core.letBody (var "lt") $
        "forResult" <~ ("hr" ~> Lists.cons (Pairs.first $ var "hr") (Pairs.second $ var "hr")) $
        "newBindings" <~ Lists.concat (Lists.map (var "forResult" <.> var "rewriteBinding") (var "bindings")) $
        Core.termLet $ Core.let_ (var "newBindings") (var "body")]) $
  rewriteTerm @@ var "flatten" @@ var "term"

foldOverTerm :: TBinding (TraversalOrder -> (x -> Term -> x) -> x -> Term -> x)
foldOverTerm = define "foldOverTerm" $
  doc "Fold over a term, traversing its subterms in the specified order" $
  "order" ~> "fld" ~> "b0" ~> "term" ~> cases _TraversalOrder (var "order") Nothing [
    _TraversalOrder_pre>>: constant (Phantoms.fold (foldOverTerm @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "term")
      @@ (subterms @@ var "term")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Phantoms.fold (foldOverTerm @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (subterms @@ var "term"))
      @@ var "term")]

foldOverType :: TBinding (TraversalOrder -> (x -> Type -> x) -> x -> Type -> x)
foldOverType = define "foldOverType" $
  doc "Fold over a type, traversing its subtypes in the specified order" $
  "order" ~> "fld" ~> "b0" ~> "typ" ~> cases _TraversalOrder (var "order") Nothing [
    _TraversalOrder_pre>>: constant (Phantoms.fold (foldOverType @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "typ")
      @@ (subtypes @@ var "typ")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Phantoms.fold (foldOverType @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (subtypes @@ var "typ"))
      @@ var "typ")]

--freeTypeVariablesInTerm :: TBinding (Term -> S.Set Name)
--freeTypeVariablesInTerm = define "freeTypeVariablesInTerm" $
--  doc "Find free type variables introduced by type applications within a term." $
--  "term" ~> cases _Term (var "term")
--    (Just $ Lists.foldl (binaryFunction Sets.union) Sets.empty $
--      Lists.map (freeTypeVariablesInTerm) (subterms @@ var "term")) [
--    _Term_typeApplication>>: "tt" ~> Sets.union
--      (freeVariablesInType @@ (Core.typeApplicationTermType $ var "tt"))
--      (freeTypeVariablesInTerm @@ (Core.typeApplicationTermBody $ var "tt")),
--    _Term_typeLambda>>: "tl" ~>
--      "tmp" <~ freeTypeVariablesInTerm @@ (Core.typeLambdaBody $ var "tl") $
--      Sets.delete (Core.typeLambdaParameter $ var "tl") (var "tmp")]

freeTypeVariablesInTerm :: TBinding (Term -> S.Set Name)
freeTypeVariablesInTerm = define "freeTypeVariablesInTerm" $
  doc ("Get the set of free type variables in a term (including schema names, where they appear in type annotations)."
    <> " In this context, only the type schemes of let bindings can bind type variables; type lambdas do not.") $
  "term0" ~>
  "allOf" <~ ("sets" ~> Lists.foldl (binaryFunction Sets.union) Sets.empty $ var "sets") $
  "tryType" <~ ("tvars" ~> "typ" ~> Sets.difference (freeVariablesInType @@ var "typ") (var "tvars")) $
  "getAll" <~ ("vars" ~> "term" ~>
    "recurse" <~ var "getAll" @@ var "vars" $
    "dflt" <~ (var "allOf" @@ Lists.map (var "recurse") (subterms @@ var "term")) $
    cases _Term (var "term")
      (Just $ var "dflt") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "dflt") [
        _Function_elimination>>: "e" ~> var "dflt",
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
          (var "recurse" @@ (Core.letBody $ var "l")),
      _Term_typeApplication>>: "tt" ~>
        Sets.union
          (var "tryType" @@ var "vars" @@ (Core.typeApplicationTermType $ var "tt"))
          (var "recurse" @@ (Core.typeApplicationTermBody $ var "tt")),
      _Term_typeLambda>>: "tl" ~>
        Sets.union
          -- The type variable introduced by a type lambda is considered unbound unless it is also introduced in an
          -- enclosing let binding, as all type lambda terms are in Hydra.
          (var "tryType" @@ var "vars" @@ (Core.typeVariable $ Core.typeLambdaParameter $ var "tl"))
          (var "recurse" @@ (Core.typeLambdaBody $ var "tl"))]) $
  var "getAll" @@ Sets.empty @@ var "term0"

freeVariablesInTerm :: TBinding (Term -> S.Set Name)
freeVariablesInTerm = define "freeVariablesInTerm" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a term" $
  "term" ~>
  "dfltVars" <~ Lists.foldl ("s" ~> "t" ~> Sets.union (var "s") (freeVariablesInTerm @@ var "t"))
    Sets.empty
    (subterms @@ var "term") $
  cases _Term (var "term")
    (Just $ var "dfltVars") [
    _Term_function>>: match _Function (Just $ var "dfltVars") [
      _Function_lambda>>: "l" ~> Sets.delete
        (Core.lambdaParameter $ var "l")
        (freeVariablesInTerm @@ (Core.lambdaBody $ var "l"))],
    _Term_let>>: "l" ~> (Sets.difference
      (var "dfltVars")
      (Sets.fromList (Lists.map (unaryFunction Core.bindingName) (Core.letBindings $ var "l")))),
    _Term_variable>>: "v" ~> Sets.singleton $ var "v"]

--freeVariablesInTermOpt :: TBinding (Term -> S.Set Name)
--freeVariablesInTermOpt = define "freeVariablesInTermOpt" $
--  doc ("A possibly more efficient function to find the free variables (i.e. variables not bound by a lambda or let) in a term") $
--  "term0" ~>
--  "gather" ~> ("s" ~> "locallyBound" ~> "term"  ~>
--    "dflt" <~ (Lists.foldl
--      ("s1" ~> "term1" ~> var "gather" @@ var "s1" @@ var "locallyBound" @@ var "term1")
--      (var "s")
--      (subterms @@ var "term")) $
--    cases _Term (var "term")
--      (Just $ var "dflt") [
--      _Term_function>>: "f" ~> cases _Function (var "f")
--        (Just $ var "dflt") [
--        _Function_lambda>>: "l" ~> var "gather" @@ var "s"
--          @@ (S.insert (Core.lambdaParameter $ var "l") (var "locallyBound"))
--          @@ (Core.lambdaBody $ var "l")],
--      _Term_let>>: "l" ~>
--        "locallyBound1" <~ S.union (var "locallyBound") (Sets.fromList $ Lists.map (unaryFunction Core.bindingName) (Core.letBindings $ var "l")) $
--        "forBinding" <~ ("s1" ~> "b" ~> var "gather" @@ var "s1" @@ var "locallyBound1" @@ (Core.bindingTerm $ var "b")) $
--        Lists.foldl
--          (var "forBinding")
--          (var "gather"
--            @@ var "s"
--            @@ var "locallyBound1"
--            @@ (Core.letBody $ var "l"))
--          (Core.letBindings $ var "l"),
--      _Term_variable>>: "name" ~> Logic.ifElse (Sets.member (var "name") (var "locallyBound"))
--        (var "s")
--        (Sets.insert (var "name") (var "s"))]) $
--  "gather" @@ Sets.empty @@ Sets.empty @@ var "term0"

freeVariablesInType :: TBinding (Type -> S.Set Name)
freeVariablesInType = define "freeVariablesInType" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a type" $
  "typ" ~>
  "dfltVars" <~ Phantoms.fold ("s" ~> "t" ~> Sets.union (var "s") (recurse @@ var "t"))
    @@ Sets.empty
    @@ (subtypes @@ var "typ") $
  cases _Type (var "typ")
    (Just $ var "dfltVars") [
    _Type_forall>>: "lt" ~> Sets.delete
        (Core.forallTypeParameter $ var "lt")
        (recurse @@ (Core.forallTypeBody $ var "lt")),
    -- TODO: let-types
    _Type_variable>>: "v" ~> Sets.singleton $ var "v"]
  where
    recurse = freeVariablesInType

freeVariablesInTypeOrdered :: TBinding (Type -> [Name])
freeVariablesInTypeOrdered = define "freeVariablesInTypeOrdered" $
  doc "Find the free variables in a type in deterministic left-to-right order" $
  "typ" ~>
  "collectVars" <~ ("boundVars" ~> "t" ~>
    cases _Type (var "t")
      (Just $ Lists.concat $ Lists.map (var "collectVars" @@ var "boundVars") $
              subtypes @@ var "t") [
      _Type_variable>>: "v" ~>
        Logic.ifElse (Sets.member (var "v") (var "boundVars"))
          (list ([] :: [TTerm Name]))
          (list [var "v"]),
      _Type_forall>>: "ft" ~>
        var "collectVars" @@
          (Sets.insert (Core.forallTypeParameter $ var "ft") (var "boundVars")) @@
          (Core.forallTypeBody $ var "ft")]) $
  (Lists.nub :: TTerm [Name] -> TTerm [Name]) $ var "collectVars" @@ Sets.empty @@ var "typ"

freeVariablesInTypeSimple :: TBinding (Type -> S.Set Name)
freeVariablesInTypeSimple = define "freeVariablesInTypeSimple" $
  doc "Same as freeVariablesInType, but ignores the binding action of lambda types" $
  "typ" ~>
  "helper" <~ ("types" ~> "typ" ~> cases _Type (var "typ")
    (Just $ var "types") [
    _Type_variable>>: "v" ~> Sets.insert (var "v") (var "types")]) $
  foldOverType @@ Coders.traversalOrderPre @@ var "helper" @@ Sets.empty @@ var "typ"

freeVariablesInTypeScheme :: TBinding (TypeScheme -> S.Set Name)
freeVariablesInTypeScheme = define "freeVariablesInTypeScheme" $
  doc "Find free variables in a type scheme" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "t" <~ Core.typeSchemeType (var "ts") $
  Sets.difference (freeVariablesInType @@ var "t") (Sets.fromList $ var "vars")

freeVariablesInTypeSchemeSimple :: TBinding (TypeScheme -> S.Set Name)
freeVariablesInTypeSchemeSimple = define "freeVariablesInTypeSchemeSimple" $
  doc "Find free variables in a type scheme (simple version)" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "t" <~ Core.typeSchemeType (var "ts") $
  Sets.difference (freeVariablesInTypeSimple @@ var "t") (Sets.fromList $ var "vars")

inlineType :: TBinding (M.Map Name Type -> Type -> Flow s Type)
inlineType = define "inlineType" $
  doc "Inline all type variables in a type using the provided schema. Note: this function is only appropriate for nonrecursive type definitions" $
  "schema" ~> "typ" ~>
  "f" <~ ("recurse" ~> "typ" ~>
    "afterRecurse" <~ ("tr" ~> cases _Type (var "tr")
      (Just $ produce $ var "tr") [
      _Type_variable>>: "v" ~>
        Maybes.maybe
          (Flows.fail $ Strings.cat2 (string "No such type in schema: ") (unwrap _Name @@ var "v"))
          (inlineType @@ var "schema")
          (Maps.lookup (var "v") (var "schema"))]) $
    "tr" <<~ var "recurse" @@ var "typ" $
    var "afterRecurse" @@ var "tr") $
  rewriteTypeM @@ var "f" @@ var "typ"

isFreeVariableInTerm :: TBinding (Name -> Term -> Bool)
isFreeVariableInTerm = define "isFreeVariableInTerm" $
 doc "Check whether a variable is free (not bound) in a term" $
 "v" ~> "term" ~>
   Logic.not $ Sets.member (var "v") (freeVariablesInTerm @@ var "term")

isLambda :: TBinding (Term -> Bool)
isLambda = define "isLambda" $
  doc "Check whether a term is a lambda, possibly nested within let and/or annotation terms" $
  "term" ~> cases _Term (deannotateTerm @@ var "term")
    (Just false) [
    _Term_function>>: match _Function
      (Just false) [
      _Function_lambda>>: constant true],
    _Term_let>>: "lt" ~> isLambda @@ (project _Let _Let_body @@ var "lt")]

-- TODO: account for shadowing among let- and lambda-bound variables
liftLambdaAboveLet :: TBinding (Term -> Term)
liftLambdaAboveLet = define "liftLambdaAboveLet" $
  doc ("Rewrite terms like `let foo = bar in λx.baz` to `λx.let foo = bar in baz`, lifting lambda-bound variables"
    <> " above let-bound variables, recursively. This is helpful for targets such as Python.") $
  "term0" ~>
  "rewrite" <~ ("recurse" ~> "term" ~>
    "rewriteBinding" <~ ("b" ~> Core.bindingWithTerm (var "b") $ var "rewrite" @@ var "recurse" @@ Core.bindingTerm (var "b")) $
    "rewriteBindings" <~ ("bs" ~> Lists.map (var "rewriteBinding") (var "bs")) $
    "digForLambdas" <~ ("original" ~> "cons" ~> "term" ~> cases _Term (var"term")
      (Just $ var "recurse" @@ var "original") [
      _Term_annotated>>: "at" ~> var "digForLambdas"
        @@ var "original"
        @@ ("t" ~> Core.termAnnotated $ Core.annotatedTermWithBody (var "at") (var "cons" @@ var "t"))
        @@ (Core.annotatedTermBody $ var "at"),
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "recurse" @@ var "original") [
        _Function_lambda>>: "l" ~> Core.termFunction $ Core.functionLambda $ Core.lambdaWithBody (var "l") $
          var "digForLambdas"
            @@ (var "cons" @@ (Core.lambdaBody $ var "l"))
            @@ ("t" ~> var "cons" @@ var "t")
            @@ (Core.lambdaBody $ var "l")],
      _Term_let>>: "l" ~> var "digForLambdas"
        @@ var "original"
        @@ ("t" ~> var "cons" @@ (Core.termLet $ Core.let_ (var "rewriteBindings" @@ (Core.letBindings $ var "l")) (var "t")))
        @@ Core.letBody (var "l")]) $
    -- Note: we match *before* recursing for the sake of efficiency.
    cases _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      _Term_let>>: "l" ~> var "digForLambdas"
        @@ var "term"
        @@ ("t" ~> Core.termLet $ Core.let_ (var "rewriteBindings" @@ (Core.letBindings $ var "l")) (var "t"))
        @@ Core.letBody (var "l")]) $
  rewriteTerm @@ var "rewrite" @@ var "term0"

mapBeneathTypeAnnotations :: TBinding ((Type -> Type) -> Type -> Type)
mapBeneathTypeAnnotations = define "mapBeneathTypeAnnotations" $
  doc "Apply a transformation to the first type beneath a chain of annotations" $
  "f" ~> "t" ~> cases _Type (var "t")
    (Just $ var "f" @@ var "t") [
    _Type_annotated>>: "at" ~> Core.typeAnnotated $ Core.annotatedType
      (mapBeneathTypeAnnotations @@ var "f" @@ (Core.annotatedTypeBody $ var "at"))
      (Core.annotatedTypeAnnotation $ var "at")]

normalizeTypeVariablesInTerm :: TBinding (Term -> Term)
normalizeTypeVariablesInTerm = define "normalizeTypeVariablesInTerm" $
  doc "Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ..." $
  "term" ~>
  "replaceName" <~ ("subst" ~> "v" ~> Maybes.fromMaybe (var "v") $ Maps.lookup (var "v") (var "subst")) $
  "substType" <~ ("subst" ~> "typ" ~>
    "rewrite" <~ ("recurse" ~> "typ" ~> cases _Type (var "typ")
      (Just $ var "recurse" @@ var "typ") [
      _Type_variable>>: "v" ~> Core.typeVariable $ var "replaceName" @@ var "subst" @@ var "v"]) $
    rewriteType @@ var "rewrite" @@ var "typ") $
  -- Thread a triple: ((subst, boundVars), next)
  "rewriteWithSubst" <~ ("state" ~> "term0" ~>
    "sb"   <~ Pairs.first  (var "state") $
    "next" <~ Pairs.second (var "state") $
    "subst"     <~ Pairs.first  (var "sb") $
    "boundVars" <~ Pairs.second (var "sb") $
    "rewrite" <~ ("recurse" ~> "term" ~> cases _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      -- Lambdas have a "domain" type which needs to be rewritten
      _Term_function>>: match _Function
        (Just $ var "recurse" @@ var "term") [
        _Function_elimination>>: constant $ var "recurse" @@ var "term",
        _Function_lambda>>: "l" ~>
          "domain" <~ Core.lambdaDomain (var "l") $
          Core.termFunction $ Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Maybes.map (var "substType" @@ var "subst") (var "domain"))
            (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.lambdaBody $ var "l"))],
      -- Let bindings each have a type which needs to be rewritten
      _Term_let>>: "lt" ~>
        "bindings0" <~ Core.letBindings (var "lt") $
        "body0"     <~ Core.letBody (var "lt") $
        -- Sequentially rewrite bindings without advancing 'next' across siblings
        "step" <~ ("acc" ~> "bs" ~>
          "b"  <~ Lists.head (var "bs") $
          "tl" <~ Lists.tail (var "bs") $
          "noType" <~ (
            "newVal" <~ var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.bindingTerm $ var "b") $
            "b1"     <~ Core.binding (Core.bindingName $ var "b") (var "newVal") nothing $
            var "step" @@ (Lists.cons (var "b1") (var "acc")) @@ var "tl") $
          "withType" <~ ("ts" ~>
            "vars" <~ Core.typeSchemeVariables (var "ts") $
            "typ"  <~ Core.typeSchemeType (var "ts") $
            "k"    <~ Lists.length (var "vars") $
            -- Build exactly k fresh names t{next}, t{next+1}, ...
            "gen"  <~ ("i" ~> "rem" ~> "acc2" ~>
              "ti" <~ Core.name (Strings.cat2 (string "t") (Literals.showInt32 (Math.add (var "next") (var "i")))) $
              Logic.ifElse (Equality.equal (var "rem") (int32 0))
                (Lists.reverse (var "acc2"))
                (var "gen"
                  @@ (Math.add (var "i") (int32 1))
                  @@ (Math.sub (var "rem") (int32 1))
                  @@ (Lists.cons (var "ti") (var "acc2")))) $
            "newVars"  <~ var "gen" @@ (int32 0) @@ (var "k") @@ (list ([] :: [TTerm Name])) $
            "newSubst" <~ Maps.union (Maps.fromList $ Lists.zip (var "vars") (var "newVars")) (var "subst") $
            "newBound" <~ Sets.union (var "boundVars") (Sets.fromList (var "newVars")) $
            "newVal"   <~ var "rewriteWithSubst" @@ (pair (pair (var "newSubst") (var "newBound")) (Math.add (var "next") (var "k"))) @@ (Core.bindingTerm $ var "b") $
            "b1"       <~ Core.binding
              (Core.bindingName $ var "b")
              (var "newVal")
              (just $ Core.typeScheme (var "newVars") (var "substType" @@ var "newSubst" @@ var "typ")) $
            -- Note: do not advance 'next' for the next sibling; keep current 'next'
            var "step" @@ (Lists.cons (var "b1") (var "acc")) @@ var "tl") $
          Logic.ifElse (Lists.null (var "bs"))
            (Lists.reverse (var "acc"))
            (optCases (Core.bindingType $ var "b")
               -- Untyped binding: rewrite its term with current state; 'next' unchanged for siblings
               (var "noType")
               -- Typed binding: allocate |vars| fresh t{next+i}; bump 'next' only for the binding's TERM
               ("ts" ~> var "withType" @@ var "ts"))) $
        "bindings1" <~ var "step" @@ (list ([] :: [TTerm Binding])) @@ (var "bindings0") $
        Core.termLet $ Core.let_
          (var "bindings1")
          -- Body sees the original 'next' (binding lambdas don't bind in the body)
          (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ var "body0"),
      -- Type application terms have a type which needs to be rewritten, and we also recurse into the body term.
      _Term_typeApplication>>: "tt" ~> Core.termTypeApplication $ Core.typeApplicationTerm
        (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.typeApplicationTermBody $ var "tt"))
        (var "substType" @@ var "subst" @@ (Core.typeApplicationTermType $ var "tt")),
      -- Type lambdas introduce a type variable which needs to be replaced, and we also recurse into the body term.
      -- Note: in Hydra currently, type lambdas are exclusively created during type inference in combination with
      -- polymorphic let bindings, so the type variable should already be present in the substitution.
      -- If "free-standing" type lambdas are ever supported in the future, we will have to create a fresh type variable here.
      _Term_typeLambda>>: "ta" ~> Core.termTypeLambda $ Core.typeLambda
        (var "replaceName" @@ var "subst" @@ (Core.typeLambdaParameter $ var "ta"))
        (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.typeLambdaBody $ var "ta"))]) $
    rewriteTerm @@ var "rewrite" @@ var "term0") $
  -- initial state: ((emptySubst, emptyBound), next=0)
  var "rewriteWithSubst" @@ (pair (pair Maps.empty Sets.empty) (int32 0)) @@ var "term"

pruneLet :: TBinding (Let -> Let)
pruneLet = define "pruneLet" $
  doc ("Given a let expression, remove any unused bindings. The resulting expression is still a let,"
    <> " even if has no remaining bindings") $
  "l" ~>
  "bindingMap" <~ Maps.fromList (Lists.map
    ("b" ~> pair (Core.bindingName $ var "b") (Core.bindingTerm $ var "b")) $ Core.letBindings $ var "l") $
  "rootName" <~ Core.name (string "[[[root]]]") $
  "adj" <~ ("n" ~> Sets.intersection (Sets.fromList $ Maps.keys $ var "bindingMap")
      (freeVariablesInTerm @@ (Logic.ifElse (Equality.equal (var "n") (var "rootName"))
        (Core.letBody $ var "l")
        (Maybes.fromJust $ Maps.lookup (var "n") (var "bindingMap"))))) $
  "reachable" <~ Sorting.findReachableNodes @@ var "adj" @@ var "rootName" $
  "prunedBindings" <~ Lists.filter
    ("b" ~> Sets.member (Core.bindingName $ var "b") (var "reachable"))
    (Core.letBindings $ var "l") $
  Core.let_
    (var "prunedBindings")
    (Core.letBody $ var "l")

removeTermAnnotations :: TBinding (Term -> Term)
removeTermAnnotations = define "removeTermAnnotations" $
  doc "Recursively remove term annotations, including within subterms" $
  "term" ~>
  "remove" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    cases _Term (var "term")
      (Just $ var "rewritten") [
      _Term_annotated>>: "at" ~> Core.annotatedTermBody $ var "at"]) $
  rewriteTerm @@ var "remove" @@ var "term"

removeTypeAnnotations :: TBinding (Type -> Type)
removeTypeAnnotations = define "removeTypeAnnotations" $
  doc "Recursively remove type annotations, including within subtypes" $
  "typ" ~>
  "remove" <~ ("recurse" ~> "typ" ~>
    "rewritten" <~ var "recurse" @@ var "typ" $
    cases _Type (var "rewritten")
      (Just $ var "rewritten") [
      _Type_annotated>>: "at" ~> Core.annotatedTypeBody $ var "at"]) $
  rewriteType @@ var "remove" @@ var "typ"

removeTypesFromTerm :: TBinding (Term -> Term)
removeTypesFromTerm = define "removeTypesFromTerm" $
  doc "Strip type annotations from terms while preserving other annotations" $
  "term" ~>
  "strip" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    "stripBinding" <~ ("b" ~> Core.binding
      (Core.bindingName $ var "b")
      (Core.bindingTerm $ var "b")
      nothing) $
    cases _Term (var "rewritten")
      (Just $ var "rewritten") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ Core.termFunction $ var "f") [
        _Function_elimination>>: "e" ~> Core.termFunction $ Core.functionElimination $ var "e",
        _Function_lambda>>: "l" ~> Core.termFunction $ Core.functionLambda $ Core.lambda
          (Core.lambdaParameter $ var "l")
          nothing
          (Core.lambdaBody $ var "l")],
      _Term_let>>: "lt" ~> Core.termLet $ Core.let_
        (Lists.map (var "stripBinding") (Core.letBindings $ var "lt"))
        (Core.letBody $ var "lt"),
      _Term_typeApplication>>: "tt" ~> Core.typeApplicationTermBody $ var "tt",
      _Term_typeLambda>>: "ta" ~> Core.typeLambdaBody $ var "ta"]) $
  rewriteTerm @@ var "strip" @@ var "term"

replaceFreeTermVariable :: TBinding (Name -> Term -> Term -> Term)
replaceFreeTermVariable = define "replaceFreeTermVariable" $
  doc "Replace a free variable in a term" $
  "vold" ~> "tnew" ~> "term" ~>
  "rewrite" <~ ("recurse" ~> "t" ~> cases _Term (var "t")
    (Just $ var "recurse" @@ var "t") [
    _Term_function>>: "f" ~>
      cases _Function (var "f")
        (Just $ var "recurse" @@ var "t") [
        _Function_lambda>>: "l" ~>
          "v" <~ Core.lambdaParameter (var "l") $
          Logic.ifElse (Equality.equal (var "v") (var "vold"))
            (var "t")
            (var "recurse" @@ var "t")],
    _Term_variable>>: "v" ~>
      Logic.ifElse (Equality.equal (var "v") (var "vold"))
        (var "tnew")
        (Core.termVariable $ var "v")]) $
  rewriteTerm @@ var "rewrite" @@ var "term"

replaceFreeTypeVariable :: TBinding (Name -> Type -> Type -> Type)
replaceFreeTypeVariable = define "replaceFreeTypeVariable" $
  doc "Replace free occurrences of a name in a type" $
  "v" ~> "rep" ~> "typ" ~>
  "mapExpr" <~ ("recurse" ~> "t" ~> cases _Type (var "t")
    (Just $ var "recurse" @@ var "t") [
    _Type_forall>>: "ft" ~> Logic.ifElse
      (Equality.equal (var "v") (Core.forallTypeParameter $ var "ft"))
      (var "t")
      (Core.typeForall $ Core.forallType
        (Core.forallTypeParameter $ var "ft")
        (var "recurse" @@ (Core.forallTypeBody $ var "ft"))),
    _Type_variable>>: "v'" ~> Logic.ifElse
      (Equality.equal (var "v") (var "v'"))
      (var "rep")
      (var "t")]) $
  rewriteType @@ var "mapExpr" @@ var "typ"

replaceTypedefs :: TBinding (M.Map Name TypeScheme -> Type -> Type)
replaceTypedefs = define "replaceTypedefs" $
  doc "Replace all occurrences of simple typedefs (type aliases) with the aliased types, recursively" $
  "types" ~> "typ0" ~>
  "rewrite" <~ ("recurse" ~> "typ" ~>
    "dflt" <~ (var "recurse" @@ var "typ") $
    cases _Type (var "typ")
      (Just $ var "dflt") [
--      _Type_forall>>: "ft" ~> ... -- TODO: shadowing via forall-bound variables
      _Type_annotated>>: "at" ~> var "rewrite" @@ var "recurse" @@ (Core.annotatedTypeBody $ var "at"),
      _Type_record>>: constant $ var "typ",
      _Type_union>>: constant $ var "typ",
      _Type_variable>>: "v" ~>
        "forMono" <~ ("t" ~> cases _Type (var "t")
          (Just $ var "rewrite" @@ var "recurse" @@ var "t") [
          _Type_record>>: constant $ var "dflt",
          _Type_union>>: constant $ var "dflt",
          _Type_wrap>>: constant $ var "dflt"]) $
        "forTypeScheme" <~ ("ts" ~>
          "t" <~ Core.typeSchemeType (var "ts") $
          Logic.ifElse (Lists.null $ Core.typeSchemeVariables $ var "ts")
            (var "forMono" @@ var "t")
            (var "dflt")) $ -- TODO: this may be too simple
        optCases (Maps.lookup (var "v") (var "types"))
        (var "dflt")
        ("ts" ~> var "forTypeScheme" @@ var "ts"),
      _Type_wrap>>: constant $ var "typ"]) $
  rewriteType @@ var "rewrite" @@ var "typ0"

-- TODO: this is a fixpoint combinator, but its type is sometimes incorrectly inferred based on how it is used.
--       For now, we generally define local "rewrite"/"recurse" helper functions rather than using this global one.
--rewrite :: TBinding (((x -> y) -> x -> y) -> ((x -> y) -> x -> y) -> x -> y)
--rewrite = define "rewrite" $ "fsub" ~> "f" ~>
--  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
--  var "recurse"

rewriteAndFoldTerm :: TBinding (((a -> Term -> (a, Term)) -> a -> Term -> (a, Term)) -> a -> Term -> (a, Term))
rewriteAndFoldTerm = define "rewriteAndFoldTerm" $
  doc "Rewrite a term, and at the same time, fold a function over it, accumulating a value" $
  "f" ~> "term0" ~>
  "fsub" <~ ("recurse" ~> "val0" ~> "term0" ~>
    "forSingle" <~ ("rec" ~> "cons" ~> "val" ~> "term" ~>
      "r" <~ var "rec" @@ var "val" @@ var "term" $
      pair (Pairs.first $ var "r") (var "cons" @@ (Pairs.second $ var "r"))) $
    "forMany" <~ ("rec" ~> "cons" ~> "val" ~> "els" ~>
      "rr" <~ Lists.foldl
        ("r" ~> "el" ~>
          "r2" <~ var "rec" @@ (Pairs.first $ var "r") @@ var "el" $
          pair (Pairs.first $ var "r2") (Lists.cons (Pairs.second $ var "r2") (Pairs.second $ var "r")))
        (pair (var "val") (list ([] :: [TTerm Term])))
        (var "els") $
      pair (Pairs.first $ var "rr") (var "cons" @@ (Lists.reverse $ Pairs.second $ var "rr"))) $
    "forField" <~ ("val" ~> "field" ~>
      "r" <~ var "recurse" @@ var "val" @@ Core.fieldTerm (var "field") $
      pair (Pairs.first $ var "r") (Core.field (Core.fieldName $ var "field") (Pairs.second $ var "r"))) $
    "forFields" <~ var "forMany" @@ var "forField" @@ ("x" ~> var "x") $
    "forPair" <~ ("val" ~> "kv" ~>
      "rk" <~ var "recurse" @@ var "val" @@ (Pairs.first $ var "kv") $
      "rv" <~ var "recurse" @@ (Pairs.first $ var "rk") @@ (Pairs.second $ var "kv") $
      pair
        (Pairs.first $ var "rv")
        (pair (Pairs.second $ var "rk") (Pairs.second $ var "rv"))) $
    "forBinding" <~ ("val" ~> "binding" ~>
      "r" <~ var "recurse" @@ var "val" @@ Core.bindingTerm (var "binding") $
      pair
        (Pairs.first $ var "r")
        (Core.binding
          (Core.bindingName $ var "binding")
          (Pairs.second $ var "r")
          (Core.bindingType $ var "binding"))) $
    "forElimination" <~ ("val" ~> "elm" ~>
      "r" <~ cases _Elimination (var "elm")
        (Just $ pair (var "val") (var "elm")) [
        _Elimination_union>>: "cs" ~>
          "rmd" <~ Maybes.map (var "recurse" @@ var "val") (Core.caseStatementDefault $ var "cs") $
          "val1" <~ optCases (var "rmd")
            (var "val")
            (unaryFunction Pairs.first) $
          "rcases" <~ var "forFields" @@ var "val1" @@ (Core.caseStatementCases $ var "cs") $
          pair
            (Pairs.first $ var "rcases")
            (Core.eliminationUnion $ Core.caseStatement
              (Core.caseStatementTypeName $ var "cs")
              (Maybes.map (unaryFunction Pairs.second) (var "rmd"))
              (Pairs.second $ var "rcases"))] $
      pair (Pairs.first $ var "r") (Pairs.second $ var "r")) $
    "forFunction" <~ ("val" ~> "fun" ~> cases _Function (var "fun")
      (Just $ pair (var "val") (var "fun")) [
      _Function_elimination>>: "elm" ~>
         "r" <~ var "forElimination" @@ var "val" @@ var "elm" $
         pair (Pairs.first $ var "r") (Core.functionElimination (Pairs.second $ var "r")),
      _Function_lambda>>: "l" ~>
        "r" <~ var "recurse" @@ var "val" @@ (Core.lambdaBody $ var "l") $
        pair
          (Pairs.first $ var "r")
          (Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Core.lambdaDomain $ var "l")
            (Pairs.second $ var "r"))]) $
    "dflt" <~ pair (var "val0") (var "term0") $
    cases _Term (var "term0")
      (Just $ var "dflt") [
      _Term_annotated>>: "at" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termAnnotated $ Core.annotatedTerm (var "t") (Core.annotatedTermAnnotation $ var "at"))
        @@ var "val0"
        @@ (Core.annotatedTermBody $ var "at"),
      _Term_application>>: "a" ~>
        "rlhs" <~ var "recurse" @@ var "val0" @@ (Core.applicationFunction $ var "a") $
        "rrhs" <~ var "recurse" @@ (Pairs.first $ var "rlhs") @@ (Core.applicationArgument $ var "a") $
        pair
          (Pairs.first $ var "rrhs")
          (Core.termApplication $ Core.application
            (Pairs.second $ var "rlhs")
            (Pairs.second $ var "rrhs")),
      _Term_either>>: "e" ~> Eithers.either_
        ("l" ~>
          "rl" <~ var "recurse" @@ var "val0" @@ var "l" $
          pair (Pairs.first $ var "rl") (Core.termEither $ left $ Pairs.second $ var "rl"))
        ("r" ~>
          "rr" <~ var "recurse" @@ var "val0" @@ var "r" $
          pair (Pairs.first $ var "rr") (Core.termEither $ right $ Pairs.second $ var "rr"))
        (var "e"),
      _Term_function>>: "f" ~> var "forSingle"
        @@ var "forFunction"
        @@ ("f" ~> Core.termFunction $ var "f")
        @@ var "val0"
        @@ var "f",
      _Term_let>>: "l" ~>
        "renv" <~ var "recurse" @@ var "val0" @@ (Core.letBody $ var "l") $
        var "forMany" @@ var "forBinding"
          @@ ("bins" ~> Core.termLet $ Core.let_ (var "bins") (Pairs.second $ var "renv"))
          @@ Pairs.first (var "renv") @@ (Core.letBindings $ var "l"),
      _Term_list>>: "els" ~> var "forMany" @@ var "recurse" @@ (unaryFunction Core.termList) @@ var "val0" @@ var "els",
      _Term_map>>: "m" ~> var "forMany" @@ var "forPair"
        @@ ("pairs" ~> Core.termMap $ Maps.fromList $ var "pairs") @@ var "val0" @@ Maps.toList (var "m"),
      _Term_maybe>>: "mt" ~> optCases (var "mt")
        (var "dflt")
        ("t" ~> var "forSingle"
          @@ var "recurse"
          @@ ("t1" ~> Core.termMaybe $ just $ var "t1")
          @@ var "val0"
          @@ var "t"),
      _Term_pair>>: "p" ~>
        "rf" <~ var "recurse" @@ var "val0" @@ (Pairs.first $ var "p") $
        "rs" <~ var "recurse" @@ (Pairs.first $ var "rf") @@ (Pairs.second $ var "p") $
        pair (Pairs.first $ var "rs") (Core.termPair $ pair (Pairs.second $ var "rf") (Pairs.second $ var "rs")),
      _Term_record>>: "r" ~> var "forMany"
        @@ var "forField"
        @@ ("fields" ~> Core.termRecord $ Core.record (Core.recordTypeName $ var "r") (var "fields"))
        @@ var "val0"
        @@ Core.recordFields (var "r" ),
      _Term_set>>: "els" ~> var "forMany"
        @@ var "recurse"
        @@ ("e" ~> Core.termSet $ Sets.fromList $ var "e")
        @@ var "val0"
        @@ (Sets.toList $ var "els"),
      _Term_typeApplication>>: "ta" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeApplicationTermType $ var "ta"))
        @@ var "val0"
        @@ (Core.typeApplicationTermBody $ var "ta"),
      _Term_typeLambda>>: "tl" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termTypeLambda $ Core.typeLambda (Core.typeLambdaParameter $ var "tl") (var "t"))
        @@ var "val0"
        @@ (Core.typeLambdaBody $ var "tl"),
      _Term_union>>: "inj" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termUnion $ Core.injection
          (Core.injectionTypeName $ var "inj")
          (Core.field (Core.fieldName $ Core.injectionField $ var "inj") (var "t")))
        @@ var "val0"
        @@ (Core.fieldTerm $ Core.injectionField $ var "inj"),
      _Term_wrap>>: "wt" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termWrap $ Core.wrappedTerm (Core.wrappedTermTypeName $ var "wt") (var "t"))
        @@ var "val0"
        @@ (Core.wrappedTermBody $ var "wt")]) $
--  rewrite @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "term0"

rewriteAndFoldTermM :: TBinding (((a -> Term -> Flow s (a, Term)) -> a -> Term -> Flow s (a, Term)) -> a -> Term -> Flow s (a, Term))
rewriteAndFoldTermM = define "rewriteAndFoldTermM" $
  doc "Monadic version: rewrite a term and fold a function over it, accumulating a value" $
  "f" ~> "term0" ~>
  "fsub" <~ ("recurse" ~> "val0" ~> "term0" ~>
    "forSingle" <~ ("rec" ~> "cons" ~> "val" ~> "term" ~>
      "r" <<~ var "rec" @@ var "val" @@ var "term" $
      produce $ pair (Pairs.first $ var "r") (var "cons" @@ (Pairs.second $ var "r"))) $
    "forMany" <~ ("rec" ~> "cons" ~> "val" ~> "els" ~>
      "rr" <<~ Flows.foldl
        ("r" ~> "el" ~>
          "r2" <<~ var "rec" @@ (Pairs.first $ var "r") @@ var "el" $
          produce $ pair (Pairs.first $ var "r2") (Lists.cons (Pairs.second $ var "r2") (Pairs.second $ var "r")))
        (pair (var "val") (list ([] :: [TTerm Term])))
        (var "els") $
      produce $ pair (Pairs.first $ var "rr") (var "cons" @@ (Lists.reverse $ Pairs.second $ var "rr"))) $
    "forField" <~ ("val" ~> "field" ~>
      "r" <<~ var "recurse" @@ var "val" @@ Core.fieldTerm (var "field") $
      produce $ pair (Pairs.first $ var "r") (Core.field (Core.fieldName $ var "field") (Pairs.second $ var "r"))) $
    "forFields" <~ var "forMany" @@ var "forField" @@ ("x" ~> var "x") $
    "forPair" <~ ("val" ~> "kv" ~>
      "rk" <<~ var "recurse" @@ var "val" @@ (Pairs.first $ var "kv") $
      "rv" <<~ var "recurse" @@ (Pairs.first $ var "rk") @@ (Pairs.second $ var "kv") $
      produce $ pair
        (Pairs.first $ var "rv")
        (pair (Pairs.second $ var "rk") (Pairs.second $ var "rv"))) $
    "forBinding" <~ ("val" ~> "binding" ~>
      "r" <<~ var "recurse" @@ var "val" @@ Core.bindingTerm (var "binding") $
      produce $ pair
        (Pairs.first $ var "r")
        (Core.binding
          (Core.bindingName $ var "binding")
          (Pairs.second $ var "r")
          (Core.bindingType $ var "binding"))) $
    "forElimination" <~ ("val" ~> "elm" ~>
      "rw" <~ ("elm" ~> cases _Elimination (var "elm")
        (Just $ produce $ pair (var "val") (var "elm")) [
        _Elimination_union>>: "cs" ~>
          "rmd" <<~ Maybes.maybe (produce nothing)
            ("def" ~> Flows.map (unaryFunction just) (var "recurse" @@ var "val" @@ var "def"))
            (Core.caseStatementDefault $ var "cs") $
          "val1" <~ Maybes.maybe (var "val") (unaryFunction Pairs.first) (var "rmd") $
          "rcases" <<~ var "forFields" @@ var "val1" @@ (Core.caseStatementCases $ var "cs") $
          produce $ pair
            (Pairs.first $ var "rcases")
            (Core.eliminationUnion $ Core.caseStatement
              (Core.caseStatementTypeName $ var "cs")
              (Maybes.map (unaryFunction Pairs.second) (var "rmd"))
              (Pairs.second $ var "rcases"))]) $
      "r" <<~ var "rw" @@ var "elm" $
      produce $ pair (Pairs.first $ var "r") (Pairs.second $ var "r")) $
    "forFunction" <~ ("val" ~> "fun" ~> cases _Function (var "fun")
      (Just $ produce $ pair (var "val") (var "fun")) [
      _Function_elimination>>: "elm" ~>
         "r" <<~ var "forElimination" @@ var "val" @@ var "elm" $
         produce $ pair (Pairs.first $ var "r") (Core.functionElimination (Pairs.second $ var "r")),
      _Function_lambda>>: "l" ~>
        "r" <<~ var "recurse" @@ var "val" @@ (Core.lambdaBody $ var "l") $
        produce $ pair
          (Pairs.first $ var "r")
          (Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Core.lambdaDomain $ var "l")
            (Pairs.second $ var "r"))]) $
    "dflt" <~ produce (pair (var "val0") (var "term0")) $
    cases _Term (var "term0")
      (Just $ var "dflt") [
      _Term_annotated>>: "at" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termAnnotated $ Core.annotatedTerm (var "t") (Core.annotatedTermAnnotation $ var "at"))
        @@ var "val0"
        @@ (Core.annotatedTermBody $ var "at"),
      _Term_application>>: "a" ~>
        "rlhs" <<~ var "recurse" @@ var "val0" @@ (Core.applicationFunction $ var "a") $
        "rrhs" <<~ var "recurse" @@ (Pairs.first $ var "rlhs") @@ (Core.applicationArgument $ var "a") $
        produce $ pair
          (Pairs.first $ var "rrhs")
          (Core.termApplication $ Core.application
            (Pairs.second $ var "rlhs")
            (Pairs.second $ var "rrhs")),
      _Term_either>>: "e" ~> Eithers.either_
        ("l" ~>
          "rl" <<~ var "recurse" @@ var "val0" @@ var "l" $
          produce $ pair (Pairs.first $ var "rl") (Core.termEither $ left $ Pairs.second $ var "rl"))
        ("r" ~>
          "rr" <<~ var "recurse" @@ var "val0" @@ var "r" $
          produce $ pair (Pairs.first $ var "rr") (Core.termEither $ right $ Pairs.second $ var "rr"))
        (var "e"),
      _Term_function>>: "f" ~> var "forSingle"
        @@ var "forFunction"
        @@ ("f" ~> Core.termFunction $ var "f")
        @@ var "val0"
        @@ var "f",
      _Term_let>>: "l" ~>
        "renv" <<~ var "recurse" @@ var "val0" @@ (Core.letBody $ var "l") $
        var "forMany" @@ var "forBinding"
          @@ ("bins" ~> Core.termLet $ Core.let_ (var "bins") (Pairs.second $ var "renv"))
          @@ Pairs.first (var "renv") @@ (Core.letBindings $ var "l"),
      _Term_list>>: "els" ~> var "forMany" @@ var "recurse" @@ (unaryFunction Core.termList) @@ var "val0" @@ var "els",
      _Term_map>>: "m" ~> var "forMany" @@ var "forPair"
        @@ ("pairs" ~> Core.termMap $ Maps.fromList $ var "pairs") @@ var "val0" @@ Maps.toList (var "m"),
      _Term_maybe>>: "mt" ~> Maybes.maybe
        (var "dflt")
        ("t" ~> var "forSingle"
          @@ var "recurse"
          @@ ("t1" ~> Core.termMaybe $ just $ var "t1")
          @@ var "val0"
          @@ var "t")
        (var "mt"),
      _Term_pair>>: "p" ~>
        "rf" <<~ var "recurse" @@ var "val0" @@ (Pairs.first $ var "p") $
        "rs" <<~ var "recurse" @@ (Pairs.first $ var "rf") @@ (Pairs.second $ var "p") $
        produce $ pair (Pairs.first $ var "rs") (Core.termPair $ pair (Pairs.second $ var "rf") (Pairs.second $ var "rs")),
      _Term_record>>: "r" ~> var "forMany"
        @@ var "forField"
        @@ ("fields" ~> Core.termRecord $ Core.record (Core.recordTypeName $ var "r") (var "fields"))
        @@ var "val0"
        @@ Core.recordFields (var "r"),
      _Term_set>>: "els" ~> var "forMany"
        @@ var "recurse"
        @@ ("e" ~> Core.termSet $ Sets.fromList $ var "e")
        @@ var "val0"
        @@ (Sets.toList $ var "els"),
      _Term_typeApplication>>: "ta" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeApplicationTermType $ var "ta"))
        @@ var "val0"
        @@ (Core.typeApplicationTermBody $ var "ta"),
      _Term_typeLambda>>: "tl" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termTypeLambda $ Core.typeLambda (Core.typeLambdaParameter $ var "tl") (var "t"))
        @@ var "val0"
        @@ (Core.typeLambdaBody $ var "tl"),
      _Term_union>>: "inj" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termUnion $ Core.injection
          (Core.injectionTypeName $ var "inj")
          (Core.field (Core.fieldName $ Core.injectionField $ var "inj") (var "t")))
        @@ var "val0"
        @@ (Core.fieldTerm $ Core.injectionField $ var "inj"),
      _Term_wrap>>: "wt" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termWrap $ Core.wrappedTerm (Core.wrappedTermTypeName $ var "wt") (var "t"))
        @@ var "val0"
        @@ (Core.wrappedTermBody $ var "wt")]) $
--  rewrite @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "term0"

rewriteTerm :: TBinding (((Term -> Term) -> Term -> Term) -> Term -> Term)
rewriteTerm = define "rewriteTerm" $ "f" ~> "term0" ~>
  "fsub" <~ ("recurse" ~> "term" ~>
    "forField" <~ ("f" ~> Core.fieldWithTerm (var "recurse" @@ (Core.fieldTerm $ var "f")) (var "f")) $
    "forElimination" <~ ("elm" ~> cases _Elimination (var "elm") Nothing [
      _Elimination_record>>: "p" ~> Core.eliminationRecord (var "p"),
      _Elimination_union>>: "cs" ~> Core.eliminationUnion $ Core.caseStatement
        (Core.caseStatementTypeName $ var "cs")
        (Maybes.map (var "recurse") (Core.caseStatementDefault $ var "cs"))
        (Lists.map (var "forField") (Core.caseStatementCases $ var "cs")),
      _Elimination_wrap>>: "name" ~> Core.eliminationWrap $ var "name"]) $
    "forFunction" <~ ("fun" ~> cases _Function (var "fun") Nothing [
      _Function_elimination>>: "elm" ~> Core.functionElimination $ var "forElimination" @@ var "elm",
      _Function_lambda>>: "l" ~> Core.functionLambda $ Core.lambda
        (Core.lambdaParameter $ var "l")
        (Core.lambdaDomain $ var "l")
        (var "recurse" @@ (Core.lambdaBody $ var "l")),
      _Function_primitive>>: "name" ~> Core.functionPrimitive $ var "name"]) $
    "forLet" <~ ("lt" ~>
      "mapBinding" <~ ("b" ~> Core.binding
        (Core.bindingName $ var "b")
        (var "recurse" @@ (Core.bindingTerm $ var "b"))
        (Core.bindingType $ var "b")) $
      Core.let_
        (Lists.map (var "mapBinding") (Core.letBindings $ var "lt"))
        (var "recurse" @@ (Core.letBody $ var "lt"))) $
    "forMap" <~ ("m" ~>
      "forPair" <~ ("p" ~> pair (var "recurse" @@ (Pairs.first $ var "p")) (var "recurse" @@ (Pairs.second $ var "p"))) $
      Maps.fromList $ Lists.map (var "forPair") $ Maps.toList $ var "m") $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: "at" ~> Core.termAnnotated $ Core.annotatedTerm
        (var "recurse" @@ (Core.annotatedTermBody $ var "at"))
        (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "a" ~> Core.termApplication $ Core.application
        (var "recurse" @@ (Core.applicationFunction $ var "a"))
        (var "recurse" @@ (Core.applicationArgument $ var "a")),
      _Term_either>>: "e" ~> Core.termEither $ Eithers.either_
        ("l" ~> left $ var "recurse" @@ var "l")
        ("r" ~> right $ var "recurse" @@ var "r")
        (var "e"),
      _Term_function>>: "fun" ~> Core.termFunction $ var "forFunction" @@ var "fun",
      _Term_let>>: "lt" ~> Core.termLet $ var "forLet" @@ var "lt",
      _Term_list>>: "els" ~> Core.termList $ Lists.map (var "recurse") (var "els"),
      _Term_literal>>: "v" ~> Core.termLiteral $ var "v",
      _Term_map>>: "m" ~> Core.termMap $ var "forMap" @@ var "m",
      _Term_maybe>>: "m" ~> Core.termMaybe $ Maybes.map (var "recurse") (var "m"),
      _Term_pair>>: "p" ~> Core.termPair $ pair
        (var "recurse" @@ (Pairs.first $ var "p"))
        (var "recurse" @@ (Pairs.second $ var "p")),
      _Term_record>>: "r" ~> Core.termRecord $ Core.record
        (Core.recordTypeName $ var "r")
        (Lists.map (var "forField") (Core.recordFields $ var "r")),
      _Term_set>>: "s" ~> Core.termSet $ Sets.fromList $ Lists.map (var "recurse") $ Sets.toList (var "s"),
      _Term_typeApplication>>: "tt" ~> Core.termTypeApplication $ Core.typeApplicationTerm
        (var "recurse" @@ (Core.typeApplicationTermBody $ var "tt"))
        (Core.typeApplicationTermType $ var "tt"),
      _Term_typeLambda>>: "ta" ~> Core.termTypeLambda $ Core.typeLambda
        (Core.typeLambdaParameter $ var "ta")
        (var "recurse" @@ (Core.typeLambdaBody $ var "ta")),
      _Term_union>>: "i" ~> Core.termUnion $ Core.injection
        (Core.injectionTypeName $ var "i")
        (var "forField" @@ (Core.injectionField $ var "i")),
      _Term_unit>>: constant Core.termUnit,
      _Term_variable>>: "v" ~> Core.termVariable $ var "v",
      _Term_wrap>>: "wt" ~> Core.termWrap $ Core.wrappedTerm
        (Core.wrappedTermTypeName $ var "wt")
        (var "recurse" @@ (Core.wrappedTermBody $ var "wt"))]) $
--  rewrite @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "term0"

rewriteTermM :: TBinding (((Term -> Flow s Term) -> Term -> Flow s Term) -> Term -> Flow s Term)
rewriteTermM = define "rewriteTermM" $
  doc "Monadic term rewriting with custom transformation function" $
  "f" ~> "term0" ~>
  "fsub" <~ ("recurse" ~> "term" ~>
    "forField" <~ ("field" ~>
      "t" <<~ var "recurse" @@ Core.fieldTerm (var "field") $
      produce $ Core.fieldWithTerm (var "t") (var "field")) $
    "forPair" <~ ("kv" ~>
      "k" <<~ var "recurse" @@ (Pairs.first $ var "kv") $
      "v" <<~ var "recurse" @@ (Pairs.second $ var "kv") $
      produce $ pair (var "k") (var "v")) $
    "mapBinding" <~ ("b" ~>
      "v" <<~ var "recurse" @@ (Core.bindingTerm $ var "b") $
      produce $ Core.binding (Core.bindingName $ var "b") (var "v") (Core.bindingType $ var "b")) $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: "at" ~>
        "ex" <<~ var "recurse" @@ Core.annotatedTermBody (var "at") $
        produce $ Core.termAnnotated $ Core.annotatedTerm (var "ex") (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "app" ~>
        "lhs" <<~ var "recurse" @@ Core.applicationFunction (var "app") $
        "rhs" <<~ var "recurse" @@ Core.applicationArgument (var "app") $
        produce $ Core.termApplication $ Core.application (var "lhs") (var "rhs"),
      _Term_either>>: "e" ~>
        "re" <<~ Eithers.either_
          ("l" ~> Flows.map (unaryFunction left) $ var "recurse" @@ var "l")
          ("r" ~> Flows.map (unaryFunction right) $ var "recurse" @@ var "r")
          (var "e") $
        produce $ Core.termEither $ var "re",
      _Term_function>>: "fun" ~>
        "forElm" <~ ("e" ~> cases _Elimination (var "e") Nothing [
          _Elimination_record>>: "p" ~> produce $ Core.functionElimination $ Core.eliminationRecord $ var "p",
          _Elimination_union>>: "cs" ~>
            "n" <~ Core.caseStatementTypeName (var "cs") $
            "def" <~ Core.caseStatementDefault (var "cs") $
            "cases" <~ Core.caseStatementCases (var "cs") $
            "rdef" <<~ Maybes.maybe (produce nothing)
              ("t" ~> Flows.map (unaryFunction just) $ var "recurse" @@ var "t")
              (var "def") $
            Flows.map
              ("rcases" ~> Core.functionElimination $ Core.eliminationUnion $
                Core.caseStatement (var "n") (var "rdef") (var "rcases"))
              (Flows.mapList (var "forField") (var "cases")),
          _Elimination_wrap>>: "name" ~> produce $ Core.functionElimination $ Core.eliminationWrap $ var "name"]) $
        "forFun" <~ ("fun" ~> cases _Function (var "fun") Nothing [
          _Function_elimination>>: "e" ~> var "forElm" @@ var "e",
          _Function_lambda>>: "l" ~>
            "v" <~ Core.lambdaParameter (var "l") $
            "d" <~ Core.lambdaDomain (var "l") $
            "body" <~ Core.lambdaBody (var "l") $
            "rbody" <<~ var "recurse" @@ var "body" $
            produce $ Core.functionLambda $ Core.lambda (var "v") (var "d") (var "rbody"),
          _Function_primitive>>: "name" ~> produce $ Core.functionPrimitive $ var "name"]) $
        "rfun" <<~ var "forFun" @@ var "fun" $
        produce $ Core.termFunction $ var "rfun",
      _Term_let>>: "lt" ~>
        "bindings" <~ Core.letBindings (var "lt") $
        "env" <~ Core.letBody (var "lt") $
        "rbindings" <<~ Flows.mapList (var "mapBinding") (var "bindings") $
        "renv" <<~ var "recurse" @@ var "env" $
        produce $ Core.termLet $ Core.let_ (var "rbindings") (var "renv"),
      _Term_list>>: "els" ~>
        "rels" <<~ Flows.mapList (var "recurse") (var "els") $
        produce $ Core.termList $ var "rels",
      _Term_literal>>: "v" ~> produce $ Core.termLiteral $ var "v",
      _Term_map>>: "m" ~>
        "pairs" <<~ Flows.mapList (var "forPair") (Maps.toList $ var "m") $
        produce $ Core.termMap $ Maps.fromList $ var "pairs",
      _Term_maybe>>: "m" ~>
        "rm" <<~ Flows.mapMaybe (var "recurse") (var "m") $
        produce $ Core.termMaybe $ var "rm",
      _Term_pair>>: "p" ~>
        "rf" <<~ var "recurse" @@ (Pairs.first $ var "p") $
        "rs" <<~ var "recurse" @@ (Pairs.second $ var "p") $
        produce $ Core.termPair $ pair (var "rf") (var "rs"),
      _Term_record>>: "r" ~>
        "n" <~ Core.recordTypeName (var "r") $
        "fields" <~ Core.recordFields (var "r") $
        Flows.map
          ("rfields" ~> Core.termRecord $ Core.record (var "n") (var "rfields"))
          (Flows.mapList (var "forField") (var "fields")),
      _Term_set>>: "s" ~>
        "rlist" <<~ Flows.mapList (var "recurse") (Sets.toList $ var "s") $
        produce $ Core.termSet $ Sets.fromList $ var "rlist",
      _Term_typeApplication>>: "tt" ~>
        "t" <<~ var "recurse" @@ Core.typeApplicationTermBody (var "tt") $
        produce $ Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeApplicationTermType (var "tt")),
      _Term_typeLambda>>: "tl" ~>
        "v" <~ Core.typeLambdaParameter (var "tl") $
        "body" <~ Core.typeLambdaBody (var "tl") $
        "rbody" <<~ var "recurse" @@ var "body" $
        produce $ Core.termTypeLambda $ Core.typeLambda (var "v") (var "rbody"),
      _Term_union>>: "i" ~>
        "n" <~ Core.injectionTypeName (var "i") $
        "field" <~ Core.injectionField (var "i") $
        Flows.map
          ("rfield" ~> Core.termUnion $ Core.injection (var "n") (var "rfield"))
          (var "forField" @@ var "field"),
      _Term_unit>>: constant $ produce Core.termUnit,
      _Term_variable>>: "v" ~> produce $ Core.termVariable $ var "v",
      _Term_wrap>>: "wt" ~>
        "name" <~ Core.wrappedTermTypeName (var "wt") $
        "t" <~ Core.wrappedTermBody (var "wt") $
        "rt" <<~ var "recurse" @@ var "t" $
        produce $ Core.termWrap $ Core.wrappedTerm (var "name") (var "rt")]) $
--  rewrite @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "term0"

rewriteTermWithContext :: TBinding (((a -> Term -> Term) -> a -> Term -> Term) -> a -> Term -> Term)
rewriteTermWithContext = define "rewriteTermWithContext" $
  doc ("A variant of rewriteTerm which allows a context (e.g. a TypeContext)"
    <> " to be passed down to all subterms during rewriting") $
  "f" ~> "cx0" ~> "term0" ~>
  "forSubterms" <~ ("recurse0" ~> "cx" ~> "term" ~>
    "recurse" <~ var "recurse0" @@ var "cx" $
    "forField" <~ ("field" ~> Core.fieldWithTerm (var "recurse" @@ (Core.fieldTerm $ var "field")) (var "field")) $
    "forElimination" <~ ("elm" ~> cases _Elimination (var "elm") Nothing [
      _Elimination_record>>: "p" ~> Core.eliminationRecord (var "p"),
      _Elimination_union>>: "cs" ~> Core.eliminationUnion $ Core.caseStatement
        (Core.caseStatementTypeName $ var "cs")
        (Maybes.map (var "recurse") (Core.caseStatementDefault $ var "cs"))
        (Lists.map (var "forField") (Core.caseStatementCases $ var "cs")),
      _Elimination_wrap>>: "name" ~> Core.eliminationWrap $ var "name"]) $
    "forFunction" <~ ("fun" ~> cases _Function (var "fun") Nothing [
      _Function_elimination>>: "elm" ~> Core.functionElimination $ var "forElimination" @@ var "elm",
      _Function_lambda>>: "l" ~> Core.functionLambda $ Core.lambda
        (Core.lambdaParameter $ var "l")
        (Core.lambdaDomain $ var "l")
        (var "recurse" @@ (Core.lambdaBody $ var "l")),
      _Function_primitive>>: "name" ~> Core.functionPrimitive $ var "name"]) $
    "forLet" <~ ("lt" ~>
      "mapBinding" <~ ("b" ~> Core.binding
        (Core.bindingName $ var "b")
        (var "recurse" @@ (Core.bindingTerm $ var "b"))
        (Core.bindingType $ var "b")) $
      Core.let_
        (Lists.map (var "mapBinding") (Core.letBindings $ var "lt"))
        (var "recurse" @@ (Core.letBody $ var "lt"))) $
    "forMap" <~ ("m" ~>
      "forPair" <~ ("p" ~> pair (var "recurse" @@ (Pairs.first $ var "p")) (var "recurse" @@ (Pairs.second $ var "p"))) $
      Maps.fromList $ Lists.map (var "forPair") $ Maps.toList $ var "m") $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: "at" ~> Core.termAnnotated $ Core.annotatedTerm
        (var "recurse" @@ (Core.annotatedTermBody $ var "at"))
        (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "a" ~> Core.termApplication $ Core.application
        (var "recurse" @@ (Core.applicationFunction $ var "a"))
        (var "recurse" @@ (Core.applicationArgument $ var "a")),
      _Term_either>>: "e" ~> Core.termEither $ Eithers.either_
        ("l" ~> left $ var "recurse" @@ var "l")
        ("r" ~> right $ var "recurse" @@ var "r")
        (var "e"),
      _Term_function>>: "fun" ~> Core.termFunction $ var "forFunction" @@ var "fun",
      _Term_let>>: "lt" ~> Core.termLet $ var "forLet" @@ var "lt",
      _Term_list>>: "els" ~> Core.termList $ Lists.map (var "recurse") (var "els"),
      _Term_literal>>: "v" ~> Core.termLiteral $ var "v",
      _Term_map>>: "m" ~> Core.termMap $ var "forMap" @@ var "m",
      _Term_maybe>>: "m" ~> Core.termMaybe $ Maybes.map (var "recurse") (var "m"),
      _Term_pair>>: "p" ~> Core.termPair $ pair
        (var "recurse" @@ (Pairs.first $ var "p"))
        (var "recurse" @@ (Pairs.second $ var "p")),
      _Term_record>>: "r" ~> Core.termRecord $ Core.record
        (Core.recordTypeName $ var "r")
        (Lists.map (var "forField") (Core.recordFields $ var "r")),
      _Term_set>>: "s" ~> Core.termSet $ Sets.fromList $ Lists.map (var "recurse") $ Sets.toList (var "s"),
      _Term_typeApplication>>: "tt" ~> Core.termTypeApplication $ Core.typeApplicationTerm
        (var "recurse" @@ (Core.typeApplicationTermBody $ var "tt"))
        (Core.typeApplicationTermType $ var "tt"),
      _Term_typeLambda>>: "ta" ~> Core.termTypeLambda $ Core.typeLambda
        (Core.typeLambdaParameter $ var "ta")
        (var "recurse" @@ (Core.typeLambdaBody $ var "ta")),
      _Term_union>>: "i" ~> Core.termUnion $ Core.injection
        (Core.injectionTypeName $ var "i")
        (var "forField" @@ (Core.injectionField $ var "i")),
      _Term_unit>>: constant Core.termUnit,
      _Term_variable>>: "v" ~> Core.termVariable $ var "v",
      _Term_wrap>>: "wt" ~> Core.termWrap $ Core.wrappedTerm
        (Core.wrappedTermTypeName $ var "wt")
        (var "recurse" @@ (Core.wrappedTermBody $ var "wt"))]) $
  "rewrite" <~ ("cx" ~> "term" ~> var "f" @@ (var "forSubterms" @@ var "rewrite") @@ var "cx" @@ var "term") $
  var "rewrite" @@ var "cx0" @@ var "term0"

rewriteTermWithContextM :: TBinding (((a -> Term -> Flow s Term) -> a -> Term -> Flow s Term) -> a -> Term -> Flow s Term)
rewriteTermWithContextM = define "rewriteTermWithContextM" $
  doc ("A variant of rewriteTermM which allows a context (e.g. a TypeContext)"
    <> " to be passed down to all subterms during rewriting") $
  "f" ~> "cx0" ~> "term0" ~>
  "forSubterms" <~ ("recurse0" ~> "cx" ~> "term" ~>
    "recurse" <~ var "recurse0" @@ var "cx" $
    "forField" <~ ("field" ~>
      "t" <<~ var "recurse" @@ Core.fieldTerm (var "field") $
      produce $ Core.fieldWithTerm (var "t") (var "field")) $
    "forPair" <~ ("kv" ~>
      "k" <<~ var "recurse" @@ (Pairs.first $ var "kv") $
      "v" <<~ var "recurse" @@ (Pairs.second $ var "kv") $
      produce $ pair (var "k") (var "v")) $
    "forElimination" <~ ("e" ~> cases _Elimination (var "e") Nothing [
      _Elimination_record>>: "p" ~> produce $ Core.functionElimination $ Core.eliminationRecord $ var "p",
      _Elimination_union>>: "cs" ~>
        "n" <~ Core.caseStatementTypeName (var "cs") $
        "def" <~ Core.caseStatementDefault (var "cs") $
        "cases" <~ Core.caseStatementCases (var "cs") $
        "rdef" <<~ Maybes.maybe (produce nothing)
          ("t" ~> Flows.map (unaryFunction just) $ var "recurse" @@ var "t")
          (var "def") $
        Flows.map
          ("rcases" ~> Core.functionElimination $ Core.eliminationUnion $
            Core.caseStatement (var "n") (var "rdef") (var "rcases"))
          (Flows.mapList (var "forField") (var "cases")),
      _Elimination_wrap>>: "name" ~> produce $ Core.functionElimination $ Core.eliminationWrap $ var "name"]) $
    "forFunction" <~ ("fun" ~> cases _Function (var "fun") Nothing [
      _Function_elimination>>: "e" ~> var "forElimination" @@ var "e",
      _Function_lambda>>: "l" ~>
        "v" <~ Core.lambdaParameter (var "l") $
        "d" <~ Core.lambdaDomain (var "l") $
        "body" <~ Core.lambdaBody (var "l") $
        "rbody" <<~ var "recurse" @@ var "body" $
        produce $ Core.functionLambda $ Core.lambda (var "v") (var "d") (var "rbody"),
      _Function_primitive>>: "name" ~> produce $ Core.functionPrimitive $ var "name"]) $
    "mapBinding" <~ ("b" ~>
      "v" <<~ var "recurse" @@ (Core.bindingTerm $ var "b") $
      produce $ Core.binding (Core.bindingName $ var "b") (var "v") (Core.bindingType $ var "b")) $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: "at" ~>
        "ex" <<~ var "recurse" @@ Core.annotatedTermBody (var "at") $
        produce $ Core.termAnnotated $ Core.annotatedTerm (var "ex") (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "app" ~>
        "lhs" <<~ var "recurse" @@ Core.applicationFunction (var "app") $
        "rhs" <<~ var "recurse" @@ Core.applicationArgument (var "app") $
        produce $ Core.termApplication $ Core.application (var "lhs") (var "rhs"),
      _Term_either>>: "e" ~>
        "re" <<~ Eithers.either_
          ("l" ~> Flows.map (unaryFunction left) $ var "recurse" @@ var "l")
          ("r" ~> Flows.map (unaryFunction right) $ var "recurse" @@ var "r")
          (var "e") $
        produce $ Core.termEither $ var "re",
      _Term_function>>: "fun" ~>
        "rfun" <<~ var "forFunction" @@ var "fun" $
        produce $ Core.termFunction $ var "rfun",
      _Term_let>>: "lt" ~>
        "bindings" <~ Core.letBindings (var "lt") $
        "body" <~ Core.letBody (var "lt") $
        "rbindings" <<~ Flows.mapList (var "mapBinding") (var "bindings") $
        "rbody" <<~ var "recurse" @@ var "body" $
        produce $ Core.termLet $ Core.let_ (var "rbindings") (var "rbody"),
      _Term_list>>: "els" ~>
        "rels" <<~ Flows.mapList (var "recurse") (var "els") $
        produce $ Core.termList $ var "rels",
      _Term_literal>>: "v" ~> produce $ Core.termLiteral $ var "v",
      _Term_map>>: "m" ~>
        "pairs" <<~ Flows.mapList (var "forPair") (Maps.toList $ var "m") $
        produce $ Core.termMap $ Maps.fromList $ var "pairs",
      _Term_maybe>>: "m" ~>
        "rm" <<~ Flows.mapMaybe (var "recurse") (var "m") $
        produce $ Core.termMaybe $ var "rm",
      _Term_pair>>: "p" ~>
        "rfirst" <<~ var "recurse" @@ Pairs.first (var "p") $
        "rsecond" <<~ var "recurse" @@ Pairs.second (var "p") $
        produce $ Core.termPair $ pair (var "rfirst") (var "rsecond"),
      _Term_record>>: "r" ~>
        "n" <~ Core.recordTypeName (var "r") $
        "fields" <~ Core.recordFields (var "r") $
        Flows.map
          ("rfields" ~> Core.termRecord $ Core.record (var "n") (var "rfields"))
          (Flows.mapList (var "forField") (var "fields")),
      _Term_set>>: "s" ~>
        "rlist" <<~ Flows.mapList (var "recurse") (Sets.toList $ var "s") $
        produce $ Core.termSet $ Sets.fromList $ var "rlist",
      _Term_typeApplication>>: "tt" ~>
        "t" <<~ var "recurse" @@ Core.typeApplicationTermBody (var "tt") $
        produce $ Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeApplicationTermType (var "tt")),
      _Term_typeLambda>>: "tl" ~>
        "v" <~ Core.typeLambdaParameter (var "tl") $
        "body" <~ Core.typeLambdaBody (var "tl") $
        "rbody" <<~ var "recurse" @@ var "body" $
        produce $ Core.termTypeLambda $ Core.typeLambda (var "v") (var "rbody"),
      _Term_union>>: "i" ~>
        "n" <~ Core.injectionTypeName (var "i") $
        "field" <~ Core.injectionField (var "i") $
        Flows.map
          ("rfield" ~> Core.termUnion $ Core.injection (var "n") (var "rfield"))
          (var "forField" @@ var "field"),
      _Term_unit>>: constant $ produce Core.termUnit,
      _Term_variable>>: "v" ~> produce $ Core.termVariable $ var "v",
      _Term_wrap>>: "wt" ~>
        "name" <~ Core.wrappedTermTypeName (var "wt") $
        "t" <~ Core.wrappedTermBody (var "wt") $
        "rt" <<~ var "recurse" @@ var "t" $
        produce $ Core.termWrap $ Core.wrappedTerm (var "name") (var "rt")]) $

  "rewrite" <~ ("cx" ~> "term" ~> var "f" @@ (var "forSubterms" @@ var "rewrite") @@ var "cx" @@ var "term") $
  var "rewrite" @@ var "cx0" @@ var "term0"

rewriteType :: TBinding (((Type -> Type) -> Type -> Type) -> Type -> Type)
rewriteType = define "rewriteType" $ "f" ~> "typ0" ~>
  "fsub" <~ ("recurse" ~> "typ" ~>
    "forField" <~ ("field" ~> Core.fieldTypeWithType (var "field") (var "recurse" @@ (Core.fieldTypeType $ var "field"))) $
    cases _Type (var "typ") Nothing [
      _Type_annotated>>: "at" ~> Core.typeAnnotated $ Core.annotatedType
        (var "recurse" @@ (Core.annotatedTypeBody $ var "at"))
        (Core.annotatedTypeAnnotation $ var "at"),
      _Type_application>>: "app" ~> Core.typeApplication $ Core.applicationType
        (var "recurse" @@ (Core.applicationTypeFunction $ var "app"))
        (var "recurse" @@ (Core.applicationTypeArgument $ var "app")),
      _Type_either>>: "et" ~> Core.typeEither $ Core.eitherType
        (var "recurse" @@ (Core.eitherTypeLeft $ var "et"))
        (var "recurse" @@ (Core.eitherTypeRight $ var "et")),
      _Type_pair>>: "pt" ~> Core.typePair $ Core.pairType
        (var "recurse" @@ (Core.pairTypeFirst $ var "pt"))
        (var "recurse" @@ (Core.pairTypeSecond $ var "pt")),
      _Type_function>>: "fun" ~> Core.typeFunction $ Core.functionType
        (var "recurse" @@ (Core.functionTypeDomain $ var "fun"))
        (var "recurse" @@ (Core.functionTypeCodomain $ var "fun")),
      _Type_forall>>: "lt" ~> Core.typeForall $ Core.forallType
        (Core.forallTypeParameter $ var "lt")
        (var "recurse" @@ (Core.forallTypeBody $ var "lt")),
      _Type_list>>: "t" ~> Core.typeList $ var "recurse" @@ var "t",
      _Type_literal>>: "lt" ~> Core.typeLiteral $ var "lt",
      _Type_map>>: "mt" ~> Core.typeMap $ Core.mapType
        (var "recurse" @@ (Core.mapTypeKeys $ var "mt"))
        (var "recurse" @@ (Core.mapTypeValues $ var "mt")),
      _Type_maybe>>: "t" ~> Core.typeMaybe $ var "recurse" @@ var "t",
      _Type_record>>: "rt" ~> Core.typeRecord $ Core.rowType
        (Core.rowTypeTypeName $ var "rt")
        (Lists.map (var "forField") (Core.rowTypeFields $ var "rt")),
      _Type_set>>: "t" ~> Core.typeSet $ var "recurse" @@ var "t",
      _Type_union>>: "rt" ~> Core.typeUnion $ Core.rowType
        (Core.rowTypeTypeName $ var "rt")
        (Lists.map (var "forField") (Core.rowTypeFields $ var "rt")),
      _Type_unit>>: constant Core.typeUnit,
      _Type_variable>>: "v" ~> Core.typeVariable $ var "v",
      _Type_wrap>>: "wt" ~> Core.typeWrap $ Core.wrappedType
        (Core.wrappedTypeTypeName $ var "wt")
        (var "recurse" @@ (Core.wrappedTypeBody $ var "wt"))]) $
--  rewrite @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "typ0"

rewriteTypeM :: TBinding (((Type -> Flow s Type) -> Type -> Flow s Type) -> Type -> Flow s Type)
rewriteTypeM = define "rewriteTypeM" $
  doc "Monadic type rewriting" $
  "f" ~> "typ0" ~>
  "fsub" <~ ("recurse" ~> "typ" ~> cases _Type (var "typ") Nothing [
    _Type_annotated>>: "at" ~>
      "t" <<~ var "recurse" @@ (Core.annotatedTypeBody $ var "at") $
      produce $ Core.typeAnnotated $ Core.annotatedType (var "t") (Core.annotatedTypeAnnotation $ var "at"),
    _Type_application>>: "at" ~>
      "lhs" <<~ var "recurse" @@ (Core.applicationTypeFunction $ var "at") $
      "rhs" <<~ var "recurse" @@ (Core.applicationTypeArgument $ var "at") $
      produce $ Core.typeApplication $ Core.applicationType (var "lhs") (var "rhs"),
    _Type_either>>: "et" ~>
      "left" <<~ var "recurse" @@ (Core.eitherTypeLeft $ var "et") $
      "right" <<~ var "recurse" @@ (Core.eitherTypeRight $ var "et") $
      produce $ Core.typeEither $ Core.eitherType (var "left") (var "right"),
    _Type_pair>>: "pt" ~>
      "pairFirst" <<~ var "recurse" @@ (Core.pairTypeFirst $ var "pt") $
      "pairSecond" <<~ var "recurse" @@ (Core.pairTypeSecond $ var "pt") $
      produce $ Core.typePair $ Core.pairType (var "pairFirst") (var "pairSecond"),
    _Type_function>>: "ft" ~>
      "dom" <<~ var "recurse" @@ (Core.functionTypeDomain $ var "ft") $
      "cod" <<~ var "recurse" @@ (Core.functionTypeCodomain $ var "ft") $
      produce $ Core.typeFunction $ Core.functionType (var "dom") (var "cod"),
    _Type_forall>>: "ft" ~>
      "b" <<~ var "recurse" @@ (Core.forallTypeBody $ var "ft") $
      produce $ Core.typeForall $ Core.forallType (Core.forallTypeParameter $ var "ft") (var "b"),
    _Type_list>>: "t" ~>
      "rt" <<~ var "recurse" @@ var "t" $
      produce $ Core.typeList $ var "rt",
    _Type_literal>>: "lt" ~> produce $ Core.typeLiteral $ var "lt",
    _Type_map>>: "mt" ~>
      "kt" <<~ var "recurse" @@ (Core.mapTypeKeys $ var "mt") $
      "vt" <<~ var "recurse" @@ (Core.mapTypeValues $ var "mt") $
      produce $ Core.typeMap $ Core.mapType (var "kt") (var "vt"),
    _Type_maybe>>: "t" ~>
      "rt" <<~ var "recurse" @@ var "t" $
      produce $ Core.typeMaybe $ var "rt",
    _Type_record>>: "rt" ~>
      "name" <~ Core.rowTypeTypeName (var "rt") $
      "fields" <~ Core.rowTypeFields (var "rt") $
      "forField" <~ ("f" ~>
        "t" <<~ var "recurse" @@ (Core.fieldTypeType $ var "f") $
        produce $ Core.fieldTypeWithType (var "f") (var "t")) $
      "rfields" <<~ Flows.mapList (var "forField") (var "fields") $
      produce $ Core.typeRecord $ Core.rowType (var "name") (var "rfields"),
    _Type_set>>: "t" ~>
      "rt" <<~ var "recurse" @@ var "t" $
      produce $ Core.typeSet $ var "rt",
    _Type_union>>: "rt" ~>
      "name" <~ Core.rowTypeTypeName (var "rt") $
      "fields" <~ Core.rowTypeFields (var "rt") $
      "forField" <~ ("f" ~>
        "t" <<~ var "recurse" @@ (Core.fieldTypeType $ var "f") $
        produce $ Core.fieldTypeWithType (var "f") (var "t")) $
      "rfields" <<~ Flows.mapList (var "forField") (var "fields") $
      produce $ Core.typeUnion $ Core.rowType (var "name") (var "rfields"),
    _Type_unit>>: constant $ produce Core.typeUnit,
    _Type_variable>>: "v" ~> produce $ Core.typeVariable $ var "v",
    _Type_wrap>>: "wt" ~>
      "t" <<~ var "recurse" @@ (Core.wrappedTypeBody $ var "wt") $
      produce $ Core.typeWrap $ Core.wrappedType (Core.wrappedTypeTypeName $ var "wt") (var "t")]) $
--  rewrite @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "typ0"

simplifyTerm :: TBinding (Term -> Term)
simplifyTerm = define "simplifyTerm" $
  doc "Simplify terms by applying beta reduction where possible" $
  "term" ~>
  "simplify" <~ ("recurse" ~> "term" ~>
    "forRhs" <~ ("rhs" ~> "var" ~> "body" ~> cases _Term (deannotateTerm @@ var "rhs")
      (Just $ var "term") [
      _Term_variable>>: "v" ~>
        simplifyTerm @@ (substituteVariable @@ var "var" @@ var "v" @@ var "body")]) $
    "forLhs" <~ ("lhs" ~> "rhs" ~>
      "forFun" <~ ("fun" ~> cases _Function (var "fun")
        (Just $ var "term") [
        _Function_lambda>>: "l" ~>
          "var" <~ Core.lambdaParameter (var "l") $
          "body" <~ Core.lambdaBody (var "l") $
          Logic.ifElse (Sets.member (var "var") (freeVariablesInTerm @@ var "body"))
            (var "forRhs" @@ var "rhs" @@ var "var" @@ var "body")
            (simplifyTerm @@ var "body")]) $
      cases _Term (deannotateTerm @@ var "lhs")
        (Just $ var "term") [
        _Term_function>>: "fun" ~> var "forFun" @@ var "fun"]) $
    "forTerm" <~ ("stripped" ~> cases _Term (var "stripped")
      (Just $ var "term") [
      _Term_application>>: "app" ~>
        "lhs" <~ Core.applicationFunction (var "app") $
        "rhs" <~ Core.applicationArgument (var "app") $
        var "forLhs" @@ var "lhs" @@ var "rhs"]) $
    "stripped" <~ deannotateTerm @@ var "term" $
    var "recurse" @@ (var "forTerm" @@ var "stripped")) $
  rewriteTerm @@ var "simplify" @@ var "term"

substituteTypeVariables :: TBinding (M.Map Name Name -> Type -> Type)
substituteTypeVariables = define "substituteTypeVariables" $
  doc "Substitute type variables in a type" $
  "subst" ~> "typ" ~>
  "replace" <~ ("recurse" ~> "typ" ~> cases _Type (var "typ")
    (Just $ var "recurse" @@ var "typ") [
    _Type_variable>>: "n" ~>
      Core.typeVariable $ Maybes.fromMaybe (var "n") $ Maps.lookup (var "n") (var "subst")]) $
  rewriteType @@ var "replace" @@ var "typ"

substituteVariable :: TBinding (Name -> Name -> Term -> Term)
substituteVariable = define "substituteVariable" $
  doc "Substitute one variable for another in a term" $
  "from" ~> "to" ~> "term" ~>
  "replace" <~ ("recurse" ~> "term" ~>
    cases _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      _Term_variable>>: "x" ~>
        Core.termVariable $ Logic.ifElse (Equality.equal (var "x") (var "from")) (var "to") (var "x"),
      _Term_function>>: match _Function
        (Just $ var "recurse" @@ var "term") [
        _Function_lambda>>: "l" ~> Logic.ifElse
          (Equality.equal (Core.lambdaParameter $ var "l") (var "from"))
          (var "term")
          (var "recurse" @@ var "term")]]) $
  rewriteTerm @@ var "replace" @@ var "term"

substituteVariables :: TBinding (M.Map Name Name -> Term -> Term)
substituteVariables = define "substituteVariables" $
  doc "Substitute multiple variables in a term" $
  "subst" ~> "term" ~>
  "replace" <~ ("recurse" ~> "term" ~>
    cases _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      _Term_variable>>: "n" ~>
        Core.termVariable $ Maybes.fromMaybe (var "n") $ Maps.lookup (var "n") (var "subst"),
      _Term_function>>: match _Function
        (Just $ var "recurse" @@ var "term") [
        _Function_lambda>>: "l" ~>
          Maybes.maybe
            (var "recurse" @@ var "term")
            (constant $ var "term")
            (Maps.lookup (Core.lambdaParameter $ var "l") (var "subst"))]]) $
  rewriteTerm @@ var "replace" @@ var "term"

subterms :: TBinding (Term -> [Term])
subterms = define "subterms" $
  doc "Find the children of a given term" $
  match _Term Nothing [
    _Term_annotated>>: "at" ~> list [Core.annotatedTermBody $ var "at"],
    _Term_application>>: "p" ~> list [
      Core.applicationFunction $ var "p",
      Core.applicationArgument $ var "p"],
    _Term_either>>: "e" ~> Eithers.either_
      ("l" ~> list [var "l"])
      ("r" ~> list [var "r"])
      (var "e"),
    _Term_function>>: match _Function
      (Just $ list ([] :: [TTerm Term])) [
      _Function_elimination>>: match _Elimination
        (Just $ list ([] :: [TTerm Term])) [
        _Elimination_union>>: "cs" ~> Lists.concat2
          (Maybes.maybe (list ([] :: [TTerm Term])) ("t" ~> list [var "t"]) (Core.caseStatementDefault $ var "cs"))
          (Lists.map (unaryFunction Core.fieldTerm) (Core.caseStatementCases $ var "cs"))],
      _Function_lambda>>: "l" ~> list [Core.lambdaBody $ var "l"]],
    _Term_let>>: "lt" ~> Lists.cons
      (Core.letBody $ var "lt")
      (Lists.map (unaryFunction Core.bindingTerm) (Core.letBindings $ var "lt")),
    _Term_list>>: "l" ~> var "l",
    _Term_literal>>: constant $ list ([] :: [TTerm Term]),
    _Term_map>>: "m" ~> Lists.concat $ Lists.map
      ("p" ~> list [Pairs.first $ var "p", Pairs.second $ var "p"])
      (Maps.toList $ var "m"),
    _Term_maybe>>: "m" ~> Maybes.maybe (list ([] :: [TTerm Term])) ("t" ~> list [var "t"]) (var "m"),
    _Term_pair>>: "p" ~> list [Pairs.first $ var "p", Pairs.second $ var "p"],
    _Term_record>>: "rt" ~> Lists.map (unaryFunction Core.fieldTerm) (Core.recordFields $ var "rt"),
    _Term_set>>: "l" ~> Sets.toList $ var "l",
    _Term_typeApplication>>: "ta" ~> list [Core.typeApplicationTermBody $ var "ta"],
    _Term_typeLambda>>: "ta" ~> list [Core.typeLambdaBody $ var "ta"],
    _Term_union>>: "ut" ~> list [Core.fieldTerm $ (Core.injectionField $ var "ut")],
    _Term_unit>>: constant $ list ([] :: [TTerm Term]),
    _Term_variable>>: constant $ list ([] :: [TTerm Term]),
    _Term_wrap>>: "n" ~> list [Core.wrappedTermBody $ var "n"]]

subtermsWithAccessors :: TBinding (Term -> [(TermAccessor, Term)])
subtermsWithAccessors = define "subtermsWithAccessors" $
  doc "Find the children of a given term" $
  match _Term Nothing [
    _Term_annotated>>: "at" ~> single Accessors.termAccessorAnnotatedBody $ Core.annotatedTermBody $ var "at",
    _Term_application>>: "p" ~> list [
      result Accessors.termAccessorApplicationFunction $ Core.applicationFunction $ var "p",
      result Accessors.termAccessorApplicationArgument $ Core.applicationArgument $ var "p"],
    _Term_either>>: "e" ~> none, -- TODO: add accessors when TermAccessor type is updated
    _Term_function>>: match _Function
      (Just none) [
      _Function_elimination>>: match _Elimination
        (Just none) [
        _Elimination_union>>: "cs" ~> Lists.concat2
          (Maybes.maybe none
            ("t" ~> single Accessors.termAccessorUnionCasesDefault $ var "t")
            (Core.caseStatementDefault $ var "cs"))
          (Lists.map
            ("f" ~> result (Accessors.termAccessorUnionCasesBranch $ Core.fieldName $ var "f") $ Core.fieldTerm $ var "f")
            (Core.caseStatementCases $ var "cs"))],
      _Function_lambda>>: "l" ~> single Accessors.termAccessorLambdaBody $ Core.lambdaBody $ var "l"],
    _Term_let>>: "lt" ~> Lists.cons
      (result Accessors.termAccessorLetEnvironment $ Core.letBody $ var "lt")
      (Lists.map
        ("b" ~> result (Accessors.termAccessorLetBinding $ Core.bindingName $ var "b") $ Core.bindingTerm $ var "b")
        (Core.letBindings $ var "lt")),
    _Term_list>>: "l" ~> Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      ("e" ~> result (Accessors.termAccessorListElement $ int32 0) $ var "e")
      (var "l"),
    _Term_literal>>: constant none,
    _Term_map>>: "m" ~> Lists.concat
      (Lists.map
        ("p" ~> list [
          -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
          result (Accessors.termAccessorMapKey $ int32 0) $ Pairs.first $ var "p",
          result (Accessors.termAccessorMapValue $ int32 0) $ Pairs.second $ var "p"])
        (Maps.toList $ var "m")),
    _Term_maybe>>: "m" ~> Maybes.maybe none
      ("t" ~> single Accessors.termAccessorOptionalTerm $ var "t")
      (var "m"),
    _Term_pair>>: "p" ~> none, -- TODO: add accessors when TermAccessor type is updated
    _Term_record>>: "rt" ~> Lists.map
      ("f" ~> result (Accessors.termAccessorRecordField $ Core.fieldName $ var "f") $ Core.fieldTerm $ var "f")
      (Core.recordFields $ var "rt"),
    _Term_set>>: "s" ~> Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      ("e" ~> result (Accessors.termAccessorListElement $ int32 0) $ var "e")
      (Sets.toList $ var "s"),
    _Term_typeApplication>>: "ta" ~>
      single Accessors.termAccessorTypeApplicationTerm $
      Core.typeApplicationTermBody $ var "ta",
    _Term_typeLambda>>: "ta" ~>
      single Accessors.termAccessorTypeLambdaBody $
      Core.typeLambdaBody $ var "ta",
    _Term_union>>: "ut" ~>
      single Accessors.termAccessorInjectionTerm $
      Core.fieldTerm $ (Core.injectionField $ var "ut"),
    _Term_unit>>: constant none,
    _Term_variable>>: constant none,
    _Term_wrap>>: "n" ~> single Accessors.termAccessorWrappedTerm $ Core.wrappedTermBody $ var "n"]
  where
    none = list ([] :: [TTerm (TermAccessor, Term)])
    single accessor term = list [result accessor term]
    result accessor term = pair accessor term
    simple term = result Accessors.termAccessorAnnotatedBody term

subtypes :: TBinding (Type -> [Type])
subtypes = define "subtypes" $
  doc "Find the children of a given type expression" $
  match _Type Nothing [
    _Type_annotated>>: "at" ~> list [Core.annotatedTypeBody $ var "at"],
    _Type_application>>: "at" ~> list [
      Core.applicationTypeFunction $ var "at",
      Core.applicationTypeArgument $ var "at"],
    _Type_either>>: "et" ~> list [
      Core.eitherTypeLeft $ var "et",
      Core.eitherTypeRight $ var "et"],
    _Type_pair>>: "pt" ~> list [
      Core.pairTypeFirst $ var "pt",
      Core.pairTypeSecond $ var "pt"],
    _Type_function>>: "ft" ~> list [
      Core.functionTypeDomain $ var "ft",
      Core.functionTypeCodomain $ var "ft"],
    _Type_forall>>: "lt" ~> list [Core.forallTypeBody $ var "lt"],
    _Type_list>>: "lt" ~> list [var "lt"],
    _Type_literal>>: constant $ list ([] :: [TTerm Type]),
    _Type_map>>: "mt" ~> list [
      Core.mapTypeKeys $ var "mt",
      Core.mapTypeValues $ var "mt"],
    _Type_maybe>>: "ot" ~> list [var "ot"],
    _Type_record>>: "rt" ~> Lists.map (unaryFunction Core.fieldTypeType) (Core.rowTypeFields $ var "rt"),
    _Type_set>>: "st" ~> list [var "st"],
    _Type_union>>: "rt" ~> Lists.map (unaryFunction Core.fieldTypeType) (Core.rowTypeFields $ var "rt"),
    _Type_unit>>: constant $ list ([] :: [TTerm Type]),
    _Type_variable>>: constant $ list ([] :: [TTerm Type]),
    _Type_wrap>>: "nt" ~> list [Core.wrappedTypeBody $ var "nt"]]

termDependencyNames :: TBinding (Bool -> Bool -> Bool -> Term -> S.Set Name)
termDependencyNames = define "termDependencyNames" $
  doc "Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that" $
  "binds" ~> "withPrims" ~> "withNoms" ~> "term0" ~>
  "addNames" <~ ("names" ~> "term" ~>
    "nominal" <~ ("name" ~> Logic.ifElse (var "withNoms")
      (Sets.insert (var "name") (var "names"))
      (var "names")) $
    "prim" <~ ("name" ~> Logic.ifElse (var "withPrims")
      (Sets.insert (var "name") (var "names"))
      (var "names")) $
    "var" <~ ("name" ~> Logic.ifElse (var "binds")
      (Sets.insert (var "name") (var "names"))
      (var "names")) $
    cases _Term (var "term")
      (Just $ var "names") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "names") [
        _Function_primitive>>: "name" ~> var "prim" @@ var "name",
        _Function_elimination>>: "e" ~> cases _Elimination (var "e")
          Nothing [
          _Elimination_record>>: "proj" ~> var "nominal" @@ (Core.projectionTypeName $ var "proj"),
          _Elimination_union>>: "caseStmt" ~> var "nominal" @@ (Core.caseStatementTypeName $ var "caseStmt"),
          _Elimination_wrap>>: "name" ~> var "nominal" @@ var "name"]],
      _Term_record>>: "record" ~> var "nominal" @@ (Core.recordTypeName $ var "record"),
      _Term_union>>: "injection" ~> var "nominal" @@ (Core.injectionTypeName $ var "injection"),
      _Term_variable>>: "name" ~> var "var" @@ var "name",
      _Term_wrap>>: "wrappedTerm" ~> var "nominal" @@ (Core.wrappedTermTypeName $ var "wrappedTerm")]) $
  foldOverTerm @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty @@ var "term0"

toShortNames :: TBinding ([Name] -> M.Map Name Name)
toShortNames = define "toShortNames" $
  doc "Generate short names from a list of fully qualified names" $
  "original" ~>
  "addName" <~ ("acc" ~> "name" ~>
    "local" <~ Names.localNameOf @@ var "name" $
    "group" <~ Maybes.fromMaybe Sets.empty (Maps.lookup (var "local") (var "acc")) $
    Maps.insert (var "local") (Sets.insert (var "name") (var "group")) (var "acc")) $
  "groupNamesByLocal" <~ ("names" ~> Lists.foldl (var "addName") Maps.empty (var "names")) $
  "groups" <~ var "groupNamesByLocal" @@ var "original" $
  "renameGroup" <~ ("localNames" ~>
    "local" <~ Pairs.first (var "localNames") $
    "names" <~ Pairs.second (var "localNames") $
    "rangeFrom" <~ ("start" ~> Lists.cons (var "start") (var "rangeFrom" @@ (Math.add (var "start") (int32 1)))) $
    "rename" <~ ("name" ~> "i" ~> pair (var "name") $ Core.name $
      Logic.ifElse (Equality.gt (var "i") (int32 1))
        (Strings.cat2 (var "local") (Literals.showInt32 $ var "i"))
        (var "local")) $
    Lists.zipWith (var "rename") (Sets.toList $ var "names") (var "rangeFrom" @@ int32 1)) $
  Maps.fromList $ Lists.concat $ Lists.map (var "renameGroup") $ Maps.toList $ var "groups"

topologicalSortBindingMap :: TBinding (M.Map Name Term -> [[(Name, Term)]])
topologicalSortBindingMap = define "topologicalSortBindingMap" $
  doc "Topological sort of connected components, in terms of dependencies between variable/term binding pairs" $
  "bindingMap" ~>
  "bindings" <~ Maps.toList (var "bindingMap") $
  "keys" <~ Sets.fromList (Lists.map (unaryFunction Pairs.first) (var "bindings")) $
  -- TODO: this function currently serves no purpose; it always yields false
  "hasTypeAnnotation" <~ ("term" ~>
    cases _Term (var "term")
      (Just false) [
      _Term_annotated>>: "at" ~> var "hasTypeAnnotation" @@ (Core.annotatedTermBody $ var "at")]) $
  "depsOf" <~ ("nameAndTerm" ~>
    "name" <~ Pairs.first (var "nameAndTerm") $
    "term" <~ Pairs.second (var "nameAndTerm") $
    pair (var "name") $ Logic.ifElse (var "hasTypeAnnotation" @@ var "term")
      (list ([] :: [TTerm Name]))
      (Sets.toList $ Sets.intersection (var "keys") $ freeVariablesInTerm @@ var "term")) $
  "toPair" <~ ("name" ~> pair (var "name") $ Maybes.fromMaybe
    (Core.termLiteral $ Core.literalString $ string "Impossible!")
    (Maps.lookup (var "name") (var "bindingMap"))) $
  Lists.map (unaryFunction $ Lists.map $ var "toPair") (Sorting.topologicalSortComponents @@ Lists.map (var "depsOf") (var "bindings"))

topologicalSortBindings :: TBinding ([Binding] -> Either [[Name]] [Name])
topologicalSortBindings = define "topologicalSortBindings" $
  doc "Topological sort of elements based on their dependencies" $
  "els" ~>
  "adjlist" <~ ("e" ~> pair
    (Core.bindingName $ var "e")
    (Sets.toList $ termDependencyNames @@ false @@ true @@ true @@ (Core.bindingTerm $ var "e"))) $
  Sorting.topologicalSort @@ Lists.map (var "adjlist") (var "els")

typeDependencyNames :: TBinding (Bool -> Type -> S.Set Name)
typeDependencyNames = define "typeDependencyNames" $
  "withSchema" ~> "typ" ~> Logic.ifElse (var "withSchema")
    (Sets.union
      (freeVariablesInType @@ var "typ")
      (typeNamesInType @@ var "typ"))
    (freeVariablesInType @@ var "typ")

typeNamesInType :: TBinding (Type -> S.Set Name)
typeNamesInType = define "typeNamesInType" $
  "typ0" ~>
  "addNames" <~ ("names" ~> "typ" ~> cases _Type (var "typ")
    (Just $ var "names") [
    _Type_record>>: "rowType" ~>
      "tname" <~ Core.rowTypeTypeName (var "rowType") $
      Sets.insert (var "tname") (var "names"),
    _Type_union>>: "rowType" ~>
      "tname" <~ Core.rowTypeTypeName (var "rowType") $
      Sets.insert (var "tname") (var "names"),
    _Type_wrap>>: "wrappedType" ~>
      "tname" <~ Core.wrappedTypeTypeName (var "wrappedType") $
      Sets.insert (var "tname") (var "names")]) $
  foldOverType @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty @@ var "typ0"

unshadowVariables :: TBinding (Term -> Term)
unshadowVariables = define "unshadowVariables" $
  doc "Unshadow lambda-bound variables in a term" $
  "term0" ~>
  "rewrite" <~ ("recurse" ~> "m" ~> "term"~>
    "dflt" <~ var "recurse" @@ var "m" @@ var "term" $
    cases _Term (var "term")
      (Just $ var "dflt") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "dflt") [
        _Function_lambda>>: "l" ~>
          "v" <~ Core.lambdaParameter (var "l") $
          "domain" <~ Core.lambdaDomain (var "l") $
          "body" <~ Core.lambdaBody (var "l") $
          pair (var "m") $ optCases (Maps.lookup (var "v") (var "m"))
            (Core.termFunction $ Core.functionLambda $ Core.lambda (var "v") (var "domain")
              (Pairs.second $ var "rewrite"
                @@ var "recurse"
                @@ (Maps.insert (var "v") (int32 1) (var "m"))
                @@ (var "body")))
            ("i" ~>
              "i2" <~ Math.add (var "i") (int32 1) $
              "v2" <~ Core.name (Strings.cat2 (Core.unName $ var "v") (Literals.showInt32 $ var "i2")) $
              "m2" <~ Maps.insert (var "v") (var "i2") (var "m") $
              Core.termFunction $ Core.functionLambda $ Core.lambda (var "v2") (var "domain")
                (Pairs.second $ var "rewrite" @@ var "recurse" @@ var "m2" @@ var "body"))],
      _Term_variable>>: "v" ~> pair (var "m") $ Core.termVariable $ optCases (Maps.lookup (var "v") (var "m"))
        (var "v")
        ("i" ~> Logic.ifElse (Equality.equal (var "i") (int32 1))
          (var "v")
          (Core.name $ Strings.cat2 (Core.unName $ var "v") (Literals.showInt32 $ var "i")))]) $
  Pairs.second (rewriteAndFoldTerm @@ var "rewrite" @@ Maps.empty @@ var "term0")
