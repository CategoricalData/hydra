{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Rewriting where

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

import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Sorting as Sorting


module_ :: Module
module_ = Module (Namespace "hydra.rewriting") elements
    [Names.module_, Sorting.module_]
    kernelTypesModules $
    Just ("Utilities for type and term rewriting and analysis.")
  where
   elements = [
     el deannotateAndDetypeTermDef,
     el deannotateTermDef,
     el deannotateTypeDef,
     el deannotateTypeParametersDef,
     el deannotateTypeRecursiveDef,
     el deannotateTypeSchemeRecursiveDef,
     el detypeTermDef,
     el flattenLetTermsDef,
     el foldOverTermDef,
     el foldOverTypeDef,
     el freeTypeVariablesInTermDef,
     el freeVariablesInTermDef,
     el freeVariablesInTypeDef,
     el freeVariablesInTypeOrderedDef,
     el freeVariablesInTypeSchemeSimpleDef,
     el freeVariablesInTypeSchemeDef,
     el freeVariablesInTypeSimpleDef,
     el inlineTypeDef,
     el isFreeVariableInTermDef,
     el isLambdaDef,
     el mapBeneathTypeAnnotationsDef,
     el normalizeTypeVariablesInTermDef,
     el removeTermAnnotationsDef,
     el removeTypeAnnotationsDef,
     el removeTypesFromTermDef,
     el replaceFreeTermVariableDef,
     el replaceFreeTypeVariableDef,
     el rewriteDef,
     el rewriteAndFoldTermDef,
     el rewriteAndFoldTermMDef,
     el rewriteTermDef,
     el rewriteTermMDef,
     el rewriteTypeDef,
     el rewriteTypeMDef,
     el simplifyTermDef,
     el substituteTypeVariablesDef,
     el substituteVariableDef,
     el substituteVariablesDef,
     el subtermsDef,
     el subtermsWithAccessorsDef,
     el subtypesDef,
     el termDependencyNamesDef,
     el toShortNamesDef,
     el topologicalSortBindingMapDef,
     el topologicalSortBindingsDef,
     el typeDependencyNamesDef,
     el typeNamesInTypeDef,
     el unshadowVariablesDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

deannotateAndDetypeTermDef :: TBinding (Term -> Term)
deannotateAndDetypeTermDef = define "deannotateAndDetypeTerm" $
  doc "Strip type annotations from the top levels of a term" $
  "t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~> ref deannotateAndDetypeTermDef @@ (Core.annotatedTermSubject $ var "at"),
    _Term_typeApplication>>: "tt" ~> ref deannotateAndDetypeTermDef @@ (Core.typedTermTerm $ var "tt"),
    _Term_typeLambda>>: "ta" ~> ref deannotateAndDetypeTermDef @@ (Core.typeLambdaBody $ var "ta")]

deannotateTermDef :: TBinding (Term -> Term)
deannotateTermDef = define "deannotateTerm" $
  doc "Strip all annotations (including System F type annotations) from the top levels of a term" $
  "t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~> ref deannotateTermDef @@ (Core.annotatedTermSubject $ var "at")]

deannotateTypeDef :: TBinding (Type -> Type)
deannotateTypeDef = define "deannotateType" $
  doc "Strip all annotations from a term" $
  "t" ~> cases _Type (var "t")
    (Just $ var "t") [
    _Type_annotated>>: ref deannotateTypeDef <.> (project _AnnotatedType _AnnotatedType_subject)]

deannotateTypeParametersDef :: TBinding (Type -> Type)
deannotateTypeParametersDef = define "deannotateTypeParameters" $
  doc "Strip any top-level type lambdas from a type, extracting the (possibly nested) type body" $
  "t" ~> cases _Type (ref deannotateTypeDef @@ var "t")
    (Just $ var "t") [
    _Type_forall>>: "lt" ~> ref deannotateTypeParametersDef @@ (project _ForallType _ForallType_body @@ var "lt")]

deannotateTypeRecursiveDef :: TBinding (Type -> Type)
deannotateTypeRecursiveDef = define "deannotateTypeRecursive" $
  doc "Recursively strip all annotations from a type" $
  "typ" ~>
  "strip" <~ ("recurse" ~> "typ" ~>
    "rewritten" <~ var "recurse" @@ var "typ" $
    cases _Type (var "rewritten")
      (Just $ var "rewritten") [
      _Type_annotated>>: "at" ~> Core.annotatedTypeSubject $ var "at"]) $
  ref rewriteTypeDef @@ var "strip" @@ var "typ"

deannotateTypeSchemeRecursiveDef :: TBinding (TypeScheme -> TypeScheme)
deannotateTypeSchemeRecursiveDef = define "deannotateTypeSchemeRecursive" $
  doc "Recursively strip all annotations from a type scheme" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "typ" <~ Core.typeSchemeType (var "ts") $
  Core.typeScheme (var "vars") (ref deannotateTypeRecursiveDef @@ var "typ")

detypeTermDef :: TBinding (Term -> Term)
detypeTermDef = define "detypeTerm" $
  doc "Strip System F type annotations from the top levels of a term, but leave application-specific annotations intact" $
  "t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~>
       "subj" <~ Core.annotatedTermSubject (var "at") $
       "ann" <~ Core.annotatedTermAnnotation (var "at") $
       Core.termAnnotated $ Core.annotatedTerm (ref detypeTermDef @@ var "subj") (var "ann"),
    _Term_typeApplication>>: "tt" ~> ref deannotateAndDetypeTermDef @@ (Core.typedTermTerm $ var "tt"),
    _Term_typeLambda>>: "ta" ~> ref deannotateAndDetypeTermDef @@ (Core.typeLambdaBody $ var "ta")]

flattenLetTermsDef :: TBinding (Term -> Term)
flattenLetTermsDef = define "flattenLetTerms" $
  doc "Flatten nested let expressions" $
  "term" ~>
  "rewriteBinding" <~ ("binding" ~>
    "key0" <~ Core.bindingName (var "binding") $
    "val0" <~ Core.bindingTerm (var "binding") $
    "t" <~ Core.bindingType (var "binding") $
    cases _Term (var "val0")
      (Just $ pair (Core.binding (var "key0") (var "val0") (var "t")) (list [])) [
      _Term_annotated>>: "at" ~>
        "val1" <~ Core.annotatedTermSubject (var "at") $
        "ann" <~ Core.annotatedTermAnnotation (var "at") $
        "recursive" <~ var "rewriteBinding" @@ (Core.binding (var "key0") (var "val1") (var "t")) $
        "innerBinding" <~ first (var "recursive") $
        "deps" <~ second (var "recursive") $
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
        "replaceVars" <~ ref substituteVariablesDef @@ var "subst" $
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
        "forResult" <~ ("hr" ~> Lists.cons (first $ var "hr") (second $ var "hr")) $
        "newBindings" <~ Lists.concat (Lists.map (var "forResult" <.> var "rewriteBinding") (var "bindings")) $
        Core.termLet $ Core.let_ (var "newBindings") (var "body")]) $
  ref rewriteTermDef @@ var "flatten" @@ var "term"

foldOverTermDef :: TBinding (TraversalOrder -> (x -> Term -> x) -> x -> Term -> x)
foldOverTermDef = define "foldOverTerm" $
  doc "Fold over a term, traversing its subterms in the specified order" $
  "order" ~> "fld" ~> "b0" ~> "term" ~> cases _TraversalOrder (var "order") Nothing [
    _TraversalOrder_pre>>: constant (Phantoms.fold (ref foldOverTermDef @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "term")
      @@ (ref subtermsDef @@ var "term")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Phantoms.fold (ref foldOverTermDef @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (ref subtermsDef @@ var "term"))
      @@ var "term")]

foldOverTypeDef :: TBinding (TraversalOrder -> (x -> Type -> x) -> x -> Type -> x)
foldOverTypeDef = define "foldOverType" $
  doc "Fold over a type, traversing its subtypes in the specified order" $
  "order" ~> "fld" ~> "b0" ~> "typ" ~> cases _TraversalOrder (var "order") Nothing [
    _TraversalOrder_pre>>: constant (Phantoms.fold (ref foldOverTypeDef @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "typ")
      @@ (ref subtypesDef @@ var "typ")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Phantoms.fold (ref foldOverTypeDef @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (ref subtypesDef @@ var "typ"))
      @@ var "typ")]

--freeTypeVariablesInTermDef :: TBinding (Term -> S.Set Name)
--freeTypeVariablesInTermDef = define "freeTypeVariablesInTerm" $
--  doc "Find free type variables introduced by type applications within a term." $
--  "term" ~> cases _Term (var "term")
--    (Just $ Lists.foldl (binaryFunction Sets.union) Sets.empty $
--      Lists.map (ref freeTypeVariablesInTermDef) (ref subtermsDef @@ var "term")) [
--    _Term_typeApplication>>: "tt" ~> Sets.union
--      (ref freeVariablesInTypeDef @@ (Core.typedTermType $ var "tt"))
--      (ref freeTypeVariablesInTermDef @@ (Core.typedTermTerm $ var "tt")),
--    _Term_typeLambda>>: "tl" ~>
--      "tmp" <~ ref freeTypeVariablesInTermDef @@ (Core.typeLambdaBody $ var "tl") $
--      Sets.delete (Core.typeLambdaParameter $ var "tl") (var "tmp")]

freeTypeVariablesInTermDef :: TBinding (Term -> S.Set Name)
freeTypeVariablesInTermDef = define "freeTypeVariablesInTerm" $
  doc ("Get the set of free type variables in a term (including schema names, where they appear in type annotations)."
    <> " In this context, only the type schemes of let bindings can bind type variables; type lambdas do not.") $
  "term0" ~>
  "allOf" <~ ("sets" ~> Lists.foldl (binaryFunction Sets.union) Sets.empty $ var "sets") $
  "tryType" <~ ("tvars" ~> "typ" ~> Sets.difference (ref freeVariablesInTypeDef @@ var "typ") (var "tvars")) $
  "getAll" <~ ("vars" ~> "term" ~>
    "recurse" <~ var "getAll" @@ var "vars" $
    "dflt" <~ (var "allOf" @@ Lists.map (var "recurse") (ref subtermsDef @@ var "term")) $
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
          (var "recurse" @@ (Core.letBody $ var "l")),
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
  var "getAll" @@ Sets.empty @@ var "term0"

freeVariablesInTermDef :: TBinding (Term -> S.Set Name)
freeVariablesInTermDef = define "freeVariablesInTerm" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a term" $
  "term" ~>
  "dfltVars" <~ Phantoms.fold ("s" ~> "t" ~> Sets.union (var "s") (ref freeVariablesInTermDef @@ var "t"))
    @@ Sets.empty
    @@ (ref subtermsDef @@ var "term") $
  cases _Term (var "term")
    (Just $ var "dfltVars") [
    _Term_function>>: match _Function (Just $ var "dfltVars") [
      _Function_lambda>>: "l" ~> Sets.delete
        (Core.lambdaParameter $ var "l")
        (ref freeVariablesInTermDef @@ (Core.lambdaBody $ var "l"))],
    _Term_let>>: "l" ~> (Sets.difference
      (var "dfltVars")
      (Sets.fromList (Lists.map (unaryFunction Core.bindingName) (Core.letBindings $ var "l")))),
    _Term_variable>>: "v" ~> Sets.singleton $ var "v"]

freeVariablesInTypeDef :: TBinding (Type -> S.Set Name)
freeVariablesInTypeDef = define "freeVariablesInType" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a type" $
  "typ" ~>
  "dfltVars" <~ Phantoms.fold ("s" ~> "t" ~> Sets.union (var "s") (recurse @@ var "t"))
    @@ Sets.empty
    @@ (ref subtypesDef @@ var "typ") $
  cases _Type (var "typ")
    (Just $ var "dfltVars") [
    _Type_forall>>: "lt" ~> Sets.delete
        (Core.forallTypeParameter $ var "lt")
        (recurse @@ (Core.forallTypeBody $ var "lt")),
    -- TODO: let-types
    _Type_variable>>: "v" ~> Sets.singleton $ var "v"]
  where
    recurse = ref freeVariablesInTypeDef

freeVariablesInTypeOrderedDef :: TBinding (Type -> [Name])
freeVariablesInTypeOrderedDef = define "freeVariablesInTypeOrdered" $
  doc "Find the free variables in a type in deterministic left-to-right order" $
  "typ" ~>
  "collectVars" <~ ("boundVars" ~> "t" ~>
    cases _Type (var "t")
      (Just $ Lists.concat $ Lists.map (var "collectVars" @@ var "boundVars") $
              ref subtypesDef @@ var "t") [
      _Type_variable>>: "v" ~>
        Logic.ifElse (Sets.member (var "v") (var "boundVars"))
          (list [])
          (list [var "v"]),
      _Type_forall>>: "ft" ~>
        var "collectVars" @@
          (Sets.insert (Core.forallTypeParameter $ var "ft") (var "boundVars")) @@
          (Core.forallTypeBody $ var "ft")]) $
  (Lists.nub :: TTerm [Name] -> TTerm [Name]) $ var "collectVars" @@ Sets.empty @@ var "typ"

freeVariablesInTypeSimpleDef :: TBinding (Type -> S.Set Name)
freeVariablesInTypeSimpleDef = define "freeVariablesInTypeSimple" $
  doc "Same as freeVariablesInType, but ignores the binding action of lambda types" $
  "typ" ~>
  "helper" <~ ("types" ~> "typ" ~> cases _Type (var "typ")
    (Just $ var "types") [
    _Type_variable>>: "v" ~> Sets.insert (var "v") (var "types")]) $
  ref foldOverTypeDef @@ Coders.traversalOrderPre @@ var "helper" @@ Sets.empty @@ var "typ"

freeVariablesInTypeSchemeDef :: TBinding (TypeScheme -> S.Set Name)
freeVariablesInTypeSchemeDef = define "freeVariablesInTypeScheme" $
  doc "Find free variables in a type scheme" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "t" <~ Core.typeSchemeType (var "ts") $
  Sets.difference (ref freeVariablesInTypeDef @@ var "t") (Sets.fromList $ var "vars")

freeVariablesInTypeSchemeSimpleDef :: TBinding (TypeScheme -> S.Set Name)
freeVariablesInTypeSchemeSimpleDef = define "freeVariablesInTypeSchemeSimple" $
  doc "Find free variables in a type scheme (simple version)" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "t" <~ Core.typeSchemeType (var "ts") $
  Sets.difference (ref freeVariablesInTypeSimpleDef @@ var "t") (Sets.fromList $ var "vars")

inlineTypeDef :: TBinding (M.Map Name Type -> Type -> Flow s Type)
inlineTypeDef = define "inlineType" $
  doc "Inline all type variables in a type using the provided schema. Note: this function is only appropriate for nonrecursive type definitions" $
  "schema" ~> "typ" ~>
  "f" <~ ("recurse" ~> "typ" ~>
    "tr" <<~ var "recurse" @@ var "typ" $
    cases _Type (var "tr")
      (Just $ produce $ var "tr") [
      _Type_variable>>: "v" ~>
        Optionals.maybe
          (Flows.fail $ Strings.cat2 (string "No such type in schema: ") (unwrap _Name @@ var "v"))
          (ref inlineTypeDef @@ var "schema")
          (Maps.lookup (var "v") (var "schema"))]) $
  ref rewriteTypeMDef @@ var "f" @@ var "typ"

isFreeVariableInTermDef :: TBinding (Name -> Term -> Bool)
isFreeVariableInTermDef = define "isFreeVariableInTerm" $
 doc "Check whether a variable is free (not bound) in a term" $
 "v" ~> "term" ~>
   Logic.not $ Sets.member (var "v") (ref freeVariablesInTermDef @@ var "term")

isLambdaDef :: TBinding (Term -> Bool)
isLambdaDef = define "isLambda" $
  doc "Check whether a term is a lambda, possibly nested within let and/or annotation terms" $
  "term" ~> cases _Term (ref deannotateTermDef @@ var "term")
    (Just false) [
    _Term_function>>: match _Function
      (Just false) [
      _Function_lambda>>: constant true],
    _Term_let>>: "lt" ~> ref isLambdaDef @@ (project _Let _Let_body @@ var "lt")]

mapBeneathTypeAnnotationsDef :: TBinding ((Type -> Type) -> Type -> Type)
mapBeneathTypeAnnotationsDef = define "mapBeneathTypeAnnotations" $
  doc "Apply a transformation to the first type beneath a chain of annotations" $
  "f" ~> "t" ~> cases _Type (var "t")
    (Just $ var "f" @@ var "t") [
    _Type_annotated>>: "at" ~> Core.typeAnnotated $ Core.annotatedType
      (ref mapBeneathTypeAnnotationsDef @@ var "f" @@ (Core.annotatedTypeSubject $ var "at"))
      (Core.annotatedTypeAnnotation $ var "at")]

normalizeTypeVariablesInTermDef :: TBinding (Term -> Term)
normalizeTypeVariablesInTermDef = define "normalizeTypeVariablesInTerm" $
  doc "Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ..." $
  "term" ~>
  "replaceName" <~ ("subst" ~> "v" ~> Optionals.fromMaybe (var "v") $ Maps.lookup (var "v") (var "subst")) $
  "substType" <~ ("subst" ~> "typ" ~>
    "rewrite" <~ ("recurse" ~> "typ" ~> cases _Type (var "typ")
      (Just $ var "recurse" @@ var "typ") [
      _Type_variable>>: "v" ~> Core.typeVariable $ var "replaceName" @@ var "subst" @@ var "v"]) $
    ref rewriteTypeDef @@ var "rewrite" @@ var "typ") $
  -- Thread a triple: ((subst, boundVars), next)
  "rewriteWithSubst" <~ ("state" ~> "term0" ~>
    "sb"   <~ first  (var "state") $
    "next" <~ second (var "state") $
    "subst"     <~ first  (var "sb") $
    "boundVars" <~ second (var "sb") $
    "rewrite" <~ ("recurse" ~> "term" ~> cases _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      -- Lambdas and tuple projections have a "domain" type which needs to be rewritten
      _Term_function>>: match _Function
        (Just $ var "recurse" @@ var "term") [
        _Function_elimination>>: match _Elimination
          (Just $ var "recurse" @@ var "term") [
          _Elimination_product>>: "tproj" ~>
            "domain" <~ Core.tupleProjectionDomain (var "tproj") $
            Core.termFunction $ Core.functionElimination $ Core.eliminationProduct $ Core.tupleProjection
              (Core.tupleProjectionArity $ var "tproj")
              (Core.tupleProjectionIndex $ var "tproj")
              (Optionals.map
                ("types" ~> Lists.map (var "substType" @@ var "subst") (var "types"))
                (var "domain"))],
        _Function_lambda>>: "l" ~>
          "domain" <~ Core.lambdaDomain (var "l") $
          Core.termFunction $ Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Optionals.map (var "substType" @@ var "subst") (var "domain"))
            (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.lambdaBody $ var "l"))],
      -- Let bindings each have a type which needs to be rewritten
      _Term_let>>: "lt" ~>
        "bindings0" <~ Core.letBindings (var "lt") $
        "body0"     <~ Core.letBody (var "lt") $
        -- Sequentially rewrite bindings without advancing 'next' across siblings
        "step" <~ ("acc" ~> "bs" ~>
          Logic.ifElse (Lists.null (var "bs"))
            (Lists.reverse (var "acc"))
            ("b"  <~ Lists.head (var "bs") $
             "tl" <~ Lists.tail (var "bs") $
             Optionals.maybe
               -- Untyped binding: rewrite its term with current state; 'next' unchanged for siblings
               ("newVal" <~ var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.bindingTerm $ var "b") $
                "b1"     <~ Core.binding (Core.bindingName $ var "b") (var "newVal") nothing $
                var "step" @@ (Lists.cons (var "b1") (var "acc")) @@ var "tl")
               -- Typed binding: allocate |vars| fresh t{next+i}; bump 'next' only for the binding's TERM
               ("ts" ~>
                 "vars" <~ Core.typeSchemeVariables (var "ts") $
                 "typ"  <~ Core.typeSchemeType (var "ts") $
                 "k"    <~ Lists.length (var "vars") $
                 -- Build exactly k fresh names t{next}, t{next+1}, ...
                 "gen"  <~ ("i" ~> "rem" ~> "acc2" ~>
                   Logic.ifElse (Equality.equal (var "rem") (int32 0))
                     (Lists.reverse (var "acc2"))
                     ("ti" <~ Core.name (Strings.cat2 (string "t") (Literals.showInt32 (Math.add (var "next") (var "i")))) $
                      var "gen" @@ (Math.add (var "i") (int32 1)) @@ (Math.sub (var "rem") (int32 1)) @@ (Lists.cons (var "ti") (var "acc2")))) $
                 "newVars"  <~ var "gen" @@ (int32 0) @@ (var "k") @@ (list []) $
                 "newSubst" <~ Maps.union (Maps.fromList $ Lists.zip (var "vars") (var "newVars")) (var "subst") $
                 "newBound" <~ Sets.union (var "boundVars") (Sets.fromList (var "newVars")) $
                 "newVal"   <~ var "rewriteWithSubst" @@ (pair (pair (var "newSubst") (var "newBound")) (Math.add (var "next") (var "k"))) @@ (Core.bindingTerm $ var "b") $
                 "b1"       <~ Core.binding
                   (Core.bindingName $ var "b")
                   (var "newVal")
                   (just $ Core.typeScheme (var "newVars") (var "substType" @@ var "newSubst" @@ var "typ")) $
                 -- Note: do not advance 'next' for the next sibling; keep current 'next'
                 var "step" @@ (Lists.cons (var "b1") (var "acc")) @@ var "tl")
               (Core.bindingType $ var "b"))) $
        "bindings1" <~ var "step" @@ (list []) @@ (var "bindings0") $
        Core.termLet $ Core.let_
          (var "bindings1")
          -- Body sees the original 'next' (binding lambdas don't bind in the body)
          (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ var "body0"),
      -- Type application terms have a type which needs to be rewritten, and we also recurse into the body term.
      _Term_typeApplication>>: "tt" ~> Core.termTypeApplication $ Core.typedTerm
        (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.typedTermTerm $ var "tt"))
        (var "substType" @@ var "subst" @@ (Core.typedTermType $ var "tt")),
      -- Type lambdas introduce a type variable which needs to be replaced, and we also recurse into the body term.
      -- Note: in Hydra currently, type lambdas are exclusively created during type inference in combination with
      -- polymorphic let bindings, so the type variable should already be present in the substitution.
      -- If "free-standing" type lambdas are ever supported in the future, we will have to create a fresh type variable here.
      _Term_typeLambda>>: "ta" ~> Core.termTypeLambda $ Core.typeLambda
        (var "replaceName" @@ var "subst" @@ (Core.typeLambdaParameter $ var "ta"))
        (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.typeLambdaBody $ var "ta"))]) $
    ref rewriteTermDef @@ var "rewrite" @@ var "term0") $
  -- initial state: ((emptySubst, emptyBound), next=0)
  var "rewriteWithSubst" @@ (pair (pair Maps.empty Sets.empty) (int32 0)) @@ var "term"

removeTermAnnotationsDef :: TBinding (Term -> Term)
removeTermAnnotationsDef = define "removeTermAnnotations" $
  doc "Recursively remove term annotations, including within subterms" $
  "term" ~>
  "remove" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    cases _Term (var "term")
      (Just $ var "rewritten") [
      _Term_annotated>>: "at" ~> Core.annotatedTermSubject $ var "at"]) $
  ref rewriteTermDef @@ var "remove" @@ var "term"

removeTypeAnnotationsDef :: TBinding (Type -> Type)
removeTypeAnnotationsDef = define "removeTypeAnnotations" $
  doc "Recursively remove type annotations, including within subtypes" $
  "typ" ~>
  "remove" <~ ("recurse" ~> "typ" ~>
    "rewritten" <~ var "recurse" @@ var "typ" $
    cases _Type (var "rewritten")
      (Just $ var "rewritten") [
      _Type_annotated>>: "at" ~> Core.annotatedTypeSubject $ var "at"]) $
  ref rewriteTypeDef @@ var "remove" @@ var "typ"

removeTypesFromTermDef :: TBinding (Term -> Term)
removeTypesFromTermDef = define "removeTypesFromTerm" $
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
        _Function_elimination>>: "e" ~> cases _Elimination (var "e")
          (Just $ Core.termFunction $ Core.functionElimination $ var "e") [
          _Elimination_product>>: "tp" ~> Core.termFunction $ Core.functionElimination $ Core.eliminationProduct $
            Core.tupleProjection
              (Core.tupleProjectionIndex $ var "tp")
              (Core.tupleProjectionArity $ var "tp")
              nothing],
        _Function_lambda>>: "l" ~> Core.termFunction $ Core.functionLambda $ Core.lambda
          (Core.lambdaParameter $ var "l")
          nothing
          (Core.lambdaBody $ var "l")],
      _Term_let>>: "lt" ~> Core.termLet $ Core.let_
        (Lists.map (var "stripBinding") (Core.letBindings $ var "lt"))
        (Core.letBody $ var "lt"),
      _Term_typeApplication>>: "tt" ~> Core.typedTermTerm $ var "tt",
      _Term_typeLambda>>: "ta" ~> Core.typeLambdaBody $ var "ta"]) $
  ref rewriteTermDef @@ var "strip" @@ var "term"

replaceFreeTermVariableDef :: TBinding (Name -> Term -> Term -> Term)
replaceFreeTermVariableDef = define "replaceFreeTermVariable" $
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
  ref rewriteTermDef @@ var "rewrite" @@ var "term"

replaceFreeTypeVariableDef :: TBinding (Name -> Type -> Type -> Type)
replaceFreeTypeVariableDef = define "replaceFreeTypeVariable" $
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
  ref rewriteTypeDef @@ var "mapExpr" @@ var "typ"

-- TODO: this is a fixpoint combinator, but its type is sometimes incorrectly inferred based on how it is used.
--       For now, we generally define local "rewrite"/"recurse" helper functions rather than using this global one.
rewriteDef :: TBinding (((x -> y) -> x -> y) -> ((x -> y) -> x -> y) -> x -> y)
rewriteDef = define "rewrite" $ "fsub" ~> "f" ~>
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse"

rewriteAndFoldTermDef :: TBinding (((a -> Term -> (a, Term)) -> a -> Term -> (a, Term)) -> a -> Term -> (a, Term))
rewriteAndFoldTermDef = define "rewriteAndFoldTerm" $
  doc "Rewrite a term, and at the same time, fold a function over it, accumulating a value" $
  "f" ~>
  "fsub" <~ ("recurse" ~> "val0" ~> "term0" ~>
    "forSingle" <~ ("rec" ~> "cons" ~> "val" ~> "term" ~>
      "r" <~ var "rec" @@ var "val" @@ var "term" $
      pair (first $ var "r") (var "cons" @@ (second $ var "r"))) $
    "forMany" <~ ("rec" ~> "cons" ~> "val" ~> "els" ~>
      "rr" <~ Lists.foldl
        ("r" ~> "el" ~>
          "r2" <~ var "rec" @@ (first $ var "r") @@ var "el" $
          pair (first $ var "r2") (Lists.cons (second $ var "r2") (second $ var "r")))
        (pair (var "val") (list []))
        (var "els") $
      pair (first $ var "rr") (var "cons" @@ (Lists.reverse $ second $ var "rr"))) $
    "forField" <~ ("val" ~> "field" ~>
      "r" <~ var "recurse" @@ var "val" @@ Core.fieldTerm (var "field") $
      pair (first $ var "r") (Core.field (Core.fieldName $ var "field") (second $ var "r"))) $
    "forFields" <~ var "forMany" @@ var "forField" @@ ("x" ~> var "x") $
    "forPair" <~ ("val" ~> "kv" ~>
      "rk" <~ var "recurse" @@ var "val" @@ (first $ var "kv") $
      "rv" <~ var "recurse" @@ (first $ var "rk") @@ (second $ var "kv") $
      pair
        (first $ var "rv")
        (pair (second $ var "rk") (second $ var "rv"))) $
    "forBinding" <~ ("val" ~> "binding" ~>
      "r" <~ var "recurse" @@ var "val" @@ Core.bindingTerm (var "binding") $
      pair
        (first $ var "r")
        (Core.binding
          (Core.bindingName $ var "binding")
          (second $ var "r")
          (Core.bindingType $ var "binding"))) $
    "forElimination" <~ ("val" ~> "elm" ~>
      "r" <~ cases _Elimination (var "elm")
        (Just $ pair (var "val") (var "elm")) [
        _Elimination_union>>: "cs" ~>
          "rmd" <~ Optionals.map (var "recurse" @@ var "val") (Core.caseStatementDefault $ var "cs") $
          "val1" <~ optCases (var "rmd")
            (var "val")
            ("r" ~> first $ var "r") $
          "rcases" <~ var "forFields" @@ var "val1" @@ (Core.caseStatementCases $ var "cs") $
          pair
            (first $ var "rcases")
            (Core.eliminationUnion $ Core.caseStatement
              (Core.caseStatementTypeName $ var "cs")
              (Optionals.map (unaryFunction second) (var "rmd"))
              (second $ var "rcases"))] $
      pair (first $ var "r") (second $ var "r")) $
    "forFunction" <~ ("val" ~> "fun" ~> cases _Function (var "fun")
      (Just $ pair (var "val") (var "fun")) [
      _Function_elimination>>: "elm" ~>
         "r" <~ var "forElimination" @@ var "val" @@ var "elm" $
         pair (first $ var "r") (Core.functionElimination (second $ var "r")),
      _Function_lambda>>: "l" ~>
        "r" <~ var "recurse" @@ var "val" @@ (Core.lambdaBody $ var "l") $
        pair
          (first $ var "r")
          (Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Core.lambdaDomain $ var "l")
            (second $ var "r"))]) $
    "dflt" <~ pair (var "val0") (var "term0") $
    cases _Term (var "term0")
      (Just $ var "dflt") [
      _Term_annotated>>: "at" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termAnnotated $ Core.annotatedTerm (var "t") (Core.annotatedTermAnnotation $ var "at"))
        @@ var "val0"
        @@ (Core.annotatedTermSubject $ var "at"),
      _Term_application>>: "a" ~>
        "rlhs" <~ var "recurse" @@ var "val0" @@ (Core.applicationFunction $ var "a") $
        "rrhs" <~ var "recurse" @@ (first $ var "rlhs") @@ (Core.applicationArgument $ var "a") $
        pair
          (first $ var "rrhs")
          (Core.termApplication $ Core.application
            (second $ var "rlhs")
            (second $ var "rrhs")),
      _Term_function>>: "f" ~> var "forSingle"
        @@ var "forFunction"
        @@ ("f" ~> Core.termFunction $ var "f")
        @@ var "val0"
        @@ var "f",
      _Term_let>>: "l" ~>
        "renv" <~ var "recurse" @@ var "val0" @@ (Core.letBody $ var "l") $
        var "forMany" @@ var "forBinding"
          @@ ("bins" ~> Core.termLet $ Core.let_ (var "bins") (second $ var "renv"))
          @@ first (var "renv") @@ (Core.letBindings $ var "l"),
      _Term_list>>: "els" ~> var "forMany" @@ var "recurse" @@ (unaryFunction Core.termList) @@ var "val0" @@ var "els",
      _Term_map>>: "m" ~> var "forMany" @@ var "forPair"
        @@ ("pairs" ~> Core.termMap $ Maps.fromList $ var "pairs") @@ var "val0" @@ Maps.toList (var "m"),
      _Term_optional>>: "mt" ~> optCases (var "mt")
        (var "dflt")
        ("t" ~> var "forSingle"
          @@ var "recurse"
          @@ ("t1" ~> Core.termOptional $ just $ var "t1")
          @@ var "val0"
          @@ var "t"),
      _Term_product>>: "terms" ~> var "forMany" @@ var "recurse"
        @@ (unaryFunction Core.termProduct) @@ var "val0" @@ var "terms",
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
      _Term_sum>>: "s" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termSum $ Core.sum (Core.sumIndex $ var "s") (Core.sumSize $ var "s") (var "t"))
        @@ var "val0"
        @@ Core.sumTerm (var "s"),
      _Term_typeApplication>>: "ta" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termTypeApplication $ Core.typedTerm (var "t") (Core.typedTermType $ var "ta"))
        @@ var "val0"
        @@ (Core.typedTermTerm $ var "ta"),
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
        @@ (Core.wrappedTermObject $ var "wt")]) $
--  ref rewriteDef @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse"

rewriteAndFoldTermMDef :: TBinding (((a -> Term -> Flow s (a, Term)) -> a -> Term -> Flow s (a, Term)) -> a -> Term -> Flow s (a, Term))
rewriteAndFoldTermMDef = define "rewriteAndFoldTermM" $
  doc "Monadic version: rewrite a term and fold a function over it, accumulating a value" $
  "f" ~>
  "fsub" <~ ("recurse" ~> "val0" ~> "term0" ~>
    "forSingle" <~ ("rec" ~> "cons" ~> "val" ~> "term" ~>
      "r" <<~ var "rec" @@ var "val" @@ var "term" $
      produce $ pair (first $ var "r") (var "cons" @@ (second $ var "r"))) $
    "forMany" <~ ("rec" ~> "cons" ~> "val" ~> "els" ~>
      "rr" <<~ Flows.foldl
        ("r" ~> "el" ~>
          "r2" <<~ var "rec" @@ (first $ var "r") @@ var "el" $
          produce $ pair (first $ var "r2") (Lists.cons (second $ var "r2") (second $ var "r")))
        (pair (var "val") (list []))
        (var "els") $
      produce $ pair (first $ var "rr") (var "cons" @@ (Lists.reverse $ second $ var "rr"))) $
    "forField" <~ ("val" ~> "field" ~>
      "r" <<~ var "recurse" @@ var "val" @@ Core.fieldTerm (var "field") $
      produce $ pair (first $ var "r") (Core.field (Core.fieldName $ var "field") (second $ var "r"))) $
    "forFields" <~ var "forMany" @@ var "forField" @@ ("x" ~> var "x") $
    "forPair" <~ ("val" ~> "kv" ~>
      "rk" <<~ var "recurse" @@ var "val" @@ (first $ var "kv") $
      "rv" <<~ var "recurse" @@ (first $ var "rk") @@ (second $ var "kv") $
      produce $ pair
        (first $ var "rv")
        (pair (second $ var "rk") (second $ var "rv"))) $
    "forBinding" <~ ("val" ~> "binding" ~>
      "r" <<~ var "recurse" @@ var "val" @@ Core.bindingTerm (var "binding") $
      produce $ pair
        (first $ var "r")
        (Core.binding
          (Core.bindingName $ var "binding")
          (second $ var "r")
          (Core.bindingType $ var "binding"))) $
    "forElimination" <~ ("val" ~> "elm" ~>
      "r" <<~ cases _Elimination (var "elm")
        (Just $ produce $ pair (var "val") (var "elm")) [
        _Elimination_union>>: "cs" ~>
          "rmd" <<~ Optionals.maybe (produce nothing)
            ("def" ~> Flows.map (unaryFunction just) (var "recurse" @@ var "val" @@ var "def"))
            (Core.caseStatementDefault $ var "cs") $
          "val1" <~ Optionals.maybe (var "val") (unaryFunction first) (var "rmd") $
          "rcases" <<~ var "forFields" @@ var "val1" @@ (Core.caseStatementCases $ var "cs") $
          produce $ pair
            (first $ var "rcases")
            (Core.eliminationUnion $ Core.caseStatement
              (Core.caseStatementTypeName $ var "cs")
              (Optionals.map (unaryFunction second) (var "rmd"))
              (second $ var "rcases"))] $
      produce $ pair (first $ var "r") (second $ var "r")) $
    "forFunction" <~ ("val" ~> "fun" ~> cases _Function (var "fun")
      (Just $ produce $ pair (var "val") (var "fun")) [
      _Function_elimination>>: "elm" ~>
         "r" <<~ var "forElimination" @@ var "val" @@ var "elm" $
         produce $ pair (first $ var "r") (Core.functionElimination (second $ var "r")),
      _Function_lambda>>: "l" ~>
        "r" <<~ var "recurse" @@ var "val" @@ (Core.lambdaBody $ var "l") $
        produce $ pair
          (first $ var "r")
          (Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Core.lambdaDomain $ var "l")
            (second $ var "r"))]) $
    "dflt" <~ produce (pair (var "val0") (var "term0")) $
    cases _Term (var "term0")
      (Just $ var "dflt") [
      _Term_annotated>>: "at" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termAnnotated $ Core.annotatedTerm (var "t") (Core.annotatedTermAnnotation $ var "at"))
        @@ var "val0"
        @@ (Core.annotatedTermSubject $ var "at"),
      _Term_application>>: "a" ~>
        "rlhs" <<~ var "recurse" @@ var "val0" @@ (Core.applicationFunction $ var "a") $
        "rrhs" <<~ var "recurse" @@ (first $ var "rlhs") @@ (Core.applicationArgument $ var "a") $
        produce $ pair
          (first $ var "rrhs")
          (Core.termApplication $ Core.application
            (second $ var "rlhs")
            (second $ var "rrhs")),
      _Term_function>>: "f" ~> var "forSingle"
        @@ var "forFunction"
        @@ ("f" ~> Core.termFunction $ var "f")
        @@ var "val0"
        @@ var "f",
      _Term_let>>: "l" ~>
        "renv" <<~ var "recurse" @@ var "val0" @@ (Core.letBody $ var "l") $
        var "forMany" @@ var "forBinding"
          @@ ("bins" ~> Core.termLet $ Core.let_ (var "bins") (second $ var "renv"))
          @@ first (var "renv") @@ (Core.letBindings $ var "l"),
      _Term_list>>: "els" ~> var "forMany" @@ var "recurse" @@ (unaryFunction Core.termList) @@ var "val0" @@ var "els",
      _Term_map>>: "m" ~> var "forMany" @@ var "forPair"
        @@ ("pairs" ~> Core.termMap $ Maps.fromList $ var "pairs") @@ var "val0" @@ Maps.toList (var "m"),
      _Term_optional>>: "mt" ~> Optionals.maybe
        (var "dflt")
        ("t" ~> var "forSingle"
          @@ var "recurse"
          @@ ("t1" ~> Core.termOptional $ just $ var "t1")
          @@ var "val0"
          @@ var "t")
        (var "mt"),
      _Term_product>>: "terms" ~> var "forMany" @@ var "recurse"
        @@ (unaryFunction Core.termProduct) @@ var "val0" @@ var "terms",
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
      _Term_sum>>: "s" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termSum $ Core.sum (Core.sumIndex $ var "s") (Core.sumSize $ var "s") (var "t"))
        @@ var "val0"
        @@ Core.sumTerm (var "s"),
      _Term_typeApplication>>: "ta" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termTypeApplication $ Core.typedTerm (var "t") (Core.typedTermType $ var "ta"))
        @@ var "val0"
        @@ (Core.typedTermTerm $ var "ta"),
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
        @@ (Core.wrappedTermObject $ var "wt")]) $
--  ref rewriteDef @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse"

rewriteTermDef :: TBinding (((Term -> Term) -> Term -> Term) -> Term -> Term)
rewriteTermDef = define "rewriteTerm" $ "f" ~>
  "fsub" <~ ("recurse" ~> "term" ~>
    "forField" <~ ("f" ~> Core.fieldWithTerm (var "recurse" @@ (Core.fieldTerm $ var "f")) (var "f")) $
    "forElimination" <~ ("elm" ~> cases _Elimination (var "elm") Nothing [
      _Elimination_product>>: "tp" ~> Core.eliminationProduct (var "tp"),
      _Elimination_record>>: "p" ~> Core.eliminationRecord (var "p"),
      _Elimination_union>>: "cs" ~> Core.eliminationUnion $ Core.caseStatement
        (Core.caseStatementTypeName $ var "cs")
        (Optionals.map (var "recurse") (Core.caseStatementDefault $ var "cs"))
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
      "forPair" <~ ("p" ~> pair (var "recurse" @@ (untuple 2 0 @@ var "p")) (var "recurse" @@ (untuple 2 1 @@ var "p"))) $
      Maps.fromList $ Lists.map (var "forPair") $ Maps.toList $ var "m") $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: "at" ~> Core.termAnnotated $ Core.annotatedTerm
        (var "recurse" @@ (Core.annotatedTermSubject $ var "at"))
        (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "a" ~> Core.termApplication $ Core.application
        (var "recurse" @@ (Core.applicationFunction $ var "a"))
        (var "recurse" @@ (Core.applicationArgument $ var "a")),
      _Term_function>>: "fun" ~> Core.termFunction $ var "forFunction" @@ var "fun",
      _Term_let>>: "lt" ~> Core.termLet $ var "forLet" @@ var "lt",
      _Term_list>>: "els" ~> Core.termList $ Lists.map (var "recurse") (var "els"),
      _Term_literal>>: "v" ~> Core.termLiteral $ var "v",
      _Term_map>>: "m" ~> Core.termMap $ var "forMap" @@ var "m",
      _Term_optional>>: "m" ~> Core.termOptional $ Optionals.map (var "recurse") (var "m"),
      _Term_product>>: "tuple" ~> Core.termProduct $ Lists.map (var "recurse") (var "tuple"),
      _Term_record>>: "r" ~> Core.termRecord $ Core.record
        (Core.recordTypeName $ var "r")
        (Lists.map (var "forField") (Core.recordFields $ var "r")),
      _Term_set>>: "s" ~> Core.termSet $ Sets.fromList $ Lists.map (var "recurse") $ Sets.toList (var "s"),
      _Term_sum>>: "s" ~> Core.termSum $ Core.sum
        (Core.sumIndex $ var "s")
        (Core.sumSize $ var "s")
        (var "recurse" @@ (Core.sumTerm $ var "s")),
      _Term_typeApplication>>: "tt" ~> Core.termTypeApplication $ Core.typedTerm
        (var "recurse" @@ (Core.typedTermTerm $ var "tt"))
        (Core.typedTermType $ var "tt"),
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
        (var "recurse" @@ (Core.wrappedTermObject $ var "wt"))]) $
--  ref rewriteDef @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse"

rewriteTermMDef :: TBinding (((Term -> Flow s Term) -> Term -> Flow s Term) -> Term -> Flow s Term)
rewriteTermMDef = define "rewriteTermM" $
  doc "Monadic term rewriting with custom transformation function" $
  "f" ~>
  "fsub" <~ ("recurse" ~> "term" ~>
    "forField" <~ ("field" ~>
      "t" <<~ var "recurse" @@ Core.fieldTerm (var "field") $
      produce $ Core.fieldWithTerm (var "t") (var "field")) $
    "forPair" <~ ("kv" ~>
      "k" <<~ var "recurse" @@ (first $ var "kv") $
      "v" <<~ var "recurse" @@ (second $ var "kv") $
      produce $ pair (var "k") (var "v")) $
    "mapBinding" <~ ("b" ~>
      "v" <<~ var "recurse" @@ (Core.bindingTerm $ var "b") $
      produce $ Core.binding (Core.bindingName $ var "b") (var "v") (Core.bindingType $ var "b")) $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: "at" ~>
        "ex" <<~ var "recurse" @@ Core.annotatedTermSubject (var "at") $
        produce $ Core.termAnnotated $ Core.annotatedTerm (var "ex") (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "app" ~>
        "lhs" <<~ var "recurse" @@ Core.applicationFunction (var "app") $
        "rhs" <<~ var "recurse" @@ Core.applicationArgument (var "app") $
        produce $ Core.termApplication $ Core.application (var "lhs") (var "rhs"),
      _Term_function>>: "fun" ~>
        "rfun" <<~ cases _Function (var "fun") Nothing [
          _Function_elimination>>: "e" ~>
            cases _Elimination (var "e") Nothing [
              _Elimination_product>>: "tp" ~> produce $ Core.functionElimination $ Core.eliminationProduct $ var "tp",
              _Elimination_record>>: "p" ~> produce $ Core.functionElimination $ Core.eliminationRecord $ var "p",
              _Elimination_union>>: "cs" ~>
                "n" <~ Core.caseStatementTypeName (var "cs") $
                "def" <~ Core.caseStatementDefault (var "cs") $
                "cases" <~ Core.caseStatementCases (var "cs") $
                "rdef" <<~ Optionals.maybe (produce nothing)
                  ("t" ~> Flows.map (unaryFunction just) $ var "recurse" @@ var "t")
                  (var "def") $
                Flows.map
                  ("rcases" ~> Core.functionElimination $ Core.eliminationUnion $
                    Core.caseStatement (var "n") (var "rdef") (var "rcases"))
                  (Flows.mapList (var "forField") (var "cases")),
              _Elimination_wrap>>: "name" ~> produce $ Core.functionElimination $ Core.eliminationWrap $ var "name"],
          _Function_lambda>>: "l" ~>
            "v" <~ Core.lambdaParameter (var "l") $
            "d" <~ Core.lambdaDomain (var "l") $
            "body" <~ Core.lambdaBody (var "l") $
            "rbody" <<~ var "recurse" @@ var "body" $
            produce $ Core.functionLambda $ Core.lambda (var "v") (var "d") (var "rbody"),
          _Function_primitive>>: "name" ~> produce $ Core.functionPrimitive $ var "name"] $
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
      _Term_optional>>: "m" ~>
        "rm" <<~ Flows.mapOptional (var "recurse") (var "m") $
        produce $ Core.termOptional $ var "rm",
      _Term_product>>: "tuple" ~> Flows.map
          ("rtuple" ~> Core.termProduct $ var "rtuple")
          (Flows.mapList (var "recurse") (var "tuple")),
      _Term_record>>: "r" ~>
        "n" <~ Core.recordTypeName (var "r") $
        "fields" <~ Core.recordFields (var "r") $
        Flows.map
          ("rfields" ~> Core.termRecord $ Core.record (var "n") (var "rfields"))
          (Flows.mapList (var "forField") (var "fields")),
      _Term_set>>: "s" ~>
        "rlist" <<~ Flows.mapList (var "recurse") (Sets.toList $ var "s") $
        produce $ Core.termSet $ Sets.fromList $ var "rlist",
      _Term_sum>>: "sum" ~>
        "i" <~ Core.sumIndex (var "sum") $
        "s" <~ Core.sumSize (var "sum") $
        "trm" <~ Core.sumTerm (var "sum") $
        "rtrm" <<~ var "recurse" @@ var "trm" $
        produce $ Core.termSum $ Core.sum (var "i") (var "s") (var "rtrm"),
      _Term_typeApplication>>: "tt" ~>
        "t" <<~ var "recurse" @@ Core.typedTermTerm (var "tt") $
        produce $ Core.termTypeApplication $ Core.typedTerm (var "t") (Core.typedTermType (var "tt")),
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
        "t" <~ Core.wrappedTermObject (var "wt") $
        "rt" <<~ var "recurse" @@ var "t" $
        produce $ Core.termWrap $ Core.wrappedTerm (var "name") (var "rt")]) $
--  ref rewriteDef @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse"

rewriteTypeDef :: TBinding (((Type -> Type) -> Type -> Type) -> Type -> Type)
rewriteTypeDef = define "rewriteType" $ "f" ~>
  "fsub" <~ ("recurse" ~> "typ" ~>
    "forField" <~ ("field" ~> Core.fieldTypeWithType (var "field") (var "recurse" @@ (Core.fieldTypeType $ var "field"))) $
    cases _Type (var "typ") Nothing [
      _Type_annotated>>: "at" ~> Core.typeAnnotated $ Core.annotatedType
        (var "recurse" @@ (Core.annotatedTypeSubject $ var "at"))
        (Core.annotatedTypeAnnotation $ var "at"),
      _Type_application>>: "app" ~> Core.typeApplication $ Core.applicationType
        (var "recurse" @@ (Core.applicationTypeFunction $ var "app"))
        (var "recurse" @@ (Core.applicationTypeArgument $ var "app")),
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
      _Type_optional>>: "t" ~> Core.typeOptional $ var "recurse" @@ var "t",
      _Type_product>>: "ts" ~> Core.typeProduct $ Lists.map (var "recurse") (var "ts"),
      _Type_record>>: "rt" ~> Core.typeRecord $ Core.rowType
        (Core.rowTypeTypeName $ var "rt")
        (Lists.map (var "forField") (Core.rowTypeFields $ var "rt")),
      _Type_set>>: "t" ~> Core.typeSet $ var "recurse" @@ var "t",
      _Type_sum>>: "ts" ~> Core.typeSum $ Lists.map (var "recurse") (var "ts"),
      _Type_union>>: "rt" ~> Core.typeUnion $ Core.rowType
        (Core.rowTypeTypeName $ var "rt")
        (Lists.map (var "forField") (Core.rowTypeFields $ var "rt")),
      _Type_unit>>: constant Core.typeUnit,
      _Type_variable>>: "v" ~> Core.typeVariable $ var "v",
      _Type_wrap>>: "wt" ~> Core.typeWrap $ Core.wrappedType
        (Core.wrappedTypeTypeName $ var "wt")
        (var "recurse" @@ (Core.wrappedTypeObject $ var "wt"))]) $
--  ref rewriteDef @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse"

rewriteTypeMDef :: TBinding (((Type -> Flow s Type) -> Type -> Flow s Type) -> Type -> Flow s Type)
rewriteTypeMDef = define "rewriteTypeM" $
  doc "Monadic type rewriting" $
  "f" ~>
  "fsub" <~ ("recurse" ~> "typ" ~> cases _Type (var "typ") Nothing [
    _Type_annotated>>: "at" ~>
      "t" <<~ var "recurse" @@ (Core.annotatedTypeSubject $ var "at") $
      produce $ Core.typeAnnotated $ Core.annotatedType (var "t") (Core.annotatedTypeAnnotation $ var "at"),
    _Type_application>>: "at" ~>
      "lhs" <<~ var "recurse" @@ (Core.applicationTypeFunction $ var "at") $
      "rhs" <<~ var "recurse" @@ (Core.applicationTypeArgument $ var "at") $
      produce $ Core.typeApplication $ Core.applicationType (var "lhs") (var "rhs"),
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
    _Type_optional>>: "t" ~>
      "rt" <<~ var "recurse" @@ var "t" $
      produce $ Core.typeOptional $ var "rt",
    _Type_product>>: "types" ~>
      "rtypes" <<~ Flows.mapList (var "recurse") (var "types") $
      produce $ Core.typeProduct $ var "rtypes",
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
    _Type_sum>>: "types" ~>
      "rtypes" <<~ Flows.mapList (var "recurse") (var "types") $
      produce $ Core.typeSum $ var "rtypes",
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
      "t" <<~ var "recurse" @@ (Core.wrappedTypeObject $ var "wt") $
      produce $ Core.typeWrap $ Core.wrappedType (Core.wrappedTypeTypeName $ var "wt") (var "t")]) $
--  ref rewriteDef @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse"

simplifyTermDef :: TBinding (Term -> Term)
simplifyTermDef = define "simplifyTerm" $
  doc "Simplify terms by applying beta reduction where possible" $
  "term" ~>
  "simplify" <~ ("recurse" ~> "term" ~>
    "stripped" <~ ref deannotateTermDef @@ var "term" $
    var "recurse" @@ (cases _Term (var "stripped")
      (Just $ var "term") [
      _Term_application>>: "app" ~>
        "lhs" <~ Core.applicationFunction (var "app") $
        "rhs" <~ Core.applicationArgument (var "app") $
        "strippedLhs" <~ ref deannotateTermDef @@ var "lhs" $
        cases _Term (var "strippedLhs")
          (Just $ var "term") [
          _Term_function>>: match _Function
            (Just $ var "term") [
            _Function_lambda>>: "l" ~>
              "var" <~ Core.lambdaParameter (var "l") $
              "body" <~ Core.lambdaBody (var "l") $
              Logic.ifElse (Sets.member (var "var") (ref freeVariablesInTermDef @@ var "body"))
                ("strippedRhs" <~ ref deannotateTermDef @@ var "rhs" $
                  cases _Term (var "strippedRhs")
                    (Just $ var "term") [
                    _Term_variable>>: "v" ~>
                      ref simplifyTermDef @@ (ref substituteVariableDef @@ var "var" @@ var "v" @@ var "body")])
                (ref simplifyTermDef @@ var "body")]]])) $
  ref rewriteTermDef @@ var "simplify" @@ var "term"

substituteTypeVariablesDef :: TBinding (M.Map Name Name -> Type -> Type)
substituteTypeVariablesDef = define "substituteTypeVariables" $
  doc "Substitute type variables in a type" $
  "subst" ~> "typ" ~>
  "replace" <~ ("recurse" ~> "typ" ~> cases _Type (var "typ")
    (Just $ var "recurse" @@ var "typ") [
    _Type_variable>>: "n" ~>
      Core.typeVariable $ Optionals.fromMaybe (var "n") $ Maps.lookup (var "n") (var "subst")]) $
  ref rewriteTypeDef @@ var "replace" @@ var "typ"

substituteVariableDef :: TBinding (Name -> Name -> Term -> Term)
substituteVariableDef = define "substituteVariable" $
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
  ref rewriteTermDef @@ var "replace" @@ var "term"

substituteVariablesDef :: TBinding (M.Map Name Name -> Term -> Term)
substituteVariablesDef = define "substituteVariables" $
  doc "Substitute multiple variables in a term" $
  "subst" ~> "term" ~>
  "replace" <~ ("recurse" ~> "term" ~>
    cases _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      _Term_variable>>: "n" ~>
        Core.termVariable $ Optionals.fromMaybe (var "n") $ Maps.lookup (var "n") (var "subst"),
      _Term_function>>: match _Function
        (Just $ var "recurse" @@ var "term") [
        _Function_lambda>>: "l" ~>
          Optionals.maybe
            (var "recurse" @@ var "term")
            (constant $ var "term")
            (Maps.lookup (Core.lambdaParameter $ var "l") (var "subst"))]]) $
  ref rewriteTermDef @@ var "replace" @@ var "term"

subtermsDef :: TBinding (Term -> [Term])
subtermsDef = define "subterms" $
  doc "Find the children of a given term" $
  match _Term Nothing [
    _Term_annotated>>: "at" ~> list [Core.annotatedTermSubject $ var "at"],
    _Term_application>>: "p" ~> list [
      Core.applicationFunction $ var "p",
      Core.applicationArgument $ var "p"],
    _Term_function>>: match _Function
      (Just $ list []) [
      _Function_elimination>>: match _Elimination
        (Just $ list []) [
        _Elimination_union>>: "cs" ~> Lists.concat2
          (Optionals.maybe (list []) ("t" ~> list [var "t"]) (Core.caseStatementDefault $ var "cs"))
          (Lists.map (unaryFunction Core.fieldTerm) (Core.caseStatementCases $ var "cs"))],
      _Function_lambda>>: "l" ~> list [Core.lambdaBody $ var "l"]],
    _Term_let>>: "lt" ~> Lists.cons
      (Core.letBody $ var "lt")
      (Lists.map (unaryFunction Core.bindingTerm) (Core.letBindings $ var "lt")),
    _Term_list>>: "l" ~> var "l",
    _Term_literal>>: constant $ list [],
    _Term_map>>: "m" ~> Lists.concat $ Lists.map
      ("p" ~> list [first $ var "p", second $ var "p"])
      (Maps.toList $ var "m"),
    _Term_optional>>: "m" ~> Optionals.maybe (list []) ("t" ~> list [var "t"]) (var "m"),
    _Term_product>>: "tuple" ~> var "tuple",
    _Term_record>>: "rt" ~> Lists.map (unaryFunction Core.fieldTerm) (Core.recordFields $ var "rt"),
    _Term_set>>: "l" ~> Sets.toList $ var "l",
    _Term_sum>>: "st" ~> list [Core.sumTerm $ var "st"],
    _Term_typeApplication>>: "ta" ~> list [Core.typedTermTerm $ var "ta"],
    _Term_typeLambda>>: "ta" ~> list [Core.typeLambdaBody $ var "ta"],
    _Term_union>>: "ut" ~> list [Core.fieldTerm $ (Core.injectionField $ var "ut")],
    _Term_unit>>: constant $ list [],
    _Term_variable>>: constant $ list [],
    _Term_wrap>>: "n" ~> list [Core.wrappedTermObject $ var "n"]]

subtermsWithAccessorsDef :: TBinding (Term -> [(TermAccessor, Term)])
subtermsWithAccessorsDef = define "subtermsWithAccessors" $
  doc "Find the children of a given term" $
  match _Term Nothing [
    _Term_annotated>>: "at" ~> single Mantle.termAccessorAnnotatedSubject $ Core.annotatedTermSubject $ var "at",
    _Term_application>>: "p" ~> list [
      result Mantle.termAccessorApplicationFunction $ Core.applicationFunction $ var "p",
      result Mantle.termAccessorApplicationArgument $ Core.applicationArgument $ var "p"],
    _Term_function>>: match _Function
      (Just none) [
      _Function_elimination>>: match _Elimination
        (Just none) [
        _Elimination_union>>: "cs" ~> Lists.concat2
          (Optionals.maybe none
            ("t" ~> single Mantle.termAccessorUnionCasesDefault $ var "t")
            (Core.caseStatementDefault $ var "cs"))
          (Lists.map
            ("f" ~> result (Mantle.termAccessorUnionCasesBranch $ Core.fieldName $ var "f") $ Core.fieldTerm $ var "f")
            (Core.caseStatementCases $ var "cs"))],
      _Function_lambda>>: "l" ~> single Mantle.termAccessorLambdaBody $ Core.lambdaBody $ var "l"],
    _Term_let>>: "lt" ~> Lists.cons
      (result Mantle.termAccessorLetEnvironment $ Core.letBody $ var "lt")
      (Lists.map
        ("b" ~> result (Mantle.termAccessorLetBinding $ Core.bindingName $ var "b") $ Core.bindingTerm $ var "b")
        (Core.letBindings $ var "lt")),
    _Term_list>>: "l" ~> Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      ("e" ~> result (Mantle.termAccessorListElement $ int32 0) $ var "e")
      (var "l"),
    _Term_literal>>: constant none,
    _Term_map>>: "m" ~> Lists.concat
      (Lists.map
        ("p" ~> list [
          -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
          result (Mantle.termAccessorMapKey $ int32 0) $ first $ var "p",
          result (Mantle.termAccessorMapValue $ int32 0) $ second $ var "p"])
        (Maps.toList $ var "m")),
    _Term_optional>>: "m" ~> Optionals.maybe none
      ("t" ~> single Mantle.termAccessorOptionalTerm $ var "t")
      (var "m"),
    _Term_product>>: "p" ~> Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      ("e" ~> result (Mantle.termAccessorProductTerm $ int32 0) $ var "e")
      (var "p"),
    _Term_record>>: "rt" ~> Lists.map
      ("f" ~> result (Mantle.termAccessorRecordField $ Core.fieldName $ var "f") $ Core.fieldTerm $ var "f")
      (Core.recordFields $ var "rt"),
    _Term_set>>: "s" ~> Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      ("e" ~> result (Mantle.termAccessorListElement $ int32 0) $ var "e")
      (Sets.toList $ var "s"),
    _Term_sum>>: "st" ~>
      single Mantle.termAccessorSumTerm $
      Core.sumTerm $ var "st",
    _Term_typeApplication>>: "ta" ~>
      single Mantle.termAccessorTypeApplicationTerm $
      Core.typedTermTerm $ var "ta",
    _Term_typeLambda>>: "ta" ~>
      single Mantle.termAccessorTypeLambdaBody $
      Core.typeLambdaBody $ var "ta",
    _Term_union>>: "ut" ~>
      single Mantle.termAccessorInjectionTerm $
      Core.fieldTerm $ (Core.injectionField $ var "ut"),
    _Term_unit>>: constant none,
    _Term_variable>>: constant none,
    _Term_wrap>>: "n" ~> single Mantle.termAccessorWrappedTerm $ Core.wrappedTermObject $ var "n"]
  where
    none = list []
    single accessor term = list [result accessor term]
    result accessor term = pair accessor term
    simple term = result Mantle.termAccessorAnnotatedSubject term

subtypesDef :: TBinding (Type -> [Type])
subtypesDef = define "subtypes" $
  doc "Find the children of a given type expression" $
  match _Type Nothing [
    _Type_annotated>>: "at" ~> list [Core.annotatedTypeSubject $ var "at"],
    _Type_application>>: "at" ~> list [
      Core.applicationTypeFunction $ var "at",
      Core.applicationTypeArgument $ var "at"],
    _Type_function>>: "ft" ~> list [
      Core.functionTypeDomain $ var "ft",
      Core.functionTypeCodomain $ var "ft"],
    _Type_forall>>: "lt" ~> list [Core.forallTypeBody $ var "lt"],
    _Type_list>>: "lt" ~> list [var "lt"],
    _Type_literal>>: constant $ list [],
    _Type_map>>: "mt" ~> list [
      Core.mapTypeKeys $ var "mt",
      Core.mapTypeValues $ var "mt"],
    _Type_optional>>: "ot" ~> list [var "ot"],
    _Type_product>>: "pt" ~> var "pt",
    _Type_record>>: "rt" ~> Lists.map (unaryFunction Core.fieldTypeType) (Core.rowTypeFields $ var "rt"),
    _Type_set>>: "st" ~> list [var "st"],
    _Type_sum>>: "st" ~> var "st",
    _Type_union>>: "rt" ~> Lists.map (unaryFunction Core.fieldTypeType) (Core.rowTypeFields $ var "rt"),
    _Type_unit>>: constant $ list [],
    _Type_variable>>: constant $ list [],
    _Type_wrap>>: "nt" ~> list [Core.wrappedTypeObject $ var "nt"]]

termDependencyNamesDef :: TBinding (Bool -> Bool -> Bool -> Term -> S.Set Name)
termDependencyNamesDef = define "termDependencyNames" $
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
          (Just $ var "names") [
          _Elimination_record>>: "proj" ~> var "nominal" @@ (Core.projectionTypeName $ var "proj"),
          _Elimination_union>>: "caseStmt" ~> var "nominal" @@ (Core.caseStatementTypeName $ var "caseStmt"),
          _Elimination_wrap>>: "name" ~> var "nominal" @@ var "name"]],
      _Term_record>>: "record" ~> var "nominal" @@ (Core.recordTypeName $ var "record"),
      _Term_union>>: "injection" ~> var "nominal" @@ (Core.injectionTypeName $ var "injection"),
      _Term_variable>>: "name" ~> var "var" @@ var "name",
      _Term_wrap>>: "wrappedTerm" ~> var "nominal" @@ (Core.wrappedTermTypeName $ var "wrappedTerm")]) $
  ref foldOverTermDef @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty @@ var "term0"

toShortNamesDef :: TBinding ([Name] -> M.Map Name Name)
toShortNamesDef = define "toShortNames" $
  doc "Generate short names from a list of fully qualified names" $
  "original" ~>
  "addName" <~ ("acc" ~> "name" ~>
    "local" <~ ref Names.localNameOfDef @@ var "name" $
    "group" <~ Optionals.fromMaybe Sets.empty (Maps.lookup (var "local") (var "acc")) $
    Maps.insert (var "local") (Sets.insert (var "name") (var "group")) (var "acc")) $
  "groupNamesByLocal" <~ ("names" ~> Lists.foldl (var "addName") Maps.empty (var "names")) $
  "groups" <~ var "groupNamesByLocal" @@ var "original" $
  "renameGroup" <~ ("localNames" ~>
    "local" <~ first (var "localNames") $
    "names" <~ second (var "localNames") $
    "rangeFrom" <~ ("start" ~> Lists.cons (var "start") (var "rangeFrom" @@ (Math.add (var "start") (int32 1)))) $
    "rename" <~ ("name" ~> "i" ~> pair (var "name") $ Core.name $
      Logic.ifElse (Equality.gt (var "i") (int32 1))
        (Strings.cat2 (var "local") (Literals.showInt32 $ var "i"))
        (var "local")) $
    Lists.zipWith (var "rename") (Sets.toList $ var "names") (var "rangeFrom" @@ int32 1)) $
  Maps.fromList $ Lists.concat $ Lists.map (var "renameGroup") $ Maps.toList $ var "groups"

topologicalSortBindingMapDef :: TBinding (M.Map Name Term -> [[(Name, Term)]])
topologicalSortBindingMapDef = define "topologicalSortBindingMap" $
  doc "Topological sort of connected components, in terms of dependencies between variable/term binding pairs" $
  "bindingMap" ~>
  "bindings" <~ Maps.toList (var "bindingMap") $
  "keys" <~ Sets.fromList (Lists.map (unaryFunction first) (var "bindings")) $
  -- TODO: this function currently serves no purpose; it always yields false
  "hasTypeAnnotation" <~ ("term" ~>
    cases _Term (var "term")
      (Just false) [
      _Term_annotated>>: "at" ~> var "hasTypeAnnotation" @@ (Core.annotatedTermSubject $ var "at")]) $
  "depsOf" <~ ("nameAndTerm" ~>
    "name" <~ first (var "nameAndTerm") $
    "term" <~ second (var "nameAndTerm") $
    pair (var "name") $ Logic.ifElse (var "hasTypeAnnotation" @@ var "term")
      (list [])
      (Sets.toList $ Sets.intersection (var "keys") $ ref freeVariablesInTermDef @@ var "term")) $
  "toPair" <~ ("name" ~> pair (var "name") $ Optionals.fromMaybe
    (Core.termLiteral $ Core.literalString $ string "Impossible!")
    (Maps.lookup (var "name") (var "bindingMap"))) $
  Lists.map (unaryFunction $ Lists.map $ var "toPair") (ref Sorting.topologicalSortComponentsDef @@ Lists.map (var "depsOf") (var "bindings"))

topologicalSortBindingsDef :: TBinding ([Binding] -> Either [[Name]] [Name])
topologicalSortBindingsDef = define "topologicalSortBindings" $
  doc "Topological sort of elements based on their dependencies" $
  "els" ~>
  "adjlist" <~ ("e" ~> pair
    (Core.bindingName $ var "e")
    (Sets.toList $ ref termDependencyNamesDef @@ false @@ true @@ true @@ (Core.bindingTerm $ var "e"))) $
  ref Sorting.topologicalSortDef @@ Lists.map (var "adjlist") (var "els")

typeDependencyNamesDef :: TBinding (Bool -> Type -> S.Set Name)
typeDependencyNamesDef = define "typeDependencyNames" $
  "withSchema" ~> "typ" ~>
    Logic.ifElse (var "withSchema")
      (Sets.union
        (ref freeVariablesInTypeDef @@ var "typ")
        (ref typeNamesInTypeDef @@ var "typ"))
      (ref freeVariablesInTypeDef @@ var "typ")

typeNamesInTypeDef :: TBinding (Type -> S.Set Name)
typeNamesInTypeDef = define "typeNamesInType" $
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
  ref foldOverTypeDef @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty @@ var "typ0"

unshadowVariablesDef :: TBinding (Term -> Term)
unshadowVariablesDef = define "unshadowVariables" $
  doc "Unshadow lambda-bound variables in a term" $
  "term" ~>
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
                (second $ var "rewrite"
                  @@ var "recurse"
                  @@ (Maps.insert (var "v") (int32 1) (var "m"))
                  @@ (var "body")))
              ("i" ~>
                "i2" <~ Math.add (var "i") (int32 1) $
                "v2" <~ Core.name (Strings.cat2 (Core.unName $ var "v") (Literals.showInt32 $ var "i2")) $
                "m2" <~ Maps.insert (var "v") (var "i2") (var "m") $
                Core.termFunction $ Core.functionLambda $ Core.lambda (var "v2") (var "domain")
                  (second $ var "rewrite" @@ var "recurse" @@ var "m2" @@ var "body"))],
        _Term_variable>>: "v" ~> pair (var "m") $ Core.termVariable $ optCases (Maps.lookup (var "v") (var "m"))
          (var "v")
          ("i" ~> Logic.ifElse (Equality.equal (var "i") (int32 1))
            (var "v")
            (Core.name $ Strings.cat2 (Core.unName $ var "v") (Literals.showInt32 $ var "i")))]) $
    second (ref rewriteAndFoldTermDef @@ var "rewrite" @@ Maps.empty @@ var "term")
