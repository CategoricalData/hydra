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
     el expandTypedLambdasDef,
     el flattenLetTermsDef,
     el foldOverTermDef,
     el foldOverTypeDef,
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
     el replaceFreeNameDef,
     el rewriteDef,
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
     el topologicalSortBindingsDef,
     el topologicalSortElementsDef,
     el typeDependencyNamesDef,
     el typeNamesInTypeDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

deannotateAndDetypeTermDef :: TElement (Term -> Term)
deannotateAndDetypeTermDef = define "deannotateAndDetypeTerm" $
  doc "Strip type annotations from the top levels of a term" $
  lambda "t" $ cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: lambda "at" $ ref deannotateAndDetypeTermDef @@ (Core.annotatedTermSubject $ var "at"),
    _Term_typeAbstraction>>: lambda "ta" $ ref deannotateAndDetypeTermDef @@ (Core.typeAbstractionBody $ var "ta"),
    _Term_typeApplication>>: lambda "tt" $ ref deannotateAndDetypeTermDef @@ (Core.typedTermTerm $ var "tt")]

deannotateTermDef :: TElement (Term -> Term)
deannotateTermDef = define "deannotateTerm" $
  doc "Strip all annotations from a term" $
  lambda "t" $ cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: ref deannotateTermDef <.> (project _AnnotatedTerm _AnnotatedTerm_subject)]

deannotateTypeDef :: TElement (Type -> Type)
deannotateTypeDef = define "deannotateType" $
  doc "Strip all annotations from a term" $
  lambda "t" $ cases _Type (var "t")
    (Just $ var "t") [
    _Type_annotated>>: ref deannotateTypeDef <.> (project _AnnotatedType _AnnotatedType_subject)]

deannotateTypeParametersDef :: TElement (Type -> Type)
deannotateTypeParametersDef = define "deannotateTypeParameters" $
  doc "Strip any top-level type lambdas from a type, extracting the (possibly nested) type body" $
  lambda "t" $ cases _Type (ref deannotateTypeDef @@ var "t")
    (Just $ var "t") [
    _Type_forall>>: lambda "lt" (ref deannotateTypeParametersDef @@ (project _ForallType _ForallType_body @@ var "lt"))]

deannotateTypeRecursiveDef :: TElement (Type -> Type)
deannotateTypeRecursiveDef = define "deannotateTypeRecursive" $
  doc "Recursively strip all annotations from a type" $
  lambda "typ" $ lets [
    "strip">: lambdas ["recurse", "typ"] $ lets [
      "rewritten">: var "recurse" @@ var "typ"] $
      cases _Type (var "rewritten")
        (Just $ var "rewritten") [
        _Type_annotated>>: lambda "at" $ Core.annotatedTypeSubject $ var "at"]] $
    ref rewriteTypeDef @@ var "strip" @@ var "typ"

deannotateTypeSchemeRecursiveDef :: TElement (TypeScheme -> TypeScheme)
deannotateTypeSchemeRecursiveDef = define "deannotateTypeSchemeRecursive" $
  doc "Recursively strip all annotations from a type scheme" $
  lambda "ts" $ lets [
    "vars">: Core.typeSchemeVariables $ var "ts",
    "typ">: Core.typeSchemeType $ var "ts"] $
    Core.typeScheme (var "vars") (ref deannotateTypeRecursiveDef @@ var "typ")

expandTypedLambdasDef :: TElement (Term -> Term)
expandTypedLambdasDef = define "expandTypedLambdas" $
  doc "A variation of expandLambdas which also attaches type annotations when padding function terms" $
  lambda "term" $ lets [
    "toNaryFunType">: lambda "typ" $ lets [
      "helper">: lambda "t" $
        cases _Type (var "t")
          (Just $ pair (list []) (var "t")) [
          _Type_function>>: lambda "ft" $ lets [
            "dom0">: Core.functionTypeDomain $ var "ft",
            "cod0">: Core.functionTypeCodomain $ var "ft",
            "recursive">: var "helper" @@ var "cod0",
            "doms">: first $ var "recursive",
            "cod1">: second $ var "recursive"]
            $ pair (Lists.cons (var "dom0") (var "doms")) (var "cod1")]]
      $ var "helper" @@ (ref deannotateTypeDef @@ var "typ"),
    "padTerm">: lambdas ["i", "doms", "cod", "term"] $
      Logic.ifElse (Lists.null $ var "doms")
        (var "term")
        (lets [
          "dom">: Lists.head $ var "doms",
          "var">: Core.name $ Strings.cat2 (string "v") (Literals.showInt32 $ var "i"),
          "tailDoms">: Lists.tail $ var "doms",
          "toFunctionType">: lambdas ["doms", "cod"] $
            Lists.foldl
              (lambda "c" $ lambda "d" $ Core.typeFunction $ Core.functionType (var "d") (var "c"))
              (var "cod")
              (var "doms")]
          $ Core.termFunction $ Core.functionLambda $ Core.lambda (var "var") (just $ var "dom") $
            var "padTerm"
              @@ (Math.add (var "i") (int32 1))
              @@ (var "tailDoms")
              @@ (var "cod")
              @@ (Core.termApplication $ Core.application
                    (var "term")
                    (Core.termVariable $ var "var"))),
    "expand">: lambdas ["doms", "cod", "term"] $
      cases _Term (var "term")
        (Just $ ref rewriteTermDef @@ var "rewrite" @@ var "term") [
        _Term_annotated>>: lambda "at" $ Core.termAnnotated $ Core.annotatedTerm
          (var "expand" @@ var "doms" @@ var "cod" @@ (Core.annotatedTermSubject $ var "at"))
          (Core.annotatedTermAnnotation $ var "at"),
        _Term_application>>: lambda "app" $ lets [
          "lhs">: Core.applicationFunction $ var "app",
          "rhs">: Core.applicationArgument $ var "app"]
          $ ref rewriteTermDef @@ var "rewrite" @@ var "term",
        _Term_function>>: match _Function
          (Just $ var "padTerm" @@ int32 1 @@ var "doms" @@ var "cod" @@ var "term") [
          _Function_lambda>>: lambda "l" $ Core.termFunction $ Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Core.lambdaDomain $ var "l")
            (var "expand" @@ (Lists.tail $ var "doms") @@ var "cod" @@ (Core.lambdaBody $ var "l"))],
        _Term_let>>: lambda "lt" $ lets [
          "expandBinding">: lambda "b" $ Core.letBinding
            (Core.letBindingName $ var "b")
            (ref expandTypedLambdasDef @@ (Core.letBindingTerm $ var "b"))
            (Core.letBindingType $ var "b")]
          $ Core.termLet $ Core.let_
            (Lists.map (var "expandBinding") (Core.letBindings $ var "lt"))
            (var "expand" @@ var "doms" @@ var "cod" @@ (Core.letEnvironment $ var "lt"))],
    "rewrite">: lambdas ["recurse", "term"] $ var "recurse" @@ var "term"]
    $ ref rewriteTermDef @@ var "rewrite" @@ var "term"

flattenLetTermsDef :: TElement (Term -> Term)
flattenLetTermsDef = define "flattenLetTerms" $
  doc "Flatten nested let expressions" $
  lambda "term" $ lets [
    "rewriteBinding">: lambda "binding" $ lets [
      "key0">: Core.letBindingName $ var "binding",
      "val0">: Core.letBindingTerm $ var "binding",
      "t">: Core.letBindingType $ var "binding"] $
      cases _Term (var "val0")
        (Just $ pair (Core.letBinding (var "key0") (var "val0") (var "t")) (list [])) [
        _Term_annotated>>: lambda "at" $ lets [
          "val1">: Core.annotatedTermSubject $ var "at",
          "ann">: Core.annotatedTermAnnotation $ var "at",
          "recursive">: var "rewriteBinding" @@ (Core.letBinding (var "key0") (var "val1") (var "t")),
          "innerBinding">: first $ var "recursive",
          "deps">: second $ var "recursive",
          "val2">: Core.letBindingTerm $ var "innerBinding"]
          $ pair
            (Core.letBinding (var "key0") (Core.termAnnotated $ Core.annotatedTerm (var "val2") (var "ann")) (var "t"))
            (var "deps"),
        _Term_let>>: lambda "innerLet" $ lets [
          "bindings1">: Core.letBindings $ var "innerLet",
          "body1">: Core.letEnvironment $ var "innerLet",
          "prefix">: Strings.cat2 (unwrap _Name @@ var "key0") (string "_"),
          "qualify">: lambda "n" $ Core.name $ Strings.cat2 (var "prefix") (unwrap _Name @@ var "n"),
          "toSubstPair">: lambda "b" $ pair (Core.letBindingName $ var "b") (var "qualify" @@ (Core.letBindingName $ var "b")),
          "subst">: Maps.fromList $ Lists.map (var "toSubstPair") (var "bindings1"),
          "replaceVars">: ref substituteVariablesDef @@ var "subst",
          "newBody">: var "replaceVars" @@ var "body1",
          "newBinding">: lambda "b" $ Core.letBinding
            (var "qualify" @@ (Core.letBindingName $ var "b"))
            (var "replaceVars" @@ (Core.letBindingTerm $ var "b"))
            (Core.letBindingType $ var "b")]
          $ pair
            (Core.letBinding (var "key0") (var "newBody") (var "t"))
            (Lists.map (var "newBinding") (var "bindings1"))],
    "flatten">: lambdas ["recurse", "term"] $ lets [
      "rewritten">: var "recurse" @@ var "term"] $
      cases _Term (var "rewritten")
        (Just $ var "rewritten") [
        _Term_let>>: lambda "lt" $ lets [
          "bindings">: Core.letBindings $ var "lt",
          "body">: Core.letEnvironment $ var "lt",
          "forResult">: lambda "hr" $ Lists.cons (first $ var "hr") (second $ var "hr"),
          "newBindings">: Lists.concat $ Lists.map (var "forResult" <.> var "rewriteBinding") (var "bindings")] $
          Core.termLet $ Core.let_ (var "newBindings") (var "body")]]
    $ ref rewriteTermDef @@ var "flatten" @@ var "term"

foldOverTermDef :: TElement (TraversalOrder -> (x -> Term -> x) -> x -> Term -> x)
foldOverTermDef = define "foldOverTerm" $
  doc "Fold over a term, traversing its subterms in the specified order" $
  lambdas ["order", "fld", "b0", "term"] $ cases _TraversalOrder (var "order") Nothing [
    _TraversalOrder_pre>>: constant (Phantoms.fold (ref foldOverTermDef @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "term")
      @@ (ref subtermsDef @@ var "term")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Phantoms.fold (ref foldOverTermDef @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (ref subtermsDef @@ var "term"))
      @@ var "term")]

foldOverTypeDef :: TElement (TraversalOrder -> (x -> Type -> x) -> x -> Type -> x)
foldOverTypeDef = define "foldOverType" $
  doc "Fold over a type, traversing its subtypes in the specified order" $
  lambdas ["order", "fld", "b0", "typ"] $ cases _TraversalOrder (var "order") Nothing [
    _TraversalOrder_pre>>: constant (Phantoms.fold (ref foldOverTypeDef @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "typ")
      @@ (ref subtypesDef @@ var "typ")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Phantoms.fold (ref foldOverTypeDef @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (ref subtypesDef @@ var "typ"))
      @@ var "typ")]

freeVariablesInTermDef :: TElement (Term -> S.Set Name)
freeVariablesInTermDef = define "freeVariablesInTerm" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a term" $
  lambda "term" $ lets [
    "dfltVars">: Phantoms.fold (lambda "s" $ lambda "t" $ Sets.union (var "s") (ref freeVariablesInTermDef @@ var "t"))
      @@ Sets.empty
      @@ (ref subtermsDef @@ var "term")] $
    cases _Term (var "term")
      (Just $ var "dfltVars") [
      _Term_function>>: match _Function (Just $ var "dfltVars") [
        _Function_lambda>>: lambda "l" (Sets.delete
          (Core.lambdaParameter $ var "l")
          (ref freeVariablesInTermDef @@ (Core.lambdaBody $ var "l")))],
--      TODO: restore the following
--      _Term_let>>: lambda "l" (Sets.difference
--        @@ (ref freeVariablesInTermDef @@ (Core.letEnvironment $ var "l"))
--        @@ (Sets.fromList (Lists.map first (Maps.toList (Core.letBindings $ var "l"))))),
      _Term_variable>>: lambda "v" (Sets.singleton $ var "v")]

freeVariablesInTypeDef :: TElement (Type -> S.Set Name)
freeVariablesInTypeDef = define "freeVariablesInType" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a type" $
  lambda "typ" $ lets [
    "dfltVars">: Phantoms.fold (lambda "s" $ lambda "t" $ Sets.union (var "s") (recurse @@ var "t"))
      @@ Sets.empty
      @@ (ref subtypesDef @@ var "typ")] $
    cases _Type (var "typ")
      (Just $ var "dfltVars") [
      _Type_forall>>: lambda "lt" (Sets.delete
          (Core.forallTypeParameter $ var "lt")
          (recurse @@ (Core.forallTypeBody $ var "lt"))),
      -- TODO: let-types
      _Type_variable>>: lambda "v" (Sets.singleton $ var "v")]
  where
    recurse = ref freeVariablesInTypeDef

freeVariablesInTypeOrderedDef :: TElement (Type -> [Name])
freeVariablesInTypeOrderedDef = define "freeVariablesInTypeOrdered" $
  doc "Find the free variables in a type in deterministic left-to-right order" $
  lambda "typ" $ lets [
    "collectVars">: lambdas ["boundVars", "t"] $
      cases _Type (var "t")
        (Just $ Lists.concat $ Lists.map (var "collectVars" @@ var "boundVars") $
                ref subtypesDef @@ var "t") [
        _Type_variable>>: lambda "v" $
          Logic.ifElse (Sets.member (var "v") (var "boundVars"))
            (list [])
            (list [var "v"]),
        _Type_forall>>: lambda "ft" $
          var "collectVars" @@
            (Sets.insert (Core.forallTypeParameter $ var "ft") (var "boundVars")) @@
            (Core.forallTypeBody $ var "ft")]] $
    (Lists.nub :: TTerm [Name] -> TTerm [Name]) $ var "collectVars" @@ Sets.empty @@ var "typ"

freeVariablesInTypeSimpleDef :: TElement (Type -> S.Set Name)
freeVariablesInTypeSimpleDef = define "freeVariablesInTypeSimple" $
  doc "Same as freeVariablesInType, but ignores the binding action of lambda types" $
  lambda "typ" $ lets [
    "helper">: lambdas ["types", "typ"] $ cases _Type (var "typ")
      (Just $ var "types") [
      _Type_variable>>: lambda "v" $ Sets.insert (var "v") (var "types")]] $
    ref foldOverTypeDef @@ Coders.traversalOrderPre @@ var "helper" @@ Sets.empty @@ var "typ"

freeVariablesInTypeSchemeDef :: TElement (TypeScheme -> S.Set Name)
freeVariablesInTypeSchemeDef = define "freeVariablesInTypeScheme" $
  doc "Find free variables in a type scheme" $
  lambda "ts" $ lets [
    "vars">: Core.typeSchemeVariables $ var "ts",
    "t">: Core.typeSchemeType $ var "ts"]
    $ Sets.difference (ref freeVariablesInTypeDef @@ var "t") (Sets.fromList $ var "vars")

freeVariablesInTypeSchemeSimpleDef :: TElement (TypeScheme -> S.Set Name)
freeVariablesInTypeSchemeSimpleDef = define "freeVariablesInTypeSchemeSimple" $
  doc "Find free variables in a type scheme (simple version)" $
  lambda "ts" $ lets [
    "vars">: Core.typeSchemeVariables $ var "ts",
    "t">: Core.typeSchemeType $ var "ts"]
    $ Sets.difference (ref freeVariablesInTypeSimpleDef @@ var "t") (Sets.fromList $ var "vars")

inlineTypeDef :: TElement (M.Map Name Type -> Type -> Flow s Type)
inlineTypeDef = define "inlineType" $
  doc "Inline all type variables in a type using the provided schema. Note: this function is only appropriate for nonrecursive type definitions" $
  lambdas ["schema", "typ"] $ lets [
    "f">: lambdas ["recurse", "typ"] $ binds [
      "tr">: var "recurse" @@ var "typ"] $
      cases _Type (var "tr")
        (Just $ produce $ var "tr") [
        _Type_variable>>: lambda "v" $
          Optionals.maybe
            (Flows.fail $ Strings.cat2 (string "No such type in schema: ") (unwrap _Name @@ var "v"))
            (ref inlineTypeDef @@ var "schema")
            (Maps.lookup (var "v") (var "schema"))]] $
    ref rewriteTypeMDef @@ var "f" @@ var "typ"

isFreeVariableInTermDef :: TElement (Name -> Term -> Bool)
isFreeVariableInTermDef = define "isFreeVariableInTerm" $
 doc "Check whether a variable is free (not bound) in a term" $
 lambda "v" $ lambda "term" $
   Logic.not $ Sets.member (var "v") (ref freeVariablesInTermDef @@ var "term")

isLambdaDef :: TElement (Term -> Bool)
isLambdaDef = define "isLambda" $
  doc "Check whether a term is a lambda, possibly nested within let and/or annotation terms" $
  lambda "term" $ cases _Term (ref deannotateTermDef @@ var "term")
    (Just false) [
    _Term_function>>: match _Function
      (Just false) [
      _Function_lambda>>: constant true],
    _Term_let>>: lambda "lt" (ref isLambdaDef @@ (project _Let _Let_environment @@ var "lt"))]

mapBeneathTypeAnnotationsDef :: TElement ((Type -> Type) -> Type -> Type)
mapBeneathTypeAnnotationsDef = define "mapBeneathTypeAnnotations" $
  doc "Apply a transformation to the first type beneath a chain of annotations" $
  lambdas ["f", "t"] $ cases _Type (var "t")
    (Just $ var "f" @@ var "t") [
    _Type_annotated>>: lambda "at" $ Core.typeAnnotated $ Core.annotatedType
      (ref mapBeneathTypeAnnotationsDef @@ var "f" @@ (Core.annotatedTypeSubject $ var "at"))
      (Core.annotatedTypeAnnotation $ var "at")]

normalizeTypeVariablesInTermDef :: TElement (Term -> Term)
normalizeTypeVariablesInTermDef = define "normalizeTypeVariablesInTerm" $
  doc "Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ..." $
  lambda "term" $ lets [
    "substType">: lambdas ["subst", "typ"] $ lets [
      "rewrite">: lambdas ["recurse", "typ"] $ cases _Type (var "typ")
        (Just $ var "recurse" @@ var "typ") [
        _Type_variable>>: lambda "v" $ Core.typeVariable $ var "replaceName" @@ var "subst" @@ var "v"]] $
      ref rewriteTypeDef @@ var "rewrite" @@ var "typ",
    "replaceName">: lambdas ["subst", "v"] $ Optionals.fromMaybe (var "v") $ Maps.lookup (var "v") (var "subst"),
    "rewriteWithSubst">: lambda "substAndBound" $ lets [
      "subst">: first $ var "substAndBound",
      "boundVars">: second $ var "substAndBound",
      "rewrite">: lambdas ["recurse", "term"] $ cases _Term (var "term")
        (Just $ var "recurse" @@ var "term") [
        _Term_function>>: match _Function
          (Just $ var "recurse" @@ var "term") [
          _Function_elimination>>: match _Elimination
            (Just $ var "recurse" @@ var "term") [
            _Elimination_product>>: lambda "tproj" $ lets [
              "arity">: Core.tupleProjectionArity $ var "tproj",
              "index">: Core.tupleProjectionIndex $ var "tproj",
              "domain">: Core.tupleProjectionDomain $ var "tproj"] $
              Core.termFunction $ Core.functionElimination $ Core.eliminationProduct $ Core.tupleProjection
                (var "arity")
                (var "index")
                (Optionals.map
                  (lambda "types" $ Lists.map (var "substType" @@ var "subst") (var "types"))
                  (var "domain"))],
          _Function_lambda>>: lambda "l" $ Core.termFunction $ Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Optionals.map (var "substType" @@ var "subst") (Core.lambdaDomain $ var "l"))
            (var "rewriteWithSubst" @@ (pair (var "subst") (var "boundVars")) @@ (Core.lambdaBody $ var "l"))],
        _Term_let>>: lambda "lt" $ lets [
          "bindings">: Core.letBindings $ var "lt",
          "env">: Core.letEnvironment $ var "lt",
          "rewriteBinding">: lambda "b" $
            Optionals.maybe
              (var "b")
              (lambda "ts" $ lets [
                "vars">: Core.typeSchemeVariables $ var "ts",
                "typ">: Core.typeSchemeType $ var "ts",
                "varsLen">: Lists.length $ var "vars",
                "boundVarsLen">: Sets.size $ var "boundVars",
                "normalVariables">: Lists.map (lambda "i" $ Core.name $ Strings.cat2 (string "t") (Literals.showInt32 $ var "i")) $
                  Math.range (int32 0) (Math.add (var "varsLen") (var "boundVarsLen")),
                "newVars">: Lists.take (Lists.length $ var "vars") $ Lists.filter
                  (lambda "n" $ Logic.not $ Sets.member (var "n") (var "boundVars"))
                  (var "normalVariables"),
                "newSubst">: Maps.union (Maps.fromList $ Lists.zip (var "vars") (var "newVars")) (var "subst"),
                "newValue">: var "rewriteWithSubst" @@ (pair (var "newSubst") (Sets.union (var "boundVars") (Sets.fromList $ var "newVars"))) @@ (Core.letBindingTerm $ var "b")]
                $ Core.letBinding
                  (Core.letBindingName $ var "b")
                  (var "newValue")
                  (just $ Core.typeScheme (var "newVars") (var "substType" @@ var "newSubst" @@ var "typ")))
              (Core.letBindingType $ var "b")]
          $ Core.termLet $ Core.let_
            (Lists.map (var "rewriteBinding") (var "bindings"))
            (var "rewriteWithSubst" @@ (pair (var "subst") (var "boundVars")) @@ var "env"),
        _Term_typeAbstraction>>: lambda "ta" $ Core.termTypeAbstraction $ Core.typeAbstraction
          (var "replaceName" @@ var "subst" @@ (Core.typeAbstractionParameter $ var "ta"))
          (var "rewriteWithSubst" @@ (pair (var "subst") (var "boundVars")) @@ (Core.typeAbstractionBody $ var "ta")),
        _Term_typeApplication>>: lambda "tt" $ Core.termTypeApplication $ Core.typedTerm
          (var "rewriteWithSubst" @@ (pair (var "subst") (var "boundVars")) @@ (Core.typedTermTerm $ var "tt"))
          (var "substType" @@ var "subst" @@ (Core.typedTermType $ var "tt"))]] $
      ref rewriteTermDef @@ var "rewrite"] $
    var "rewriteWithSubst" @@ (pair Maps.empty Sets.empty) @@ var "term"

removeTermAnnotationsDef :: TElement (Term -> Term)
removeTermAnnotationsDef = define "removeTermAnnotations" $
  doc "Recursively remove term annotations, including within subterms" $
  lambda "term" $ lets [
    "remove">: lambdas ["recurse", "term"] $ lets [
      "rewritten">: var "recurse" @@ var "term"] $
      cases _Term (var "term")
        (Just $ var "rewritten") [
        _Term_annotated>>: lambda "at" $ Core.annotatedTermSubject $ var "at"]]
    $ ref rewriteTermDef @@ var "remove" @@ var "term"

removeTypeAnnotationsDef :: TElement (Type -> Type)
removeTypeAnnotationsDef = define "removeTypeAnnotations" $
  doc "Recursively remove type annotations, including within subtypes" $
  lambda "typ" $ lets [
    "remove">: lambdas ["recurse", "typ"] $ lets [
      "rewritten">: var "recurse" @@ var "typ"] $
      cases _Type (var "rewritten")
        (Just $ var "rewritten") [
        _Type_annotated>>: lambda "at" $ Core.annotatedTypeSubject $ var "at"]] $
    ref rewriteTypeDef @@ var "remove" @@ var "typ"

removeTypesFromTermDef :: TElement (Term -> Term)
removeTypesFromTermDef = define "removeTypesFromTerm" $
  doc "Strip type annotations from terms while preserving other annotations" $
  lambda "term" $ lets [
    "strip">: lambdas ["recurse", "term"] $ lets [
      "rewritten">: var "recurse" @@ var "term",
      "stripBinding">: lambda "b" $ Core.letBinding
        (Core.letBindingName $ var "b")
        (Core.letBindingTerm $ var "b")
        nothing] $
      cases _Term (var "rewritten")
        (Just $ var "rewritten") [
        _Term_function>>: lambda "f" $ cases _Function (var "f")
          (Just $ Core.termFunction $ var "f") [
          _Function_elimination>>: lambda "e" $ cases _Elimination (var "e")
            (Just $ Core.termFunction $ Core.functionElimination $ var "e") [
            _Elimination_product>>: lambda "tp" $ Core.termFunction $ Core.functionElimination $ Core.eliminationProduct $
              Core.tupleProjection
                (Core.tupleProjectionIndex $ var "tp")
                (Core.tupleProjectionArity $ var "tp")
                nothing],
          _Function_lambda>>: lambda "l" $ Core.termFunction $ Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            nothing
            (Core.lambdaBody $ var "l")],
        _Term_let>>: lambda "lt" $ Core.termLet $ Core.let_
          (Lists.map (var "stripBinding") (Core.letBindings $ var "lt"))
          (Core.letEnvironment $ var "lt"),
        _Term_typeAbstraction>>: lambda "ta" $ Core.typeAbstractionBody $ var "ta",
        _Term_typeApplication>>: lambda "tt" $ Core.typedTermTerm $ var "tt"]]
    $ ref rewriteTermDef @@ var "strip" @@ var "term"

replaceFreeNameDef :: TElement (Name -> Type -> Type -> Type)
replaceFreeNameDef = define "replaceFreeName" $
  doc "Replace free occurrences of a name in a type" $
  lambdas ["v", "rep", "typ"] $ lets [
    "mapExpr">: lambdas ["recurse", "t"] $ cases _Type (var "t")
      (Just $ var "recurse" @@ var "t") [
      _Type_forall>>: lambda "ft" $ Logic.ifElse
        (Equality.equal (var "v") (Core.forallTypeParameter $ var "ft"))
        (var "t")
        (Core.typeForall $ Core.forallType
          (Core.forallTypeParameter $ var "ft")
          (var "recurse" @@ (Core.forallTypeBody $ var "ft"))),
      _Type_variable>>: lambda "v'" $ Logic.ifElse
        (Equality.equal (var "v") (var "v'"))
        (var "rep")
        (var "t")]] $
    ref rewriteTypeDef @@ var "mapExpr" @@ var "typ"

rewriteDef :: TElement (((x -> y) -> x -> y) -> ((x -> y) -> x -> y) -> x -> y)
rewriteDef = define "rewrite" $ lambdas ["fsub", "f"] $ lets [
  "recurse">: var "f" @@ (var "fsub" @@ var "recurse")] $
  var "recurse"

rewriteTermDef :: TElement (((Term -> Term) -> Term -> Term) -> Term -> Term)
rewriteTermDef = define "rewriteTerm" $ lambda "f" $ lets [
  "fsub">: lambdas ["recurse", "term"] $ lets [
    "forElimination">: lambda "elm" $ cases _Elimination (var "elm") Nothing [
      _Elimination_product>>: lambda "tp" $ Core.eliminationProduct $ var "tp",
      _Elimination_record>>: lambda "p" $ Core.eliminationRecord $ var "p",
      _Elimination_union>>: lambda "cs" $ Core.eliminationUnion $ Core.caseStatement
        (Core.caseStatementTypeName $ var "cs")
        (Optionals.map (var "recurse") (Core.caseStatementDefault $ var "cs"))
        (Lists.map (var "forField") (Core.caseStatementCases $ var "cs")),
      _Elimination_wrap>>: lambda "name" $ Core.eliminationWrap $ var "name"],
    "forField">: lambda "f" $ Core.fieldWithTerm (var "recurse" @@ (Core.fieldTerm $ var "f")) (var "f"),
    "forFunction">: lambda "fun" $ cases _Function (var "fun") Nothing [
      _Function_elimination>>: lambda "elm" $ Core.functionElimination $ var "forElimination" @@ var "elm",
      _Function_lambda>>: lambda "l" $ Core.functionLambda $ Core.lambda
        (Core.lambdaParameter $ var "l")
        (Core.lambdaDomain $ var "l")
        (var "recurse" @@ (Core.lambdaBody $ var "l")),
      _Function_primitive>>: lambda "name" $ Core.functionPrimitive $ var "name"],
    "forLet">: lambda "lt" $ lets [
      "mapBinding">: lambda "b" $ Core.letBinding
        (Core.letBindingName $ var "b")
        (var "recurse" @@ (Core.letBindingTerm $ var "b"))
        (Core.letBindingType $ var "b")] $
      Core.let_
        (Lists.map (var "mapBinding") (Core.letBindings $ var "lt"))
        (var "recurse" @@ (Core.letEnvironment $ var "lt")),
    "forMap">: lambda "m" $ lets [
      "forPair">: lambda "p" $ pair (var "recurse" @@ (untuple 2 0 @@ var "p")) (var "recurse" @@ (untuple 2 1 @@ var "p"))] $
      Maps.fromList $ Lists.map (var "forPair") $ Maps.toList $ var "m"] $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: lambda "at" $ Core.termAnnotated $ Core.annotatedTerm
        (var "recurse" @@ (Core.annotatedTermSubject $ var "at"))
        (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: lambda "a" $ Core.termApplication $ Core.application
        (var "recurse" @@ (Core.applicationFunction $ var "a"))
        (var "recurse" @@ (Core.applicationArgument $ var "a")),
      _Term_function>>: lambda "fun" $ Core.termFunction $ var "forFunction" @@ var "fun",
      _Term_let>>: lambda "lt" $ Core.termLet $ var "forLet" @@ var "lt",
      _Term_list>>: lambda "els" $ Core.termList $ Lists.map (var "recurse") (var "els"),
      _Term_literal>>: lambda "v" $ Core.termLiteral $ var "v",
      _Term_map>>: lambda "m" $ Core.termMap $ var "forMap" @@ var "m",
      _Term_wrap>>: lambda "wt" $ Core.termWrap $ Core.wrappedTerm
        (Core.wrappedTermTypeName $ var "wt")
        (var "recurse" @@ (Core.wrappedTermObject $ var "wt")),
      _Term_optional>>: lambda "m" $ Core.termOptional $ Optionals.map (var "recurse") (var "m"),
      _Term_product>>: lambda "tuple" $ Core.termProduct $ Lists.map (var "recurse") (var "tuple"),
      _Term_record>>: lambda "r" $ Core.termRecord $ Core.record
        (Core.recordTypeName $ var "r")
        (Lists.map (var "forField") (Core.recordFields $ var "r")),
      _Term_set>>: lambda "s" $ Core.termSet $ Sets.fromList $ Lists.map (var "recurse") $ Sets.toList (var "s"),
      _Term_sum>>: lambda "s" $ Core.termSum $ Core.sum
        (Core.sumIndex $ var "s")
        (Core.sumSize $ var "s")
        (var "recurse" @@ (Core.sumTerm $ var "s")),
      _Term_typeAbstraction>>: lambda "ta" $ Core.termTypeAbstraction $ Core.typeAbstraction
        (Core.typeAbstractionParameter $ var "ta")
        (var "recurse" @@ (Core.typeAbstractionBody $ var "ta")),
      _Term_typeApplication>>: lambda "tt" $ Core.termTypeApplication $ Core.typedTerm
        (var "recurse" @@ (Core.typedTermTerm $ var "tt"))
        (Core.typedTermType $ var "tt"),
      _Term_union>>: lambda "i" $ Core.termUnion $ Core.injection
        (Core.injectionTypeName $ var "i")
        (var "forField" @@ (Core.injectionField $ var "i")),
      _Term_unit>>: constant Core.termUnit,
      _Term_variable>>: lambda "v" $ Core.termVariable $ var "v"]] $
  ref rewriteDef @@ var "fsub" @@ var "f"
  where
    foo = emit

emit :: String
emit = "pure"

rewriteTermMDef :: TElement (((Term -> Flow s Term) -> Term -> Flow s Term) -> Term -> Flow s Term)
rewriteTermMDef = define "rewriteTermM" $
  doc "Monadic term rewriting with custom transformation function" $
  lambda "f" $ lets [
    "fsub">: lambda "recurse" $ lambda "term" $ lets [
      "forField">: lambda "f" $ Flows.map
        (lambda "t" $ Core.fieldWithTerm (var "t") (var "f"))
        (var "recurse" @@ Core.fieldTerm (var "f")),
      "forPair">: lambda "kv" $ lets [
        "k">: first $ var "kv",
        "v">: second $ var "kv"] $ binds [
        "km">: var "recurse" @@ var "k",
        "vm">: var "recurse" @@ var "v"] $
        produce $ pair (var "km") (var "vm"),
      "mapBinding">: lambda "binding" $ lets [
        "k">: Core.letBindingName $ var "binding",
        "v">: Core.letBindingTerm $ var "binding",
        "t">: Core.letBindingType $ var "binding"] $ binds [
        "v'">: var "recurse" @@ var "v"] $
        produce $ Core.letBinding (var "k") (var "v'") (var "t")] $
      cases _Term (var "term") Nothing [
        _Term_annotated>>: lambda "at" $ binds [
          "ex">: var "recurse" @@ Core.annotatedTermSubject (var "at")] $
          produce $ Core.termAnnotated $ Core.annotatedTerm (var "ex") (Core.annotatedTermAnnotation $ var "at"),
        _Term_application>>: lambda "app" $ binds [
          "lhs">: var "recurse" @@ Core.applicationFunction (var "app"),
          "rhs">: var "recurse" @@ Core.applicationArgument (var "app")] $
          produce $ Core.termApplication $ Core.application (var "lhs") (var "rhs"),
        _Term_function>>: lambda "fun" $ binds [
          "rfun">: cases _Function (var "fun") Nothing [
            _Function_elimination>>: lambda "e" $
              cases _Elimination (var "e") Nothing [
                _Elimination_product>>: lambda "tp" $ produce $ Core.functionElimination $ Core.eliminationProduct $ var "tp",
                _Elimination_record>>: lambda "p" $ produce $ Core.functionElimination $ Core.eliminationRecord $ var "p",
                _Elimination_union>>: lambda "cs" $ lets [
                  "n">: Core.caseStatementTypeName $ var "cs",
                  "def">: Core.caseStatementDefault $ var "cs",
                  "cases">: Core.caseStatementCases $ var "cs"]
                  $ Flows.bind
                      (Optionals.maybe (produce nothing)
                        (lambda "t" $ Flows.map (unaryFunction just) $ var "recurse" @@ var "t")
                        (var "def")) $
                    lambda "rdef" $
                      Flows.map
                        (lambda "rcases" $ Core.functionElimination $ Core.eliminationUnion $
                          Core.caseStatement (var "n") (var "rdef") (var "rcases"))
                        (Flows.mapList (var "forField") (var "cases")),
                _Elimination_wrap>>: lambda "name" $ produce $ Core.functionElimination $ Core.eliminationWrap $ var "name"],
            _Function_lambda>>: lambda "l" $ lets [
              "v">: Core.lambdaParameter $ var "l",
              "d">: Core.lambdaDomain $ var "l",
              "body">: Core.lambdaBody $ var "l"] $ binds [
              "rbody">: var "recurse" @@ var "body"] $
              produce $ Core.functionLambda $ Core.lambda (var "v") (var "d") (var "rbody"),
            _Function_primitive>>: lambda "name" $ produce $ Core.functionPrimitive $ var "name"]] $
          produce $ Core.termFunction $ var "rfun",
        _Term_let>>: lambda "lt" $ lets [
          "bindings">: Core.letBindings $ var "lt",
          "env">: Core.letEnvironment $ var "lt"] $ binds [
          "rbindings">: Flows.mapList (var "mapBinding") (var "bindings"),
          "renv">: var "recurse" @@ var "env"] $
          produce $ Core.termLet $ Core.let_ (var "rbindings") (var "renv"),
        _Term_list>>: lambda "els" $ binds [
          "rels">: Flows.mapList (var "recurse") (var "els")] $
          produce $ Core.termList $ var "rels",
        _Term_literal>>: lambda "v" $ produce $ Core.termLiteral $ var "v",
        _Term_map>>: lambda "m" $ binds [
          "pairs">: Flows.mapList (var "forPair") $ Maps.toList $ var "m"] $
          produce $ Core.termMap $ Maps.fromList $ var "pairs",
        _Term_optional>>: lambda "m" $ binds [
          "rm">: Flows.traverseOptional (var "recurse") (var "m")] $
          produce $ Core.termOptional $ var "rm",
        _Term_product>>: lambda "tuple" $ Flows.map
            (lambda "rtuple" $ Core.termProduct $ var "rtuple")
            (Flows.mapList (var "recurse") (var "tuple")),
        _Term_record>>: lambda "r" $ lets [
          "n">: Core.recordTypeName $ var "r",
          "fields">: Core.recordFields $ var "r"] $
          Flows.map
            (lambda "rfields" $ Core.termRecord $ Core.record (var "n") (var "rfields"))
            (Flows.mapList (var "forField") (var "fields")),
        _Term_set>>: lambda "s" $ binds [
          "rlist">: Flows.mapList (var "recurse") $ Sets.toList $ var "s"] $
          produce $ Core.termSet $ Sets.fromList $ var "rlist",
        _Term_sum>>: lambda "sum" $ lets [
          "i">: Core.sumIndex $ var "sum",
          "s">: Core.sumSize $ var "sum",
          "trm">: Core.sumTerm $ var "sum"] $ binds [
          "rtrm">: var "recurse" @@ var "trm"] $
          produce $ Core.termSum $ Core.sum (var "i") (var "s") (var "rtrm"),
        _Term_union>>: lambda "i" $ lets [
          "n">: Core.injectionTypeName $ var "i",
          "field">: Core.injectionField $ var "i"] $
          Flows.map
            (lambda "rfield" $ Core.termUnion $ Core.injection (var "n") (var "rfield"))
            (var "forField" @@ var "field"),
        _Term_unit>>: constant $ produce Core.termUnit,
        _Term_variable>>: lambda "v" $ produce $ Core.termVariable $ var "v",
        _Term_wrap>>: lambda "wt" $ lets [
          "name">: Core.wrappedTermTypeName $ var "wt",
          "t">: Core.wrappedTermObject $ var "wt"] $ binds [
          "rt">: var "recurse" @@ var "t"] $
          produce $ Core.termWrap $ Core.wrappedTerm (var "name") (var "rt")]] $
    ref rewriteDef @@ var "fsub" @@ var "f"

rewriteTypeDef :: TElement (((Type -> Type) -> Type -> Type) -> Type -> Type)
rewriteTypeDef = define "rewriteType" $ lambda "f" $ lets [
  "fsub">: lambdas ["recurse", "typ"] $ lets [
    "forField">: lambda "f" $ Core.fieldTypeWithType (var "f") (var "recurse" @@ (Core.fieldTypeType $ var "f"))] $
    cases _Type (var "typ") Nothing [
      _Type_annotated>>: lambda "at" $ Core.typeAnnotated $ Core.annotatedType
        (var "recurse" @@ (Core.annotatedTypeSubject $ var "at"))
        (Core.annotatedTypeAnnotation $ var "at"),
      _Type_application>>: lambda "app" $ Core.typeApplication $ Core.applicationType
        (var "recurse" @@ (Core.applicationTypeFunction $ var "app"))
        (var "recurse" @@ (Core.applicationTypeArgument $ var "app")),
      _Type_function>>: lambda "fun" $ Core.typeFunction $ Core.functionType
        (var "recurse" @@ (Core.functionTypeDomain $ var "fun"))
        (var "recurse" @@ (Core.functionTypeCodomain $ var "fun")),
      _Type_forall>>: lambda "lt" $ Core.typeLambda $ Core.forallType
        (Core.forallTypeParameter $ var "lt")
        (var "recurse" @@ (Core.forallTypeBody $ var "lt")),
      _Type_list>>: lambda "t" $ Core.typeList $ var "recurse" @@ var "t",
      _Type_literal>>: lambda "lt" $ Core.typeLiteral $ var "lt",
      _Type_map>>: lambda "mt" $ Core.typeMap $ Core.mapType
        (var "recurse" @@ (Core.mapTypeKeys $ var "mt"))
        (var "recurse" @@ (Core.mapTypeValues $ var "mt")),
      _Type_optional>>: lambda "t" $ Core.typeOptional $ var "recurse" @@ var "t",
      _Type_product>>: lambda "ts" $ Core.typeProduct $ Lists.map (var "recurse") (var "ts"),
      _Type_record>>: lambda "rt" $ Core.typeRecord $ Core.rowType
        (Core.rowTypeTypeName $ var "rt")
        (Lists.map (var "forField") (Core.rowTypeFields $ var "rt")),
      _Type_set>>: lambda "t" $ Core.typeSet $ var "recurse" @@ var "t",
      _Type_sum>>: lambda "ts" $ Core.typeSum $ Lists.map (var "recurse") (var "ts"),
      _Type_union>>: lambda "rt" $ Core.typeUnion $ Core.rowType
        (Core.rowTypeTypeName $ var "rt")
        (Lists.map (var "forField") (Core.rowTypeFields $ var "rt")),
      _Type_unit>>: constant Core.typeUnit,
      _Type_variable>>: lambda "v" $ Core.typeVariable $ var "v",
      _Type_wrap>>: lambda "wt" $ Core.typeWrap $ Core.wrappedType
        (Core.wrappedTypeTypeName $ var "wt")
        (var "recurse" @@ (Core.wrappedTypeObject $ var "wt"))]] $
  ref rewriteDef @@ var "fsub" @@ var "f"

rewriteTypeMDef :: TElement (((Type -> Flow s Type) -> Type -> Flow s Type) -> Type -> Flow s Type)
rewriteTypeMDef = define "rewriteTypeM" $
  doc "Monadic type rewriting" $ lets [
  "fsub">: lambdas ["recurse", "typ"] $ cases _Type (var "typ") Nothing [
    _Type_annotated>>: lambda "at" $ binds [
      "t">: var "recurse" @@ (Core.annotatedTypeSubject $ var "at")] $
      produce $ Core.typeAnnotated $ Core.annotatedType (var "t") (Core.annotatedTypeAnnotation $ var "at"),
    _Type_application>>: lambda "at" $ binds [
      "lhs">: var "recurse" @@ (Core.applicationTypeFunction $ var "at"),
      "rhs">: var "recurse" @@ (Core.applicationTypeArgument $ var "at")] $
      produce $ Core.typeApplication $ Core.applicationType (var "lhs") (var "rhs"),
    _Type_function>>: lambda "ft" $ binds [
      "dom">: var "recurse" @@ (Core.functionTypeDomain $ var "ft"),
      "cod">: var "recurse" @@ (Core.functionTypeCodomain $ var "ft")] $
      produce $ Core.typeFunction $ Core.functionType (var "dom") (var "cod"),
    _Type_forall>>: lambda "ft" $ binds [
      "b">: var "recurse" @@ (Core.forallTypeBody $ var "ft")] $
      produce $ Core.typeForall $ Core.forallType (Core.forallTypeParameter $ var "ft") (var "b"),
    _Type_list>>: lambda "t" $ binds [
      "rt">: var "recurse" @@ var "t"] $
      produce $ Core.typeList $ var "rt",
    _Type_literal>>: lambda "lt" $ produce $ Core.typeLiteral $ var "lt",
    _Type_map>>: lambda "mt" $ binds [
      "kt">: var "recurse" @@ (Core.mapTypeKeys $ var "mt"),
      "vt">: var "recurse" @@ (Core.mapTypeValues $ var "mt")] $
      produce $ Core.typeMap $ Core.mapType (var "kt") (var "vt"),
    _Type_optional>>: lambda "t" $ binds [
      "rt">: var "recurse" @@ var "t"] $
      produce $ Core.typeOptional $ var "rt",
    _Type_product>>: lambda "types" $ binds [
      "rtypes">: Flows.mapList (var "recurse") (var "types")] $
      produce $ Core.typeProduct $ var "rtypes",
    _Type_record>>: lambda "rt" $ lets [
      "name">: Core.rowTypeTypeName $ var "rt",
      "fields">: Core.rowTypeFields $ var "rt",
      "forField">: lambda "f" $ binds [
        "t">: var "recurse" @@ (Core.fieldTypeType $ var "f")] $
        produce $ Core.fieldTypeWithType (var "f") (var "t")] $ binds [
      "rfields">: Flows.mapList (var "forField") (var "fields")] $
      produce $ Core.typeRecord $ Core.rowType (var "name") (var "rfields"),
    _Type_set>>: lambda "t" $ binds [
      "rt">: var "recurse" @@ var "t"] $
      produce $ Core.typeSet $ var "rt",
    _Type_sum>>: lambda "types" $ binds [
      "rtypes">: Flows.mapList (var "recurse") (var "types")] $
      produce $ Core.typeSum $ var "rtypes",
    _Type_union>>: lambda "rt" $ lets [
      "name">: Core.rowTypeTypeName $ var "rt",
      "fields">: Core.rowTypeFields $ var "rt",
      "forField">: lambda "f" $ binds [
        "t">: var "recurse" @@ (Core.fieldTypeType $ var "f")] $
        produce $ Core.fieldTypeWithType (var "f") (var "t")] $ binds [
      "rfields">: Flows.mapList (var "forField") (var "fields")] $
      produce $ Core.typeUnion $ Core.rowType (var "name") (var "rfields"),
    _Type_unit>>: constant $ produce Core.typeUnit,
    _Type_variable>>: lambda "v" $ produce $ Core.typeVariable $ var "v",
    _Type_wrap>>: lambda "wt" $ binds [
      "t">: var "recurse" @@ (Core.wrappedTypeObject $ var "wt")] $
      produce $ Core.typeWrap $ Core.wrappedType (Core.wrappedTypeTypeName $ var "wt") (var "t")]] $
  lambda "f" $ ref rewriteDef @@ var "fsub" @@ var "f"

simplifyTermDef :: TElement (Term -> Term)
simplifyTermDef = define "simplifyTerm" $
  doc "Simplify terms by applying beta reduction where possible" $
  lambda "term" $ lets [
    "simplify">: lambdas ["recurse", "term"] $ lets [
      "stripped">: ref deannotateTermDef @@ var "term"] $
      var "recurse" @@ (cases _Term (var "stripped")
        (Just $ var "term") [
        _Term_application>>: lambda "app" $ lets [
          "lhs">: Core.applicationFunction $ var "app",
          "rhs">: Core.applicationArgument $ var "app",
          "strippedLhs">: ref deannotateTermDef @@ var "lhs"] $
          cases _Term (var "strippedLhs")
            (Just $ var "term") [
            _Term_function>>: match _Function
              (Just $ var "term") [
              _Function_lambda>>: lambda "l" $ lets [
                "var">: Core.lambdaParameter $ var "l",
                "body">: Core.lambdaBody $ var "l"] $
                Logic.ifElse (Sets.member (var "var") (ref freeVariablesInTermDef @@ var "body"))
                  (lets [
                    "strippedRhs">: ref deannotateTermDef @@ var "rhs"] $
                    cases _Term (var "strippedRhs")
                      (Just $ var "term") [
                      _Term_variable>>: lambda "v" $
                        ref simplifyTermDef @@ (ref substituteVariableDef @@ var "var" @@ var "v" @@ var "body")])
                  (ref simplifyTermDef @@ var "body")]]])] $
    ref rewriteTermDef @@ var "simplify" @@ var "term"

substituteTypeVariablesDef :: TElement (M.Map Name Name -> Type -> Type)
substituteTypeVariablesDef = define "substituteTypeVariables" $
  doc "Substitute type variables in a type" $
  lambdas ["subst", "typ"] $ lets [
    "replace">: lambdas ["recurse", "typ"] $ cases _Type (var "typ")
      (Just $ var "recurse" @@ var "typ") [
      _Type_variable>>: lambda "n" $
        Core.typeVariable $ Optionals.fromMaybe (var "n") $ Maps.lookup (var "n") (var "subst")]] $
    ref rewriteTypeDef @@ var "replace" @@ var "typ"

substituteVariableDef :: TElement (Name -> Name -> Term -> Term)
substituteVariableDef = define "substituteVariable" $
  doc "Substitute one variable for another in a term" $
  lambdas ["from", "to", "term"] $ lets [
    "replace">: lambdas ["recurse", "term"] $
      cases _Term (var "term")
        (Just $ var "recurse" @@ var "term") [
        _Term_variable>>: lambda "x" $
          Core.termVariable $ Logic.ifElse (Equality.equal (var "x") (var "from")) (var "to") (var "x"),
        _Term_function>>: match _Function
          (Just $ var "recurse" @@ var "term") [
          _Function_lambda>>: lambda "l" $ Logic.ifElse
            (Equality.equal (Core.lambdaParameter $ var "l") (var "from"))
            (var "term")
            (var "recurse" @@ var "term")]]] $
    ref rewriteTermDef @@ var "replace" @@ var "term"

substituteVariablesDef :: TElement (M.Map Name Name -> Term -> Term)
substituteVariablesDef = define "substituteVariables" $
  doc "Substitute multiple variables in a term" $
  lambdas ["subst", "term"] $ lets [
    "replace">: lambdas ["recurse", "term"] $
      cases _Term (var "term")
        (Just $ var "recurse" @@ var "term") [
        _Term_variable>>: lambda "n" $
          Core.termVariable $ Optionals.fromMaybe (var "n") $ Maps.lookup (var "n") (var "subst"),
        _Term_function>>: match _Function
          (Just $ var "recurse" @@ var "term") [
          _Function_lambda>>: lambda "l" $
            Optionals.maybe
              (var "recurse" @@ var "term")
              (constant $ var "term")
              (Maps.lookup (Core.lambdaParameter $ var "l") (var "subst"))]]] $
    ref rewriteTermDef @@ var "replace" @@ var "term"

subtermsDef :: TElement (Term -> [Term])
subtermsDef = define "subterms" $
  doc "Find the children of a given term" $
  match _Term Nothing [
    _Term_annotated>>: lambda "at" $ list [Core.annotatedTermSubject $ var "at"],
    _Term_application>>: lambda "p" $ list [
      Core.applicationFunction $ var "p",
      Core.applicationArgument $ var "p"],
    _Term_function>>: match _Function
      (Just $ list []) [
      _Function_elimination>>: match _Elimination
        (Just $ list []) [
        _Elimination_union>>: lambda "cs" $ Lists.concat2
          (Optionals.maybe (list []) (lambda "t" $ list [var "t"]) (Core.caseStatementDefault $ var "cs"))
          (Lists.map (unaryFunction Core.fieldTerm) (Core.caseStatementCases $ var "cs"))],
      _Function_lambda>>: lambda "l" $ list [Core.lambdaBody $ var "l"]],
    _Term_let>>: lambda "lt" $ Lists.cons
      (Core.letEnvironment $ var "lt")
      (Lists.map (unaryFunction Core.letBindingTerm) (Core.letBindings $ var "lt")),
    _Term_list>>: lambda "l" $ var "l",
    _Term_literal>>: constant $ list [],
    _Term_map>>: lambda "m" $ Lists.concat $ Lists.map
      (lambda "p" $ list [first $ var "p", second $ var "p"])
      (Maps.toList $ var "m"),
    _Term_optional>>: lambda "m" $ Optionals.maybe (list []) (lambda "t" $ list [var "t"]) (var "m"),
    _Term_product>>: lambda "tuple" $ var "tuple",
    _Term_record>>: lambda "rt" (Lists.map (unaryFunction Core.fieldTerm) (Core.recordFields $ var "rt")),
    _Term_set>>: lambda "l" $ Sets.toList $ var "l",
    _Term_sum>>: lambda "st" $ list [Core.sumTerm $ var "st"],
    _Term_typeAbstraction>>: lambda "ta" $ list [Core.typeAbstractionBody $ var "ta"],
    _Term_typeApplication>>: lambda "ta" $ list [Core.typedTermTerm $ var "ta"],
    _Term_union>>: lambda "ut" $ list [Core.fieldTerm $ (Core.injectionField $ var "ut")],
    _Term_unit>>: constant $ list [],
    _Term_variable>>: constant $ list [],
    _Term_wrap>>: lambda "n" $ list [Core.wrappedTermObject $ var "n"]]

subtermsWithAccessorsDef :: TElement (Term -> [(TermAccessor, Term)])
subtermsWithAccessorsDef = define "subtermsWithAccessors" $
  doc "Find the children of a given term" $
  match _Term Nothing [
    _Term_annotated>>: lambda "at" $ single Mantle.termAccessorAnnotatedSubject $ Core.annotatedTermSubject $ var "at",
    _Term_application>>: lambda "p" $ list [
      result Mantle.termAccessorApplicationFunction $ Core.applicationFunction $ var "p",
      result Mantle.termAccessorApplicationArgument $ Core.applicationArgument $ var "p"],
    _Term_function>>: match _Function
      (Just none) [
      _Function_elimination>>: match _Elimination
        (Just none) [
        _Elimination_union>>: lambda "cs" $ Lists.concat2
          (Optionals.maybe none
            (lambda "t" $ single Mantle.termAccessorUnionCasesDefault $ var "t")
            (Core.caseStatementDefault $ var "cs"))
          (Lists.map
            (lambda "f" $ result (Mantle.termAccessorUnionCasesBranch $ Core.fieldName $ var "f") $ Core.fieldTerm $ var "f")
            (Core.caseStatementCases $ var "cs"))],
      _Function_lambda>>: lambda "l" $ single Mantle.termAccessorLambdaBody $ Core.lambdaBody $ var "l"],
    _Term_let>>: lambda "lt" $ Lists.cons
      (result Mantle.termAccessorLetEnvironment $ Core.letEnvironment $ var "lt")
      (Lists.map
        (lambda "b" $ result (Mantle.termAccessorLetBinding $ Core.letBindingName $ var "b") $ Core.letBindingTerm $ var "b")
        (Core.letBindings $ var "lt")),
    _Term_list>>: lambda "l" $ Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      (lambda "e" $ result (Mantle.termAccessorListElement $ int32 0) $ var "e")
      (var "l"),
    _Term_literal>>: constant none,
    _Term_map>>: lambda "m" (Lists.concat
      (Lists.map
        (lambda "p" $ list [
          -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
          result (Mantle.termAccessorMapKey $ int32 0) $ first $ var "p",
          result (Mantle.termAccessorMapValue $ int32 0) $ second $ var "p"])
        (Maps.toList $ var "m"))),
    _Term_optional>>: lambda "m" $ Optionals.maybe none
      (lambda "t" $ single Mantle.termAccessorOptionalTerm $ var "t")
      (var "m"),
    _Term_product>>: lambda "p" $ Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      (lambda "e" $ result (Mantle.termAccessorProductTerm $ int32 0) $ var "e")
      (var "p"),
    _Term_record>>: lambda "rt" (Lists.map
      (lambda "f" $ result (Mantle.termAccessorRecordField $ Core.fieldName $ var "f") $ Core.fieldTerm $ var "f")
      (Core.recordFields $ var "rt")),
    _Term_set>>: lambda "s" $ Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      (lambda "e" $ result (Mantle.termAccessorListElement $ int32 0) $ var "e")
      (Sets.toList $ var "s"),
    _Term_sum>>: lambda "st" $
      single Mantle.termAccessorSumTerm $
      Core.sumTerm $ var "st",
    _Term_typeAbstraction>>: lambda "ta" $
      single Mantle.termAccessorTypeAbstractionBody $
      Core.typeAbstractionBody $ var "ta",
    _Term_typeApplication>>: lambda "ta" $
      single Mantle.termAccessorTypeApplicationTerm $
      Core.typedTermTerm $ var "ta",
    _Term_union>>: lambda "ut" $
      single Mantle.termAccessorInjectionTerm $
      Core.fieldTerm $ (Core.injectionField $ var "ut"),
    _Term_unit>>: constant none,
    _Term_variable>>: constant none,
    _Term_wrap>>: lambda "n" $ single Mantle.termAccessorWrappedTerm $ Core.wrappedTermObject $ var "n"]
  where
    none = list []
    single accessor term = list [result accessor term]
    result accessor term = pair accessor term
    simple term = result Mantle.termAccessorAnnotatedSubject term

subtypesDef :: TElement (Type -> [Type])
subtypesDef = define "subtypes" $
  doc "Find the children of a given type expression" $
  match _Type Nothing [
    _Type_annotated>>: lambda "at" $ list [Core.annotatedTypeSubject $ var "at"],
    _Type_application>>: lambda "at" $ list [
      Core.applicationTypeFunction $ var "at",
      Core.applicationTypeArgument $ var "at"],
    _Type_function>>: lambda "ft" $ list [
      Core.functionTypeDomain $ var "ft",
      Core.functionTypeCodomain $ var "ft"],
    _Type_forall>>: lambda "lt" $ list [Core.forallTypeBody $ var "lt"],
    _Type_list>>: lambda "lt" $ list [var "lt"],
    _Type_literal>>: constant $ list [],
    _Type_map>>: lambda "mt" $ list [
      Core.mapTypeKeys $ var "mt",
      Core.mapTypeValues $ var "mt"],
    _Type_optional>>: lambda "ot" $ list [var "ot"],
    _Type_product>>: lambda "pt" $ var "pt",
    _Type_record>>: lambda "rt" (Lists.map (unaryFunction Core.fieldTypeType) (Core.rowTypeFields $ var "rt")),
    _Type_set>>: lambda "st" $ list [var "st"],
    _Type_sum>>: lambda "st" $ var "st",
    _Type_union>>: lambda "rt" (Lists.map (unaryFunction Core.fieldTypeType) (Core.rowTypeFields $ var "rt")),
    _Type_unit>>: constant $ list [],
    _Type_variable>>: constant $ list [],
    _Type_wrap>>: lambda "nt" $ list [Core.wrappedTypeObject $ var "nt"]]

termDependencyNamesDef :: TElement (Bool -> Bool -> Bool -> Term -> S.Set Name)
termDependencyNamesDef = define "termDependencyNames" $
  doc "Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that" $
  lambdas ["binds", "withPrims", "withNoms"] $ lets [
    "addNames">: lambdas ["names", "term"] $ lets [
      "nominal">: lambda "name" $ Logic.ifElse (var "withNoms")
        (Sets.insert (var "name") (var "names"))
        (var "names"),
      "prim">: lambda "name" $ Logic.ifElse (var "withPrims")
        (Sets.insert (var "name") (var "names"))
        (var "names"),
      "var">: lambda "name" $ Logic.ifElse (var "binds")
        (Sets.insert (var "name") (var "names"))
        (var "names")]
      $ cases _Term (var "term")
        (Just $ var "names") [
        _Term_function>>: lambda "f" $ cases _Function (var "f")
          (Just $ var "names") [
          _Function_primitive>>: lambda "name" $ var "prim" @@ var "name",
          _Function_elimination>>: lambda "e" $ cases _Elimination (var "e")
            (Just $ var "names") [
            _Elimination_record>>: lambda "proj" $ var "nominal" @@ (Core.projectionTypeName $ var "proj"),
            _Elimination_union>>: lambda "caseStmt" $ var "nominal" @@ (Core.caseStatementTypeName $ var "caseStmt"),
            _Elimination_wrap>>: lambda "name" $ var "nominal" @@ var "name"]],
        _Term_record>>: lambda "record" $ var "nominal" @@ (Core.recordTypeName $ var "record"),
        _Term_union>>: lambda "injection" $ var "nominal" @@ (Core.injectionTypeName $ var "injection"),
        _Term_variable>>: lambda "name" $ var "var" @@ var "name",
        _Term_wrap>>: lambda "wrappedTerm" $ var "nominal" @@ (Core.wrappedTermTypeName $ var "wrappedTerm")]]
    $ ref foldOverTermDef @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty

toShortNamesDef :: TElement ([Name] -> M.Map Name Name)
toShortNamesDef = define "toShortNames" $
  doc "Generate short names from a list of fully qualified names" $
  lambda "original" $ lets [
    "groupNamesByLocal">: lambda "names" $ Lists.foldl (var "addName") Maps.empty (var "names"),
    "addName">: lambda "acc" $ lambda "name" $ lets [
      "local">: ref Names.localNameOfDef @@ var "name",
      "group">: Optionals.fromMaybe Sets.empty $ Maps.lookup (var "local") (var "acc")]
      $ Maps.insert (var "local") (Sets.insert (var "name") (var "group")) (var "acc"),
    "groups">: var "groupNamesByLocal" @@ var "original",
    "renameGroup">: lambda "localNames" $ lets [
      "local">: first $ var "localNames",
      "names">: second $ var "localNames",
      "rangeFrom">: lambda "start" $ Lists.cons (var "start") (var "rangeFrom" @@ (Math.add (var "start") (int32 1))),
      "rename">: lambda "name" $ lambda "i" $ pair (var "name") $ Core.name $
        Logic.ifElse (Equality.gt (var "i") (int32 1))
          (Strings.cat2 (var "local") (Literals.showInt32 $ var "i"))
          (var "local")]
      $ Lists.zipWith (var "rename") (Sets.toList $ var "names") (var "rangeFrom" @@ int32 1)]
    $ Maps.fromList $ Lists.concat $ Lists.map (var "renameGroup") $ Maps.toList $ var "groups"

topologicalSortBindingsDef :: TElement (M.Map Name Term -> [[(Name, Term)]])
topologicalSortBindingsDef = define "topologicalSortBindings" $
  doc "Topological sort of connected components, in terms of dependencies between variable/term binding pairs" $
  lambda "bindingMap" $ lets [
    "bindings">: Maps.toList $ var "bindingMap",
    "keys">: Sets.fromList $ Lists.map (unaryFunction first) (var "bindings"),
    "hasTypeAnnotation">: lambda "term" $
      cases _Term (var "term")
        (Just false) [
        _Term_annotated>>: lambda "at" $ var "hasTypeAnnotation" @@ (Core.annotatedTermSubject $ var "at")],
    "depsOf">: lambda "nameAndTerm" $ lets [
      "name">: first $ var "nameAndTerm",
      "term">: second $ var "nameAndTerm"]
      $ pair (var "name") $ Logic.ifElse (var "hasTypeAnnotation" @@ var "term")
        (list [])
        (Sets.toList $ Sets.intersection (var "keys") $ ref freeVariablesInTermDef @@ var "term"),
    "toPair">: lambda "name" $ pair (var "name") $ Optionals.fromMaybe
      (Core.termLiteral $ Core.literalString $ string "Impossible!")
      (Maps.lookup (var "name") (var "bindingMap"))]
    $ Lists.map (unaryFunction $ Lists.map $ var "toPair") (ref Sorting.topologicalSortComponentsDef @@ Lists.map (var "depsOf") (var "bindings"))

topologicalSortElementsDef :: TElement ([Element] -> Either [[Name]] [Name])
topologicalSortElementsDef = define "topologicalSortElements" $
  doc "Topological sort of elements based on their dependencies" $
  lambda "els" $ lets [
    "adjlist">: lambda "e" $ pair
      (Graph.elementName $ var "e")
      (Sets.toList $ ref termDependencyNamesDef @@ false @@ true @@ true @@ (Graph.elementTerm $ var "e"))]
    $ ref Sorting.topologicalSortDef @@ Lists.map (var "adjlist") (var "els")

typeDependencyNamesDef :: TElement (Bool -> Type -> S.Set Name)
typeDependencyNamesDef = define "typeDependencyNames" $
  lambdas ["withSchema", "typ"] $
    Logic.ifElse (var "withSchema")
      (Sets.union
        (ref freeVariablesInTypeDef @@ var "typ")
        (ref typeNamesInTypeDef @@ var "typ"))
      (ref freeVariablesInTypeDef @@ var "typ")

typeNamesInTypeDef :: TElement (Type -> S.Set Name)
typeNamesInTypeDef = define "typeNamesInType" $ lets [
  "addNames">: lambdas ["names", "typ"] $ cases _Type (var "typ")
    (Just $ var "names") [
    _Type_record>>: lambda "rowType" $ lets [
      "tname">: Core.rowTypeTypeName $ var "rowType"] $
      Sets.insert (var "tname") (var "names"),
    _Type_union>>: lambda "rowType" $ lets [
      "tname">: Core.rowTypeTypeName $ var "rowType"] $
      Sets.insert (var "tname") (var "names"),
    _Type_wrap>>: lambda "wrappedType" $ lets [
      "tname">: Core.wrappedTypeTypeName $ var "wrappedType"] $
      Sets.insert (var "tname") (var "names")]] $
  ref foldOverTypeDef @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty
